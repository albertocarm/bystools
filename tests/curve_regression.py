#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""Regression suite for the bundled Kaplan-Meier digitizer.

The archive under ``curves/`` records real published figures together with the
result the engine is expected to produce for each. Running this script replays
every figure through ``inst/python/kmdig3.py`` and compares the outcome against
that record, so a change made to fix one figure is checked against all the
others before it is kept.

The record is versioned; the figures themselves are held locally and are not
redistributed, so each record carries the checksum of the image it was taken
from. A record whose image is not present is skipped.

What is compared, per figure:

  * the number of curves recovered, and whether the engine's own self-check
    (``fit_ok``) still passes;
  * the calibrated survival of each curve sampled on a fixed grid of times,
    within a tolerance in percentage points -- this is the quantity the
    downstream analysis actually consumes, and it is insensitive to harmless
    sub-pixel differences;
  * the numbers-at-risk rows the app would seat as the two study arms.

Figures whose ``status`` is ``known-failure`` are replayed as well, but their
failures do not fail the run: they record the current frontier, and the script
reports when one of them starts passing so it can be promoted.

Usage
-----
    python tests/curve_regression.py                 check every figure
    python tests/curve_regression.py -k titan        check figures matching a substring
    python tests/curve_regression.py --update        re-record expectations from the
                                                     current engine output
    python tests/curve_regression.py --add FILE --id ID --source "..." [--status ...]

The exit status is non-zero when a figure recorded as passing no longer does.
"""
import argparse
import csv
import hashlib
import json
import os
import shutil
import subprocess
import sys
import tempfile

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
ARCHIVE = os.path.join(ROOT, "curves")
FIGURES = os.path.join(ARCHIVE, "figures")
MANIFEST = os.path.join(ARCHIVE, "manifest.json")
ENGINE = os.path.join(ROOT, "inst", "python", "kmdig3.py")

GRID_POINTS = 11
STATUSES = ("passing", "known-failure")


# --------------------------------------------------------------------------- #
# archive
# --------------------------------------------------------------------------- #
def load_manifest():
    if not os.path.exists(MANIFEST):
        return {"version": 1,
                "tolerance": {"survival_pct": 2.0, "offmask": 0.10},
                "figures": []}
    with open(MANIFEST, encoding="utf-8") as fh:
        return json.load(fh)


def save_manifest(man):
    man["figures"].sort(key=lambda f: f["id"])
    os.makedirs(ARCHIVE, exist_ok=True)
    with open(MANIFEST, "w", encoding="utf-8", newline="\n") as fh:
        json.dump(man, fh, indent=2, ensure_ascii=False)
        fh.write("\n")


def sha256(path):
    h = hashlib.sha256()
    with open(path, "rb") as fh:
        for chunk in iter(lambda: fh.read(1 << 20), b""):
            h.update(chunk)
    return h.hexdigest()


# --------------------------------------------------------------------------- #
# engine
# --------------------------------------------------------------------------- #
def run_engine(image, n_curves=2):
    """Digitize one figure; return (meta, curves) or raise RuntimeError."""
    tmp = tempfile.mkdtemp(prefix="km2curves_")
    try:
        prefix = os.path.join(tmp, "out")
        proc = subprocess.run([sys.executable, ENGINE, image, prefix, str(n_curves)],
                              capture_output=True, text=True)
        meta_path = prefix + "_meta.json"
        if not os.path.exists(meta_path):
            raise RuntimeError((proc.stderr or proc.stdout or "engine produced no output")
                               .strip().splitlines()[-1:][0] if (proc.stderr or proc.stdout)
                               else "engine produced no output")
        with open(meta_path, encoding="utf-8") as fh:
            meta = json.load(fh)
        curves = {}
        csv_path = prefix + ".csv"
        if os.path.exists(csv_path):
            with open(csv_path, encoding="utf-8") as fh:
                for row in csv.DictReader(fh):
                    try:
                        t, s = float(row["x_val"]), float(row["y_val"])
                    except ValueError:
                        continue
                    if t == t and s == s:                       # drop NaN
                        curves.setdefault(row["curve"], []).append((t, s))
        return meta, [sorted(curves[k]) for k in sorted(curves)]
    finally:
        shutil.rmtree(tmp, ignore_errors=True)


def step_at(points, t, reach=0.0):
    """Value of a step function at time ``t``: the last sample at or before it.

    A trace rarely has a sample exactly at the origin, so a time up to ``reach``
    before the first sample takes that sample's value -- survival is flat there
    by definition. Further back than that returns None, which is how a trace
    that starts far too late is caught.
    """
    lo, hi = 0, len(points)
    while lo < hi:
        mid = (lo + hi) // 2
        if points[mid][0] <= t:
            lo = mid + 1
        else:
            hi = mid
    if lo:
        return points[lo - 1][1]
    return points[0][1] if points and t >= points[0][0] - reach else None


def arm_rows(meta):
    """The numbers-at-risk rows the app seats as the two study arms.

    Mirrors the selection in .km2_build_risk_table(): keep rows that are long,
    non-increasing and start above zero, then rank by column count and cohort
    size. This is what a user sees in the grid, so it is what the archive pins.
    """
    rows = []
    for r in meta.get("at_risk") or []:
        vals = [v for v in (r.get("values") or []) if isinstance(v, (int, float))]
        if len(vals) >= 4 and vals[0] > 0 and max(vals) > min(vals) \
                and all(b <= a for a, b in zip(vals, vals[1:])):
            rows.append(vals)
    rows.sort(key=lambda v: (-len(v), -max(v)))
    return rows[:2]


def summarise(meta, curves, grid=None):
    """Reduce an engine run to the quantities the archive compares."""
    tmax = 0.0
    for pts in curves:
        if pts:
            tmax = max(tmax, pts[-1][0])
    if grid is None:
        top = 0.0
        for v, _ in (meta.get("xcal") or {}).get("ticks") or []:
            top = max(top, float(v))
        top = top or tmax
        grid = [round(top * i / (GRID_POINTS - 1), 4) for i in range(GRID_POINTS)]
    reach = (grid[1] - grid[0]) if len(grid) > 1 else 0.0
    sampled = []
    for pts in curves:
        row = []
        for t in grid:
            row.append(None if not pts or t > pts[-1][0] + 1e-9 else step_at(pts, t, reach))
        sampled.append([None if v is None else round(v, 3) for v in row])
    offmask = [q for q in (meta.get("curve_offmask") or []) if isinstance(q, (int, float))]
    return {
        "n_curves": int(meta.get("n_curves") or 0),
        "arms_detected": int(meta.get("arms_detected") or 0),
        "fit_ok": bool(meta.get("fit_ok")),
        "max_offmask": round(max(offmask), 3) if offmask else None,
        "time_grid": grid,
        "survival": sampled,
        "at_risk_arms": arm_rows(meta),
    }


# --------------------------------------------------------------------------- #
# comparison
# --------------------------------------------------------------------------- #
def compare(expect, actual, tol):
    """Return a list of human-readable regressions (empty when the run matches)."""
    bad = []
    if actual.get("error"):
        return ["engine failed: " + actual["error"]]

    if expect["n_curves"] != actual["n_curves"]:
        bad.append("curves recovered: %d -> %d" % (expect["n_curves"], actual["n_curves"]))
    if expect["arms_detected"] != actual["arms_detected"]:
        bad.append("arms detected: %d -> %d" % (expect["arms_detected"], actual["arms_detected"]))
    if expect["fit_ok"] and not actual["fit_ok"]:
        bad.append("self-check fit_ok: true -> false")

    e_off, a_off = expect.get("max_offmask"), actual.get("max_offmask")
    if e_off is not None and a_off is not None and a_off > e_off + tol["offmask"]:
        bad.append("worst off-mask score: %.3f -> %.3f" % (e_off, a_off))

    if expect["at_risk_arms"] != actual["at_risk_arms"]:
        bad.append("numbers-at-risk arm rows changed:\n      was %s\n      now %s"
                   % (expect["at_risk_arms"], actual["at_risk_arms"]))

    if expect["n_curves"] == actual["n_curves"]:
        lim = tol["survival_pct"]
        for i, (exp_row, act_row) in enumerate(zip(expect["survival"], actual["survival"])):
            worst = None
            for t, e, a in zip(expect["time_grid"], exp_row, act_row):
                if e is None:
                    continue
                if a is None:
                    bad.append("curve %d no longer reaches t=%g" % (i + 1, t))
                    break
                d = abs(e - a)
                if d > lim and (worst is None or d > worst[1]):
                    worst = (t, d, e, a)
            if worst:
                bad.append("curve %d drifts %.2f pts at t=%g (%.2f -> %.2f)"
                           % (i + 1, worst[1], worst[0], worst[2], worst[3]))
    return bad


# --------------------------------------------------------------------------- #
# commands
# --------------------------------------------------------------------------- #
def measure(fig):
    """Run one archived figure; returns the summary, or an error/absence marker.

    The images are held locally rather than distributed with the repository, so
    a record whose image is not present is reported as absent and skipped, not
    counted against the run.
    """
    path = os.path.join(FIGURES, fig["file"])
    if not os.path.exists(path):
        return {"missing": True}
    try:
        meta, curves = run_engine(path, fig.get("n_curves", 2))
    except Exception as exc:                                    # engine crash or no output
        return {"error": str(exc)[:200]}
    grid = (fig.get("expect") or {}).get("time_grid")
    return summarise(meta, curves, grid)


def cmd_check(man, selected):
    tol = man["tolerance"]
    failures = promotions = absent = 0
    for fig in selected:
        actual = measure(fig)
        if actual.get("missing"):
            absent += 1
            print("  --   %-38s image not held locally" % fig["id"])
            continue
        expect = fig.get("expect")
        if not expect:
            print("  ?    %-38s no expectation recorded (run --update)" % fig["id"])
            continue
        bad = compare(expect, actual, tol)
        known = fig.get("status") == "known-failure"
        if not bad:
            if known:
                promotions += 1
                print("  +    %-38s now matches its record; promote it to passing" % fig["id"])
            else:
                print("  ok   %-38s" % fig["id"])
        elif known:
            print("  -    %-38s known failure (%s)" % (fig["id"], bad[0].split("\n")[0]))
        else:
            failures += 1
            print("  FAIL %-38s" % fig["id"])
            for b in bad:
                print("       %s" % b)
    print("\n%d of %d figure(s) checked, %d regression(s)%s%s."
          % (len(selected) - absent, len(selected), failures,
             ", %d ready to promote" % promotions if promotions else "",
             ", %d image(s) not held locally" % absent if absent else ""))
    return 1 if failures else 0


def cmd_update(man, selected):
    for fig in selected:
        actual = measure(fig)
        if actual.get("missing"):
            print("  skip     %-34s image not held locally" % fig["id"])
            continue
        if actual.get("error"):
            print("  skip     %-34s %s" % (fig["id"], actual["error"]))
            continue
        fig["expect"] = actual
        print("  recorded %-34s %d curve(s), fit_ok=%s"
              % (fig["id"], actual["n_curves"], actual["fit_ok"]))
    save_manifest(man)
    return 0


def cmd_add(man, args):
    if not os.path.exists(args.add):
        print("no such file: %s" % args.add)
        return 1
    if any(f["id"] == args.id for f in man["figures"]):
        print("id already in the archive: %s" % args.id)
        return 1
    os.makedirs(FIGURES, exist_ok=True)
    dest_name = args.id + os.path.splitext(args.add)[1].lower()
    shutil.copyfile(args.add, os.path.join(FIGURES, dest_name))
    fig = {
        "id": args.id,
        "file": dest_name,
        "sha256": sha256(os.path.join(FIGURES, dest_name)),
        "source": args.source,
        "traits": [t.strip() for t in (args.traits or "").split(",") if t.strip()],
        "n_curves": args.n_curves,
        "status": args.status,
    }
    man["figures"].append(fig)
    actual = measure(fig)
    if not actual.get("error"):
        fig["expect"] = actual
        print("added %s: %d curve(s) recovered, fit_ok=%s"
              % (args.id, actual["n_curves"], actual["fit_ok"]))
    else:
        print("added %s: engine reported '%s'" % (args.id, actual["error"]))
    save_manifest(man)
    print("Review the trace before marking it passing; a figure the engine still "
          "gets wrong belongs in the archive as status 'known-failure'.")
    return 0


def main():
    ap = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    ap.add_argument("-k", "--filter", help="only figures whose id contains this text")
    ap.add_argument("--update", action="store_true",
                    help="re-record expectations from the current engine output")
    ap.add_argument("--add", metavar="FILE", help="add a figure to the archive")
    ap.add_argument("--id", help="archive id for --add (kebab-case)")
    ap.add_argument("--source", default="", help="citation for --add")
    ap.add_argument("--traits", default="", help="comma-separated traits for --add")
    ap.add_argument("--status", default="known-failure", choices=STATUSES,
                    help="status for --add (default: known-failure)")
    ap.add_argument("--n-curves", type=int, default=2,
                    help="arms the figure shows (default: 2)")
    args = ap.parse_args()

    man = load_manifest()
    if args.add:
        if not args.id:
            ap.error("--add requires --id")
        return cmd_add(man, args)

    figs = man["figures"]
    if args.filter:
        figs = [f for f in figs if args.filter.lower() in f["id"].lower()]
    if not figs:
        print("no figures selected")
        return 0
    if args.update:
        return cmd_update(man, figs)
    return cmd_check(man, figs)


if __name__ == "__main__":
    sys.exit(main())
