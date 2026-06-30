#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
kmdig.py  -  General Kaplan-Meier curve digitizer for published figures.

Outputs per figure:
  <prefix>_overlay.png : original with the digitization overlaid in RED.
  <prefix>_clean.png   : clean canvas (curves + axes + axis numbers).
  <prefix>.csv         : points (curve, x_px, y_px, x_val, y_val).
  <prefix>_meta.json   : box, calibration and numbers-at-risk vector.

Calibration is computed in code (tick OCR + robust linear regression).
No hand-set values.
"""
import sys, os, re, json
import numpy as np
import cv2
import pytesseract
from pytesseract import Output
from sklearn.cluster import KMeans
from sklearn.isotonic import IsotonicRegression


# --------------------------------------------------------------------------- #
# On Windows, locate the Tesseract OCR engine even if it is not on PATH.
# Tries the default installer locations so the user does not have to edit PATH.
# --------------------------------------------------------------------------- #
def _ensure_tesseract():
    import shutil
    if shutil.which("tesseract"):
        return  # already on PATH
    candidates = [
        # Windows
        r"C:\Program Files\Tesseract-OCR\tesseract.exe",
        r"C:\Program Files (x86)\Tesseract-OCR\tesseract.exe",
        os.path.expandvars(r"%LOCALAPPDATA%\Programs\Tesseract-OCR\tesseract.exe"),
        # macOS (Homebrew: Apple Silicon and Intel) and Linux
        "/opt/homebrew/bin/tesseract",
        "/usr/local/bin/tesseract",
        "/usr/bin/tesseract",
        "/bin/tesseract",
    ]
    for c in candidates:
        if c and os.path.exists(c):
            pytesseract.pytesseract.tesseract_cmd = c
            return


_ensure_tesseract()


# --------------------------------------------------------------------------- #
def _longest_run(boolvec):
    idx = np.where(boolvec)[0]
    if len(idx) == 0:
        return 0, 0, 0
    best = cur = 0
    bs = be = start = idx[0]
    prev = idx[0] - 1
    for p in idx:
        cur = cur + 1 if p == prev + 1 else 1
        if p != prev + 1:
            start = p
        if cur > best:
            best, bs, be = cur, start, p
        prev = p
    return best, bs, be


def colorful_mask(rgb, thr=25):
    ch = rgb.astype(int)
    return (ch.max(2) - ch.min(2)) > thr


# --------------------------------------------------------------------------- #
# plot box
# --------------------------------------------------------------------------- #
def detect_box(gray, colorful, dark_thr=175):
    H, W = gray.shape
    dark = gray < dark_thr
    bd = max(2, int(0.01 * min(H, W)))                       # ignore a thin border frame

    # Y axis: the single longest vertical dark line in the left ~45% of the image.
    # Scanning globally (not a window around the colorful centroid) keeps colored
    # titles, rotated axis labels and legends from dragging the edge leftward -- the
    # plot frame is by far the longest continuous vertical stroke.
    bestv = None
    for x in range(bd, max(bd + 1, int(0.45 * W))):
        r, y0, y1 = _longest_run(dark[:, x])
        if bestv is None or r > bestv[1]:
            bestv = (x, r, y0, y1)
    if bestv and bestv[1] >= 0.30 * H:
        L_axis, ytop, ybot = bestv[0], bestv[2], bestv[3]
    else:
        # No dark Y axis (e.g. survminer): fall back to the colorful left edge.
        ys, xs = np.where(colorful)
        L_axis = int(np.percentile(xs, 2)) if len(xs) > 50 else int(0.12 * W)
        ytop = int(np.percentile(ys, 2)) if len(ys) > 50 else int(0.08 * H)
        ybot = int(np.percentile(ys, 98)) if len(ys) > 50 else int(0.88 * H)

        # In axis-less styles the labels sit left of the colored trace; extend L left
        # to the first run of colored columns so the Y-label band is captured.
        colcount = colorful.sum(0)
        for x in range(0, min(W, int(.5 * W))):
            if all(colcount[min(W - 1, x + k)] >= 3 for k in range(3)):
                L_axis = max(3, min(L_axis, x)); break

    # X axis: the lowest long horizontal dark line (the baseline of the plot).
    runs = []
    x_from = max(0, L_axis - 2)
    for y in range(int(H * .30), H - bd):
        r, a, b = _longest_run(dark[y, x_from:W - bd])
        runs.append((y, r, a + x_from, b + x_from))
    rmax = max((r for _, r, _, _ in runs), default=0)
    cand = [t for t in runs if t[1] >= .8 * rmax and t[1] >= 0.30 * W]
    if cand:
        B, _, _, bx1 = max(cand, key=lambda t: t[0])
    else:
        B, bx1 = int(ybot), W - bd
    return int(L_axis), int(bx1), int(ytop), int(B), int(L_axis)


# --------------------------------------------------------------------------- #
# OCR
# --------------------------------------------------------------------------- #
def _ocr_tokens(crop_gray, scale, psm, whitelist=None):
    cfg = f"--psm {psm}"
    if whitelist:
        cfg += f" -c tessedit_char_whitelist={whitelist}"
    big = cv2.resize(crop_gray, None, fx=scale, fy=scale, interpolation=cv2.INTER_CUBIC)
    d = pytesseract.image_to_data(big, config=cfg, output_type=Output.DICT)
    out = []
    for i in range(len(d["text"])):
        t = d["text"][i].strip()
        if not t:
            continue
        try:
            conf = float(d["conf"][i])
        except ValueError:
            conf = -1
        out.append(dict(text=t, conf=conf,
                        x=d["left"][i] / scale, y=d["top"][i] / scale,
                        w=d["width"][i] / scale, h=d["height"][i] / scale))
    return out


_NUM = re.compile(r"^[0-9]+(\.[0-9]+)?$")


def _clean_num(s):
    s = (s.replace("O", "0").replace("o", "0").replace("l", "1").replace("I", "1")
          .replace("|", "1").replace("S", "5").replace("B", "8").replace(",", "."))
    s = re.sub(r"[^0-9.]", "", s)
    if s.count(".") > 1:
        s = s.replace(".", "", s.count(".") - 1)
    return s


def _ocr_numeric_pos(crop, x_off, y_off, allow_dot, scales=(2, 3, 4), psms=(11, 6, 7)):
    wl = "0123456789." if allow_dot else "0123456789"
    toks = []
    for sc in scales:
        for ps in psms:
            for t in _ocr_tokens(crop, sc, ps, wl):
                cs = _clean_num(t["text"])
                if cs and _NUM.match(cs):
                    toks.append(dict(val=float(cs), dot=("." in cs),
                                     xc=t["x"] + t["w"] / 2 + x_off,
                                     yc=t["y"] + t["h"] / 2 + y_off,
                                     w=t["w"], h=t["h"], conf=t["conf"]))
    return toks


# --------------------------------------------------------------------------- #
# robust linear regression (manual RANSAC)
# --------------------------------------------------------------------------- #
def robust_line(px, val, tol_frac=0.02):
    px = np.asarray(px, float); val = np.asarray(val, float)
    n = len(px)
    if n < 2:
        return None
    span = max(val.max() - val.min(), 1e-6)
    tol = max(tol_frac * span, 1e-6)
    best = None
    for i in range(n):
        for j in range(i + 1, n):
            if px[j] == px[i]:
                continue
            m = (val[j] - val[i]) / (px[j] - px[i])
            c = val[i] - m * px[i]
            res = np.abs(m * px + c - val)
            inl = res <= tol
            sc = inl.sum()
            if best is None or sc > best[0] or (sc == best[0] and res[inl].sum() < best[1]):
                best = (sc, res[inl].sum(), inl.copy())
    inl = best[2]
    if inl.sum() < 2:
        inl = np.ones(n, bool)
    m, c = np.polyfit(px[inl], val[inl], 1)
    return float(m), float(c), inl


def _dedup(toks, key):
    d = {}
    for t in toks:
        b = round(t[key] / 4) * 4
        if b not in d or t["conf"] > d[b][2]:
            d[b] = (t[key], t["val"], t["conf"])
    return list(d.values())


def _fit_axis(pts):
    if len(pts) < 2:
        return None
    pos = [p[0] for p in pts]; val = [p[1] for p in pts]
    fit = robust_line(pos, val)
    if fit is None:
        return None
    m, c, inl = fit
    pairs = [(val[i], pos[i]) for i in range(len(pos)) if inl[i]]
    byval = {}
    for v, p in pairs:
        byval.setdefault(round(v, 4), []).append(p)
    ticks = sorted([(v, float(np.median(ps))) for v, ps in byval.items()], key=lambda z: z[1])
    return dict(m=m, c=c, ticks=ticks)


def calibrate_y(gray, L_band, T_rough, yband):
    H, W = gray.shape
    x0 = max(0, L_band - yband); x1 = min(W, L_band + 4)   # include labels to the right of the trace edge
    y0 = max(0, T_rough - int(.04 * H)); y1 = H
    crop = gray[y0:y1, x0:x1]
    if crop.size == 0:
        return None, False
    dec = _ocr_numeric_pos(crop, x0, y0, allow_dot=True)
    dotted = set(round(t["val"], 2) for t in dec if t["dot"] and 0 <= t["val"] <= 1.0)
    fraction = len(dotted) >= 3
    toks = [t for t in dec if 0 <= t["val"] <= 1.5] if fraction \
        else _ocr_numeric_pos(crop, x0, y0, allow_dot=False)
    return _fit_axis(_dedup(toks, "yc")), fraction


def calibrate_x(gray, L, R, B, band_h):
    H, W = gray.shape
    y0 = min(B + 1, H - 1)
    if R <= L or y0 >= H:
        return None
    # Full-width, full-depth scan of the region below the axis: dense tick labels
    # often OCR cleanly only with this wider context (the same place the numbers at
    # risk are read), where a thin L:R strip fails.
    crop = gray[y0:H, 0:W]
    if crop.size == 0:
        return None
    toks = _ocr_numeric_pos(crop, 0, y0, allow_dot=False, scales=(2, 3), psms=(6, 11, 7))
    toks = [t for t in toks if L - 6 <= t["xc"] <= R + 6]
    # Drop OCR garbage: a survival-time axis label is a modest number. Concatenated
    # misreads (e.g. "3691215" or a 13-digit blob) otherwise inflate the fit span,
    # blow up the slope and corrupt every calibrated time.
    toks = [t for t in toks if 0 <= t["val"] <= 10000]
    if not toks:
        return None
    toks.sort(key=lambda t: t["yc"])
    rows = []; cur = [toks[0]]
    for t in toks[1:]:
        if t["yc"] - cur[-1]["yc"] <= 12:
            cur.append(t)
        else:
            rows.append(cur); cur = [t]
    rows.append(cur)
    best = None
    for row in rows:
        pts = _dedup(row, "xc")
        if len(pts) < 3:
            continue
        fit = _fit_axis(pts)
        # The time axis increases left to right (m>0); numbers-at-risk rows decrease,
        # so this rejects count rows and locks onto the real tick row even when it is
        # read from the deeper at-risk header.
        if fit is None or fit["m"] <= 0:
            continue
        score = (len(fit["ticks"]), -float(np.mean([t["yc"] for t in row])))
        if best is None or score > best[0]:
            best = (score, fit)
    return best[1] if best else None


# --------------------------------------------------------------------------- #
# colors and line masks
# --------------------------------------------------------------------------- #
def find_colors(lab, colorful, interior, k=2):
    mask = colorful & interior
    pts = lab[mask][:, 1:3].astype(np.float32)
    if len(pts) < 20:
        return []
    k = min(k, max(1, len(np.unique(pts, axis=0))))
    km = KMeans(n_clusters=k, n_init=5, random_state=0).fit(pts)
    return [c for c in km.cluster_centers_ if (abs(c[0] - 128) > 8 or abs(c[1] - 128) > 8)]


def line_mask_for_color(rgb, lab, centroid, colorful, interior, tol=20):
    ab = lab[:, :, 1:3].astype(np.float32)
    dist = np.sqrt(((ab - centroid) ** 2).sum(2))
    hue = (dist < tol) & colorful & interior
    if hue.sum() < 10:
        return hue
    ys, xs = np.where(hue)
    xspan = max(1, xs.max() - xs.min())
    fat = hue.sum() / xspan > 8.0                          # CI band present if "thick"
    if not fat:
        return hue
    # translucent band is light; the line sits above and is dark -> separate by L
    Lch = lab[:, :, 0].astype(np.float32)
    thr, _ = cv2.threshold(Lch[hue].astype(np.uint8), 0, 255,
                           cv2.THRESH_BINARY + cv2.THRESH_OTSU)
    line = hue & (Lch <= thr)
    return line if line.sum() >= 0.04 * hue.sum() else hue


def neutral_line_mask(rgb, gray, colorful, interior, txt, box):
    """Mask for a near-neutral (gray) curve, agnostic to the exact shade.

    Many journals draw one trial arm in gray; it carries almost no chroma, so the
    color clustering ignores it. We recover any low-saturation trace whose shade
    sits between the dark text/axis ink and the white page, then keep only the
    parts that are not text and not the plot frame. The acceptance test in the
    caller (wide column coverage) is what actually guards against picking up
    stray gray pixels, so the band here is deliberately broad rather than tuned
    to one figure."""
    L, R, T, B = box
    sat = rgb.astype(int).max(2) - rgb.astype(int).min(2)
    g = gray.astype(int)
    # neutral, clearly off-white, and lighter than typical axis/text ink
    m = (sat < 28) & (g > 110) & (g < 235) & interior & ~colorful & ~txt
    m[max(T, B - 3):, :] = False                            # drop the X-axis baseline
    m[:, :min(R, L + 3)] = False                            # drop the Y-axis line
    n, labc, st, _ = cv2.connectedComponentsWithStats(m.astype(np.uint8), 8)
    clean = np.zeros(m.shape, np.uint8)
    span = max(1, R - L); high = max(1, B - T)
    for i in range(1, n):
        w = st[i, cv2.CC_STAT_WIDTH]; h = st[i, cv2.CC_STAT_HEIGHT]
        if st[i, cv2.CC_STAT_AREA] >= 3 and (w >= 0.04 * span or h >= 0.04 * high):
            clean[labc == i] = 1                            # elongated -> line, not a glyph
    return clean.astype(bool)


def ocr_text_mask(gray, rgb, ocr_conf=25):
    """Text mask (to erase labels), protecting only curve-like colored structures.

    Colored text (titles, legends, in-plot statistics tables) must be erased too,
    otherwise it survives the color clustering as a spurious curve. We therefore
    protect a colored pixel only when it belongs to a wide connected component --
    i.e. the data curve itself -- and erase everything else inside a text box.
    """
    H, W = gray.shape
    txt = np.zeros((H, W), bool)
    ch = rgb.astype(int)
    strong = (ch.max(2) - ch.min(2)) > 30
    protect = np.zeros((H, W), bool)                        # wide colored components = curves
    n, lab, st, _ = cv2.connectedComponentsWithStats(strong.astype(np.uint8), 8)
    for i in range(1, n):
        if st[i, cv2.CC_STAT_WIDTH] >= 0.30 * W:
            protect[lab == i] = True
    sc = 2
    d = pytesseract.image_to_data(cv2.resize(gray, None, fx=sc, fy=sc,
                                  interpolation=cv2.INTER_CUBIC),
                                  config="--psm 11", output_type=Output.DICT)
    for i in range(len(d["text"])):
        if not d["text"][i].strip():
            continue
        try:
            conf = float(d["conf"][i])
        except ValueError:
            conf = -1
        if conf < ocr_conf:
            continue
        x, y = int(d["left"][i] / sc), int(d["top"][i] / sc)
        w, h = int(d["width"][i] / sc), int(d["height"][i] / sc)
        if h >= 60 or w >= 0.8 * W:
            continue
        txt[max(0, y - 2):y + h + 2, max(0, x - 2):x + w + 2] = True
    return txt & ~protect


# --------------------------------------------------------------------------- #
# step reconstruction
# --------------------------------------------------------------------------- #
def column_trace(mask, L, R, thin_max):
    cols, ys = [], []
    for x in range(L, R + 1):
        yy = np.sort(np.where(mask[:, x])[0])
        if len(yy) == 0:
            continue
        gaps = np.diff(yy)
        if len(gaps) and gaps.max() > 3:
            seg = max(np.split(yy, np.where(gaps > 3)[0] + 1), key=len)
        else:
            seg = yy
        if seg.max() - seg.min() <= thin_max:
            cols.append(x); ys.append(float(np.median(seg)))
    return np.array(cols, float), np.array(ys, float)


def reconstruct(mask, L, R, thin_max=8):
    xs, ys = column_trace(mask, L, R, thin_max)
    if len(xs) < 5:
        cols, yv = [], []
        for x in range(L, R + 1):
            yy = np.where(mask[:, x])[0]
            if len(yy):
                cols.append(x); yv.append(float(np.median(yy)))
        xs, ys = np.array(cols, float), np.array(yv, float)
    if len(xs) < 3:
        return None
    if xs.min() > L + 1:                                    # anchor the origin to its own top
        xs = np.concatenate([[L], xs]); ys = np.concatenate([[ys[0]], ys])
    xs, idx = np.unique(xs, return_index=True); ys = ys[idx]
    iso = IsotonicRegression(increasing=True, out_of_bounds="clip")
    ys = iso.fit(xs, ys).predict(xs)
    return xs, ys


def step_polyline(xs, ys):
    pts = []
    for i in range(len(xs)):
        if i == 0:
            pts.append((xs[i], ys[i]))
        else:
            pts.append((xs[i], ys[i - 1]))
            pts.append((xs[i], ys[i]))
    return np.array(np.round(pts), int)


# --------------------------------------------------------------------------- #
# numbers at risk
# --------------------------------------------------------------------------- #
def extract_at_risk(gray, L, R, B, xcal):
    H, W = gray.shape
    y0 = B + 1
    region = gray[y0:H, 0:W]
    if region.size == 0:
        return []
    toks = []
    for sc in (2, 3):
        for ps in (6, 11):
            for t in _ocr_tokens(region, sc, ps):
                toks.append(dict(text=t["text"], xc=t["x"] + t["w"] / 2,
                                 yc=t["y"] + t["h"] / 2 + y0, conf=t["conf"]))
    if not toks:
        return []
    ys = np.array(sorted(set(round(t["yc"]) for t in toks)), float)
    rows = []; cur = [ys[0]]
    for v in ys[1:]:
        if v - cur[-1] <= 12:
            cur.append(v)
        else:
            rows.append((min(cur), max(cur))); cur = [v]
    rows.append((min(cur), max(cur)))

    axis_vals = set(round(v) for v, _ in xcal["ticks"]) if xcal else set()
    out = []
    for (ra, rb) in rows:
        rt = [t for t in toks if ra - 6 <= t["yc"] <= rb + 6]
        nums, labels = [], []
        for t in rt:
            cs = _clean_num(t["text"])
            if cs and _NUM.match(cs) and t["xc"] >= L - 5:
                nums.append((t["xc"], int(float(cs))))
            elif not _NUM.match(t["text"].replace(".", "")):
                labels.append(t["text"])
        nums.sort()
        merged = []
        for xc, v in nums:
            if merged and xc - merged[-1][0] < 8:
                merged[-1] = (xc, v)
            else:
                merged.append((xc, v))
        vals = [v for _, v in merged]; xcs = [xc for xc, _ in merged]
        if len(vals) < 4:
            continue
        times = [round(xcal["m"] * xc + xcal["c"], 2) for xc in xcs] if xcal else None
        if times is not None:                               # X-axis label row
            rng = max(max(times) - min(times), 1.0)
            if np.mean([abs(v - t) for v, t in zip(vals, times)]) <= 0.06 * rng:
                continue
        if axis_vals and set(round(v) for v in vals) <= axis_vals:
            continue
        # Numbers-at-risk decrease left to right and begin at the cohort size.
        # Anchor at the maximum and keep the non-increasing run so an OCR
        # intrusion (e.g. a stray "10" from a nearby caption) cannot shift the
        # row and misalign it against the other arm.
        if len(vals) >= 4:
            mi = int(np.argmax(vals))
            kx, kv = [xcs[mi]], [vals[mi]]
            for x, v in zip(xcs[mi + 1:], vals[mi + 1:]):
                if v <= kv[-1]:
                    kx.append(x); kv.append(v)
            if len(kv) >= 4:
                xcs, vals = kx, kv
                times = [round(xcal["m"] * xc + xcal["c"], 2) for xc in xcs] if xcal else None
        seen = []
        for w in labels:                                    # dedup labels (multiple scales)
            wl = w.lower()
            if wl not in [s.lower() for s in seen]:
                seen.append(w)
        label = " ".join(seen[:3]) if seen else f"row@{int(ra)}"
        out.append(dict(label=label, values=vals, times=times, y=int((ra + rb) / 2)))
    return out


# --------------------------------------------------------------------------- #
# render
# --------------------------------------------------------------------------- #
def render_overlay(rgb, curves):
    vis = rgb.copy()
    for pts, _ in curves:
        cv2.polylines(vis, [pts.reshape(-1, 1, 2)], False, (255, 0, 0), 2, cv2.LINE_AA)
    return vis


def render_clean(shape, box, curves, xcal, ycal):
    H, W = shape[:2]
    L, R, T, B = box
    out = np.full((H, W, 3), 255, np.uint8)
    for pts, col in curves:
        cv2.polylines(out, [pts.reshape(-1, 1, 2)], False, col, 2, cv2.LINE_AA)
    cv2.line(out, (L, T), (L, B), (0, 0, 0), 2)
    cv2.line(out, (L, B), (R, B), (0, 0, 0), 2)
    font = cv2.FONT_HERSHEY_SIMPLEX
    if xcal:
        for v, px in xcal["ticks"]:
            px = int(px)
            cv2.line(out, (px, B), (px, B + 6), (0, 0, 0), 1)
            s = f"{v:g}"; (tw, th), _ = cv2.getTextSize(s, font, 0.45, 1)
            cv2.putText(out, s, (px - tw // 2, B + 8 + th), font, 0.45, (0, 0, 0), 1, cv2.LINE_AA)
    if ycal:
        for v, py in ycal["ticks"]:
            py = int(py)
            cv2.line(out, (L - 6, py), (L, py), (0, 0, 0), 1)
            s = f"{v:g}"; (tw, th), _ = cv2.getTextSize(s, font, 0.45, 1)
            cv2.putText(out, s, (L - 10 - tw, py + th // 2), font, 0.45, (0, 0, 0), 1, cv2.LINE_AA)
    return out


# --------------------------------------------------------------------------- #
def digitize(path, prefix, n_colors=2, color_tol=20, verbose=True):
    bgr = cv2.imread(path)
    if bgr is None:
        raise FileNotFoundError(path)
    rgb = cv2.cvtColor(bgr, cv2.COLOR_BGR2RGB)
    lab = cv2.cvtColor(bgr, cv2.COLOR_BGR2LAB)
    gray = cv2.cvtColor(bgr, cv2.COLOR_BGR2GRAY)
    H, W = gray.shape
    colorful = colorful_mask(rgb)

    L, R, T0, B0, L_axis = detect_box(gray, colorful)

    yband = max(30, int(.13 * W))
    ycal, fraction = calibrate_y(gray, L_axis, T0, yband)
    if ycal:
        tmax = max(v for v, _ in ycal["ticks"])
        topval = max(1.0 if fraction else 100.0, tmax)
        T = int(round((topval - ycal["c"]) / ycal["m"]))
        B = int(round((0.0 - ycal["c"]) / ycal["m"]))
        T = max(0, min(T, H - 2)); B = max(T + 10, min(B, H - 1))
    else:
        T, B = T0, B0

    interior = np.zeros((H, W), bool)
    interior[T:B + 1, L:R + 1] = True
    xcal = calibrate_x(gray, L, R, B, int(.10 * H))

    txt = ocr_text_mask(gray, rgb)

    centroids = find_colors(lab, colorful, interior, k=n_colors)
    curves = []
    for cen in centroids:
        m = line_mask_for_color(rgb, lab, cen, colorful, interior, tol=color_tol)
        m = m & ~txt                                        # remove colored labels
        cu = m.astype(np.uint8)
        n, labc, st, _ = cv2.connectedComponentsWithStats(cu, 8)
        clean = np.zeros_like(cu)
        for i in range(1, n):
            if st[i, cv2.CC_STAT_AREA] >= 3:
                clean[labc == i] = 1
        m = clean.astype(bool)
        rec = reconstruct(m, L, R)
        if rec is None:
            continue
        xs, ys = rec
        col = tuple(int(v) for v in np.median(rgb[m], axis=0))
        curves.append((step_polyline(xs, ys), col, xs, ys, cen))

    # Neutral (gray) arm: invisible to the color clustering above. Only sought when
    # a colored arm is missing, and accepted only when it forms a wide, convincing
    # trace -- this keeps gray gridlines and text speckle from becoming a curve.
    gm = neutral_line_mask(rgb, gray, colorful, interior, txt, (L, R, T, B)) \
        if len(curves) < n_colors else np.zeros((H, W), bool)
    gcols = np.where(gm.any(0))[0]
    if gm.sum() >= 30 and len(gcols) >= 0.45 * max(1, R - L):
        rec = reconstruct(gm, L, R)
        if rec is not None:
            xs, ys = rec
            col = tuple(int(v) for v in np.median(rgb[gm], axis=0))
            curves.append((step_polyline(xs, ys), col, xs, ys,
                           np.array([128.0, 128.0], np.float32)))

    at_risk = extract_at_risk(gray, L, R, B, xcal)

    overlay = render_overlay(rgb, [(p, c) for p, c, *_ in curves])
    clean_img = render_clean((H, W), (L, R, T, B),
                             [(p, c) for p, c, *_ in curves], xcal, ycal)
    cv2.imwrite(prefix + "_overlay.png", cv2.cvtColor(overlay, cv2.COLOR_RGB2BGR))
    cv2.imwrite(prefix + "_clean.png", cv2.cvtColor(clean_img, cv2.COLOR_RGB2BGR))

    with open(prefix + ".csv", "w") as f:
        f.write("curve,x_px,y_px,x_val,y_val\n")
        for i, (pts, col, xs, ys, cen) in enumerate(curves):
            nm = f"curve{i+1}"
            for x, y in zip(xs, ys):
                xv = xcal["m"] * x + xcal["c"] if xcal else float("nan")
                yv = ycal["m"] * y + ycal["c"] if ycal else float("nan")
                f.write(f"{nm},{int(x)},{int(round(y))},{xv:.3f},{yv:.4f}\n")

    meta = dict(box=[L, R, T, B], fraction=bool(fraction),
                xcal=None if not xcal else dict(m=xcal["m"], c=xcal["c"], ticks=xcal["ticks"]),
                ycal=None if not ycal else dict(m=ycal["m"], c=ycal["c"], ticks=ycal["ticks"]),
                n_curves=len(curves), at_risk=at_risk)
    with open(prefix + "_meta.json", "w") as f:
        json.dump(meta, f, indent=2)

    if verbose:
        print(f"[{os.path.basename(path)}]  H,W={H},{W}")
        print(f"  box L,R,T,B = {L},{R},{T},{B}   fraction={fraction}")
        print(f"  xcal ticks: {None if not xcal else [round(t[0],2) for t in xcal['ticks']]}")
        print(f"  ycal ticks: {None if not ycal else [round(t[0],2) for t in ycal['ticks']]}")
        print(f"  curves detected: {len(curves)}")
        for r in at_risk:
            print(f"  at-risk [{r['label']}]: {r['values']}  (times {r['times']})")
    return meta


if __name__ == "__main__":
    src = sys.argv[1]
    pre = sys.argv[2] if len(sys.argv) > 2 else "out"
    digitize(src, pre)