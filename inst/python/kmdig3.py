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

    # Degenerate-box guard: if the detected plot area is implausibly short (the
    # vertical axis line was mis-detected as a tick/legend, or the baseline landed
    # too high), fall back to the span of the coloured pixels so we still emit a
    # usable box instead of a sliver near the bottom.
    if (B - ytop) < 0.25 * H or ytop > 0.60 * H:
        ys, xs = np.where(colorful)
        if len(xs) > 50:
            ytop = int(np.percentile(ys, 1)); B = int(np.percentile(ys, 99))
            L_axis = min(L_axis, int(np.percentile(xs, 1)))
            bx1 = max(bx1, int(np.percentile(xs, 99)))
        else:
            ytop, B = int(0.08 * H), int(0.88 * H)
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
    # Scan a deep band: on dense axes the thin label strip OCRs poorly, but the same
    # tick values read cleanly a little lower (in the numbers-at-risk header region).
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
def find_colors(lab, colorful, interior, k=None, kmax=4):
    """Cluster the coloured pixels into ``k`` curve colours (default 2).

    Clusters by hue in ``(a, b)`` (so a line and its translucent confidence band,
    which share a hue, stay in one cluster) and returns each chromatic cluster's
    centroid as ``(L, a, b)`` -- the mean lightness is carried along so two arms of
    the same hue but different lightness (e.g. dark vs light purple) can be split
    by luminance at the masking stage.
    """
    mask = colorful & interior
    pts = lab[mask].astype(np.float32)                     # (n, 3) = L, a, b
    if len(pts) < 20:
        return []
    ab = pts[:, 1:3]
    kk = min(max(1, k if k else 2), max(1, len(np.unique(ab, axis=0))))
    km = KMeans(n_clusters=kk, n_init=5, random_state=0).fit(ab)
    out = []
    for i in range(kk):
        c = km.cluster_centers_[i]
        if abs(c[0] - 128) > 8 or abs(c[1] - 128) > 8:
            sel = km.labels_ == i
            out.append(np.array([float(pts[sel, 0].mean()), c[0], c[1]], np.float32))
    return out


def count_distinct_arms(lab, colorful, interior, min_support=0.15, sep=35.0,
                        hue_tol=0.30, kmax=4):
    """Robustly count how many distinct *coloured* arms the plot really has.

    Independent of the "N Curves" setting, so it can flag 3+ arm figures for
    rejection. Only clusters that are chromatic and hold a large share of the
    coloured pixels (>=15%) are kept; kept clusters are then merged when they
    plainly belong to the same arm, and the number of merged groups is the count.

    Two kept clusters are merged when EITHER they sit close together in (a,b)
    space (Euclidean distance < ``sep`` -- catches a muted line and its
    anti-aliased edge, whose hue angle is noisy near grey) OR they share a hue
    (angular difference < ``hue_tol`` -- catches a saturated line and its
    translucent confidence band / halo, which are the same colour at different
    saturation and can drift well apart in (a,b)). Genuinely different colours
    (e.g. blue vs orange) satisfy neither and stay separate, so true 3+ arm
    figures are still counted as such.
    """
    mask = colorful & interior
    pts = lab[mask][:, 1:3].astype(np.float32)
    n = len(pts)
    if n < 50:
        return 0
    hi = min(kmax, len(np.unique(pts, axis=0)))
    best = 0
    for kk in range(1, hi + 1):
        km = KMeans(n_clusters=kk, n_init=5, random_state=0).fit(pts)
        cs = km.cluster_centers_
        kept = [cs[i] for i in range(kk)
                if (abs(cs[i][0] - 128) > 8 or abs(cs[i][1] - 128) > 8)
                and np.count_nonzero(km.labels_ == i) / n >= min_support]
        m = len(kept)
        parent = list(range(m))
        def find(x):
            while parent[x] != x:
                parent[x] = parent[parent[x]]; x = parent[x]
            return x
        for a in range(m):
            for b in range(a + 1, m):
                eu = np.hypot(kept[a][0] - kept[b][0], kept[a][1] - kept[b][1])
                ha = np.arctan2(kept[a][1] - 128, kept[a][0] - 128)
                hb = np.arctan2(kept[b][1] - 128, kept[b][0] - 128)
                ang = abs((ha - hb + np.pi) % (2 * np.pi) - np.pi)
                if eu < sep or ang < hue_tol:
                    parent[find(a)] = find(b)
        groups = len({find(i) for i in range(m)})
        best = max(best, groups)
    return best


def line_mask_for_color(rgb, lab, centroid, others, colorful, interior, tol=20):
    """Mask the pixels of one curve colour.

    ``centroid`` and ``others`` are ``(L, a, b)`` centroids. A pixel is admitted
    if it is within ``tol`` of this centroid in hue ``(a, b)`` AND this centroid is
    its nearest among all of them in weighted ``(L, a, b)`` space. The luminance
    tie-break separates two same-hue arms (dark vs light purple) that a pure hue
    radius would lump together, while single-hue figures (``others`` empty) behave
    exactly as before.
    """
    Lab = lab.astype(np.float32)
    ab = Lab[:, :, 1:3]
    dist = np.sqrt(((ab - centroid[1:3]) ** 2).sum(2))
    hue = (dist < tol) & colorful & interior
    if others:
        wL = 0.5                                            # weight L so it counts with (a,b)
        def dlab(cen):
            return ((wL * (Lab[:, :, 0] - cen[0])) ** 2
                    + (ab[:, :, 0] - cen[1]) ** 2 + (ab[:, :, 1] - cen[2]) ** 2)
        dme = dlab(centroid)
        win = np.ones(hue.shape, bool)
        for o in others:
            win &= dme <= dlab(o)
        hue &= win
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


def _clean_components(m, span, high):
    """Keep only elongated (line-like) connected components of a boolean mask."""
    n, labc, st, _ = cv2.connectedComponentsWithStats(m.astype(np.uint8), 8)
    clean = np.zeros(m.shape, np.uint8)
    for i in range(1, n):
        w = st[i, cv2.CC_STAT_WIDTH]; h = st[i, cv2.CC_STAT_HEIGHT]
        if st[i, cv2.CC_STAT_AREA] >= 3 and (w >= 0.04 * span or h >= 0.04 * high):
            clean[labc == i] = 1
    return clean.astype(bool)


def _drop_boxes(mask, span, min_area=200, hole_frac=0.5):
    """Remove hollow rectangular components (in-plot legend / annotation boxes).

    A survival curve is an open path -- filling its contour adds almost nothing.
    A boxed legend is a closed rectangle outline whose filled contour is nearly all
    interior. Components that are mostly hollow and not near-full-width are dropped,
    so a legend frame can no longer be traced as a spurious flat curve.
    """
    m = mask.astype(np.uint8)
    n, lab, st, _ = cv2.connectedComponentsWithStats(m, 8)
    out = mask.copy()
    for i in range(1, n):
        area = st[i, cv2.CC_STAT_AREA]
        if area < min_area or st[i, cv2.CC_STAT_WIDTH] >= 0.6 * span:
            continue
        comp = (lab == i).astype(np.uint8)
        cnts, _ = cv2.findContours(comp, cv2.RETR_EXTERNAL, cv2.CHAIN_APPROX_SIMPLE)
        ff = np.zeros_like(comp)
        cv2.drawContours(ff, cnts, -1, 1, -1)
        fa = int(ff.sum())
        if fa > 0 and (fa - area) / fa > hole_frac:
            out[lab == i] = False
    return out


def neutral_curve_masks(rgb, gray, colorful, interior, txt, box, n_needed):
    """Masks for up to ``n_needed`` near-neutral (grayscale) curves.

    Many oncology figures are black & white: one arm solid black, the other gray.
    Both carry no chroma, so the colour clustering misses them. We take the neutral,
    off-white, non-text pixels inside the plot (minus the axis frame) and split them
    by *lightness* into at most ``n_needed`` shades -- so a black curve (~60) and a
    gray curve (~175) become two separate traces, while a single-shade arm stays one.
    """
    L, R, T, B = box
    if n_needed < 1:
        return []
    sat = rgb.astype(int).max(2) - rgb.astype(int).min(2)
    g = gray.astype(int)
    # any neutral, non-white pixel (includes a solid-black arm); the axis frame is
    # removed by the margins below and text by the OCR mask.
    neutral = (sat < 28) & (g < 240) & interior & ~colorful & ~txt
    neutral[max(T, B - 3):, :] = False                     # drop the X-axis baseline
    neutral[:, :min(R, L + 4)] = False                     # drop the Y-axis line
    span, high = max(1, R - L), max(1, B - T)
    # Strip near-full-width horizontal gridlines: they are neutral and line-like, so
    # they survive component cleaning and pull the per-column trace toward round-number
    # levels. A genuine Kaplan-Meier plateau never spans the whole plot width, so an
    # opening with a wide horizontal kernel isolates only gridlines to subtract.
    kw = max(15, int(0.7 * span))
    horiz = cv2.morphologyEx(neutral.astype(np.uint8), cv2.MORPH_OPEN,
                             cv2.getStructuringElement(cv2.MORPH_RECT, (kw, 1)))
    neutral = neutral & ~horiz.astype(bool)
    neutral = _drop_boxes(neutral, span)                   # strip in-plot legend boxes
    if neutral.sum() < 50:
        return []

    masks = [neutral]
    if n_needed >= 2:
        gv = g[neutral].reshape(-1, 1).astype(np.float32)
        if len(np.unique(gv)) > 1:
            km = KMeans(n_clusters=2, n_init=5, random_state=0).fit(gv)
            c0, c1 = sorted(float(x) for x in km.cluster_centers_.ravel())
            if c1 - c0 >= 50:                              # two distinct shades (black vs gray)
                thr = 0.5 * (c0 + c1)
                masks = [neutral & (g <= thr), neutral & (g > thr)]
    return [mm for mm in (_clean_components(m, span, high) for m in masks[:n_needed]) if mm.any()]


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
def _split_merged_count(digits, k, hi):
    """Split a merged at-risk cell (e.g. '141110') into k non-increasing integers
    with balanced digit lengths, each <= hi. Returns list[int] or None.

    Numbers at risk only ever decrease, so among all digit partitions there is
    almost always a single one that is both monotone and balanced -- that is the
    correct split."""
    from itertools import combinations
    n = len(digits)
    if k <= 1 or n < k:
        return None
    best = None
    for cuts in combinations(range(1, n), k - 1):
        idx = [0] + list(cuts) + [n]
        parts = [digits[idx[i]:idx[i + 1]] for i in range(k)]
        if any(len(p) > 1 and p[0] == "0" for p in parts):      # no leading zeros
            continue
        vals = [int(p) for p in parts]
        if any(vals[i] < vals[i + 1] for i in range(k - 1)):     # must be non-increasing
            continue
        if hi is not None and vals[0] > hi:
            continue
        lens = [len(p) for p in parts]
        score = (max(lens) - min(lens), -min(vals))              # prefer balanced lengths
        if best is None or score < best[0]:
            best = (score, vals)
    return best[1] if best else None


def _ocr_cell(cell):
    """OCR one isolated at-risk cell -> int or None (adjacent cells can't merge)."""
    if cell.size == 0 or min(cell.shape[:2]) < 4:
        return None
    best = None
    for sc in (3, 4):
        big = cv2.resize(cell, None, fx=sc, fy=sc, interpolation=cv2.INTER_CUBIC)
        d = pytesseract.image_to_data(
            big, config="--psm 7 -c tessedit_char_whitelist=0123456789", output_type=Output.DICT)
        for i in range(len(d["text"])):
            s = _clean_num(d["text"][i])
            if s and _NUM.match(s) and "." not in s:
                try:
                    conf = float(d["conf"][i])
                except ValueError:
                    conf = -1.0
                if best is None or conf > best[1]:
                    best = (int(s), conf)
    return best[0] if best else None


def _atrisk_quality(vals, expected_n):
    """Higher is better: rewards monotone non-increasing rows of the expected
    column count with no absurd values."""
    v = [x for x in vals if x is not None]
    if len(v) < 4:
        return -1e9
    mono = sum(1 for i in range(len(v) - 1) if v[i] >= v[i + 1]) / (len(v) - 1)
    count_pen = abs(len(v) - expected_n) if expected_n else 0
    huge = sum(1 for x in v if x > 100000)
    return 12.0 * mono - 2.0 * count_pen - 6.0 * huge + 0.3 * len(v)


def _atrisk_anchored(gray, y0, y1, xcal, L, R):
    """Strategy B: OCR each column cell at the axis-tick x positions so adjacent
    numbers can never merge. Returns (values, xcs) on the tick grid, or None."""
    H, W = gray.shape
    m, c = xcal["m"], xcal["c"]
    tv = sorted(v for v, _ in xcal["ticks"])
    if m <= 0 or len(tv) < 2:
        return None
    step = min((b - a for a, b in zip(tv, tv[1:]) if b > a), default=0)
    if step <= 0:
        return None
    vmax = m * R + c
    grid = [i * step for i in range(int(round(vmax / step)) + 1) if i * step <= vmax + 0.5 * step]
    if len(grid) < 4:
        return None
    pitch = step / m
    vals, xcs = [], []
    for v in grid:
        cx = (v - c) / m
        x0 = max(0, int(cx - 0.45 * pitch)); x1 = min(W, int(cx + 0.45 * pitch))
        vals.append(_ocr_cell(gray[int(y0):int(y1) + 1, x0:x1]) if x1 > x0 else None)
        xcs.append(cx)
    return vals, xcs


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
                toks.append(dict(text=t["text"], xc=t["x"] + t["w"] / 2, w=t["w"],
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
            if cs and _NUM.match(cs) and "." not in cs and t["xc"] >= L - 5:
                nums.append((t["xc"], int(cs), t.get("w", 0), cs))
            elif not _NUM.match(t["text"].replace(".", "")):
                labels.append(t["text"])
        nums.sort()
        # Repair OCR-merged cells: a token spanning ~k column pitches holds k numbers.
        # Split its digits into a non-increasing, balanced run (only for clearly wide,
        # >=4-digit tokens, so clean single cells are never touched).
        xcs_all = [xc for xc, *_ in nums]
        pitch = float(np.median(np.diff(xcs_all))) if len(xcs_all) >= 2 else 0.0
        expanded, hi = [], None
        for xc, v, w, cs in nums:
            k = int(round(w / pitch)) if pitch > 0 else 1
            parts = _split_merged_count(cs, min(k, len(cs)), hi) if (k >= 2 and len(cs) >= 4) else None
            if parts:
                left = xc - w / 2.0
                for i, pv in enumerate(parts):
                    expanded.append((left + (i + 0.5) * w / len(parts), pv)); hi = pv
            else:
                expanded.append((xc, v)); hi = v
        merged = []
        for xc, v in expanded:
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
        out.append(dict(label=label, values=vals, times=times, y=int((ra + rb) / 2),
                        _y0=ra, _y1=rb))

    # Ensemble refinement: a row that reads cleanly (monotone, expected column
    # count) is kept as-is; a poor one is re-read with the tick-anchored cell OCR
    # and the higher-scoring version wins. Good tables are never altered.
    if xcal:
        m, c = xcal["m"], xcal["c"]
        tv = sorted(v for v, _ in xcal["ticks"])
        step = min((b - a for a, b in zip(tv, tv[1:]) if b > a), default=0)
        expected_n = int(round((m * R + c) / step)) + 1 if step > 0 else 0
        for r in out:
            qa = _atrisk_quality(r["values"], expected_n)
            if qa >= 11.5:                                  # already essentially perfect
                continue
            # Re-read on a fixed band centred on the row: Strategy A may have split a
            # printed row across two y-clusters, leaving a partial band.
            cy = 0.5 * (r["_y0"] + r["_y1"])
            half = min(16, max(11, int((r["_y1"] - r["_y0"]) / 2) + 7))
            anc = _atrisk_anchored(gray, cy - half, cy + half, xcal, L, R)
            if not anc:
                continue
            bvals, bxcs = anc
            if _atrisk_quality(bvals, expected_n) > qa:
                keep = [(x, v) for x, v in zip(bxcs, bvals) if v is not None]
                if len(keep) >= 4:
                    r["values"] = [v for _, v in keep]
                    r["times"] = [round(m * x + c, 2) for x, _ in keep]
    for r in out:
        r.pop("_y0", None); r.pop("_y1", None)
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
def digitize(path, prefix, n_colors=None, color_tol=20, verbose=True):
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
    if ycal and not ycal.get("m"):
        ycal = None                                        # degenerate slope -> no calibration
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

    # This tool supports two-arm studies only. Count the real coloured arms up front
    # so a 3+ arm figure can be rejected by the caller.
    n_arms = count_distinct_arms(lab, colorful, interior)

    centroids = find_colors(lab, colorful, interior, k=n_colors)   # n_colors from "N Curves"
    curves = []
    for idx, cen in enumerate(centroids):
        others = [c for j, c in enumerate(centroids) if j != idx]
        m = line_mask_for_color(rgb, lab, cen, others, colorful, interior, tol=color_tol)
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

    # Neutral (grayscale) arms: invisible to the colour clustering above. Recover up
    # to the missing count -- this covers black-and-white figures (black + gray) as
    # well as one colour + one gray arm. Each is accepted only when it forms a wide,
    # descending trace, so gridlines/text speckle never become an arm.
    want = n_colors if n_colors else 2
    for gm in neutral_curve_masks(rgb, gray, colorful, interior, txt, (L, R, T, B),
                                  want - len(curves)):
        gcols = np.where(gm.any(0))[0]
        if gm.sum() < 30 or len(gcols) < 0.45 * max(1, R - L):
            continue
        rec = reconstruct(gm, L, R)
        if rec is None:
            continue
        xs, ys = rec
        # A gridline reconstructs perfectly flat (~0 variation); any real survival
        # arm has steps, so a small threshold separates them while still admitting
        # near-flat high-survival arms.
        if ys.max() - ys.min() >= 0.02 * max(1, B - T):
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
                n_curves=len(curves), arms_detected=int(n_arms),
                too_many_arms=bool(n_arms >= 3), at_risk=at_risk)
    with open(prefix + "_meta.json", "w") as f:
        json.dump(meta, f, indent=2)

    if verbose:
        print(f"[{os.path.basename(path)}]  H,W={H},{W}")
        print(f"  box L,R,T,B = {L},{R},{T},{B}   fraction={fraction}")
        print(f"  xcal ticks: {None if not xcal else [round(t[0],2) for t in xcal['ticks']]}")
        print(f"  ycal ticks: {None if not ycal else [round(t[0],2) for t in ycal['ticks']]}")
        print(f"  curves detected: {len(curves)}   arms_detected: {n_arms}   too_many_arms: {n_arms >= 3}")
        for r in at_risk:
            print(f"  at-risk [{r['label']}]: {r['values']}  (times {r['times']})")
    return meta


if __name__ == "__main__":
    src = sys.argv[1]
    pre = sys.argv[2] if len(sys.argv) > 2 else "out"
    nc  = int(sys.argv[3]) if len(sys.argv) > 3 and sys.argv[3].isdigit() else None
    digitize(src, pre, n_colors=nc)