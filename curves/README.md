# Curve archive

A record of real published Kaplan-Meier figures and the result the digitizer is
expected to produce for each one. The archive exists so that a change made to
handle one figure is checked against every figure that already worked.

```
curves/
  manifest.json     versioned: one record per figure, with its expected result
  figures/          the images, held locally and not redistributed
```

`manifest.json` is part of the repository. The figures are published material, so
they are not committed; each record instead carries the SHA-256 of the image it
was taken from, which identifies the exact file without reproducing it. A record
whose image is not present locally is skipped by the suite rather than failed, so
a fresh clone runs cleanly and grows coverage as figures are added.

Run the suite from the repository root:

```
python tests/curve_regression.py            # check every figure held locally
python tests/curve_regression.py -k titan   # only ids containing "titan"
```

or, from R, without having to locate the Python environment yourself:

```r
source("tests/curve_regression.R")
```

The run reports one line per figure and exits with a non-zero status if a figure
recorded as passing no longer is.

## What is compared

Pixel-exact comparison would fail on every harmless refactor, so the archive pins
the quantities the analysis downstream actually consumes:

| Recorded | Why |
|----------|-----|
| Number of curves recovered, and the arm count the two-arm guard sees | A dropped or invented arm is the failure that matters most |
| The engine's own self-check (`fit_ok`) and worst off-mask score | Catches a trace that stops following its curve |
| Calibrated survival of each curve on a fixed grid of times | The digitized data itself, compared within a tolerance in percentage points |
| The numbers-at-risk rows the app would seat as the two arms | These drive the reconstruction, so a change here changes the results |

Tolerances live at the top of `manifest.json`.

`expect` records **what the engine currently produces**, not verified ground
truth. Its purpose is to detect change. Where the engine is known to be imperfect
on a figure, that imperfection is part of the record, and the run will flag it the
day it changes so it can be reviewed.

## Status of a figure

| Status | Meaning |
|--------|---------|
| `passing` | The trace was inspected and is faithful. A departure fails the run. |
| `known-failure` | The engine does not handle this figure yet. It is replayed on every run but does not fail it; when it starts matching its record the run says so, and it can be promoted. |

Reported figures that the engine gets wrong belong here as `known-failure`. They
document the frontier and make sure a later fix is measured against something.

## Adding a figure

```
python tests/curve_regression.py --add path/to/figure.png \
    --id descriptive-kebab-case-id \
    --source "Journal, article, figure number" \
    --traits "coloured arm plus neutral arm,confidence bands" \
    --status known-failure
```

This copies the image into `figures/`, records its checksum and the current
engine output, and appends the record to the manifest. Inspect the overlay the
digitizer produces before changing the status to `passing`.

`--traits` is free text describing what makes the figure hard — which arms are
coloured, whether there are confidence bands, in-plot labels, dashed reference
lines, gridlines, an unusual at-risk table. It is what makes the archive useful
when deciding where a new failure fits.

## Re-recording expectations

After a deliberate improvement, refresh the affected records:

```
python tests/curve_regression.py --update -k some-figure-id
```

Only do this once the new output has been inspected: `--update` accepts whatever
the engine currently produces, so running it blindly erases the very evidence the
archive exists to keep.

## Obtaining the figures

Every record names its source, so the figures can be retrieved from the original
articles. To confirm a copy matches the one a record was taken from:

```
python -c "import hashlib,sys; print(hashlib.sha256(open(sys.argv[1],'rb').read()).hexdigest())" figure.png
```

A copy that hashes differently — a re-render, a crop, a different resolution — is
still worth adding, but as its own record: the engine's behaviour genuinely
depends on those details.
