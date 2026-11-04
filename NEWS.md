# MokkenLab 0.1.0 (development), completing the independent audit

* **Statistical cache.** jamovi re-runs an analysis on every option change;
  mokken results are now cached per data and statistical options, so a
  cosmetic change (plot size, palette, report language) no longer recomputes
  them. Invariant item ordering of the 80 TROG items: 27 s on first run,
  about 3 s after a palette change. The cache is capped at 25 MB.
* **Reproducibility record** at the end of every analysis: MokkenLab, mokken
  and R versions, date, seeds, and the equivalent R code with the exact
  mokken calls (checked to reproduce the results in a clean R session).
* **Bootstrap stability of the AISP partition**: the selection is repeated on
  resamples of respondents (of whole clusters, with nested data), with the
  co-membership of every item pair, the share of samples that reproduce the
  whole partition, and a co-membership heat map.
* Negatively worded items listed under Items in every analysis.
* CI on Windows and macOS as well as Linux; actions/checkout v5.
* An entirely empty dataset is tested; licence metadata say GPL (>= 3), as
  every source header.

# MokkenLab 0.1.0 (development), after the independent audit of 2026-11-04

An independent review reproduced the numerical equivalence with mokken and the
test suite, and confirmed several problems. All confirmed ones are fixed and
pinned by `tests/testthat/test-audit.R`:

* **Conditional association** failed ("Not computable") with W indices off,
  the default: `check.ca()` returns a different structure in that case, now
  normalized.
* **Two-level item pairs** split item names at spaces; pairs are now built by
  position, so names with spaces are kept whole.
* **"Not evaluable" is never shown as "No violations"**: assumption checks
  have an explicit not-evaluable state, and the violation profile keeps it.
* **Key findings** now list every section that could not be computed and, in
  Model Assumptions, every check not run.
* **Automated Item Selection** with one lower bound now has a plot (Hi of
  every item within its scale); before, only the sequence mode had one.
* Interpretations made **conditional on the evidence**: a global H, high
  reliability or no flagged violation no longer read as proof of one
  attribute, of an invariant ordering or of local independence (Hemker et
  al., 1997, added for polytomous items).
* The **reversal row** is always shown (also "None"), and the two-level
  analysis states its design and flags very few subjects.
* Several completed data sets are labeled as a **sensitivity check, not
  pooled** inference.
* Weights for Guttman errors explain mokken's internal failure on tied item
  steps; range-preserving intervals are defined at H = 1; LCRC states how its
  number of classes was chosen; norm tables are labeled as sample statistics.
* `validation/mokken_equivalence.R` checks lengths, missing positions and
  finiteness, exits non-zero on any failure, and runs in CI on every push.

# MokkenLab 0.1.0 (development)

First version. Every exported function of mokken 3.1.2 is exposed in four
analyses, with teaching blocks, plain-language interpretation in English and
Spanish, and a shared, fully parameterized graphics layer.

* **Mokken Scale Analysis**: `coefH` (pairs, items, scale; standard errors;
  Wald-based and range-preserving intervals; by group; nested data), `coefZ`,
  `check.reliability`, `check.errors` (with G+ and O+ saved as columns),
  `check.bounds`, `check.norms`.
* **Automated Item Selection**: `aisp` with Mokken's procedure or the genetic
  algorithm (reproducible seed), one lower bound or a sequence, start sets,
  Hi tests and nested data.
* **Model Assumptions**: `check.monotonicity`, `check.iio` (MIIO, MSCPM, IT;
  estimated, listed, clustered or typed order), `check.restscore`,
  `check.pmatrix`, `check.ca`, with the plots of mokken rebuilt in ggplot and
  a violation profile.
* **Two-Level Mokken Scale Analysis**: `MLcoefH`, `MLcoefZ`, `ICC`,
  `MLweight`.
* Data handling shared by all analyses: listwise deletion or `twoway`
  imputation with several completed data sets, declared or observed score
  range, negatively worded items reversed as `recode`.
* Four bundled datasets from mokken: DS14, transitive reasoning, autonomy
  support and TROG.
* Known quirks of mokken 3.1.2 handled explicitly: `coefH` intervals with
  `nice.output = FALSE`, upper bounds outside [-1, 1] in `check.bounds`,
  result columns named after item positions, console output.
* `validation/mokken_equivalence.R`: 20 of 20 numerical checks against mokken
  pass. `tests/testthat/`: 112 expectations pass.
