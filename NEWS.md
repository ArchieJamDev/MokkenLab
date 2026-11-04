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
