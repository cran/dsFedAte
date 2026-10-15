# dsFedAte 0.1.0

Initial CRAN release.

## Features

* `localAteDS()` aggregate function computes the local ATE estimate and,
  optionally, the variance statistics (arm-specific residual sums of squares
  and within-server projected scatter) needed for the delta-method variance.
  Set `compute_variance = FALSE` to limit disclosure to the three scalars
  strictly needed for the point estimate (`n_k`, `n_k_1`, `tau_k`). The
  `heterogeneous` argument (default `TRUE`) additionally returns covariate
  column sums, allowing the client to assemble the global covariate mean
  for the leverage-corrected variance estimator.

* `addStudyIdDS()` assign function appends K-1 integer study-dummy columns
  to a server-side data frame, supporting the adjusted generalised difference
  estimator when study membership directly shifts the outcome.

* All functions enforce DataSHIELD disclosure controls: privacy control
  level is checked via `dsBase::checkPermissivePrivacyControlLevel()` and
  minimum subset size is validated against `nfilter.subset` from
  `dsBase::listDisclosureSettingsDS()`.
