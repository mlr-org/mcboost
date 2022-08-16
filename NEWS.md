# mcboost (development version)

# mcboost 0.4.2
* Removed new functionality for survival tasks added in `0.4.0`. 
  A dependency, `mlr3proba` was removed from CRAN for now.
  The functionality will be added back when `mlr3proba` is re-introduced to CRAN.
  Users who wish to use `mcboost` for `survival` are adviced to use version `0.4.1` usetogether with the GitHub version of `mlr3proba`.
* Improved stability of unit tests and example checks on CRAN.

# mcboost 0.4.1
* Fixed unit error in unit tests that led to non-passing unit tests with new mlr3proba version.

# mcboost 0.4.0
* [Experimental] mcboost now has experimental support for *survival* tasks.
  See `MCBoostSurv` and the corresponding vignette "MCBoostSurv - Basics" for more information.
* We have published an article about mcboost in the Journal of Open Source Software: "https://joss.theoj.org/papers/10.21105/joss.03453". See citation("mcboost") for the citation info.


# mcboost 0.3.3
* Auditors can now also update weights if correlations are negative by switching the sign of the update direction as intended in the paper.
* Minor adaptions to improve stability of unit tests

# mcboost 0.3.2
* Minor adpations to improve stability of unit tests

# mcboost 0.3.1

* Fixed a bug for additive weight updates, were updates went
  in the wrong direction.
* Added new parameter `eval_fulldata` that allows to compute
  auditor effect across the full sample (as opposed to the bucket).

# mcboost 0.3.0

* First CRAN-ready version of the package.
* Added a `NEWS.md` file to track changes to the package.
