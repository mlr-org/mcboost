# mcboost (development version)

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
