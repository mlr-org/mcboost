# mcboost

<!-- badges: start -->
[![tic](https://github.com/mlr-org/mcboost/workflows/tic/badge.svg?branch=main)](https://github.com/mlr-org/mcboost/actions)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN Status](https://www.r-pkg.org/badges/version-ago/mcboost)](https://cran.r-project.org/package=mcboost)
[![DOI](https://joss.theoj.org/papers/10.21105/joss.03453/status.svg)](https://doi.org/10.21105/joss.03453)
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Mattermost](https://img.shields.io/badge/chat-mattermost-orange.svg)](https://lmmisld-lmu-stats-slds.srv.mwn.de/mlr_invite/)
<!-- badges: end -->

## What does it do?

**mcboost** implements Multi-Calibration Boosting ([Hebert-Johnson et al., 2018](https://proceedings.mlr.press/v80/hebert-johnson18a.html); [Kim et al., 2019](https://arxiv.org/pdf/1805.12317.pdf)) for the multi-calibration of a machine learning model's prediction. Multi-Calibration works best in scenarios where the underlying data & labels are unbiased but a bias is introduced within the algorithm's fitting procedure. This is often the case, e.g. when an algorithm fits a majority population while ignoring or under-fitting minority populations.

For more information and example, see the package's [website](https://mlr-org.github.io/mcboost/).

More details with respect to usage and the procedures can be found in the package vignettes.

## Installation

The current version can be downloaded from CRAN using:

```r
install.packages("mcboost")
```

You can install the development version of mcboost from **Github** with:

```r
remotes::install_github("mlr-org/mcboost")
```

## Usage

Post-processing with `mcboost` needs three components. We start with an initial prediction model (1) and an auditing algorithm (2) that may be customized by the user. The auditing algorithm then runs Multi-Calibration-Boosting on a labeled auditing dataset (3). The resulting model can be used for obtaining multi-calibrated predictions.

<p align="center">
  <img src="https://github.com/mlr-org/mcboost/raw/main/paper/MCBoost.png" />
</p>

## Example

In this simple example, our goal is to improve calibration
for an `initial predictor`, e.g. a ML algorithm trained on
an initial task.
Internally, `mcboost` often makes use of `mlr3` and learners that come with `mlr3learners`.


``` r
library(mcboost)
library(mlr3)
```

First we set up an example dataset.

```r
  #  Example Data: Sonar Task
  tsk = tsk("sonar")
  tid = sample(tsk$row_ids, 100) # 100 rows for training
  train_data = tsk$data(cols = tsk$feature_names, rows = tid)
  train_labels = tsk$data(cols = tsk$target_names, rows = tid)[[1]]
```

To provide an example, we assume that we have already a learner `l` which we train below.
We can now wrap this initial learner's predict function for use with `mcboost`, since `mcboost` expects the initial model to be specified as a `function` with `data` as input.

```r
  l = lrn("classif.rpart")
  l$train(tsk$clone()$filter(tid))

  init_predictor = function(data) {
    # Get response prediction from Learner
    p = l$predict_newdata(data)$response
    # One-hot encode and take first column
    one_hot(p)
  }
```

We can now run Multi-Calibration Boosting by instantiating the object and calling the `multicalibrate` method.
Note, that typically, we would use Multi-Calibration on a separate validation set!
We furthermore select the auditor model, a `SubpopAuditorFitter`,
in our case a `Decision Tree`:

```r
  mc = MCBoost$new(
    init_predictor = init_predictor,
    auditor_fitter = "TreeAuditorFitter")
  mc$multicalibrate(train_data, train_labels)
```

Lastly, we predict on new data.

```r
tstid = setdiff(tsk$row_ids, tid) # held-out data
test_data = tsk$data(cols = tsk$feature_names, rows = tstid)
mc$predict_probs(test_data)
```

### Multi-Calibration

While `mcboost` in its defaults implements Multi-Accuracy ([Kim et al., 2019](https://arxiv.org/pdf/1805.12317.pdf)),
it can also multi-calibrate predictors ([Hebert-Johnson et al., 2018](http://proceedings.mlr.press/v80/hebert-johnson18a.html)).
In order to achieve this, we have to set the following hyperparameters:

```r
  mc = MCBoost$new(
    init_predictor = init_predictor,
    auditor_fitter = "TreeAuditorFitter",
    num_buckets = 10,
    multiplicative = FALSE
  )
```

## MCBoost as a PipeOp

`mcboost` can also be used within a `mlr3pipeline` in order to use at the full end-to-end pipeline (in the form of a `GraphLearner`).

```r
  library(mlr3)
  library(mlr3pipelines)
  gr = ppl_mcboost(lrn("classif.rpart"))
  tsk = tsk("sonar")
  tid = sample(1:208, 108)
  gr$train(tsk$clone()$filter(tid))
  gr$predict(tsk$clone()$filter(setdiff(1:208, tid)))
```



## Further Examples

The `mcboost` vignettes [**Basics and Extensions**](https://mlr-org.github.io/mcboost/articles/mcboost_basics_extensions.html) and [**Health Survey Example**](https://mlr-org.github.io/mcboost/articles/mcboost_example.html) demonstrate a lot of interesting showcases for applying `mcboost`.


## Contributing

This R package is licensed under the LGPL-3.
If you encounter problems using this software (lack of documentation, misleading or wrong documentation, unexpected behaviour, bugs, …) or just want to suggest features, please open an issue in the issue tracker.
Pull requests are welcome and will be included at the discretion of the maintainers.

As this project is developed with [mlr3's](https://github.com/mlr-org/mlr3/) style guide in mind, the following resources can be helpful
to individuals wishing to contribute: Please consult the [wiki](https://github.com/mlr-org/mlr3/wiki/) for a [style guide](https://github.com/mlr-org/mlr3/wiki/Style-Guide), a [roxygen guide](https://github.com/mlr-org/mlr3/wiki/Roxygen-Guide) and a [pull request guide](https://github.com/mlr-org/mlr3/wiki/PR-Guidelines).

### Code of Conduct

Please note that the mcboost project is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.

## Citing mcboost

If you use `mcboost`, please cite our package as well as the two papers it is based on:

```
  @article{pfisterer2021,
    author = {Pfisterer, Florian and Kern, Christoph and Dandl, Susanne and Sun, Matthew and 
    Kim, Michael P. and Bischl, Bernd},
    title = {mcboost: Multi-Calibration Boosting for R},
    journal = {Journal of Open Source Software},
    doi = {10.21105/joss.03453},
    url = {https://doi.org/10.21105/joss.03453},
    year = {2021},
    publisher = {The Open Journal},
    volume = {6},
    number = {64},
    pages = {3453}
  }
  # Multi-Calibration
  @inproceedings{hebert-johnson2018,
    title = {Multicalibration: Calibration for the ({C}omputationally-Identifiable) Masses},
    author = {Hebert-Johnson, Ursula and Kim, Michael P. and Reingold, Omer and Rothblum, Guy},
    booktitle = {Proceedings of the 35th International Conference on Machine Learning},
    pages = {1939--1948},
    year = {2018},
    editor = {Jennifer Dy and Andreas Krause},
    volume = {80},
    series = {Proceedings of Machine Learning Research},
    address = {Stockholmsmässan, Stockholm Sweden},
    publisher = {PMLR}
  }
  # Multi-Accuracy
  @inproceedings{kim2019,
    author = {Kim, Michael P. and Ghorbani, Amirata and Zou, James},
    title = {Multiaccuracy: Black-Box Post-Processing for Fairness in Classification},
    year = {2019},
    isbn = {9781450363242},
    publisher = {Association for Computing Machinery},
    address = {New York, NY, USA},
    url = {https://doi.org/10.1145/3306618.3314287},
    doi = {10.1145/3306618.3314287},
    booktitle = {Proceedings of the 2019 AAAI/ACM Conference on AI, Ethics, and Society},
    pages = {247--254},
    location = {Honolulu, HI, USA},
    series = {AIES '19}
  }
```
