# mcboost

<!-- badges: start -->
[![tic](https://github.com/pfistfl/mcboost/workflows/tic/badge.svg?branch=main)](https://github.com/pfistfl/mcboost/actions)
<!-- badges: end -->

**mcboost** implements Multi-Calibration (Hebert Johnson et al., 2018) and Multi-Accuracy Boosting (Kim et al., 2019) for calibration of a machine learning model's prediction. Multi-Calibration works best in scenarios where the underlying data & labels are un-biased but a bias is introduced within the algoritm's fitting procedure. This is often the case, e.g. when an algorithm fits a majority population while ignoring or under-fitting minority populations.

Literature:
  - [Hebert-Johnson et al., 2018](http://proceedings.mlr.press/v80/hebert-johnson18a.html)
  - [Kim et al., 2019](https://arxiv.org/pdf/1805.12317.pdf)

## Installation

You can install the released version of mcboost from **Github** with:

``` r
remotes::install_github("pfistfl/mcboost")
```

## Example

In this simple example, our goal is to improve calibration
for an `initial predictor`, e.g. a ML algorithm trained on
an initial task.

``` r
library(mcboost)
library(mlr3)
```

First we set up an example dataset:

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

We can now run Multi-Accuracy Boosting by instantiating the object and calling the `multicalibrate` method.
Note, that typically, we would use Multi-Calibration on a smaller validation set!

```r
  mc = MCBoost$new(init_predictor = init_predictor)
  mc$multicalibrate(train_data, train_labels)
```

and alternatively **predict** on new data.

```r
tstid = setdiff(tsk$row_ids, tid) # held-out data
test_data = tsk$data(cols = tsk$feature_names, rows = tstid)
mc$predict_probs(test_data)
```


## Further Examples

`mcboost` allows for Multi-Calibration as described in [Hebert-Johnson et al., 2017](http://proceedings.mlr.press/v80/hebert-johnson18a) as well as Multi-Accuracy calibration as described in [Kim et al., 2018](https://arxiv.org/pdf/1805.12317.pdf).

The `mcboost` vignettes [**Basics and Extensions**](https://pfistfl.github.io/mcboost/articles/mcboost_basics_extensions.html) and [**Health Survey Example**](https://pfistfl.github.io/mcboost/articles/mcboost_example.html) demonstrate a lot of interesting showcases for applying **mcboost**.


## Contributing

This R package is licensed under the LGPL-3.
If you encounter problems using this software (lack of documentation, misleading or wrong documentation, unexpected behaviour, bugs, …) or just want to suggest features, please open an issue in the issue tracker.
Pull requests are welcome and will be included at the discretion of the maintainers.

As this project is developed with [mlr3's](https://github.com/mlr-org/mlr3/) style guide in mind, the following ressources can be helpful
to individuals wishing to contribute: Please consult the [wiki](https://github.com/mlr-org/mlr3/wiki/) for a [style guide](https://github.com/mlr-org/mlr3/wiki/Style-Guide), a [roxygen guide](https://github.com/mlr-org/mlr3/wiki/Roxygen-Guide) and a [pull request guide](https://github.com/mlr-org/mlr3/wiki/PR-Guidelines).

## Citing mcboost

If you use `mcboost`, please cite our package as well as the relevant paper it is based on:

```
  @Manual{mcboost_software,
    title = {mcboost: Implements Multi-Accuracy Boosting (Kim et al., 2018).},
    author = {Florian Pfisterer and Christoph Kern},
    year = {2021},
    note = {R package version 0.1.0},
  }
  # Multi-calibration
  @InProceedings{pmlr-v80-hebert-johnson18a,
    title = {Multicalibration: Calibration for the ({C}omputationally-Identifiable) Masses},
    author = {Hebert-Johnson, Ursula and Kim, Michael and Reingold, Omer and Rothblum, Guy},
    booktitle = {Proceedings of the 35th International Conference on Machine Learning},
    pages = {1939--1948},
    year = {2018},
    editor = {Jennifer Dy and Andreas Krause},
    volume = {80},
    series = {Proceedings of Machine Learning Research},
    address = {Stockholmsmässan, Stockholm Sweden},
    month = {10--15 Jul},
    publisher = {PMLR}
  }
  # Multi-accuracy calibration
  @inproceedings{10.1145/3306618.3314287,
    author = {Kim, Michael P. and Ghorbani, Amirata and Zou, James},
    title = {Multiaccuracy: Black-Box Post-Processing for Fairness in Classification},
    year = {2019},
    isbn = {9781450363242},
    publisher = {Association for Computing Machinery},
    address = {New York, NY, USA},
    url = {https://doi.org/10.1145/3306618.3314287},
    doi = {10.1145/3306618.3314287},
    booktitle = {Proceedings of the 2019 AAAI/ACM Conference on AI, Ethics, and Society},
    pages = {247–254},
    numpages = {8},
    keywords = {fairness, discrimination, post-processing, machine learning},
    location = {Honolulu, HI, USA},
    series = {AIES '19}
  }

```
