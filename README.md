
# mcboost

<!-- badges: start -->
<!-- badges: end -->

The goal of mcboost is to ...

## Installation

You can install the released version of mcboost from [CRAN](https://CRAN.R-project.org) with:

``` r
remotes::install_github("pfistfl/mcboost")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(mcboost)
library(mlr3)
```


First we set up an example dataset:

```r
  #  Example Data: Sonar Task
  tsk = tsk("sonar")
  data = tsk$data(cols = tsk$feature_names)
  labels = tsk$data(cols = tsk$target_names)[[1]]
```

Then we fit an initial predictor, e.g. using **mlr3**.

```r
  l = lrn("classif.rpart")
  l$train(tsk)

  init_predictor = function(data) {
    # Get response prediction from Learner
    p = l$predict_newdata(data)$response
    # One-hot encode and take first column
    one_hot(p)[,1]
  }
```

Now we can run Multi-Accuracy boosting

```r
  mc = MCBoost$new(init_predictor = init_predictor)
  mc$multicalibrate(data, labels)
```

and **predict** on new data.

```r
mc$predict_probs(data)
```