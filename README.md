
# mcboost

<!-- badges: start -->
<!-- badges: end -->

**mcboost** implements Multi-Accuracy Boosting (Kim et al., 2018) for R.
See the [paper](https://arxiv.org/pdf/1805.12317.pdf) for more information.

## Installation

You can install the released version of mcboost from Github with:

``` r
remotes::install_github("pfistfl/mcboost")
```

## Example

In this simple example, our goal is to improve calibration
for a `initial predictor`, e.g. an ML algorithm trained on
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

To provide an example, we assume that we have a already trained learner `l` which we train below.
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


We can now run Multi-Accuracy boosting by instantiating the object and calling the `multicalibrate` method.
Note, that typically, we would use multi-calibration on a smaller validation set!

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

The `mcboost` **vignette** has a lot of interesting showcases for applying **mcboost** for several use-cases.


## Contributing
