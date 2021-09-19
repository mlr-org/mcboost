# installs dependencies, runs R CMD check, runs covr::codecov()
do_package_checks()

if (ci_on_ghactions() && ci_has_env("BUILD_PKGDOWN")) {
  # creates pkgdown site and pushes to gh-pages branch
  # only for the runner with the "BUILD_PKGDOWN" env var set
  do_pkgdown()
}

library(mlr3proba)
library(paradox)
library(mlr3learners)
library(mlr3tuning)
library(mlr3pipelines)
library(mlr3)

pipe =   po("imputemedian") %>>% po("removeconstants") %>>% po("encode", param_vals = list(method="one-hot"))

task = tsk("rats")


## name can't be changed, distr is output
lrn = as_learner(ppl("distrcompositor",
               learner = as_learner(pipe %>>% lrn("surv.xgboost", id = "xgb"))))
lrn$id = "xgb"
lrn$param_set
lrn$train(task)
ps_xgb = ps(imputemedian.removeconstants.encode.xgb.xgb.max_depth = p_int(lower = 1, upper = 20))
xgb = AutoTuner$new(learner = lrn,
                    resampling = rsmp("cv", folds = 10),
                    measure = msr("surv.graf"),
                    terminator =  trm("evals", n_evals = 1), #higher in real
                    tuner = tnr("random_search"),
                    search_space = ps_xgb)
xgb$train(task)


#name can be change, but distr is not the output?!
distr = po("compose_distr", param_vals = list(form = "aft", overwrite = TRUE))
g = pipe %>>% lrn("surv.xgboost", id = "xgb") %>>% distr

g$predict_type = "distr"
lrn$id = "xgb"
lrn$param_set
lrn$train(task)
ps_xgb = ps(xgb.max_depth = p_int(lower = 1, upper = 20))
xgb = AutoTuner$new(learner = lrn,
                    resampling = rsmp("cv", folds = 10),
                    measure = msr("surv.graf"),
                    terminator =  trm("evals", n_evals = 1), #higher in real
                    tuner = tnr("random_search"),
                    search_space = ps_xgb)
xgb$train(task)

