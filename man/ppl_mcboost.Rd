% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PipelineMCBoost.R
\name{ppl_mcboost}
\alias{ppl_mcboost}
\title{Multi-calibration pipeline}
\usage{
ppl_mcboost(learner = lrn("classif.featureless"), param_vals = list())
}
\arguments{
\item{learner}{(mlr3)\code{\link[mlr3:Learner]{mlr3::Learner}}\cr
Initial learner. Internally wrapped into a \code{PipeOpLearnerCV}
with \code{resampling.method = "insample"} as a default.
All parameters can be adjusted through the resulting Graph's \code{param_set}.
Defaults to \code{lrn("classif.featureless")}.
Note: An initial predictor can also be supplied via the \code{init_predictor} parameter.}

\item{param_vals}{\code{list} \cr
List of parameter values passed on to \code{MCBoost$new}.}
}
\value{
(mlr3pipelines) \code{\link{Graph}}
}
\description{
Wraps MCBoost in a Pipeline to be used with \code{mlr3pipelines}.
For now this assumes training on the same dataset that is later used
for multi-calibration.
}
\examples{
  \dontrun{
  library("mlr3pipelines")
  gr = ppl_mcboost()
  }
}
