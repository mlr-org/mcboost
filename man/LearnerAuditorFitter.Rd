% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AuditorFitters.R
\name{LearnerAuditorFitter}
\alias{LearnerAuditorFitter}
\alias{TreeAuditorFitter}
\alias{RidgeAuditorFitter}
\title{Create an AuditorFitter from a Learner}
\value{
\code{\link{AuditorFitter}}\cr

\code{list} with items\cr
\itemize{
\item \code{corr}: pseudo-correlation between residuals and learner prediction.
\item \code{l}: the trained learner.
}
}
\description{
Instantiates an AuditorFitter that trains a \code{\link[mlr3:Learner]{mlr3::Learner}}
on the data.
}
\section{Functions}{
\itemize{
\item \code{TreeAuditorFitter}: Learner auditor based on rpart

\item \code{RidgeAuditorFitter}: Learner auditor based on glmnet

}}
\seealso{
Other AuditorFitter: 
\code{\link{CVLearnerAuditorFitter}},
\code{\link{SubgroupAuditorFitter}},
\code{\link{SubpopAuditorFitter}}

Other AuditorFitter: 
\code{\link{CVLearnerAuditorFitter}},
\code{\link{SubgroupAuditorFitter}},
\code{\link{SubpopAuditorFitter}}

Other AuditorFitter: 
\code{\link{CVLearnerAuditorFitter}},
\code{\link{SubgroupAuditorFitter}},
\code{\link{SubpopAuditorFitter}}
}
\concept{AuditorFitter}
\section{Super class}{
\code{\link[mcboost:AuditorFitter]{mcboost::AuditorFitter}} -> \code{LearnerAuditorFitter}
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{learner}}{\code{LearnerPredictor}\cr
Learner used for fitting residuals.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-LearnerAuditorFitter-new}{\code{LearnerAuditorFitter$new()}}
\item \href{#method-LearnerAuditorFitter-fit}{\code{LearnerAuditorFitter$fit()}}
\item \href{#method-LearnerAuditorFitter-clone}{\code{LearnerAuditorFitter$clone()}}
}
}
\if{html}{\out{
<details open><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="mcboost" data-topic="AuditorFitter" data-id="fit_to_resid"><a href='../../mcboost/html/AuditorFitter.html#method-AuditorFitter-fit_to_resid'><code>mcboost::AuditorFitter$fit_to_resid()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-LearnerAuditorFitter-new"></a>}}
\if{latex}{\out{\hypertarget{method-LearnerAuditorFitter-new}{}}}
\subsection{Method \code{new()}}{
Define an \code{AuditorFitter} from a Learner.
Available instantiations:\cr \code{\link{TreeAuditorFitter}} (rpart) and
\code{\link{RidgeAuditorFitter}} (glmnet).
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LearnerAuditorFitter$new(learner)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{learner}}{\code{\link[mlr3:Learner]{mlr3::Learner}}\cr
Regression learner to use.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-LearnerAuditorFitter-fit"></a>}}
\if{latex}{\out{\hypertarget{method-LearnerAuditorFitter-fit}{}}}
\subsection{Method \code{fit()}}{
Fit the learner and compute correlation
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LearnerAuditorFitter$fit(data, resid, mask)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{data}}{\code{\link{data.table}}\cr
Features.}

\item{\code{resid}}{\code{\link{numeric}}\cr
Residuals (of same length as data).}

\item{\code{mask}}{\code{\link{integer}}\cr
Mask applied to the data. Only used for \code{SubgroupAuditorFitter}.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-LearnerAuditorFitter-clone"></a>}}
\if{latex}{\out{\hypertarget{method-LearnerAuditorFitter-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{LearnerAuditorFitter$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
\section{Super classes}{
\code{\link[mcboost:AuditorFitter]{mcboost::AuditorFitter}} -> \code{\link[mcboost:LearnerAuditorFitter]{mcboost::LearnerAuditorFitter}} -> \code{TreeAuditorFitter}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-TreeAuditorFitter-new}{\code{TreeAuditorFitter$new()}}
\item \href{#method-TreeAuditorFitter-clone}{\code{TreeAuditorFitter$clone()}}
}
}
\if{html}{\out{
<details open><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="mcboost" data-topic="AuditorFitter" data-id="fit_to_resid"><a href='../../mcboost/html/AuditorFitter.html#method-AuditorFitter-fit_to_resid'><code>mcboost::AuditorFitter$fit_to_resid()</code></a></span></li>
<li><span class="pkg-link" data-pkg="mcboost" data-topic="LearnerAuditorFitter" data-id="fit"><a href='../../mcboost/html/LearnerAuditorFitter.html#method-LearnerAuditorFitter-fit'><code>mcboost::LearnerAuditorFitter$fit()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-TreeAuditorFitter-new"></a>}}
\if{latex}{\out{\hypertarget{method-TreeAuditorFitter-new}{}}}
\subsection{Method \code{new()}}{
Define a AuditorFitter from a rpart learner.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{TreeAuditorFitter$new()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-TreeAuditorFitter-clone"></a>}}
\if{latex}{\out{\hypertarget{method-TreeAuditorFitter-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{TreeAuditorFitter$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
\section{Super classes}{
\code{\link[mcboost:AuditorFitter]{mcboost::AuditorFitter}} -> \code{\link[mcboost:LearnerAuditorFitter]{mcboost::LearnerAuditorFitter}} -> \code{RidgeAuditorFitter}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-RidgeAuditorFitter-new}{\code{RidgeAuditorFitter$new()}}
\item \href{#method-RidgeAuditorFitter-clone}{\code{RidgeAuditorFitter$clone()}}
}
}
\if{html}{\out{
<details open><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="mcboost" data-topic="AuditorFitter" data-id="fit_to_resid"><a href='../../mcboost/html/AuditorFitter.html#method-AuditorFitter-fit_to_resid'><code>mcboost::AuditorFitter$fit_to_resid()</code></a></span></li>
<li><span class="pkg-link" data-pkg="mcboost" data-topic="LearnerAuditorFitter" data-id="fit"><a href='../../mcboost/html/LearnerAuditorFitter.html#method-LearnerAuditorFitter-fit'><code>mcboost::LearnerAuditorFitter$fit()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-RidgeAuditorFitter-new"></a>}}
\if{latex}{\out{\hypertarget{method-RidgeAuditorFitter-new}{}}}
\subsection{Method \code{new()}}{
Define a AuditorFitter from a glmnet learner.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{RidgeAuditorFitter$new()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-RidgeAuditorFitter-clone"></a>}}
\if{latex}{\out{\hypertarget{method-RidgeAuditorFitter-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{RidgeAuditorFitter$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
