---
title: 'mcboost: Multi-Calibration Boosting for R'
tags:
  - R
  - Multi-Calibration
  - Multi-Accuracy
  - Boosting
  - Post-Processing
  - Fair ML
authors:
  - name: Florian Pfisterer^[Corresponding author]
    orcid: 0000-0001-8867-762X
    affiliation: 1
  - name: Christoph Kern
    orcid: 0000-0001-7363-4299
    affiliation: 2
  - name: Susanne Dandl
    orcid: 0000-0003-4324-4163
    affiliation: 1
affiliations:
 - name: Ludwig Maximilian University of Munich
   index: 1
 - name: University of Mannheim
   index: 2
date: 13 May 2021
bibliography: paper.bib
---

# Summary

Given the increasing usage of automated prediction systems in the context of high-stakes decisions, a growing body of research focuses on methods for detecting and mitigating biases in algorithmic decision-making.
One important framework to audit for and mitigate biases in predictions is that of Multi-Calibration, introduced by @hebert-johnson2018.
The underlying fairness notion, Multi-Calibration, promotes the idea of multi-group fairness and requires calibrated predictions not only for marginal populations, but also for subpopulations that may be defined by complex intersections of many attributes.
A simpler variant of Multi-Calibration, referred to as Multi-Accuracy, requires unbiased predictions for large collections of subpopulations.
@hebert-johnson2018 proposed a boosting-style algorithm for learning multi-calibrated predictors.
@kim2019 demonstrated how to turn this algorithm into a post-processing strategy to achieve multi-accuracy, demonstrating empirical effectiveness across various domains.
This package provides a stable implementation of the multi-calibration algorithm, called MCBoost.
In contrast to other Fair ML approaches, MCBoost does not harm the overall utility of a prediction model, but rather aims at improving calibration and accuracy for large sets of subpopulations post-training.
MCBoost comes with strong theoretical guarantees, which have been explored formally in @hebert-johnson2018, @kim2019, @dwork-rankings, @dwork-oi and @kimkern2021.

`mcboost` implements Multi-Calibration Boosting for R.
`mcboost` is model agnostic and allows the user to post-process any supervised machine learning model.
For convenience and ease of use, `mcboost` tightly integrates with the **mlr3** [@mlr3] machine learning eco-system in R.
Post-processing with `mcboost` starts with an initial prediction model that is passed on to an auditing algorithm that runs Multi-Calibration-Boosting on a labeled auditing dataset (Fig. 1). The resulting model can be used for obtaining multi-calibrated predictions.
`mcboost` includes two pre-defined learners for auditing (ridge regression and decision trees), and allows to easily adjust the learner and its parameters for Multi-Calibration Boosting.
Users may also specify a fixed set of subgroups, instead of a learner, on which predictions should be audited.
Furthermore, `mcboost` includes utilities to guard against overfitting to the auditing dataset during post-processing.


<p align="center">
  <img src="MCBoost.png" />
</p>
<div align="center"> Fig 1. Conceptual illustration of Multi-Calibration Boosting with `mcboost`.</div>


# Statement of need

Given the ubiquitous use of machine learning models in crucial areas and growing concerns of biased predictions for minority subpopulations, Multi-Calibration Boosting should be widely accessible in form of a free and open-source software package.
Prior to the development of `mcboost`, Multi-Calibration Boosting has not been released as a software package for R.

The results in @kim2019 highlight that MCBoost can improve classification accuracy for subpopulations in various settings, including gender detection with image data, income classification with survey data and disease prediction using biomedical data.
@Barda2020bias show that post-processing for Multi-Calibration can greatly improve calibration metrics of two medical risk assessment models when evaluated in subpopulations defined by intersections of age, sex, ethnicity, socioeconomic status and immigration history.
@Barda2020covid demonstrate that Multi-Calibration can also be used to adjust an initial classifier for a new task. They re-calibrate a baseline model for predicting the risk of severe respiratory infection with data on COVID-19 fatality rates in subpopulations, resulting in an accurate and calibrated COVID-19 mortality prediction model.


We hope that with `mcboost`, Multi-Calibration Boosting can be utilized by a wide community of developers and data scientists to audit and post-process prediction models and helps to promote fairness in machine learning and statistical estimation applications.

# Acknowledgements

We thank Matthew Sun for developing an initial Python implementation of MCBoost.
This work has been partially supported by the German Federal Ministry of Education and Research (BMBF) under Grant No. 01IS18036A. The authors of this work take full responsibilities for its content.

# References
