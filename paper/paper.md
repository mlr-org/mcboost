---
title: 'mcboost: Multi-Accuracy Boosting for R'
tags:
  - R
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
date: 20 April 2021
bibliography: paper.bib
---

# Summary

Given the increasing usage of automated prediction systems in the context of high-stakes decisions, a growing body of research focuses on methods for detecting and mitigating biases in algorithmic decision-making. Two important frameworks to audit for and mitigate biases are Multi-Calibration (@hebert-johnson2018) and Multi-Accuracy (@kim2019).
The underlying fairness notion, Multi-Calibration, promotes the idea of multigroup fairness and requires calibrated predictions not only for marginal populations, but also for subpopulations that may be defined by complex intersections of many attributes. 
Multi-Accuracy, as a simpler variant of Multi-Calibration, requires unbiased predictions for large collections of subpopulations.
@kim2019 propose Multi-Accuracy Boosting (MCBoost), a boosting-style post-processing algorithm for producing multi-calibrated predictors.
In contrast to other Fair ML approaches, MCBoost does not harm the overall utility of a prediction model by imposing a fairness constraint in the training process, but rather aims at improving calibration and accuracy for large sets of subpopulations post-training.

`mcboost` implements Multi-Accuracy Boosting for R.
`mcboost` is model agnostic and allows the user to post-process any supervised machine learning model.
For convenience and ease of use, `mcboost` tightly integrates with the **mlr3**(@mlr3) machine learning eco-system in R.
Post-processing with `mcboost` would typically be run on a labeled auditing dataset after training an initial prediction model.
`mcboost` includes two pre-defined learners for post-processing (ridge regression and decision trees), and allows to easily adjust the learner that is used for Multi-Accuracy Boosting.
Users may also specify a fixed set of subgroups, instead of a learner, on which predictions should be audited. 
Furthermore, `mcboost` includes utilities to guard against overfitting to the auditing set during post-processing.

# Statement of need

Given the ubiquitous use of machine learning models in crucial areas and growing concerns of biased predictions for minority subpopulations, Multi-Accuracy Boosting should be widely accessible in form of a free and open-source software package. 
Previous to the development of `mcboost`, Multi-Accuracy Boosting has not been released as a software package for R.

The results in @kim2019 highlight that MCBoost can improve classification accuracy for subpopulations in various settings, including gender detection with image data, income classification with survey data and disease prediction using biomedical data. 
@Barda2020bias show that post-processing for Multi-Calibration can greatly improve calibration metrics of two medical risk assessment models when evaluated in subpopulations defined by intersections of age, sex, ethnicity, socioeconomic status and immigration history.
@Barda2020covid demonstrate that Multi-Calibration can also be used to adjust an initial classifier for a new task. They re-calibrate a baseline model for predicting the risk of severe respiratory infection with data on COVID-19 fatality rates in subpopulations, resulting in an accurate and calibrated COVID-19 mortality prediction model.

We hope that with `mcboost`, Multi-Accuracy Boosting can be utilized by a wide community of developers and data scientists to audit and post-process prediction models and helps to promote fairness in machine learning applications.

# Acknowledgements

We thank Matthew Sun for developing an initial Python implementation of MCBoost.

# References
