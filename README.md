IMPORTANT: Portfolio repo — public for viewing only. Reuse, redistribution, modification, or commercial use of this code is NOT permitted without prior written permission. If you would like to request access, a collaboration, or re-use rights, contact: your.email@example.com.

Survey Analysis Toolkit
=======================

A compact collection of R functions for categorical survey workflows: demographic summaries, association tests, numeric diagnostics, moderation, mediation, path analysis, and network modelling. All functions require explicit variable selection and return tidy summaries, detailed objects, and ggplot-ready visualisations.

Quick start and examples are below. Full usage and examples: see the sections that follow.

---

Release notes for v0.1.0 (paste into "Draft a new release")
----------------------------------------------------------
Title:
v0.1.0 — Initial public release

Body:
Initial public release of Survey Analysis Toolkit.

What’s included
- survey_summary_categorical: vectorized categorical summaries (counts, proportions, missing).
- run_assoc_tests_general: numeric dependent vs categorical predictors (Welch t / ANOVA, nonparametric tests, Levene, effect sizes) with per-predictor plots.
- numeric_diagnostics_general: numeric variable diagnostics, normality tests, histograms and QQ plots.
- moderation_analysis: interaction models, simple-slope probing (supports emmeans), and interaction plots.
- mediation_analysis: mediator and outcome models plus mediation::mediate wrapper for causal mediation.
- path_analysis: lavaan-based path model runner returning standardized estimates and fit indices.
- network_analysis: Gaussian graphical model (bootnet/EBICglasso) and optional Ising network support for binary data.

How to reproduce the minimal demo
1. Clone the repo and open R in the project root.
2. Install required packages (only those you need):
   install.packages(c("tidyverse","janitor","broom","rstatix","effectsize","e1071","nortest"))
   install additional packages for advanced features as needed (emmeans, lavaan, bootnet, qgraph, mediation).
3. Source the R scripts (R/*.R) or run the example RMarkdown in /examples to reproduce the quick demo.
4. Example: survey_summary_categorical(sample_df, vars = c("gender","education","age_group")).

Notes and contact
- This repository is public for portfolio and discovery only. Code reuse, modification, redistribution or commercial use requires prior written permission. To request access, propose collaboration, or ask for runnable demos, contact: your.email@example.com.

Changelog
- v0.1.0 — initial release (basic functions and examples).
