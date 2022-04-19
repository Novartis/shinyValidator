
# shinyValidator

<!-- badges: start -->
[![R-CMD-check](https://github.com/Novartis/shinyValidator/workflows/R-CMD-check/badge.svg)](https://github.com/Novartis/shinyValidator/actions)
<!-- badges: end -->

## Purpose

<p>
<img src="reference/figures/shinyValidator.gif" alt="shinyValidator-report" style="display: block;
  margin-left: auto;
  margin-right: auto;
  width: 100%;">
</p>

`{shinyValidator}` aims at __automating__ the audit of a Shiny App project's __quality__, particularly required during a __validation/qualification__ process. There are many different tools available to validate Shiny apps but, to the best of our knowledge, nothing has been made to get a global overview in a centralized report. 

By default, `{shinyValidator}`:

- Lints the code to ensure __consistency__ and quality.
- Runs a `devtools::check()` to assess package quality.
- Checks whether the Shiny app starts and is alive after heavy usage (requires headless browser solutions like `{shinytest2}`).
- Performs output comparison between commits (plot, htmlwidget). 
- Launches a load test to measure app __performance__ and identify bottlenecks (user concurrency, ...) with `{shinyloadtest}`.
- Records an overall code profiling with `{profvis}`.
- Audits reactivity with `{reactlog}`.
- Provides overview of project structure with `{flow}`. 

Another goal of `{shinyValidator}` is to promote testing and automation (CI/CD) to deliver better Shiny apps in production. 

## Pre-requisites
- `{shinyValidator}` requires `R >= 4.0.0`. 
- It must be run within a __package__ Shiny app project, compatible with `{golem}`. 
- It requires to control R package dependencies with `{renv}`.

## Installation

You can install the preview version of `{shinyValidator}` with:

``` r
remotes::install_github("Novartis/shinyValidator")
```

<div style="text-align:center;"><a href="articles/shinyValidator.html" class="btn btn-outline-secondary btn-lg">Go to Get Started</a></div>
