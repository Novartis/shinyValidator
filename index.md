
# shinyValidator

<!-- badges: start -->
[![R-CMD-check](https://github.com/Novartis/shinyValidator/workflows/R-CMD-check/badge.svg)](https://github.com/Novartis/shinyValidator/actions)
<!-- badges: end -->

<iframe src="shinyValidator-report.html" title="Coverage report" width="100%" allowfullscreen="" frameborder="0" scrolling="yes" height="500px"></iframe>

## Introduction

Don't know where to start? Follow this introduction video and slides from R Pharma 2022.

<iframe src="https://www.youtube.com/embed/STpBxos_IXY" title="shinyValidator workshop R Pharma 2022" width="100%" height="500px"></iframe>

<br/>

<iframe src="https://rinpharma2022.rinterface.com" title="shinyValidator slides" width="100%" height="500px"></iframe>

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
- It must be run within a __package__ Shiny app project, compatible with `{golem}`. 
- It requires to control R package dependencies with `{renv}`.

## Installation

You can install the preview version of `{shinyValidator}` with:

``` r
remotes::install_github("Novartis/shinyValidator")
```

<div style="text-align:center;"><a href="articles/shinyValidator.html" class="btn btn-outline-secondary btn-lg">Go to Get Started</a></div>

## Licence

Copyright 2022 Novartis AG

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
