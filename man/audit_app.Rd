% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/audit-app.R
\name{audit_app}
\alias{audit_app}
\title{Run Shiny app validation tools in your project}
\usage{
audit_app(
  headless_actions = NULL,
  scope = c("manual", "DMC", "POC"),
  debug = FALSE,
  timeout = NULL,
  cran = FALSE,
  vignettes = FALSE,
  error_on = "never",
  workers = 5,
  crash_test = TRUE,
  output_validation = FALSE,
  coverage = TRUE,
  load_testing = TRUE,
  profile_code = TRUE,
  check_reactivity = TRUE,
  flow = FALSE,
  r_version = NULL,
  locked_deps = NULL,
  port = randomPort(min = 3000, max = 3500),
  ...
)
}
\arguments{
\item{headless_actions}{Custom code passed as an expression to manipulate the app with headless
web browser, for instance
\code{app$set_inputs(obs = 200); app$run_js('1+1');}.
See \url{https://rstudio.github.io/shinytest2/reference/AppDriver.html} to
get all available methods.
If NULL, the app will undergo a classic random Monkey test session, taking a screenshot
right after loading and after the monkey test. We do not recommend running a monkey test
if your app is pointing to a database, unless the pipeline CI/CD runs in a dedicated test environment.}

\item{scope}{Project scope. Accepted values \code{c("manual", "DMC", "POC")}.}

\item{debug}{Special mode during which unit tests are skipped for faster output.}

\item{timeout}{Time to wait after starting the subprocess (s). Useful is you know
how much time the app takes to load. Defaults to 10 seconds locally and 20 seconds
on CI/CD.}

\item{cran}{Whether to apply as CRAN check. Defaults to FALSE.}

\item{vignettes}{Whether to build vignettes. Defaults to FALSE.}

\item{error_on}{When to raise an error. Possible choices:
\code{c("never", "error", "warning", "note")}. Defaults to never.}

\item{workers}{Number of workers for shinycannon. Default to 5.}

\item{crash_test}{Whether to enable crash test of Shiny app. Default to TRUE.}

\item{output_validation}{Whether to compare output snapshots for
plots and htmlwidgets. Default to TRUE.}

\item{coverage}{Whether to perform coverage report. Default to TRUE.}

\item{load_testing}{Whether to perform load test. Default to TRUE.}

\item{profile_code}{Whether to profile R code. Default to TRUE.}

\item{check_reactivity}{Whether to check reactivity log. Default to TRUE.}

\item{flow}{Whether to display project overview. Default to TRUE.}

\item{r_version}{R version supported by your IT.}

\item{locked_deps}{List of packages supported by your IT. For instance you
can pass a dataframe like
\code{available.packages(repos = "https://cran.microsoft.com/snapshot/2017-01-19/")},
or read a csv file with the same structure.}

\item{port}{Port to start the app, default to \code{httpuv::randomPort()}.}

\item{...}{Pass extra parameters to run_app_audit. This is useful
if you work with packages like golem.}
}
\description{
Run all specified tools and requirements to validate Shiny apps project.
}
