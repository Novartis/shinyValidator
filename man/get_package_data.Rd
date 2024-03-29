% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check-dependencies.R
\name{get_package_data}
\alias{get_package_data}
\title{Get package metadata from renv lock}
\usage{
get_package_data(package_names, r_version, locked_deps)
}
\arguments{
\item{package_names}{Obtained after running \link{find_imports}.}

\item{r_version}{R version supported by your IT.}

\item{locked_deps}{List of packages supported by your IT. For instance you
can pass a dataframe like
\code{available.packages(repos = "https://cran.microsoft.com/snapshot/2017-01-19/")},
or read a csv file with the same structure.}
}
\value{
An HTML tag.
}
\description{
Also build a card tag for each package.
}
\keyword{internal}
