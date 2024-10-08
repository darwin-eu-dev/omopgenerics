% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/assert.R
\name{assertList}
\alias{assertList}
\title{Assert that an object is a list.}
\usage{
assertList(
  x,
  length = NULL,
  na = FALSE,
  null = FALSE,
  unique = FALSE,
  named = FALSE,
  class = NULL,
  call = parent.frame(),
  msg = NULL
)
}
\arguments{
\item{x}{Variable to check.}

\item{length}{Required length. If \code{NULL} length is not checked.}

\item{na}{Whether it can contain NA values.}

\item{null}{Whether it can be NULL.}

\item{unique}{Whether it has to contain unique elements.}

\item{named}{Whether it has to be named.}

\item{class}{Class that the elements must have.}

\item{call}{Call argument that will be passed to \code{cli} error message.}

\item{msg}{Custom error message.}
}
\description{
Assert that an object is a list.
}
