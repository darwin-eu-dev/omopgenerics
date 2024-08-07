% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/assert.R
\name{assertTrue}
\alias{assertTrue}
\title{Assert that an expression is TRUE.}
\usage{
assertTrue(x, null = FALSE, call = parent.frame(), msg = NULL)
}
\arguments{
\item{x}{Expression to check.}

\item{null}{Whether it can be NULL.}

\item{call}{Call argument that will be passed to \code{cli} error message.}

\item{msg}{Custom error message.}
}
\description{
Assert that an expression is TRUE.
}
