% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validate.R
\name{validateConceptSetArgument}
\alias{validateConceptSetArgument}
\title{Validate conceptSet argument.}
\usage{
validateConceptSetArgument(
  conceptSet,
  cdm = NULL,
  validation = "error",
  call = parent.frame()
)
}
\arguments{
\item{conceptSet}{It can be either a named list of concepts or a codelist,
codelist_with_details or conceptSetExpression object.}

\item{cdm}{A cdm_reference object, needed if a conceptSetExpression is
provided.}

\item{validation}{How to perform validation: "error", "warning".}

\item{call}{A call argument to pass to cli functions.}
}
\description{
Validate conceptSet argument.
}
