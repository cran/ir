% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ir_flatten.R
\name{ir_flat_clean}
\alias{ir_flat_clean}
\title{Cleans objects of class \code{ir_flat}}
\usage{
ir_flat_clean(x, return_empty = FALSE)
}
\arguments{
\item{x}{An object of class \code{\link[=ir_new_ir_flat]{ir_flat}}.}

\item{return_empty}{A logical value indicating if the empty spectra should be
returned (\code{return_empty = TRUE}) or the non-empty spectra
(\code{return_empty = FALSE}).}
}
\value{
\code{x} where empty spectra are dropped (if \code{return_empty = TRUE}) or
only empty spectra are returned (\code{return_empty = FALSE}).
}
\description{
\code{ir_flatten_clean} takes an object of class \code{ir_flat} and either
returns all non-empty spectra or all empty spectra as object of class
\code{ir_flat}.
}
