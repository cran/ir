% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ir.R
\name{subsetting}
\alias{subsetting}
\alias{[.ir}
\alias{$.ir}
\alias{[[.ir}
\alias{$<-.ir}
\alias{[<-.ir}
\alias{[[<-.ir}
\title{Subsetting \code{ir} objects}
\usage{
\method{[}{ir}(x, i, j, ..., exact = TRUE)

\method{$}{ir}(x, i)

\method{[[}{ir}(x, i, j, ..., exact = TRUE)

\method{$}{ir}(x, i, j, ...) <- value

\method{[}{ir}(i, j, ..., exact = TRUE) <- value

\method{[[}{ir}(i, j, ..., exact = TRUE) <- value
}
\arguments{
\item{x}{An object of class \code{ir}.}

\item{i, j}{Row and column indices. If \code{j} is omitted, \code{i} is used as column index.}

\item{...}{Ignored.}

\item{exact}{Ignored, with a warning.}

\item{value}{A value to store in a row, column, range or cell.
Tibbles are stricter than data frames in what is accepted here.}
}
\value{
If the subsetting operation preserves a valid \code{spectra} column
(see \code{\link[=ir_new_ir]{ir()}}), an object of class \code{ir} with
accordingly subsetted rows or columns. Else a \code{\link[tibble:tbl_df-class]{tibble::tbl_df()}} or
vector.
}
\description{
Subsetting \code{ir} objects
}
\examples{
# subsetting rows
ir_sample_data[1, ]
ir_sample_data[10:15, ]
ir_sample_data[ir_sample_data$sample_type == "office paper", ]

# subsetting columns
ir_sample_data[, "spectra"]
ir_sample_data[["spectra"]]
ir_sample_data$spectra

# not explicitly selecting the spectra column drops the ir class
class(ir_sample_data[, 1])
class(ir_sample_data[, "spectra"])

# subsetting values
ir_sample_data[, 1] # drops the ir class
ir_sample_data[, c("id_sample", "spectra")]
ir_sample_data$id_sample
ir_sample_data[[1, 1]]

# setting and replacing columns
x <- ir::ir_sample_data
x$a <- 3
x[, "a"] <- 4
x$sample_type <- "a"
x[[1]] <- rev(x[[1]])

# deleting the spectra column drops the ir class
x$spectra <- NULL
class(x)

# setting and replacing rows
x <- ir::ir_sample_data
x[1, ] <- x[2, ]
class(x)

# setting invalid values in the spectra column drops the ir class
x_replacement <- x[1, ]
x_replacement$spectra <- list(1)
x[1, ] <- x_replacement
class(x)

# setting and replacing values
x <- ir::ir_sample_data
x[[1, 1]] <- 100

# replacing an element in the spectra column by an invalid element drops the
# ir class attribute
x[[3, "spectra"]] <- list(1)
class(x)

}
