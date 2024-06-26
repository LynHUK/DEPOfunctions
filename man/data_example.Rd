% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_data_path.R
\name{data_example}
\alias{data_example}
\title{Get path to example data for DEPOfunction::split_tables() example}
\usage{
data_example(path = NULL)
}
\arguments{
\item{path}{Name of file. If \code{NULL}, the example files will be listed.}
}
\description{
DEPOfunction comes bundled with an example file in its \code{inst/ex_data}
directory. This function make it easy to access.
}
\examples{
data_example()
data_example("sample_download_file.xlsx")
}
