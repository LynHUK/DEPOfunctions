#' Get path to example data for DEPOfunction::split_tables() example
#'
#' DEPOfunction comes bundled with an example file in its `inst/ex_data`
#' directory. This function make it easy to access.
#'
#' @param path Name of file. If `NULL`, the example files will be listed.
#' @export
#' @examples
#' data_example()
#' data_example("sample_download_file.xlsx")
data_example <- function(path = NULL) {
  if (is.null(path)) {
    dir(system.file("ex_data", package = "DEPOfunctions"))
  } else {
    system.file("ex_data", path, package = "DEPOfunctions", mustWork = TRUE)
  }
}
