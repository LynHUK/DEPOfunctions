
#' Split a dataframe read from an untidy sheet in Excel
#'
#' @param df A single dataframe containing mutliple table with a blank row between them.
#'
#' @return A single dataframe combining all the tables as one, with the columns "folder_name", "table_name", "index_no", "col_names", "value".
#' @export
#'
#' @examples split_tables("inst/sample_download_file.xlsx")

split_tables <- function(filename, folder_name_position = "A5", skip_rows = 8, sheet = "Sheet") {
  folder_name <- readxl::read_excel(filename, sheet = sheet,
                            range = folder_name_position, col_names = FALSE) |> as.character()

  df <- readxl::read_excel(filename, sheet = sheet, skip = skip_rows, col_names = FALSE)

  # Find indices of rows where all values are NA or empty strings, i.e. the breaks between tables
  split_indices <- which(apply(df, 1, function(row) all(is.na(row) | row == "")))

  # Create a list to store split data frames
  split_dfs <- list()

  # Iterate over split indices and create separate data frames
  start_idx <- 1
  for (end_idx in split_indices) {
    split_df <- df[start_idx:(end_idx - 1), , drop = FALSE]
    # Extract value from the first cell and make it a new column
    first_cell_value <- as.character(split_df[1, 1])
    split_df <- dplyr::mutate(split_df, table_name = first_cell_value, index_no = dplyr::row_number())# |> remove_empty(which = "cols", cutoff = 1, quiet = TRUE)
    split_df <- tidyr::pivot_longer(split_df, cols = -c(table_name, index_no), names_to = "col_names", values_to = "value")
    split_dfs <- rbind(split_dfs, split_df)
    start_idx <- end_idx + 1
  }

  # Add last segment if necessary
  if (start_idx <= nrow(df)) {
    split_df <- df[start_idx:nrow(df), , drop = FALSE]
    # Extract value from the first cell and make it a new column
    first_cell_value <- as.character(split_df[1, 1])
    split_df <- dplyr::mutate(split_df, table_name = first_cell_value, index_no = dplyr::row_number()) #|> remove_empty(which = "cols", cutoff = 1, quiet = TRUE)
    split_df <- tidyr::pivot_longer(split_df, cols = -c(table_name, index_no), names_to = "col_names", values_to = "value")
    split_dfs <- rbind(split_dfs, split_df)
  }
  split_dfs <- dplyr::mutate(split_dfs, folder_name = folder_name) |> dplyr::filter(!is.na(value))
  return(split_dfs)
}

