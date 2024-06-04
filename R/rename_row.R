#' Title
#'
#' @param df the dataframe with column names in the wrong row
#' @param row_number the number of the row containing the desired column names
#'
#' @return a dataframe with the correct column names
#' @export
#'
#' @examples rename_row(df = rename_example_df, row_number = 2)

rename_row <- function(df, row_number){
  # Extract the named row containing the new column names)
  new_col_names <- df[{{row_number}},-c(1:3)]

  # Rename the columns (excluding the first three columns)
  df <- df  |> dplyr::rename_at(dplyr::vars(-table_name, -folder_name, -index_no), ~ as.character(new_col_names))
  # Remove the first row_number rows
  #df_new <- df[-c(1:row_number), ]
  df_new <- df |> dplyr::slice(-(1:{{row_number}}))
  return(df_new)
}
