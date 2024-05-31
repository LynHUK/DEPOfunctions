#' Title
#'
#' @param df dataframe containing multiple tables
#' @param which_table the text from the first cell (1st row, 1st column) of the desired table
#'
#' @return dataframe of the selected contents
#' @export
#'
#' @examples split_tables(data_example("sample_download_file.xlsx"))|> df_extract("Workforce Overview")
df_extract <- function(df, which_table){df |> dplyr::filter(table_name == which_table) |> tidyr::pivot_wider(id_cols = c(table_name, folder_name, index_no), names_from = col_names) }
