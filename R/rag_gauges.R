#' DEPO specific function for the 4 dial RAG output
#'
#' @param df a dataframe with the rag rating as numeric values
#'
#' @return a ggplot object of 4 rag gauges
#' @export
#'
#' @examples rag_gauges(rag_df) #rag_df is an example dataframe included in this package
#' @examples rag_gauges(df = rag_df)


rag_gauges <- function(df) {
  #df <- rag_df |> filter(folder_name == area)
  g1 <- df |> dplyr::select(overall_delivery_confidence_rag) |> as.numeric() |> gg_gauge(title = "Confidence")
  g2 <- df |> dplyr::select(delivery_against_the_plan) |> as.numeric() |> gg_gauge(title = "Against Plan")
  g3 <- df |> dplyr::select(delivery_against_the_budget) |> as.numeric() |> gg_gauge(title = "Against Budget")
  g4 <- df |> dplyr::select(delivery_against_resource) |> as.numeric() |> gg_gauge(title = "Against Resource")

  ggpubr::ggarrange(g1, g2, g3, g4, ncol = 4)
}
