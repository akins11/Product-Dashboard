#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  prod_data <- mod_overview_server(id = "overview")

  mod_accessories_server(id = "mod_accessories_id", p_df = prod_data)

  mod_clothing_server(id = "mod_clothing_id", p_df = prod_data)

  mod_bike_server(id = "mod_bike_id", p_df = prod_data)
}
