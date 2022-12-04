#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    bslib::page_navbar(

      title = "Product Dashboard",
      windowTitle = "Region Dashboard",

      header = shiny::tags$head(
        shiny::tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),

        shinyjs::useShinyjs(),
        waiter::useWaiter()
      ),

      bslib::nav_spacer(),
      bslib::nav_spacer(),
      bslib::nav_spacer(),
      bslib::nav_spacer(),



      shiny::tabPanel(
        title = "Overview",
        value = "tab_home_overview",

        mod_overview_ui(id = "overview")
      ),

      shiny::tabPanel(
        title = "Accessories",
        value = "tab_prod_accessories",

        mod_accessories_ui(id = "mod_accessories_id")
      ),

      shiny::tabPanel(
        title = "Clothing",
        value = "tab_prod_clothing",

        mod_clothing_ui(id = "mod_clothing_id")
      ),

      shiny::tabPanel(
        title = "Bike",
        value = "tab_prod_bike",

        mod_bike_ui(id = "mod_bike_id")
      ),


      bslib::nav_spacer(),


      bg = "#37474e",

      theme = bslib::bs_theme(version = 5,
                              bootswatch = "materia",
                              # bg = "#FCFCFC",
                              # fg = "#000000",
                              primary = "#37474e",
                              secondary = "#F5F5F5",
                              # info = "#858585",
                              warning = "#858585")
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "productDashboard"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
