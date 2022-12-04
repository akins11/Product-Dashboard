nav_ov_income_summary <- function(id) {
  ns <- shiny::NS(id)

  bslib::nav( # ------------------------ NAV 1 ------------------------->>
    title = "Income",
    icon = fontawesome::fa_i("fas fa-coins"),

    shiny::br(),

    shiny::fluidRow( # rev spark line cards ---------------------------->>
      shiny::column(
        width = 4,

        card(
          shiny::uiOutput(outputId = ns("ov_rev_revenue_card")),

          border = FALSE
        )
      ),

      shiny::column(
        width = 4,

        card(
          shiny::uiOutput(outputId = ns("ov_rev_cost_card")),

          border = FALSE
        )
      ),

      shiny::column(
        width = 4,

        card(
          shiny::uiOutput(outputId = ns("ov_rev_profit_card")),

          border = FALSE
        )
      )
    ),

    shiny::br(),

    shiny::fluidRow(
      shiny::column( # YoY / MTD / Top product ------------------------------->>
        width = 4,

        card(
          shiny::br(),

          input_dropdown(dropdown_id = ns("yoy_input_dd"),
                         change_period_ui(month_quarter_id = ns("month_quarter"),
                                          month_picker_id = ns("m_date"),
                                          quarter_id = ns("q_date"))),

          uiOutput(outputId = ns("ov_rev_yoy")),


          shiny::br(), shiny::br(),


          shinyjs::hidden(
            shinyWidgets::airDatepickerInput(inputId = ns("mtd_date_range"),
                                             label = shiny::tags$h6("Select `start` and `end` date"),
                                             range  = TRUE,
                                             clearButton = TRUE,
                                             position = "top right",
                                             autoClose = FALSE)
          ),


          detail_button(btn_id = ns("mtd_input_btn"), toggle = TRUE),

          uiOutput(outputId = ns("ov_rev_mtd")),


          shiny::br(), shiny::br(),


          input_dropdown(dropdown_id = ns("top_product_dd"),
                         shiny::selectInput(inputId = ns("top_product_by"),
                                            label = "By",
                                            choices = rev_choice,
                                            selected = "profit"),

                         shinyWidgets::prettyRadioButtons(inputId = ns("top_product_agg"),
                                                          label = "",
                                                          choices = c("Total"="sum",
                                                                      "Average"="mean"),
                                                          selected = "sum",
                                                          shape = "curve",
                                                          fill = TRUE,
                                                          animation = "pulse",
                                                          bigger = TRUE,
                                                          inline = TRUE)),

          uiOutput(outputId = ns("top_product")),

          shiny::br(), shiny::br(),
        )
      ),

      shiny::column(
        width = 8,

        shiny::fluidRow(
          shiny::column(
            width = 6,

            card(
              apexcharter::apexchartOutput(outputId = ns("cost_revenue_percent"))
            )
          ),

          shiny::column(
            width = 6,

            card(
              apexcharter::apexchartOutput(outputId = ns("profit_revenue_percent"))
            )
          )
        ),

        shiny::br(),

        shiny::fluidRow(
          shiny::column(
            width = 12,

            card(
              input_dropdown(dropdown_id = ns("coun_age_gp_dd"),
                             shinyWidgets::prettyRadioButtons(inputId = ns("coun_age_gp_agg"),
                                                              label = "",
                                                              choices = c("Total"="sum",
                                                                          "Average"="mean",
                                                                          "n-Transactions"="count"),
                                                              selected = "sum",
                                                              shape = "curve",
                                                              fill = TRUE,
                                                              animation = "pulse",
                                                              bigger = TRUE),

                             shinyjs::hidden(shiny::selectInput(inputId = ns("coun_age_gp_by"),
                                                                label = "By",
                                                                choices = rev_choice,
                                                                selected = "profit"))),

              shiny::br(),

              echarts4r::echarts4rOutput(outputId = ns("country_age_gp_summy")),
              # highcharter::highchartOutput(outputId = ns("country_age_gp_summy")),

              class = "mh-490"
            )
          )
        ),

        shiny::br()
      )
    ),


    shiny::br(),

    shiny::fluidRow( # trend row ---------------------------------------->>
      shiny::column(
        width = 12,

        card(
          input_dropdown(dropdown_id = ns("rev_year_setting"),
                         shiny::selectInput(inputId = ns("rev_year_trend_by"),
                                            label = "By",
                                            choices = rev_choice,
                                            selected = "profit"),
                         shiny::selectInput(inputId = ns("rev_year_trend_agg"),
                                            label = "Aggregate function",
                                            choices = agg_choice,
                                            selected = "sum")),

          detail_button(btn_id = ns("rev_year_trend_dt")),

          highcharter::highchartOutput(outputId = ns("rev_year_trend"))
        )
      )
    )
  )
}


nav_ov_demographic <- function(id) {
  ns <- shiny::NS(id)

  bslib::nav( # ------------------------ NAV 2 ------------------------->>
    title = "Demographic",
    icon = fontawesome::fa_i("fas fa-users-line"),

    shiny::br(),

    shiny::fluidRow(
      shiny::column(
        width = 3,

        card(
          uiOutput(outputId = ns("var_num_product")),

          border = FALSE
        )
      ),

      shiny::column(
        width = 3,

        card(
          uiOutput(outputId = ns("var_num_country")),

          border = FALSE
        )
      ),

      shiny::column(
        width = 3,

        card(
          uiOutput(outputId = ns("var_num_state")),

          border = FALSE
        )
      ),

      shiny::column(
        width = 3,

        card(
          uiOutput(outputId = ns("var_num_age")),

          border = FALSE
        )
      )
    ),

    shiny::br(),

    shiny::fluidRow(
      shiny::column(
        width = 7,

        card(
          agg_by_select_input_dd(dropdown_id   = ns("rev_prod_category_setting"),
                                 select_by_id  = ns("rev_prod_category_by"),
                                 select_agg_id = ns("rev_prod_category_agg")),

          highcharter::highchartOutput(outputId = ns("rev_prod_category")),

          class = "mh-466"
        )
      ),

      shiny::column(
        width = 5,

        card(
          shiny::br(),

          echarts4r::echarts4rOutput(outputId = ns("across_years")),

          class = "mh-466"
        )
      )
    ),

    shiny::br(),

    shiny::fluidRow(
      shiny::column(
        width = 7,

        card(
          detail_button(btn_id = ns("age_group_density")),

          shiny::br(),

          echarts4r::echarts4rOutput(outputId = ns("cus_age_density")),

          class = "mh-458"
        )
      ),

      shiny::column(
        width = 5,

        card(
          agg_by_select_input_dd(dropdown_id   = ns("top_5_state_dd"),
                                 select_by_id  = ns("top_5_state_by"),
                                 select_agg_id = ns("top_5_state_agg")),

          shiny::br(),

          reactable::reactableOutput(outputId = ns("top_5_states")),

          class = "mh-458"
        )
      )
    ),

    shiny::br(),

    shiny::fluidRow(
      shiny::column(
        width = 7,

        card(
          detail_button(btn_id = ns("age_group_gender_density")),

          shiny::br(),

          echarts4r::echarts4rOutput(outputId = ns("age_gender_density"))
        )
      ),

      shiny::column(
        width = 5,

        card(
          agg_by_select_input_dd(dropdown_id   = ns("rev_country_dd"),
                                 select_by_id  = ns("rev_country_by"),
                                 select_agg_id = ns("rev_country_agg")),

          shiny::br(),

          echarts4r::echarts4rOutput(outputId = ns("rev_country"))
        )
      )
    ),

    shiny::br(), br()
  )
}



#' overview UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_overview_ui <- function(id) {

  ns <- shiny::NS(id)

  shiny::tagList(

    shiny::fluidPage(

      waiter::waiterPreloader(
        html = waiter::spin_chasing_dots(),
        color = "#37474e",
        fadeout = TRUE
      ),

      bslib::navs_pill(

        nav_ov_income_summary(id),

        nav_ov_demographic(id),


        bslib::nav_spacer(),

        bslib::nav_item(
          filter_year_dropdown(dropdowm_id = ns("ov_year_filter_dd_btn2"),
                               radio_year_id = ns("ov_year_filter"),
                               tooltip_dir = "left")
        )
      )
    )
  )
}





#' overview Server Functions
#'
#' @noRd
mod_overview_server <- function(id) {
  shiny::moduleServer(
    id = id,

    module = function(input, output, session) {

    ns <- session$ns

    p_df <- shiny::reactive({
      vroom::vroom("data/prod.csv", delim = ",") |>
        transform_data_type()
    })

    r_year <- shiny::reactive({ as.numeric(input$ov_year_filter) })

    fp_df <- shiny::reactive({
      filter_r_data(df = p_df(), f_year = r_year())
    })


    # ==== NAV 1 =============================================================
    # -------- Rev Cards --------------------------------------------------->>
    # Revenue --------------------------------------------------------------->
    rev_val <- shiny::reactive({
      overview_rev_summary(df = p_df(), rev_var = "revenue", f_year = r_year())
    })

    output$ov_rev_revenue_card <- renderUI({

      shiny::tagList(
        rev_description_card(
          title = "Revenue",
          change = rev_val()$change,
          change_label = rev_val()$change_label,
          spark_id = ns("ov_revenue_spark"),
          md_btn_id = ns("ov_btn_revenue_dt"),
          background_color = ov_card_light[1]
        )
      )
    })

    output$ov_revenue_spark <- apexcharter::renderSparkBox({
      cs_sparkline(df = p_df(),
                   variable = "revenue",
                   title =  rev_val()$value_label)
    })

    shiny::observe({
      render_modal(
        apexcharter::apexchartOutput(outputId = ns("ov_revenue_mdl_plot"))
      )
    }) |>
      bindEvent(input$ov_btn_revenue_dt)

    output$ov_revenue_mdl_plot <- apexcharter::renderApexchart({
      overview_rev_months_summary(fy_df = fp_df(),
                                  variable = "revenue",
                                  c_color = ov_card_light[1])
    })


    # Cost ------------------------------------------------------------------>
    cost_val <- shiny::reactive({
      overview_rev_summary(df = p_df(), rev_var = "cost", f_year = r_year())
    })

    output$ov_rev_cost_card <- renderUI({

      shiny::tagList(
        rev_description_card(
          title = "Cost",
          change = cost_val()$change,
          change_label = cost_val()$change_label,
          spark_id = ns("ov_cost_spark"),
          md_btn_id = ns("ov_btn_cost_dt"),
          is_cost = TRUE,
          background_color = ov_card_light[2]
        )
      )
    })

    output$ov_cost_spark <- apexcharter::renderSparkBox({
      cs_sparkline(df = p_df(),
                   variable = "cost",
                   title =  cost_val()$value_label)
    })

    shiny::observe({
      render_modal(
        apexcharter::apexchartOutput(outputId = ns("ov_cost_mdl_plot"))
      )
    }) |>
      bindEvent(input$ov_btn_cost_dt)

    output$ov_cost_mdl_plot <- apexcharter::renderApexchart({
      overview_rev_months_summary(fy_df = fp_df(),
                                  variable = "cost",
                                  c_color = ov_card_light[2])
    })


    # Profit ---------------------------------------------------------------->
    profit_val <- shiny::reactive({
      overview_rev_summary(df = p_df(), rev_var = "profit", f_year = r_year())
    })

    output$ov_rev_profit_card <- renderUI({

      shiny::tagList(
        rev_description_card(
          title = "Profit",
          change = profit_val()$change,
          change_label = profit_val()$change_label,
          spark_id = ns("ov_profit_spark"),
          md_btn_id = ns("ov_btn_profit_dt"),
          background_color = ov_card_light[3]
        )
      )
    })

    output$ov_profit_spark <- apexcharter::renderSparkBox({
      cs_sparkline(df = p_df(),
                   variable = "profit",
                   title =  profit_val()$value_label)
    })

    observe({
      render_modal(
        apexcharter::apexchartOutput(outputId = ns("ov_profit_mdl_plot"))
      )
    }) |>
      bindEvent(input$ov_btn_profit_dt)

    output$ov_profit_mdl_plot <- apexcharter::renderApexchart({
      overview_rev_months_summary(fy_df = fp_df(),
                                  variable = "profit",
                                  c_color = ov_card_light[3])
    })



    # Period changes ------------------------------------------------------->>
    # YoY ------------------------------------------------------------------->
    shiny::observe({
      mnth_val <- get_period_choosen_value(fp_df(), p_type = "month")

      shinyWidgets::updateAirDateInput(
        session = session,
        inputId = "m_date",
        label = "",
        value = mnth_val$min,
        options = list(
          minDate = mnth_val$min_date,
          maxDate = mnth_val$max_date
        )
      )
    })

    shiny::observe({
      qurt_val <- get_period_choosen_value(fp_df(), p_type = "quarter")

      shiny::updateSelectInput(
        session = session,
        inputId = "q_date",
        choices = qurt_val,
        selected = qurt_val[1]
      )
    })

    yoy_value <- shiny::reactive({
      shiny::req(p_df())

      if (!is.null(input$month_quarter) & !is.null(input$m_date) & !is.null(input$q_date)) {
        p_value <- ifelse(input$month_quarter == "month", input$m_date, input$q_date)

        get_yoy(df = p_df(),
                period = input$month_quarter,
                period_value = p_value,
                f_year = r_year(),
                agg_fun = "sum")
      }
    })


    output$ov_rev_yoy <- shiny::renderUI({
      shiny::req(p_df())

      shiny::tagList(
        period_diff_card(
          dropdown_id = ns("yoy_dd"),
          title = "YoY",
          value = yoy_value()$value,
          value_label = yoy_value()$value_label,
          subtitle = yoy_value()$s_period
        )
      )
    })

    shiny::observe({
      shiny::req(input$month_quarter)

      if (input$month_quarter == "quarter") {
        show_input(id = "q_date")
        hide_input(id = "m_date")

      } else if (input$month_quarter == "month") {
        show_input(id = "m_date")
        hide_input(id = "q_date")
      }
    }) |>
      shiny::bindEvent(input$month_quarter)


    # MTD ------------------------------------------------------------------->
    shiny::observe({
      if (input$mtd_input_btn) {
        show_input(id = "mtd_date_range")

      } else {
        hide_input(id = "mtd_date_range")
      }
    })

    shiny::observe({
      req(fp_df())

      updt <- valid_mtd_range(fp_df())

      shinyWidgets::updateAirDateInput(
        session = session,
        inputId = "mtd_date_range",
        value = c(updt$min_date, updt$max_date),
        options = list(minDate = updt$min_date,
                       maxDate = updt$max_date)
      )
    })


    shiny::observe({
      valid_mtd_values(input$mtd_date_range)
    }) |>
      bindEvent(input$mtd_date_range)


    output$ov_rev_mtd <- shiny::renderUI({
      shiny::req(fp_df())

      dr <- input$mtd_date_range

      if (length(dr) == 2) {
        mtd_val <- get_mtd(df = fp_df(),
                           start_date = dr[1],
                           end_date = dr[2],
                           agg_fun = "sum")
      } else {
        mtd_val <- list(value = 0, value_label = "--", subtitle = "Select 2 date values")
      }

      shiny::tagList(
        period_diff_card(
          dropdown_id = ns("mtd_dd"),
          title = "MTD",
          value = mtd_val$value,
          value_label = mtd_val$value_label,
          subtitle = mtd_val$subtitle
        )
      )
    })


    # Top product ----------------------------------------------------------->
    output$top_product <- shiny::renderUI({
      shiny::req(fp_df(), input$top_product_by, input$top_product_agg)

      top_values <- top_overall_product(fy_df = fp_df(),
                                        rev_var = input$top_product_by,
                                        agg_fun = input$top_product_agg)

      shiny::tagList(
        period_diff_card(
          dropdown_id = ns("top_prod_dd"),
          title = glue::glue("Top product by {clean_label(input$top_product_by)}"),
          value = top_values$value,
          value_label = top_values$value_label,
          subtitle = top_values$product,
          is_cost = ifelse(input$top_product_by == "cost", TRUE, FALSE)
        )
      )
    })

    # country age group summary -------------------------------------------->>
    shiny::observe({
      if (input$coun_age_gp_agg == "count") {
        hide_input("coun_age_gp_by")
      } else {
        show_input("coun_age_gp_by")
      }
    })

    output$country_age_gp_summy <- echarts4r::renderEcharts4r({
      shiny::req(fp_df(), input$coun_age_gp_agg, input$coun_age_gp_by)

      country_age_group_summary(fy_df = fp_df(),
                                rev_var = input$coun_age_gp_by,
                                agg_fun = input$coun_age_gp_agg)
    })

    # percentage of revenue ------------------------------------------------>>
    rev_precent <- shiny::reactive({
      shiny::req(fp_df())

      percent_revenue(fp_df())
    })

    output$cost_revenue_percent <- apexcharter::renderApexchart({
      shiny::req(rev_precent())

      percent_rev_radial(percent_list = rev_precent(),
                         val = "cost",
                         r_color = "#E71D36")
    })

    output$profit_revenue_percent <- apexcharter::renderApexchart({
      shiny::req(rev_precent())

      percent_rev_radial(percent_list = rev_precent(),
                         val = "profit",
                         r_color = "#2EC4B6")
    })

    # revenue trend -------------------------------------------------------->>
    # month ----------------------------------------------------------------->
    shiny::observe({

      render_modal(
        shiny::fluidRow(
          shiny::column(
            width = 4,

            shiny::selectInput(inputId = ns("rev_month_trend_month"),
                               label = "Month",
                               choices = unique(fp_df()$month))
          ),

          shiny::column(
            width = 4,

            shiny::selectInput(inputId = ns("rev_month_trend_var"),
                               label = "By",
                               choices = rev_choice,
                               selected = "profit")
          ),


          shiny::column(
            width = 4,

            shiny::selectInput(inputId = ns("rev_month_trend_agg"),
                               label = "Aggregate Function",
                               choices = agg_choice,
                               selected = "sum")
          )
        ),

        shiny::fluidRow(
          shiny::column(
            width = 12,

            highcharter::highchartOutput(outputId = ns("rev_month_trend"))
          )
        ),

        m_size = "l"
      )
    }) |>
      bindEvent(input$rev_year_trend_dt)


    output$rev_month_trend <- highcharter::renderHighchart({
      shiny::req(fp_df(), input$rev_month_trend_var, input$rev_month_trend_month, input$rev_month_trend_agg)

      if (!is.null(input$rev_month_trend_var) &
          !is.null(input$rev_month_trend_month) &
          !is.null(input$rev_month_trend_agg)) {

        ov_rev_trend_month(fp_df(),
                           rev_var = input$rev_month_trend_var,
                           f_month = input$rev_month_trend_month,
                           agg_fun = input$rev_month_trend_agg)
      }

    })


    # whole year ------------------------------------------------------------>
    output$rev_year_trend <- highcharter::renderHighchart({
      shiny::req(fp_df(), input$rev_year_trend_by, input$rev_year_trend_agg)

      if (!is.null(input$rev_year_trend_by) & !is.null(input$rev_year_trend_agg)) {
        ov_rev_trend(df = fp_df(),
                     rev_var = input$rev_year_trend_by,
                     agg_fun = input$rev_year_trend_agg)
      }
    })

    # ==== NAV 2 =============================================================
    # variable numbers ----------------------------------------------------->>
    var_num <- shiny::reactive({
      shiny::req(fp_df())

      variable_numbers(fp_df())
    })

    # product --------------------------------------------------------------->
    output$var_num_product <- renderUI({
      req(var_num())

      shiny::tagList(
        variable_number_card(title = "Total",
                             m_icon = "cart-shopping",
                             value = var_num()$product,
                             subtitle = "Product")
      )
    })

    # country --------------------------------------------------------------->
    output$var_num_country <- renderUI({
      req(var_num())

      shiny::tagList(
        variable_number_card(title = "Total",
                             m_icon = "location-pin",
                             value = var_num()$country,
                             subtitle = "Country")
      )
    })

    # state ----------------------------------------------------------------->
    output$var_num_state <- renderUI({
      req(var_num())

      shiny::tagList(
        variable_number_card(title = "Total",
                             m_icon = "location-dot",
                             value = var_num()$state,
                             subtitle = "State")
      )
    })

    # average age ----------------------------------------------------------->
    output$var_num_age <- renderUI({
      req(var_num())

      shiny::tagList(
        variable_number_card(title = "Average",
                             m_icon = "user",
                             value = var_num()$avg_age,
                             subtitle = "Customer Age")
      )
    })


    # Product category & revenue info -------------------------------------->>
    # prod category --------------------------------------------------------->
    output$rev_prod_category <- highcharter::renderHighchart({
      shiny::req(fp_df(), input$rev_prod_category_by, input$rev_prod_category_agg)

      ov_product_category_summary(fy_df =  fp_df(),
                                  rev_var = input$rev_prod_category_by,
                                  agg_fun = input$rev_prod_category_agg)
    })

    # with year ------------------------------------------------------------->
    output$across_years <- echarts4r::renderEcharts4r({
      shiny::req(p_df(), input$rev_prod_category_by, input$rev_prod_category_agg)

      ov_product_category_pv_year(df = p_df(),
                                  rev_var = input$rev_prod_category_by,
                                  f_year = r_year(),
                                  agg_fun = input$rev_prod_category_agg)
    })



    # Customer Age --------------------------------------------------------->>
    output$cus_age_density <- echarts4r::renderEcharts4r({
      age_distribution(fp_df())
    })

    shiny::observe({
      shiny::req(fp_df())

      dist_age_group <- distinct_values(df = fp_df(), dis_var1 = "age_group")

      render_modal(
        shiny::fluidRow(
          shiny::column(
            width = 4,

            shiny::selectInput(inputId = ns("age_group_den"),
                               label = "Age Group",
                               choices = dist_age_group,
                               selected = dist_age_group[2])
          )
        ),

        shiny::fluidRow(
          shiny::column(
            width = 12,

            echarts4r::echarts4rOutput(outputId = ns("cus_age_group_density"))
          )
        ),

        m_size = "l"
      )
    }) |>
      bindEvent(input$age_group_density)

    output$cus_age_group_density <- echarts4r::renderEcharts4r({
      shiny::req(fp_df(), input$age_group_den)

      age_distribution(fy_df = fp_df(), cus_age_group = input$age_group_den)
    })


    # Customer Age and Gender ---------------------------------------------->>
    output$age_gender_density <- echarts4r::renderEcharts4r({
      shiny::req(fp_df())

      age_distribution(fy_df = fp_df(), include_gender = TRUE)
    })

    shiny::observe({
      shiny::req(fp_df())

      dist_age_group <- distinct_values(df = fp_df(), dis_var1 = "age_group")

      render_modal(
        shiny::fluidRow(
          shiny::column(
            width = 4,

            shiny::selectInput(inputId = ns("age_group_gender_den"),
                               label = "Age Group",
                               choices = dist_age_group,
                               selected = dist_age_group[2])
          )
        ),

        shiny::fluidRow(
          shiny::column(
            width = 12,

            echarts4r::echarts4rOutput(outputId = ns("cus_age_group_gender_density"))
          )
        ),

        m_size = "l"
      )
    }) |>
      bindEvent(input$age_group_gender_density)

    output$cus_age_group_gender_density <- echarts4r::renderEcharts4r({
      shiny::req(fp_df(), input$age_group_gender_den)

      age_distribution(fy_df = fp_df(),
                       cus_age_group = input$age_group_gender_den,
                       include_gender = TRUE)
    })


    # Top locations -------------------------------------------------------->>
    # state & country ------------------------------------------------------->
    output$top_5_states <- reactable::renderReactable({
      req(p_df(), input$top_5_state_by, input$top_5_state_agg)

      ov_top_5_state(df = p_df(),
                     rev_var = input$top_5_state_by,
                     f_year =  r_year(),
                     agg_fun = input$top_5_state_agg)
    })

    # country --------------------------------------------------------------->
    output$rev_country <- echarts4r::renderEcharts4r({
      shiny::req(fp_df(), input$rev_country_by, input$rev_country_agg)

      ov_country_rev_summary(fy_df = fp_df(),
                             rev_var = input$rev_country_by,
                             agg_fun = input$rev_country_agg)
    })

    # Module output ------------------------------------------------------->>>
    return(p_df)
  }
 )
}

