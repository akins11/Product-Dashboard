nav_bike_prod_overview <- function(id) {

  ns <- shiny::NS(id)

  bslib::nav(
    title = "Overiew",
    icon = fontawesome::fa_i("fas fa-chart-gantt"),

    shiny::br(),

    shiny::fluidRow(
      column(
        width = 4,

        card(
          shiny::uiOutput(outputId = ns("bike_revenue_ow_card")),

          border = FALSE
        )
      ),

      column(
        width = 4,

        card(
          shiny::uiOutput(outputId = ns("bike_cost_ow_card")),

          border = FALSE
        )
      ),

      column(
        width = 4,

        card(
          shiny::uiOutput(outputId = ns("bike_profit_ow_card")),

          border = FALSE
        )
      )
    ),

    shiny::br(),

    shiny::fluidRow(
      shiny::column(
        width = 8,

        card(
          input_dropdown(
            dropdown_id = ns("bike_tras_days_dd"),
            shiny::selectInput(inputId = ns("bike_trans_days_sub_cat"),
                               label = "Product Sub-Category",
                               choice = NULL),

            shiny::selectInput(inputId = ns("bike_trans_days_prod"),
                               label = "Product",
                               choice = NULL)
          ),

          shiny::br(),

          echarts4r::echarts4rOutput(outputId = ns("bike_trans_days_calender")),

          class = "mh-458"
        )
      ),

      shiny::column(
        width = 4,

        card(
          shiny::br(),

          reactable::reactableOutput(outputId = ns("bike_trans_age_gp_gender")),

          class = "mh-458"
        )
      )
    ),

    shiny::br(),

    shiny::fluidRow(
      shiny::column(
        width = 6,

        card(
          echarts4r::echarts4rOutput(outputId = ns("bike_trans_sub_cat"))
        )
      ),

      shiny::column(
        width = 6,

        card(
          highcharter::highchartOutput(outputId = ns("bike_trans_location"))
        )
      )
    ),

    shiny::br(), shiny::br()
  )
}



nav_bike_prod_order <- function(id) {

  ns <- shiny::NS(id)

  bslib::nav(
    title = "Qty Order",
    icon = fontawesome::fa_i("fas fa-cart-plus"),

    shiny::br(),

    shiny::fluidRow(
      shiny::column(
        width = 3,

        card(
          shiny::uiOutput(outputId = ns("bike_var_num_products")),

          border = FALSE
        )
      ),

      shiny::column(
        width = 3,

        card(
          shiny::uiOutput(outputId = ns("bike_var_num_country")),

          border = FALSE
        )
      ),

      shiny::column(
        width = 3,

        card(
          shiny::uiOutput(outputId = ns("bike_var_num_state")),

          border = FALSE
        )
      ),

      shiny::column(
        width = 3,

        card(
          shiny::uiOutput(outputId = ns("bike_var_num_avg_age")),

          border = FALSE
        )
      )
    ),

    shiny::br(),

    shiny::fluidRow(
      shiny::column(
        width = 7,

        card(
          input_dropdown(dropdown_id = ns("bike_prod_ap_dd"),
                         shiny::selectInput(inputId = ns("bike_ord_prod_sub_cat"),
                                            label = "Product Sub Category",
                                            choice = NULL)),

          reactable::reactableOutput(outputId = ns("bike_ord_prod_ref_table")),

          class =  "mh-544"
        )
      ),

      shiny::column(
        width = 5,

        card(
          input_dropdown(dropdown_id = ns("bike_prod_ap_yr_dd"),
                         shiny::selectInput(inputId = ns("bike_ord_prod_ref_plt_agg"),
                                            label = "Aggregate By",
                                            choice = agg_choice,
                                            selected = "sum")),
          shiny::br(),

          highcharter::highchartOutput(outputId = ns("bike_ord_prod_ref_plot")),

          class = "mh-544"
        )
      )
    ),

    shiny::br(),

    shiny::fluidRow(
      shiny::column(
        width = 7,

        card(
          input_dropdown(dropdown_id = ns("bike_prod_at_yr_dd"),
                         shiny::selectInput(inputId = ns("bike_ord_age_group_month_input"),
                                            label = "Month",
                                            choices = NULL),

                         shiny::selectInput(inputId = ns("bike_ord_age_group_month_agg"),
                                            label = "Aggregate By",
                                            choice = agg_choice,
                                            selected = "sum")),

          highcharter::highchartOutput(outputId = ns("bike_ord_month_age_group")),

          class = "mh-466"
        )
      ),

      shiny::column(
        width = 5,

        card(
          reactable::reactableOutput(outputId = ns("bike_gender_qty_order")),

          class = "mh-466"
        )
      )
    ),

    shiny::br(),

    shiny::fluidRow(
      shiny::column(
        width = 12,

        card(
          input_dropdown(
            dropdown_id = ns("trend_bike_qty_dd"),
            shiny::selectInput(inputId = ns("bike_trend_qty_sub_cat"),
                               label = "Product Sub-Category",
                               choice = NULL),

            shiny::selectInput(inputId = ns("bike_trend_qty_prod"),
                               label = "Product",
                               choice = NULL)
          ),

          detail_button(btn_id = ns("bike_ord_trend_dt")),

          highcharter::highchartOutput(outputId = ns("bike_ord_trend"))
        )
      )
    ),

    shiny::br(), shiny::br()
  )
}



nav_bike_prod_income <- function(id) {
  ns <- shiny::NS(id)

  bslib::nav(
    title = "Income",
    icon = fontawesome::fa_i("fas fa-coins"),

    shiny::br(),

    shiny::fluidRow(
      shiny::column(
        width = 8,

        card(
          input_dropdown(dropdown_id = ns("bike_prod_rev_ap_dd"),
                         shiny::selectInput(inputId = ns("bike_rev_revenue_sumy_agg"),
                                            label = "Aggregate By",
                                            choice = agg_choice,
                                            selected = "sum")),

          reactable::reactableOutput(outputId = ns("bike_rev_revenue_summary")),

          class = "mh-520"
        )
      ),

      shiny::column(
        width = 4,

        card(
          input_dropdown(dropdown_id = ns("bike_prod_rev_quarter_dd"),
                         shinyWidgets::prettyRadioButtons(inputId = ns("bike_rev_qtr_rev"),
                                                          label = "",
                                                          choices = rev_choice,
                                                          selected = "profit",
                                                          fill = TRUE,
                                                          shape = "curve",
                                                          animation = "pulse",
                                                          bigger = TRUE,
                                                          inline = TRUE)),

          shiny::br(), shiny::br(),

          echarts4r::echarts4rOutput(outputId = ns("bike_rev_quarter_summary")),

          class = "mh-520"
        )
      )
    ),

    shiny::br(),

    shiny::fluidRow(
      shiny::column(
        width = 8,

        card(
          input_dropdown(dropdown_id = ns("bike_rev_prod_at_yr_dd"),
                         shiny::selectInput(inputId = ns("bike_rev_age_group_month_by"),
                                            label = "By",
                                            choices = rev_choice,
                                            selected = "profit"),

                         shiny::selectInput(inputId = ns("bike_rev_age_group_month_input"),
                                            label = "Month",
                                            choices = NULL),

                         shiny::selectInput(inputId = ns("bike_rev_age_group_month_agg"),
                                            label = "Aggregate By",
                                            choice = agg_choice,
                                            selected = "sum")),

          highcharter::highchartOutput(outputId = ns("bike_rev_month_age_group"))
        )
      ),

      shiny::column(
        width = 4,

        card(
          input_dropdown(dropdown_id = ns("bike_rev_age_group_dd"),
                         shiny::selectInput(inputId = ns("bike_rev_age_group_by"),
                                            label = "By",
                                            choices = rev_choice,
                                            selected = "profit"),

                         shiny::selectInput(inputId = ns("bike_rev_age_group_agg"),
                                            label = "Aggregate By",
                                            choice = agg_choice,
                                            selected = "sum")),

          detail_button(btn_id = ns("bike_rev_age_group_prod_btn"),
                        icon_line = FALSE),

          highcharter::highchartOutput(outputId = ns("bike_rev_age_group"))
        )
      )
    ),

    shiny::br(),

    shiny::fluidRow(
      shiny::column(
        width = 8,

        card(
          input_dropdown(dropdown_id = ns("bike_rev_country_state_dd"),
                         shiny::selectInput(inputId = ns("bike_rev_country_state_by"),
                                            label = "By",
                                            choices = rev_choice,
                                            selected = "profit"),

                         shiny::selectInput(inputId = ns("bike_rev_country_state_country"),
                                            label = "Country",
                                            choice = NULL)),

          reactable::reactableOutput(outputId = ns("bike_rev_country_state")),

          class = "mh-470"
        )
      ),

      shiny::column(
        width = 4,

        card(
          input_dropdown(dropdown_id = ns("bike_rev_state_dd"),
                         shiny::selectInput(inputId = ns("bike_rev_state_agg"),
                                            label = "Aggregate By",
                                            choices = agg_choice,
                                            selected = "sum")),

          highcharter::highchartOutput(outputId = ns("bike_rev_state")),

          class = "mh-470"
        )
      )
    ),

    shiny::br(),

    shiny::fluidRow(
      shiny::column(
        width = 12,

        card(
          input_dropdown(dropdown_id = ns("bike_rev_all_prod_trend_dd"),
                         shiny::selectInput(inputId = ns("bike_rev_all_prod_trend_by"),
                                            label = "By",
                                            choices = rev_choice,
                                            selected = "profit")),

          detail_button(btn_id = ns("bike_rev_all_prod_trend_btn")),

          highcharter::highchartOutput(outputId = ns("bike_rev_all_prod_trend"))
        )
      )
    ),

    shiny::br(),

    shiny::fluidRow(
      shiny::column(
        width = 12,

        card(
          input_dropdown(dropdown_id = ns("bike_rev_prod_trend_dd"),
                         shiny::selectInput(inputId = ns("bike_rev_prod_trend_sub_cat"),
                                            label = "Product sub-category",
                                            choices = NULL),

                         shiny::selectInput(inputId = ns("bike_rev_prod_trend_prod"),
                                            label = "Product",
                                            choice = NULL),

                         shiny::selectInput(inputId = ns("bike_rev_prod_trend_by"),
                                            label = "By",
                                            choice = rev_choice,
                                            selected = "profit")),

          detail_button(btn_id = ns("bike_rev_prod_trend_btn")),

          highcharter::highchartOutput(outputId = ns("bike_rev_prod_trend"))
        )
      )
    ),

    shiny::br(), shiny::br()
  )
}




mod_bike_ui <- function(id) {

  ns <- shiny::NS(id)

  shiny::tagList(

    bslib::navs_pill(

      nav_bike_prod_overview(id),
      nav_bike_prod_order(id),
      nav_bike_prod_income(id),


      bslib::nav_spacer(),

      bslib::nav_item(
        filter_year_dropdown(dropdowm_id = ns("bike_year_filter_dd_btn2"),
                             radio_year_id = ns("bike_year_filter"),
                             yr_choices = 2016:2021,
                             yr_select = 2021,
                             tooltip_dir = "left")
      )
    )
  )
}




mod_bike_server <- function(id, p_df) {
  stopifnot(shiny::is.reactive(p_df))

  moduleServer(
    id = id,

    module = function(input, output, session) {
      ns <- session$ns

      # data ----------------------------------------------------------------->>

      r_year <- shiny::reactive({ as.numeric(input$bike_year_filter) })

      fyp_df <- shiny::reactive({
        filter_r_data(df = p_df(),
                      f_year = r_year(),
                      f_product_category = "Bikes")
      })


      # ===== NAV 1 ============================================================
      # rev description card ------------------------------------------------->>
      # revenue --------------------------------------------------------------->
      output$bike_revenue_ow_card <- shiny::renderUI({
        rev_prod_desc <- get_product_desc(df = p_df(),
                                          rev_var = "revenue",
                                          f_product = "Bikes",
                                          f_year = r_year(),
                                          agg_fun = "sum")
        shiny::tagList(
          prod_description_card(title = "Revenue",
                                value = rev_prod_desc$value,
                                value_label = rev_prod_desc$value_lab,
                                change = rev_prod_desc$change,
                                change_label = rev_prod_desc$change_lab,
                                md_btn_id = ns("bike_prod_revenue_desc_dt"),
                                background_color = pd_card_light[1],
                                badge_bg = bike_pal_dark[1])
        )
      })

      shiny::observe({
        render_modal(
          apexcharter::apexchartOutput(outputId = ns("bike_rev_prod_month_desc"))
        )
      }) |>
        shiny::bindEvent(input$bike_prod_revenue_desc_dt)

      output$bike_rev_prod_month_desc <- apexcharter::renderApexchart({
        shiny::req(fyp_df())

        prod_month_summary(py_df = fyp_df(),
                           rev_var = "revenue",
                           agg_fun = "sum",
                           c_color = ov_card_light[1])
      })


      # cost ------------------------------------------------------------------>
      output$bike_cost_ow_card <- shiny::renderUI({
        cost_prod_desc <- get_product_desc(df = p_df(),
                                           rev_var = "cost",
                                           f_product = "Bikes",
                                           f_year = r_year(),
                                           agg_fun = "sum")
        shiny::tagList(
          prod_description_card(title = "Cost",
                                value = cost_prod_desc$value,
                                value_label = cost_prod_desc$value_lab,
                                change = cost_prod_desc$change,
                                change_label = cost_prod_desc$change_lab,
                                md_btn_id = ns("bike_prod_cost_desc_dt"),
                                background_color = pd_card_light[2],
                                badge_bg = bike_pal_dark[1])
        )
      })

      shiny::observe({
        render_modal(
          apexcharter::apexchartOutput(outputId = ns("bike_cost_prod_month_desc"))
        )
      }) |>
        shiny::bindEvent(input$bike_prod_cost_desc_dt)

      output$bike_cost_prod_month_desc <- apexcharter::renderApexchart({
        shiny::req(fyp_df())

        prod_month_summary(py_df = fyp_df(),
                           rev_var = "cost",
                           agg_fun = "sum",
                           c_color = ov_card_light[2])
      })


      # profit ---------------------------------------------------------------->
      output$bike_profit_ow_card <- shiny::renderUI({
        profit_prod_desc <- get_product_desc(df = p_df(),
                                             rev_var = "profit",
                                             f_product = "Bikes",
                                             f_year = r_year(),
                                             agg_fun = "sum")
        shiny::tagList(
          prod_description_card(title = "Profit",
                                value = profit_prod_desc$value,
                                value_label = profit_prod_desc$value_lab,
                                change = profit_prod_desc$change,
                                change_label = profit_prod_desc$change_lab,
                                md_btn_id = ns("bike_prod_profit_desc_dt"),
                                background_color = pd_card_light[3],
                                badge_bg = bike_pal_dark[1])
        )
      })

      shiny::observe({
        render_modal(
          apexcharter::apexchartOutput(outputId = ns("bike_profit_prod_month_desc"))
        )
      }) |>
        shiny::bindEvent(input$bike_prod_profit_desc_dt)

      output$bike_profit_prod_month_desc <- apexcharter::renderApexchart({
        shiny::req(fyp_df())

        prod_month_summary(py_df = fyp_df(),
                           rev_var = "profit",
                           agg_fun = "sum",
                           c_color = ov_card_light[3])
      })



      # transaction calender ------------------------------------------------->>
      observe({
        shiny::req(fyp_df())

        prod_sc <- distinct_values(df = fyp_df(), dis_var1 = "sub_category")

        updateSelectInput(session = session,
                          inputId = "bike_trans_days_sub_cat",
                          choices = prod_sc)
      })

      observe({
        shiny::req(fyp_df(), input$bike_trans_days_sub_cat)

        prod <- distinct_values(df = fyp_df(),
                                dis_var1 = "sub_category",
                                dis_var1_uq_value = input$bike_trans_days_sub_cat,
                                dis_var2 = "product")

        updateSelectInput(session = session,
                          inputId = "bike_trans_days_prod",
                          choices = prod)
      })

      output$bike_trans_days_calender <- echarts4r::renderEcharts4r({
        shiny::req(fyp_df(), input$bike_trans_days_sub_cat, input$bike_trans_days_prod)

        days_prod_purchase(py_df = fyp_df(),
                           sub_prod_cat = input$bike_trans_days_sub_cat,
                           prod = input$bike_trans_days_prod,
                           c_color = bike_cal)
      })


      # gender & age group summary ------------------------------------------->>
      output$bike_trans_age_gp_gender <- reactable::renderReactable({
        shiny::req(fyp_df())

        gender_age_group_count(py_df = fyp_df(),
                               t_color = c(bike_pal_dark[1], "#F0FFFF"))
      })

      # Customer Age --------------------------------------------------------->>
      # output$trans_age_group

      # Product Sub Categories ----------------------------------------------->>
      output$bike_trans_sub_cat <- echarts4r::renderEcharts4r({
        shiny::req(fyp_df())

        n_trans_prod_subcat(py_df = fyp_df(), b_color = bike_pal_dark[1])
      })

      # Product Location summary --------------------------------------------->>
      output$bike_trans_location <- highcharter::renderHighchart({
        shiny::req(fyp_df())

        number_trans_loc(py_df = fyp_df(), b_color = bike_pal_dark[1])
      })


      # ===== NAV 2 ============================================================
      # variable number ------------------------------------------------------>>
      # variable numbers ----------------------------------------------------->>
      var_a_num <- shiny::reactive({
        shiny::req(fyp_df())

        variable_numbers(fyp_df())
      })

      # product --------------------------------------------------------------->
      output$bike_var_num_products <- renderUI({
        req(var_a_num())

        shiny::tagList(
          variable_number_card(title = "Total",
                               m_icon = "cart-shopping",
                               value = var_a_num()$product,
                               subtitle = "Product")
        )
      })

      # country --------------------------------------------------------------->
      output$bike_var_num_country <- renderUI({
        req(var_a_num())

        shiny::tagList(
          variable_number_card(title = "Total",
                               m_icon = "location-pin",
                               value = var_a_num()$country,
                               subtitle = "Country")
        )
      })

      # state ----------------------------------------------------------------->
      output$bike_var_num_state <- renderUI({
        req(var_a_num())

        shiny::tagList(
          variable_number_card(title = "Total",
                               m_icon = "location-dot",
                               value = var_a_num()$state,
                               subtitle = "State")
        )
      })

      # average age ----------------------------------------------------------->
      output$bike_var_num_avg_age <- renderUI({
        req(var_a_num())

        shiny::tagList(
          variable_number_card(title = "Average",
                               m_icon = "user",
                               value = var_a_num()$avg_age,
                               subtitle = "Customer Age")
        )
      })


      # sub cat product average/total order ---------------------------------->>
      shiny::observe({
        shiny::req(fyp_df())

        at_prod <- distinct_values(fyp_df(), "sub_category")

        shiny::updateSelectInput(session = session,
                                 inputId = "bike_ord_prod_sub_cat",
                                 choices = at_prod)
      })

      bike_ord_prod_sumy_tbl <- shiny::reactive({
        shiny::req(fyp_df(), input$bike_ord_prod_sub_cat)

        sub_prod_qty_summary_tbl(py_df = fyp_df(),
                                 sub_cat = input$bike_ord_prod_sub_cat)
      })

      output$bike_ord_prod_ref_table <- reactable::renderReactable({
        shiny::req(bike_ord_prod_sumy_tbl())

        sub_prod_qty_summary(pys_df = bike_ord_prod_sumy_tbl(),
                             b_colors = c(bike_pal_dark[3],
                                          bike_pal_light[3],
                                          bike_pal_dark[1],
                                          bike_pal_light[3]))
      })

      # product year summary -------------------------------------------------->
      selected_prod <- shiny::reactive({
        shiny::req(bike_ord_prod_sumy_tbl())

        p_value <- reactable::getReactableState(outputId = "bike_ord_prod_ref_table",
                                                name = "selected")

        get_data_point(df = bike_ord_prod_sumy_tbl(),
                       row_index = p_value,
                       column_index = "product")
      })

      output$bike_ord_prod_ref_plot <- highcharter::renderHighchart({
        shiny::req(p_df(), selected_prod(), input$bike_ord_prod_sub_cat,
                   input$bike_ord_prod_ref_plt_agg)

        sub_prod_qty_year_summary(df = p_df(),
                                  prod_sub_cat = input$bike_ord_prod_sub_cat,
                                  prod = selected_prod(),
                                  agg_fun = input$bike_ord_prod_ref_plt_agg,
                                  l_color = bike_pal_dark[4])
      })



      # Customer age group summary by month ---------------------------------->>
      shiny::observe({
        shiny::req(fyp_df())

        months <- distinct_values(df = fyp_df(), dis_var1 = "month")

        shiny::updateSelectInput(session = session,
                                 inputId = "bike_ord_age_group_month_input",
                                 choices = months)
      })

      output$bike_ord_month_age_group <- highcharter::renderHighchart({
        shiny::req(fyp_df(), input$bike_ord_age_group_month_input,
                   input$bike_ord_age_group_month_agg)

        age_group_rev_month(py_df = fyp_df(),
                            variable = "order_quantity",
                            f_month = input$bike_ord_age_group_month_input,
                            agg_fun = input$bike_ord_age_group_month_agg,
                            color_theme = bike_pal_dark[-1])
      })


      # Gender order quantity summary ---------------------------------------->>
      output$bike_gender_qty_order <- reactable::renderReactable({
        req(fyp_df())

        gender_qty_order_summary(py_df = fyp_df(),
                                 b_color = c(bike_pal_dark[3],
                                             bike_pal_light[3],
                                             bike_pal_dark[1],
                                             bike_pal_light[3]))
      })


      # quantity order trend ------------------------------------------------->>
      observe({
        shiny::req(fyp_df())

        prod_sc_trend <- distinct_values(df = fyp_df(), dis_var1 = "sub_category")

        updateSelectInput(session = session,
                          inputId = "bike_trend_qty_sub_cat",
                          choices = prod_sc_trend)
      })

      observe({
        shiny::req(fyp_df(), input$bike_trans_days_sub_cat)

        prod_trend <- distinct_values(df = fyp_df(),
                                      dis_var1 = "sub_category",
                                      dis_var1_uq_value = input$bike_trend_qty_sub_cat,
                                      dis_var2 = "product")

        updateSelectInput(session = session,
                          inputId = "bike_trend_qty_prod",
                          choices = prod_trend)
      })


      output$bike_ord_trend <- highcharter::renderHighchart({
        var_prod_trend(py_df = fyp_df(),
                       var = "order_quantity",
                       sub_cat = input$bike_trend_qty_sub_cat,
                       prod = input$bike_trend_qty_prod,
                       agg_fun = "sum",
                       l_color = bike_pal_dark[1])
      }) |>
        shiny::debounce(800)

      shiny::observe({
        tr_months <- distinct_values(df = fyp_df(), dis_var1 = "month")

        render_modal(
          shiny::fluidRow(
            shiny::column(
              width = 4,

              selectInput(inputId = ns("bike_month_qty_trend_input"),
                          label = "Month",
                          choices = tr_months)
            )
          ),

          shiny::fluidRow(
            shiny::column(
              width = 12,

              highcharter::highchartOutput(outputId = ns("bike_ord_month_trend"))
            )
          ),

          m_size = "l"
        )
      }) |>
        shiny::bindEvent(input$bike_ord_trend_dt)

      output$bike_ord_month_trend <- highcharter::renderHighchart({
        var_prod_trend(py_df = fyp_df(),
                       var = "order_quantity",
                       sub_cat = input$bike_trend_qty_sub_cat,
                       prod = input$bike_trend_qty_prod,
                       f_month = input$bike_month_qty_trend_input,
                       agg_fun = "sum",
                       l_color = bike_pal_dark[1])
      })


      # ===== NAV 3 ============================================================
      # revenue summary ------------------------------------------------------>>
      output$bike_rev_revenue_summary <- reactable::renderReactable({
        shiny::req(fyp_df(), input$bike_rev_revenue_sumy_agg)

        prod_rev_summary(py_df = fyp_df(),
                         agg_fun = input$bike_rev_revenue_sumy_agg)
      })


      # revenue quarter summary ---------------------------------------------->>
      output$bike_rev_quarter_summary <- echarts4r::renderEcharts4r({
        shiny::req(fyp_df(), input$bike_rev_qtr_rev)

        rev_quarter(py_df =  fyp_df(),
                    rev_var = input$bike_rev_qtr_rev,
                    d_colors = bike_pal_dark[-1])
      })


      # customer age group --------------------------------------------------->>
      # revenue month --------------------------------------------------------->
      shiny::observe({
        shiny::req(fyp_df())

        rev_months <- distinct_values(df = fyp_df(), dis_var1 = "month")

        shiny::updateSelectInput(session = session,
                                 inputId = "bike_rev_age_group_month_input",
                                 choices = rev_months)
      })

      output$bike_rev_month_age_group <- highcharter::renderHighchart({
        shiny::req(fyp_df(), input$bike_ord_age_group_month_input,
                   input$bike_ord_age_group_month_agg,
                   input$bike_rev_age_group_month_by)

        age_group_rev_month(py_df = fyp_df(),
                            variable = input$bike_rev_age_group_month_by,
                            f_month = input$bike_rev_age_group_month_input,
                            agg_fun = input$bike_rev_age_group_month_agg,
                            color_theme = bike_pal_dark[-1])
      })


      # revenue agg summary --------------------------------------------------->
      output$bike_rev_age_group <- highcharter::renderHighchart({
        shiny::req(fyp_df(), input$bike_rev_age_group_by,
                   input$bike_rev_age_group_agg)

        age_group_var_summary(py_df = fyp_df(),
                              variable = input$bike_rev_age_group_by,
                              agg_fun = input$bike_rev_age_group_agg,
                              b_colors = bike_pal_dark[-1])
      })

      shiny::observe({
        shiny::req(fyp_df())

        ag_prod_sc <- distinct_values(df = fyp_df(), dis_var1 = "sub_category")

        render_modal(
          shiny::fluidRow(
            shiny::column(
              width = 4,

              shiny::selectInput(inputId = ns("bike_age_gp_sub_cat"),
                                 label = "Product sub-category",
                                 choices = ag_prod_sc)
            ),

            shiny::column(
              width = 4,

              shiny::selectInput(inputId = ns("bike_age_gp_prod"),
                                 label = "Product",
                                 choices = NULL)
            )
          ),

          shiny::fluidRow(
            shiny::column(
              width = 12,

              highcharter::highchartOutput(outputId = ns("bike_rev_age_group_prod"))
            )
          ),

          m_size = "l"
        )
      }) |>
        shiny::bindEvent(input$bike_rev_age_group_prod_btn)

      shiny::observe({
        shiny::req(fyp_df(), input$bike_age_gp_sub_cat)

        ag_prod <- distinct_values(df = fyp_df(),
                                   dis_var1 = "sub_category",
                                   dis_var1_uq_value = input$bike_age_gp_sub_cat,
                                   dis_var2 = "product")

        shiny::updateSelectInput(session = session,
                                 inputId = "bike_age_gp_prod",
                                 choices = ag_prod)
      })

      output$bike_rev_age_group_prod <- highcharter::renderHighchart({
        shiny::req(fyp_df(),
                   input$bike_rev_age_group_by,
                   input$bike_rev_age_group_agg,
                   input$bike_age_gp_sub_cat,
                   input$bike_age_gp_prod)

        age_group_var_summary(py_df = fyp_df(),
                              variable = input$bike_rev_age_group_by,
                              sub_cat = input$bike_age_gp_sub_cat,
                              prod = input$bike_age_gp_prod,
                              agg_fun = input$bike_rev_age_group_agg,
                              b_colors = bike_pal_dark[-1])
      }) |>
        shiny::debounce(800)


      # Country & state summary ---------------------------------------------->>
      shiny::observe({
        countries <- distinct_values(df = fyp_df(), dis_var1 = "country")

        shiny::updateSelectInput(session = session,
                                 inputId = "bike_rev_country_state_country",
                                 choices = countries)
      })

      bike_rev_country_state_tbl <- shiny::reactive({
        shiny::req(p_df(), input$bike_rev_country_state_country,
                   input$bike_rev_country_state_by)

        country_state_rev_tbl(df = p_df(),
                              rev_var = input$bike_rev_country_state_by,
                              f_year = r_year(),
                              f_country = input$bike_rev_country_state_country)
      })

      output$bike_rev_country_state <- reactable::renderReactable({
        shiny::req(p_df(), bike_rev_country_state_tbl(),
                   input$bike_rev_country_state_country,
                   input$bike_rev_country_state_by)

        country_state_rev_summary(cs_df = bike_rev_country_state_tbl(),
                                  df = p_df(),
                                  rev_var = input$bike_rev_country_state_by,
                                  f_year = r_year(),
                                  f_country = input$bike_rev_country_state_country,
                                  b_colors = c(bike_pal_dark[1], bike_pal_light[3]))
      })


      # state ----------------------------------------------------------------->
      selected_state <- shiny::reactive({
        shiny::req(bike_rev_country_state_tbl())

        s_value <- reactable::getReactableState(outputId = "bike_rev_country_state",
                                                name = "selected")

        get_data_point(df = bike_rev_country_state_tbl(),
                       row_index = s_value,
                       column_index = "state")
      })

      output$bike_rev_state <- highcharter::renderHighchart({
        shiny::req(p_df(), selected_state(),
                   input$bike_rev_country_state_by,
                   input$bike_rev_country_state_country,
                   input$bike_rev_state_agg)

        country_state_rev_month_summary(df = p_df(),
                                        rev_var = input$bike_rev_country_state_by,
                                        f_year = r_year(),
                                        f_state = selected_state(),
                                        f_country = input$bike_rev_country_state_country,
                                        agg_fun = input$bike_rev_state_agg,
                                        l_color = bike_pal_dark[3])
      })


      # all product revenue trend -------------------------------------------->>
      output$bike_rev_all_prod_trend <- highcharter::renderHighchart({
        shiny::req(fyp_df(), input$bike_rev_all_prod_trend_by)

        rev_prod_trend(py_df =  fyp_df(),
                       rev_var = input$bike_rev_all_prod_trend_by,
                       l_color = bike_pal_dark[2])
      })

      # product by months ----------------------------------------------------->
      shiny::observe({
        shiny::req(fyp_df())

        trend_months <- distinct_values(df = fyp_df(), dis_var1 = "month")

        render_modal(
          shiny::fluidRow(
            shiny::column(
              width = 4,

              shiny::selectInput(inputId = ns("bike_rev_all_month_trend_input"),
                                 label = "Month",
                                 choices = trend_months)
            )
          ),

          shiny::fluidRow(
            shiny::column(
              width = 12,

              highcharter::highchartOutput(outputId = ns("bike_rev_all_month_trend"))
            )
          ),

          m_size = "l"
        )
      }) |>
        shiny::bindEvent(input$bike_rev_all_prod_trend_btn)


      output$bike_rev_all_month_trend <- highcharter::renderHighchart({
        shiny::req(fyp_df(),
                   input$bike_rev_all_prod_trend_by,
                   input$bike_rev_all_month_trend_input)

        rev_prod_trend(py_df =  fyp_df(),
                       rev_var = input$bike_rev_all_prod_trend_by,
                       f_month = input$bike_rev_all_month_trend_input,
                       l_color = bike_pal_dark[2])
      })


      # Single product revenue summary --------------------------------------->>
      shiny::observe({
        shiny::req(fyp_df())

        prod_ssc_trend <- distinct_values(df = fyp_df(), dis_var1 = "sub_category")

        shiny::updateSelectInput(session = session,
                                 inputId = "bike_rev_prod_trend_sub_cat",
                                 choices = prod_ssc_trend)
      })

      shiny::observe({
        shiny::req(fyp_df(), input$bike_rev_prod_trend_sub_cat)

        prod_s_trend <- distinct_values(df = fyp_df(),
                                        dis_var1 = "sub_category",
                                        dis_var1_uq_value = input$bike_rev_prod_trend_sub_cat,
                                        dis_var2 = "product")

        shiny::updateSelectInput(session = session,
                                 inputId = "bike_rev_prod_trend_prod",
                                 choices = prod_s_trend)
      })

      output$bike_rev_prod_trend <- highcharter::renderHighchart({
        shiny::req(fyp_df(),
                   input$bike_rev_prod_trend_sub_cat,
                   input$bike_rev_prod_trend_prod,
                   input$bike_rev_prod_trend_by)

        var_prod_trend(py_df = fyp_df(),
                       var = input$bike_rev_prod_trend_by,
                       sub_cat = input$bike_rev_prod_trend_sub_cat,
                       prod = input$bike_rev_prod_trend_prod,
                       agg_fun = "sum",
                       l_color = bike_pal_dark[3])
      }) |>
        shiny::debounce(800)

      shiny::observe({
        shiny::req(fyp_df())

        tr_s_months <- distinct_values(df = fyp_df(), dis_var1 = "month")

        render_modal(
          shiny::fluidRow(
            shiny::column(
              width = 4,

              selectInput(inputId = ns("bike_rev_prod_trend_month_input"),
                          label = "Month",
                          choices = tr_s_months)
            )
          ),

          shiny::fluidRow(
            shiny::column(
              width = 12,

              highcharter::highchartOutput(outputId = ns("bike_rev_prod_trend_month"))
            )
          ),

          m_size = "l"
        )
      }) |>
        shiny::bindEvent(input$bike_rev_prod_trend_btn)

      output$bike_rev_prod_trend_month <- highcharter::renderHighchart({
        shiny::req(fyp_df(),
                   input$bike_rev_prod_trend_sub_cat,
                   input$bike_rev_prod_trend_prod,
                   input$bike_rev_prod_trend_by,
                   input$bike_rev_prod_trend_month_input)

        var_prod_trend(py_df = fyp_df(),
                       var = input$bike_rev_prod_trend_by,
                       sub_cat = input$bike_rev_prod_trend_sub_cat,
                       prod = input$bike_rev_prod_trend_prod,
                       f_month = input$bike_rev_prod_trend_month_input,
                       agg_fun = "sum",
                       l_color = bike_pal_dark[3])
      })
    }
  )
}
