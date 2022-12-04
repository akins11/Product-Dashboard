#' Income Summary values.
#'
#' @param df product data.
#' @param rev_var the variable summary to extract, either "revenue", "cost" or "profit".
#' @param f_year the year to filter by.
#'
#' @details if f_year = 2016 which is the first year in the data no change value will be calculated so change will be zero.
#'
#' @return if f_year is not 2016, a list containing a value, value_label, change and change_value else just a value and value label with change of zero.
#' @export
#'
#' @examples overview_rev_summary(product_data, "profit", 2018)
#'
overview_rev_summary <- function(df, rev_var, f_year) {
  if (f_year >= 2016 && f_year <= 2021) {
    present_value <- df |>
      dplyr::filter(year == f_year) |>
      dplyr::summarise(sum(.data[[rev_var]])) |>
      dplyr::pull()

  } else {
    present_value <- 0
  }

  if (f_year > 2016 && f_year <= 2021) {
    previous_value <- df |>
      dplyr::filter(year == f_year - 1) |>
      dplyr::summarise(sum(.data[[rev_var]])) |>
      dplyr::pull()

    change <- (present_value - previous_value) / previous_value

  } else {
    change <- 0L
  }

  if (change == 0L) {
    cl <- "-"

  } else {
    cl <- scales::label_percent(0.01)(change)
  }

  dplyr::lst(
    value = present_value,
    value_label = scales::label_number(accuracy =  0.01,
                                       prefix = "$",
                                       scale_cut = scales::cut_short_scale())(value),
    change = change,
    change_label = cl
  )
}



#' A spark-line for overview income card.
#'
#' @param df product data.
#' @param variable the variable summary to plot.  either "revenue", "cost" or "profit".
#' @param title plot title, variable summary value
#' @param color color of the plot geom.
#' @param plot_type the type of plot.
#'
#' @return an apexcharter() htmlwidget object.
#' @export
#'
#' @examples cs_sparkline(product_data, "revenue", "1025.36K")
#'
cs_sparkline <- function(df,
                         variable,
                         title = "title",
                         color = "#000000",
                         plot_type = "area-spline") {
  spark <- df |>
    dplyr::group_by(year) |>
    dplyr::summarise("{variable}" := sum(.data[[variable]])) |>

    apexcharter::apex(apexcharter::aes(x = year, y = .data[[variable]]),
                      name = clean_label(variable),
                      type = plot_type,
                      auto_update = apexcharter::config_update(update_options = TRUE)) |>
    apexcharter::ax_chart(sparkline = list(enabled = TRUE), group = NULL) |>
    apexcharter::ax_yaxis(show = FALSE) |>
    apexcharter::ax_colors(color) |>
    apexcharter::ax_tooltip(y = list(formatter = apexcharter::format_num(","))) |>
    apexcharter::ax_title(text = title, style = list(fontSize = "20px"))

  # spark$x$sparkbox <- list(color = "#000000", background = "#FAFAD2")
  spark$sizingPolicy <- htmlwidgets::sizingPolicy(defaultWidth = "100%",
                                                  defaultHeight = "160px",
                                                  viewer.defaultHeight = "160px",
                                                  viewer.defaultWidth = "100%",
                                                  viewer.fill = FALSE,
                                                  knitr.figure = FALSE,
                                                  knitr.defaultWidth = "100%",
                                                  knitr.defaultHeight = "160px",
                                                  browser.fill = FALSE,
                                                  viewer.suppress = FALSE,
                                                  browser.external = TRUE,
                                                  padding = 15)
  return(spark)
}



#' Variable summary by month.
#'
#' @param fy_df product data of a single year.
#' @param variable the variable summary to plot.  either "revenue", "cost" or "profit".
#' @param c_color a single color for the bar.
#'
#' @return an apexcharter() htmlwidget object.
#'
#' @export
#'
#' @examples overview_rev_months_summary(product_data, "cost")
#'
overview_rev_months_summary <- function(fy_df, variable, c_color = "#1E90FF") {
  var_lab <- stringr::str_to_title(variable)

  fy_df |>
    dplyr::group_by(month_abb) |>
    dplyr::summarise("{var_lab}" := sum(.data[[variable]])) |>

    apexcharter::apex(type = "column",
                      mapping = apexcharter::aes(x = month_abb,
                                                 y = .data[[var_lab]])) |>
    apexcharter::ax_chart(toolbar = list(show = FALSE)) |>
    apexcharter::ax_colors(c_color) |>
    apexcharter::ax_yaxis(labels = list(formatter = apexcharter::format_num("~s"))) |>
    apexcharter::ax_tooltip(y = list(formatter = apexcharter::format_num(".2s"))) |>
    apexcharter::ax_title(text = glue::glue("Total {clean_label(variable)} By Month"),
                          style = list(color = plt_title_clr,
                                       fontWeight = 200))
}



#' unique available values.
#'
#' @details For either month or quarter for a particular year.
#'
#' @param df product data of a single year.
#' @param p_type the variable unique values to extract.
#'
#' @return a list of minimum and maximum data if p_type = "month" else a vector of quarter values.
#'
#' @export
#'
#' @examples get_period_choosen_value(product_data, "quarter")
#'
get_period_choosen_value <- function(df, p_type = "month") {

  f_df <- dplyr::filter(df, year == unique(df$year))

  if (p_type == "month") {
    f_df |>
      dplyr::summarise(min_date = min(date),
                       max_date = max(date)) |>
      as.list()

  } else {
    f_df |>
      dplyr::distinct(quarter) |>
      dplyr::mutate(val = dplyr::case_when(quarter == 1 ~ "First Quarter",
                                           quarter == 2 ~ "Second Quarter",
                                           quarter == 3 ~ "Third Quarter",
                                           quarter == 4 ~ "Fourth Quarter",
                                           TRUE ~ "All Quarters")) |>
      dplyr::pull(val)
  }
}



# overview_rev_months_summary(s_df, "revenue", 2020)

#  YoY filter by months/Quarter
#'
#' @param df product data.
#' @param period either month or quarter.
#' @param f_year year to filter by.
#' @param agg_fun an aggregate function.
#' @param period_value which particular month or quarter.
#'
#' @return a list containing value, value_label and s_period for selected period.
#'
#' @export
#'
#' @examples get_yoy(r_df, "month", "March", 2017)
#'
get_yoy <- function(df, period, period_value = NULL, f_year, agg_fun = "sum") {
  fun <- rlang::as_closure(agg_fun)

  if (!is.null(period_value)) {
    if (period == "month") {
      period_value <- months(as.Date(period_value, origin = "1970-01-01"))

    } else {
      if (period_value == "First Quarter") {
        period_value <- 1
      } else if (period_value == "Second Quarter") {
        period_value <- 2
      } else if (period_value == "Third Quarter") {
        period_value <- 3
      } else {
        period_value <- 4
      }
    }
  }

  if (f_year != 2016) {
    val <- lapply(
      c(f_year - 1, f_year),

      function(.x) {
        df |>
          dplyr::filter(year == .x & .data[[period]] == period_value) |>
          dplyr::summarise(value = fun(profit)) |>
          dplyr::pull()
      }
    )

    if (val[[1]] != 0) {
      dplyr::lst(
        value = (val[[2]] - val[[1]]) / val[[1]],
        value_label = numeric_lab(value, "percent"),
        s_period = ifelse(period == "month",
                          period_value,
                          quarter_label[[period_value]])
      )

    } else {
      list(
        value = 0,
        value_label = "0-0",
        s_period = glue::glue("Period not avaliable in {f_year- 1}.")
      )
    }

  } else {
    list(
      value = 0,
      value_label = "0-0",
      s_period = "No previous year."
    )
  }
}



#' Customer country and age group summary
#'
#' @param fy_df product data for a particular year.
#' @param rev_var  variable to summarise by.
#' @param agg_fun aggregate function.
#'
#' @return echarts4r htmlWidgets object.
#' @export
#'
#' @examples country_age_group_summary(product_data, "profit")
#'
country_age_group_summary <- function(fy_df, rev_var = NULL, agg_fun = "sum") {
  if (agg_fun == "count") {
    f_tbl <- dplyr::count(fy_df, country, age_group, name = "d_value")

    t_t <- "Number of transactions by country & age group"

  } else {
    if (!is.null(rev_var)) {
      fun <- rlang::as_closure(agg_fun)

      f_tbl <- fy_df |>
        dplyr::group_by(country, age_group) |>
        dplyr::summarise(d_value = fun(.data[[rev_var]]), .groups = "drop")

      t_t <- glue::glue("{agg_labels[[agg_fun]]} {clean_label(rev_var)} By Country & Customer Age Group")
    }
  }

  f_tbl |>
    echarts4r::e_charts(x = age_group) |>
    echarts4r:: e_heatmap(y = country,
                          z = d_value,
                          itemStyle = list(emphasis = list(shadowBlur = 10))) |>
    echarts4r::e_visual_map(d_value, color = verd_color) |>
    echarts4r::e_grid(left = "18%", right = "5%") |>
    echarts4r::e_x_axis(axisLabel = list(color = "#0F0F0F"),
                        name = "Age Group",
                        nameLocation = "center",
                        nameGap = 35) |>
    echarts4r::e_y_axis(axisLabel = list(color = "#0F0F0F"),
                        name = "Country",
                        nameLocation = "center",
                        nameGap = 100) |>
    echarts4r::e_tooltip(
      formatter = htmlwidgets::JS("
          function(params){
          return('<strong>' + echarts.format.addCommas(params.value[2]) + '</strong>')
          }
                                    ")
    ) |>
    echarts4r::e_title(text = t_t,
                       textStyle = list(color = plt_title_clr, fontWeight = "normal"))
}



#' Top product based on a particular metric.
#'
#' @param fy_df product data for a single year.
#' @param rev_var variable to summarise by.
#' @param agg_fun aggregate function.
#'
#' @return a list containing the product name, value and value label.
#'
#' @export
#'
#' @examples top_overall_product(product_data, "revenue", "mean")
#'
top_overall_product <- function(fy_df, rev_var, agg_fun = "sum") {
  fun <- rlang::as_closure(agg_fun)

  f_list <- fy_df |>
    dplyr::group_by(product) |>
    dplyr::summarise(value = fun(.data[[rev_var]]))  |>
    dplyr::slice_max(order_by = value) |>
    as.list()

  f_list$value_label <- numeric_lab(f_list$value, "number")

  return(f_list)
}





#' Valid MTD date range
#'
#' @param df product data of a single year.
#'
#' @return a list with the minimum and maximum date of the year.
#'
#' @export
#'
#' @examples valid_mtd_range(product_data)
#'
valid_mtd_range <- function(df) {
  # selection must be within the choosen year.

  list(
    min_date = min(df$date),
    max_date = max(df$date)
  )
}


#' Valid MTD value placement
#'
#' @param selected_range a vector of two dates to test.
#'
#' @return shinyWidgets toast ui
#'
#' @export
#'
#' @examples valid_mtd_values(c("2020-01-01", "2020-12-31"))
#'
valid_mtd_values <- function(selected_range) {
  if (!is.null(selected_range)) {
    if (length(selected_range) == 2) {
      if (selected_range[1] > selected_range[2]) {
        shinyWidgets::show_toast(
          title = "Wrong Range",
          text = "Start date should come before end date",
          timer = 4000,
          type = "warning",
          position  = "top-start",
          timerProgressBar = FALSE
        )

      } else if (selected_range[1] == selected_range[2]) {
        shinyWidgets::show_toast(
          title = "Wrong Range",
          text = "Start date and end date are the same",
          timer = 4000,
          type = "warning",
          timerProgressBar = FALSE,
          position  = "top-start"
        )
      }
    }
  }
}



#' Month to date.
#'
#' @param df product data for a single year.
#' @param start_date date to begin calculation.
#' @param end_date  final date of the calculation.
#' @param agg_fun aggregate function
#'
#' @return a list containing value value_label, and subtitle.
#' @export
#'
#' @examples get_mtd(r_df, "2020-11-22", "2020-12-21)
#'
get_mtd <- function(df, start_date, end_date, agg_fun = "sum") {

  if (!is.null(start_date) & !is.null(end_date)) {
    if (start_date > end_date) {
      list(0.0, "--", "From -- to --")
    } else if (start_date == end_date) {
      list(0.0, "--", "From -- to --")
    } else {
      fun <- rlang::as_closure(agg_fun)

      mtd <- df |>
        dplyr::filter(date >= start_date & date <= end_date) |>
        dplyr::summarise(value = fun(profit)) |>
        dplyr::pull()

      list(
        value = mtd,
        value_label = numeric_lab(mtd, "dollar"),
        subtitle = glue::glue("From `{start_date}` to `{end_date}`")
      )
    }
  }
}




#' The fraction of revenue for profit and cost values.
#'
#' @param df product data for a single year.
#'
#' @return a list containing both profit and cost fractions of revenue
#' @export
#'
#' @examples percent_revenue(product_data)
#'
percent_revenue <- function(df) {
  df |>
    dplyr::summarise(revenue = sum(revenue),
                     cost = sum(cost),
                     profit = sum(profit)) |>
    dplyr::mutate(percent_cost = cost / revenue,
                  percent_profit = profit / revenue) |>
    dplyr::select(dplyr::contains("percent")) |>
    as.list()
}


#' The fraction of revenue for profit or cost plot.
#'
#' @param percent_list percentage of revenue list
#' @param val either "profit" or "cost"
#' @param r_color color for the radial bar.
#' @param f_color color for the value label.
#'
#' @return an apexcharter() htmlwidgets object
#'
#' @export
#'
#' @examples percent_rev_radial(
#'    percent_list = percent_revenue(product_data),
#'    val = "profit"
#' )
#'
percent_rev_radial <- function(percent_list,
                               val,
                               r_color = "#912CEE",
                               f_color = "#000000") {
  r_lab <- clean_label(val)

  r_val <- paste0("percent_", val)
  plt_val <- round(percent_list[[r_val]] * 100, 2)

  apexcharter::apex(data = NULL,
                    type = "radialBar",
                    mapping = apexcharter::aes(x = glue::glue("{r_lab} % of Revenue"),
                                               y = plt_val)) |>
    apexcharter::ax_colors(r_color) |>
    apexcharter::ax_chart(background = "#FFFFFF",
                          foreColor = f_color)
}



#' Summary by product category
#'
#' @param fy_df product data for a single year.
#' @param rev_var variable to summarise by.
#' @param agg_fun aggregate function.
#'
#' @return a highcharter htmlwidget object.
#'
#' @export
#'
#' @examples ov_product_category_summary(product_data, "profit" "mean")
#'
ov_product_category_summary <- function(fy_df, rev_var, agg_fun) {
  fun <- rlang::as_closure(agg_fun)

  if (!is.null(rev_var)) {
    f_tbl <- fy_df |>
      dplyr::group_by(product_category) |>
      dplyr::summarise(value = fun(.data[[rev_var]]), .groups = "drop") |>
      dplyr::mutate(value = round(value, 2)) |>
      dplyr::arrange(desc(value))

    f_tbl_drilldown <- fy_df |>
      dplyr::group_by(product_category, month_abb) |>
      dplyr::summarise(value = fun(.data[[rev_var]]), .groups = "drop") |>
      dplyr::mutate(value = round(value, 2)) |>
      dplyr::group_nest(product_category) |>
      dplyr::mutate(id = product_category,
                    type = "column",
                    data = lapply(data, \(.x) dplyr::mutate(.x, name = month_abb, y = value)),
                    data = lapply(data, \(.x) highcharter::list_parse(.x)))

    tt <- highcharter::tooltip_table(clean_label(rev_var), "${point.value:,.0f}")

    highcharter::hchart(f_tbl,
                        "column",
                        highcharter::hcaes(x = product_category,
                                           y = value,
                                           name = product_category,
                                           drilldown = product_category),
                        name = "Overall",
                        color = ov_main,
                        colorByPoint = FALSE) |>
      highcharter::hc_drilldown(allowPointDrilldown = TRUE,
                                series = highcharter::list_parse(f_tbl_drilldown),
                                activeAxisLabelStyle = list(cursor = "default",
                                                            color = ov_main,
                                                            fontWeight = "normal",
                                                            textDecoration = "none")) |>
      highcharter::hc_tooltip(pointFormat = tt, useHTML = TRUE, valueDecimals = 2) |>
      highcharter::hc_yAxis(title = "") |>
      highcharter::hc_xAxis(title = "") |>
      highcharter::hc_title(text = glue::glue("{agg_labels[[agg_fun]]} {clean_label(rev_var)} by Product Category"),
                            align = "left",
                            style = list(color = plt_title_clr)) |>
      highcharter::hc_subtitle(text = "Click on bar to drilldowm <b>by month</b>",
                               align = "left",
                               useHTML = TRUE,
                               style = list(color = "#DEDEDE"))
  }
}



#' product category summary for each year
#'
#' @param df product data.
#' @param rev_var variable to summarise by.
#' @param f_year the current year.
#' @param agg_fun aggregate function
#'
#' @return a echarts4r htmlwidget object.
#'
#' @export
#'
#' @examples ov_product_category_pv_year(product_data, 2018, "sum")
#'
ov_product_category_pv_year <- function(df, rev_var, f_year, agg_fun) {

  if (!f_year %in% c(2016, 2021)) {
    yrs <- c(f_year - 1, f_year, f_year + 1)

  } else if (f_year == 2016) {
    yrs <- c(f_year, f_year + 1)

  } else if (f_year == 2021) {
    yrs <- c(f_year - 1, f_year)
  }

  fun <- rlang::as_closure(agg_fun)
  yrl <- as.character(yrs)

  if (!is.null(rev_var)) {
    e_plt <- df |>
      dplyr::filter(year %in% yrs) |>
      dplyr::group_by(product_category, year) |>
      dplyr::summarise(value = fun(.data[[rev_var]]), .groups = "drop") |>
      tidyr::pivot_wider(id_cols = product_category, names_from = year, values_from = value, values_fill = 0) |>
      dplyr::relocate(product_category, .after = yrl[length(yrl)]) |>
      dplyr::ungroup() |>

      echarts4r::e_charts()



    if (length(yrs) == 3) {
      e_plt |>
        echarts4r::e_parallel(yrl[1], yrl[2], yrl[3], product_category,
                              name = clean_label(rev_var),
                              opts = list(smooth = FALSE,
                                          lineStyle = list(color = ov_main,
                                                           width = 3)))
    } else if (length(yrs) == 2) {
      e_plt |>
        echarts4r::e_parallel(yrl[1], yrl[2], product_category,
                              name = clean_label(rev_var),
                              opts = list(smooth = FALSE,
                                          lineStyle = list(color = ov_main,
                                                           width = 3)))
    }
  }
}



#' Table for the top 5 state based on the income of cost generated.
#'
#' @param df product data.
#' @param rev_var income or cost variable to summarise by.
#' @param f_year year to filter by
#' @param agg_fun aggregate function.
#'
#' @return a reactable htmlwidget.
#'
#' @export
#'
#' @examples ov_top_5_state(product_data, "revenue", 2017)
#'
ov_top_5_state <- function(df, rev_var, f_year, agg_fun = "sum") {
  fun <- rlang::as_closure(agg_fun)

  if (!is.null(rev_var)) {
    f_tbl <- df |>
      dplyr::filter(year == f_year) |>
      dplyr::group_by(country, state) |>
      dplyr::summarise(d_value = fun(.data[[rev_var]]), .groups = "drop") |>
      dplyr::ungroup() |>
      dplyr::arrange(dplyr::desc(d_value)) |>
      dplyr::slice_head(n = 5)

    if (f_year != 2016) {
      c_tbl <- df |>
        dplyr::filter(country %in% dplyr::pull(f_tbl, country) &
                        state %in% dplyr::pull(f_tbl, state) &
                        year == f_year - 1) |>
        dplyr::group_by(country, state) |>
        dplyr::summarise(prv_value = fun(.data[[rev_var]]), .groups = "drop") |>
        dplyr::ungroup() |>
        dplyr::select(-country)

    } else {
      c_tbl <- dplyr::tibble(state = dplyr::pull(f_tbl, state),
                             prv_value = dplyr::pull(f_tbl, d_value))
    }


    f_tbl <- f_tbl |>
      dplyr::inner_join(c_tbl, by = c("state" = "state")) |>
      dplyr::mutate(change = round(((d_value - prv_value) / prv_value) * 100, 2),
                    c_icon = dplyr::case_when(change > 0  ~ "arrow-up",
                                              change < 0  ~ "arrow-down",
                                              change == 0 ~ "minus"),
                    c_icon_colr = dplyr::case_when(change > 0  ~ change_pos,
                                                   change < 0  ~ change_neg,
                                                   change == 0 ~ change_nue)) |>
      dplyr::select(-prv_value) |>
      dplyr::rename_with(clean_label)


    reactable::reactable(
      data = f_tbl,
      theme = table_style(type = "theme",
                          header_font_size = 16,
                          header_color = "#A3A3A3",
                          cell_padding = 17),

      defaultColDef = reactable::colDef(align = "center"),

      columns = list(
        Country = reactable::colDef(
          style = list(
            color = "#B0B0B0",
            fontSize = "13px"
          )
        ),
        State = reactable::colDef(
          style = list(
            color = "#888888",
            fontSize = "16px"
          )
        ),
        `D Value` = reactable::colDef(
          name = glue::glue("{agg_labels[[agg_fun]]} {clean_label(agg_fun)}"),
          cell = reactablefmtr::data_bars(
            data = f_tbl,
            text_position = "above",
            text_color = "#888888",
            text_size = 14,
            fill_color = ov_main,
            number_fmt = scales::label_number(0.01,
                                              prefix = "$",
                                              scale_cut = scales::cut_short_scale())
          )
        ),
        Change = reactable::colDef(
          show = ifelse(f_year != 2016, TRUE, FALSE),
          name = "Change (%)",
          cell = reactablefmtr::icon_sets(
            data = f_tbl,
            icon_ref = "C Icon",
            icon_color_ref = "C Icon Colr",
            icon_position = "right",
            icon_size = 15
          ),
          minWidth = 80
        ),

        `C Icon` = reactable::colDef(show = FALSE),
        `C Icon Colr` = reactable::colDef(show = FALSE)
      ),

      sortable = FALSE
    )
  }
}




#' income and cost summary by country.
#'
#' @param fy_df product data for a single year.
#' @param rev_var variable to summarise by.
#' @param agg_fun aggregate function.
#'
#' @return an echarts4r htmlwidget object.
#'
#' @export
#'
#' @examples ov_country_rev_summary(product_object, "cost", "sum")
#'
ov_country_rev_summary <- function(fy_df, rev_var, agg_fun) {
  fun <- rlang::as_closure(agg_fun)

  font_size <- ifelse(agg_fun == "sum", 10, 12)

  fy_df |>
    dplyr::group_by(country) |>
    dplyr::summarise(d_value = fun(.data[[rev_var]])) |>
    dplyr::ungroup() |>
    dplyr::arrange(d_value) |>

    echarts4r::e_charts(x = country) |>
    echarts4r::e_bar(serie = d_value, legend = FALSE, name = clean_label(rev_var)) |>
    echarts4r::e_flip_coords() |>
    echarts4r::e_grid(left = "20%") |>
    echarts4r::e_color(ov_main) |>
    echarts4r::e_title(text = glue::glue("{agg_labels[[agg_fun]]} {clean_label(rev_var)} By Country"),
                       textStyle = list(color = plt_title_clr,
                                        fontWeight = "normal")) |>
    echarts4r::e_x_axis(axisLabel = list(fontSize = font_size)) |>
    echarts4r::e_tooltip()
}




#' minimum & maximum values for highcharter line annotation.
#'
#' @param df product data.
#' @param rev_var variable to use in extracting the min and max value, this should be the same value summarised in the main plot.
#' @param x_axis_var teh name of the variable on the x axis.
#'
#' @return a list of list with annotation labels.
#'
#' @export
#'
#' @examples highcharter::hc_annotations(min_max(summarized_product_data, "profit"))
#'
min_max <- function(df, rev_var, x_axis_var = NULL) {

  agg_funs <- c("min", "max")

  # Get minimum and maximum values
  mm_l <- lapply(
    agg_funs,
    function(.x) {
      fun <- rlang::as_closure(.x)

      dplyr::filter(df, .data[[rev_var]] == fun(.data[[rev_var]])) |> as.list()
    }
  )

  names(mm_l) <- agg_funs

  if (is.null(x_axis_var)) {
    # change date to highcharter time-stamp
    mm_l$min$date <- highcharter::datetime_to_timestamp(as.Date(mm_l$min$date))
    mm_l$max$date <- highcharter::datetime_to_timestamp(as.Date(mm_l$max$date))

    mm_l_min_x_cord <- mm_l$min$date
    mm_l_max_x_cord <- mm_l$max$date

  } else {
    # if (any(class(mm_l$min[[x_axis_var]]) %in% c("ordered", "factor"))) {
    #   mm_l_min_x_cord <- as.character(mm_l$min[[x_axis_var]])
    #   mm_l_max_x_cord <- as.character(mm_l$max[[x_axis_var]])
    # }
    mm_l_min_x_cord <- mm_l$min[[x_axis_var]]
    mm_l_max_x_cord <- mm_l$max[[x_axis_var]]
  }

  # assign annotation labels
  mm_l$min$lab <- scales::label_number(
    0.01, prefix = "$", scale_cut = scales::cut_short_scale()
  )(mm_l$min[[rev_var]])

  mm_l$max$lab <- scales::label_number(
    0.01, prefix = "$", scale_cut = scales::cut_short_scale()
  )(mm_l$max[[rev_var]])


  list(
    labels = list(
      list(point = list(x = mm_l_max_x_cord,
                        y = mm_l$max[[rev_var]], xAxis = 0, yAxis = 0),
           text  = glue::glue("Max: {mm_l$max$lab}")),

      list(point = list(x = mm_l_min_x_cord,
                        y = mm_l$min[[rev_var]], xAxis = 0, yAxis = 0),
           text  = glue::glue("Min: {mm_l$min$lab}"))
    )
  )
}



#' Income/cost trend for a single year.
#'
#' @param df product data for a single year.
#' @param rev_var variable to summarise by.
#' @param agg_fun aggregate function.
#'
#' @return a highcharter htmlwidget object.
#'
#' @export
#'
#' @examples ov_rev_trend(product_data, "cost", "mean")
#'
ov_rev_trend <- function(df, rev_var, agg_fun) {

  fun <- rlang::as_closure(agg_fun)

  if (!is.null(rev_var)) {
    rev_var_sym <- rlang::ensym(rev_var)

    rev_lab <- clean_label(rev_var)

    f_year <- unique(df$year)

    f_tbl <- df |>
      dplyr::group_by(date) |>
      dplyr::summarise("{rev_var}" := fun(.data[[rev_var]]))

    f_tbl |>
      highcharter::hchart(type = "spline",
                          highcharter::hcaes(x = date, y = !!rev_var_sym),
                          color = ov_main_dark,
                          name = rev_lab) |>
      highcharter::hc_title(text = glue::glue("{agg_labels[[agg_fun]]} {rev_lab} During {f_year}"),
                            align = "left",
                            style = list(color = plt_title_clr)) |>
      highcharter::hc_yAxis(title  = list(text = "")) |>
      highcharter::hc_xAxis(title  = list(text = "")) |>
      highcharter::hc_annotations(min_max(f_tbl, rev_var))
  }
}



#' Income/cost trend for a single month within a year.
#'
#' @param df product data for a single year.
#' @param rev_var variable to summarise by.
#' @param f_month the month to filter by. make sure the month supplied is present in that particular year.
#' @param agg_fun aggregate function.
#'
#' @return a highcharter htmlwidget object.
#' @export
#'
#' @examples ov_rev_trend_month(product_data, "revenue", "January", "min")
#'
ov_rev_trend_month <- function(df, rev_var, f_month, agg_fun) {

  fun <- rlang::as_closure(agg_fun)

  if (!is.null(rev_var)) {
    rev_var_sym <- rlang::ensym(rev_var)

    ver_lab <- clean_label(rev_var)

    f_year <- unique(df$year)

    f_tbl <- df |>
      dplyr::filter(month == f_month) |>
      dplyr::group_by(date) |>
      dplyr::summarise("{rev_var}" := fun(.data[[rev_var]]))

    f_tbl |>
      highcharter::hchart(type = "spline",
                          highcharter::hcaes(x = date, y = !!rev_var_sym),
                          color = ov_main_dark,
                          name = ver_lab) |>
      highcharter::hc_title(text = glue::glue("{agg_labels[[agg_fun]]} {ver_lab} During {f_month} {f_year}"),
                            align = "left",
                            style = list(color = plt_title_clr)) |>
      highcharter::hc_yAxis(title  = list(text = "")) |>
      highcharter::hc_xAxis(title  = list(text = "")) |>
      highcharter::hc_annotations(min_max(f_tbl, rev_var))
  }
}



#' Age Distribution
#'
#' @param fy_df product data for a single year.
#' @param cus_age_group a unique customer age group.
#' @param include_gender whether to include customer gender.
#'
#' @return a echarts4r htmlwidgets object.
#' @export
#'
#' @examples age_distribution(product_data, "Young Adults", TRUE)
#'
age_distribution <- function(fy_df, cus_age_group = NULL, include_gender = FALSE) {

  f_year <- unique(fy_df$year)

  if (is.null(cus_age_group)) {
    title <- glue::glue("Customers Age Distribution In {f_year}")

  } else {
    fy_df <- dplyr::filter(fy_df, age_group == cus_age_group)

    title <- glue::glue("Customers Age Distribution In {f_year} For {cus_age_group} Age Group")
  }

  avg_age <- mean(fy_df$age)


  if (include_gender) {
    e_plt <- fy_df |>
      dplyr::mutate(ids = seq_len(dplyr::n())) |>
      tidyr::pivot_wider(id_cols = ids, names_from = gender, values_from = age) |>

      echarts4r::e_chart() |>
      echarts4r::e_density(serie = Male,
                           areaStyle = list(opacity = .4),
                           name = "Male",
                           symbol = "none",
                           legend = TRUE) |>

      echarts4r::e_density(serie = Female,
                           areaStyle = list(opacity = .4),
                           name = "Female",
                           symbol = "none",
                           legend = TRUE) |>

      echarts4r::e_mark_line(data = list(xAxis = 30.6),
                             title = "average",
                             symbol = "circle",
                             lineStyle = list(color = "#0D0D0D")) |>
      echarts4r::e_legend(right = 20,
                          top = 30) |>

      echarts4r::e_color(c("#00509D", "#FDC500"))

  } else {
    e_plt <- echarts4r::e_charts(data = fy_df) |>
      echarts4r::e_density(serie = age,
                           areaStyle = list(opacity = .4),
                           name = "Age",
                           symbol = "none",
                           legend = FALSE) |>

      echarts4r::e_color("#FF6B35") |>

      echarts4r::e_mark_line(data = list(xAxis = avg_age),
                             title = "average",
                             symbol = "circle",
                             lineStyle = list(color = "#0D0D0D"))
  }

  e_plt |>
    echarts4r::e_title(text = title,
                       textStyle = list(fontWeight = "normal",
                                        color = plt_title_clr)) |>
    echarts4r::e_axis_labels(x = "Age") |>

    echarts4r::e_y_axis(formatter = echarts4r::e_axis_formatter(style = "percent"))
}



#' Title
#'
#' @param fy_df product data for a single year.
#'
#' @return a list of the number of unique product, country, state, and average customer age
#' @export
#'
#' @examples variable_numbers(product_data)
#'
variable_numbers <- function(fy_df) {
  fy_df |>
    dplyr::summarise(product = dplyr::n_distinct(product),
                     country = dplyr::n_distinct(country),
                     state = dplyr::n_distinct(state),
                     avg_age = round(mean(age), 1)) |>
    as.list()
}

