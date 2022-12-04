#' extract data point
#'
#' @param df product data for a single year filtered by product category and sub-category
#' @param row_index row index to extract from.
#' @param column_index column name to extract.
#'
#' @return a character vector.
#' @export
#'
#' @examples get_data_point(product_data, 3, "product")
#'
get_data_point <- function(df, row_index, column_index) {

  if (!is.null(row_index)) {
    f_val <- df[row_index, column_index][[column_index]]

    if (any(class(f_val) %in% c("ordered", "factor"))) {
      as.character(f_val)

    } else {
      f_val
    }
  }
}



#' product category unique transaction counts.
#'
#' @param py_df product data for a single year.
#' @param b_color bar color.
#'
#' @return an echarts4r htmlwidget object.
#' @export
#'
#' @examples n_trans_prod_subcat(product_data, "#3A5FCD")
#'
n_trans_prod_subcat <- function(py_df, b_color) {
  py_df |>
    dplyr::count(sub_category, name = "d_value") |>
    dplyr::arrange(d_value) |>

    echarts4r::e_charts(x = sub_category) |>
    echarts4r::e_bar(serie = d_value, legend = FALSE, name = "Transaction") |>
    echarts4r::e_flip_coords() |>
    echarts4r::e_color(b_color) |>
    echarts4r::e_grid(left = "20%") |>
    echarts4r::e_tooltip() |>
    echarts4r::e_title(text = "Number of Transaction By Product Sub Category",
                       textStyle = list(fontWeight = "normal",
                                        color = plt_title_clr))
}



#' number of unique transaction by country & state.
#'
#' @param py_df product data for a single year.
#' @param b_color bar color.
#'
#' @return
#' @export
#'
#' @examples number_trans_loc(product_data, "#3A5FCD")
#'
number_trans_loc <- function(py_df, b_color) {
  f_tbl <- py_df |>
    dplyr::count(country, name = "d_value") |>
    dplyr::arrange(dplyr::desc(d_value))

  f_tbl_drilldown <- py_df |>
    dplyr::count(country, state, name = "d_value") |>
    dplyr::arrange(dplyr::desc(d_value)) |>
    dplyr::group_nest(country) |>
    dplyr::mutate(id = country,
                  type = "bar",
                  data = lapply(data, \(.x) dplyr::mutate(.x, name = state, y = d_value)),
                  data = lapply(data, \(.x) highcharter::list_parse(.x)))

  highcharter::hchart(f_tbl,
                      type = "bar",
                      highcharter::hcaes(x = country,
                                         y = d_value,
                                         name = country,
                                         drilldown = country),
                      name = "Transaction",
                      color = b_color,
                      colorByPoint = FALSE) |>
    highcharter::hc_drilldown(allowPointDrilldown = TRUE,
                              series = highcharter::list_parse(f_tbl_drilldown),
                              activeAxisLabelStyle = list(cursor = "default",
                                                          color = b_color,
                                                          fontWeight = "normal",
                                                          textDecoration = "none")) |>
    highcharter::hc_yAxis(title = "") |>
    highcharter::hc_xAxis(title = "") |>
    highcharter::hc_title(text = "Number of Transaction By Location",
                          align = "left",
                          style = list(color = plt_title_clr)) |>
    highcharter::hc_subtitle(text = "Click on bar to <b>view by states<b>",
                             align = "left",
                             useHTML = TRUE,
                             style = list(color = "#EDEDED"))

}



#' income/cost summary.
#'
#' @param df product data for a single year.
#' @param rev_var variable to summarise.
#' @param f_year year to filter by.
#' @param f_product product category.
#' @param agg_fun aggregate function.
#'
#' @return a list containing the value, value_label, change, change_label values.
#' @export
#'
#' @examples get_product_desc(product_data, "profit" 2021, "Clothing"  )
#'
get_product_desc <- function(df, rev_var, f_year, f_product, agg_fun = "sum") {

  f_product <- rlang::arg_match0(f_product, c("Accessories", "Clothing", "Bikes"))

  fun <- rlang::as_closure(agg_fun)

  present_year <- df |>
    dplyr::filter(year == f_year & product_category == f_product) |>
    dplyr::summarise(fun(.data[[rev_var]])) |>
    dplyr::pull()

  if (f_product %in% c("Accessories", "Clothing") && f_year <= 2018) {
    previous_year <- 0

  }  else if (f_product == "Bikes" && f_year <= 2016) {
    previous_year <- 0

  } else if (f_product %in% c("Accessories", "Clothing") && f_year > 2018) {
    previous_year <- df |>
      dplyr::filter(year == f_year - 1 & product_category == f_product) |>
      dplyr::summarise(fun(.data[[rev_var]])) |>
      dplyr::pull()

  } else if (f_product == "Bikes" && f_year > 2016) {
    previous_year <- df |>
      dplyr::filter(year == f_year - 1 & product_category == f_product) |>
      dplyr::summarise(fun(.data[[rev_var]])) |>
      dplyr::pull()
  }

  #
  if (previous_year != 0) {
    dplyr::lst(
      value = present_year,
      value_lab = scales::label_number(accuracy =  0.01,
                                       prefix = "$",
                                       scale_cut = scales::cut_short_scale())(value),

      change = ((present_year - previous_year) / previous_year),
      change_lab = scales::label_percent(0.01)(change)
    )

  } else {
    dplyr::lst(
      value = present_year,
      value_lab = scales::label_number(accuracy =  0.01,
                                       prefix = "$",
                                       scale_cut = scales::cut_short_scale())(value),
      change = 0,
      change_lab = "0"
    )
  }
}



#' product monthly summary.
#'
#' @param py_df product data for a single year and product category.
#' @param rev_var variable to summarise
#' @param agg_fun aggregate function.
#' @param c_color bar color.
#'
#' @return an apexcharter htmlwidget object.
#' @export
#'
#' @examples prod_month_summary(product_data, "profit", "sum")
#'
prod_month_summary <- function(py_df, rev_var, agg_fun, c_color = "#1E90FF") {
  fun <- rlang::as_closure(agg_fun)

  var_lab <- stringr::str_to_title(rev_var)

  year <- unique(py_df$year)

  prod_cat <- unique(py_df$product_category)

  t_t <- glue::glue("{agg_labels[[agg_fun]]} {clean_label(rev_var)} Made In {year} From {prod_cat} Products")

  py_df |>
    dplyr::group_by(month_abb) |>
    dplyr::summarise("{var_lab}" := fun(.data[[rev_var]])) |>

    apexcharter::apex(apexcharter::aes(x = month_abb, y = .data[[var_lab]]), type = "column") |>
    apexcharter::ax_chart(toolbar = list(show = FALSE)) |>
    apexcharter::ax_colors(c_color) |>
    apexcharter::ax_tooltip(y = list(formatter = apexcharter::format_num(".2s"))) |>
    apexcharter::ax_yaxis(labels = list(formatter = apexcharter::format_num("~s"))) |>
    apexcharter::ax_labs(title = glue::glue("Total {clean_label(rev_var)}")) |>
    apexcharter::ax_title(style = list(color = plt_title_clr))
}



#' profit, cost & revenue summary by product
#'
#' @param py_df product data for a single year and product category.
#' @param agg_fun aggregate function.
#' @param b_colors table bar colors.
#'
#' @return a reactable htmlwidget object.
#' @export
#'
#' @examples prod_rev_summary(product_data, "mean")
#'
prod_rev_summary <- function(py_df, agg_fun, b_colors = rcp_pal) {
  fun <- rlang::as_closure(agg_fun)

  f_tbl <- py_df |>
    dplyr::group_by(product) |>
    dplyr::summarise(Revenue = fun(revenue),
                     Cost    = fun(cost),
                     Profit  = fun(profit)) |>
    dplyr::arrange(dplyr::desc(Profit))

  cus_data_bar <- function(df, bar_color, bg_color) {
    reactablefmtr::data_bars(
      data = df,
      text_position = "above",
      fill_color = bar_color,
      background = bg_color,
      number_fmt = scales::label_comma(prefix = "$"),
      text_color = "#878787"
    )
  }

  reactable::reactable(
    data = f_tbl,
    theme = table_style("theme",
                        header_font_size = 19,
                        header_color = tbl_title_clr,
                        cell_padding = 9),

    columns = list(

      product = reactable::colDef(
        name = "Product",
        minWidth = 130,
        style = list(fontSize = "16px",
                     color = "#6E6E6E"),
        sortable = FALSE
      ),

      Revenue = reactable::colDef(
        cell = cus_data_bar(f_tbl, b_colors$r[1], b_colors$r[2])
      ),

      Cost = reactable::colDef(
        cell = cus_data_bar(f_tbl, b_colors$c[1], b_colors$c[2])
      ),

      Profit = reactable::colDef(
        cell = cus_data_bar(f_tbl, b_colors$p[1], b_colors$p[2])
      )
    ),
    defaultPageSize = 8,
    showSortable  = TRUE,

    columnGroups = list(
      reactable::colGroup(name = agg_labels[[agg_fun]],
                          columns = c("Revenue", "Cost", "Profit"))
    ),

    language = table_style(type = "lang", info_text = "Products")
  )
}




#' Quantity order summary by product and product sub-category table.
#'
#' @param py_df product data for a single year and product category.
#' @param sub_cat product sub category.
#'
#' @return summarise tibble
#' @export
#'
#' @examples sub_prod_qty_summary_tbl(product_data, "Touring Bikes")
#'
sub_prod_qty_summary_tbl <- function(py_df, sub_cat) {
  py_df |>
    dplyr::filter(sub_category == sub_cat) |>
    dplyr::group_by(product) |>
    dplyr::summarise(involved= dplyr::n(),
                     average = mean(order_quantity),
                     total   = sum(order_quantity)) |>
    dplyr::arrange(dplyr::desc(total))
}



#' Quantity order summary by product and product sub-category plot
#'
#' @param pys_df product data summarized by products with a product sub-category.
#' @param b_colors table bar color.
#'
#' @return a reactable object.
#' @export
#'
#' @examples sub_prod_qty_summary(
#'    sub_prod_qty_summary_tbl(product_data, "Touring Bikes"),
#'    "#000080"
#' )
#'
sub_prod_qty_summary <- function(pys_df, b_colors) {

  cus_data_bar <- function(df, bar_color = NULL, bg_color = NULL) {
    reactablefmtr::data_bars(df,
                             text_position = "above",
                             number_fmt = scales::label_comma(),
                             fill_color = bar_color,
                             background = bg_color,
                             round_edges = TRUE)
  }

  reactable::reactable(
    data = pys_df,
    theme = table_style(type = "theme",
                        header_color = tbl_title_clr,
                        header_font_size = 17),

    defaultColDef = reactable::colDef(header = \(value) clean_label(value)),

    columns = list(
      product = reactable::colDef(
        minWidth = 130,
        sortable = FALSE
      ),

      involved = reactable::colDef(
        align = "center"
      ),

      average = reactable::colDef(
        cell = cus_data_bar(pys_df, b_colors[1], b_colors[2])
      ),

      total = reactable::colDef(
        cell = cus_data_bar(pys_df, b_colors[3], b_colors[4])
      )
    ),

    selection = "single",
    defaultSelected = 1,
    onClick = "select",
    showSortable = TRUE,

    columnGroups = list(
      reactable::colGroup(name = "Order Quantity",  columns = c("average", "total"))
    ),

    language = table_style(type = "lang", info_text = "Products")
  )
}


#' product summary across all years sold in.
#'
#' @param df product data for product category.
#' @param prod_sub_cat product sub-category.
#' @param prod product to filter by.
#' @param agg_fun aggregate function.
#' @param l_color line color.
#'
#' @return a highcharter htmlwidget object
#' @export
#'
#' @examples sub_prod_qty_year_summary(product_data, "Touring Bikes", "Touring-1000 Blue, 46", "mean", "#4169E1")
#'
sub_prod_qty_year_summary <- function(df, prod_sub_cat, prod, agg_fun, l_color) {
  fun <- rlang::as_closure(agg_fun)

  if (!is.null(prod)) {
    df |>
      dplyr::filter(sub_category == prod_sub_cat & product == prod) |>
      dplyr::group_by(year) |>
      dplyr::summarise(d_value = round(fun(order_quantity))) |>
      dplyr::mutate(year = as.character(year)) |>

      highcharter::hchart(highcharter::hcaes(x = year, y = d_value),
                          type = "area",
                          color = l_color,
                          name = glue::glue("{agg_labels[[agg_fun]]} Quantity")) |>
      highcharter::hc_title(text = glue::glue("{agg_labels[[agg_fun]]} Order For <b>{prod}</b> Product"),
                            align = "left",
                            useHTML = TRUE,
                            style = list(color = plt_title_clr)) |>
      highcharter::hc_yAxis(title = list(text = "")) |>
      highcharter::hc_xAxis(title = list(text = ""))
  }
}



#' Product transaction days
#'
#' @param py_df product data for a single year and product category.
#' @param sub_prod_cat product sub-category.
#' @param prod product to filter by.
#' @param c_color a list of calender colors.
#'
#' @return an echarts4r object.
#' @export
#'
#' @examples days_prod_purchase(product_data, "Touring Bikes", "Touring-3000 Yellow, 44", bike_cal)
#'
days_prod_purchase <- function(py_df, sub_prod_cat, prod, c_color) {
  yr <- unique(py_df$year)

  # arrange data
  py_df |>
    dplyr::filter(sub_category == sub_prod_cat & product == prod) |>
    dplyr::group_by(date) |>
    dplyr::summarise(d_value = dplyr::n()) |>
    dplyr::mutate(ds_value = dplyr::case_when(d_value <= 3  ~ d_value + 6,
                                              d_value <= 7  ~ d_value + 4,
                                              d_value <= 20 ~ d_value + 2,
                                              d_value > 20  ~ d_value + 0)) |>
    dplyr::group_by(months(date)) |>
    dplyr::mutate(de_value = dplyr::case_when(d_value == suppressWarnings(max(d_value)) ~ 18,
                                              TRUE ~ 0)) |>
    dplyr::ungroup() |>

    # create calender chart
    echarts4r::e_charts(x = date, height = 450) |>

    echarts4r::e_calendar(range = c(glue::glue("{yr}-01-01"), glue::glue("{yr}-06-30")),
                          top = 100,
                          left = "center",
                          cellSize = c(20, 18),
                          splitLine = list(show = TRUE,
                                           lineStyle = list(color = c_color$border,
                                                            width = 5,
                                                            type  = 'solid')),

                          itemStyle = list(color = c_color$bg,
                                           borderWidth = 1,
                                           borderColor = '#111111'),

                          yearLabel = list(formatter = '{start}  1st',
                                           color = c_color$ylab)) |>


    echarts4r::e_calendar(range = c(glue::glue("{yr}-07-01"), glue::glue("{yr}-12-31")),
                          top = 270,
                          left = "center",
                          cellSize = c(20, 18),
                          splitLine = list(show = TRUE,
                                           lineStyle = list(color = c_color$border,
                                                            width = 6,
                                                            type  = 'solid')),

                          itemStyle = list(color = c_color$bg,
                                           borderWidth = 1,
                                           borderColor = '#111111'),

                          yearLabel = list(formatter = '{start}  2nd',
                                           color = c_color$ylab)) |>

    echarts4r::e_scatter(serie = d_value,
                         size = ds_value,
                         coord_system = "calendar",
                         symbol = "roundRect",
                         name = "Transaction",
                         legend = TRUE,
                         itemStyle = list(color = c_color$trans)) |>

    echarts4r::e_scatter(serie = d_value,
                         size = ds_value,
                         coord_system = "calendar",
                         calendarIndex = 1,
                         symbol = "roundRect",
                         name = "Transaction",
                         legend = TRUE,
                         itemStyle = list(color = c_color$trans)) |>

    echarts4r::e_scatter(serie = d_value,
                         size = de_value,
                         coord_system = "calendar",
                         name = "Top",
                         legend = TRUE,
                         itemStyle = list(color = c_color$top)) |>

    echarts4r::e_scatter(serie = d_value,
                         size = de_value,
                         coord_system = "calendar",
                         calendarIndex = 1,
                         name = "Top",
                         legend = TRUE,
                         itemStyle = list(color = c_color$top)) |>

    echarts4r::e_title(text = glue::glue("Transaction Per Day In {yr}"),
                       subtext = glue::glue("For '{prod}' Product"),
                       textStyle = list(fontWeight = "normal",
                                        color = plt_title_clr),
                       subtextStyle = list(color = plt_sub_title_clr))|>
    echarts4r::e_tooltip(trigger = "item") |>
    echarts4r::e_legend(top = 15,
                        right = 100,
                        data = c("Transaction", "Top"),
                        textStyle = list(color = "#8A8A8A"))
}



#'
#'
#' @param py_df product data for a single year and product category.
#' @param variable variable to summarise by.
#' @param sub_cat product sub-category.
#' @param prod product to filter by.
#' @param agg_fun aggregate function.
#' @param b_colors bar colors.
#'
#' @return a highcharter object
#' @export
#'
#' @examples age_group_var_summary(product_data, "profit", "Touring Bikes", "Touring-3000 Yellow, 44", "sum", c(blue, green, red, black))
#'
age_group_var_summary <- function(py_df, variable, sub_cat, prod, agg_fun, b_colors) {

  if (!missing(sub_cat) && !missing(prod)) {

    py_df <- py_df |>
      dplyr::filter(sub_category == sub_cat & product == prod)
  }

  fun <- rlang::as_closure(agg_fun)
  # NOTE:: when variables is order_quantity, only sum function should be used.

  series_nm <- clean_label(variable)

  t_t_var <- ifelse(variable == "order_quantity", "Order", clean_label(variable))

  t_t <- glue::glue("{agg_labels[[agg_fun]]} {t_t_var} By Customer Age Group")

  sub_tt <- ifelse(
    missing(sub_cat) & missing(prod),
    glue::glue("For <b>{unique(py_df$product_category)}</b> Product Category"),
    glue::glue("For <b>{clean_label(prod)}</b> Product")
  )

  py_df |>
    dplyr::group_by(age_group) |>
    dplyr::summarise(d_value = fun(.data[[variable]])) |>
    dplyr::mutate(b_color = dplyr::case_when(age_group == "Youth" ~ b_colors[1],
                                             age_group == "Young Adults" ~ b_colors[2],
                                             age_group == "Adults" ~ b_colors[3],
                                             age_group == "Seniors" ~ b_colors[4])) |>

    highcharter::hchart(highcharter::hcaes(x = age_group,
                                           y = d_value,
                                           color = b_color),
                        type = "column",
                        name = series_nm) |>
    highcharter::hc_xAxis(title = list(text = "")) |>
    highcharter::hc_yAxis(title = list(text = "")) |>
    highcharter::hc_title(text = t_t,
                          align = "left",
                          style = list(color = plt_title_clr)) |>
    highcharter::hc_subtitle(text = sub_tt,
                             align = "left",
                             useHTML = TRUE,
                             style = list(color = plt_sub_title_clr)) |>
    highcharter::hc_tooltip(valueDecimals = 2)
}



#' customer age summary for a particular month.
#'
#' @param py_df product data for a single year and product category.
#' @param variable variable to summarise by.
#' @param f_month month to summarise.
#' @param agg_fun aggregate function.
#' @param color_theme a set of 4 colors for each age group.
#'
#' @return highcharter object.
#' @export
#'
#' @examples age_group_rev_month(product_data, "revenue", "March", "min", bike_pal_dark[-4])
#'
age_group_rev_month <- function(py_df, variable, f_month, agg_fun, color_theme) {
  fun <- rlang::as_closure(agg_fun)

  yr <- unique(py_df$year)

  f_df <- py_df |>
    dplyr::filter(month == f_month)

  v_color <- color_theme[1:length(unique(f_df$age_group))]

  f_df |>
    dplyr::group_by(date, age_group) |>
    dplyr::summarise(d_value = fun(.data[[variable]]), .groups = "drop") |>

    highcharter::hchart(type = "line",
                        highcharter::hcaes(x = date, y = d_value, group = age_group),
                        color = v_color) |>
    highcharter::hc_tooltip(table = TRUE) |>
    highcharter::hc_plotOptions(series = list(marker = list(enabled = FALSE))) |>
    highcharter::hc_legend(layout = "proximate", align = "right") |>
    highcharter::hc_xAxis(title = list(text = NULL)) |>
    highcharter::hc_yAxis(title = list(text = "")) |>
    highcharter::hc_title(text = glue::glue("{agg_labels[[agg_fun]]} {clean_label(variable)} By Customer Age Group."),
                          align = "left",
                          style = list(color = plt_title_clr)) |>
    highcharter::hc_subtitle(text = glue::glue("During {f_month} {yr}"),
                             align = "left",
                             style = list(color = plt_sub_title_clr))
}



#' income/cost summary by country and state table.
#'
#' @param df product data for a single year and product category.
#' @param rev_var variable to summarise.
#' @param f_year year to filter by.
#' @param f_country country to filter by.
#'
#' @return summarise tibble by state.
#' @export
#'
#' @examples country_state_rev_tbl(product_data, "profit", 2021, "Canada")
#'
country_state_rev_tbl <- function(df, rev_var, f_year, f_country) {
  df |>
    dplyr::filter(year == f_year & country == f_country) |>
    dplyr::group_by(state) |>
    dplyr::summarise(minimum = min(.data[[rev_var]]),
                     average = mean(.data[[rev_var]]),
                     maximum = max(.data[[rev_var]]),
                     total   = sum(.data[[rev_var]])) |>
    dplyr::arrange(dplyr::desc(total))
}



#' income/cost summary by country and state plot
#'
#' @param cs_df country and state summarized product data.
#' @param df product data for a single year and product category.
#' @param rev_var variable to summarise
#' @param f_year year to filter by.
#' @param f_country country to filter by.
#' @param b_colors bar color.
#'
#' @return a reactable object,
#' @export
#'
#' @examples country_state_rev_summary(
#'    country_state_rev_tbl(product_data, "profit", 2021, "Canada"),
#'    "profit",
#'    2021,
#'    "Canada",
#'    c(grey25, grey 20)
#' )
#'
country_state_rev_summary <- function(cs_df, df, rev_var, f_year, f_country, b_colors) {

  if (f_year != 2016) {
    f_tbl <- cs_df |>
      dplyr::left_join(
        df |>
          dplyr::filter(year == f_year - 1 & country == f_country) |>
          dplyr::group_by(state) |>
          dplyr::summarise(prev_total = sum(.data[[rev_var]])),
        by = c("state" = "state")
      ) |>
      dplyr::mutate(change_val = change_values(total, prev_total),
                    change_lab = change_values(total, prev_total, output_type = "label"),
                    m_icon = assign_change_dis(change_val, output_type = "icon"),
                    m_color = assign_change_dis(change_val, output_type = "color")) |>
      dplyr::select(-change_val)

  } else {
    f_tbl <- cs_df |>
      dplyr::mutate(prev_total = 0, change_lab = " ", m_icon = " ", m_color = " ")
  }

  reactable::reactable(
    data = f_tbl,

    theme = table_style(type = "theme",
                        header_color = tbl_title_clr,
                        header_font_size = 16),

    defaultColDef = reactable::colDef(header = \(.x) clean_label(.x),
                                      align = "center"),

    columns = list(
      state = reactable::colDef(
        style = list(fontSize = "17px",
                     color = "#A3A3A3")
      ),

      minimum = reactable::colDef(
        headerStyle = list(color = "#9C9C9C", fontSize = "15px"),
        style = list(fontSize = "15px", color = "#B0B0B0") # light color
      ),

      average = reactable::colDef(
        format = reactable::colFormat(digits = 2, separators = TRUE),
        style = list(fontSize = "17px",
                     color = "#8A8A8A")
      ),

      maximum = reactable::colDef(
        headerStyle = list(color = "#9C9C9C", fontSize = "15px"),
        format = reactable::colFormat(digits = 0, separators = TRUE),
        style = list(fontSize = "15px", color = "#B0B0B0") # light color
      ),

      total = reactable::colDef(
        cell = reactablefmtr::data_bars(f_tbl,
                                        text_position = "above",
                                        fill_color = b_colors[1],
                                        background = b_colors[2],
                                        text_size = 15,
                                        text_color = "#8A8A8A",
                                        max_value = max(f_tbl$total),
                                        min_value = min(f_tbl$total),
                                        number_fmt = scales::label_comma(),
                                        round_edges = TRUE)
      ),

      prev_total = reactable::colDef(show = FALSE),

      change_lab = ifelse(
        f_year != 2016,

        list(
          reactable::colDef(
            name = " ",
            align = "center",
            cell = reactablefmtr::icon_sets(data = f_tbl,
                                            icon_ref = "m_icon",
                                            icon_color_ref = "m_color",
                                            icon_position = "right",
                                            icon_size = 17),
            sortable = FALSE
          )),

        list(reactable::colDef(show = FALSE))
      )[[1]],

      m_icon = reactable::colDef(show = FALSE),
      m_color = reactable::colDef(show = FALSE)
    ),

    selection = "single",
    defaultSelected = 1,
    onClick = "select",
    showSortable = TRUE,

    columnGroups = list(
      reactable::colGroup(name = glue::glue("{f_country} | {f_year}"),
                          columns = c("minimum", "average", "maximum", "total"),
                          align = "right",
                          headerStyle = list(color = "#C4C4C4",
                                             fontWeight = 200))
    ),

    language = table_style(type = "lang", info_text = "States")
  )
}



#' income/cost summary by country and state plot by month.
#'
#' @param df product data for a single year and product category.
#' @param rev_var variable to summarise.
#' @param f_year year to filter by.
#' @param f_state state to filter by.
#' @param f_country country to filter by.
#' @param agg_fun aggregate function.
#' @param l_color line color.
#'
#' @return a highcharter object.
#' @export
#'
#' @examples country_state_rev_month_summary(product_data, "cost", 2019, "Ontario", "Canada", "sum", "#2E8B57")
#'
country_state_rev_month_summary <- function(df, rev_var,
                                            f_year, f_state, f_country,
                                            agg_fun, l_color) {
  fun <- rlang::as_closure(agg_fun)

  f_tbl <- dplyr::filter(df, year == f_year & country == f_country & state == f_state)

  number_months <- unique(f_tbl$month) |> length()

  month <- ifelse(number_months > 3, "month_abb", "month")

  t_t <- glue::glue("{agg_labels[[agg_fun]]} {clean_label(rev_var)} From <b>{f_state}</b>, {f_country}")

  f_plt <- f_tbl |>
    dplyr::group_by(d_month = .data[[month]]) |>
    dplyr::summarise(d_value = fun(.data[[rev_var]])) |>

    highcharter::hchart(type = "line",
                        highcharter::hcaes(x = d_month, y = d_value),
                        name = clean_label(rev_var)) |>
    highcharter::hc_title(text = t_t,
                          align = "left",
                          useHTML = TRUE,
                          style = list(color = plt_title_clr)) |>
    highcharter::hc_xAxis(title = list(text = "")) |>
    highcharter::hc_yAxis(title = list(text = "")) |>
    highcharter::hc_plotOptions(line = list(color = l_color,
                                            marker = list(fillColor = "white",
                                                          lineWidth = 2,
                                                          lineColor = NULL)))


  if (agg_fun == "mean") {
    highcharter::hc_tooltip(f_plt, valueDecimals = 2)

  } else {
    f_plt
  }
}


#' Gender & product quantity order.
#'
#' @param py_df product data for a single year and product category.
#' @param b_color table bar color
#'
#' @return a reactable object.
#' @export
#'
#' @examples gender_qty_order_summary(product_data, "#241882")
#'
gender_qty_order_summary <- function(py_df, b_color) {
  f_tbl <- py_df |>
    dplyr::group_by(gender, product) |>
    dplyr::summarise(average = mean(order_quantity),
                     total = sum(order_quantity), .groups = "drop")
  reactable::reactable(
    data = f_tbl,
    theme = table_style(type = "theme", header_color = tbl_title_clr),

    defaultColDef = reactable::colDef(header = \(.x) clean_label(.x)),

    groupBy = "gender",

    columns = list(
      gender = reactable::colDef(minWidth = 53),

      product = reactable::colDef(minWidth = 150),

      average = reactable::colDef(
        cell = reactablefmtr::data_bars(data = f_tbl,
                                        text_position = "above",
                                        fill_color = b_color[1],
                                        background = b_color[2],
                                        number_fmt = scales::label_number(0.1),
                                        round_edges = TRUE),
        aggregate = "mean",
        format = reactable::colFormat(digits = 1, suffix = " | Overall Average")
      ),

      total = reactable::colDef(
        cell = reactablefmtr::data_bars(data = f_tbl,
                                        text_position = "above",
                                        fill_color = b_color[3],
                                        background = b_color[4],
                                        number_fmt = scales::label_comma(),
                                        round_edges = TRUE),
        aggregate = "mean",
        format = reactable::colFormat(digits = 1, separators = TRUE, suffix = " | Avg")
      )
    ),

    columnGroups = list(
      reactable::colGroup(name = "Quantity Order",
                          columns = c("average", "total"))
    ),

    language = table_style(type = "lang", info_text = "Rows"),
    paginateSubRows = TRUE
  )
}



#' income and cost trend for a year of month.
#'
#' @param py_df product data for a single year and product category.
#' @param rev_var variable to summarise
#' @param f_month month to filter by.
#' @param l_color line color.
#'
#' @return a highcharter html output.
#' @export
#'
#' @examples rev_prod_trend(product_data, "revenue", "July", "#00009C")
#'
rev_prod_trend <- function(py_df, rev_var, f_month = NULL, l_color) {

  if (!is.null(f_month)) {
    py_df <- dplyr::filter(py_df, month == f_month)

    t_t <- glue::glue("Total <b>{clean_label(rev_var)}</b> In <b>{f_month}</b> {unique(py_df$year)}")

  } else {
    py_df

    t_t <- glue::glue("Total <b>{clean_label(rev_var)}</b> During The Year {unique(py_df$year)}")
  }

  x <- c("Total |", "Average |")
  y <- c("${point.y}", "${point.average}")

  # chart_type <-

  f_tbl <- py_df |>
    dplyr::group_by(date) |>
    dplyr::summarise(total = sum(profit),
                     average = round(mean(profit), 1))

  f_plt <- f_tbl |>
    highcharter::hchart(type = ifelse(is.null(f_month), "spline", "area"),
                        highcharter::hcaes(x = date, y = total),
                        color = l_color,
                        name = "total") |>
    highcharter::hc_xAxis(title = list(text = "")) |>
    highcharter::hc_yAxis(title = list(text = "")) |>
    highcharter::hc_title(text = t_t,
                          useHTML = TRUE,
                          align = "left",
                          style = list(color = plt_title_clr)) |>
    highcharter::hc_tooltip(pointFormat = highcharter::tooltip_table(x, y),
                            useHTML = TRUE,
                            table = TRUE)

  if (is.null(f_month)) {
    f_plt |>
      highcharter::hc_annotations(min_max(f_tbl, "total"))

  } else {
    f_plt
  }
}



#' numeric variables trend for a year of month.
#'
#' @param py_df product data for a single year and product category.
#' @param var variable to summarise.
#' @param sub_cat product sub-category.
#' @param prod product.
#' @param f_month month to filter by.
#' @param agg_fun aggregate function.
#' @param l_color line color.
#'
#' @return a highcharter object.
#' @export
#'
#' @examples var_prod_trend(product_data, "profit", "Helmets", "Sport-100 Helmet, Red", "June", "sum", "#EE7621")
#'
var_prod_trend <- function(py_df,
                           var, sub_cat, prod, f_month = NULL,
                           agg_fun = "sum", l_color = "blue") {

  f_year <- unique(py_df$year)

  if (is.null(f_month)) {
    f_tbl <- py_df |>
      dplyr::filter(sub_category == sub_cat & product == prod)

    t_t <- glue::glue("<b>{prod}</b> {clean_label(var)} During {f_year}")

  } else {
    f_tbl <- py_df |>
      dplyr::filter(sub_category == sub_cat & product == prod & month == f_month)

    t_t <- glue::glue("<b>{prod}</b> {clean_label(var)} In <b>{f_month}</b> {f_year}")
  }

  fun <- rlang::as_closure(agg_fun)

  nm_label <- ifelse(var == "order_quantity", "Quantity", clean_label(var))

  f_tbl <- f_tbl |>
    dplyr::group_by(date) |>
    dplyr::summarise(d_value = fun(.data[[var]]))

  if (is.null(f_month)) {
    f_plt <- f_tbl |>
      highcharter::hchart(type = "spline",
                          highcharter::hcaes(x = date, y = d_value),
                          name = nm_label,
                          color = l_color)

  } else {
    f_plt <- f_tbl |>
      highcharter::hchart(type = "line",
                          highcharter::hcaes(x = date, y = d_value),
                          name = nm_label) |>
      highcharter::hc_plotOptions(line = list(color = l_color,
                                              marker = list(fillColor = "white",
                                                            lineWidth = 2,
                                                            lineColor = NULL)))
  }

  f_plt |>
    highcharter::hc_xAxis(title = list(text = "")) |>
    highcharter::hc_yAxis(title = list(text = "")) |>
    highcharter::hc_title(text = t_t,
                          useHTML = TRUE,
                          align = "left",
                          style = list(color = plt_title_clr))
}


#' variable summary in each quarter.
#'
#' @param py_df product data for a single year and product category.
#' @param rev_var variable to summarise.
#' @param d_colors pie colors.
#'
#' @return a echarts4r htmlwidget object.
#' @export
#'
#' @examples rev_quarter(product_data, "profit", c(blue, red, yellow, green))
#'
rev_quarter <- function(py_df, rev_var, d_colors) {

  py_df |>
    dplyr::group_by(quarter) |>
    dplyr::summarise(d_value = sum(.data[[rev_var]])) |>

    dplyr::mutate(quarter = dplyr::case_when(quarter == 1 ~ "First",
                                             quarter == 2 ~ "Second",
                                             quarter == 3 ~ "Third",
                                             quarter == 4 ~ "Fourth")) |>

    echarts4r::e_charts(x = quarter) |>
    echarts4r::e_pie(serie = d_value,
                     # legend = FALSE,
                     label = list(show = FALSE),
                     name = "Profit",
                     radius = c("40%", "80%"),
                     itemStyle = list(borderRadius = 2,
                                      borderColor = '#fff',
                                      borderWidth = 5)) |>
    echarts4r::e_color(d_colors) |>
    echarts4r::e_tooltip(
      formatter =  htmlwidgets::JS(
        "
          function(params)
          {
              return `<small>${params.name}</small>
                      <br/>Total: <strong>${echarts.format.addCommas(params.value)}</strong>
                      <br/>Percent: ${params.percent}%`
          }  "
      )
    ) |>
    echarts4r::e_title(text = glue::glue("{clean_label(rev_var)} Made in Each Quarter"),
                       textStyle = list(fontWeight = "normal",
                                        color = plt_title_clr)) |>
    echarts4r::e_legend(orient = "horizontal", bottom = 10)
}



#' Customer gender and age group summary.
#'
#' @param py_df product data for a single year and product category.
#' @param t_color table bar color.
#'
#' @return a reactable object.
#' @export
#'
#' @examples gender_age_group_count(product_data, "#00CED1")
#'
gender_age_group_count <- function(py_df, t_color) {
  f_tbl <- py_df |>
    dplyr::count(gender, age_group) |>
    dplyr::arrange(gender, dplyr::desc(n))

  reactable::reactable(
    data = f_tbl,
    theme = table_style(type = "theme", cell_padding = 8,
                        header_font_size = 15),
    defaultColDef = reactable::colDef(header = \(.x) clean_label(.x)),

    columns = list(
      gender = reactable::colDef(
        minWidth = 70,
        style = reactablefmtr::group_merge_sort("gender")
      ),

      n = reactable::colDef(
        name = "N-Transactions",
        minWidth = 110,
        cell = reactablefmtr::data_bars(data = f_tbl,
                                        text_position = "above",
                                        number_fmt = scales::label_comma(),
                                        round_edges = TRUE,
                                        fill_color = t_color[1],
                                        background = t_color[2])
      ),

      row_color = reactable::colDef(show = FALSE)
    )
  )
}
