#' Clean Data type
#'
#' @param df product data
#'
#' @return tibble, data.frame
#' @export
#'
#' @examples transform_data_type(product_data)
#'
transform_data_type <- function(df) {
  df |>
    dplyr::mutate(date = as.Date(date),
                  month = factor(month, levels = month.name, ordered = TRUE),
                  month_abb = factor(month_abb, levels = month.abb, ordered = TRUE),
                  age = as.integer(age),
                  quarter = as.integer(quarter),
                  age_group = factor(age_group,
                                     levels = c("Youth", "Young Adults", "Adults", "Seniors")))
}



#' Filter by year of product category
#'
#' @param df product data.
#' @param f_year numeric year to filter by.
#' @param f_product_category product category to filter by.
#'
#' @return tibble with only filtered values.
#' @export
#'
#' @examples filter_r_data(df = product_data, f_year = 2021)
#'
filter_r_data <- function(df, f_year, f_product_category = NULL) {
  if (is.null(f_product_category)) {
    dplyr::filter(df, year == f_year)

  } else {
    dplyr::filter(df, year == f_year & product_category == f_product_category)
  }
}



#' clean string
#'
#' @description this removes underscore and convert a string case to title.
#'
#' @param label string to clean.
#'
#' @return transformed string.
#' @export
#'
#' @examples clean_label(label = "quantity_order")
#'
clean_label <- function(label) {
  stringr::str_replace_all(label, "_", " ") |>
    stringr::str_to_title()
}



#' Table theme and language
#'
#' @param type type of output, either "theme" or "lang".
#' @param cell_padding the amount of top, bottom, left & right padding for each cell.
#' @param info_text additional info for the table pagination.
#'
#' @return a react table output.
#' @export
#'
#' @examples reactable(theme = table_style(type = "theme"))
#'
table_style <- function(type = "theme",
                        cell_padding = 6,
                        header_font_size = 16,
                        header_color = "#666666",
                        tbl_font_size = 15,
                        info_text = "entries") {
  if (type == "theme") {
    reactable::reactableTheme(
      backgroundColor = "#ffffff",
      color = "#787878",
      borderWidth = "1px",
      borderColor = "#EDEDED",
      stripedColor = "#FCFCFC",
      cellPadding = cell_padding,

      cellStyle = list(display = "flex",
                       flexDirection = "column",
                       justifyContent = "center"
      ),

      tableStyle = list(fontSize = tbl_font_size),
      headerStyle = list(borderWidth = "1px",
                         padding = "5px",
                         background = "#FFFFFF",
                         borderColor = "#828282",
                         fontWeight = "600",
                         fontSize = header_font_size,
                         color = header_color),

      inputStyle = NULL,
      rowSelectedStyle = NULL,
      selectStyle = NULL,
      paginationStyle = NULL,

      # style = list(
      #   fontFamily = tbl_font_family
      # )
    )

  }  else if (type == "lang") {
    reactable::reactableLang(pageInfo = stringr::str_glue("{{rows}} {info_text}"),
                             pagePrevious = "\u276e",
                             pageNext = "\u276f")
  }
}


# collect month ===============================================================|
collect_months <- function(df, f_year) {

  if (!missing(f_year)) {
    df <- dplyr::filter(df, year == f_year)
  }

  df |>
    dplyr::distinct(month) |>
    dplyr::pull() |>
    as.character()
}



#' Collect unique values.
#'
#' @param df product data
#' @param dis_var1 (required) a character/factor variable from the product data to extract its unique values
#' @param dis_var1_uq_value a unique value from dis_var1
#' @param dis_var2 a character/factor variable from the product data to extract its unique values given the filtered dis_var1.
#' @param f_year a numeric year value to filter by.
#' @param f_month a character month value (no abbreviation) to filter by.
#'
#' @details When a variable is given to only dis_var1 argument then only unique values from dis_var1 will be returned else
#' unique values from dis_var2 after the unique value given to dis_var1_uq_value have been filtered.
#'
#' @return unique character vector.
#' @export
#'
#' @examples distinct_values(
#'  product_data,
#'  dis_var1 = "product_category",
#'  dis_var1_uq_value = "Bikes",
#'  dis_var2 = "product"
#' )
#'
distinct_values <- function(df,
                            dis_var1, dis_var1_uq_value = NULL, dis_var2 = NULL,
                            f_year = NULL, f_month = NULL) {

  if (!is.null(f_year)) {
    df <- dplyr::filter(df, year == f_year)
  }

  if (!is.null(f_month)) {
    df <- dplyr::filter(df, month == f_month)
  }

  if (!is.null(dis_var2) && !is.null(dis_var1_uq_value)) {
    df |>
      dplyr::filter(.data[[dis_var1]] == dis_var1_uq_value) |>
      dplyr::distinct(.data[[dis_var2]]) |>
      dplyr::pull() |>
      as.character()

  } else {
    df |>
      dplyr::distinct(.data[[dis_var1]]) |>
      dplyr::pull() |>
      as.character()
  }
}



#' Value change based on different periods.
#'
#' @param new_value  the recent value.
#' @param prev_value the previous value.
#' @param output_type type of output to return
#'
#' @return if output_type = value then a numeric change value else a character with the `%` sign.
#' @export
#'
#' @examples change_values(120, 100, "label")
#'
change_values <- function(new_value, prev_value, output_type = "value") {
  output_type <- rlang::arg_match0(output_type, c("value", "label"))

  if (output_type == "value") {
    purrr::map2_dbl(
      new_value, prev_value,

      function(nv, pv) {
        if (is.na(nv) || is.na(pv)) {
          0

        } else {
          if (pv != 0) {
            f_val <- (nv - pv) / pv
            round(f_val * 100, 2)

          } else {
            0
          }
        }
      }
    )

  } else if (output_type == "label") {
    purrr::map2_chr(
      new_value, prev_value,

      function(nv, pv) {
        if (is.na(nv) || is.na(pv)) {
          "( -- )"

        } else {
          if (pv != 0) {
            f_val <- (nv - pv) / pv

            scales::label_percent(0.1)(f_val)

          } else {
            "( -- )"
          }
        }
      }
    )
  }
}



#' Assign icon or color given a period change value.
#'
#' @param change_variable a variable in the data that represent the changes.
#' @param values a vector of 3 icons or colors to assign.
#' @param output_type the type of output to return. either 'color' or 'icon'
#'
#' @return a vector of colors or icon given the output type.
#'
#' @export
#'
#' @examples assign_change_dis(change, output_type = "icon")
#'
assign_change_dis <- function(change_variable, values = NULL, output_type) {
  output_type <- rlang::arg_match0(output_type, c("icon", "color"))

  if (is.null(values)) {
    if (output_type == "icon") {
      values <- c("arrow-up", "minus", "arrow-down")

    } else if (output_type == "color") {
      values <- c(rf_pal$up, rf_pal$no_change, rf_pal$down)
    }
  }

  dplyr::case_when(
    change_variable > 0  ~ values[1],
    change_variable == 0 ~ values[2],
    change_variable < 0  ~ values[3],
    TRUE ~ "empty"
  )
}



#' numeric label
#'
#' @param value numeric value
#' @param output_type type of output any of "percent", "dollar", "number" or "comma"
#'
#' @return character value.
#' @export
#'
#' @examples numeric_lab(2458974, "number")
#'
numeric_lab <- function(value, output_type = "dollar") {
  output_type <- rlang::arg_match0(output_type, c("dollar", "percent", "number", "comma"))
  if (output_type == "percent") {
    scales::label_percent(0.01)(value)

  } else if (output_type == "dollar") {
    scales::label_number(accuracy = 0.01,
                         prefix = "$",
                         scale_cut = scales::cut_short_scale())(value)

  } else if (output_type == "number") {
    scales::label_number(accuracy = 0.01,
                         scale_cut = scales::cut_short_scale())(value)

  } else if (output_type == "comma") {
    scales::label_comma(accuracy = 0.01,
                        prefix = "$")(value)
  }
}
