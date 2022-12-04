#' Bootstrap 5 card
#'
#' @param ... ui element
#' @param border whether to include border.
#' @param class card extra class.
#'
#' @return div with bootstrap 5 card class.
#' @export
#'
#' @examples
card <- function(..., border = TRUE, class = NULL) {
  bd <- ifelse(border, "border border-1 bg-white c-border-grey-light", "c-bg-grey")

  if (!is.null(class)) {
    cls <- glue::glue("card shadow-none {bd} rounded-2 {class}")

  } else {
    cls <- glue::glue("card shadow-none {bd} rounded-2")
  }

  shiny::div(
    # class = "card shadow-none border border-1 rounded-2",
    class = cls,

    shiny::div(
      class = "card-body",

      ...
    )
  )
}



#' income description card
#'
#' @param title card title
#' @param change change from the previous year to the current year.
#' @param change_label change label.
#' @param spark_id spark-line plot id
#' @param md_btn_id detail modal button.
#' @param is_cost whether the value (change) is a product cost value.
#' @param background_color card background color.
#'
#' @return div with bootstrap 5 card class.
#' @export
#'
#' @examples
rev_description_card <- function(
    title = NULL,
    change = NULL,
    change_label = NULL,
    spark_id = NULL,
    md_btn_id = NULL,
    is_cost = FALSE,
    background_color = "#EEE9BF"
) {

  if (is.numeric(change)) {
    if (is_cost) {
      if (change_label < 0) {
        m_icon <- "arrow-up"

      } else if (change_label > 0) {
        m_icon <- "arrow-down"

      } else {
        m_icon <- "minus"
      }

    } else {
      if (change_label > 0) {
        m_icon <- "arrow-up"

      } else if (change_label < 0) {
        m_icon <- "arrow-down"

      } else {
        m_icon <- "minus"
      }
    }
  }

  shiny::div(
    class = "card shadow-none border border-0 rounded-2 py-0",
    style = glue::glue("background-color: {background_color}"),

    shiny::div(
      class = "card-body my-0 py-0 px-1",

      shiny::div(
        class = "d-flex justify-content-between my-1 pt-0",

        shiny::h5(
          class = "card-title fs-5 ps-1",

          title,
        ),

        detail_button(btn_id = md_btn_id,
                      icon_line = FALSE,
                      use_shiny_btn = TRUE,
                      btn_class = "align-self-center")
      ),

      shiny::div(
        apexcharter::sparkBoxOutput(outputId = spark_id, height = "140px")
      ),

      shiny::h5(
        class = "fs-5  ps-1",

        change_label,
        shiny::span(
          fontawesome::fa_i(glue::glue("fas fa-{m_icon}"))
        )
      )
    )
  )
}




#' product description card.
#'
#' @param title card title
#' @param value summary value.
#' @param value_label summary value label.
#' @param change change from the previous year to the current year.
#' @param change_label change label.
#' @param md_btn_id detail modal button.
#' @param background_color card background color.
#' @param badge_bg badge background color.
#'
#' @return div with bootstrap 5 card class.
#' @export
#'
#' @examples
prod_description_card <- function(
    title = NULL,
    value = NULL,
    value_label = NULL,
    change = NULL,
    change_label = NULL,
    md_btn_id = NULL,
    background_color = "#FFFFFF",
    badge_bg = "#FFFFFF"
) {

  if (change > 0) {
    m_icon <- "arrow-up"

    txt_color <- ch_pal$up

  } else if (change < 0)  {
    m_icon <- "arrow-down"

    txt_color <- ch_pal$down

  } else {
    m_icon <- "minus"

    txt_color <- ch_pal$no_ch
  }


  shiny::div(
    class = "card shadow-none border border-0 rounded-2",
    style = glue::glue("background-color: {background_color};"),

    shiny::div(
      class = "card-body pt-1 mb-3",

      shiny::div(
        class = "d-flex justify-content-between mb-2",

        shiny::h5(
          class = "card-title fs-3",

          title,
        ),

        detail_button(btn_id = md_btn_id,
                      icon_line = FALSE,
                      use_shiny_btn = TRUE,
                      btn_class = "align-self-center")
      ),

      shiny::div(
        class = "d-flex justify-content-center",

        shiny::div(
          class = "badge text-center fs-3 me-2", #bg-dark
          style = glue::glue("background-color: {badge_bg};"),

          value_label
        ),

        shiny::div(
          class = "fs-4 border-start border-dark ms-2 ps-4 pt-2",
          style = glue::glue("color: {txt_color};"),

          change_label,
          fontawesome::fa_i(glue::glue("fas fa-{m_icon}"))
        )
      )
    )
  )
}




#' ui input drop down
#'
#' @param dropdown_id id
#' @param ... any ui input except for shinyWidgets pickerInput.
#' @param db_placement drop down placement.
#' @param max_width maximum width of the drop down.
#' @param btn_class class of the drop down button.
#'
#' @return drop down with inputs.
#' @export
#'
#' @examples
input_dropdown <- function(dropdown_id,
                           ...,
                           db_placement = "bottom",
                           max_width = 300,
                           btn_class = "float-end") {

  shinyWidgets::dropMenu(
    shinyWidgets::actionBttn(inputId = dropdown_id,
                             label = "",
                             icon  = fontawesome::fa("fas fa-square-pen",
                                                     fill  = "#858585",
                                                     height = "1.2em",
                                                     width = "1.1em",
                                                     title = "Details"),
                             style = "bordered",
                             color = "default",
                             size  = "sm",
                             class = btn_class),

    ...,

    theme = "light",
    placement = db_placement,
    maxWidth = max_width
  )
}



#' period card
#'
#' @param dropdown_id drop down id.
#' @param title title of the card.
#' @param value summary value.
#' @param value_label summary value label.
#' @param subtitle card sub-title.
#' @param is_cost whether the value (change) is a product cost value.
#' @param box_width minimum width.
#'
#' @return div with bootstrap 5 card class.
#' @export
#'
#' @examples
period_diff_card <- function(dropdown_id,
                             title = "YoY",
                             value = NULL,
                             value_label = NULL,
                             subtitle = NULL,
                             is_cost = FALSE,
                             box_width = "100%") {

  if(!is.null(value)) {
    if (is_cost) {
      if (value > 0) {
        m_icon <- "fas fa-arrow-trend-down"
        d_color <- ch_pal$down

      } else if (value == 0) {
        m_icon <- "fas fa-minus"
        d_color <- ch_pal$no_ch

      } else if (value < 0) {
        m_icon <- "fas fa-arrow-trend-up"
        d_color <- ch_pal$up
      }
    } else {
      if (value < 0) {
        m_icon <- "fas fa-arrow-trend-down"
        d_color <- ch_pal$down

      } else if (value == 0) {
        m_icon <- "fas fa-minus"
        d_color <- ch_pal$no_ch

      } else if (value > 0) {
        m_icon <- "fas fa-arrow-trend-up"
        d_color <- ch_pal$up
      }
    }

    shiny::div(
      class = "card shadow-none border border-1 rounded-2", #border-dark
      style = glue::glue("min-width: {box_width}; border-color: {d_color} !important;"),

      shiny::div(
        class = "card-body",

        shiny::h5(
          class = "card-title",

          title
        ),

        shiny::div(
          class = "d-flex justify-content-center",

          shiny::h3(
            class = "badge text-center fw-bolder fs-3",
            style = glue::glue("background-color: #FCFCFC; color: {d_color};"),

            value_label,

            shiny::span(fontawesome::fa_i(m_icon))
          ),
        ),

        shiny::h6(
          class = "p-1",

          subtitle
        )
      )
    )

  } else {
    shiny::div(
      class = "card",

      shiny::div(
        class = "card-body",

        shiny::h5(
          class = "card-title",

          "--"
        ),

        shiny::div(
          class = "d-flex justify-content-center",

          shiny::h3(
            class = "badge bg-secondary text-center fw-bolder fs-3",

            "--%"
          ),
        ),

        shiny::h6(
          class = "p-1",

          "----"
        )
      )
    )
  }
}



#' variable number card.
#'
#' @param title title of the card.
#' @param m_icon card icon.
#' @param value count value.
#' @param subtitle card sub-title.
#'
#' @return div with bootstrap 5 card class.
#' @export
#'
#' @examples
variable_number_card <- function(title = NULL,
                                 m_icon = NULL,
                                 value = NULL,
                                 subtitle = NULL) {

  shiny::div(
    class = "card shadow-none border border-0 rounded-2",
    style = "background-color: #F7F7F7;",

    shiny::div(
      class = "card-body",

      shiny::p(
        class = "card-title p-1",

        title,

        shiny::span(fontawesome::fa_i(glue::glue("fas fa-{m_icon}")))
      ),

      shiny::div(
        class = "d-flex justify-content-center",

        shiny::div(
          style = "background-color: #F2F2F2;
                       width: 100px;
                       height: 100px;
                       border-radius: 50%;

                       align-items: center;
                       text-align: center;",
          shiny::h2(
            class = "text-center p-2",
            value
          )
        )
      ),

      shiny::h4(
        class = "float-end",

        subtitle
      )
    )
  )
}



#' nav page year filter.
#'
#' @param dropdowm_id drop down id.
#' @param radio_year_id radio button id.
#' @param yr_choices year choice.
#' @param yr_select default selected year.
#' @param tooltip_dir position of the drop-down.
#'
#' @return shinyWidget ui element.
#' @export
#'
#' @examples
filter_year_dropdown <- function(dropdowm_id,
                                 radio_year_id,
                                 yr_choices = 2016:2021,
                                 yr_select = 2021,
                                 tooltip_dir = "bottom") {

  shinyWidgets::dropMenu(
    shinyWidgets::actionBttn(inputId = dropdowm_id,
                             label = "",
                             icon = fontawesome::fa("fas fa-filter",
                                                    fill  = "#000000",
                                                    height = "1.2em",
                                                    width = "1.1em",
                                                    title = "Filter Year"),
                             style = "simple",
                             color = "default",
                             size = "sm"),

    theme = "translucent",
    maxWidth = 160,
    placement = tooltip_dir,

    shinyWidgets::prettyRadioButtons(inputId = radio_year_id,
                                     label = shiny::h5("Filter Year"),
                                     choices = yr_choices,
                                     selected = yr_select,
                                     status = "primary",
                                     shape = "square",
                                     outline = TRUE,
                                     animation = "pulse",
                                     bigger = TRUE)
  )
}


#' Create model for ui output details.
#'
#' @param ... ui element to include in the modal.
#' @param m_size the size of the modal see shiny::modalDialog
#'
#' @return a ui modal.
#' @export
#'
#' @examples
render_modal <- function(..., m_size = "m") {

  shiny::tagList(
    shiny::showModal(
      shiny::modalDialog(
        ...,

        title = NULL,
        footer = NULL,
        size = m_size,
        easyClose = TRUE
      )
    )
  )
}




#' period ui input
#'
#' @param month_quarter_id toggle button.
#' @param month_picker_id month date picker input id.
#' @param quarter_id  quarter select input id.
#'
#' @return ui component.
#' @export
#'
#' @examples
change_period_ui <- function(month_quarter_id, month_picker_id, quarter_id) {
  shiny::tagList(
    shinyWidgets::prettyRadioButtons(
      inputId = month_quarter_id,
      label = "Period ?",
      choiceNames = c("Month", "Quarter"),
      choiceValues = c("month", "quarter"),
      selected = "month",
      status = "primary",
      shape = "square",
      outline = TRUE,
      animation = "pulse"
    ),

    shinyjs::hidden(
      shinyWidgets::airDatepickerInput(
        inputId = month_picker_id,
        label = "",
        view = "months",
        minView = "months",
        dateFormat = "MM yyyy",
        monthsField = "months"
      )
    ),

    shinyjs::hidden(
      shiny::selectInput(
        inputId = quarter_id,
        label = "",
        choices = NULL
      )
    )
  )
}



#' wrapper for shinyjs show function.
#'
#' @param id input id.
#'
#' @return
#' @export
#'
#' @examples
show_input <- function(id) {
  shinyjs::show(id = id, anim = TRUE, animType = "slide")
}

#' wrapper for shinyjs hide function.
#'
#' @param id input id.
#'
#' @return
#' @export
#'
#' @examples
hide_input <- function(id) {
  shinyjs::hide(id = id, anim = TRUE, animType = "slide")
}



#' input toggle button.
#'
#' @param inputId button input id.
#' @param value button active state.
#'
#' @return toggle button.
#' @export
#'
#' @examples
toggle_btn <- function(inputId, value = FALSE) {

  icon_up   <- fontawesome::fa_i("fas fa-circle-chevron-up")
  icon_down <- fontawesome::fa_i("fas fa-circle-chevron-down")

  icon_up$attribs$class   <- paste("icon", icon_up$attribs$class)
  icon_down$attribs$class <- paste("icon", icon_down$attribs$class)

  inputTag <- tags$input(id = inputId, type = "checkbox")

  if (!is.null(value) && value) {
    inputTag$attribs$checked <- "checked"
  }

  toggleTag <- tags$div(
    class = "form-group shiny-input-container",
    style = "padding-left: 92%;",

    tags$div(
      class = "pretty p-toggle",
      inputTag,
      class = "p-plain p-bigger p-icon",

      tags$div(
        class = "state p-on p-default",
        icon_up
      ),

      tags$div(
        class = "state p-off p-default",
        icon_down
      )
    )
  )

  shinyWidgets:::attachShinyWidgetsDep(toggleTag, "pretty")
}




#' modal button.
#'
#' @param btn_id button id.
#' @param toggle whether to use toggle button.
#' @param b_style button style see ?shinyWidgets::actionBtn
#' @param b_color button color see ?shinyWidgets::actionBtn
#' @param icon_line type of icon to use either a line chart if TRUE or bar chart when FALSE
#' @param use_shiny_btn whether to use base shiny button
#' @param btn_class button class.
#'
#' @return action/toggle ui button.
#' @export
#'
#' @examples
detail_button <- function(btn_id,
                          toggle = FALSE,
                          b_style = "simple",
                          b_color = "default",
                          icon_line = TRUE,
                          use_shiny_btn = FALSE,
                          btn_class = "float-end") {
  m_icon <- ifelse(icon_line, "fas fa-chart-line", "fas fa-chart-column")

  if (toggle) {

    toggle_btn(
      inputId = btn_id,
      value = FALSE
    )

  } else {
    if (use_shiny_btn) {
      shiny::actionButton(
        inputId = btn_id,
        label = "",
        class = glue::glue("{btn_class} btn-sm shadow-none"),
        icon = fontawesome::fa_i(m_icon),
        style = "background-color: transparent !important;"
      )

    } else {

      shinyWidgets::actionBttn(
        inputId = btn_id,
        label = "",
        icon = fontawesome::fa(m_icon,
                               fill  = "#858585",
                               height = "1.2em",
                               width = "1.1em",
                               title = "Details"),
        style = b_style,
        color = b_color,
        size  = "sm",
        class = btn_class
      )
    }

  }
}



#' ui input.
#'
#' @param dropdown_id drop down id
#' @param select_by_id variable select input id.
#' @param select_agg_id aggregate function select input id.
#'
#' @return drop down ui with select inputs.
#' @export
#'
#' @examples
agg_by_select_input_dd <- function(dropdown_id, select_by_id, select_agg_id) {
  input_dropdown(
    dropdown_id = dropdown_id,
    shiny::selectInput(inputId = select_by_id,
                       label = "By",
                       choices = rev_choice,
                       selected = "profit"),

    shiny::selectInput(inputId = select_agg_id,
                       label = "Aggregate function",
                       choices = agg_choice,
                       selected = "sum")
  )
}
