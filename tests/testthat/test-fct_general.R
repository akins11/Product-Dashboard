pd_df <- vroom::vroom("data/prod.csv", delim = ",")


test_that("transform_data_type() return all expected data types", {
  testthat::local_edition(3)

  expect_equal(
    c("double"   , "double"   , "integer"  , "double"   , "integer"  , "integer",
      "character", "character", "character", "character", "character", "character",
      "double"   , "double"   , "double"   , "double"   , "double"   , "double",
      "integer"  , "integer"),

    transform_data_type(pd_df) |> sapply(typeof) |> unname()
  )
})




test_that("filter_r_data() return a filter data frame given the variable", {
  testthat::local_edition(3)

  expect_equal(
    2019,

    filter_r_data(df = pd_df, f_year = 2019) |> dplyr::distinct(year) |> dplyr::pull()
  )

  expect_equal(
    "Accessories",

    filter_r_data(df = pd_df, f_year = 2019, f_product_category = "Accessories") |>
      dplyr::distinct(product_category) |>
      dplyr::pull()
  )
})



test_that("clean_label() remove underscore & convert string case to title", {
  testthat::local_edition(3)

  expect_equal(
    "Quantity Order",

    clean_label("quantity_order")
  )
})



test_that("table_style() return expected results", {
  testthat::local_edition(3)

  expect_equal(
    10,
    table_style(type = "theme", cell_padding = 10)$cellPadding
  )

  expect_equal(
    20,
    table_style(type = "theme", header_font_size = 20)$headerStyle$fontSize
  )

  expect_equal(
    "#FFFFFF",
    table_style(type = "theme", header_color = "#FFFFFF")$headerStyle$color
  )

  expect_equal(
    14,
    table_style(type = "theme", tbl_font_size = 14)$tableStyle$fontSize
  )
})



test_that("distinct_values() return expected unique vectors", {
  testthat::local_edition(3)

  t_df <- dplyr::filter(pd_df, product_category == "Accessories")

  expect_equal(
    c("March", "May", "February", "January", "June", "July", "April"),

    distinct_values(df = t_df, dis_var1 = "month", f_year = 2021)
  )

  expect_equal(
    c("Bike Racks"      ,  "Bike Stands"    ,   "Bottles and Cages",
      "Cleaners"        ,  "Fenders"        ,   "Helmets",
      "Hydration Packs" ,  "Tires and Tubes"),

    distinct_values(df = t_df,
                    dis_var1 = "month",
                    dis_var1_uq_value = "February",
                    dis_var2 = "sub_category",
                    f_year = 2021)
  )
})


test_that("change_values() return the expected change value", {
  testthat::local_edition(3)

  expect_equal(
    -33.33,
    change_values(1000, 1500, "value")
  )

  expect_equal(
    "33.3%",
    change_values(2000, 1500, "label")
  )
})



test_that("assign_change_dis() returns the expected vector given the change vector", {
  testthat::local_edition(3)

  t_df <- data.frame(change = c(100, -200, 0))

  expect_equal(
    c("arrow-up", "arrow-down", "minus"),

    dplyr::mutate(t_df,
                  val = assign_change_dis(change, output_type = "icon")) |>
      dplyr::pull(val)
  )

  expect_equal(
    c("#00FA9A", "#E71D36", "#888888"),

    # NOTE that the function uses a hard coded color list (rf_pal) from the global.R file
    dplyr::mutate(t_df,
                  val = assign_change_dis(change, output_type = "color")) |>
      dplyr::pull(val)
  )
})


test_that("numeric_lab() return the expected character transformation", {
  testthat::local_edition(3)

  expect_equal(
    "$20.59",
    numeric_lab(20.589, "dollar")
  )

  expect_equal(
    "$120,258.00",
    numeric_lab(120258, "comma")
  )

  expect_equal(
    "26.87%",
    numeric_lab(0.2687, "percent")
  )

  expect_equal(
    "23.59",
    numeric_lab(23.587, "number")
  )

})
