pd_df <- vroom::vroom("data/prod.csv", delim = ",")

test_that("get_data_point() return a single data point", {
  testthat::local_edition(3)

  expect_equal(
    "Hitch Rack - 4-Bike",

    pd_df |>
      dplyr::filter(year == 2018 & product_category == "Accessories") |>
      get_data_point(3, "product")
  )
})


# n_trans_prod_subcat() snap shot test ========================================>

# number_trans_loc() snap shot test ===========================================>

test_that("get_product_desc() return a list of variable summary", {
  testthat::local_edition(3)

  expect_equal(
    list(value = 872246, value_lab = "$872.25K",
         change = -0.11877876, change_lab = "-11.88%"),

    get_product_desc(pd_df, "profit", 2020, "Clothing", "sum")
  )

  # When there is no previous year
  expect_equal(
    list(value = 899400, value_lab = "$899.40K",
         change = 0, change_lab = "0"),

    get_product_desc(pd_df, "profit", 2018, "Clothing", "sum")
  )

  # when data is not available for selected year
  expect_equal(
    list(value = 0, value_lab = "$0.00",
         change = 0, change_lab = "0"),

    get_product_desc(pd_df, "profit", 2016, "Clothing", "sum")
  )
})


# prod_month_summary() snap shot test =========================================>

# prod_rev_summary() snap shot test ===========================================>



test_that("sub_prod_qty_summary_tbl() return a data frame with expected rows and column", {
  testthat::local_edition(3)

  t_df <- dplyr::filter(pd_df, year == 2020, product_category == "Bikes")

  # number of rows
  expect_equal(
    22,

    sub_prod_qty_summary_tbl(t_df, "Touring Bikes") |> nrow()
  )

  # column names
  expect_equal(
    c("product", "involved", "average", "total"),

    sub_prod_qty_summary_tbl(t_df, "Touring Bikes") |> names()
  )
})


# sub_prod_qty_summary() snap shot test =======================================>


# days_prod_purchase() snap shot test =========================================>

# age_group_var_summary() snap shot test ======================================>


test_that("country_state_rev_tbl() return the appropriate number of row and column names",{
  testthat::local_edition(3)

  # number of rows
  expect_equal(
    2,

    country_state_rev_tbl(pd_df, "profit", 2021, "Canada") |> nrow()
  )

  # unique column names
  expect_equal(
    c("state", "minimum", "average", "maximum", "total"),

    country_state_rev_tbl(pd_df, "profit", 2021, "Canada") |> names()
  )
})


# country_state_rev_summary() snap shot test ==================================>

# country_state_rev_month_summary() snap shot test ============================>

# gender_qty_order_summary() snap shot test  ==================================>

# rev_prod_trend() snap shot test =============================================>

# var_prod_trend() snap shot test =============================================>

# rev_quarter() snap shot test ================================================>

# gender_age_group_count() snap shot test =====================================>
