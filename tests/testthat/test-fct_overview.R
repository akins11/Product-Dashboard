pd_df <- vroom::vroom("data/prod.csv", delim = ",")


test_that("overview_rev_summary() return the expected list", {
  testthat::local_edition(3)

  expect_equal(
    list(value = 9004332,
         value_label = "$9.00M",
         change = -0.091354828,
         change_label = "-9.14%"),

    overview_rev_summary(pd_df, "profit", 2021)
  )

  expect_equal(
    list(value = 6083587,
         value_label = "$6.08M",
         change = 0,
         change_label = "-"),

    overview_rev_summary(pd_df, "cost", 2016)
  )
})


# cs_sparkline() snap shot test ==============================================>


test_that("get_period_choosen_value() return the expected list or vector", {
  testthat::local_edition(3)

  t_df <- dplyr::filter(pd_df, year == 2018)

  expect_equal(
    list(min_date = as.Date("2018-01-01"),
         max_date = as.Date("2018-12-31")),

    get_period_choosen_value(t_df, "month")
  )

  expect_equal(
    c("Fourth Quarter", "Third Quarter",  "First Quarter",  "Second Quarter"),

    get_period_choosen_value(t_df, "quarter")
  )
})



test_that("get_yoy() return the expected list based on the period", {
  testthat::local_edition(3)

  expect_equal(
    list(value = 3.5178186,
         value_label = "351.78%",
         s_period = "February"),

    get_yoy(pd_df, "month", 17928, f_year = 2019, "sum")
  )

  expect_equal(
    list(value = 0,
         value_label = "0-0",
         s_period = "No previous year."),

    get_yoy(pd_df, "month", 16801, f_year = 2016, "sum")
  )

  expect_equal(
    list(value = 3.9641471,
         value_label = "396.41%",
         s_period = "First Quarter"),

    get_yoy(pd_df, "quarter", "First Quarter", f_year = 2019, "sum")
  )

  expect_equal(
    list(value = 0,
         value_label = "0-0",
         s_period = "No previous year."),

    get_yoy(pd_df, "quarter", "First Quarter", f_year = 2016, "sum")
  )
})



# country_age_group_summary() snap shot test ==================================>

test_that("top_overall_product() return the expected product list", {
  testthat::local_edition(3)

  expect_equal(
    list(product = "Sport-100 Helmet, Red",
         value = 397056,
         value_label = "397.06K"),

    top_overall_product(dplyr::filter(pd_df, year == 2019), "profit")
  )
})



test_that("valid_mtd_range() return the expected date range", {
  testthat::local_edition(3)

  expect_equal(
    list(min_date = as.Date("2020-01-01"),
         max_date = as.Date("2020-12-31")),

    valid_mtd_range(dplyr::filter(pd_df, year == 2020))
  )
})


# valid_mtd_values() test shiny ui ============================================>


test_that("get_mtd() retrun the expected list items", {
  testthat::local_edition(3)

  expect_equal(
    list(value = 9909624,
         value_label = "$9.91M",
         subtitle = "From `2020-01-01` to `2020-12-31`"),

    get_mtd(dplyr::filter(pd_df, year == 2020), "2020-01-01", "2020-12-31")
  )
})



test_that("percent_revenue() return the expected list of profit & cost precentage of revenue", {
  testthat::local_edition(3)

  expect_equal(
    list(percent_cost = 0.52773732,
         percent_profit = 0.47226268),

    percent_revenue(dplyr::filter(pd_df, year == 2019))
  )
})


# percent_rev_radial()  snap shot test ========================================>

# ov_product_category_summary() snap shot test ================================>

# ov_product_category_pv_year() snap shot test ================================>

# ov_top_5_state() snap shot test =============================================>

# ov_country_rev_summary() snap shot test =====================================>



test_that("min_max() return the expected list of minimum & maximum values", {
  testthat::local_edition(3)

  t_list <- dplyr::filter(s_df, year == 2019) |>
    dplyr::group_by(date) |>
    dplyr::summarise(profit = sum(profit)) |>
    min_max("profit")

  # maximum values
  expect_equal(1551398400000, t_list$labels[[1]]$point$x)
  expect_equal(75468, t_list$labels[[1]]$point$y)
  expect_equal("Max: $75.47K", t_list$labels[[1]]$text)

  # minimum values
  expect_equal(1.562976e+12, t_list$labels[[2]]$point$x)
  expect_equal(4370, t_list$labels[[2]]$point$y)
  expect_equal("Min: $4.37K", t_list$labels[[2]]$text)
})




# ov_rev_trend() snap shot test ===============================================>

# ov_rev_trend_month() snap shot test =========================================>

# age_distribution() snap shot test ===========================================>

test_that("variable_numbers() return a list of number of unique values of each variable", {
  testthat::local_edition(3)

  expect_equal(
    list(product = 137, country = 6, state = 49, avg_age = 35.2),

    variable_numbers(dplyr::filter(s_df, year == 2020))
  )
})

