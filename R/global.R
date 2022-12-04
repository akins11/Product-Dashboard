# libraries --------------------------------------------------------------------
# library(tidyverse)
# library(shiny)
# library(shinyWidgets)
# 
# library(dplyr)
# library(tidyr)
# library(purrr)
# library(glue)
# 
# library(apexcharter)
# library(echarts4r)
# library(highcharter)
# 
# library(reactable)
# library(reactablefmtr)


# label list ===================================================================
agg_labels <- list(min = "Minimum",
                   mean = "Average",
                   median = "Median",
                   max = "Maximum",
                   sum = "Total")

agg_choice <- c("Minumum" = "min", 
                "Average" = "mean", 
                "Maximum" = "max", 
                "Total" = "sum")


rev_choice <- c("Profit" = "profit", "Cost" = "cost", "Revenue" = "revenue")



quarter_label <- list(`1` = "First Quarter",
                      `2` = "Second Quarter",
                      `3` = "Third Quarter",
                      `4` = "Fourth Quarter")

# color ========================================================================
# verd_color <- c("#FCFFA4FF", "#F98C0AFF", "#BB3754FF", "#56106EFF", "#000004FF")
verd_color <- c("#FCFFA4FF", "#FFC300", "#003566", "#001D3D", "#000814") 

# profit cost & revenue --------------------------------------------------------
rcp <- c("#FF9F1C", "#E71D36", "#2EC4B6")

rcp_pal <- list(
  r = c("#FF9F1C", "#FF9F1C0F"),
  c = c("#E71D36", "#E71D361A"),
  p = c("#2EC4B6", "#2EC4B617")
)


# rise/fall --------------------------------------------------------------------
rf_pal <- list(
  up = "#00FA9A",
  no_change = "#888888",
  down = "#E71D36"
)

ch_pal <- list(
  up = "#006400",
  no_ch = "#737373",
  down = "#8E2323"
)


# Change colors -------------------------------------------------------------->>
change_pos <- "#00FF7F"
change_nue <- "#8A8A8A"
change_neg <- "#FF3030"


# product card colors ----------------------------------------------------------
pd_card_light <- c("#FF9F1C0F", "#E71D361A", "#2EC4B617") 


# Overview ---------------------------------------------------------------------
ov_card_light <- c("#FF9F1C80", "#E71D36B3", "#2EC4B699")


ov_main <- "#003566"
ov_main_dark <- "#001D3D"


# Accessories ------------------------------------------------------------------
access_pal_dark <- c("#0A0908", "#22333B", "#F2F4F3", "#A9927D", "#5E503F")
# NOTE: used index 2

access_pal_light <- c("#252323", "#70798C", "#F5F1ED", "#DAD2BC", "#A99985") 


access_cal <- list(
  border = "#0A0908",
  bg = "#22333B",
  trans = "#EEDD82",
  top = "#A99985",
  ylab = "#878787"
)




# Clothing ---------------------------------------------------------------------
cloth_pal_light <- c("#9D53FF", "#B29EF8", "#F8D9C6", "#FFB570", "#FB9649", "#D8BFD8") 

cloth_pal_dark <- c("#2A0052", "#BC87E8", "#FFD899", "#FF9E1F", "#FF801F") 


cloth_cal <- list(
  border = "#0A0908",
  bg = "#2A0052",
  trans = "#F8D9C6",
  top = "#FF801F",
  ylab = "#878787"
)


# Bike -------------------------------------------------------------------------
bike_pal <- c("#00B2CA", "#7DCFB6", "#FBD1A2", "#F79256") #|> scales::show_col()

bike_pal_dark <- c("#2F4F2F", "#8D99AE", "#30323D", "#8AEA92", "#E75A7C") #|> 

bike_pal_light <- c("#006077", "#83C5BE", "#EDF6F9", "#FFDDD2", "#E29578")

# scales::show_col(bike_pal_dark)

bike_cal <- list(
  border = "#30323D",
  bg = "#2F4F2F",
  trans = "#FFDDD2",
  top = "#8AEA92",
  ylab = "#878787"
)
# plot title -------------------------------------------------------------------
plt_title_clr <- "#C9C9C9"
plt_sub_title_clr <- "#D9D9D9"
tbl_title_clr <- "#BBBBBB"
