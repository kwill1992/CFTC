# Libraries
library(tidyverse)
library(readxl)
library(stringr)
library(rvest)

# get financials
ctcf_financials <- read_xls("FinFutYY.xls")
ctcf_financials




# get Commodities 
ctcf_commodities <- read_xls("f_year.xls")
ctcf_commodities

ctcf_commodities <- ctcf_commodities %>% filter(str_detect(Market_and_Exchange_Names, "WTI FINANCIAL CRUDE OIL - NEW YORK MERCANTILE EXCHANGE"))


# webscraping for 10Y
webpage <- read_html("https://www.cftc.gov/sites/default/files/files/dea/cotarchives/2023/futures/deacbtlf102423.htm")
webpage
