# Libraries
library(tidyverse)
library(readxl)
library(stringr)
library(rvest) #for webscraping; it is also in tidyverse

# webscraping help
# https://r4ds.hadley.nz/webscraping
# https://preettheman.medium.com/best-web-scraping-packages-for-r-2787979b747
# https://scrapingant.com/blog/r-web-scraping
# https://steviep42.github.io/webscraping/book/
# https://statsandr.com/blog/web-scraping-in-r/
# https://www.zenrows.com/blog/web-scraping-r
# https://www.bigbookofr.com/chapters/getting%20cleaning%20and%20wrangling%20data
# https://oxylabs.io/blog/web-scraping-r
# https://brightdata.com/blog/how-tos/web-scraping-with-r

# 15 Sep 2024:  Might not need web-scraping as there is a new file each week.




# get financials
ctcf_financials <- read_xls("FinFutYY.xls")
ctcf_financials

# test



# get Commodities 
ctcf_commodities <- read_xls("f_year.xls")
ctcf_commodities

ctcf_commodities <- ctcf_commodities %>% filter(str_detect(Market_and_Exchange_Names, "WTI FINANCIAL CRUDE OIL - NEW YORK MERCANTILE EXCHANGE"))


# webscraping for 10Y
webpage <- read_html("https://www.cftc.gov/sites/default/files/files/dea/cotarchives/2023/futures/deacbtlf102423.htm")
webpage
webpage <- read_html("https://www.cftc.gov/dea/futures/other_lf.htm")
webpage
# txt file
txt.file <- webpage %>% html_element("other_lf.txt")
text_stuff <- webpage %>%  html_text2()
text_stuff
text_stuff2 <- webpage %>%  html_text()
text_stuff2



# Use .csv files
fullFile_FUT86_06 <- read_xls("FUT86_06.xls")
fullFile_FUT86_06
palladium_FUT86_06 <- fullFile_FUT86_06 %>% filter(str_detect( Market_and_Exchange_Names, "PALL"))

fullFile_FUT07_14 <- read_xls("FUT07_14.xls")
fullFile_FUT07_14
palladium_FUT07_14 <- fullFile_FUT07_14 %>% filter(str_detect( Market_and_Exchange_Names, "PALL"))

fullFile_FUT15_16 <- read_xls("FUT15_16.xls")
fullFile_FUT15_16
palladium_FUT15_16 <- fullFile_FUT15_16 %>% filter(str_detect( Market_and_Exchange_Names, "PALL"))

fullFile_FUT17 <- read_xls("FUT17.xls")
fullFile_FUT17
palladium_FUT17 <- fullFile_FUT17 %>% filter(str_detect( Market_and_Exchange_Names, "PALL"))

library(ggplot2)
gg <- ggplot(palladium_FUT15_16, aes(x=Report_Date_as_MM_DD_YYYY,y=Comm_Positions_Long_All)) +
  geom_line()
plot(gg)


#### change of direction on 15 Sep 2024:
# Downloadable text and Excel files for historical data is located here: 
# https://www.cftc.gov/MarketReports/CommitmentsofTraders/HistoricalCompressed/index.htm
# Go to section labeled: "Traders in Financial Futures ; Futures Only Reports:."

# Step 1: Choose a product and a year and get a visual of something
# Read Excel file
fullFile_FUT15_16 <- read_xls("FUT15_16.xls")
fullFile_FUT15_16
#Get just palladium
palladium_FUT15_16 <- fullFile_FUT15_16 %>% filter(str_detect( Market_and_Exchange_Names, "PALL"))

# Plot long commercial positions vs date
library(ggplot2)
gg <- ggplot(palladium_FUT15_16, aes(x=Report_Date_as_MM_DD_YYYY,y=Comm_Positions_Long_All)) +
  geom_line()
plot(gg)

# This works
# Next, add commercial short positions also on same plot

# Add a ratio of long to short

# Add a price of Palladium

# Make interactive


