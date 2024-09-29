# Libraries
library(tidyverse)
library(readxl)
library(stringr)
library(rvest) #for webscraping; it is also in tidyverse
library(patchwork)  #for arranging plots
library(tidyquant)
library(plotly)

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
comm_long <- ggplot(palladium_FUT15_16, aes(x=Report_Date_as_MM_DD_YYYY,y=Comm_Positions_Long_All)) +
  geom_line()
plot(comm_long)
# This works


# Next, add commercial short positions also on same plot
comm_long_short <- ggplot(palladium_FUT15_16)  +
  geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=Comm_Positions_Long_All)) +
  geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=Comm_Positions_Short_All))
plot(comm_long_short)
# Works


# Add a ratio of long to short
palladium_FUT15_16 <- palladium_FUT15_16 %>% mutate(short_long_ratio = Comm_Positions_Long_All/Comm_Positions_Short_All)


# Add to plot on new axis
####. This actually may be really hard for some stupid reason!!!
comm_long_short_ratio_2 <- ggplot(palladium_FUT15_16)  +
  geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=Comm_Positions_Long_All)) +
  geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=Comm_Positions_Short_All)) +
  geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=short_long_ratio)) +
  scale_y_continuous(sec.axis = sec_axis(~  Comm_Positions_Long_All/Comm_Positions_Short_All * 1.1))
plot(comm_long_short_ratio_2)





# directions for below
# https://stackoverflow.com/questions/3099219/ggplot-with-2-y-axes-on-each-side-and-different-scales

ylim.prim <- c(0,30000) #max of comm positions
ylim.sec <- c(0,1) #ratio

b <- diff(ylim.prim)/diff(ylim.sec)
a <- ylim.prim[1] - b*ylim.sec[1]

comm_long_short_ratio_3 <- ggplot(palladium_FUT15_16, aes(x=Report_Date_as_MM_DD_YYYY,y=Comm_Positions_Long_All))  +
  geom_line() +
  geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=Comm_Positions_Short_All)) +
  geom_line(aes(y = a + short_long_ratio*b), color = "red") +
  scale_y_continuous("Comm Positions", sec.axis = sec_axis(~ (. - a)/b, name = "Ratio")) +
  #scale_x_continuous("Date") +
  ggtitle("Climatogram for Oslo (1961-1990)")  
  #geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=Comm_Positions_Long_All)) +
  #geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=Comm_Positions_Short_All)) +
  #geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=short_long_ratio)) +
  #scale_y_continuous(sec.axis = sec_axis(~  Comm_Positions_Long_All/Comm_Positions_Short_All * 1.1))
plot(comm_long_short_ratio_3)



comm_long_short_ratio <- ggplot(palladium_FUT15_16)  +
  geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=short_long_ratio))
plot(comm_long_short_ratio)

# par(mfrow = c(2,2)) only works in base R
p1 <- plot(comm_long_short)
p2 <- plot(comm_long_short_ratio)
p2 / p1

p2 / p1 + plot_layout(heights = c(1,5))

# par is base R and does not work with ggplot2
# try cowplot
# https://cran.r-project.org/web/packages/cowplot/vignettes/introduction.html
# or patchwork
# https://patchwork.data-imaginist.com




# https://posit.co/wp-content/uploads/2022/10/data-visualization-1.pdf
# https://ggplot2-book.org
# https://stackoverflow.com/questions/3099219/ggplot-with-2-y-axes-on-each-side-and-different-scales
# https://finchstudio.io/blog/ggplot-dual-y-axes/
# https://stackoverflow.com/questions/3099219/ggplot-with-2-y-axes-on-each-side-and-different-scales
# https://r-graph-gallery.com/line-chart-dual-Y-axis-ggplot2.html
# https://cran.r-project.org/web/packages/egg/vignettes/Ecosystem.html
# http://www.sthda.com/english/wiki/ggplot2-multiplot-put-multiple-graphs-on-the-same-page-using-ggplot2
# http://ianmadd.github.io/pages/multiplot.html
# http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/81-ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page/
# http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
  


grid.arrange



# options for getting stock data
# https://zimanaanalytics.medium.com/how-to-import-stock-market-price-data-into-r-programming-4896ce735f28
# https://financetrain.com/downloading-stock-data-in-r-using-quantmod
# https://rpubs.com/FJRubio/YF
# https://www.r-bloggers.com/2021/05/retrieving-stock-price-using-r/
# https://www.codingfinance.com/post/2018-03-27-download-price/


# Add a price of Palladium
# get Pall price
options("getSymbols.warning4.0"=FALSE)
options("getSymbols.yahoo.warning"=FALSE)
# Downloading Apple price using quantmod

getSymbols("PALL", from = '2000-01-01',
           to = "2024-09-23",warnings = FALSE,
           auto.assign = TRUE)
head(PALL)
chart_Series(PALL)

# or this
pall_tq <- tq_get('PALL', from = '2000-01-01', to = "2024-01-01", get = "stock.prices" )
head(pall_tq)
pall_plot <- ggplot(pall_tq, aes(x=date, y = adjusted)) +
  geom_line()
p3 <- pall_plot
p3

p1 / p2 /p3



# GET ALL DATA FROM ALL YEARS FOR PALL
# work with Palladium
# Use .csv files
palladium_FUT86_06 <- read_xls("FUT86_06.xls")
palladium_FUT86_06
palladium_FUT86_06 <- palladium_FUT86_06 %>% filter(str_detect( Market_and_Exchange_Names, "PALL"))
palladium_FUT86_06
# second column "As_of...." is in character form.  Need number to rbind with others
palladium_FUT86_06 <- palladium_FUT86_06 %>% mutate(As_of_Date_In_Form_YYMMDD = parse_number(As_of_Date_In_Form_YYMMDD))
palladium_FUT86_06


palladium_FUT07_14 <- read_xls("FUT07_14.xls")
palladium_FUT07_14
palladium_FUT07_14 <- palladium_FUT07_14 %>% filter(str_detect( Market_and_Exchange_Names, "PALL"))
# second column "As_of...." is in character form.  Need number to rbind with others
palladium_FUT07_14 <- palladium_FUT07_14 %>% mutate(As_of_Date_In_Form_YYMMDD = parse_number(As_of_Date_In_Form_YYMMDD))
palladium_FUT07_14


palladium_FUT15_16 <- read_xls("FUT15_16.xls")
palladium_FUT15_16
palladium_FUT15_16 <- palladium_FUT15_16 %>% filter(str_detect( Market_and_Exchange_Names, "PALL"))
palladium_FUT15_16
# second column "As_of...." is in character form.  Need number to rbind with others
palladium_FUT15_16 <- palladium_FUT15_16 %>% mutate(As_of_Date_In_Form_YYMMDD = parse_number(As_of_Date_In_Form_YYMMDD))
palladium_FUT15_16

palladium_FUT17 <- read_xls("FUT17.xls")
palladium_FUT17
palladium_FUT17 <- palladium_FUT17 %>% filter(str_detect( Market_and_Exchange_Names, "PALL"))

palladium_FUT18 <- read_xls("FUT18.xls")
palladium_FUT18
palladium_FUT18 <- palladium_FUT18 %>% filter(str_detect( Market_and_Exchange_Names, "PALL"))

palladium_FUT19 <- read_xls("FUT19.xls")
palladium_FUT19
palladium_FUT19 <- palladium_FUT19 %>% filter(str_detect( Market_and_Exchange_Names, "PALL"))

palladium_FUT20 <- read_xls("FUT20.xls")
palladium_FUT20
palladium_FUT20 <- palladium_FUT20 %>% filter(str_detect( Market_and_Exchange_Names, "PALL"))

palladium_FUT21 <- read_xls("FUT21.xls")
palladium_FUT21
palladium_FUT21 <- palladium_FUT21 %>% filter(str_detect( Market_and_Exchange_Names, "PALL"))

palladium_FUT22 <- read_xls("FUT22.xls")
palladium_FUT22
palladium_FUT22 <- palladium_FUT22 %>% filter(str_detect( Market_and_Exchange_Names, "PALL"))

palladium_FUT23 <- read_xls("FUT23.xls")
palladium_FUT23
palladium_FUT23 <- palladium_FUT23 %>% filter(str_detect( Market_and_Exchange_Names, "PALL"))

palladium_FUT24 <- read_xls("FUT24.xls")
palladium_FUT24
palladium_FUT24 <- palladium_FUT24 %>% filter(str_detect( Market_and_Exchange_Names, "PALL"))

palladium_all <- bind_rows(palladium_FUT24, palladium_FUT23, palladium_FUT22, palladium_FUT21, palladium_FUT20, palladium_FUT19, palladium_FUT18, palladium_FUT17, palladium_FUT15_16, palladium_FUT07_14, palladium_FUT86_06)
# one file has double and one has character for date
# fixed above

palladium_all <- palladium_all %>% mutate(long_short_ratio = Comm_Positions_Long_All/Comm_Positions_Short_All)
palladium_all <- palladium_all %>% mutate(short_long_ratio = Comm_Positions_Short_All/Comm_Positions_Long_All)

# print long and short number and long and short ratios
comm_long_short_all <- ggplot(palladium_all)  +
  geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=Comm_Positions_Long_All)) +
  geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=Comm_Positions_Short_All))
plot(comm_long_short_all)

comm_long_short_ratio_all <- ggplot(palladium_all)  +
  geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=long_short_ratio))
plot(comm_long_short_ratio_all)


comm_short_long_ratio_all <- ggplot(palladium_all)  +
  geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=short_long_ratio))
plot(comm_short_long_ratio_all)


# par(mfrow = c(2,2)) only works in base R
p1_all <- plot(comm_long_short_all)
p2_all <- plot(comm_long_short_ratio_all)
p3_all <- plot(comm_short_long_ratio_all)

p3_all / p2_all / p1_all

p2 / p1 + plot_layout(heights = c(1,5))

# ggplotly(p1_all) / ggplotly(p2_all)
p1_plotly <- ggplotly(p1_all, x = ~Report)
p1_plotly <-  p1_plotly %>% layout(
  xaxis = list(
    rangeslider = list(type = "Report_Date_as_MM_DD_YYY")))
p1_plotly

p2_plotly <- ggplotly(p2_all)
p2_plotly

p1_plotly / p2_plotly
# Make interactive
# maybe a Shiny dashboard


####. Need to figure out why 15-16 different than all
####. Need to figure out why 15-16 different than all
####. Need to figure out why 15-16 different than all


# test
palladium_all  <- palladium_all %>%  filter(As_of_Date_In_Form_YYMMDD > 149999 & As_of_Date_In_Form_YYMMDD < 170102)

# print long and short number and long and short ratios
comm_long_short_all <- ggplot(palladium_all)  +
  geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=Comm_Positions_Long_All)) +
  geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=Comm_Positions_Short_All))
plot(comm_long_short_all)

comm_long_short_ratio_all <- ggplot(palladium_all)  +
  geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=long_short_ratio))
plot(comm_long_short_ratio_all)


comm_short_long_ratio_all <- ggplot(palladium_all)  +
  geom_line(aes(x=Report_Date_as_MM_DD_YYYY,y=short_long_ratio))
plot(comm_short_long_ratio_all)


# par(mfrow = c(2,2)) only works in base R
p1_all <- plot(comm_long_short_all)
p2_all <- plot(comm_long_short_ratio_all)
p3_all <- plot(comm_short_long_ratio_all)

p3_all / p2_all / p1_all

p2 / p1 + plot_layout(heights = c(1,5))


# find maximum and minimum percentage gains over the next 6 to 12 months for:
# every price
# or maybe at extremes: less that 0.2 and greater than 0.7

