# R code using animint2 for Covid-19 cases confirmed, cured and deaths due to Covid-19 in India
# Replace filepath appropriately 
# Dataset taken from https://www.kaggle.com/sudalairajkumar/covid19-in-india
# The dataset has columns
#      Sno     Date    Time State.UnionTerritory ConfirmedIndianNational ConfirmedForeignNational Cured
# 1      1 30/01/20 6:00 PM               Kerala                       1                        0     0
# 2      2 31/01/20 6:00 PM               Kerala                       1                        0     0
# 3      3 01/02/20 6:00 PM               Kerala                       2                        0     0
# 4      4 02/02/20 6:00 PM               Kerala                       3                        0     0
# 5      5 03/02/20 6:00 PM               Kerala                       3                        0     0
# 6      6 04/02/20 6:00 PM               Kerala                       3                        0     0
# Deaths Confirmed
# 1      0         1
# 2      0         1
# 3      0         2
# 4      0         3
# 5      0         3
# 6      0         3
# Remove chunking var if making gist
filepath <- "/Users/shubhammittal/Desktop/Animated_interactive_ggplots_GSoC_Tasks/covid_19_india.csv"

# Including libraries and preprocessing dataset 
library(tidyverse)
library(animint2)
library(data.table)
library(gistr)
raw.Covid.India.Data <- read.csv(filepath)
covid.India.Data <- raw.Covid.India.Data[c("Date","State.UnionTerritory","Cured","Deaths","Confirmed")]
colnames(covid.India.Data)[2] <- "State" 
covid.India.Data <- data.table(covid.India.Data)
covid.India.Data[, date.POSIXct := suppressWarnings(strptime(Date, "%d/%m/%y"))]
covid.India.Data[, month.str := strftime(date.POSIXct, "%Y-%m")]
covid.India.Data[, month01.str := paste0(month.str, "-01")]
covid.India.Data[, month01.POSIXct := suppressWarnings(strptime(month01.str, "%Y-%m-%d"))]
uniq.month.vec <- unique(c( covid.India.Data$month.str))
covid.India.Data[, month.formatted.str := strftime(month01.POSIXct, "%B %Y")]
covid.month.levs <- covid.India.Data[order(unique(month01.POSIXct)), unique(month.formatted.str)]
covid.India.Data[, month := factor(month.formatted.str, levels = c("January 2020", "February 2020", "March 2020",   "April 2020",   
                                                                   "May 2020" , "June 2020" , "July 2020" , "August 2020" ,
                                                                   "September 2020","October 2020" , "November 2020" ,"December 2020", "January 2021", "February 2021", "March 2021",   "April 2021",   
                                                                   "May 2021" ,"June 2021" , "July 2021" , "August 2021"))]

one.day <- 60 * 60 * 24
covid.India.Data[, next.POSIXct := month01.POSIXct + one.day * 31]
covid.India.Data[, next01.str := paste0(strftime(next.POSIXct, "%Y-%m"), "-01")]
covid.India.Data[, next01.POSIXct := suppressWarnings(strptime(next01.str, "%Y-%m-%d"))]
covid.India.Data$next.POSIXct <- covid.India.Data$next01.str <- covid.India.Data$month.formatted.str <-
  covid.India.Data$month01.str <-covid.India.Data$month.str <- NULL
# Creating tallrect for Month selection and displaying covid-cases of all states
scatter <- ggplot()+
  ggtitle("Confirmed cases for India statewise")+
  guides(color="none", fill="none")+
  theme_bw() +
  theme_animint(width=600, height=500)+
  geom_tallrect(aes(
    xmin=month01.POSIXct, xmax=next01.POSIXct),
    clickSelects="month",    
    data= covid.India.Data,
    alpha=0.1, color = "gray"
  ) +
  geom_line(aes(
    date.POSIXct, Confirmed,
    group=State, color = State ),
    clickSelects="State", 
    data = covid.India.Data,
    size = 8
    )+
  xlab("Time ->")+
  ylab("Confirmed cases state-wise")

# Plot for highlighting the selected month's confirmed, cured and deaths for selected state

plot2 <- ggplot()+
  ggtitle("Confirmed, Cured and Deaths due to Covid-19 for selected State")+
  theme_animint(width=600, height=500)+
  geom_point(aes(x=date.POSIXct, y=Confirmed, tooltip=paste(Date, "Confirmed=", Confirmed)), 
             color="red",
             showSelected = c("State","month"),
             data = covid.India.Data, chunk_vars = c("State","month")
  ) +
  geom_point(aes(x=date.POSIXct, y=Confirmed),color ="red",
             showSelected = c("State"),
             data = covid.India.Data, chunk_vars = c("State"),
             alpha=0.1, size=0.5
  )+
  geom_point(aes(x=date.POSIXct, y=Cured,tooltip=paste(Date, "Cured=", Cured)),
             showSelected = c("State","month"),
             data = covid.India.Data, color="green", chunk_vars = c("State","month")
  ) +
  geom_point(aes(x=date.POSIXct, y=Cured), color="green",
             showSelected = c("State"),
             data = covid.India.Data, alpha=0.1, size=0.5, chunk_vars = c("State")
  )+
  geom_point(aes(x=date.POSIXct, y=Deaths,tooltip=paste(Date, "Deaths=", Deaths)), color="black",
             showSelected = c("State","month"),
             data = covid.India.Data, chunk_vars = c("State","month")
  ) +
  geom_point(aes(x=date.POSIXct, y=Deaths), color="black",
             showSelected = c("State"),
             data = covid.India.Data, alpha=0.1, size=0.5, chunk_vars = c("State")
  )+ 
  xlab("Time ->")+
  ylab("Cases") 

viz.one <- animint(scatter,plot2,duration=list(month=4000,State=4000))
viz.first <- viz.one
viz.first$first <- list(
  month = "September 2020",
  State = "Maharashtra"
)

# Uncomment to save as gist

if (!requireNamespace("servr")) install.packages("servr")
servr::httd("/Users/shubhammittal/Desktop/Animated_interactive_ggplots_GSoC_Tasks/India_covid_visualisation.R")
animint2dir(viz.final2, filepath)
