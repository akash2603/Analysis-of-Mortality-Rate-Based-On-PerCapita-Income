
library(tidyverse)
library(gapminder)
library(ggplot2)
library(ggthemes)
library(xkcd)
library(wesanderson)
library(gridExtra)
library(viridis)
library(RColorBrewer)
library(readxl)
library(hexbin)
library(rvest)
library(plotly)
library(stringi)
library(stringr)

getwd()
setwd("E:/DataScience/DataWrangling/Assignment 7")
maternal_mortality <- as.data.frame(read_csv("xmart.csv"))


names(maternal_mortality)[names(maternal_mortality)=="Maternal mortality ratio (per 100 000 live births)"] <- "Mortality_Ratio"
names(maternal_mortality)[names(maternal_mortality)=="Births attended by skilled health personnel (%)"] <- "Birth_Skilled_Professional"



maternal_mortality$Mortality_Ratio<- gsub(" ", "", maternal_mortality$Mortality_Ratio , fixed = TRUE)
maternal_mortality$Mortality_Ratio1<- str_extract(maternal_mortality[, "Mortality_Ratio"], "[0-9]+")

maternal_mortality$Mortality_Ratio <- str_replace(maternal_mortality$Mortality_Ratio, "[0-9]+","")

maternal_mortality$Lower_Bound <- str_extract(maternal_mortality[, "Mortality_Ratio"], "\\[[0-9]+-") %>%  str_replace("\\[","") %>% 
  str_replace("-","")

maternal_mortality$Mortality_Ratio <- str_replace(maternal_mortality$Mortality_Ratio, "\\[[0-9]+-", "") %>%  str_replace("\\[","") %>% 
  str_replace("-","")

maternal_mortality$Upper_Bound <- str_replace(maternal_mortality[, "Mortality_Ratio"], "\\]","")

maternal_mortality$Year <- str_replace(maternal_mortality[, "Year"], "-[0-9]+","")



maternal_mortality_final <- maternal_mortality[,-3]
View(maternal_mortality_final)


getwd()
setwd("E:/DataScience/DataWrangling/Assignment 7")

income <- as.data.frame(read_csv("data.csv"))
View(income)

income <- setNames(income, rep(" ", length(income)))

View(income)

colnames(income) = income[1, ]
income = income[-1,]

filter <- income %>% gather(key = "year", value = "Gross_Income", -c(Country))

final_table <- merge(maternal_mortality_final, filter, by.x = c("Country","Year"), by.y =c("Country","year") ,all=TRUE)
 


 

completeFun <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}


final_table<- completeFun(final_table, "Gross_Income")

            
final_table<- final_table[!with(final_table,is.na(Birth_Skilled_Professional)& is.na(Mortality_Ratio1)),]


final_table$Mortality_Ratio1<- as.numeric(final_table$Mortality_Ratio1)

final_table$Gross_Income <- str_replace(final_table$Gross_Income," ","")

final_table$Gross_Income<- as.numeric(final_table$Gross_Income)

ggplot(final_table,aes(Mortality_Ratio1,Gross_Income,colour = Year )) + geom_point() +  xlab("Mortality_Ratio") + ylab("Gross_Income")

ggplot(final_table,aes(Birth_Skilled_Professional,Gross_Income,colour = Year )) + geom_point()  + xlab("Birth_Skilled_Professional") + ylab("Gross_Income")



library(dplyr);
# final_table %>% group_by(Country) %>% summarise(mydates=format(max(mydates), '%m/%d/%y')) 
final_table<- final_table %>% group_by(Country) %>% slice(1) %>% select(Country, Mortality_Ratio1) %>% na.omit(final_table$Mortality_Ratio1)

colnames(final_table)[1] <- "region"

colnames(final_table)[2] <- "value"

df_country <- subset(final_table, select = c(region, value))

df_country$region <- tolower(df_country$region)

library(choroplethr)

country_choropleth(df_country , "Maternal Mortality" )


