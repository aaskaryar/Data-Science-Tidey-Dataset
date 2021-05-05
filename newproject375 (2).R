#library needed 
library(tidyverse)

#load the 4 data sets and assign each to a variable name 
confirmed <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
deaths<- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
beds <- read_csv("/Users/kcabrera/Downloads/xmart.csv")
demographics <- read_csv("/Users/kcabrera/Downloads/demographics.csv")


# data wrangling of confirmed 
confirmed2 <- confirmed %>%  select(-Lat, -Long) %>% rename(Country = `Country/Region`) %>%
  pivot_longer(
    -c(Country,`Province/State`),
    names_to = "date", 
    
    values_to = "confirmed"
  ) 
sumofconfirmednum <- confirmed2 %>% group_by(Country,date) %>% summarise(sum(confirmed))
view(confirmed2)

#data wrangling of deaths 
deaths2 <- deaths %>%  select(-Lat, -Long) %>% rename(Country = `Country/Region`) %>%
  pivot_longer(
    -c(Country,`Province/State`),
    names_to = "date", 
    values_to = "deaths"
  ) 
sumofdeathnum <- deaths2 %>% group_by(Country,date) %>% summarise(sum(deaths))
view(deaths2)

#join of deaths and confirmed datasets
covid_table<- inner_join(sumofdeathnum,sumofconfirmednum)
view(covid_table)

#data wrangling of beds 
beds2 <- beds %>% group_by(Country) %>% summarize(max(`Hospital beds (per 10 000 population)`))
view(beds2)

# inner join of previous table and tied beds dataset
covid_table_with_beds <- inner_join(covid_table,beds2)

#changed the columns names to make it look neater 
colnames(covid_table_with_beds)[3] <- "deaths"
colnames(covid_table_with_beds)[4] <- "confirmed"
colnames(covid_table_with_beds)[5] <- "beds"
view(covid_table_with_beds)

#data wrangling of demographics data set 
demographics2 <- demographics %>%select(-`Country Code`, - `Series Name`) %>% pivot_wider( names_from= `Series Code`, values_from = YR2015)%>%mutate(SP.POP.80UP = SP.POP.80UP.FE +SP.POP.80UP.MA) %>% mutate(SP.POP.1564 = SP.POP.1564.MA.IN+SP.POP.1564.FE.IN) %>% mutate(SP.POP.0014= SP.POP.0014.MA.IN+SP.POP.0014.FE.IN) %>% mutate(SP.DYN.AMRT= SP.DYN.AMRT.FE+SP.DYN.AMRT.MA) %>% mutate(SP.POP.TOTL = SP.POP.TOTL.FE.IN+ SP.POP.TOTL.MA.IN) %>% mutate(SP.POP.65UP= SP.POP.65UP.MA.IN + SP.POP.65UP.FE.IN)
demographics_tidied <- demographics2 %>% select(-c(5:16)) %>% rename(Country = `Country Name`) 
view(demographics_tidied)

#complete data set 
covid_table_complete <- inner_join(covid_table_with_beds,demographics_tidied)
view(covid_table_complete)

#linear modeling 
lin_mod1 <- lm(deaths~confirmed, data= covid_table_complete)
lin_mod2 <- lm(deaths~confirmed+beds, data= covid_table_complete)
lin_mod3 <- lm(deaths~beds, data= covid_table_complete)
lin_mod4 <- lm(deaths~beds+SP.POP.1564, data= covid_table_complete)

#variable transformation 
covid_table_complete <- covid_table_complete %>%  mutate(prop80UP= SP.POP.80UP/SP.POP.TOTL)
lin_mod5 <- lm(deaths~prop80UP, data= covid_table_complete)

#summary of linear models
summary(lin_mod1)
summary(lin_mod2)
summary(lin_mod3)
summary(lin_mod4)
summary(lin_mod5)
