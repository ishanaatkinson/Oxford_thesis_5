# Description ####

# - model diagram called "modelwithlag.drawio"
# - sensitivity analysis is included on various factors 
# - lag for seasonality - would this be time lag or shift the cosine function 
# back by a t value corresponding to 4 months


# Import packages ####
library(pacman)
p_load(deSolve, tidyverse, doParallel, manipulate,readxl,reshape2,ggfortify, knitr, printr,
       plyr, dplyr, lubridate, gridExtra, reshape2, TTR, unikn, ggpubr, zoo, scales)


# Import data ####
data<-read_excel("C:/Users/iatkinson/Documents/!_Placement Research/Data/Edited 4 SucreBD Final 2002-2018 De-ID.xlsx",
                 col_types = c("numeric", "numeric", "numeric", "text", "text", "text", "text", "text",
                               "text", "text", "text", "text", "text", "text", "text", "text", "text", "text", 
                               "text", "text"))

df_data <- as.data.frame(data)

# DATES ####

dates_98_30_years<-seq(ymd('1998-01-01'),ymd('2030-01-01'),by='year')
dates_08_21_years<-seq(ymd('2008-01-01'),ymd('2022-01-01'),by='year')
dates_98_30<-seq(ymd('1998-01-03'),ymd('2029-11-23'),by='weeks')
dates_02_21<-seq(ymd('2002-01-01'),ymd('2021-12-01'),by='weeks')
dates_08_21<-seq(ymd('2008-01-01'),ymd('2021-12-13'),by='weeks')

# Sum total incidences for all cases both originating in Sucre and imported ####

# only keep PV and MI cases

# subset data into dataframe for each year (PV and IM infections only)
inc_2002_tot <- subset(df_data, (Year == 2002 & (`General parasite species` == "PV" | `General parasite species` == "MI" )))
inc_2003_tot <- subset(df_data, (Year == 2003 & (`General parasite species` == "PV" | `General parasite species` == "MI" )))
inc_2004_tot <- subset(df_data, (Year == 2004 & (`General parasite species` == "PV" | `General parasite species` == "MI" )))
inc_2005_tot <- subset(df_data, (Year == 2005 & (`General parasite species` == "PV" | `General parasite species` == "MI" )))
inc_2006_tot <- subset(df_data, (Year == 2006 & (`General parasite species` == "PV" | `General parasite species` == "MI" )))
inc_2007_tot <- subset(df_data, (Year == 2007 & (`General parasite species` == "PV" | `General parasite species` == "MI" )))
inc_2008_tot <- subset(df_data, (Year == 2008 & (`General parasite species` == "PV" | `General parasite species` == "MI" )))
inc_2009_tot <- subset(df_data, (Year == 2009 & (`General parasite species` == "PV" | `General parasite species` == "MI" )))
inc_2010_tot <- subset(df_data, (Year == 2010 & (`General parasite species` == "PV" | `General parasite species` == "MI" )))
inc_2011_tot <- subset(df_data, (Year == 2011 & (`General parasite species` == "PV" | `General parasite species` == "MI" )))
inc_2012_tot <- subset(df_data, (Year == 2012 & (`General parasite species` == "PV" | `General parasite species` == "MI" )))
inc_2013_tot <- subset(df_data, (Year == 2013 & (`General parasite species` == "PV" | `General parasite species` == "MI" )))
inc_2014_tot <- subset(df_data, (Year == 2014 & (`General parasite species` == "PV" | `General parasite species` == "MI" )))
inc_2015_tot <- subset(df_data, (Year == 2015 & (`General parasite species` == "PV" | `General parasite species` == "MI" )))
inc_2016_tot <- subset(df_data, (Year == 2016 & (`General parasite species` == "PV" | `General parasite species` == "MI" )))
inc_2017_tot <- subset(df_data, (Year == 2017 & (`General parasite species` == "PV" | `General parasite species` == "MI" )))
inc_2018_tot <- subset(df_data, (Year == 2018 & (`General parasite species` == "PV" | `General parasite species` == "MI" )))
inc_2019_tot <- subset(df_data, (Year == 2019 & (`General parasite species` == "PV" | `General parasite species` == "MI" )))

yearlyincidence_tot <- c() # annual incidence for each year (all cases)

# subset data for only PV and IM cases 
df_data_PV_IM_only <- subset(df_data, (`General parasite species` == "PV" | `General parasite species` == "MI" ))

for (i in seq(2002,2019)) {
  yearlyincidence_tot <- c(yearlyincidence_tot,sum(df_data_PV_IM_only[which(df_data_PV_IM_only$Year == i), 1])/i)
}

# create empty vectors to fill 
weeklyincidence_2002_tot <- c()
weeklyincidence_2003_tot <- c()
weeklyincidence_2004_tot <- c()
weeklyincidence_2005_tot <- c()
weeklyincidence_2006_tot <- c()
weeklyincidence_2007_tot <- c()
weeklyincidence_2008_tot <- c()
weeklyincidence_2009_tot <- c()
weeklyincidence_2010_tot <- c()
weeklyincidence_2011_tot <- c()
weeklyincidence_2012_tot <- c()
weeklyincidence_2013_tot <- c()
weeklyincidence_2014_tot <- c()
weeklyincidence_2015_tot <- c()
weeklyincidence_2016_tot <- c()
weeklyincidence_2017_tot <- c()
weeklyincidence_2018_tot <- c()
weeklyincidence_2019_tot <- c()


# weekly incidence for each year (all cases)
for (i in seq(1,52)) {
  weeklyincidence_2002_tot <- c(weeklyincidence_2002_tot,length(which(inc_2002_tot$"Epidemiological week reported" == i)))
  weeklyincidence_2003_tot <- c(weeklyincidence_2003_tot,length(which(inc_2003_tot$"Epidemiological week reported" == i)))
  weeklyincidence_2004_tot <- c(weeklyincidence_2004_tot,length(which(inc_2004_tot$"Epidemiological week reported" == i)))
  weeklyincidence_2005_tot <- c(weeklyincidence_2005_tot,length(which(inc_2005_tot$"Epidemiological week reported" == i)))
  weeklyincidence_2006_tot <- c(weeklyincidence_2006_tot,length(which(inc_2006_tot$"Epidemiological week reported" == i)))
  weeklyincidence_2007_tot <- c(weeklyincidence_2007_tot,length(which(inc_2007_tot$"Epidemiological week reported" == i)))
  weeklyincidence_2008_tot <- c(weeklyincidence_2008_tot,length(which(inc_2008_tot$"Epidemiological week reported" == i)))
  weeklyincidence_2009_tot <- c(weeklyincidence_2009_tot,length(which(inc_2009_tot$"Epidemiological week reported" == i)))
  weeklyincidence_2010_tot <- c(weeklyincidence_2010_tot,length(which(inc_2010_tot$"Epidemiological week reported" == i)))
  weeklyincidence_2011_tot <- c(weeklyincidence_2011_tot,length(which(inc_2011_tot$"Epidemiological week reported" == i)))
  weeklyincidence_2012_tot <- c(weeklyincidence_2012_tot,length(which(inc_2012_tot$"Epidemiological week reported" == i)))
  weeklyincidence_2013_tot <- c(weeklyincidence_2013_tot,length(which(inc_2013_tot$"Epidemiological week reported" == i)))
  weeklyincidence_2014_tot <- c(weeklyincidence_2014_tot,length(which(inc_2014_tot$"Epidemiological week reported" == i)))
  weeklyincidence_2015_tot <- c(weeklyincidence_2015_tot,length(which(inc_2015_tot$"Epidemiological week reported" == i)))
  weeklyincidence_2016_tot <- c(weeklyincidence_2016_tot,length(which(inc_2016_tot$"Epidemiological week reported" == i)))
  weeklyincidence_2017_tot <- c(weeklyincidence_2017_tot,length(which(inc_2017_tot$"Epidemiological week reported" == i)))
  weeklyincidence_2018_tot <- c(weeklyincidence_2018_tot,length(which(inc_2018_tot$"Epidemiological week reported" == i)))
  weeklyincidence_2019_tot <- c(weeklyincidence_2019_tot,length(which(inc_2019_tot$"Epidemiological week reported" == i)))
  
}

# weekly incidence from 2002-2019 (all cases)
overall_weeklyincidence_tot <- c(weeklyincidence_2002_tot,weeklyincidence_2003_tot,weeklyincidence_2004_tot,
                                 weeklyincidence_2005_tot,weeklyincidence_2006_tot,weeklyincidence_2007_tot,
                                 weeklyincidence_2008_tot,weeklyincidence_2009_tot,weeklyincidence_2010_tot,
                                 weeklyincidence_2011_tot,weeklyincidence_2012_tot,weeklyincidence_2013_tot,
                                 weeklyincidence_2014_tot,weeklyincidence_2015_tot,weeklyincidence_2016_tot,
                                 weeklyincidence_2017_tot,weeklyincidence_2018_tot,weeklyincidence_2019_tot)

# weekly incidence from 2008-2019 (all cases)
overall_weeklyincidence_tot_08_19 <- c(weeklyincidence_2008_tot,weeklyincidence_2009_tot,weeklyincidence_2010_tot,
                                       weeklyincidence_2011_tot,weeklyincidence_2012_tot,weeklyincidence_2013_tot,
                                       weeklyincidence_2014_tot,weeklyincidence_2015_tot,weeklyincidence_2016_tot,
                                       weeklyincidence_2017_tot,weeklyincidence_2018_tot, weeklyincidence_2019_tot)


# Sum incidences for indigenous cases originating in Sucre ####

# subset data into dataframes for each year (indigenous cases)
inc_2002 <- subset(df_data, (Year == 2002 & `Origin of infection (State)` == "Sucre" & (`General parasite species` == "PV" | `General parasite species` == "MI" )))
inc_2003 <- subset(df_data, (Year == 2003 & `Origin of infection (State)` == "Sucre" & (`General parasite species` == "PV" | `General parasite species` == "MI" )))
inc_2004 <- subset(df_data, (Year == 2004 & `Origin of infection (State)` == "Sucre" & (`General parasite species` == "PV" | `General parasite species` == "MI" )))
inc_2005 <- subset(df_data, (Year == 2005 & `Origin of infection (State)` == "Sucre" & (`General parasite species` == "PV" | `General parasite species` == "MI" )))
inc_2006 <- subset(df_data, (Year == 2006 & `Origin of infection (State)` == "Sucre" & (`General parasite species` == "PV" | `General parasite species` == "MI" )))
inc_2007 <- subset(df_data, (Year == 2007 & `Origin of infection (State)` == "Sucre" & (`General parasite species` == "PV" | `General parasite species` == "MI" )))
inc_2008 <- subset(df_data, (Year == 2008 & `Origin of infection (State)` == "Sucre" & (`General parasite species` == "PV" | `General parasite species` == "MI" )))
inc_2009 <- subset(df_data, (Year == 2009 & `Origin of infection (State)` == "Sucre" & (`General parasite species` == "PV" | `General parasite species` == "MI" )))
inc_2010 <- subset(df_data, (Year == 2010 & `Origin of infection (State)` == "Sucre" & (`General parasite species` == "PV" | `General parasite species` == "MI" )))
inc_2011 <- subset(df_data, (Year == 2011 & `Origin of infection (State)` == "Sucre" & (`General parasite species` == "PV" | `General parasite species` == "MI" )))
inc_2012 <- subset(df_data, (Year == 2012 & `Origin of infection (State)` == "Sucre" & (`General parasite species` == "PV" | `General parasite species` == "MI" )))
inc_2013 <- subset(df_data, (Year == 2013 & `Origin of infection (State)` == "Sucre" & (`General parasite species` == "PV" | `General parasite species` == "MI" )))
inc_2014 <- subset(df_data, (Year == 2014 & `Origin of infection (State)` == "Sucre" & (`General parasite species` == "PV" | `General parasite species` == "MI" )))
inc_2015 <- subset(df_data, (Year == 2015 & `Origin of infection (State)` == "Sucre" & (`General parasite species` == "PV" | `General parasite species` == "MI" )))
inc_2016 <- subset(df_data, (Year == 2016 & `Origin of infection (State)` == "Sucre" & (`General parasite species` == "PV" | `General parasite species` == "MI" )))
inc_2017 <- subset(df_data, (Year == 2017 & `Origin of infection (State)` == "Sucre" & (`General parasite species` == "PV" | `General parasite species` == "MI" )))
inc_2018 <- subset(df_data, (Year == 2018 & `Origin of infection (State)` == "Sucre" & (`General parasite species` == "PV" | `General parasite species` == "MI" )))
inc_2019 <- subset(df_data, (Year == 2019 & `Origin of infection (State)` == "Sucre" & (`General parasite species` == "PV" | `General parasite species` == "MI" )))


yearlyincidence <- c() # annual incidence vector (indigenous cases)

for (i in seq(2002,2019)) {
  yearlyincidence <- c(yearlyincidence,sum(df_data_PV_IM_only[which(df_data_PV_IM_only$Year == i & df_data_PV_IM_only$`Origin of infection (State)` == "Sucre"), 1])/i)
}


# create empty vectors to fill 
weeklyincidence_2002 <- c()
weeklyincidence_2003 <- c()
weeklyincidence_2004 <- c()
weeklyincidence_2005 <- c()
weeklyincidence_2006 <- c()
weeklyincidence_2007 <- c()
weeklyincidence_2008 <- c()
weeklyincidence_2009 <- c()
weeklyincidence_2010 <- c()
weeklyincidence_2011 <- c()
weeklyincidence_2012 <- c()
weeklyincidence_2013 <- c()
weeklyincidence_2014 <- c()
weeklyincidence_2015 <- c()
weeklyincidence_2016 <- c()
weeklyincidence_2017 <- c()
weeklyincidence_2018 <- c()
weeklyincidence_2019 <- c()


# weekly incidence for each year (indigenous cases)
for (i in seq(1,52)) {
  weeklyincidence_2002 <- c(weeklyincidence_2002,length(which(inc_2002$"Epidemiological week reported" == i)))
  weeklyincidence_2003 <- c(weeklyincidence_2003,length(which(inc_2003$"Epidemiological week reported" == i)))
  weeklyincidence_2004 <- c(weeklyincidence_2004,length(which(inc_2004$"Epidemiological week reported" == i)))
  weeklyincidence_2005 <- c(weeklyincidence_2005,length(which(inc_2005$"Epidemiological week reported" == i)))
  weeklyincidence_2006 <- c(weeklyincidence_2006,length(which(inc_2006$"Epidemiological week reported" == i)))
  weeklyincidence_2007 <- c(weeklyincidence_2007,length(which(inc_2007$"Epidemiological week reported" == i)))
  weeklyincidence_2008 <- c(weeklyincidence_2008,length(which(inc_2008$"Epidemiological week reported" == i)))
  weeklyincidence_2009 <- c(weeklyincidence_2009,length(which(inc_2009$"Epidemiological week reported" == i)))
  weeklyincidence_2010 <- c(weeklyincidence_2010,length(which(inc_2010$"Epidemiological week reported" == i)))
  weeklyincidence_2011 <- c(weeklyincidence_2011,length(which(inc_2011$"Epidemiological week reported" == i)))
  weeklyincidence_2012 <- c(weeklyincidence_2012,length(which(inc_2012$"Epidemiological week reported" == i)))
  weeklyincidence_2013 <- c(weeklyincidence_2013,length(which(inc_2013$"Epidemiological week reported" == i)))
  weeklyincidence_2014 <- c(weeklyincidence_2014,length(which(inc_2014$"Epidemiological week reported" == i)))
  weeklyincidence_2015 <- c(weeklyincidence_2015,length(which(inc_2015$"Epidemiological week reported" == i)))
  weeklyincidence_2016 <- c(weeklyincidence_2016,length(which(inc_2016$"Epidemiological week reported" == i)))
  weeklyincidence_2017 <- c(weeklyincidence_2017,length(which(inc_2017$"Epidemiological week reported" == i)))
  weeklyincidence_2018 <- c(weeklyincidence_2018,length(which(inc_2018$"Epidemiological week reported" == i)))
  weeklyincidence_2019 <- c(weeklyincidence_2019,length(which(inc_2019$"Epidemiological week reported" == i)))
  
}

# weekly incidence 2002-2018 (indigenous cases)
overall_weeklyincidence <- c(weeklyincidence_2002,weeklyincidence_2003,weeklyincidence_2004,
                             weeklyincidence_2005,weeklyincidence_2006,weeklyincidence_2007,
                             weeklyincidence_2008,weeklyincidence_2009,weeklyincidence_2010,
                             weeklyincidence_2011,weeklyincidence_2012,weeklyincidence_2013,
                             weeklyincidence_2014,weeklyincidence_2015,weeklyincidence_2016,
                             weeklyincidence_2017,weeklyincidence_2018,weeklyincidence_2019)

# weekly incidence 2008-2018 (indigenous cases)
overall_weeklyincidence_08_19 <- c(weeklyincidence_2008,weeklyincidence_2009,weeklyincidence_2010,
                                   weeklyincidence_2011,weeklyincidence_2012,weeklyincidence_2013,
                                   weeklyincidence_2014,weeklyincidence_2015,weeklyincidence_2016,
                                   weeklyincidence_2017,weeklyincidence_2018,weeklyincidence_2019)


# dataframe for weekly incidence 2002-2018
df2 <- data.frame(seq(1,52),cbind(weeklyincidence_2002,weeklyincidence_2003,weeklyincidence_2004,
                                  weeklyincidence_2005,weeklyincidence_2006,weeklyincidence_2007,
                                  weeklyincidence_2008,weeklyincidence_2009,weeklyincidence_2010,
                                  weeklyincidence_2011,weeklyincidence_2012,weeklyincidence_2013,
                                  weeklyincidence_2014,weeklyincidence_2015,weeklyincidence_2016,
                                  weeklyincidence_2017,weeklyincidence_2018,weeklyincidence_2019 ))

# dataframe for weekly incidence 2008-2018
df3 <- data.frame(seq(1,52*12),cbind(weeklyincidence_2008,weeklyincidence_2009,weeklyincidence_2010,weeklyincidence_2011,
                                     weeklyincidence_2012,weeklyincidence_2013,weeklyincidence_2014,weeklyincidence_2015,
                                     weeklyincidence_2016,weeklyincidence_2017,weeklyincidence_2018,weeklyincidence_2019))



# Plotting incidence ####

# incidence from 2002-2018 (indigenous)
dates_02_19 <- seq(as.Date("2002-01-15"), as.Date("2019-12-19"), by="weeks")

ggplot()+
  geom_line(aes(x = dates_02_19, y=overall_weeklyincidence, colour="Incidence")) + 
  theme_gray()+
  scale_x_date(breaks = scales::breaks_pretty(10))+ 
  labs(x="Date", y="Weekly incidence", colour="Legend", title="Weekly indigenous malaria incidence in Sucre State,\nVenezuela 2002-2018")


# quick investigation into weekly incidence (indigenous) for each year
Epidemiological_week = rep(seq(1,52,by=1),times=18)

df2 %>% tidyr::gather("weekly_inc", "Incidence", 2:19) %>% 
  ggplot(., aes(Epidemiological_week, Incidence))+
  geom_line()+
  theme_gray()+
  facet_wrap(~weekly_inc)

# weekly incidence from 2013-2019
df2 %>%
  ggplot()+  
  geom_line(aes(x = seq.1..52., y=weeklyincidence_2013, colour="2013"))+ 
  geom_line(aes(x = seq.1..52., y=weeklyincidence_2014, colour="2014"))+ 
  geom_line(aes(x = seq.1..52., y=weeklyincidence_2015, colour="2015"))+
  geom_line(aes(x = seq.1..52., y=weeklyincidence_2016, colour="2016"))+
  geom_line(aes(x = seq.1..52., y=weeklyincidence_2017, colour="2017"))+
  geom_line(aes(x = seq.1..52., y=weeklyincidence_2018, colour="2018"))+
  geom_line(aes(x = seq.1..52., y=weeklyincidence_2019, colour="2019"))+
  theme_gray()+
  scale_color_manual(values=c("#556B2F", "#ffc125", "#cd5c5c", "#009acd", "#8968cd", "#8b8989", "black"))+
  labs(title="Weekly indigenous malaria incidence for years 2013-2019", x="Epidemiological week", y="Incidence", colour="Year")

# incidence data since 2008 which we will run the model with 
dates_08_19<-seq(ymd('2008-01-01'),ymd('2019-12-15'),by='weeks')


since_2009 <- c(weeklyincidence_2008,weeklyincidence_2009,weeklyincidence_2010,weeklyincidence_2011,weeklyincidence_2012,
                weeklyincidence_2013,weeklyincidence_2014, weeklyincidence_2015, weeklyincidence_2016,
                weeklyincidence_2017, weeklyincidence_2018, weeklyincidence_2019 )

# weekly indigenous incidence 2008-2018
df3 %>% ggplot()+  
  geom_line(aes(x=dates_08_19, y=since_2009, colour="Incidence"))+ 
  geom_vline(xintercept=c(as.Date("2008-01-01"),as.Date("2009-01-01"),as.Date("2010-01-01"),as.Date("2011-01-01"),as.Date("2012-01-01"),as.Date("2013-01-01"),as.Date("2014-01-01"),as.Date("2015-01-01"),as.Date("2016-01-01"),as.Date("2017-01-01"),as.Date("2018-01-01"),as.Date("2019-01-01"),as.Date("2020-01-01")), lty="dashed", color="gray") + 
  geom_smooth(formula=y~x, aes(x=dates_08_19, y=since_2009, colour="LOESS Line of best fit"), method="loess", show.legend = TRUE, size=0.5)+
  labs(title="Weekly indigenous malaria incidence from 2008-2019", x="Year", y="Incidence", colour="Legend:") + 
  theme_gray()+
  scale_color_manual(values=c("#556B2F", "#ffc125"))  


# weekly indigenous incidence 2013-2019
# assume reporting occurs on a monday so dates are first and last mondays of the starting/ending year respectively
dates_prev_7_years <- seq(as.Date("2013-01-07"), as.Date("2019-12-25"), by="weeks")
prev_7_years <- c(weeklyincidence_2013,weeklyincidence_2014, weeklyincidence_2015, weeklyincidence_2016,
                  weeklyincidence_2017, weeklyincidence_2018,weeklyincidence_2019 )

df3[261:624,c(1,7:12)] %>% # need to cut off some times to get last 6 years
  ggplot()+  
  geom_line(aes(x=dates_prev_7_years, y=prev_7_years, colour="Incidence"))+ 
  geom_vline(xintercept=c(as.Date("2013-01-01"),as.Date("2014-01-01"),as.Date("2015-01-01"),as.Date("2016-01-01"),as.Date("2017-01-01"),as.Date("2018-01-01"),as.Date("2019-01-01")), lty="dashed") + 
  geom_smooth(formula=y~x, aes(x=dates_prev_7_years, y=prev_7_years, colour="LOESS Line of best fit"), method="loess", show.legend = TRUE, size=0.5)+
  labs(title="Weekly malaria incidence originating in Sucre \nState, Venezuela from 2013-2019", x="Year", y="Incidence", colour="Legend:") + 
  scale_color_manual(values=c("#556B2F", "#ffc125"))  


# Time series decomposition to investigate seasonality ####

# dataframe for last 6 years 
df_prev_7_years <- data.frame(dates_prev_7_years,prev_7_years)
p <- ts(prev_7_years, frequency=52, start=c(2013,1), end=c(2019,52))

# Time series decomposition for the last 6 years of data to use in model
p %>%
  stl(s.window = "periodic") %>%
  autoplot(xlab="Date", ylab="Weekly incidence", main="Time series decomposition of weekly malaria incidence \noriginating in Sucre State, Venezuela from 2013-2019", colour="#556B2F")+ 
  geom_vline(xintercept=c(2013,2014,2015,2016,2017,2018,2019), lty="dashed", color="gray")  

# Find an equation for the seasonality component 

# fitting a trig function to the dataset
x<-seq(from=2013, to=2019.99, by=1/52)

ggplot() + 
  geom_line(aes(x=x, y=decompose(p, "additive")$seasonal, colour="Seasonal decomposition")) + 
  geom_line(aes(x=x, y=(220*cos(pi*(2*x) + 6.5 )), colour="Fitted cosine function"))+
  labs(x="Date", y="Seasonal component", colour="Legend", title="Fitted cosine function to seasonal \ndecompositon of indigenous incidence")+
  scale_color_manual(values=c("#ffc125", "#556B2F"))  


# compress cosine function for the range -1 < y < 1 

ggplot() + 
  geom_line(aes(x, (0.5+(0.2*cos(pi*(2*x)+6.5))), colour="Compressed fitted cosine function")) + 
  labs(x="Date", y="Compressed seasonal component", title="Compressed fitted cosine function to seasonal \ndecomposition of indigenous incidence data", colour="Legend")+
  scale_color_manual(values=c("#556B2F")) 


# the function above can be used in the model to impact the FOI 
# but change x to t/52 since the model runs in weeks

# Descriptive analysis ####

# Histogram of indigenous cases by age of patient from 2013-2019
# 336 entries are not included due to missing age data
df_data %>% 
  filter(df_data$`Origin of infection (State)` == "Sucre" & df_data$Year > 2012) %>%
  ggplot(aes(x=Age, fill=Sex)) +
  geom_histogram(binwidth=1, position = "identity", alpha=0.3)+
  theme_gray()+
  scale_color_manual(values=c("#556B2F","#ffc125")) + 
  labs(title="Distribution of patient ages for infections originating in \nSucre State, Venezuela between 2013-2019", y="Sum")  

# histogram of ages by sex for all cases (local and imported)
df_data %>%
  ggplot(aes(x=Age, fill=Sex)) +
  geom_histogram(binwidth=1, position = "identity", alpha=0.4)+
  theme_gray()+
  scale_color_manual(values=c("#556B2F","#ffc125")) + 
  labs(title="Distribution of patient ages for infections originating in \nSucre State, Venezuela between 2002-2018", y="Sum")  


# total indigenous cases by sex between 2013-2019
df_data %>%
  filter(df_data$`Origin of infection (State)` == "Sucre" & df_data$Year > 2012) %>%
  ggplot(aes(x = Sex, fill=Sex)) +
  geom_bar(stat="count", alpha=0.7, position = "identity") + 
  scale_fill_manual("Sex", values = c("Female" = "#556B2F", "Male" = "#ffc125")) + 
  stat_count(geom = "text", colour = "black", size = 2.5,
             aes(label = ..count..),position=position_stack(vjust=1.02)) + 
  theme_gray()+
  labs(title = "Total malaria incidence originating in Sucre State, \nVenezeuala by sex between 2013-2019", x="Sex", y="Incidence")

# bar chart for total incidence by types of species 2002-2018
PV_count <- rep(c("PV"), times=(count(df_data$`General parasite species` == "PV" & df_data$`Origin of infection (State)` == "Sucre")$freq[2]))
MI_count <- rep(c("MI"), times=(count(df_data$`General parasite species` == "MI" & df_data$`Origin of infection (State)` == "Sucre")$freq[2]))
species <- data.frame((c(PV_count, MI_count)))

species %>%
  ggplot(aes(x=X.c.PV_count..MI_count.., fill=X.c.PV_count..MI_count..)) + 
  geom_bar(alpha=0.7) + 
  stat_count(geom = "text", colour = "black", size = 2.5,
             aes(label = ..count..), vjust=-0.5) +   
  scale_fill_manual("Species", values = c("MI" = "#556B2F", "PV" = "#ffc125")) + 
  labs(title = "Malaria incidence originating in Sucre State, \nVenezuela by species from 2002-2018", x="Species", y="Cases")


# bar chart for total incidence by types of species 2013-2019

PV_count_13_19 <- rep(c("PV"), times=(count(df_data$`General parasite species` == "PV" & df_data$`Origin of infection (State)` == "Sucre" & df_data$Year > 2012)$freq[2]))
MI_count_13_19 <- rep(c("MI"), times=(count(df_data$`General parasite species` == "MI" & df_data$`Origin of infection (State)` == "Sucre" & df_data$Year > 2012)$freq[2]))
species_13_19 <- data.frame((c(PV_count_13_19, MI_count_13_19)))

species_13_19 %>%
  ggplot(aes(x=X.c.PV_count_13_19..MI_count_13_19.., fill=X.c.PV_count_13_19..MI_count_13_19..)) + 
  geom_bar(alpha=0.7) + 
  stat_count(geom = "text", colour = "black", size = 2.5,
             aes(label = ..count..), vjust=-0.5) +   
  scale_fill_manual("Species", values = c("MI" = "#556B2F", "PV" = "#ffc125")) + 
  labs(title = "Malaria incidence originating in Sucre State,  \nVenezuela by species from 2013-2019", x="Species", y="Cases")


# bar chart for types of case 2002-2018
indig_count <- rep(c("Indigenous"), times=(count(df_data$`Type of case` == "Indigenous")$freq[2]))
imp_count <- rep(c("Imported"), times=(count(df_data$`Type of case` == "Imported")$freq[2]))
type_case <- data.frame(c(indig_count, imp_count))

type_case %>%
  ggplot(aes(x=c.indig_count..imp_count., fill=c.indig_count..imp_count.)) + 
  geom_bar(alpha=0.7) + 
  stat_count(geom = "text", colour = "black", size = 2.5,
             aes(label = ..count..), vjust=-0.5) +   
  scale_fill_manual("Species", values = c("Indigenous" = "#556B2F", "Imported" = "#ffc125")) + 
  labs(title = "Malaria incidence originating in Sucre State, \nVenezuela by type of case from 2002-2018", x="Type of case", y="Incidence")


# bar chart for types of case 2013-2019
indig_count_13_19 <- rep(c("Indigenous"), times=(count(df_data$`Type of case` == "Indigenous" & df_data$Year > 2012)$freq[2]))
imp_count_13_19 <- rep(c("Imported"), times=(count(df_data$`Type of case` == "Imported"  & df_data$Year > 2012)$freq[2]))
type_case_13_19 <- data.frame(c(indig_count_13_19,  imp_count_13_19))

type_case_13_19 %>%
  ggplot(aes(x=c.indig_count_13_19..imp_count_13_19., fill=c.indig_count_13_19..imp_count_13_19.)) + 
  geom_bar(alpha=0.7) + 
  stat_count(geom = "text", colour = "black", size = 2.5,
             aes(label = ..count..), vjust=-0.5) +  
  scale_fill_manual("Species", values = c("Indigenous" = "#556B2F", "Imported" = "#ffc125")) + 
  labs(title = "Malaria incidence originating in Sucre State, \nVenezuela by type of case from 2013-2019", x="Type of case", y="Incidence") 




# Investigating migration patterns and time series decomposition of imported case data ####

# subset data into imported cases
mig1 <- subset(df_data, (`Type of case` == "Imported") & (`General parasite species` == "PV" | `General parasite species` == "MI")) 

# subset imported cases by year 
inc_2002_mig1 <- subset(mig1, Year == 2002)
inc_2003_mig1 <- subset(mig1, Year == 2003)
inc_2004_mig1 <- subset(mig1, Year == 2004)
inc_2005_mig1 <- subset(mig1, Year == 2005)
inc_2006_mig1 <- subset(mig1, Year == 2006)
inc_2007_mig1 <- subset(mig1, Year == 2007)
inc_2008_mig1 <- subset(mig1, Year == 2008)
inc_2009_mig1 <- subset(mig1, Year == 2009)
inc_2010_mig1 <- subset(mig1, Year == 2010)
inc_2011_mig1 <- subset(mig1, Year == 2011)
inc_2012_mig1 <- subset(mig1, Year == 2012)
inc_2013_mig1 <- subset(mig1, Year == 2013)
inc_2014_mig1 <- subset(mig1, Year == 2014)
inc_2015_mig1 <- subset(mig1, Year == 2015)
inc_2016_mig1 <- subset(mig1, Year == 2016)
inc_2017_mig1 <- subset(mig1, Year == 2017)
inc_2018_mig1 <- subset(mig1, Year == 2018)
inc_2019_mig1 <- subset(mig1, Year == 2019)


yearlyincidence_mig1 <- c() # yearly imported incidence

for (i in seq(2002,2019)) {
  yearlyincidence_mig1 <- c(yearlyincidence_mig1,sum(mig1[which(mig1$Year == i), 1])/i)
}

# create empty vectors to fill
weeklyincidence_2002_mig1 <- c()
weeklyincidence_2003_mig1 <- c()
weeklyincidence_2004_mig1 <- c()
weeklyincidence_2005_mig1 <- c()
weeklyincidence_2006_mig1 <- c()
weeklyincidence_2007_mig1 <- c()
weeklyincidence_2008_mig1 <- c()
weeklyincidence_2009_mig1 <- c()
weeklyincidence_2010_mig1 <- c()
weeklyincidence_2011_mig1 <- c()
weeklyincidence_2012_mig1 <- c()
weeklyincidence_2013_mig1 <- c()
weeklyincidence_2014_mig1 <- c()
weeklyincidence_2015_mig1 <- c()
weeklyincidence_2016_mig1 <- c()
weeklyincidence_2017_mig1 <- c()
weeklyincidence_2018_mig1 <- c()
weeklyincidence_2019_mig1 <- c()


# weekly imported incidence by year 
for (i in seq(1,52)) {
  weeklyincidence_2002_mig1 <- c(weeklyincidence_2002_mig1,length(which(inc_2002_mig1$"Epidemiological week reported" == i)))
  weeklyincidence_2003_mig1 <- c(weeklyincidence_2003_mig1,length(which(inc_2003_mig1$"Epidemiological week reported" == i)))
  weeklyincidence_2004_mig1 <- c(weeklyincidence_2004_mig1,length(which(inc_2004_mig1$"Epidemiological week reported" == i)))
  weeklyincidence_2005_mig1 <- c(weeklyincidence_2005_mig1,length(which(inc_2005_mig1$"Epidemiological week reported" == i)))
  weeklyincidence_2006_mig1 <- c(weeklyincidence_2006_mig1,length(which(inc_2006_mig1$"Epidemiological week reported" == i)))
  weeklyincidence_2007_mig1 <- c(weeklyincidence_2007_mig1,length(which(inc_2007_mig1$"Epidemiological week reported" == i)))
  weeklyincidence_2008_mig1 <- c(weeklyincidence_2008_mig1,length(which(inc_2008_mig1$"Epidemiological week reported" == i)))
  weeklyincidence_2009_mig1 <- c(weeklyincidence_2009_mig1,length(which(inc_2009_mig1$"Epidemiological week reported" == i)))
  weeklyincidence_2010_mig1 <- c(weeklyincidence_2010_mig1,length(which(inc_2010_mig1$"Epidemiological week reported" == i)))
  weeklyincidence_2011_mig1 <- c(weeklyincidence_2011_mig1,length(which(inc_2011_mig1$"Epidemiological week reported" == i)))
  weeklyincidence_2012_mig1 <- c(weeklyincidence_2012_mig1,length(which(inc_2012_mig1$"Epidemiological week reported" == i)))
  weeklyincidence_2013_mig1 <- c(weeklyincidence_2013_mig1,length(which(inc_2013_mig1$"Epidemiological week reported" == i)))
  weeklyincidence_2014_mig1 <- c(weeklyincidence_2014_mig1,length(which(inc_2014_mig1$"Epidemiological week reported" == i)))
  weeklyincidence_2015_mig1 <- c(weeklyincidence_2015_mig1,length(which(inc_2015_mig1$"Epidemiological week reported" == i)))
  weeklyincidence_2016_mig1 <- c(weeklyincidence_2016_mig1,length(which(inc_2016_mig1$"Epidemiological week reported" == i)))
  weeklyincidence_2017_mig1 <- c(weeklyincidence_2017_mig1,length(which(inc_2017_mig1$"Epidemiological week reported" == i)))
  weeklyincidence_2018_mig1 <- c(weeklyincidence_2018_mig1,length(which(inc_2018_mig1$"Epidemiological week reported" == i)))
  weeklyincidence_2019_mig1 <- c(weeklyincidence_2019_mig1,length(which(inc_2019_mig1$"Epidemiological week reported" == i)))
}

# weekly imported incidence 2002-2018
overall_weeklyincidence_mig1 <- c(weeklyincidence_2002_mig1,weeklyincidence_2003_mig1,weeklyincidence_2004_mig1,
                                  weeklyincidence_2005_mig1,weeklyincidence_2006_mig1,weeklyincidence_2007_mig1,
                                  weeklyincidence_2008_mig1,weeklyincidence_2009_mig1,weeklyincidence_2010_mig1,
                                  weeklyincidence_2011_mig1,weeklyincidence_2012_mig1,weeklyincidence_2013_mig1,
                                  weeklyincidence_2014_mig1,weeklyincidence_2015_mig1,weeklyincidence_2016_mig1,
                                  weeklyincidence_2017_mig1,weeklyincidence_2018_mig1,weeklyincidence_2019_mig1)

# weekly imported incidence 2008-2018
overall_weeklyincidence_mig1_08_19 <- c(weeklyincidence_2008_mig1,weeklyincidence_2009_mig1,weeklyincidence_2010_mig1,
                                        weeklyincidence_2011_mig1,weeklyincidence_2012_mig1,weeklyincidence_2013_mig1,
                                        weeklyincidence_2014_mig1,weeklyincidence_2015_mig1,weeklyincidence_2016_mig1,
                                        weeklyincidence_2017_mig1,weeklyincidence_2018_mig1,weeklyincidence_2019_mig1)


# dataframe for weekly imported incidence 2002-2018
df2_mig1 <- data.frame(seq(1,52),cbind(weeklyincidence_2002_mig1,weeklyincidence_2003_mig1,weeklyincidence_2004_mig1,
                                       weeklyincidence_2005_mig1,weeklyincidence_2006_mig1,weeklyincidence_2007_mig1,
                                       weeklyincidence_2008_mig1,weeklyincidence_2009_mig1,weeklyincidence_2010_mig1,
                                       weeklyincidence_2011_mig1,weeklyincidence_2012_mig1,weeklyincidence_2013_mig1,
                                       weeklyincidence_2014_mig1,weeklyincidence_2015_mig1,weeklyincidence_2016_mig1,
                                       weeklyincidence_2017_mig1,weeklyincidence_2018_mig1,weeklyincidence_2019_mig1))

# dataframe for weekly imported incidence 2008-2018
df3_mig1 <- data.frame(seq(1,52*12),cbind(weeklyincidence_2008_mig1,weeklyincidence_2009_mig1,weeklyincidence_2010_mig1,weeklyincidence_2011_mig1,
                                          weeklyincidence_2012_mig1,weeklyincidence_2013_mig1,weeklyincidence_2014_mig1,weeklyincidence_2015_mig1,
                                          weeklyincidence_2016_mig1,weeklyincidence_2017_mig1,weeklyincidence_2018_mig1,weeklyincidence_2019_mig1))

# weekly imported incidence 2013-2019
dates_mig1 <- seq(as.Date("2013-01-07"), as.Date("2019-12-25"), by="weeks")
prev_7_years_mig1 <- c(weeklyincidence_2013_mig1,weeklyincidence_2014_mig1, weeklyincidence_2015_mig1, weeklyincidence_2016_mig1,
                       weeklyincidence_2017_mig1, weeklyincidence_2018_mig1,weeklyincidence_2019_mig1)

df3_mig1[261:624,c(1,7:12)] %>% # need to cut off some times to get last 312 times 
  ggplot()+  
  geom_line(aes(x=dates_mig1, y=prev_7_years_mig1, colour="Incidence"))+ 
  geom_vline(xintercept=c(as.Date("2013-01-01"),as.Date("2014-01-01"),as.Date("2015-01-01"),as.Date("2016-01-01"),as.Date("2017-01-01"),as.Date("2018-01-01"),as.Date("2019-01-01"),as.Date("2020-01-01")), lty="dashed") + 
  geom_smooth(formula=y~x, aes(x=dates_mig1, y=prev_7_years_mig1, colour="LOESS Line of best fit"), method="loess", show.legend = TRUE, size=0.5)+
  labs(title="Weekly imported malaria incidence imported from 2013-2019", x="Year", y="Incidence", colour="Legend:") + 
  scale_color_manual(values=c("#556B2F", "#ffc125"))  


# time series decomposition of 2013-2019 weekly imported incidence data
df_prev_7_years_mig1 <- data.frame(dates_mig1,prev_7_years_mig1)
p_mig1 <- ts(prev_7_years_mig1, frequency=52, start=c(2013,1), end=c(2019,52))

p_mig1 %>%
  stl(s.window = "periodic") %>%
  autoplot(xlab="Date", ylab="Weekly incidence", main="Time series decomposition of weekly malaria incidence \nintroduced into Sucre State, Venezuela from 2013-2019", colour="#556B2F")+ 
  geom_vline(xintercept=c(2013,2014,2015,2016,2017,2018,2019,2020), lty="dashed", color="gray")  

# do the seasonal decomposition for the data before the unexpected peak to investigate seasonality 

dates_mig12 <- seq(as.Date("2013-01-07"), as.Date("2015-12-30"), by="weeks")
prev_7_years_mig12 <- c(weeklyincidence_2013_mig1,weeklyincidence_2014_mig1, weeklyincidence_2015_mig1)
df_prev_7_years_mig12 <- data.frame(dates_mig12,prev_7_years_mig12)
p_mig12 <- ts(prev_7_years_mig12, frequency=52, start=c(2013,1), end=c(2015,52))

p_mig12 %>%
  stl(s.window = "periodic") %>%
  autoplot(xlab="Date", ylab="Weekly incidence", main="Time series decomposition of weekly malaria incidence \nintroduced into Sucre State, Venezuela from 2013-2015 \n(before unexpected peak in 2016)", colour="#556B2F")+ 
  geom_vline(xintercept=c(2013,2014,2015), lty="dashed", color="gray")  

# Find an equation for the seasonality component of weekly importated incidence

# fitting a trig function to the dataset
x<-seq(from=2013, to=2019.99, by=1/52)

ggplot() + 
  geom_line(aes(x=x, y=decompose(p_mig1, "additive")$seasonal, colour="Seasonal decomposition")) + 
  geom_line(aes(x=x, y=17 + (30*cos(pi*(2*x) +1)), colour="Fitted cosine function"))+
  labs(x="Date", y="Seasonal component", colour="Legend", title="Fitted cosine function to seasonal \ndecompositon of imported incidence") + 
  scale_color_manual(values=c("#ffc125", "#556B2F"))  

# compress cosine function for the range -1 < y < 1 

ggplot() + 
  geom_line(aes(x, (0.5+(0.2*cos(pi*(2*x)+7.5))), colour="Compressed fitted cosine function")) + 
  labs(x="Date", y="Compressed seasonal component", title="Compressed fitted cosine function to seasonal \ndecomposition of imported incidence data", colour="Legend") +
  scale_color_manual(values=c("#556B2F")) 

# the function above can be used in the model to impact the imported seasonality 
# variable but change x to t/52 since the model runs in weeks



# ORIGINAL MODEL
