# Import packages ####
library(pacman)
p_load(deSolve, tidyverse, doParallel, manipulate,readxl,reshape2,ggfortify, knitr, printr,
       plyr, dplyr, lubridate, gridExtra, reshape2, TTR, unikn, ggpubr, zoo, scales)


# Define scenario variables ####

coverage <- "70"
param_file <- "rec072"

params <- as_tibble(as.data.frame(read_csv(paste0("https://raw.githubusercontent.com/student1445449/MGH_thesis/main/", param_file, ".csv"), show_col_types = FALSE))) # Read excel file with all parameters

# Run Models ####s

source("C:/Users/iatkinson/Documents/!_Placement Research/R files/INCLUDE IN ZIP/BASESCRIPT_MODEL.R")


# 3 rounds 
ggplot() + 
  geom_line(aes(x=dates_23_30, y=(df1 %>%
                                    filter(variable %in% c("LocDiag"), time>1298))$value, colour="Model output")) + 
  geom_line(aes(x=dates_23_30, y=(df1_pq3 %>%
                                    filter(variable %in% c("LocDiag"), time>1298))$value, colour="Model PQ MDA output")) + 
  geom_line(aes(x=dates_23_30, y=(df1_taf3 %>%
                                    filter(variable %in% c("LocDiag"), time>1298))$value, colour="Model TQ MDA output")) + 
  labs(title = paste0("Impact of 3 year MDA programme (", coverage, "% \ncoverage) on diagnosed weekly incidence"), x=("Date"),y =("Incidence"), colour="Legend") +
  scale_x_date(breaks = scales::breaks_pretty(8)) + 
  theme_gray()



# 5 rounds 
ggplot() + 
  geom_line(aes(x=dates_23_30, y=(df1 %>%
                                    filter(variable %in% c("LocDiag"), time>1298))$value, colour="Model output")) + 
  geom_line(aes(x=dates_23_30, y=(df1_pq5 %>%
                                    filter(variable %in% c("LocDiag"), time>1298))$value, colour="Model PQ MDA output")) + 
  geom_line(aes(x=dates_23_30, y=(df1_taf5 %>%
                                    filter(variable %in% c("LocDiag"), time>1298))$value, colour="Model TQ MDA output")) + 
  labs(title = paste0("Impact of 5 year MDA programme (", coverage, "% \ncoverage) on diagnosed weekly incidence"), x=("Date"),y =("Incidence"), colour="Legend") +
  scale_x_date(breaks = scales::breaks_pretty(8)) + 
  theme_gray()

# 7 rounds 
ggplot() + 
  geom_line(aes(x=dates_23_30, y=(df1 %>%
                                    filter(variable %in% c("LocDiag"), time>1298))$value, colour="Model output")) + 
  geom_line(aes(x=dates_23_30, y=(df1_pq7 %>%
                                    filter(variable %in% c("LocDiag"), time>1298))$value, colour="Model PQ MDA output")) + 
  geom_line(aes(x=dates_23_30, y=(df1_taf7 %>%
                                    filter(variable %in% c("LocDiag"), time>1298))$value, colour="Model TQ MDA output")) + 
  labs(title = paste0("Impact of 7 year MDA programme (", coverage, "% \ncoverage) on diagnosed weekly incidence"), x=("Date"),y =("Incidence"), colour="Legend") +
  scale_x_date(breaks = scales::breaks_pretty(8)) + 
  theme_gray()

# PQ success

# PQ all rounds 
ggplot() + 
  geom_line(aes(x=dates_23_30, y=(df1 %>%
                                    filter(variable %in% c("LocDiag"), time>1298))$value, colour="No MDA")) + 
  geom_line(aes(x=dates_23_30, y=(df1_pq3 %>%
                                    filter(variable %in% c("LocDiag"), time>1298))$value, colour="PQ MDA (3 years)")) + 
  geom_line(aes(x=dates_23_30, y=(df1_pq5 %>%
                                    filter(variable %in% c("LocDiag"), time>1298))$value, colour="PQ MDA (5 years)")) + 
  geom_line(aes(x=dates_23_30, y=(df1_pq7 %>%
                                    filter(variable %in% c("LocDiag"), time>1298))$value, colour="PQ MDA (7 years)")) + 
  labs(title = paste0("Impact of PQ MDA for 3,5 and 7 years (", coverage, "% coverage) \non diagnosed weekly incidence"), x=("Date"),y =("Incidence"), colour="Legend") +
  scale_x_date(breaks = scales::breaks_pretty(8)) + 
  theme_gray()

# TQ success

# TQ all rounds 
ggplot() + 
  geom_line(aes(x=dates_23_30, y=(df1 %>%
                                    filter(variable %in% c("LocDiag"), time>1298))$value, colour="No MDA")) + 
  geom_line(aes(x=dates_23_30, y=(df1_taf3 %>%
                                    filter(variable %in% c("LocDiag"), time>1298))$value, colour="TQ MDA (3 years)")) + 
  geom_line(aes(x=dates_23_30, y=(df1_taf5 %>%
                                    filter(variable %in% c("LocDiag"), time>1298))$value, colour="TQ MDA (5 years)")) + 
  geom_line(aes(x=dates_23_30, y=(df1_taf7 %>%
                                    filter(variable %in% c("LocDiag"), time>1298))$value, colour="TQ MDA (7 years)")) + 
  labs(title = paste0("Impact of TQ MDA for 3,5 and 7 years (", coverage, "% coverage) \non diagnosed weekly incidence"), x=("Date"),y =("Incidence"), colour="Legend") +
  scale_x_date(breaks = scales::breaks_pretty(8)) + 
  theme_gray()


# Percentage reductions in annual cases 2023-2030 ####

pd <- function(x,y){
  
  return((x-y)/x * 100)
  
}

model<-sum((df1 %>%filter(variable %in% c("LocDiag"), time>1299))$value)


MDA_pq3<-sum((df1_pq3 %>%filter(variable %in% c("LocDiag"), time>1299))$value)


MDA_pq5<-sum((df1_pq5 %>%filter(variable %in% c("LocDiag"), time>1299))$value)


MDA_pq7<-sum((df1_pq7 %>%filter(variable %in% c("LocDiag"), time>1299))$value)


MDA_taf3<-sum((df1_taf3 %>%filter(variable %in% c("LocDiag"), time>1299))$value)


MDA_taf5<-sum((df1_taf5 %>%filter(variable %in% c("LocDiag"), time>1299))$value)


MDA_taf7<-sum((df1_taf7 %>%filter(variable %in% c("LocDiag"), time>1299))$value)


Scenario <- c(paste0("PQ, 3 years, ", coverage, "% coverage"),
              paste0("PQ, 5 years, ", coverage, "% coverage"),
              paste0("PQ, 7 years, ", coverage, "% coverage"),
              paste0("TQ, 3 years, ", coverage, "% coverage"),
              paste0("TQ, 5 years, ", coverage, "% coverage"),
              paste0("TQ, 7 years, ", coverage, "% coverage"))

Average <- c(paste0(round(pd(model,MDA_pq3), digits=4), "%"),
             paste0(round(pd(model,MDA_pq5), digits=4), "%"),
             paste0(round(pd(model,MDA_pq7), digits=4), "%"),
             paste0(round(pd(model,MDA_taf3), digits=4), "%"),
             paste0(round(pd(model,MDA_taf5), digits=4), "%"),
             paste0(round(pd(model,MDA_taf7), digits=4), "%"))

Results <- data.frame(Scenario, Average)
print(Results)