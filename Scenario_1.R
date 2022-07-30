# Import packages ####
library(pacman)
p_load(deSolve, tidyverse, doParallel, manipulate,readxl,reshape2,ggfortify, knitr, printr,
       plyr, dplyr, lubridate, gridExtra, reshape2, TTR, unikn, ggpubr, zoo, scales)


# Define scenario variables ####

coverage <- "30"
param_file <- "rec03"

params <- as_tibble(as.data.frame(read_csv(paste0("https://raw.githubusercontent.com/student1445449/MGH_thesis/main/", param_file, ".csv"), show_col_types = FALSE))) # Read excel file with all parameters

# Run Models ####

source("C:/Users/iatkinson/Documents/!_Placement Research/R files/INCLUDE IN ZIP/BASESCRIPT_MODEL.R")

# Confidence intervals ####

#model
overall_sim_results = read_csv(paste0("https://raw.githubusercontent.com/student1445449/MGH_thesis_", coverage, "cov/main/sim_results.csv"), show_col_types = FALSE)
# 3 rounds

#PQ mda
overall_sim_results_pq3 = read_csv(paste0("https://raw.githubusercontent.com/student1445449/MGH_thesis_", coverage, "cov/main/sim_results_pq3.csv"), show_col_types = FALSE)

# taf mda
overall_sim_results_taf3 = read_csv(paste0("https://raw.githubusercontent.com/student1445449/MGH_thesis_", coverage, "cov/main/sim_results_taf3.csv"), show_col_types = FALSE)

# 5 rounds

#PQ mda
overall_sim_results_pq5 = read_csv(paste0("https://raw.githubusercontent.com/student1445449/MGH_thesis_", coverage, "cov/main/sim_results_pq5.csv"), show_col_types = FALSE)
# taf mda
overall_sim_results_taf5 = read_csv(paste0("https://raw.githubusercontent.com/student1445449/MGH_thesis_", coverage, "cov/main/sim_results_taf5.csv"), show_col_types = FALSE)
# 7 rounds

#PQ mda
overall_sim_results_pq7 = read_csv(paste0("https://raw.githubusercontent.com/student1445449/MGH_thesis_", coverage, "cov/main/sim_results_pq7.csv"), show_col_types = FALSE)
# taf mda
overall_sim_results_taf7 = read_csv(paste0("https://raw.githubusercontent.com/student1445449/MGH_thesis_", coverage, "cov/main/sim_results_taf7.csv"), show_col_types = FALSE)


#model
conf_int <- apply(overall_sim_results, 1, function(x){mean(x)+c(-1.96,1.96)*sd(x)/sqrt(length(x))})
conf_int_l <- conf_int[1,]
conf_int_u <- conf_int[2,]

# 3 rounds
# pq mda 
conf_int_pq3 <- apply(overall_sim_results_pq3, 1, function(x){mean(x)+c(-1.96,1.96)*sd(x)/sqrt(length(x))})
conf_int_l_pq3 <- conf_int_pq3[1,]
conf_int_u_pq3 <- conf_int_pq3[2,]

# taf mda
conf_int_taf3 <- apply(overall_sim_results_taf3, 1, function(x){mean(x)+c(-1.96,1.96)*sd(x)/sqrt(length(x))})
conf_int_l_taf3 <- conf_int_taf3[1,]
conf_int_u_taf3 <- conf_int_taf3[2,]


# 5 rounds
# pq mda 
conf_int_pq5 <- apply(overall_sim_results_pq5, 1, function(x){mean(x)+c(-1.96,1.96)*sd(x)/sqrt(length(x))})
conf_int_l_pq5 <- conf_int_pq5[1,]
conf_int_u_pq5 <- conf_int_pq5[2,]

# taf mda
conf_int_taf5 <- apply(overall_sim_results_taf5, 1, function(x){mean(x)+c(-1.96,1.96)*sd(x)/sqrt(length(x))})
conf_int_l_taf5 <- conf_int_taf5[1,]
conf_int_u_taf5 <- conf_int_taf5[2,]


# 7 rounds
# pq mda 
conf_int_pq7 <- apply(overall_sim_results_pq7, 1, function(x){mean(x)+c(-1.96,1.96)*sd(x)/sqrt(length(x))})
conf_int_l_pq7 <- conf_int_pq7[1,]
conf_int_u_pq7 <- conf_int_pq7[2,]

# taf mda
conf_int_taf7 <- apply(overall_sim_results_taf7, 1, function(x){mean(x)+c(-1.96,1.96)*sd(x)/sqrt(length(x))})
conf_int_l_taf7 <- conf_int_taf7[1,]
conf_int_u_taf7 <- conf_int_taf7[2,]


# 3 rounds 
ggplot() + 
  geom_line(aes(x=dates_23_30, y=(df1 %>%
                                    filter(variable %in% c("LocDiag"), time>1298))$value, colour="Model output")) + 
  geom_line(aes(x=dates_23_30, y=(df1_pq3 %>%
                                    filter(variable %in% c("LocDiag"), time>1298))$value, colour="Model PQ MDA output")) + 
  geom_line(aes(x=dates_23_30, y=(df1_taf3 %>%
                                    filter(variable %in% c("LocDiag"), time>1298))$value, colour="Model TQ MDA output")) + 
  geom_ribbon(aes(x = dates_23_30, ymin=conf_int_l[1300:1664], ymax=conf_int_u[1300:1664]), fill="red", alpha=0.2)+
  geom_ribbon(aes(x = dates_23_30, ymin=conf_int_l_pq3[1300:1664], ymax=conf_int_u_pq3[1300:1664]), fill="green", alpha=0.2)+
  geom_ribbon(aes(x = dates_23_30, ymin=conf_int_l_taf3[1300:1664], ymax=conf_int_u_taf3[1300:1664]), fill="blue", alpha=0.2)+
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
  geom_ribbon(aes(x = dates_23_30, ymin=conf_int_l[1300:1664], ymax=conf_int_u[1300:1664]), fill="red", alpha=0.2)+
  geom_ribbon(aes(x = dates_23_30, ymin=conf_int_l_pq5[1300:1664], ymax=conf_int_u_pq5[1300:1664]), fill="green", alpha=0.2)+
  geom_ribbon(aes(x = dates_23_30, ymin=conf_int_l_taf5[1300:1664], ymax=conf_int_u_taf5[1300:1664]), fill="blue", alpha=0.2)+
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
  geom_ribbon(aes(x = dates_23_30, ymin=conf_int_l[1300:1664], ymax=conf_int_u[1300:1664]), fill="red", alpha=0.2)+
  geom_ribbon(aes(x = dates_23_30, ymin=conf_int_l_pq7[1300:1664], ymax=conf_int_u_pq7[1300:1664]), fill="green", alpha=0.2)+
  geom_ribbon(aes(x = dates_23_30, ymin=conf_int_l_taf7[1300:1664], ymax=conf_int_u_taf7[1300:1664]), fill="blue", alpha=0.2)+
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
  geom_ribbon(aes(x = dates_23_30, ymin=conf_int_l[1300:1664], ymax=conf_int_u[1300:1664]), fill="red", alpha=0.2)+
  geom_ribbon(aes(x = dates_23_30, ymin=conf_int_l_pq3[1300:1664], ymax=conf_int_u_pq3[1300:1664]), fill="green", alpha=0.2)+
  geom_ribbon(aes(x = dates_23_30, ymin=conf_int_l_pq5[1300:1664], ymax=conf_int_u_pq5[1300:1664]), fill="blue", alpha=0.2)+
  geom_ribbon(aes(x = dates_23_30, ymin=conf_int_l_pq7[1300:1664], ymax=conf_int_u_pq7[1300:1664]), fill="purple", alpha=0.2)+
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
  geom_ribbon(aes(x = dates_23_30, ymin=conf_int_l[1300:1664], ymax=conf_int_u[1300:1664]), fill="red", alpha=0.2)+
  geom_ribbon(aes(x = dates_23_30, ymin=conf_int_l_taf3[1300:1664], ymax=conf_int_u_taf3[1300:1664]), fill="green", alpha=0.2)+
  geom_ribbon(aes(x = dates_23_30, ymin=conf_int_l_taf5[1300:1664], ymax=conf_int_u_taf5[1300:1664]), fill="blue", alpha=0.2)+
  geom_ribbon(aes(x = dates_23_30, ymin=conf_int_l_taf7[1300:1664], ymax=conf_int_u_taf7[1300:1664]), fill="purple", alpha=0.2)+
  labs(title = paste0("Impact of TQ MDA for 3,5 and 7 years (", coverage, "% coverage) \non diagnosed weekly incidence"), x=("Date"),y =("Incidence"), colour="Legend") +
  scale_x_date(breaks = scales::breaks_pretty(8)) + 
  theme_gray()


# Percentage reductions in annual cases 2023-2030 ####

annual_model<- as.data.frame(df1 %>% filter(variable %in% c("LocDiag"), time>1299))$value
annual_model_sum <- unname(tapply(annual_model, (seq_along(annual_model)-1) %/% 52, sum))

# PQ
# 3 rounds
annual_3_pq<- as.data.frame(df1_pq3 %>% filter(variable %in% c("LocDiag"), time>1299))$value
annual_3_pq_sum <- unname(tapply(annual_3_pq, (seq_along(annual_3_pq)-1) %/% 52, sum))

mat3_pq<- t(rbind(annual_model, annual_3_pq))
annual_model_sum_diff3_pq <- apply(mat3_pq, 1, function(x){diff(x)})
annual_model_sum_per3_pq <- mean(annual_model_sum_diff3_pq/mat3_pq[,1]*100)
annual_model_sum_cf3_pq <- apply(as.data.frame(annual_model_sum_diff3_pq/mat3_pq[,1]*100), 2, function(x){mean(x)+c(-1.96,1.96)*sd(x)/sqrt(length(x))})

# 5 rounds
annual_5_pq<- as.data.frame(df1_pq5 %>% filter(variable %in% c("LocDiag"), time>1299))$value
annual_5_pq_sum <- unname(tapply(annual_5_pq, (seq_along(annual_5_pq)-1) %/% 52, sum))

mat5_pq<- t(rbind(annual_model, annual_5_pq))
annual_model_sum_diff5_pq <- apply(mat5_pq, 1, function(x){diff(x)})
annual_model_sum_per5_pq <- mean(annual_model_sum_diff5_pq/mat5_pq[,1]*100)
annual_model_sum_cf5_pq <- apply(as.data.frame(annual_model_sum_diff5_pq/mat5_pq[,1]*100), 2, function(x){mean(x)+c(-1.96,1.96)*sd(x)/sqrt(length(x))})

# 7 rounds
annual_7_pq<- as.data.frame(df1_pq7 %>% filter(variable %in% c("LocDiag"), time>1299))$value
annual_7_pq_sum <- unname(tapply(annual_7_pq, (seq_along(annual_7_pq)-1) %/% 52, sum))

mat7_pq<- t(rbind(annual_model, annual_7_pq))
annual_model_sum_diff7_pq <- apply(mat7_pq, 1, function(x){diff(x)})
annual_model_sum_per7_pq <- mean(annual_model_sum_diff7_pq/mat7_pq[,1]*100)
annual_model_sum_cf7_pq <- apply(as.data.frame(annual_model_sum_diff7_pq/mat7_pq[,1]*100), 2, function(x){mean(x)+c(-1.96,1.96)*sd(x)/sqrt(length(x))})

# TQ
# 3 rounds
annual_3_taf<- as.data.frame(df1_taf3 %>% filter(variable %in% c("LocDiag"), time>1299))$value
annual_3_taf_sum <- unname(tapply(annual_3_taf, (seq_along(annual_3_taf)-1) %/% 52, sum))

mat3_taf<- t(rbind(annual_model, annual_3_taf))
annual_model_sum_diff3_taf <- apply(mat3_taf, 1, function(x){diff(x)})
annual_model_sum_per3_taf <- mean(annual_model_sum_diff3_taf/mat3_taf[,1]*100)
annual_model_sum_cf3_taf <- apply(as.data.frame(annual_model_sum_diff3_taf/mat3_taf[,1]*100), 2, function(x){mean(x)+c(-1.96,1.96)*sd(x)/sqrt(length(x))})

# 5 rounds
annual_5_taf<- as.data.frame(df1_taf5 %>% filter(variable %in% c("LocDiag"), time>1299))$value
annual_5_taf_sum <- unname(tapply(annual_5_taf, (seq_along(annual_5_taf)-1) %/% 52, sum))

mat5_taf<- t(rbind(annual_model, annual_5_taf))
annual_model_sum_diff5_taf <- apply(mat5_taf, 1, function(x){diff(x)})
annual_model_sum_per5_taf <- mean(annual_model_sum_diff5_taf/mat5_taf[,1]*100)
annual_model_sum_cf5_taf <- apply(as.data.frame(annual_model_sum_diff5_taf/mat5_taf[,1]*100), 2, function(x){mean(x)+c(-1.96,1.96)*sd(x)/sqrt(length(x))})

# 7 rounds
annual_7_taf<- as.data.frame(df1_taf7 %>% filter(variable %in% c("LocDiag"), time>1299))$value
annual_7_taf_sum <- unname(tapply(annual_7_taf, (seq_along(annual_7_taf)-1) %/% 52, sum))

mat7_taf<- t(rbind(annual_model, annual_7_taf))
annual_model_sum_diff7_taf <- apply(mat7_taf, 1, function(x){diff(x)})
annual_model_sum_per7_taf <- mean(annual_model_sum_diff7_taf/mat7_taf[,1]*100)
annual_model_sum_cf7_taf <- apply(as.data.frame(annual_model_sum_diff7_taf/mat7_taf[,1]*100), 2, function(x){mean(x)+c(-1.96,1.96)*sd(x)/sqrt(length(x))})

# Data-frame of results

Scenario <- c(paste0("PQ, 3 years, ", coverage, "% coverage"),
              paste0("PQ, 5 years, ", coverage, "% coverage"),
              paste0("PQ, 7 years, ", coverage, "% coverage"),
              paste0("TQ, 3 years, ", coverage, "% coverage"),
              paste0("TQ, 5 years, ", coverage, "% coverage"),
              paste0("TQ, 7 years, ", coverage, "% coverage"))

Average <- c(annual_model_sum_per3_pq,
             annual_model_sum_per5_pq,
             annual_model_sum_per7_pq,
             annual_model_sum_per3_taf,
             annual_model_sum_per5_taf,
             annual_model_sum_per7_taf)

Lower_Bound <- c(annual_model_sum_cf3_pq[2],
                 annual_model_sum_cf5_pq[2],
                 annual_model_sum_cf7_pq[2],
                 annual_model_sum_cf3_taf[2],
                 annual_model_sum_cf5_taf[2],
                 annual_model_sum_cf7_taf[2])

Upper_Bound <- c(annual_model_sum_cf3_pq[1],
                 annual_model_sum_cf5_pq[1],
                 annual_model_sum_cf7_pq[1],
                 annual_model_sum_cf3_taf[1],
                 annual_model_sum_cf5_taf[1],
                 annual_model_sum_cf7_taf[1])

Results <- data.frame(Scenario, Average, Lower_Bound, Upper_Bound)
print(Results)
