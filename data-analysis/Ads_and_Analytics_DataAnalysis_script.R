#Installing necessary packages
install.packages('ggplot2')
install.packages("dplyr")
install.packages( "ggstatsplot")
install.packages("svglite")
install.packages("car")
install.packages("moments")
install.packages("bestNormalize")
install.packages("effsize")
install.packages("readr")
install.packages("formattable")
install.packages("e1071")
require("dplyr")
library(ggplot2)
library(readr)
library(plyr)
library(tidyverse)
library(ggstatsplot)
library(car)
library(svglite)
library(moments)
library(bestNormalize)
library(effsize)
library(readr)
library(formattable)
library(e1071)

# Read raw data csv files and create dataframe
# Joule
joule_csv_paths_batch1 <- list.files( path="./batch1", pattern = "^Joule.*\\.csv", recursive = TRUE, full.names = TRUE)
joule_csv_paths_batch2 <- list.files( path="./batch2", pattern = "^Joule.*\\.csv", recursive = TRUE, full.names = TRUE)
experiment_joule_data_b1 <- joule_csv_paths_batch1 %>%
  lapply(read_csv) %>%
  bind_rows
experiment_joule_data_b1['subject'] <- joule_csv_paths_batch1 %>%
  strsplit('/', fixed = TRUE) %>%
  rapply(nth, n = 3) %>%
  factor
experiment_joule_data_b1['browser'] <- joule_csv_paths_batch1 %>%
  strsplit('/', fixed = TRUE) %>%
  rapply(nth, n = 4) %>%
  factor
experiment_joule_data_b2 <- joule_csv_paths_batch2 %>%
  lapply(read_csv) %>%
  bind_rows
experiment_joule_data_b2['subject'] <- joule_csv_paths_batch2 %>%
  strsplit('/', fixed = TRUE) %>%
  rapply(nth, n = 3) %>%
  factor
experiment_joule_data_b2['browser'] <- joule_csv_paths_batch2 %>%
  strsplit('/', fixed = TRUE) %>%
  rapply(nth, n = 4) %>%
  factor
#FCP
fcp_csv_paths_batch1 <- list.files( path="./batch1", pattern = "^fcp.*\\.csv", recursive = TRUE, full.names = TRUE)
fcp_csv_paths_batch2 <- list.files( path="./batch2", pattern = "^fcp.*\\.csv", recursive = TRUE, full.names = TRUE)
experiment_fcp_data_b1 <- fcp_csv_paths_batch1 %>%
  lapply(read_csv) %>%
  bind_rows
experiment_fcp_data_b1['subject'] <- fcp_csv_paths_batch1 %>%
  strsplit('/', fixed = TRUE) %>%
  rapply(nth, n = 3) %>%
  factor
experiment_fcp_data_b1['browser'] <- fcp_csv_paths_batch1 %>%
  strsplit('/', fixed = TRUE) %>%
  rapply(nth, n = 4) %>%
  factor
experiment_fcp_data_b2 <- fcp_csv_paths_batch2 %>%
  lapply(read_csv) %>%
  bind_rows
experiment_fcp_data_b2['subject'] <- fcp_csv_paths_batch2 %>%
  strsplit('/', fixed = TRUE) %>%
  rapply(nth, n = 3) %>%
  factor
experiment_fcp_data_b2['browser'] <- fcp_csv_paths_batch2 %>%
  strsplit('/', fixed = TRUE) %>%
  rapply(nth, n = 4) %>%
  factor
#TBT
tbt_csv_paths_batch1 <- list.files(path="./batch1", pattern = "^tbt.*\\.csv", recursive = TRUE, full.names = TRUE)
tbt_csv_paths_batch2 <- list.files(path="./batch2", pattern = "^tbt.*\\.csv", recursive = TRUE, full.names = TRUE)
experiment_tbt_data_b1 <- tbt_csv_paths_batch1 %>%
  lapply(read_csv) %>%
  bind_rows
experiment_tbt_data_b1['subject'] <- tbt_csv_paths_batch1 %>%
  strsplit('/', fixed = TRUE) %>%
  rapply(nth, n = 3) %>%
  factor
experiment_tbt_data_b1['browser'] <- tbt_csv_paths_batch1 %>%
  strsplit('/', fixed = TRUE) %>%
  rapply(nth, n = 4) %>%
  factor
experiment_tbt_data_b2 <- tbt_csv_paths_batch2 %>%
  lapply(read_csv) %>%
  bind_rows
experiment_tbt_data_b2['subject'] <- tbt_csv_paths_batch2 %>%
  strsplit('/', fixed = TRUE) %>%
  rapply(nth, n = 3) %>%
  factor
experiment_tbt_data_b2['browser'] <- tbt_csv_paths_batch2 %>%
  strsplit('/', fixed = TRUE) %>%
  rapply(nth, n = 4) %>%
  factor
#Payload
payload_csv_paths_batch1 <- list.files(path="./batch1", pattern = "^2022.*\\.txt", recursive = TRUE, full.names = TRUE)
payload_csv_paths_batch2 <- list.files(path="./batch2", pattern = "^2022.*\\.txt", recursive = TRUE, full.names = TRUE)
experiment_payload_data_b1 <- payload_csv_paths_batch1 %>%
  lapply(read.table) %>%
  bind_rows
experiment_payload_data_b1['subject'] <- payload_csv_paths_batch1 %>%
  strsplit('/', fixed = TRUE) %>%
  rapply(nth, n = 3) %>%
  factor
experiment_payload_data_b1['browser'] <- payload_csv_paths_batch1 %>%
  strsplit('/', fixed = TRUE) %>%
  rapply(nth, n = 4) %>%
  factor
experiment_payload_data_b2 <- payload_csv_paths_batch2 %>%
  lapply(read.table) %>%
  bind_rows
experiment_payload_data_b2['subject'] <- payload_csv_paths_batch2 %>%
  strsplit('/', fixed = TRUE) %>%
  rapply(nth, n = 3) %>%
  factor
experiment_payload_data_b2['browser'] <- payload_csv_paths_batch2 %>%
  strsplit('/', fixed = TRUE) %>%
  rapply(nth, n = 4) %>%
  factor

#Aggregate the recorded metrics across the 5 runs based on the subject and browser used

#Merge into one final dataframe containing all of the information
experiment_complete_data_b1 <- cbind(experiment_joule_data_b1, experiment_fcp_data_b1, experiment_payload_data_b1)
experiment_complete_data_b2 <- cbind(experiment_joule_data_b2, experiment_fcp_data_b2, experiment_payload_data_b2)
experiment_complete_data <- rbind(experiment_complete_data_b2,experiment_complete_data_b1)
experiment_complete_data<-subset(experiment_complete_data, select=which(!duplicated(colnames(experiment_complete_data)))) 

#Add treatment column for experiment
experiment_complete_data <- experiment_complete_data %>%
  mutate(type = case_when(        
    grepl("full", subject) ~ "full",
    grepl("noads", subject) ~ "noads",
    grepl("noad", subject) ~ "noads",
    grepl("noanalytics", subject) ~ "noanalytics",
  ))
#=========================================================
#==================Descriptive statistics=================
#=========================================================

get_descriptive_stats <- function(data, browser, type) {
  cat(sprintf("Descriptive statistics for '%s' and '%s'", browser, type))
  cat("\n")
  cat("\n")
  cat("Skewness for Energy Consumption: ", as.character(formattable(skewness(data[which(data$browser == browser & data$type==type), ]$Joule_calculated), digits = 2, format = "f")))
  cat("\n")
  cat("Kurtosis for Energy Consumption: ", as.character(formattable(kurtosis(data[which(data$browser == browser & data$type==type), ]$Joule_calculated), digits = 2, format = "f")))
  cat("\n")
  cat("Std dev for Energy Consumption: ", as.character(formattable(sd(data[which(data$browser == browser & data$type==type), ]$Joule_calculated), digits = 2, format = "f")))
  cat("\n")
  cat("Mean for Energy Consumption: ", as.character(formattable(mean(data[which(data$browser == browser & data$type==type), ]$Joule_calculated), digits = 2, format = "f")))
  cat("\n")
  cat("Max for Energy Consumption: ", as.character(formattable(max(data[which(data$browser == browser & data$type==type), ]$Joule_calculated), digits = 2, format = "f")))
  cat("\n")
  cat("Min for Energy Consumption: ", as.character(formattable(min(data[which(data$browser == browser & data$type==type), ]$Joule_calculated), digits = 2, format = "f")))
  cat("\n")
  cat("Median for Energy Consumption: ", as.character(formattable(median(data[which(data$browser == browser & data$type==type), ]$Joule_calculated), digits = 2, format = "f")))
  cat("\n")
  cat("Quantiles 25 and 75 for Energy Consumption: ", as.character(formattable(quantile(data[which(data$browser == browser & data$type==type), ]$Joule_calculated,c(0.25,0.75)), digits = 2, format = "f")))
  cat("\n")
  cat("\n")
  cat("Skewness for FCP: ", as.character(formattable(skewness(data[which(data$browser == browser & data$type==type), ]$fcp / 1000), digits = 2, format = "f")))
  cat("\n")
  cat("Kurtosis for FCP: ", as.character(formattable(kurtosis(data[which(data$browser == browser & data$type==type), ]$fcp /1000), digits = 2, format = "f")))
  cat("\n")
  cat("Std dev for FCP: ", as.character(formattable(sd(data[which(data$browser == browser & data$type==type), ]$fcp / 1000), digits = 2, format = "f")))
  cat("\n")
  cat("Mean for FCP: ", as.character(formattable(mean(data[which(data$browser == browser & data$type==type), ]$fcp / 1000), digits = 2, format = "f")))
  cat("\n")
  cat("Max for FCP: ", as.character(formattable(max(data[which(data$browser == browser & data$type==type), ]$fcp / 1000), digits = 2, format = "f")))
  cat("\n")
  cat("Min for FCP: ", as.character(formattable(min(data[which(data$browser == browser & data$type==type), ]$fcp / 1000), digits = 2, format = "f")))
  cat("\n")
  cat("Median for FCP: ", as.character(formattable(median(data[which(data$browser == browser & data$type==type), ]$fcp / 1000), digits = 2, format = "f")))
  cat("\n")
  cat("Quantiles 25 and 75  for FCP: ", as.character(formattable(quantile(data[which(data$browser == browser & data$type==type), ]$fcp / 1000, c(0.25,0.75)), digits = 2, format = "f")))
  cat("\n")
  cat("\n")
  cat("Skewness for FPLT: ", as.character(formattable(skewness(data[which(data$browser == browser & data$type==type), ]$V1 / 1000), digits = 2, format = "f")))
  cat("\n")
  cat("Kurtosis for FPLT: ", as.character(formattable(kurtosis(data[which(data$browser == browser & data$type==type), ]$V1 / 1000), digits = 2, format = "f")))
  cat("\n")
  cat("Std dev for FPLT: ", as.character(formattable(sd(data[which(data$browser == browser & data$type==type), ]$V1 / 1000), digits = 2, format = "f")))
  cat("\n")
  cat("Mean for FPLT: ", as.character(formattable(mean(data[which(data$browser == browser & data$type==type), ]$V1 / 1000), digits = 2, format = "f")))
  cat("\n")
  cat("Max for FPLT: ", as.character(formattable(max(data[which(data$browser == browser & data$type==type), ]$V1 / 1000), digits = 2, format = "f")))
  cat("\n")
  cat("Min for FPLT: ", as.character(formattable(min(data[which(data$browser == browser & data$type==type), ]$V1 / 1000), digits = 2, format = "f")))
  cat("\n")
  cat("Median for FPLT: ", as.character(formattable(median(data[which(data$browser == browser & data$type==type), ]$V1 / 1000), digits = 2, format = "f")))
  cat("\n")
  cat("Quantiles 25 and 75  for FPLT: ", as.character(formattable(quantile(data[which(data$browser == browser & data$type==type), ]$V1 / 1000,c(0.25,0.75)), digits = 2, format = "f")))
  cat("\n")
}

get_descriptive_stats(experiment_complete_data, "chrome", "full")
get_descriptive_stats(experiment_complete_data, "opera", "full")
get_descriptive_stats(experiment_complete_data, "chrome", "noads")
get_descriptive_stats(experiment_complete_data, "opera", "noads")
get_descriptive_stats(experiment_complete_data, "chrome", "noanalytics")
get_descriptive_stats(experiment_complete_data, "opera", "noanalytics")


#Check for normality

check_normality <- function(data, x, title) {
  plot(density(data), xlab=x, main=paste("Density plot for", title, sep=" "))
  car::qqPlot(data)
  print(shapiro.test(data)) 
}

#Energy Consumption normality check
check_normality(experiment_complete_data[which(experiment_complete_data$browser == "chrome" & experiment_complete_data$type=="full"), ]$Joule_calculated, "Energy Consumption (J)", "Chrome, ads and analytics")


check_normality(experiment_complete_data[which(experiment_complete_data$browser == "chrome" & experiment_complete_data$type=="noads"), ]$Joule_calculated, "Energy Consumption (J)", "Chrome, analytics") 

check_normality(experiment_complete_data[which(experiment_complete_data$browser == "chrome" & experiment_complete_data$type=="noanalytics"), ]$Joule_calculated, "Energy Consumption (J)", "Chrome, ads")


check_normality(experiment_complete_data[which(experiment_complete_data$browser == "opera" & experiment_complete_data$type=="full"), ]$Joule_calculated, "Energy Consumption (J)", "Opera, ads and analytics")

check_normality(experiment_complete_data[which(experiment_complete_data$browser == "opera" & experiment_complete_data$type=="noads"), ]$Joule_calculated, "Energy Consumption (J)", "Opera, analytics")

check_normality(experiment_complete_data[which(experiment_complete_data$browser == "opera" & experiment_complete_data$type=="noanalytics"), ]$Joule_calculated, "Energy Consumption (J)", "Opera, ads")

#FCP normality check
check_normality(experiment_complete_data[which(experiment_complete_data$browser == "chrome" & experiment_complete_data$type=="full"), ]$fcp, "FCP", "Chrome, ads and analytics")

check_normality(experiment_complete_data[which(experiment_complete_data$browser == "chrome" & experiment_complete_data$type=="noads"), ]$fcp, "FCP", "Chrome, analytics")

check_normality(experiment_complete_data[which(experiment_complete_data$browser == "chrome" & experiment_complete_data$type=="noanalytics"), ]$fcp, "FCP", "Chrome, ads")


check_normality(experiment_complete_data[which(experiment_complete_data$browser == "opera" & experiment_complete_data$type=="full"), ]$fcp, "FCP", "Opera, ads and analytics")

check_normality(experiment_complete_data[which(experiment_complete_data$browser == "opera" & experiment_complete_data$type=="noads"), ]$fcp, "FCP", "Opera, analytics")

check_normality(experiment_complete_data[which(experiment_complete_data$browser == "opera" & experiment_complete_data$type=="noanalytics"), ]$fcp, "FCP", "Opera, ads")


#FLPT normality check 
check_normality(experiment_complete_data[which(experiment_complete_data$browser == "chrome" & experiment_complete_data$type=="full"), ]$V1, "FPLT", "Chrome, ads and analytics")

check_normality(experiment_complete_data[which(experiment_complete_data$browser == "chrome" & experiment_complete_data$type=="noads"), ]$V1, "FPLT", "Chrome, analytics")

check_normality(experiment_complete_data[which(experiment_complete_data$browser == "chrome" & experiment_complete_data$type=="noanalytics"), ]$V1, "FPLT", "Chrome, ads")

check_normality(experiment_complete_data[which(experiment_complete_data$browser == "opera" & experiment_complete_data$type=="full"), ]$V1, "FPLT", "Opera, ads and analytics")

check_normality(experiment_complete_data[which(experiment_complete_data$browser == "opera" & experiment_complete_data$type=="noads"), ]$V1, "FPLT", "Opera, analytics")
check_normality(experiment_complete_data[which(experiment_complete_data$browser == "opera" & experiment_complete_data$type=="noanalytics"), ]$V1, "FPLT", "Opera, ads")

#Perform data normalization since we do not have normal distributions
joule_normalized <- predict(bestNormalize(experiment_complete_data$Joule_calculated))
fcp_normalized <- predict(bestNormalize(experiment_complete_data$fcp))
payload_normalized <- predict(bestNormalize(experiment_complete_data$V1))

experiment_complete_data$joule_normalized <- joule_normalized
experiment_complete_data$fcp_normalized <- fcp_normalized
experiment_complete_data$payload_normalized <- payload_normalized
#====Test normalized data for normality========
#Energy Consumption normality check
check_normality(experiment_complete_data[which(experiment_complete_data$browser == "chrome" & experiment_complete_data$type=="full"), ]$joule_normalized, "Energy Consumption (J)", "Chrome, ads and analytics")


check_normality(experiment_complete_data[which(experiment_complete_data$browser == "chrome" & experiment_complete_data$type=="noads"), ]$joule_normalized, "Energy Consumption (J)", "Chrome, analytics") 

check_normality(experiment_complete_data[which(experiment_complete_data$browser == "chrome" & experiment_complete_data$type=="noanalytics"), ]$joule_normalized, "Energy Consumption (J)", "Chrome, ads")


check_normality(experiment_complete_data[which(experiment_complete_data$browser == "opera" & experiment_complete_data$type=="full"), ]$joule_normalized, "Energy Consumption (J)", "Opera, ads and analytics")

check_normality(experiment_complete_data[which(experiment_complete_data$browser == "opera" & experiment_complete_data$type=="noads"), ]$joule_normalized, "Energy Consumption (J)", "Opera, analytics")

check_normality(experiment_complete_data[which(experiment_complete_data$browser == "opera" & experiment_complete_data$type=="noanalytics"), ]$joule_normalized, "Energy Consumption (J)", "Opera, ads")

#FCP normality check
check_normality(experiment_complete_data[which(experiment_complete_data$browser == "chrome" & experiment_complete_data$type=="full"), ]$fcp_normalized, "FCP", "Chrome, ads and analytics")

check_normality(experiment_complete_data[which(experiment_complete_data$browser == "chrome" & experiment_complete_data$type=="noads"), ]$fcp_normalized, "FCP", "Chrome, analytics")

check_normality(experiment_complete_data[which(experiment_complete_data$browser == "chrome" & experiment_complete_data$type=="noanalytics"), ]$fcp_normalized, "FCP", "Chrome, ads")


check_normality(experiment_complete_data[which(experiment_complete_data$browser == "opera" & experiment_complete_data$type=="full"), ]$fcp_normalized, "FCP", "Opera, ads and analytics")

check_normality(experiment_complete_data[which(experiment_complete_data$browser == "opera" & experiment_complete_data$type=="noads"), ]$fcp_normalized, "FCP", "Opera, analytics")

check_normality(experiment_complete_data[which(experiment_complete_data$browser == "opera" & experiment_complete_data$type=="noanalytics"), ]$fcp_normalized, "FCP", "Opera, ads")


#FLPT normality check 
check_normality(experiment_complete_data[which(experiment_complete_data$browser == "chrome" & experiment_complete_data$type=="full"), ]$payload_normalized, "FPLT", "Chrome, ads and analytics")

check_normality(experiment_complete_data[which(experiment_complete_data$browser == "chrome" & experiment_complete_data$type=="noads"), ]$payload_normalized, "FPLT", "Chrome, analytics")

check_normality(experiment_complete_data[which(experiment_complete_data$browser == "chrome" & experiment_complete_data$type=="noanalytics"), ]$payload_normalized, "FPLT", "Chrome, ads")

check_normality(experiment_complete_data[which(experiment_complete_data$browser == "opera" & experiment_complete_data$type=="full"), ]$payload_normalized, "FPLT", "Opera, ads and analytics")

check_normality(experiment_complete_data[which(experiment_complete_data$browser == "opera" & experiment_complete_data$type=="noads"), ]$payload_normalized, "FPLT", "Opera, analytics")
check_normality(experiment_complete_data[which(experiment_complete_data$browser == "opera" & experiment_complete_data$type=="noanalytics"), ]$payload_normalized, "FPLT", "Opera, ads")

#BoxPlots / Violin graphs 
full_vs_noads_colors <- c("#ffbaba", "#a8e5e7")  # Add your desired colors here
full_vs_noads_labels <- c("Ads & Analytics", "Without Ads")  # Add your desired labels here

full_vs_noanalytics_colors <- c("#ffbaba", "#a76e9d")  # Add your desired colors here
full_vs_noanalytics_labels <- c("Ads & Analytics", "Without Analytics")  # Add your desired labels here

#==========Density PLOTS==========

#========Energy Consumption==========
#=========Chrome=========
joule_full_vs_noads_chrome_dp <- ggplot(subset(experiment_complete_data, browser=="chrome" & type != "noanalytics"), aes(x=Joule_calculated, fill = type)) +
  geom_density(alpha = 0.7)  +
  ylab("Density") + 
  xlab("Energy Consumption (Joule)") +
  theme(
    panel.background = element_rect(fill = "white"),  # Set background to white
    axis.title.x = element_text(hjust = 0.5, size = 25),
    axis.title.y = element_text(hjust = 0.5, size = 25, vjust = + 1.2),
    axis.text.x = element_text(size = 25),
    axis.text.y = element_text(size = 25),  
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 15),
    legend.position = c(0.8, 0.8),  # Adjust legend position inside the plot
  ) +
  scale_x_continuous(limits = c(0, 1500))+    # Set X-axis limits
  scale_y_continuous(limits = c(0.000, 0.005))+    # Set X-axis limits
  scale_fill_manual(name = "Treatment", values = full_vs_noads_colors, labels = full_vs_noads_labels)  # Set custom fill colors and labels

joule_full_vs_noads_chrome_dp    
#Save as pdf
ggsave('./plots/joule_full_vs_noads_chrome_dp.pdf', scale = 1.5, height = 12, width = 22, unit='cm', bg='transparent')

#New
joule_full_vs_noanalytics_chrome_dp <- ggplot(subset(experiment_complete_data, browser=="chrome" & type != "noads"), aes(x=Joule_calculated, fill = type)) +
geom_density(alpha = 0.7)  +
  ylab("Density") + 
  xlab("Energy Consumption (Joule)") +
  theme(
    panel.background = element_rect(fill = "white"),  # Set background to white
    axis.title.x = element_text(hjust = 0.5, size = 25),
    axis.title.y = element_text(hjust = 0.5, size = 25, vjust = + 1.2),
    axis.text.x = element_text(size = 25),
    axis.text.y = element_text(size = 25),  
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 15),
    legend.position = c(0.8, 0.8),  # Adjust legend position inside the plot
  ) +
  scale_x_continuous(limits = c(0, 1500))+    # Set X-axis limits
  scale_y_continuous(limits = c(0.000, 0.005))+    # Set X-axis limits
  scale_fill_manual(name = "Treatment", values = full_vs_noanalytics_colors, labels = full_vs_noanalytics_labels)  # Set custom fill colors and labels

joule_full_vs_noanalytics_chrome_dp    
#Save as pdf
ggsave('./plots/joule_full_vs_noanalytics_chrome_dp.pdf', scale = 1.5, height = 12, width = 22, unit='cm')

#=========Opera=========
joule_full_vs_noads_opera_dp <- ggplot(subset(experiment_complete_data, browser=="opera" & type != "noanalytics"), aes(x=Joule_calculated, fill = type)) +
  geom_density(alpha = 0.7)  +
  ylab("Density") + 
  xlab("Energy Consumption (Joule)") +
  theme(
    panel.background = element_rect(fill = "white"),  # Set background to white
    axis.title.x = element_text(hjust = 0.5, size = 25),
    axis.title.y = element_text(hjust = 0.5, size = 25, vjust = + 1.2),
    axis.text.x = element_text(size = 25),
    axis.text.y = element_text(size = 25),  
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 15),
    legend.position = c(0.8, 0.8),  # Adjust legend position inside the plot
  ) +
  scale_x_continuous(limits = c(0, 1500))+    # Set X-axis limits
  scale_y_continuous(limits = c(0.000, 0.005))+    # Set X-axis limits
  scale_fill_manual(name = "Treatment", values = full_vs_noads_colors, labels = full_vs_noads_labels)  # Set custom fill colors and labels

joule_full_vs_noads_opera_dp    
#Save as pdf
ggsave('./plots/joule_full_vs_noads_opera_dp.pdf', scale = 1.5, height = 12, width = 22, unit='cm', bg='transparent')

#New
joule_full_vs_noanalytics_opera_dp <- ggplot(subset(experiment_complete_data, browser=="opera" & type != "noads"), aes(x=Joule_calculated, fill = type)) +
geom_density(alpha = 0.7)  +
  ylab("Density") + 
  xlab("Energy Consumption (Joule)") +
  theme(
    panel.background = element_rect(fill = "white"),  # Set background to white
    axis.title.x = element_text(hjust = 0.5, size = 25),
    axis.title.y = element_text(hjust = 0.5, size = 25, vjust = + 1.2),
    axis.text.x = element_text(size = 25),
    axis.text.y = element_text(size = 25),  
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 15),
    legend.position = c(0.8, 0.8),  # Adjust legend position inside the plot
  ) +
  scale_x_continuous(limits = c(0, 1500))+    # Set X-axis limits
  scale_y_continuous(limits = c(0.000, 0.005))+    # Set X-axis limits
  scale_fill_manual(name = "Treatment", values = full_vs_noanalytics_colors, labels = full_vs_noanalytics_labels)  # Set custom fill colors and labels

joule_full_vs_noanalytics_opera_dp    
#Save as pdf
ggsave('./plots/joule_full_vs_noanalytics_opera_dp.pdf', scale = 1.5, height = 12, width = 22, unit='cm')

#=========================================
#========FCP============
#=========Chrome=========
fcp_full_vs_noads_chrome_dp <- ggplot(subset(experiment_complete_data, browser=="chrome" & type != "noanalytics"), aes(x=fcp, fill = type)) +
  geom_density(alpha = 0.7 )  +
  ylab("Density") + 
  xlab("First Contentful Paint (ms)") +
  theme(
    panel.background = element_rect(fill = "white"),  # Set background to white
    axis.title.x = element_text(hjust = 0.5, size = 25),
    axis.title.y = element_text(hjust = 0.5, size = 25, vjust = + 1.2),
    axis.text.x = element_text(size = 25),
    axis.text.y = element_text(size = 25),  
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 15),
    legend.position = c(0.8, 0.8),  # Adjust legend position inside the plot
  ) +
  scale_x_continuous(limits = c(0, 6000))+    # Set X-axis limits
  scale_y_continuous(limits = c(0.0000, 0.00065))+    # Set X-axis limits
  scale_fill_manual(name = "Treatment", values = full_vs_noads_colors, labels = full_vs_noads_labels)  # Set custom fill colors and labels

fcp_full_vs_noads_chrome_dp    
#Save as pdf
ggsave('./plots/fcp_full_vs_noads_chrome_dp.pdf', scale = 1.5, height = 12, width = 22, unit='cm')


#New
fcp_full_vs_noanalytics_chrome_dp <- ggplot(subset(experiment_complete_data, browser=="chrome" & type != "noads"), aes(x=fcp, fill = type)) +
  geom_density(alpha = 0.7)  +
  ylab("Density") + 
  xlab("First Contentful Paint (ms)") +
  theme(
    panel.background = element_rect(fill = "white"),  # Set background to white
    axis.title.x = element_text(hjust = 0.5, size = 25),
    axis.title.y = element_text(hjust = 0.5, size = 25, vjust = + 1.2),
    axis.text.x = element_text(size = 25),
    axis.text.y = element_text(size = 25),  
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 15),
    legend.position = c(0.8, 0.8),  # Adjust legend position inside the plot
  ) +
  scale_x_continuous(limits = c(0, 6000))+    # Set X-axis limits
  scale_y_continuous(limits = c(0.0000, 0.00065))+    # Set X-axis limits
  scale_fill_manual(name = "Treatment", values = full_vs_noanalytics_colors, labels = full_vs_noanalytics_labels)  # Set custom fill colors and labels

fcp_full_vs_noanalytics_chrome_dp    
#Save as pdf
ggsave('./plots/fcp_full_vs_noanalytics_chrome_dp.pdf', scale = 1.5, height = 12, width = 22, unit='cm')

#=========Opera=========
fcp_full_vs_noads_opera_dp <- ggplot(subset(experiment_complete_data, browser=="opera" & type != "noanalytics"), aes(x=fcp, fill = type)) +
  geom_density(alpha = 0.7)  +
  ylab("Density") + 
  xlab("First Contentful Paint (ms)") +
  theme(
    panel.background = element_rect(fill = "white"),  # Set background to white
    axis.title.x = element_text(hjust = 0.5, size = 25),
    axis.title.y = element_text(hjust = 0.5, size = 25, vjust = + 1.2),
    axis.text.x = element_text(size = 25),
    axis.text.y = element_text(size = 25),  
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 15),
    legend.position = c(0.8, 0.8),  # Adjust legend position inside the plot
  ) +
  scale_x_continuous(limits = c(0, 4000))+    # Set X-axis limits
  scale_y_continuous(limits = c(0.0000, 0.0015))+    # Set X-axis limits
  scale_fill_manual(name = "Treatment", values = full_vs_noads_colors, labels = full_vs_noads_labels)  # Set custom fill colors and labels

fcp_full_vs_noads_opera_dp    
#Save as pdf
ggsave('./plots/fcp_full_vs_noads_opera_dp.pdf', scale = 1.5, height = 12, width = 22, unit='cm')

#New
fcp_full_vs_noanalytics_opera_dp <- ggplot(subset(experiment_complete_data, browser=="opera" & type != "noads"), aes(x=fcp, fill = type)) +
  geom_density(alpha = 0.7)  +
  ylab("Density") + 
  xlab("First Contentful Paint (ms)") +
  theme(
    panel.background = element_rect(fill = "white"),  # Set background to white
    axis.title.x = element_text(hjust = 0.5, size = 25),
    axis.title.y = element_text(hjust = 0.5, size = 25, vjust = + 1.2),
    axis.text.x = element_text(size = 25),
    axis.text.y = element_text(size = 25),  
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 15),
    legend.position = c(0.8, 0.8),  # Adjust legend position inside the plot
  ) +
  scale_x_continuous(limits = c(0, 4000))+    # Set X-axis limits
  scale_y_continuous(limits = c(0.0000, 0.0015))+    # Set X-axis limits
  scale_fill_manual(name = "Treatment", values = full_vs_noanalytics_colors, labels = full_vs_noanalytics_labels)  # Set custom fill colors and labels


fcp_full_vs_noanalytics_opera_dp    
#Save as pdf
ggsave('./plots/fcp_full_vs_noanalytics_opera_dp.pdf', scale = 1.5, height = 12, width = 22, unit='cm')


#=========FPLT===========
#=========Chrome=========
fplt_full_vs_noads_chrome_dp <- ggplot(subset(experiment_complete_data, browser=="chrome" & type != "noanalytics"), aes(x=V1, fill = type)) +
  geom_density(alpha = 0.7 )  +
  ylab("Density") + 
  xlab("Full-Page Load Time (ms)") +
  theme(
    panel.background = element_rect(fill = "white"),  # Set background to white
    axis.title.x = element_text(hjust = 0.5, size = 25),
    axis.title.y = element_text(hjust = 0.5, size = 25, vjust = + 1.2),
    axis.text.x = element_text(size = 25),
    axis.text.y = element_text(size = 25),  
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 15),
    legend.position = c(0.8, 0.8),  # Adjust legend position inside the plot
  ) +
  scale_x_continuous(limits = c(0, 18000))+    # Set X-axis limits
  scale_y_continuous(limits = c(0.0000, 0.00025))+    # Set X-axis limits
  scale_fill_manual(name = "Treatment", values = full_vs_noads_colors, labels = full_vs_noads_labels)  # Set custom fill colors and labels

fplt_full_vs_noads_chrome_dp    
#Save as pdf
ggsave('./plots/fplt_full_vs_noads_chrome_dp.pdf', scale = 1.5, height = 12, width = 22, unit='cm')


#New
fplt_full_vs_noanalytics_chrome_dp <- ggplot(subset(experiment_complete_data, browser=="chrome" & type != "noads"), aes(x=V1, fill = type)) +
  geom_density(alpha = 0.7)  +
  ylab("Density") + 
  xlab("Full-Page Load Time (ms)") +
  theme(
    panel.background = element_rect(fill = "white"),  # Set background to white
    axis.title.x = element_text(hjust = 0.5, size = 25),
    axis.title.y = element_text(hjust = 0.5, size = 25, vjust = + 1.2),
    axis.text.x = element_text(size = 25),
    axis.text.y = element_text(size = 25),  
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 15),
    legend.position = c(0.8, 0.8),  # Adjust legend position inside the plot
  ) +
  scale_x_continuous(limits = c(0, 18000))+    # Set X-axis limits
  scale_y_continuous(limits = c(0.0000, 0.00025))+    # Set X-axis limits
  scale_fill_manual(name = "Treatment", values = full_vs_noanalytics_colors, labels = full_vs_noanalytics_labels)  # Set custom fill colors and labels

fplt_full_vs_noanalytics_chrome_dp    
#Save as pdf
ggsave('./plots/fplt_full_vs_noanalytics_chrome_dp.pdf', scale = 1.5, height = 12, width = 22, unit='cm')

#=========Opera=========
fplt_full_vs_noads_opera_dp <- ggplot(subset(experiment_complete_data, browser=="opera" & type != "noanalytics"), aes(x=V1, fill = type)) +
  geom_density(alpha = 0.7)  +
  ylab("Density") + 
  xlab("Full-Page Load Time (ms)") +
  theme(
    panel.background = element_rect(fill = "white"),  # Set background to white
    axis.title.x = element_text(hjust = 0.5, size = 25),
    axis.title.y = element_text(hjust = 0.5, size = 25, vjust = + 1.2),
    axis.text.x = element_text(size = 25),
    axis.text.y = element_text(size = 25),  
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 15),
    legend.position = c(0.8, 0.8),  # Adjust legend position inside the plot
  ) +
  scale_x_continuous(limits = c(0, 18000))+    # Set X-axis limits
  scale_y_continuous(limits = c(0.0000, 0.00025))+    # Set X-axis limits
  scale_fill_manual(name = "Treatment", values = full_vs_noads_colors, labels = full_vs_noads_labels)  # Set custom fill colors and labels

fplt_full_vs_noads_opera_dp    
#Save as pdf
ggsave('./plots/fplt_full_vs_noads_opera_dp.pdf', scale = 1.5, height = 12, width = 22, unit='cm')

#New
fplt_full_vs_noanalytics_opera_dp <- ggplot(subset(experiment_complete_data, browser=="opera" & type != "noads"), aes(x=V1, fill = type)) +
  geom_density(alpha = 0.7)  +
  ylab("Density") + 
  xlab("Full-Page Load Time (ms)") +
  theme(
    panel.background = element_rect(fill = "white"),  # Set background to white
    axis.title.x = element_text(hjust = 0.5, size = 25),
    axis.title.y = element_text(hjust = 0.5, size = 25, vjust = + 1.2),
    axis.text.x = element_text(size = 25),
    axis.text.y = element_text(size = 25),  
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 15),
    legend.position = c(0.8, 0.8),  # Adjust legend position inside the plot
  ) +
  scale_x_continuous(limits = c(0, 18000))+    # Set X-axis limits
  scale_y_continuous(limits = c(0.0000, 0.00025))+    # Set X-axis limits
  scale_fill_manual(name = "Treatment", values = full_vs_noanalytics_colors, labels = full_vs_noanalytics_labels)  # Set custom fill colors and labels


fplt_full_vs_noanalytics_opera_dp    
#Save as pdf
ggsave('./plots/fplt_full_vs_noanalytics_opera_dp.pdf', scale = 1.5, height = 12, width = 22, unit='cm')


###3 Treatments in fig
#================================Exploratory BOXPLOTS
joule_bp_chrome <- ggplot(subset(experiment_complete_data, browser=="chrome"), aes(x=type, y=Joule_calculated, fill = type)) +
  xlab("Treatment type") + ylab("First Contentful Paint (ms)") + ggtitle("Energy Consumption for ads and analytics in chrome") +
  geom_boxplot(outlier.size =2,) +
  stat_summary(fun = mean, color="black", geom="point", shape=1, size = 2) +
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5))
#Exploratory
joule_bp_opera <- ggplot(subset(experiment_complete_data, browser=="opera"), aes(x=type, y=Joule_calculated, fill = type)) +
  xlab("Treatment type") + ylab("Energy Consumption (Joule)") + ggtitle("Energy Consumption for ads and analytics in opera") +
  geom_boxplot(outlier.size =2,) +
  stat_summary(fun = mean, color="black", geom="point", shape=1, size = 2) +
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5))


#==================Violins
#=============Energy Consumption violins=============
violin_colors <- c("#FFBABA", "#A8E5E7", "#A76E9D")
violin_labels <- c("full", "no_ads", "no_analytics")

joule_violin_chrome <- ggplot(subset(experiment_complete_data, browser=="chrome"), aes(x=type, y=Joule_calculated, fill = type)) +
  xlab("Treatment") + 
  ylab("Energy Consumption (Joule)")+
  geom_violin(trim = FALSE, alpha=0.5,) +
  scale_fill_manual(values = violin_colors, labels = violin_labels) +  # Set custom fill colors
  scale_x_discrete(labels = violin_labels) +  # Specify x-axis labels
  theme(
    legend.position="none",
    panel.background = element_rect(fill = "white"),  # Set background to white
    axis.title.x = element_text(hjust = 0.5, size = 25),
    axis.title.y = element_text(hjust = 0.5, size = 25, vjust = + 1.2),
    axis.text.x = element_text(size = 25),
    axis.text.y = element_text(size = 25),  
    plot.title = element_text(hjust = 0.5, size = 25)  # Center and increase font size of the title
  ) +
  scale_y_continuous(limits = c(0, 1800)) +
  ggtitle("Energy Consumption in Chrome")  

joule_violin_chrome    
#Save as pdf
ggsave('./plots/joule_violin_chrome.pdf', scale = 1.5, height = 12, width = 22, unit='cm')

#New
joule_violin_opera<- ggplot(subset(experiment_complete_data, browser=="opera"), aes(x=type, y=Joule_calculated, fill = type)) +
  xlab("Treatment") +
  ylab("Energy Consumption (Joule)")+
  geom_violin(trim = FALSE, alpha=0.5,) +
  scale_fill_manual(values = violin_colors, labels = violin_labels) +  # Set custom fill colors
  scale_x_discrete(labels = violin_labels) +  # Specify x-axis labels
  theme(
    legend.position="none",
    panel.background = element_rect(fill = "white"),  # Set background to white
    axis.title.x = element_text(hjust = 0.5, size = 25),
    axis.title.y = element_text(hjust = 0.5, size = 25, vjust = + 1.2),
    axis.text.x = element_text(size = 25),
    axis.text.y = element_text(size = 25),  
    plot.title = element_text(hjust = 0.5, size = 25)  # Center and increase font size of the title
  ) +
  scale_y_continuous(limits = c(0, 1800)) +
  ggtitle("Energy Consumption in Opera")    # Set X-axis limits

joule_violin_opera    
#Save as pdf
ggsave('./plots/joule_violin_opera.pdf', scale = 1.5, height = 12, width = 22, unit='cm')


#=============FCP violins=============
fcp_violin_chrome <- ggplot(subset(experiment_complete_data, browser=="chrome"), aes(x=type, y=fcp, fill = type)) +
  xlab("Treatment") + 
  ylab("FCP (ms)")  +
  geom_violin(trim = FALSE, alpha=0.5,) +
  scale_fill_manual(values = violin_colors, labels = violin_labels) +  # Set custom fill colors
  scale_x_discrete(labels = violin_labels) +  # Specify x-axis labels
  theme(
    legend.position="none",
    panel.background = element_rect(fill = "white"),  # Set background to white
    axis.title.x = element_text(hjust = 0.5, size = 25),
    axis.title.y = element_text(hjust = 0.5, size = 25, vjust = + 1.2),
    axis.text.x = element_text(size = 25),
    axis.text.y = element_text(size = 25),  
    plot.title = element_text(hjust = 0.5, size = 25)  # Center and increase font size of the title
  ) +
  ggtitle("FCP in Chrome") +
  scale_y_continuous(limits = c(0, 6000)) 
  
fcp_violin_chrome    
#Save as pdf
ggsave('./plots/fcp_violin_chrome.pdf', scale = 1.5, height = 12, width = 22, unit='cm')

#New
fcp_violin_opera <- ggplot(subset(experiment_complete_data, browser=="opera"), aes(x=type, y=fcp, fill = type)) +
  xlab("Treatment") +
  ylab("FCP (ms)")  +
  geom_violin(trim = FALSE, alpha=0.5,) +
  scale_fill_manual(values = violin_colors, labels = violin_labels) +  # Set custom fill colors
  scale_x_discrete(labels = violin_labels) +  # Specify x-axis labels
  theme(
    legend.position="none",
    panel.background = element_rect(fill = "white"),  # Set background to white
    axis.title.x = element_text(hjust = 0.5, size = 25),
    axis.title.y = element_text(hjust = 0.5, size = 25, vjust = + 1.2),
    axis.text.x = element_text(size = 25),
    axis.text.y = element_text(size = 25),  
    plot.title = element_text(hjust = 0.5, size = 25)  # Center and increase font size of the title
  ) +
  ggtitle("FCP in Opera") +
  scale_y_continuous(limits = c(0, 6000)) 

fcp_violin_opera    
#Save as pdf
ggsave('./plots/fcp_violin_opera.pdf', scale = 1.5, height = 12, width = 22, unit='cm')

#=============FPLT violins=============
payload_violin_chrome <- ggplot(subset(experiment_complete_data, browser=="chrome"), aes(x=type, y=V1, fill = type)) +
  xlab("Treatment") + 
  ylab("FPLT (ms)")  +
  geom_violin(trim = FALSE, alpha=0.5,) +
  scale_fill_manual(values = violin_colors, labels = violin_labels) +  # Set custom fill colors
  scale_x_discrete(labels = violin_labels) +  # Specify x-axis labels
  theme(
    legend.position="none",
    panel.background = element_rect(fill = "white"),  # Set background to white
    axis.title.x = element_text(hjust = 0.5, size = 25),
    axis.title.y = element_text(hjust = 0.5, size = 25, vjust = + 1.2),
    axis.text.x = element_text(size = 25),
    axis.text.y = element_text(size = 25),  
    plot.title = element_text(hjust = 0.5, size = 25)  # Center and increase font size of the title
  ) +
  ggtitle("FPLT in Chrome") +
  scale_y_continuous(limits = c(0, 25000)) 

payload_violin_chrome    
#Save as pdf
ggsave('./plots/payload_violin_chrome.pdf', scale = 1.5, height = 12, width = 22, unit='cm')

#New
payload_violin_opera <- ggplot(subset(experiment_complete_data, browser=="opera"), aes(x=type, y=V1, fill = type)) +
  xlab("Treatment") +
  ylab("FPLT (ms)")  +
  geom_violin(trim = FALSE, alpha=0.5,) +
  scale_fill_manual(values = violin_colors, labels = violin_labels) +  # Set custom fill colors
  scale_x_discrete(labels = violin_labels) +  # Specify x-axis labels
  theme(
    legend.position="none",
    panel.background = element_rect(fill = "white"),  # Set background to white
    axis.title.x = element_text(hjust = 0.5, size = 25),
    axis.title.y = element_text(hjust = 0.5, size = 25, vjust = + 1.2),
    axis.text.x = element_text(size = 25),
    axis.text.y = element_text(size = 25),  
    plot.title = element_text(hjust = 0.5, size = 25)  # Center and increase font size of the title
  ) +
  ggtitle("FPLT in Opera") +
  scale_y_continuous(limits = c(0, 25000)) 

payload_violin_opera    
#Save as pdf
ggsave('./plots/payload_violin_opera.pdf', scale = 1.5, height = 12, width = 22, unit='cm')

#===========Histograms============
#========Energy============
#=====Before normalization========
c1 <- rgb(255,149,148, max = 255, alpha= 95, names = "Powder blue")
c3 <- rgb(146,151,255, max = 255, alpha= 95, names = "Cool grey")

breakpoint_chrome_min <- min(c(experiment_complete_data[which(experiment_complete_data$browser == "chrome" & experiment_complete_data$type=="full"), ]$Joule_calculated,
                               experiment_complete_data[which(experiment_complete_data$browser == "chrome" & experiment_complete_data$type=="noads"), ]$Joule_calculated,
                               experiment_complete_data[which(experiment_complete_data$browser == "chrome" & experiment_complete_data$type=="noanalytics"), ]$Joule_calculated))# Set the minimum for the breakpoints
breakpoint_chrome_max <- max(c(experiment_complete_data[which(experiment_complete_data$browser == "chrome" & experiment_complete_data$type=="full"), ]$Joule_calculated,
                               experiment_complete_data[which(experiment_complete_data$browser == "chrome" & experiment_complete_data$type=="noads"), ]$Joule_calculated,
                               experiment_complete_data[which(experiment_complete_data$browser == "chrome" & experiment_complete_data$type=="noanalytics"), ]$Joule_calculated)) # Set the maximum for the breakpoints

breakpoint_opera_min <- min(c(experiment_complete_data[which(experiment_complete_data$browser == "opera" & experiment_complete_data$type=="full"), ]$Joule_calculated,
                              experiment_complete_data[which(experiment_complete_data$browser == "opera" & experiment_complete_data$type=="noads"), ]$Joule_calculated,
                              experiment_complete_data[which(experiment_complete_data$browser == "opera" & experiment_complete_data$type=="noanalytics"), ]$Joule_calculated))# Set the minimum for the breakpoints
breakpoint_opera_max <- max(c(experiment_complete_data[which(experiment_complete_data$browser == "opera" & experiment_complete_data$type=="full"), ]$Joule_calculated,
                              experiment_complete_data[which(experiment_complete_data$browser == "opera" & experiment_complete_data$type=="noads"), ]$Joule_calculated,
                              experiment_complete_data[which(experiment_complete_data$browser == "opera" & experiment_complete_data$type=="noanalytics"), ]$Joule_calculated)) # Set the maximum for the breakpoints


ax_chrome <- pretty(breakpoint_chrome_min:breakpoint_chrome_max, n = 30) # Make a neat vector for the breakpoints
ax_opera <- pretty(breakpoint_opera_min:breakpoint_opera_max, n = 30) # Make a neat vector for the breakpoints

hg_chrome_full <- hist(experiment_complete_data[which(experiment_complete_data$browser == "chrome" & experiment_complete_data$type=="full"), ]$Joule_calculated, breaks = ax_chrome, plot = FALSE) # Save first histogram data
hg_chrome_noads <- hist(experiment_complete_data[which(experiment_complete_data$browser == "chrome" & experiment_complete_data$type=="noads"), ]$Joule_calculated, breaks = ax_chrome, plot = FALSE) # Save 2nd histogram data
hg_chrome_noanalytics <- hist(experiment_complete_data[which(experiment_complete_data$browser == "chrome" & experiment_complete_data$type=="noanalytics"), ]$Joule_calculated, breaks = ax_chrome, plot = FALSE) # Save 2nd histogram data

hg_opera_full <- hist(experiment_complete_data[which(experiment_complete_data$browser == "opera" & experiment_complete_data$type=="full"), ]$Joule_calculated, breaks = ax_opera, plot = FALSE) # Save first histogram data
hg_opera_noads <- hist(experiment_complete_data[which(experiment_complete_data$browser == "opera" & experiment_complete_data$type=="noads"), ]$Joule_calculated, breaks = ax_opera, plot = FALSE) # Save 2nd histogram data
hg_opera_noanalytics <- hist(experiment_complete_data[which(experiment_complete_data$browser == "opera" & experiment_complete_data$type=="noanalytics"), ]$Joule_calculated, breaks = ax_opera, plot = FALSE) # Save 2nd histogram data

pdf(file='./plots/joule_full_histogram_original.pdf', width = 7, height = 7)
plot(hg_chrome_full, col = c1, main="Energy consumption distribution", xlab="Energy consumption (Joule)", ylab="Frequency") 
plot(hg_opera_full, col = c3, add = TRUE)
legend("topright", c("Chrome - Ads and analytics", "Opera - Ads and analytics"), fill=c(c1,c3))
dev.off()

pdf(file='./plots/joule_noads_histogram_original.pdf', width = 7, height = 7)
plot(hg_chrome_noads, col = c1, main="Energy consumption distribution", xlab="Energy consumption (Joule)", ylab="Frequency") 
plot(hg_opera_noads, col = c3, add = TRUE)
legend("topright", c("Chrome - Analytics", "Opera - Analytics"), fill=c(c1,c3))
dev.off()

pdf(file='./plots/joule_noanalytics_histogram_original.pdf', width = 7, height = 7)
plot(hg_chrome_noanalytics, col = c1, main="Energy consumption distribution", xlab="Energy consumption (Joule)", ylab="Frequency") 
plot(hg_opera_noanalytics, col = c3, add = TRUE)
legend("topright", c("Chrome - Ads", "Opera - Ads"), fill=c(c1,c3))
dev.off()


#=====After normalization=========
# Set the minimum for the breakpoints
breakpoint_chrome_min <- min(c(experiment_complete_data[which(experiment_complete_data$browser == "chrome" & experiment_complete_data$type=="full"), ]$joule_normalized,
                               experiment_complete_data[which(experiment_complete_data$browser == "chrome" & experiment_complete_data$type=="noads"), ]$joule_normalized,
                               experiment_complete_data[which(experiment_complete_data$browser == "chrome" & experiment_complete_data$type=="noanalytics"), ]$joule_normalized))# Set the minimum for the breakpoints
breakpoint_chrome_max <- max(c(experiment_complete_data[which(experiment_complete_data$browser == "chrome" & experiment_complete_data$type=="full"), ]$joule_normalized,
                               experiment_complete_data[which(experiment_complete_data$browser == "chrome" & experiment_complete_data$type=="noads"), ]$joule_normalized,
                               experiment_complete_data[which(experiment_complete_data$browser == "chrome" & experiment_complete_data$type=="noanalytics"), ]$joule_normalized)) # Set the maximum for the breakpoints

breakpoint_opera_min <- min(c(experiment_complete_data[which(experiment_complete_data$browser == "opera" & experiment_complete_data$type=="full"), ]$joule_normalized,
                              experiment_complete_data[which(experiment_complete_data$browser == "opera" & experiment_complete_data$type=="noads"), ]$joule_normalized,
                              experiment_complete_data[which(experiment_complete_data$browser == "opera" & experiment_complete_data$type=="noanalytics"), ]$joule_normalized))# Set the minimum for the breakpoints
breakpoint_opera_max <- max(c(experiment_complete_data[which(experiment_complete_data$browser == "opera" & experiment_complete_data$type=="full"), ]$joule_normalized,
                              experiment_complete_data[which(experiment_complete_data$browser == "opera" & experiment_complete_data$type=="noads"), ]$joule_normalized,
                              experiment_complete_data[which(experiment_complete_data$browser == "opera" & experiment_complete_data$type=="noanalytics"), ]$joule_normalized)) # Set the maximum for the breakpoints

hg_chrome_full <- hist(experiment_complete_data[which(experiment_complete_data$browser == "chrome" & experiment_complete_data$type=="full"), ]$joule_normalized, plot = FALSE) # Save first histogram data
hg_chrome_noads <- hist(experiment_complete_data[which(experiment_complete_data$browser == "chrome" & experiment_complete_data$type=="noads"), ]$joule_normalized, plot = FALSE) # Save 2nd histogram data
hg_chrome_noanalytics <- hist(experiment_complete_data[which(experiment_complete_data$browser == "chrome" & experiment_complete_data$type=="noanalytics"), ]$joule_normalized, plot = FALSE) # Save 2nd histogram data

hg_opera_full <- hist(experiment_complete_data[which(experiment_complete_data$browser == "opera" & experiment_complete_data$type=="full"), ]$joule_normalized, plot = FALSE) # Save first histogram data
hg_opera_noads <- hist(experiment_complete_data[which(experiment_complete_data$browser == "opera" & experiment_complete_data$type=="noads"), ]$joule_normalized, plot = FALSE) # Save 2nd histogram data
hg_opera_noanalytics <- hist(experiment_complete_data[which(experiment_complete_data$browser == "opera" & experiment_complete_data$type=="noanalytics"), ]$joule_normalized, plot = FALSE) # Save 2nd histogram data

pdf(file='./plots/joule_full_histogram_normalized.pdf', width = 7, height = 7)
plot(hg_chrome_full, col = c1, main="Energy consumption distribution", xlab="Energy consumption", ylab="Frequency") 
plot(hg_opera_full, col = c3, add = TRUE)
legend("topleft", c("Chrome - Ads and analytics", "Opera - Ads and analytics"), fill=c(c1,c3))
dev.off()

pdf(file='./plots/joule_noads_histogram_normalized.pdf', width = 7, height = 7)
plot(hg_chrome_noads, col = c1, main="Energy consumption distribution", xlab="Energy consumption", ylab="Frequency") 
plot(hg_opera_noads, col = c3, add = TRUE)
legend("topright", c("Chrome - Analytics", "Opera - Analytics"), fill=c(c1,c3))
dev.off()

pdf(file='./plots/joule_noanalytics_histogram_normalized.pdf', width = 7, height = 7)
plot(hg_chrome_noanalytics, col = c1, main="Energy consumption distribution", xlab="Energy consumption", ylab="Frequency") 
plot(hg_opera_noanalytics, col = c3, add = TRUE)
legend("topright", c("Chrome - Ads", "Opera - Ads"), fill=c(c1,c3))
dev.off()


#======Performance============
breakpoint_chrome_min <- min(c(experiment_complete_data[which(experiment_complete_data$browser == "chrome" & experiment_complete_data$type=="full"), ]$fcp,
                               experiment_complete_data[which(experiment_complete_data$browser == "chrome" & experiment_complete_data$type=="noads"), ]$fcp,
                               experiment_complete_data[which(experiment_complete_data$browser == "chrome" & experiment_complete_data$type=="noanalytics"), ]$fcp))# Set the minimum for the breakpoints
breakpoint_chrome_max <- max(c(experiment_complete_data[which(experiment_complete_data$browser == "chrome" & experiment_complete_data$type=="full"), ]$fcp,
                               experiment_complete_data[which(experiment_complete_data$browser == "chrome" & experiment_complete_data$type=="noads"), ]$fcp,
                               experiment_complete_data[which(experiment_complete_data$browser == "chrome" & experiment_complete_data$type=="noanalytics"), ]$fcp)) # Set the maximum for the breakpoints

breakpoint_opera_min <- min(c(experiment_complete_data[which(experiment_complete_data$browser == "opera" & experiment_complete_data$type=="full"), ]$fcp,
                              experiment_complete_data[which(experiment_complete_data$browser == "opera" & experiment_complete_data$type=="noads"), ]$fcp,
                              experiment_complete_data[which(experiment_complete_data$browser == "opera" & experiment_complete_data$type=="noanalytics"), ]$fcp))# Set the minimum for the breakpoints
breakpoint_opera_max <- max(c(experiment_complete_data[which(experiment_complete_data$browser == "opera" & experiment_complete_data$type=="full"), ]$fcp,
                              experiment_complete_data[which(experiment_complete_data$browser == "opera" & experiment_complete_data$type=="noads"), ]$fcp,
                              experiment_complete_data[which(experiment_complete_data$browser == "opera" & experiment_complete_data$type=="noanalytics"), ]$fcp)) # Set the maximum for the breakpoints


ax_chrome <- pretty(breakpoint_chrome_min:breakpoint_chrome_max, n = 50) # Make a neat vector for the breakpoints
ax_opera <- pretty(breakpoint_opera_min:breakpoint_opera_max, n = 50) # Make a neat vector for the breakpoints

hg_chrome_full <- hist(experiment_complete_data[which(experiment_complete_data$browser == "chrome" & experiment_complete_data$type=="full"), ]$fcp, breaks = ax_chrome, plot = FALSE) # Save first histogram data
hg_chrome_noads <- hist(experiment_complete_data[which(experiment_complete_data$browser == "chrome" & experiment_complete_data$type=="noads"), ]$fcp, breaks = ax_chrome, plot = FALSE) # Save 2nd histogram data
hg_chrome_noanalytics <- hist(experiment_complete_data[which(experiment_complete_data$browser == "chrome" & experiment_complete_data$type=="noanalytics"), ]$fcp, breaks = ax_chrome, plot = FALSE) # Save 2nd histogram data

hg_opera_full <- hist(experiment_complete_data[which(experiment_complete_data$browser == "opera" & experiment_complete_data$type=="full"), ]$fcp, breaks = ax_opera, plot = FALSE) # Save first histogram data
hg_opera_noads <- hist(experiment_complete_data[which(experiment_complete_data$browser == "opera" & experiment_complete_data$type=="noads"), ]$fcp, breaks = ax_opera, plot = FALSE) # Save 2nd histogram data
hg_opera_noanalytics <- hist(experiment_complete_data[which(experiment_complete_data$browser == "opera" & experiment_complete_data$type=="noanalytics"), ]$fcp, breaks = ax_opera, plot = FALSE) # Save 2nd histogram data

pdf(file= './plots/fcp_full_histogram_original.pdf', width = 7, height = 7)
plot(hg_chrome_full, col = c1, main="First Contentful Paint distribution", xlab="First Contentful Paint (ms)", ylab="Frequency") 
plot(hg_opera_full, col = c3, add = TRUE)
legend("topright", c("Chrome - Ads and analytics", "Opera - Ads and analytics"), fill=c(c1,c3))
dev.off()

pdf(file='./plots/fcp_noads_histogram_original.pdf', width = 7, height = 7,)
plot(hg_chrome_noads, col = c1, main="First Contentful Paint distribution", xlab="First Contentful Paint (ms)", ylab="Frequency") 
plot(hg_opera_noads, col = c3, add = TRUE)
legend("topright", c("Chrome - Analytics", "Opera - Analytics"), fill=c(c1,c3))
dev.off()

pdf(file='./plots/fcp_noanalytics_histogram_original.pdf', width = 7, height = 7)
plot(hg_chrome_noanalytics, col = c1, main="First Contentful Paint distribution", xlab="First Contentful Paint (ms)", ylab="Frequency") 
plot(hg_opera_noanalytics, col = c3, add = TRUE)
legend("topright", c("Chrome - Ads", "Opera - Ads"), fill=c(c1,c3))
dev.off()


#=====After normalization=========

breakpoint_chrome_min <- min(c(experiment_complete_data[which(experiment_complete_data$browser == "chrome" & experiment_complete_data$type=="full"), ]$fcp_normalized,
                               experiment_complete_data[which(experiment_complete_data$browser == "chrome" & experiment_complete_data$type=="noads"), ]$fcp_normalized,
                               experiment_complete_data[which(experiment_complete_data$browser == "chrome" & experiment_complete_data$type=="noanalytics"), ]$fcp_normalized))# Set the minimum for the breakpoints
breakpoint_chrome_max <- max(c(experiment_complete_data[which(experiment_complete_data$browser == "chrome" & experiment_complete_data$type=="full"), ]$fcp_normalized,
                               experiment_complete_data[which(experiment_complete_data$browser == "chrome" & experiment_complete_data$type=="noads"), ]$fcp_normalized,
                               experiment_complete_data[which(experiment_complete_data$browser == "chrome" & experiment_complete_data$type=="noanalytics"), ]$fcp_normalized)) # Set the maximum for the breakpoints

breakpoint_opera_min <- min(c(experiment_complete_data[which(experiment_complete_data$browser == "opera" & experiment_complete_data$type=="full"), ]$fcp_normalized,
                              experiment_complete_data[which(experiment_complete_data$browser == "opera" & experiment_complete_data$type=="noads"), ]$fcp_normalized,
                              experiment_complete_data[which(experiment_complete_data$browser == "opera" & experiment_complete_data$type=="noanalytics"), ]$fcp_normalized))# Set the minimum for the breakpoints
breakpoint_opera_max <- max(c(experiment_complete_data[which(experiment_complete_data$browser == "opera" & experiment_complete_data$type=="full"), ]$fcp_normalized,
                              experiment_complete_data[which(experiment_complete_data$browser == "opera" & experiment_complete_data$type=="noads"), ]$fcp_normalized,
                              experiment_complete_data[which(experiment_complete_data$browser == "opera" & experiment_complete_data$type=="noanalytics"), ]$fcp_normalized)) # Set the maximum for the breakpoints

hg_chrome_full <- hist(experiment_complete_data[which(experiment_complete_data$browser == "chrome" & experiment_complete_data$type=="full"), ]$fcp_normalized, plot = FALSE) # Save first histogram data
hg_chrome_noads <- hist(experiment_complete_data[which(experiment_complete_data$browser == "chrome" & experiment_complete_data$type=="noads"), ]$fcp_normalized, plot = FALSE) # Save 2nd histogram data
hg_chrome_noanalytics <- hist(experiment_complete_data[which(experiment_complete_data$browser == "chrome" & experiment_complete_data$type=="noanalytics"), ]$fcp_normalized, plot = FALSE) # Save 2nd histogram data

hg_opera_full <- hist(experiment_complete_data[which(experiment_complete_data$browser == "opera" & experiment_complete_data$type=="full"), ]$fcp_normalized, plot = FALSE) # Save first histogram data
hg_opera_noads <- hist(experiment_complete_data[which(experiment_complete_data$browser == "opera" & experiment_complete_data$type=="noads"), ]$fcp_normalized, plot = FALSE) # Save 2nd histogram data
hg_opera_noanalytics <- hist(experiment_complete_data[which(experiment_complete_data$browser == "opera" & experiment_complete_data$type=="noanalytics"), ]$fcp_normalized, plot = FALSE) # Save 2nd histogram data

pdf(file='./plots/fcp_full_histogram_normalized.pdf', width = 7, height = 7)
plot(hg_chrome_full, col = c1, main="Normalized First Contentful paint distribution", xlab="Normalized First Contentful paint", ylab="Frequency") 
plot(hg_opera_full, col = c3, add = TRUE)
legend("topright", c("Chrome - Ads and analytics", "Opera - Ads and analytics"), fill=c(c1,c3))
dev.off()

pdf(file='./plots/fcp_noads_histogram_normalized.pdf', width = 7, height = 7)
plot(hg_chrome_noads, col = c1, main="Normalized First Contentful paint distribution", xlab="Normalized First Contentful paint", ylab="Frequency") 
plot(hg_opera_noads, col = c3, add = TRUE)
legend("topright", c("Chrome - Analytics", "Opera - Analytics"), fill=c(c1,c3))
dev.off()

pdf(file='./plots/fcp_noanalytics_histogram_normalized.pdf', width = 7, height = 7)
plot(hg_chrome_noanalytics, col = c1, main="Normalized First Contentful paint distribution", xlab="Normalized First Contentful paint", ylab="Frequency") 
plot(hg_opera_noanalytics, col = c3, add = TRUE)
legend("topright", c("Chrome - Ads", "Opera - Ads"), fill=c(c1,c3))
dev.off()

#=========================================================
#==================Hypothesis Testing=====================
#=========================================================


# One-sided t-test assumes the following property:  No significant outliers in the data. %Normality%. The data should be approximately normally distributed.

#===========================Perform Wilcox ranksum test since it does not assume normality===========================

#======Energy Consumption=========
wilcox.test(x=experiment_complete_data[which(experiment_complete_data$browser == "chrome" &experiment_complete_data$type=="full"), ]$joule_normalized, 
            y=experiment_complete_data[which(experiment_complete_data$browser == "chrome" &experiment_complete_data$type=="noads"), ]$joule_normalized,)  

# H0 : 𝜇𝑗,𝑎𝑑𝑠 = 𝜇𝑗,𝑛𝑜_𝑎𝑑𝑠 , 𝑗 ∈ {𝐺𝐶, 𝑂𝑃 }
# H1 : 𝜇𝑗,𝑎𝑑𝑠 > 𝜇𝑗,𝑛𝑜_𝑎𝑑𝑠 , 𝑗 ∈ {𝐺𝐶, 𝑂𝑃 }

wilcox.test(x=experiment_complete_data[which(experiment_complete_data$browser == "chrome" &experiment_complete_data$type=="full"), ]$joule_normalized, 
            y=experiment_complete_data[which(experiment_complete_data$browser == "chrome" &experiment_complete_data$type=="noanalytics"), ]$joule_normalized,) 

# H0 : 𝜇𝑗,𝑎𝑑𝑠 = 𝜇𝑗,𝑛𝑜_𝑎𝑑𝑠 , 𝑗 ∈ {𝐺𝐶, 𝑂𝑃 }
# H1 : 𝜇𝑗,𝑎𝑑𝑠 > 𝜇𝑗,𝑛𝑜_𝑎𝑑𝑠 , 𝑗 ∈ {𝐺𝐶, 𝑂𝑃 }

wilcox.test(x=experiment_complete_data[which(experiment_complete_data$browser == "opera" &experiment_complete_data$type=="full"), ]$joule_normalized, 
            y=experiment_complete_data[which(experiment_complete_data$browser == "opera" &experiment_complete_data$type=="noads"), ]$joule_normalized,)

# H0 : 𝜇𝑗,𝑎𝑑𝑠 = 𝜇𝑗,𝑛𝑜_𝑎𝑑𝑠 , 𝑗 ∈ {𝐺𝐶, 𝑂𝑃 }
# H1 : 𝜇𝑗,𝑎𝑑𝑠 > 𝜇𝑗,𝑛𝑜_𝑎𝑑𝑠 , 𝑗 ∈ {𝐺𝐶, 𝑂𝑃 }

wilcox.test(x=experiment_complete_data[which(experiment_complete_data$browser == "opera" &experiment_complete_data$type=="full"), ]$joule_normalized, 
            y=experiment_complete_data[which(experiment_complete_data$browser == "opera" &experiment_complete_data$type=="noanalytics"), ]$joule_normalized, ) 

# H0 : 𝜇𝑗,𝑎𝑑𝑠 = 𝜇𝑗,𝑛𝑜_𝑎𝑑𝑠 , 𝑗 ∈ {𝐺𝐶, 𝑂𝑃 }
# H1 : 𝜇𝑗,𝑎𝑑𝑠 > 𝜇𝑗,𝑛𝑜_𝑎𝑑𝑠 , 𝑗 ∈ {𝐺𝐶, 𝑂𝑃 }

#=============Performance=========
#===FCP===
wilcox.test(x=experiment_complete_data[which(experiment_complete_data$browser == "chrome" &experiment_complete_data$type=="full"), ]$fcp_normalized, 
            y=experiment_complete_data[which(experiment_complete_data$browser == "chrome" &experiment_complete_data$type=="noads"), ]$fcp_normalized,)

# H0 : 𝛽𝑖,𝑗,𝑎𝑛𝑎𝑙𝑦𝑡𝑖𝑐𝑠 = 𝛽𝑖,𝑗,𝑛𝑜_𝑎𝑛𝑎𝑙𝑦𝑡𝑖𝑐𝑠 , 𝑖 ∈ {𝐹𝐶𝑃, 𝐹 𝑃𝐿𝑇 ,𝑇 𝐵𝑇 }, 𝑗 ∈ {𝐺𝐶, 𝑂𝑃 }
# H1 : 𝛽𝑖,𝑗,𝑎𝑛𝑎𝑙𝑦𝑡𝑖𝑐𝑠 > 𝛽𝑖,𝑗,𝑛𝑜_𝑎𝑛𝑎𝑙𝑦𝑡𝑖𝑐𝑠 , 𝑖 ∈ {𝐹𝐶𝑃, 𝐹 𝑃𝐿𝑇 ,𝑇 𝐵𝑇 }, 𝑗 ∈ {𝐺𝐶, 𝑂𝑃 }

wilcox.test(x=experiment_complete_data[which(experiment_complete_data$browser == "chrome" &experiment_complete_data$type=="full"), ]$fcp_normalized, 
            y=experiment_complete_data[which(experiment_complete_data$browser == "chrome" &experiment_complete_data$type=="noanalytics"), ]$fcp_normalized, )

# H0 : 𝛽𝑖,𝑗,𝑎𝑛𝑎𝑙𝑦𝑡𝑖𝑐𝑠 = 𝛽𝑖,𝑗,𝑛𝑜_𝑎𝑛𝑎𝑙𝑦𝑡𝑖𝑐𝑠 , 𝑖 ∈ {𝐹𝐶𝑃, 𝐹 𝑃𝐿𝑇 ,𝑇 𝐵𝑇 }, 𝑗 ∈ {𝐺𝐶, 𝑂𝑃 }
# H1 : 𝛽𝑖,𝑗,𝑎𝑛𝑎𝑙𝑦𝑡𝑖𝑐𝑠 > 𝛽𝑖,𝑗,𝑛𝑜_𝑎𝑛𝑎𝑙𝑦𝑡𝑖𝑐𝑠 , 𝑖 ∈ {𝐹𝐶𝑃, 𝐹 𝑃𝐿𝑇 ,𝑇 𝐵𝑇 }, 𝑗 ∈ {𝐺𝐶, 𝑂𝑃 }

wilcox.test(x=experiment_complete_data[which(experiment_complete_data$browser == "opera" &experiment_complete_data$type=="full"), ]$fcp_normalized, 
            y=experiment_complete_data[which(experiment_complete_data$browser == "opera" &experiment_complete_data$type=="noads"), ]$fcp_normalized, )


# H0 : 𝛽𝑖,𝑗,𝑎𝑛𝑎𝑙𝑦𝑡𝑖𝑐𝑠 = 𝛽𝑖,𝑗,𝑛𝑜_𝑎𝑛𝑎𝑙𝑦𝑡𝑖𝑐𝑠 , 𝑖 ∈ {𝐹𝐶𝑃, 𝐹 𝑃𝐿𝑇 ,𝑇 𝐵𝑇 }, 𝑗 ∈ {𝐺𝐶, 𝑂𝑃 }
# H1 : 𝛽𝑖,𝑗,𝑎𝑛𝑎𝑙𝑦𝑡𝑖𝑐𝑠 > 𝛽𝑖,𝑗,𝑛𝑜_𝑎𝑛𝑎𝑙𝑦𝑡𝑖𝑐𝑠 , 𝑖 ∈ {𝐹𝐶𝑃, 𝐹 𝑃𝐿𝑇 ,𝑇 𝐵𝑇 }, 𝑗 ∈ {𝐺𝐶, 𝑂𝑃 }

wilcox.test(x=experiment_complete_data[which(experiment_complete_data$browser == "opera" &experiment_complete_data$type=="full"), ]$fcp_normalized, 
            y=experiment_complete_data[which(experiment_complete_data$browser == "opera" &experiment_complete_data$type=="noanalytics"), ]$fcp_normalized,)

# H0 : 𝛽𝑖,𝑗,𝑎𝑛𝑎𝑙𝑦𝑡𝑖𝑐𝑠 = 𝛽𝑖,𝑗,𝑛𝑜_𝑎𝑛𝑎𝑙𝑦𝑡𝑖𝑐𝑠 , 𝑖 ∈ {𝐹𝐶𝑃, 𝐹 𝑃𝐿𝑇 ,𝑇 𝐵𝑇 }, 𝑗 ∈ {𝐺𝐶, 𝑂𝑃 }
# H1 : 𝛽𝑖,𝑗,𝑎𝑛𝑎𝑙𝑦𝑡𝑖𝑐𝑠 > 𝛽𝑖,𝑗,𝑛𝑜_𝑎𝑛𝑎𝑙𝑦𝑡𝑖𝑐𝑠 , 𝑖 ∈ {𝐹𝐶𝑃, 𝐹 𝑃𝐿𝑇 ,𝑇 𝐵𝑇 }, 𝑗 ∈ {𝐺𝐶, 𝑂𝑃 }

#===FPLT===
wilcox.test(x=experiment_complete_data[which(experiment_complete_data$browser == "chrome" &experiment_complete_data$type=="full"), ]$payload_normalized, 
            y=experiment_complete_data[which(experiment_complete_data$browser == "chrome" &experiment_complete_data$type=="noads"), ]$payload_normalized,)

# H0 : 𝛽𝑖,𝑗,𝑎𝑛𝑎𝑙𝑦𝑡𝑖𝑐𝑠 = 𝛽𝑖,𝑗,𝑛𝑜_𝑎𝑛𝑎𝑙𝑦𝑡𝑖𝑐𝑠 , 𝑖 ∈ {𝐹𝐶𝑃, 𝐹 𝑃𝐿𝑇 ,𝑇 𝐵𝑇 }, 𝑗 ∈ {𝐺𝐶, 𝑂𝑃 }
# H1 : 𝛽𝑖,𝑗,𝑎𝑛𝑎𝑙𝑦𝑡𝑖𝑐𝑠 > 𝛽𝑖,𝑗,𝑛𝑜_𝑎𝑛𝑎𝑙𝑦𝑡𝑖𝑐𝑠 , 𝑖 ∈ {𝐹𝐶𝑃, 𝐹 𝑃𝐿𝑇 ,𝑇 𝐵𝑇 }, 𝑗 ∈ {𝐺𝐶, 𝑂𝑃 }

wilcox.test(x=experiment_complete_data[which(experiment_complete_data$browser == "chrome" &experiment_complete_data$type=="full"), ]$payload_normalized, 
            y=experiment_complete_data[which(experiment_complete_data$browser == "chrome" &experiment_complete_data$type=="noanalytics"), ]$payload_normalized,)

# H0 : 𝛽𝑖,𝑗,𝑎𝑛𝑎𝑙𝑦𝑡𝑖𝑐𝑠 = 𝛽𝑖,𝑗,𝑛𝑜_𝑎𝑛𝑎𝑙𝑦𝑡𝑖𝑐𝑠 , 𝑖 ∈ {𝐹𝐶𝑃, 𝐹 𝑃𝐿𝑇 ,𝑇 𝐵𝑇 }, 𝑗 ∈ {𝐺𝐶, 𝑂𝑃 }
# H1 : 𝛽𝑖,𝑗,𝑎𝑛𝑎𝑙𝑦𝑡𝑖𝑐𝑠 > 𝛽𝑖,𝑗,𝑛𝑜_𝑎𝑛𝑎𝑙𝑦𝑡𝑖𝑐𝑠 , 𝑖 ∈ {𝐹𝐶𝑃, 𝐹 𝑃𝐿𝑇 ,𝑇 𝐵𝑇 }, 𝑗 ∈ {𝐺𝐶, 𝑂𝑃 }

wilcox.test(x=experiment_complete_data[which(experiment_complete_data$browser == "opera" &experiment_complete_data$type=="full"), ]$payload_normalized, 
            y=experiment_complete_data[which(experiment_complete_data$browser == "opera" &experiment_complete_data$type=="noads"), ]$payload_normalized,)
# H0 : 𝛽𝑖,𝑗,𝑎𝑛𝑎𝑙𝑦𝑡𝑖𝑐𝑠 = 𝛽𝑖,𝑗,𝑛𝑜_𝑎𝑛𝑎𝑙𝑦𝑡𝑖𝑐𝑠 , 𝑖 ∈ {𝐹𝐶𝑃, 𝐹 𝑃𝐿𝑇 ,𝑇 𝐵𝑇 }, 𝑗 ∈ {𝐺𝐶, 𝑂𝑃 }
# H1 : 𝛽𝑖,𝑗,𝑎𝑛𝑎𝑙𝑦𝑡𝑖𝑐𝑠 > 𝛽𝑖,𝑗,𝑛𝑜_𝑎𝑛𝑎𝑙𝑦𝑡𝑖𝑐𝑠 , 𝑖 ∈ {𝐹𝐶𝑃, 𝐹 𝑃𝐿𝑇 ,𝑇 𝐵𝑇 }, 𝑗 ∈ {𝐺𝐶, 𝑂𝑃 }

wilcox.test(x=experiment_complete_data[which(experiment_complete_data$browser == "opera" &experiment_complete_data$type=="full"), ]$payload_normalized, 
            y=experiment_complete_data[which(experiment_complete_data$browser == "opera" &experiment_complete_data$type=="noanalytics"), ]$payload_normalized,)

# H0 : 𝛽𝑖,𝑗,𝑎𝑛𝑎𝑙𝑦𝑡𝑖𝑐𝑠 = 𝛽𝑖,𝑗,𝑛𝑜_𝑎𝑛𝑎𝑙𝑦𝑡𝑖𝑐𝑠 , 𝑖 ∈ {𝐹𝐶𝑃, 𝐹 𝑃𝐿𝑇 ,𝑇 𝐵𝑇 }, 𝑗 ∈ {𝐺𝐶, 𝑂𝑃 }
# H1 : 𝛽𝑖,𝑗,𝑎𝑛𝑎𝑙𝑦𝑡𝑖𝑐𝑠 > 𝛽𝑖,𝑗,𝑛𝑜_𝑎𝑛𝑎𝑙𝑦𝑡𝑖𝑐𝑠 , 𝑖 ∈ {𝐹𝐶𝑃, 𝐹 𝑃𝐿𝑇 ,𝑇 𝐵𝑇 }, 𝑗 ∈ {𝐺𝐶, 𝑂𝑃 }

#Size of effect testing with Cliff delta

#Chrome and control(full) vs treatment(noads/noanalytics)
#Energy Consumption
chrome_ec_full_vs_noads_res <- cliff.delta(experiment_complete_data[which(experiment_complete_data$browser == "chrome" &experiment_complete_data$type=="full"), ]$joule_normalized,
                                           experiment_complete_data[which(experiment_complete_data$browser == "chrome" &experiment_complete_data$type=="noads"), ]$joule_normalized, return.dm=TRUE)

chrome_ec_full_vs_noanalytics_res <- cliff.delta(experiment_complete_data[which(experiment_complete_data$browser == "chrome" &experiment_complete_data$type=="full"), ]$joule_normalized,
                                                 experiment_complete_data[which(experiment_complete_data$browser == "chrome" &experiment_complete_data$type=="noanalytics"), ]$joule_normalized, return.dm=TRUE)
#FCP
chrome_fcp_full_vs_noads_res <- cliff.delta(experiment_complete_data[which(experiment_complete_data$browser == "chrome" &experiment_complete_data$type=="full"), ]$fcp_normalized,
                                            experiment_complete_data[which(experiment_complete_data$browser == "chrome" &experiment_complete_data$type=="noads"), ]$fcp_normalized, return.dm=TRUE)
rchrome_fcp_full_vs_noanalytics_res <- cliff.delta(experiment_complete_data[which(experiment_complete_data$browser == "chrome" &experiment_complete_data$type=="full"), ]$fcp_normalized,
                                                   experiment_complete_data[which(experiment_complete_data$browser == "chrome" &experiment_complete_data$type=="noanalytics"), ]$fcp_normalized, return.dm=TRUE)
#FPLT
chrome_fplt_full_vs_noads_res <- cliff.delta(experiment_complete_data[which(experiment_complete_data$browser == "chrome" &experiment_complete_data$type=="full"), ]$payload_normalized,
                                             experiment_complete_data[which(experiment_complete_data$browser == "chrome" &experiment_complete_data$type=="noads"), ]$payload_normalized, return.dm=TRUE)
chrome_fplt_full_vs_noanalytics_res <- cliff.delta(experiment_complete_data[which(experiment_complete_data$browser == "chrome" &experiment_complete_data$type=="full"), ]$payload_normalized,
                                                   experiment_complete_data[which(experiment_complete_data$browser == "chrome" &experiment_complete_data$type=="noanalytics"), ]$payload_normalized, return.dm=TRUE)
#Opera and control(full) vs treatment(noads/noanalytics)
#Energy Consumption
opera_ec_full_vs_noads_res <- cliff.delta(experiment_complete_data[which(experiment_complete_data$browser == "opera" &experiment_complete_data$type=="full"), ]$joule_normalized,
                                          experiment_complete_data[which(experiment_complete_data$browser == "opera" &experiment_complete_data$type=="noads"), ]$joule_normalized, return.dm=TRUE)
opera_ec_full_vs_noanalytics_res <- cliff.delta(experiment_complete_data[which(experiment_complete_data$browser == "opera" &experiment_complete_data$type=="full"), ]$joule_normalized,
                                                experiment_complete_data[which(experiment_complete_data$browser == "opera" &experiment_complete_data$type=="noanalytics"), ]$joule_normalized, return.dm=TRUE)
#FCP
opera_fcp_full_vs_noads_res <- cliff.delta(experiment_complete_data[which(experiment_complete_data$browser == "opera" &experiment_complete_data$type=="full"), ]$fcp_normalized,
                                           experiment_complete_data[which(experiment_complete_data$browser == "opera" &experiment_complete_data$type=="noads"), ]$fcp_normalized, return.dm=TRUE)
opera_fcp_full_vs_noanalytics_res <- cliff.delta(experiment_complete_data[which(experiment_complete_data$browser == "opera" &experiment_complete_data$type=="full"), ]$fcp_normalized,
                                                 experiment_complete_data[which(experiment_complete_data$browser == "opera" &experiment_complete_data$type=="noanalytics"), ]$fcp_normalized, return.dm=TRUE)
#FPLT
opera_fplt_full_vs_noads_res <- cliff.delta(experiment_complete_data[which(experiment_complete_data$browser == "opera" &experiment_complete_data$type=="full"), ]$payload_normalized,
                                            experiment_complete_data[which(experiment_complete_data$browser == "opera" &experiment_complete_data$type=="noads"), ]$payload_normalized, return.dm=TRUE)
opera_fplt_full_vs_noanalytics_res <- cliff.delta(experiment_complete_data[which(experiment_complete_data$browser == "opera" &experiment_complete_data$type=="full"), ]$payload_normalized,
                                                  experiment_complete_data[which(experiment_complete_data$browser == "opera" &experiment_complete_data$type=="noanalytics"), ]$payload_normalized, return.dm=TRUE)