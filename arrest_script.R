#script to read in, clean, and visualize police data by age data from bureau of justice statistics
#Evan Moore
#March 2018

library(dplyr)
library(ggplot2)
library(tidyr)

read_data_age <- function(filepath) {
  data <- read.csv(filepath, skip = 11)
  colnames(data) <- as.character(unlist(data[1,]))
  data <- data[-1, ]
  data <- data[-(c(35:45)), ]
}

create_drug_df <- function(data) {
  aggs <- c("Total all ages", "Ages 18 or over", "Ages under 18", "Ages under 15", "Ages under 10")
  data_drug <- data[19:21,]
  data_drug <- data_drug %>% 
    gather(Age, Count, 2:22) %>% 
    mutate(Count = as.numeric(Count), aggregate = ifelse(Age %in% aggs, 1, 0))
  return(data_drug)
}

plot_bar_age <- function(data, cityStr) {
  data %>% 
    dplyr::filter(aggregate == 0 & Offense != "Drug Abuse Violations -Total") %>%
    ggplot(aes(x = Age, y = Count)) + 
    geom_bar(stat="identity") + 
    geom_line(group= 1, size = 1) + 
    facet_grid(Offense~.) + 
    labs(title = paste0("2014 Drug Arrest Rates for ", cityStr, ", MA by Age Group"), caption = "Data from the Bureau of Justice Statistics website")
}

chisq_drug_age <- function(data) {
  tbl = table(factor(data$Age), data$Count)
  return(chisq.test(table))
}

drug_total <- function(data) {
  data <- data %>% gather(Age, Count, 2:22)
  count_vals <- c("~","- 1", "-1")
  data_total <- data %>% 
    filter(!(Count %in% count_vals)) %>% 
    filter(Age == "Total all ages") %>% 
    mutate(Count = as.numeric(Count))
  num <- data_total[which(data_total == "Drug Abuse Violations -Total"),3] / data_total[1,3]
  return(paste0("The percentage of drug-related crimes is ", round(num*100, 2), "%. This represents ", data_total[which(data_total == "Drug Abuse Violations -Total"),3], " arrests for drugs out of ", data_total[1,3], " total arrests."))
}

read_data_race <- function(filepath) {
  data <- read.csv(filepath, skip = 11)
  
  colnames(data)[2:6] = paste0("All Ages", " ", as.character(unlist(data[2,2:6])))
  colnames(data)[7:11] = paste0("Ages under 18", " ", as.character(unlist(data[2,2:6])))
  colnames(data)[12:16] = paste0("Ages over 18", " ", as.character(unlist(data[2,2:6])))
  colnames(data)[1] <- 'Offense'
  
  data <- data[-(c(1:2, 37:48)), ]
  drug <- c("Drug Abuse Violations -Total", "Sale-Manufacturing-Total", "Possession-SubTotal")
  data <- data %>% filter(Offense %in% drug)
  
  #convert counts to numeric
  data[,2:16]<- lapply(data[,2:16], as.character)
  data[,2:16]<- lapply(data[,2:16], as.integer)
  
  data <- data %>% gather(Group, Count, 2:16) %>% 
    mutate(Age = ifelse(grepl('All Ages', Group), 1, 
                        ifelse(grepl('Ages under 18', Group), 2, 3)))
  return(data)
}

plot_bar_race <- function(data, cityStr) {
  data %>% 
    filter(Age == 1, !grepl("Total", Group), !grepl("Drug Abuse", Offense)) %>% 
    ggplot(aes(x = Group, y = Count)) + 
    geom_bar(stat="identity") + 
    facet_grid(Offense~.) + 
    labs(title = paste0("2014 Drug Arrest Rates for ", cityStr, ", MA by Race"), caption = "Data from the Bureau of Justice Statistics website")
}
