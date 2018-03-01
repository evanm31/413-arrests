#EDA of crime data

library(tidyverse)
library(readxl)
setwd("~/drugarrests/data")

holyoke <- read.csv("holyoke_arrest.csv", skip = 11)
colnames(holyoke) <- as.character(unlist(holyoke[1,]))
holyoke <- holyoke[-1, ]
holyoke <- holyoke[-(c(35:45)), ]
holyoke <- holyoke %>% gather(Age, Count, 2:22)

aggs <- c("Total all ages", "Ages 18 or over", "Ages under 18", "Ages under 15", "Ages under 10")
holyoke_drug <- holyoke[19:21,]
holyoke_drug <- holyoke_drug %>% 
  gather(Age, Count, 2:22) %>% 
  mutate(Count = as.numeric(Count), aggregate = ifelse(Age %in% aggs, 1, 0))

#barplot of drug arrests faceted by type
holyoke_drug %>% 
filter(aggregate == 0 & Offense != "Drug Abuse Violations -Total") %>%
ggplot(aes(x = Age, y = Count)) + 
  geom_bar(stat="identity") + 
  geom_line(group= 1, size = 1) + 
  facet_grid(Offense~.)

#dependence between age and drug arrests 
tbl = table(factor(holyoke_drug$Age), holyoke_drug$Count)
chisq.test(table)$p.value

count_vals <- c("~","- 1", "-1")
holyoke_total <- holyoke %>% filter(!(Count %in% count_vals)) %>% filter(Age == "Total all ages") %>% mutate(Count = as.numeric(Count))
#percent of reported crimes that are drug related
holyoke_total[14,3] / holyoke_total[1,3]

#data involving race
holyoke_race <- read.csv("holyoke_arrest_race.csv", skip = 11)

colnames(holyoke_race)[2:6] = paste0("All Ages", " ", as.character(unlist(holyoke_race[2,2:6])))
colnames(holyoke_race)[7:11] = paste0("Ages under 18", " ", as.character(unlist(holyoke_race[2,2:6])))
colnames(holyoke_race)[12:16] = paste0("Ages over 18", " ", as.character(unlist(holyoke_race[2,2:6])))
colnames(holyoke_race)[1] <- 'Offense'

holyoke_race <- holyoke_race[-(c(1:2, 37:48)), ]
drug <- c("Drug Abuse Violations -Total", "Sale-Manufacturing-Total", "Possession-SubTotal")
holyoke_race <- holyoke_race %>% filter(Offense %in% drug)

#convert counts to numeric
holyoke_race[,2:16]<- lapply(holyoke_race[,2:16], as.character)
holyoke_race[,2:16]<- lapply(holyoke_race[,2:16], as.integer)

holyoke_race <- holyoke_race %>% gather(Group, Count, 2:16) %>% 
  mutate(Age = ifelse(grepl('All Ages', Group), 1, 
  ifelse(grepl('Ages under 18', Group), 2, 3)))

#barplot of counts by race
#order by count? 
holyoke_race %>% 
  filter(Age == 1, !grepl("Total", Group), !grepl("Drug Abuse", Offense)) %>% 
  ggplot(aes(x = Group, y = Count)) + 
  geom_bar(stat="identity") + 
  facet_grid(Offense~.)

# #ineffective due to inbalanced classes
# tbl = table(factor(holyoke_race$Group), holyoke_race$Count)
# chisq.test(tbl)$p.value
