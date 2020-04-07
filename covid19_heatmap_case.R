
#heatmap plot for ontario daily cases by PHU
library(tidyverse)
library(ggplot2)
library(dplyr)
library(reshape2)

# Reference:
# COVID-19 Canada Open Data Working Group. Epidemiological Data from the COVID-19 Outbreak in Canada. 
# https://github.com/ishaberry/Covid19Canada. (Access Date).
# I use the working groups cases.csv data set for generating the ontario daily case heatmap;

# 1. Read spreadsheet;
can_case <- read.table("C:/Users/kuan liu/Dropbox (UT_KLiu)/covid19/data/Covid19Canada-master/cases.csv", 
                      header = TRUE, sep = ",")

# 2. Create daily aggregate cases by reporting PHU city after march 5th;
can_case$date_report<-as.Date(can_case$date_report, format="%d-%m-%Y")
on_case <- filter(can_case, province == "Ontario")  %>% group_by(date_report,health_region) %>% summarise(Freq=n())

# If you desire to plot after a specific date, for example before I only extract case data reported after March 5th, 2020, 
#you can uncomment and run the code line below;
# on_case <- filter(can_case, province == "Ontario" & date_report > "2020-03-05")  %>% group_by(date_report,health_region) %>% summarise(Freq=n())

# 3. checking the daily case count distribution;
summary(on_case$Freq)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.00    1.00    3.00   10.82   12.00  206.00 
#I decide to break daily case numbers in the following groups, 0, 1-9, 10-49, 50-99, >100 (5 levels)

# 4. create daily case groups, relabel in ggplot to 1-9, 10-49, 50-99 and 100+;
on_case <- on_case %>%
  mutate( cases = case_when(
    Freq >=1 & Freq <=9    ~ "1",
    Freq >=10 & Freq <=49  ~ "2",
    Freq >=50 & Freq <=99  ~ "3",
    Freq >=100 ~ "4"))

# 5.1 reorder health region by case frequency count, note that toronto should have the highest cumulative frequncy;
on_case_region<- filter(can_case, province == "Ontario" & date_report > "2020-03-05")  %>% group_by(health_region) %>% summarise(Freq=n())
region_order <- unlist(on_case_region[rev(order(on_case_region$Freq)),][,1]) #save new region order by total case frequency to a vector;

# 5.2 update original level to the new ordered regional level;
on_case$health_region<- factor(on_case$health_region, levels=region_order[1:dim(on_case_region)[1]])


# 6.plot;

png("covid19_oncase_heatmap.png", width = 8, height = 10, units = "in", res = 400)

ggplot(on_case, aes( health_region, as.factor(reorder(date_report, desc(date_report))))) +
  coord_equal()+
  scale_x_discrete(position = "top")+
  geom_tile(aes(fill = cases )) + 
  geom_text(aes(label = round(Freq, 1)), size=3) +
  scale_fill_brewer(palette = "YlOrRd",label=c("1-9","10-49","50-99",expression(phantom(x) >=100))) +
  labs(y = " ", x = "",fill="Daily cases",caption = "Heat Map of Ontario Daily Confirmed COVID-19 Cases by Region")+
  theme(panel.background = element_rect(fill = "white", colour = "black"),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(), 
      legend.position = "right",
      axis.text.x = element_text(angle = 90, hjust = 0),
      plot.margin = margin(0,0, 0, 0, "cm"), 
      plot.caption = element_text(hjust=1, size=rel(1.2)))

dev.off()
