
#pyramid plot ontario cases
library(tidyverse)
library(ggpol)
library(dplyr)

on_case <- read.table("https://data.ontario.ca/dataset/f4112442-bdc8-45d2-be3c-12efae72fb27/resource/455fd63b-603d-4608-8216-7d8647f43350/download/conposcovidloc.csv", 
                 header = TRUE,
                 sep = ",")

#keep data with only two sex groups and non-missing age;
# table(on_case$CLIENT_GENDER)
# table(on_case$Age_Group)


#regrouping age group, clapsing 80s and 90s;
on_case_clean<-on_case
# levels(on_case_clean$Age_Group)<-ifelse(levels(on_case_clean$Age_Group) %in% c("80s","90s"),">80",levels(on_case_clean$Age_Group))

on_case_clean$CLIENT_GENDER[on_case_clean$CLIENT_GENDER %in% c("UNKNOWN", "OTHER","TRANSGENDER")]<-NA
on_case_clean$Age_Group[on_case_clean$Age_Group == "Unknown"]<-NA


on_case_clean<-on_case_clean[complete.cases(on_case_clean),] 
on_case_resolve<-subset(on_case_clean, OUTCOME1 == "Resolved") 
on_case_death<-subset(on_case_clean, OUTCOME1 == "Fatal") 


# 1.Create Dataset;
case <- on_case_clean %>% group_by(Age_Group,CLIENT_GENDER) %>% summarise(Freq=n())
case_resolve<- on_case_resolve %>% group_by(Age_Group,CLIENT_GENDER) %>% summarise(Freq=n())
case_death<- on_case_death %>% group_by(Age_Group,CLIENT_GENDER) %>% summarise(Freq=n())

case$Freq[case$CLIENT_GENDER=="FEMALE"]<- (-case$Freq[case$CLIENT_GENDER=="FEMALE"])
case_resolve$Freq[case_resolve$CLIENT_GENDER=="FEMALE"]<- (-case_resolve$Freq[case_resolve$CLIENT_GENDER=="FEMALE"])
case_death$Freq[case_death$CLIENT_GENDER=="FEMALE"]<- (-case_death$Freq[case_death$CLIENT_GENDER=="FEMALE"])

names(case)<-c("Age group","Gender","Freqency count (n)")
names(case_resolve)<-c("Age group","Gender","Freqency count (n)")
names(case_death)<-c("Age group","Gender","Freqency count (n)")

dfcase <- tibble(
  Gender = case$Gender,
  Data =  case$`Freqency count (n)`,
  Agegp = case$`Age group`
)


dfresolve <- tibble(
  Gender = case_resolve$Gender,
  Data =  case_resolve$`Freqency count (n)`,
  Agegp = case_resolve$`Age group`
)

dfdeath <- tibble(
  Gender = case_death$Gender,
  Data =  case_death$`Freqency count (n)`,
  Agegp = case_death$`Age group`
)

png("covid19_demo_on_pyramid.png", width = 13, height = 9, units = "in", res = 300)

casep<-ggplot(dfcase, aes(x = forcats::fct_rev(Agegp), y = Data, fill = Gender)) +
  geom_bar(stat = "identity", position = position_dodge(width=-0.5), width = 0.8, colour="black") +
  geom_text(size = 3, stat = 'identity',
            aes(label=c(abs(Data)),
  hjust=c(rep(c(-0.4,1.4),9)))) +
  facet_share(~Gender, dir = "h", scales = "free", reverse_num = TRUE) +
  coord_flip() + 
  labs(y = paste("Frequency count (n)", " \n Total number of confirmed cases ",sum(abs(dfcase$Data))), x = "", title = paste("Demographic of Ontario COVID-19 confirm cases, as of", format(Sys.time(), "%b %d, %Y"), "\n (known age and gender in female and male)")) +
  scale_fill_manual(values = c("pink", "lightblue"))+
  theme(panel.background = element_rect(fill = "white", colour = "white"),
        panel.grid.major = element_line(colour = "grey95"),
        panel.grid.minor = element_line(colour = "grey95"), 
        legend.position = "none",
        axis.title = element_text(size=11),
        plot.margin = margin(1,1, 0, 1, "cm"))

resolvep<-ggplot(dfresolve, aes(x = forcats::fct_rev(Agegp), y = Data, fill = Gender)) +
  geom_bar(stat = "identity", position = position_dodge(width=-0.5), width = 0.8, colour="black") +
  geom_text(size = 3, stat = 'identity',
            aes(label=c(abs(Data)),
                hjust=c(rep(c(-0.4,1.4),8),-0.4,1))) +
  facet_share(~Gender, dir = "h", scales = "free", reverse_num = TRUE) +
  coord_flip() + 
  labs(y = paste("Frequency count (n)", " \n Total number of resolved cases ",sum(abs(dfresolve$Data))), x = "", title = "Demographic of Ontario COVID-19 resolved cases") +
  scale_fill_manual(values = c("pink", "lightblue"))+
  theme(panel.background = element_rect(fill = "white", colour = "white"),
        panel.grid.major = element_line(colour = "grey95"),
        panel.grid.minor = element_line(colour = "grey95"), 
        legend.position = "none",
        axis.title = element_text(size=11),
        plot.margin = margin(1,0, 0, 0, "cm"))

dp<-ggplot(dfdeath, aes(x = forcats::fct_rev(Agegp), y = Data, fill = Gender)) +
  geom_bar(stat = "identity", position = position_dodge(width=-0.5), width = 0.6, colour="black") +
  geom_text(size = 3, stat = 'identity',
            aes(label=c(abs(Data)),
                hjust=c(rep(c(1.4,-0.4),5),1.4))) +
  facet_share(~Gender, dir = "h", scales = "free", reverse_num = TRUE) +
  coord_flip() + 
  labs(y = paste("Frequency count (n)", " \n Total number of resolved cases ",sum(abs(dfresolve$Data))), x = "", title = "Demographic of Ontario COVID-19 fatal cases") +
  scale_fill_manual(values = c("pink", "lightblue"))+
  theme(panel.background = element_rect(fill = "white", colour = "white"),
        panel.grid.major = element_line(colour = "grey95"),
        panel.grid.minor = element_line(colour = "grey95"), 
        legend.position = "none",
        axis.title = element_text(size=11),
        plot.margin = margin(1,1, 0, 0, "cm"))

combo <- cowplot::plot_grid(resolvep, dp, rel_widths = c(0.5,0.5), align = "h")

cowplot::plot_grid(casep, combo, nrow=2, rel_heights = c(0.5,0.5),align = "v")

dev.off()
