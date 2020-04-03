# Demographic Pyramid Plot of Ontario confirmed COVID-19 cases

Contributor: [Kuan Liu](https://www.linkedin.com/in/kuan-liu-uoft/)

Use of Data: Confirmed positive cases of COVID-19 in Ontario. This dataset compiles daily snapshots of publicly reported data on 2019 Novel Coronavirus (COVID-19) testing in Ontario. Contains information licensed under the Open Government Licence – Ontario.

```{r setup, echo=FALSE, results='none', message = FALSE, warning = FALSE}
on_case <- read.table("https://data.ontario.ca/dataset/f4112442-bdc8-45d2-be3c-12efae72fb27/resource/455fd63b-603d-4608-8216-7d8647f43350/download/conposcovidloc.csv", 
                 header = TRUE,
                 sep = ",")
  on_case_clean<-on_case
levels(on_case_clean$Age_Group)<-ifelse(levels(on_case_clean$Age_Group) %in% c("80s","90s"),">80",levels(on_case_clean$Age_Group))

on_case_clean$CLIENT_GENDER[on_case_clean$CLIENT_GENDER == "UNKNOWN"]<-NA
on_case_clean$CLIENT_GENDER[on_case_clean$CLIENT_GENDER == "TRANSGENDER"]<-NA
on_case_clean$Age_Group[on_case_clean$Age_Group == "Unknown"]<-NA


on_case_clean<-on_case_clean[complete.cases(on_case_clean),] #2793-2774 = 19;
on_case_resolve<-subset(on_case_clean, RESOLVED == "Yes") #828;

#19 unknown age or gender, 4 out of 19 resolved;

# 1.Create Dataset;
case <- on_case_clean %>% group_by(Age_Group,CLIENT_GENDER) %>% summarise(Freq=n())
case_resolve<- on_case_resolve %>% group_by(Age_Group,CLIENT_GENDER) %>% summarise(Freq=n())

case$Freq[case$CLIENT_GENDER=="FEMALE"]<- (-case$Freq[case$CLIENT_GENDER=="FEMALE"])
case_resolve$Freq[case_resolve$CLIENT_GENDER=="FEMALE"]<- (-case_resolve$Freq[case_resolve$CLIENT_GENDER=="FEMALE"])

names(case)<-c("Age group","Gender","Freqency count (n)")
names(case_resolve)<-c("Age group","Gender","Freqency count (n)")


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

casep<-ggplot(dfcase, aes(x = forcats::fct_rev(Agegp), y = Data, fill = Gender)) +
  geom_bar(stat = "identity", position = position_dodge(width=-0.5), width = 0.8, colour="black") +
  geom_text(size = 3, stat = 'identity',
            aes(label=c(abs(Data)),
  hjust=c(rep(c(-0.4,1.4),8)))) +
  facet_share(~Gender, dir = "h", scales = "free", reverse_num = TRUE) +
  coord_flip() + 
  labs(y = paste("Frequency count (n)", " \n Total number of confirmed cases ",sum(abs(dfcase$Data))), x = "", title = paste("Demographic of Ontario COVID-19 confirm cases, as of", format(Sys.time(), "%b %d, %Y"), "\n (known age and gender in female and male)")) +
  scale_fill_manual(values = c("orange", "lightblue"))+
  theme(panel.background = element_rect(fill = "white", colour = "white"),
        panel.grid.major = element_line(colour = "grey95"),
        panel.grid.minor = element_line(colour = "grey95"), 
        legend.position = "none",
        axis.title = element_text(size=11),
        plot.margin = margin(1,2, 1, 1, "cm"))

resolvep<-ggplot(dfresolve, aes(x = forcats::fct_rev(Agegp), y = Data, fill = Gender)) +
  geom_bar(stat = "identity", position = position_dodge(width=-0.5), width = 0.8, colour="black") +
  geom_text(size = 3, stat = 'identity',
            aes(label=c(abs(Data)),
                hjust=c(rep(c(-0.4,1.4),8)))) +
  facet_share(~Gender, dir = "h", scales = "free", reverse_num = TRUE) +
  coord_flip() + 
  labs(y = paste("Frequency count (n)", " \n Total number of resolved cases ",sum(abs(dfresolve$Data))), x = "", title = paste("Demographic of Ontario COVID-19 resolved cases, as of", format(Sys.time(), "%b %d, %Y"), "\n (known age and gender in female and male)")) +
  scale_fill_manual(values = c("orange", "lightblue"))+
  theme(panel.background = element_rect(fill = "white", colour = "white"),
        panel.grid.major = element_line(colour = "grey95"),
        panel.grid.minor = element_line(colour = "grey95"), 
        legend.position = "none",
        axis.title = element_text(size=11),
        plot.margin = margin(1,2, 1, 1, "cm"))

cowplot::plot_grid(casep, resolvep, nrow=2, rel_heights = c(0.5,0.5),align = "v")
            
```
