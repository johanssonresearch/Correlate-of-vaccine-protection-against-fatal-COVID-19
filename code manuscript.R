setwd("C:/Covid")
library(readr)
library(tidyverse)
library(lubridate)
library(magrittr)
library(xlsx)
library(scales)
library(boot)
library(survival)
library(survminer)
library(ggfortify)
library(viridisLite)
library(mgcv)
library(splines)


# For plotting

plasma_pal <- c("steelblue", "#FC4E07")

# Anders tema
mitt_tema <- theme(
  # Hide panel borders and remove grid lines
  panel.border = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  # Change axis line
  axis.line = element_line(colour = "black"),
  # text size
  text = element_text(size = 30)
) 



df_big <- readRDS("Data/FINAL_big_2022-11-16.RDS")
df_lab <- readRDS("Data/Clean Lab-data 2022-11-17.RDS")


sabo_combined <- merge(df_lab, df_big, by.x = "Personnummer", by.y = "person_number")




# Make Data frame with correct format for iterative alorithm to add to, will remove first row later
proportion <- data.frame(Day = ymd("2050-01-01"), Prop = 0, Dead = 0, Alive = 0)
for(i in 0:307){
  # Count number of dead within 30 days
  dead <- sabo_summer_update %>% distinct(Personnummer, .keep_all = T) %>% filter(Death_date >= ymd("2021-09-16") + i & Death_date <= (ymd("2021-09-16") + 30 + i)) %>% count()
  # Count the number that was alive at start of 30 days
  alive <- sabo_summer_update %>% distinct(Personnummer, .keep_all = T) %>% filter(Death_date >= ymd("2021-09-16") + i | is.na(Death_date)) %>% count()
  # Give proportion
  prop <- dead/alive
  # Save what day was the end of 30 day period
  Day <- i
  proportion <- rbind(proportion, data.frame(Day = ymd("2021-10-28") + i, Prop = prop$n, Dead = dead$n, Alive = alive$n))
}



# Remove first row with nonsense data
proportion <- proportion[-1, ]
# Rescale to per 1000 individuals instead of proportion
proportion$Prop <- proportion$Prop * 1000

#############################

proportion_infected <- data.frame(Day = ymd("2050-01-01"), Prop = 0, Infected = 0, Population = 0)
for(i in 0:307){
  dead <- sabo_summer_update %>% distinct(Personnummer, .keep_all = T) %>% filter(Infection_date >= ymd("2021-09-16") + i & Infection_date <= (ymd("2021-09-16") + 30 + i)) %>% count()
  alive <- sabo_summer_update %>% distinct(Personnummer, .keep_all = T) %>% filter(Death_date >= ymd("2021-09-16") + i | is.na(Death_date)) %>% count()
  prop <- dead/alive
  Day <- i
  proportion_infected <- rbind(proportion_infected, data.frame(Day = ymd("2021-10-28") + i, Prop = prop$n, Infected = dead$n, Population = alive$n))
}

proportion_infected <- proportion_infected[-1, ]
proportion_infected$Prop <- proportion_infected$Prop * 1000


######




ggplot(data = proportion, aes(x = Day, y = Prop)) + geom_line() + geom_point() + theme_bw() + mitt_tema +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_y_continuous(name = "Moving 30 day average of events\n/ 1000 individuals", limits = c(0, 130), breaks = c(seq(0, 130, by = 10)))+
  scale_x_date(limits=c(ymd("2021-10-28", ymd("2022-08-31"))), breaks = "14 days" , date_labels = "%d-%b-%y") +
  geom_line(data = proportion_infected, aes(y = Prop ), stat = "identity")





titel <- paste("S-antibodies over time, N =", df_lab %>% filter(`Provtagningsdatum_S ELISA` >= ymd("2022-09-16"),`Provtagningsdatum_S ELISA` <= ymd("2021-08-31"), !is.na(`AUC_S ELISA`), `AUC_S ELISA` > 0.1) %>%  count())
sabo_combined %>% filter(`Provtagningsdatum_S ELISA` >= ymd("2021-09-21"),`Provtagningsdatum_S ELISA` <= ymd("2022-10-31"),!is.na(`AUC_S ELISA`), `AUC_S ELISA` > 0.1) %>% ggplot(aes(x = `Provtagningsdatum_S ELISA`, y = `AUC_S ELISA`)) +
  geom_point(size = 2) + geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs", k = 4), size = 3) + theme_bw() + xlab("Days since dose 4") + ylab("AUC (log)") +
  scale_x_date(limits=c(ymd("2021-09-21", ymd("2022-10-31"))), breaks = "2 weeks" , date_labels = "%d-%b-%y") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  scale_y_continuous(trans='log2', breaks = c(1, 10, 100, 1000, 10000, 100000, 1000000), labels = function(x) format(x, scientific = FALSE) )+
  mitt_tema +scale_color_manual(values = plasma_pal)+scale_fill_manual(values= plasma_pal) +labs(fill='Covid-19', color = 'Covid-19') +
  ggtitle(titel) 




# Vaccination and dates
datedose1 <- sabo_combined %>% distinct(Personnummer, .keep_all = T) %>% group_by(date_dose1) %>% count()
datedose2 <- sabo_combined %>% distinct(Personnummer, .keep_all = T) %>% group_by(date_dose2) %>% count()
datedose3 <- sabo_combined %>% distinct(Personnummer, .keep_all = T) %>% group_by(date_dose3) %>% count()
datedose4 <- sabo_combined %>% distinct(Personnummer, .keep_all = T) %>% group_by(date_dose4) %>% count()

# write.xlsx(data.frame(datedose1), "datedose1.xlsx")
# write.xlsx(data.frame(datedose2), "datedose2.xlsx")
# write.xlsx(data.frame(datedose3), "datedose3.xlsx")
# write.xlsx(data.frame(datedose4), "datedose4.xlsx")


df_for_age <- sabo_combined %>% distinct(Personnummer, .keep_all = T)
df_age_pyramid <- data.frame(Age = df_for_age$Age)
df_age_pyramid$AGEcut <- cut(df_age_pyramid$Age, breaks = seq(45, 105, 5), right = FALSE)
df_age_pyramid$Population <- 1
df_age_pyramid$Gender <- df_for_age$gender
df_age_pyramid <- aggregate(formula = Population ~ Gender + AGEcut, data = df_age_pyramid, FUN = sum)

## for simplicity, add the age group labels we used in popGHcens above
df_age_pyramid <- df_age_pyramid %>% arrange(Gender)
df_age_pyramid <- rbind(df_age_pyramid[11, ], df_age_pyramid)
df_age_pyramid[1,1]<- "Kvinna"
df_age_pyramid[1,3]<- 0
df_age_pyramid$Age <- rep(factor(paste(10:20*5, "-", 10:20*5 + 4, sep = "")), 2)
df_age_pyramid$Age <- factor(df_age_pyramid$Age,levels(df_age_pyramid$Age)[c(2:11,1 )])

## only use the three variables age, gender and population from the DF data
df_age_pyramid <- df_age_pyramid[,c("Age","Gender","Population")]
#write.xlsx(df_age_pyramid, "Age pyramid.xlsx")

# Population total number
N <- sum(df_age_pyramid$Population)
df_age_pyramid$Population[df_age_pyramid$Gender == "Man"] <-   round(df_age_pyramid$Population[df_age_pyramid$Gender == "Man"] / N * 100, 1)
df_age_pyramid$Population[df_age_pyramid$Gender == "Kvinna"] <-round(df_age_pyramid$Population[df_age_pyramid$Gender == "Kvinna"]/ N* 100, 1)



## barplots for male populations goes to the left (thus negative sign)
df_age_pyramid$Population <- ifelse(df_age_pyramid$Gender == "Man", -1*df_age_pyramid$Population, df_age_pyramid$Population)

plasma_pal <- c("steelblue", "#FC4E07")
## pyramid charts are two barcharts with axes flipped
pyramidGH2 <- ggplot(df_age_pyramid, aes(x = Age, y = Population)) +
  geom_bar(data = subset(df_age_pyramid, Gender == "Kvinna"), stat = "identity", fill = plasma_pal[1]) +
  geom_bar(data = subset(df_age_pyramid, Gender == "Man"), stat = "identity", fill = plasma_pal[2])+
  theme(axis.text.x = element_text(colour="grey20",size=25),
        axis.text.y = element_text(colour="grey20",size=25),
        axis.title.y = element_text(colour="grey20",size=30),
        panel.border=element_rect(fill=NA, size = 1)) +
  scale_y_continuous(breaks = seq(-25, 25, 5), labels = c(paste(as.character(round(seq(25, 0, -5), 2))), paste(as.character(round(seq(25, 0, -5), 2)))[5:1]), "Percentage") +
  coord_flip() + theme_bw() +
  theme(axis.text.x = element_text(colour="grey20",size=25),
        axis.text.y = element_text(colour="grey20",size=25),
        axis.title.y = element_text(colour="grey20",size=30),
        axis.title.x = element_text(colour="grey20",size=30),
        panel.border=element_rect(fill=NA, size = 1)) + mitt_tema
pyramidGH2





# Totalt antal individer oavsett provsvar
sabo_combined %>% distinct(Personnummer, .keep_all = TRUE) %>% count()
sabo_combined %>% distinct(Personnummer, .keep_all = TRUE) %>% group_by(gender) %>% count()

# Totalt antal individer med minst 1 provsvar på AUC
sabo_combined %>% filter(!is.na(`AUC_S ELISA`)) %>% distinct(Personnummer, .keep_all = TRUE) %>% count()
sabo_combined %>% filter(!is.na(`AUC_S ELISA`)) %>% distinct(Personnummer, .keep_all = TRUE) %>% group_by(gender) %>% count()

# Tabell totalt antal unika individer på boenden, oavsett provsvar
antal_per_boende_totalt <- sabo_combined %>% distinct(Personnummer, .keep_all = TRUE) %>% group_by(region_code, Äldreboende) %>% count()
write.xlsx(data.frame(antal_per_boende_totalt), "Resultat/antal_unika_individer_per_boende.xlsx")


## Regioner
sabo_combined %>% distinct(Personnummer, .keep_all = T) %>% group_by(region_code) %>% count() 

# Könsfördelning, alla individer oavsett provsvar.
sabo_combined %>% distinct(Personnummer, .keep_all = TRUE) %>% group_by(gender) %>% count()

# Vaccinationstäckning
df_dose4 <- sabo_combined %>% filter(!is.na(vaccine_dose4) | (is.na(Death_date) | Death_date > ymd("2022-05-03")))
df_dose4 <- df_dose4 %>% distinct(Personnummer, .keep_all = TRUE)
df_dose4 %>% group_by(vaccine_dose4) %>% summarise(N = n()) %>% mutate(Prop = N / sum(N))

df_dose3 <- sabo_combined %>% filter(!is.na(vaccine_dose3) | (is.na(Death_date) | Death_date > ymd("2021-11-01")))
df_dose3 <- df_dose3 %>% distinct(Personnummer, .keep_all = TRUE)
df_dose3 %>% group_by(vaccine_dose3) %>% summarise(N = n()) %>% mutate(Prop = N / sum(N))

df_dose2 <- sabo_combined %>% filter((is.na(Death_date) | Death_date > ymd("2021-10-09")))
df_dose2 <- df_dose2 %>% distinct(Personnummer, .keep_all = TRUE)
df_dose2 %>% group_by(vaccine_dose2) %>% summarise(N = n()) %>% mutate(Prop = N / sum(N))

df_dose1 <- sabo_combined %>% filter(!is.na(vaccine_dose1) | (is.na(Death_date) | Death_date > ymd("2021-03-01")))
df_dose1 <- df_dose1 %>% distinct(Personnummer, .keep_all = TRUE)
df_dose1 %>% group_by(vaccine_dose1) %>% summarise(N = n()) %>% mutate(Prop = N / sum(N))


sabo_combined %>% filter(covid == 1) %>% distinct(Personnummer)%>% count()
sabo_combined %>% filter(Infection_nr == 2) %>% group_by(Infection_date) %>% count()
sabo_combined %>% filter(Infection_nr == 1, Infection_date > ymd("2021-09-16")) %>% distinct(Personnummer, .keep_all = T)  %>% count()
sabo_combined %>% filter(covid == 1,Infection_date >= ymd("2021-10-01")) %>% distinct(Personnummer) %>% count()
sabo_combined %>% filter(covid == 1, Infection_nr == 1,Infection_date >= ymd("2022-01-01"))%>% distinct(Personnummer) %>% count()
sabo_combined %>% filter(!is.na(Death_date), Death_date >= ymd("2021-10-01")) %>% distinct(Personnummer) %>% count()
sabo_combined %>% filter(!is.na(Death_date), Death_date >= ymd("2022-01-01"))%>% distinct(Personnummer) %>% count()
sabo_combined %>% filter(!is.na(Death_date), Infection_date >=("2022-01-01"),Infection_date + 30 >= Death_date) %>% count()
sabo_combined$Ankomst_numeric <- as.numeric(sabo_combined$`Ankomstdatum_lab_S ELISA`)



sabo_combined %>% distinct(Personnummer, .keep_all = T) %>% count()
sabo_combined %>% distinct(Personnummer, .keep_all = T) %>% filter(Death_date < ymd("2021-11-28")) %>% count()
sabo_combined %>% distinct(Personnummer, .keep_all = T) %>% filter(Death_date < ymd("2022-02-27")) %>% count()
sabo_combined %>% distinct(Personnummer, .keep_all = T) %>% filter(Death_date < ymd("2022-02-27")) %>% count()




##### Remove those without provsvar
#sabo_combined %>% filter(is.na(`Ankomstdatum_lab_S ELISA`)) %>% utils::View()
#sabo_combined <- sabo_combined %>% filter(!is.na(`Ankomstdatum_lab_S ELISA`))
sabo_combined <- sabo_combined %>% filter(!is.na(`AUC_S ELISA`))
sabo_combined <- sabo_combined %>% mutate(Ankomst_numeric = as.numeric(`Ankomstdatum_lab_S ELISA`))

# Antal provsvar per provtillfälle, unika individer
sabo_combined %>% filter(!is.na(`AUC_S ELISA`)) %>% group_by(Provtagningstillfalle, gender) %>% distinct(Personnummer, .keep_all = TRUE) %>% summarise(N = n())


sabo_combined_dose3 <- sabo_combined %>% filter(!is.na(date_dose3)) %>% mutate(Ankomst_numeric = as.numeric(`Ankomstdatum_lab_S ELISA`) - 2, Dos3_numeric = as.numeric(date_dose3), Diff_to_dose_3 = Ankomst_numeric - Dos3_numeric)
# For those with dose 4, calculate Diff between sample date and dose 3 date.
#df_sabo_big %>% filter(!is.na(date_dose4)) %>% mutate(Ankomst_numeric = as.numeric(Ankomst_lab) - 2, Dos4_numeric = as.numeric(date_dose4), Diff = Ankomst_numeric - Dos4_numeric) -> fjutt


sabo_combined_dose3$date_dose4 <- as.numeric(sabo_combined_dose3$date_dose4)
sabo_combined_dose3$Ankomst_numeric <- as.numeric(sabo_combined_dose3$`Ankomstdatum_lab_S ELISA`)




############
titel <- paste("Covid både före och efter provtagning, N =", sabo_combined_dose3 %>% filter(Diff_to_dose_3 > -31  ) %>% distinct(Personnummer) %>% count())
sabo_combined_dose3 %>% filter(Diff_to_dose_3 > -31  , covid_before_lab ==1) %>% ggplot(aes(x = Diff_to_dose_3, y = `AUC_S ELISA`)) +
  geom_point(size = 2, color = plasma_pal[1]) + geom_smooth(color = "#FC4E07") + theme_bw() + xlab("Days since dose 3") + ylab("AUC (log)") +
  scale_x_continuous(limits=c(-30, 180), breaks =  seq(-30, 180, by = 10)) + scale_y_continuous(trans='log2', breaks = c(1, 10, 100, 1000, 10000, 100000, 1000000) )+
  mitt_tema + ggtitle(titel)


titel <- paste("De utan covid före provtagning, N =", sabo_combined_dose3 %>% filter(Diff_to_dose_3 > -31  , covid_before_lab == 0) %>% distinct(Personnummer) %>% count())
sabo_combined_dose3 %>% filter(Diff_to_dose_3 > -31  , covid_before_lab == 0) %>% ggplot(aes(x = Diff_to_dose_3, y = `AUC_S ELISA`)) +
  geom_point(size = 2, color = plasma_pal[1]) + geom_smooth(color = "#FC4E07") + theme_bw() + xlab("Days since dose 3") + ylab("AUC (log)") +
  scale_x_continuous(limits=c(-30, 180), breaks =  seq(-30, 180, by = 10)) + scale_y_continuous(trans='log2', breaks = c(1, 10, 100, 1000, 10000, 100000, 1000000) )+
  mitt_tema + ggtitle(titel)



# Select subpopulation to regress AUC decline
df_regression <- sabo_combined_dose3 %>% filter(Diff_to_dose_3 > 20, vaccine_dose3 != "Astra", date_dose4 > `Ankomstdatum_lab_S ELISA`, covid_before_lab == 0)
# Find persons with 2 samples after dose 3 with no infection
persons_two_samples_after_dose3_no_covid <- df_regression %>% group_by(Personnummer) %>% summarise(N = n()) %>% filter(N > 1)

colnames(df_regression)[22] <- "AUC"
modelt1000 <- gam(log(AUC)~s(Diff_to_dose_3, bs = "cs", k = 4) + vaccine_dose3 , family = gaussian, random= list(Personnummer~1),
                  data = df_regression %>% filter(Personnummer %in% persons_two_samples_after_dose3_no_covid$Personnummer))


plot(modelt1000, pages = 1)
summary(modelt1000)
qqnorm(modelt1000$residuals)
qqline(modelt1000$residuals)
shapiro.test(modelt1000$residuals)

## Linear regression from day 20 to day 80 after dose 3

glm1 <- lm(log(AUC)~Diff_to_dose_3, data = df_regression %>% filter(Diff_to_dose_3 <= 80, Diff_to_dose_3 >= 20))
summary(glm1)
Daily_decline_AUC <- exp(summary(glm1)$coefficient[2])
Daily_decline_AUC
qqplot(glm1)
library(car)
qqPlot(glm1$residuals)
plot(glm1)


## Linear regression from day 20 to day 80 after dose 4

glm2 <- lm(log(`AUC_S ELISA`)~Diff_to_dose_4, data = df_regression %>% filter(Diff_to_dose_4 <= 80, Diff_to_dose_4 >= 20))
summary(glm2)
Daily_decline_AUC <- exp(summary(glm2)$coefficient[2])
qqplot(glm2)
qqPlot(glm2$residuals)
plot(glm2)



titel <- paste("Delat på vaccin ,\nej covid eller dos 4 före blödning, N =", sabo_combined_dose3 %>% filter( vaccine_dose3 != "Astra", date_dose4 > `Ankomstdatum_lab_S ELISA`, Diff_to_dose_3 > -31, Diff_to_dose_3 < 181,!is.na(`AUC_S ELISA`) , covid_before_lab == 0) %>% count())
sabo_combined_dose3 %>% filter( vaccine_dose3 != "Astra", date_dose4 > `Ankomstdatum_lab_S ELISA`, Diff_to_dose_3 > -31, Diff_to_dose_3 < 181,!is.na(`AUC_S ELISA`) , covid_before_lab == 0) %>% ggplot(aes(x = Diff_to_dose_3, y = `AUC_S ELISA`, col= as.factor(gender))) +
  geom_point(size = 2, aes(colour = as.factor(gender))) + geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs", k = 4), aes(colour = as.factor(gender), fill = as.factor(gender)), size = 3) + theme_bw() + xlab("Days since dose 3") + ylab("AUC (log)") +
  scale_x_continuous(limits=c(-30, 180), breaks =  seq(-30, 180, by = 10)) + scale_y_continuous(trans='log2', breaks = c(1, 10, 100, 1000, 10000, 100000, 1000000) )+
  mitt_tema +scale_color_manual(values = vaccine_color[c(8,3,6)])+scale_fill_manual(values = vaccine_color[c(8,3,6)]) +labs(fill='Gender', color = 'Gender') +
  ggtitle(titel)



titel <- paste("Delat på vaccin ,\nej covid eller dos 4 före blödning, N =", sabo_combined_dose3 %>% filter( date_dose4 > `Ankomstdatum_lab_S ELISA`, Diff_to_dose_3 > -31 , `AUC_S ELISA` > 1250, covid_before_lab == 0) %>% count())
sabo_combined_dose3 %>% filter(Personnummer %in% persons_two_samples_after_dose3_no_covid$Personnummer, vaccine_dose3 != "Astra", date_dose4 > `Ankomstdatum_lab_S ELISA`, Diff_to_dose_3 > 20  , covid_before_lab == 0) %>% ggplot(aes(x = Diff_to_dose_3, y = `AUC_S ELISA`, col= as.factor(vaccine_dose3))) +
  geom_point(size = 2, aes(colour = as.factor(vaccine_dose3))) + geom_line(aes(color = Personnummer)) + theme_bw() + xlab("Days since dose 3") + ylab("AUC (log)") +
  scale_x_continuous(limits=c(-30, 180), breaks =  seq(-30, 180, by = 10)) + scale_y_continuous(trans='log2', breaks = c(1, 10, 100, 1000, 10000, 100000, 1000000) )+
  mitt_tema + theme(legend.position="none")
ggtitle(titel)


#### Dose 3, current manuscript
titel <- paste("Covid vs. icke-covid, ej dos 4 före blödning 3, N =", sabo_combined_dose3 %>% filter( date_dose4 > `Ankomstdatum_lab_S ELISA`,Diff_to_dose_3 >= -30, Diff_to_dose_3 <= 160  )  %>% count())
sabo_combined_dose3 %>% filter( date_dose4 > `Ankomstdatum_lab_S ELISA`,Diff_to_dose_3 >= -50, Diff_to_dose_3 <= 160  ) %>% ggplot(aes(x = Diff_to_dose_3, y = `AUC_S ELISA`, col= as.factor(covid_before_lab))) +
  geom_point(size = 2, aes(colour = as.factor(covid_before_lab))) + geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs", k = 4), aes(colour = as.factor(covid_before_lab), fill = as.factor(covid_before_lab)), size = 3) + theme_bw() + xlab("Days since dose 3") + ylab("AUC (log)") +
  scale_x_continuous(limits=c(-50, 160), breaks =  seq(-50, 160, by = 10)) + scale_y_continuous(trans='log2', breaks = c(1, 10, 100, 1000, 10000, 100000, 1000000), labels = function(x) format(x, scientific = FALSE) )+
  mitt_tema +scale_color_manual(values = plasma_pal)+scale_fill_manual(values= plasma_pal) +labs(fill='Covid-19', color = 'Covid-19') +
  ggtitle(titel)


### ACE-2, inhib plot current manuscript
titel <- paste("Covid vs. icke-covid, ej dos 4 före blödning 3, N =", sabo_combined_dose3 %>% filter(Diff_to_dose_3 >= -20, Diff_to_dose_3 <= 35, !is.na(`SARS-CoV-2_Neut_Spike ACE-2_Mesoscale_AU_ml`))  %>% count())
sabo_combined_dose3 %>% filter(Diff_to_dose_3 >= -20,Diff_to_dose_3 <=35, !is.na(`SARS-CoV-2_Neut_Spike ACE-2_Mesoscale_AU_ml`)) %>% ggplot(aes(x = Diff_to_dose_3, y = `SARS-CoV-2_Neut_Spike ACE-2_Mesoscale_AU_ml`, col= as.factor(covid_before_lab))) +
  geom_point(size = 2, aes(colour = as.factor(covid_before_lab))) + geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs", k = 4), aes(colour = as.factor(covid_before_lab), fill = as.factor(covid_before_lab)), size = 3) + theme_bw() + xlab("Days since dose 3") + ylab("ACE-2") +
  scale_x_continuous(limits=c(-20, 35), breaks =  seq(-20, 35, by = 10)) + scale_y_continuous(limits = c(0, 100),oob=scales::rescale_none) + mitt_tema +scale_color_manual(values = plasma_pal)+scale_fill_manual(values= plasma_pal) +labs(fill='Covid-19', color = 'Covid-19') +
  ggtitle(titel)



# Dose 4 data frame
sabo_combined_dose4 <- sabo_combined %>% filter(!is.na(date_dose4)) %>% mutate(Ankomst_numeric = as.numeric(`Ankomstdatum_lab_S ELISA`) - 2, Dos4_numeric = as.numeric(date_dose4), Diff_to_dose_4 = Ankomst_numeric - Dos4_numeric)
sabo_combined_dose4$date_dose4 <- as.numeric(sabo_combined_dose4$date_dose4)




#### Current figure Figure 2

titel <- paste("S-antibodies dose 4, N =", sabo_combined_dose4 %>% filter(Diff_to_dose_4 >= -30, Diff_to_dose_4 <= 125  , !is.na(Dos4_numeric)) %>%  count())
sabo_combined_dose4 %>% filter(!is.na(`AUC_S ELISA`) ,!is.na(Dos4_numeric), Diff_to_dose_4 >= -30, Diff_to_dose_4 <= 125) %>% ggplot(aes(x = Diff_to_dose_4, y = `AUC_S ELISA`, col= as.factor(covid_before_lab))) +
  geom_point(size = 2, aes(colour = as.factor(covid_before_lab))) + geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs", k = 4), aes(colour = as.factor(covid_before_lab), fill = as.factor(covid_before_lab)), size = 3) + theme_bw() + xlab("Days since dose 4") + ylab("AUC (log)") +
  scale_x_continuous(limits=c(-30, 125), breaks =  seq(-30, 125, by = 10)) + scale_y_continuous(trans='log2', breaks = c(1, 10, 100, 1000, 10000, 100000, 1000000), labels = function(x) format(x, scientific = FALSE) )+
  mitt_tema +scale_color_manual(values = plasma_pal)+scale_fill_manual(values= plasma_pal) +labs(fill='Covid-19', color = 'Covid-19') +
  ggtitle(titel) 


#### Mattias figure


titel <- paste("S-antibodies dose 4, N =", sabo_combined_dose4 %>% filter(Diff_to_dose_4 >= -150  , !is.na(Dos4_numeric)) %>%  count())
sabo_combined_dose4 %>% filter(!is.na(`AUC_S ELISA`) ,!is.na(Dos4_numeric)) %>% ggplot(aes(x = Diff_to_dose_4, y = `AUC_S ELISA`, col= as.factor(covid_before_lab))) +
  geom_point(size = 2, aes(colour = as.factor(covid_before_lab))) + geom_smooth(method = 'gam', formula = y ~ s(x, bs = "cs", k = 4), aes(colour = as.factor(covid_before_lab), fill = as.factor(covid_before_lab)), size = 3) + theme_bw() + xlab("Days since dose 4") + ylab("AUC (log)") +
  scale_x_continuous(limits=c(-150, 125), breaks =  seq(-150, 125, by = 10)) + scale_y_continuous(trans='log2', breaks = c(1, 10, 100, 1000, 10000, 100000, 1000000), labels = function(x) format(x, scientific = FALSE) )+
  mitt_tema +scale_color_manual(values = plasma_pal)+scale_fill_manual(values= plasma_pal) +labs(fill='Covid-19', color = 'Covid-19') +
  ggtitle(titel) 






# Select subpopulation to regress AUC decline
df_glm <- sabo_combined_dose3 %>% filter(Diff_to_dose_3 > 20, `AUC_S ELISA` > 495,vaccine_dose3 != "Astra", date_dose4 > `Ankomstdatum_lab_S ELISA`, covid_before_lab == 0)
# Find persons with 2 samples after dose 3 with no infection
persons_two_samples_after_dose3_no_covid <- df_glm %>% group_by(Personnummer) %>% summarise(N = n()) %>% filter(N > 1)

colnames(df_glm)[22] <- "AUC"

modelt1000 <- gam(log(AUC)~s(Diff_to_dose_3, bs = "cs", k = 4) + vaccine_dose3 , family = gaussian, random= list(Personnummer~1),
                  data = df_glm %>% filter(Personnummer %in% persons_two_samples_after_dose3_no_covid$Personnummer))
df_glm %>% filter(Personnummer %in% persons_two_samples_after_dose3_no_covid$Personnummer) %>% group_by(vaccine_dose3) %>% count()
df_glm %>% filter(Personnummer %in% persons_two_samples_after_dose3_no_covid$Personnummer) %>% group_by(vaccine_dose3) %>% summarise(median(AUC))

sabo_combined %>% filter(`Provtagningsdatum_S ELISA` <= date_dose3, covid_before_lab  == 0)%>% summarise(mean(`AUC_S ELISA`))

sabo_combined %>% filter(Infection_date <= ymd("2022-08-31") | is.na(Infection_date), Death_date <= ymd("2022-09-30") | is.na(Death_date))

plot(modelt1000)
gam.check(modelt1000)
png("All residual GAM-model.png", width = 180, height = 140, units = "mm", pointsize = 9, res = 300 )
par(mfrow=c(2,2))
gam.check(modelt1000,pch=19,cex=.3)

dev.off()


# Difference in AUC level vaccine
exp(summary(modelt1000)$p.coeff)
qqnorm(modelt1000$residuals)
qqline(modelt1000$residuals)
shapiro.test(modelt1000$residuals)


# Difference AUC level gender


modelt2000 <- gam(log(AUC)~s(Diff_to_dose_3, bs = "cs", k = 4) + gender+ vaccine_dose3 , family = gaussian, random= list(Personnummer~1),
                  data = df_glm %>% filter(Personnummer %in% persons_two_samples_after_dose3_no_covid$Personnummer))



plot(modelt2000)
gam.check(modelt2000)
png("All residual GAM-model gender.png", width = 180, height = 140, units = "mm", pointsize = 9, res = 300 )
par(mfrow=c(2,2))
gam.check(modelt2000,pch=19,cex=.3)

dev.off()

# Difference in AUC level gender
exp(summary(modelt2000)$p.coeff)
qqnorm(modelt2000$residuals)
qqline(modelt2000$residuals)
shapiro.test(modelt2000$residuals)



sabo_combinedsabo_combined_dose3$`AUC_S ELISA`
modelt3000 <- gam(log(`AUC_S ELISA`)~s(Diff_to_dose_3, bs = "cs", k = 4) + gender , family = gaussian, random= list(Personnummer~1),
                  data = sabo_combined_dose3)



plot(modelt2000)
gam.check(modelt2000)
png("All residual GAM-model gender.png", width = 180, height = 140, units = "mm", pointsize = 9, res = 300 )
par(mfrow=c(2,2))
gam.check(modelt2000,pch=19,cex=.3)

dev.off()












#########################

### Make data to compare two slopes

df_new_lm <- rbind(sabo_combined %>% filter(Doses == 3), sabo_combined %>% filter(Doses == 4))

glm3 <- lm(log(AUC)~`Diff to dose`+ Doses + Doses:`Diff to dose`, data = df_new_lm %>% filter(`Diff to dose` <= 80, `Diff to dose` >= 20))
summary(glm3)
exp(summary(glm3)$coefficients[2])
exp(summary(glm3)$coefficients)

#########################




p_tile <- quantile(sabo_combined$AUC_ADJUSTED[sabo_combined$Group == "Infected Covid" & !is.na(sabo_combined$AUC_ADJUSTED)], probs = 0.1, na.rm = T)

sabo_combined$Responders <- "Normal"
sabo_combined$Responders[sabo_combined$AUC_ADJUSTED <= p_tile] <- "Low"


fit = survfit(Surv(Diff_to_death_from_covid,Dead_or_alive)~Responders, data=sabo_combined[sabo_combined$Group == "Infected Covid" & !is.na(sabo_combined$AUC_ADJUSTED), ])

ggsurvplot(fit,
           fun = "event",
           font.main = c(16),
           ylim = c(0, 0.45),
           font.x = c(16),
           font.y = c(16),
           font.tickslab = c(16),
           risk.table.fontsize = c(6),
           risk.table.x.text = c(9),
           conf.int=T, # add confidence intervals
           pval=TRUE, # show the p-value for the log-rank test
           risk.table=TRUE,# show a risk table below the plot
           #legend.labs = brks,
           #legend.labs=c("Low responders (AUC < 1500 at any given time)", "Normal responders"), # change group labels
           #legend.title="Group",  # add legend title
           #palette=c("orchid2","dodgerblue4"), # change colors of the groups
           title=" 10-Percentile (AUC < 479) of \nAUC at day 60 after dose 3", # add title to plot
           risk.table.height=.2,break.x.by = 5)



# Relevel to plot correct order
sabo_combined$Group <- relevel(sabo_combined$Group, "Infected Covid")
fit_covid = survfit(Surv(Diff_to_death_from_covid,Dead_or_alive)~Group, data=sabo_combined)


ggsurvplot(fit_covid,
           ylim = c(0, 0.12),pval.coord =c(0.05, 0.07),
           fun = "event",
           
           font.main = c(16),
           font.x = c(16),
           font.y = c(16),
           font.tickslab = c(16),
           risk.table.fontsize = c(6),
           risk.table.x.text = c(9),
           conf.int=TRUE, # add confidence intervals
           pval=TRUE, # show the p-value for the log-rank test
           risk.table=TRUE, # show a risk table below the plot
           legend.labs=c("Covid", "Icke-Covid"), # change group labels
           legend.title="Group",  # add legend title
           palette=c("orchid2", "dodgerblue4"), # change colors of the groups
           title="Kaplan-Meier Curve for Covid vs non-covid\nat least dose 3 at infection", # add title to plot
           risk.table.height=.2,break.x.by = 5)
