# Chargement des bibliothèques nécessaires
library(readr)
library(openxlsx)
library(readxl)
library(ggplot2)
library(questionr)
library(tidyverse)
library(dplyr)
library(RColorBrewer)
library(haven)
library(gridExtra)
library(rmarkdown)




# Importation et analyse des données de placement
Placement_Data_Full_Class <- read_csv("data/Placement_Data_Full_Class.csv")
df <- Placement_Data_Full_Class
View(df)

# Nettoyage des données
df_notna <- drop_na(df)  # Suppression des valeurs manquantes
df_valuena <- colSums(is.na(df))  # Compte des valeurs manquantes par colonne
df_valuena
df_isnabysalary <- df %>% filter(is.na(salary))
df2 <- df %>% mutate(salary = replace_na(salary, 0))
freq.na(Placement_Data_Full_Class)
View(df2)

# Analyse descriptive par genre

df_gender <- df %>%
  group_by(gender) %>%
  summarise(Effectif = n()) %>%
  mutate(percent = Effectif / sum(Effectif) * 100)
View(df_gender)

df_hsc_svsgender<-df2%>%
  group_by(gender,hsc_s)%>%
  summarize(Count=n(),.groups = 'drop')%>%
  pivot_wider(names_from =hsc_s , values_from = Count, values_fill = list(Count = 0))
view(df_hsc_svsgender)

p1<-ggplot(df_hsc_svsgender,aes(x=gender,y=Arts,fill = gender))+
  geom_bar(stat = 'identity',position = 'dodge')+
  geom_text(aes(label = Arts),size = 10)
p2<-ggplot(df_hsc_svsgender,aes(x=gender,y=Science,fill = gender))+
  geom_bar(stat = 'identity',position = 'dodge')+
  geom_text(aes(label = Science),size = 10)
p3<-ggplot(df_hsc_svsgender,aes(x=gender,y=Commerce,fill = gender))+
  geom_bar(stat = 'identity',position = 'dodge')+
  geom_text(aes(label = Commerce),size = 10)
grid.arrange(p1,p2,p3,ncol=3)

# Visualisation des données par genre
ggplot(df_gender, aes(x = gender, y = percent, fill = gender)) +
  geom_bar(stat = "identity", position = "dodge") +
  #scale_fill_manual(values = c("blue", "skyblue")) +
  labs(
    title = "Gender Repartition in the Population",
    subtitle = "2023-2024",
    y = "Percentage (%)",
    x = "Gender",
    caption = "Source: www.gnielgandaho@gmail.com"
  )+
  geom_text(aes(label=percent))

# Visualisation des données de placement par salaire et diplôme
ggplot(df2, aes(x = degree_p, y = salary, colour = gender)) +
  geom_jitter(width = 0.75, height = 0.75) +
  facet_wrap(~gender, nrow = 2) +
  labs(
    title = "Salary Distribution by Degree and Gender",
    x = "Degree Percentage",
    y = "Salary",
    caption = "Source: Placement_Data_Full_Class.csv"
  )
#correlation en variable
library(ggplot2)
ggplot(df, aes(x = hsc_p)) +
  geom_histogram (aes(y = ..density..), bins = 30, fill = "blue", alpha = 0.5) +
  geom_density(color = "red", size = 1)
shapiro.test(df$hsc_p)



# Affichage des résumés des données
summary(df)
summary(df2)