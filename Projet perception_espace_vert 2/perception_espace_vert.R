#------------------------Package-------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(readxl)
library(readr)
library(openxlsx)
library(questionr)
library(tidyverse)
library(dplyr)
library(RColorBrewer)
library(ggplot2)
library(tidyverse)
library(ggplot2)
library(carData)
library(car)
library(officer)
library(tidyr)
library(rmarkdown)
library(flextable)
library(fBasics)
library(nnet)

#------------------------Chargement de bases------------------------------------------------------------------

Cartographie_et_perception_des_services_écosystémiques_all_versions_labels_2024_07_08_14_45_00_1_ <- read_excel("H:/Mon Drive/Documents/HP ELITEBOOK/R/TRAVAUX-R/data/perception_espace_vert/Cartographie_et_perception_des_services_écosystémiques_-_all_versions_-_labels_-_2024-07-08-14-45-00 (1).xlsx")
df<-Cartographie_et_perception_des_services_écosystémiques_all_versions_labels_2024_07_08_14_45_00_1_
df2<-df%>%select(-1,-2,-3,-5,-6,-58,-59,-60,-61,-62,-63,-64,-65,-66,-67,-68,-69,-70)

#------------------------Recodage des noms---------------------------------------------------------------------

# Créer un vecteur contenant les nouveaux noms de variables
new_names <- c("arrondissement", "age", "sexe", "niveau_education", "autre_education",
               "profession", "autre_profession", "frequence_visite", "raison_non_visite",
               "autre_raison", "espace_visite", "activite_generale", "activite_promenade",
               "activite_sports", "activite_pique_nique", "activite_lecture",
               "activite_observation", "activite_autre", "autre_activite",
               "benefices_espaces_verts", "benefice_loisir_detente", "benefice_rencontre_sociale",
               "benefice_reduction_bruit", "benefice_autre", "autre_benefice",
               "services_ecosystemiques", "service_loisir_detente", "service_rencontre_sociale",
               "service_reduction_bruit", "service_autre", "autre_service",
               "evaluation_etat_espaces_verts", "plaisir_parc", "plaisir_cadre_naturel",
               "plaisir_paysage", "plaisir_ambiance_paisible", "plaisir_equipements",
               "plaisir_evenements", "plaisir_rencontre", "plaisir_autre", "autre_plaisir",
               "amelioration_espaces_verts", "amelioration_bancs_aires", "amelioration_proprete",
               "amelioration_espaces_jeux", "amelioration_securite", "amelioration_arbre_vegetation",
               "amelioration_autre", "autre_amelioration", "participation_initiatives",
               "implication_autorites", "implication_communaute")

df3<- df2%>%rename_with(~ new_names)

colnames(df3)
labels(df3)

#------------------------verification prealable de bases------------------------------------------------------------------------------
#valeur nul
freq.na(df3)
head(df3)
tail(df3)
freq(df3$frequence_visiter)

#------------------------Recodage des modalite--------------------------------------------------------------------------------

df3$espace_visite <-factor(df3$espace_visite, labels=c("BIO GUERRA","BIO GUERRA/PAPINI","BIO GUERRA/PAPINI","BIO GUERRA","COTEB","Carrefour al houda","Devant Kognonsa","Devant confort line","FORET","COTEB","FORET/COTEB","FORET","HUBERT MAGA","HUBERT MAGA","HUBERT MAGA/COTEB","Devant confort line","PAPINI","HUBERT MAGA","PAPINI","BIO GUERRA/PAPINI","BIO GUERRA/PAPINI","COTEB","COTEB","COTEB","COTEB/FORET","HUBERT MAGA/COTEB","HUBERT MAGA/COTEB","COTEB/FORET"
))
df3$activite_generale<-factor(df3$activite_generale,labels = c("PROMENADE","PROMENADE/SPORT","PROMENADE/OBSERVATION","PROMENADE", "PROMENADE/LECTURE","AUTRE","PROMENADE/LECTURE","PROMENADE/SPORT/ETUDE","PROMENADE/PICNIC","PROMENADE/LECTURE/PICNIC", "PROMENADE/OBSERVATION" ,"PROMENADE/LECTURE", "PROMENADE/LECTURE/PICNIC","PROMENADE/SPORT/ETUDE/OBSERVATION","PROMENADE/LECTURE/OBSERVATION" ,"PROMENADE/LECTURE/PICNIC","PROMENADE/LECTURE/SPORT",  "PROMENADE/SPORT"))
df3$amelioration_espaces_verts<-factor(df3$amelioration_espaces_verts,labels = c("BANCS+AIRE REPOS+PROPRETE","BANCS+AIRE REPOS+PROPRETE", "PROPRETE","BANCS+AIRE REPOS+PROPRETE+ABRE","AIRE+ABRE","BANCS+AIRE REPOS+PROPRETE+ABRE","BANCS+AIRE REPOS", "PROPRETE+ABRE" ,"BANCS+AIRE REPOS+PROPRETE+ABRE", "PROPRETE+ABRE", "ESPACE JEUX+AIRE REPOS+PROPRETE+ABRE", "BANCS+AIRE REPOS+PROPRETE+JEUX","BANCS+AIRE REPOS+PROPRETE+ABRE+JEUX","BANCS+AIRE REPOS+JEUX", "AUTRE","PROPRETE+AUTRE", "BANCS+AIRE REPOS+PROPRETE+ABRE+JEUX","PROPRETE+ABRE+JEUX","BANCS+AIRE REPOS+ABRE+JEUX","BANCS+AIRE REPOS+PROPRETE+ABRE+SECURITE","BANCS+AIRE REPOS+PROPRETE+ABRE+SECURITE","BANCS+AIRE REPOS+PROPRETE+ABRE+JEUX","BANCS+AIRE REPOS+ABRE+JEUX","BANCS+AIRE REPOS+ABRE+JEUX"))
df3$benefices_espaces_verts<-factor(df3$benefices_espaces_verts,labels = c("DETENTE","DETENTE/RENCONTRE","RENCONTRE",  "DETENTE/RENCONTRE", "DETENTE/RENCONTRE/REDUCTION BRUIT", "RENCONTRE","DETENTE/RENCONTRE","DETENTE/RENCONTRE/REDUCTION BRUIT","DETENTE/RENCONTRE", "DETENTE/REDUCTION BRUIT", "AUTRE", "RENCONTRE", "DETENTE", "DETENTE/RENCONTRE/REDUCTION BRUIT","DETENTE","DETENTE/RENCONTRE/REDUCTION BRUIT" , "REDUCTION BRUIT","RENCONTRE/REDUCTION BRUIT"))

df3$sexer<-as.numeric(factor(df3$sexe,labels = c(0,1)))
df3$ager<-as.numeric(factor(df3$age,labels = c(0,1,2,3)))
df3$frequence_visiter<-as.numeric(factor(df3$frequence_visite,labels = c(0,1,2,3,4,5)))
df3$espace_visiter<-as.numeric(factor(df3$espace_visite,labels = c(0,1,2,3,4,5,6,7,8,9,10,11)))
df3$niveau_educationr<-as.numeric(factor(df3$niveau_education,labels = c(0,1,2)))
df3$professionr <-as.numeric(factor(df3$profession,labels = c(0,1,2,3,4)))
df3$autreprofessionr <-as.numeric(factor(df3$autre_profession,labels = c(0,1,2,3,4)))
df3$raison_non_visiter <-as.numeric(factor(df3$raison_non_visite,labels = c(0,1,2)))
df3$activite_generaler <-as.numeric(factor(df3$activite_generale,labels = c(0,1,2,3,4,5,6,7,8,9,10)))
df3$benefices_espaces_vertsr<-as.numeric(factor(df3$benefices_espaces_verts,labels = c(0,1,2,3,4,5,6,7)))
df3$evaluation_etat_espaces_vertsr<-as.numeric(factor(df3$evaluation_etat_espaces_verts,labels = c(0,1,2,3)))
df3$participation_initiativesr<-as.numeric(factor(df3$participation_initiatives,labels = c(0,1,2)))
df3$implication_autoritesr<-as.numeric(factor(df3$implication_autorites,labels = c(2,0,1)))
df3$implication_communauter<-as.numeric(factor(df3$implication_communaute,labels = c(1,0,2)))

#------------------------Analyse descriptif-------------------------------------------------------------------------

#analyse descriptif

df3_numeriq<-df3[sapply(df3, is.numeric)]
df3_numeriq<-df3_numeriq%>%select(autreprofessionr,raison_non_visiter,espace_visiter,activite_generaler,benefices_espaces_vertsr, evaluation_etat_espaces_vertsr,sexer,ager,frequence_visiter,niveau_educationr ,professionr,participation_initiativesr,implication_autoritesr,implication_communauter   )
describe(df3_numeriq ,num.desc = c("mean", "median", "sd","min", "max", "valid.n"))


df_frqplus<-df3%>%group_by(frequence_visite)%>%summarise(Effectif=n())



df_age<-df3%>%group_by(age)%>%summarise(Effectif=n())%>%mutate(`Pourcentage%` =round (((Effectif/102)*100),2))

df_agen<-df3%>%group_by(age)%>%summarise(Effectif=n())%>%mutate(`Pourcentage%` =round (((Effectif/102)*100),2))
Total <- df_agen %>%
  summarize(across(where(is.numeric), sum, na.rm = TRUE)) %>%
  mutate(age = "Total")
df_agen <- bind_rows(df_agen, Total)

df_frqvisit<-df3%>%group_by(frequence_visite)%>%summarise(Effectif=n())%>%mutate(`Pourcentage%` =round (((Effectif/102)*100),2))

df_raisonn<-df3%>%group_by(raison_non_visite)%>%summarise(Effectif=n())%>%mutate(`Pourcentage%` =round (((Effectif/102)*100),2))

df_raison<-df3%>%group_by(raison_non_visite)%>%summarise(Effectif=n())%>%mutate(`Pourcentage%` =round (((Effectif/102)*100),2))
df_raison<-df_raison%>%na.omit()
Total <- df_raison %>%
  summarize(across(where(is.numeric), sum, na.rm = TRUE)) %>%
  mutate(raison_non_visite = "Total")
df_raison <- bind_rows(df_raison, Total)
df_raison<-df_raison%>%na.omit()



#df_frqplus_sexe<-df3%>%group_by(frequence_visite,sexe)%>%summarize(Count=n(),.groups = 'drop')%>%pivot_wider(names_from =sexe , values_from = Count, values_fill = list(Count = 0))
#df_frqplus_niveduc<-df3%>%group_by(frequence_visite,niveau_education)%>%summarize(Count=n(),.groups = 'drop')%>%pivot_wider(names_from =niveau_education , values_from = Count, values_fill = list(Count = 0))
#df_frqplus_espacevisite_plus<-df3%>%group_by(frequence_visite,espace_visite)%>%summarize(Count=n(),.groups = 'drop')%>%pivot_wider(names_from =espace_visite , values_from = Count, values_fill = list(Count = 0))

df_niveduc<-df3%>%group_by(niveau_education)%>%summarise(Effectif=n())%>%mutate(`Pourcentage%` =round (((Effectif/102)*100),2))
Total <- df_niveduc %>%
  summarize(across(where(is.numeric), sum, na.rm = TRUE)) %>%
  mutate(niveau_education = "Total")
df_niveduc <- bind_rows(df_niveduc, Total)

df_sexe<-df3%>%group_by(sexe)%>%summarise(Effectif=n())%>%mutate(`Pourcentage%` =round (((Effectif/102)*100),2))
Total <- df_sexe %>%
  summarize(across(where(is.numeric), sum, na.rm = TRUE)) %>%
  mutate(sexe = "Total")
df_sexe <- bind_rows(df_sexe, Total)


df_profession<-df3%>%group_by(profession)%>%summarise(Effectif=n())%>%mutate(`Pourcentage%` =round (((Effectif/102)*100),2))
Total <- df_profession %>%
  summarize(across(where(is.numeric), sum, na.rm = TRUE)) %>%
  mutate(profession = "Total")
df_profession <- bind_rows(df_profession, Total)


df_autreprofession<-df3%>%group_by(autre_profession)%>%summarise(Effectif=n())

df_visiten<-df3%>%group_by(espace_visite)%>%na.omit()%>%summarise(Effectif=n())%>%mutate(`Pourcentage%` =round (((Effectif/102)*100),2))

df_visite<-df3%>%group_by(espace_visite)%>%summarise(Effectif=n())%>%mutate(`Pourcentage%` =round (((Effectif/102)*100),2))
Total <- df_visite %>%
  summarize(across(where(is.numeric), sum, na.rm = TRUE)) %>%
  mutate(espace_visite = "Total")
df_visite <- bind_rows(df_visite, Total)


df_activite_generale<-df3%>%group_by(activite_generale)%>%summarise(Effectif=n())%>%mutate(`Pourcentage%` =round (((Effectif/102)*100),2))
Total <- df_activite_generale %>%
  summarize(across(where(is.numeric), sum, na.rm = TRUE)) %>%
  mutate(activite_generale = "Total")
df_activite_generale <- bind_rows(df_activite_generale, Total)


df_beneficeepacevertr<-df3%>%group_by(benefices_espaces_verts)%>%summarise(Effectif=n())%>%mutate(`Pourcentage%` =round (((Effectif/102)*100),2))
Total <- df_beneficeepacevertr %>%
  summarize(across(where(is.numeric), sum, na.rm = TRUE)) %>%
  mutate(benefices_espaces_verts = "Total")
df_beneficeepacevertr <- bind_rows(df_beneficeepacevertr, Total)

df_evaluation<-df3%>%group_by(evaluation_etat_espaces_verts)%>%summarise(Effectif=n())%>%mutate(`Pourcentage%` =round (((Effectif/102)*100),2))
Total <- df_evaluation %>%
  summarize(across(where(is.numeric), sum, na.rm = TRUE)) %>%
  mutate(evaluation_etat_espaces_verts = "Total")
df_evaluation <- bind_rows(df_evaluation, Total)


df_amelioration<-df3%>%group_by(amelioration_espaces_verts)%>%summarise(Effectif=n())%>%mutate(`Pourcentage%` =round (((Effectif/102)*100),2))
Total <- df_amelioration %>%
  summarize(across(where(is.numeric), sum, na.rm = TRUE)) %>%
  mutate(amelioration_espaces_verts = "Total")
df_amelioration <- bind_rows(df_amelioration, Total)


df_participation_initi<-df3%>%group_by(participation_initiatives)%>%summarise(Effectif=n())%>%mutate(`Pourcentage%` =round (((Effectif/102)*100),2))
Total <- df_participation_initi %>%
  summarize(across(where(is.numeric), sum, na.rm = TRUE)) %>%
  mutate(participation_initiatives = "Total")
df_participation_initi <- bind_rows(df_participation_initi, Total)


df_implication_auto<-df3%>%group_by(implication_autorites)%>%summarise(Effectif=n())%>%mutate(`Pourcentage%` =round (((Effectif/102)*100),2))
Total <- df_implication_auto %>%
  summarize(across(where(is.numeric), sum, na.rm = TRUE)) %>%
  mutate(implication_autorites = "Total")
df_implication_auto <- bind_rows(df_implication_auto, Total)


df_implication_communaute<-df3%>%group_by(implication_communaute)%>%summarise(Effectif=n())%>%mutate(`Pourcentage%` =round (((Effectif/102)*100),2))
Total <- df_implication_communaute %>%
  summarize(across(where(is.numeric), sum, na.rm = TRUE)) %>%
  mutate(implication_communaute = "Total")
df_implication_communaute <- bind_rows(df_implication_communaute, Total)

#df_sexe_espacevisite_plus<-df3%>%group_by(espace_visite,sexe)%>%summarize(Count=n(),.groups = 'drop')%>%pivot_wider(names_from =sexe , values_from = Count, values_fill = list(Count = 0))
labels(df3)


#------------------------Interpretation--------------
#------------------------Graphique------------------------------------------------------------------------------------

library(ggplot2)

ggplot(data = df_visiten, aes(x =Effectif, y = reorder(espace_visite, Effectif), fill = espace_visite)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Effectif), position = position_stack(vjust = 0.5), color = "black") +
  labs(
    y = "Espace visite",
    x = "Effectifs",
    title = "Graphique1",
    subtitle = "Representation graphique des espace vert les plus visite a Parakou ",
    caption = "Source : Enquete sur les habitant a parakou"
    
  )

ggplot(data = df_age, aes(x = `Pourcentage%`, y = reorder(age, Effectif), fill =factor(age))) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = `Pourcentage%`), position = position_stack(vjust = 0.5), color = "black") +
  labs(
    y = "Age",
    x = "Pourcentage %",
    title = "Graphique2",
    subtitle = "Repartition de par age des participants a l'enquete",
    caption = "Source : Enquete sur les habitant a parakou"
    
  )




#------------------------Interpretation--------------
#------------------------Test statiatique----------------------------------------------------------------------

correlationtest<-cor.test(df3$espace_visiter, df3$frequence_visiter,
         method = c("pearson", "kendall", "spearman"),
         exact = NULL, conf.level = 0.95, continuity = FALSE)
correlationTest(df3$espace_visiter,df3$frequence_visiter, "kendall")
cor.test (df3$espace_visiter,df3$frequence_visiter)


# Modèle de régression logistique multinomiale
#model <- multinom(espace_visite ~ frequence_visite + activite_generale, data = df3)
#summary(model)

#model <- multinom(espace_visite ~ frequence_visiter, data = df3)

#new_data <- data.frame(
#espace_visite<-df3$espace_visite,frequence_visiter<-df3$frequence_visiter
#)

#predictions_new <- predict(model, newdata = new_data)
#predictions_new
#------------------------Interpretation--------------
