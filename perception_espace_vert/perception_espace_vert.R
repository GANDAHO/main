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



Cartographie_et_perception_des_services_écosystémiques_all_versions_labels_2024_07_08_14_45_00_1_ <- read_excel("H:/Mon Drive/Documents/HP ELITEBOOK/R/TRAVAUX-R/data/perception_espace_vert/Cartographie_et_perception_des_services_écosystémiques_-_all_versions_-_labels_-_2024-07-08-14-45-00 (1).xlsx")
df<-Cartographie_et_perception_des_services_écosystémiques_all_versions_labels_2024_07_08_14_45_00_1_
df2<-df%>%select(-1,-2,-3,-5,-6,-58,-59,-60,-61,-62,-63,-64,-65,-66,-67,-68,-69,-70)





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

# Supprimer les colonnes qui sont complètement vides
df4 <- df3[, colSums(is.na(df3)) < nrow(df3)]
# Supprimer les lignes qui sont complètement vides
df4<- df4[rowSums(is.na(df4)) < ncol(df4), ]

#analyse descriptif

df2<-df2%>%rename(sexe=`Quel est votre sexe ?`,age=`Quel est votre âge ?`,niveduc =`Quel est votre niveau d'éducation ?`,profession = `Quelle est votre profession ?`,freqvisite=`À quelle fréquence visitez-vous les espaces verts de Parakou ?`,raisonviste=`Quelle est la raison qui vous empêche de visiter les espaces verts de Parakou ?`,
                  espacvisite_plus=`Quel(s) espace(s) vert(s) visitez-vous le plus souvent ? (Nom ou description)`,
                  activpratiq=`Quelles activités pratiquez-vous généralement dans les espaces verts ?`)

df_frqplus<-df2%>%group_by(freqvisite)%>%summarise(effectif=n())
df_age<-df3%>%group_by(age)%>%summarise(effectif=n())
df_frqvisit<-df3%>%group_by(frequence_visite)%>%summarise(effectif=n())

df_raison<-df2%>%group_by(raisonviste)%>%summarise(effectif=n())
df_frqplus_sexe<-df2%>%group_by(freqvisite,sexe)%>%summarize(Count=n(),.groups = 'drop')%>%pivot_wider(names_from =sexe , values_from = Count, values_fill = list(Count = 0))
df_frqplus_niveduc<-df2%>%group_by(freqvisite,niveduc)%>%summarize(Count=n(),.groups = 'drop')%>%pivot_wider(names_from =niveduc , values_from = Count, values_fill = list(Count = 0))
df_frqplus_espacevisite_plus<-df3%>%group_by(frequence_visite,espace_visite)%>%summarize(Count=n(),.groups = 'drop')%>%pivot_wider(names_from =espace_visite , values_from = Count, values_fill = list(Count = 0))
df_niveduc<-df2%>%group_by(niveduc)%>%summarise(effectif=n())

df3$espace_visite <-factor(df3$espace_visite, labels=c("BIO GUERRA","BIO GUERRA/PAPINI","BIO GUERRA/PAPINI","BIO GUERRA","COTEB","Carrefour al houda","Devant Kognonsa","Devant confort line","FORET","COTEB","FORET/COTEB","FORET","HUBERT MAGA","HUBERT MAGA","HUBERT MAGA/COTEB","Devant confort line","PAPINI","HUBERT MAGA","PAPINI","BIO GUERRA/PAPINI","BIO GUERRA/PAPINI","COTEB","COTEB","COTEB","COTEB/FORET","HUBERT MAGA/COTEB","HUBERT MAGA/COTEB","COTEB/FORET"
))
df3$sexer<-as.numeric(factor(df3$sexe,labels = c(0,1)))
df3$ager<-as.numeric(factor(df3$age,labels = c(1,2,3,0)))
df3$frequence_visiter<-as.numeric(factor(df3$frequence_visite,labels = c(0,1,2,3,4,5)))
df3$espace_visiter<-as.numeric(factor(df3$espace_visite,labels = c(0,1,2,3,4,5,6,7,8,9,10,11)))
df3visite<-df3%>%group_by(espace_visite)%>%summarise(Effectif=n())

library(ggplot2)

ggplot(data = df3visite, aes(x = Effectif, y = reorder(espace_visite, Effectif), fill = espace_visite)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Effectif), position = position_stack(vjust = 0.5), color = "black") +
  labs(
    y = "Espace visite",
    x = "Effectifs",
    title = "Representation des lieux visite",
    subtitle = "Enquete sur la perception des espace vers a parakou",
    
  )

ggplot(data = df_age, aes(x = effectif, y = reorder(age, effectif), fill = age)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = effectif), position = position_stack(vjust = 0.5), color = "black") +
  labs(
    y = "age",
    x = "effectifs",
    title = "effectifs par age des participant",
    subtitle = "Enquete sur la perception des espace vers a parakou",
    
  )





df_sexe_espacevisite_plus<-df2%>%group_by(espacvisite_plus,sexe)%>%summarize(Count=n(),.groups = 'drop')%>%pivot_wider(names_from =sexe , values_from = Count, values_fill = list(Count = 0))

describe(df3$profession)

labels(df3)
correlationtest<-cor.test(df3$espace_visiter, df3$frequence_visiter,
         method = c("pearson", "kendall", "spearman"),
         exact = NULL, conf.level = 0.95, continuity = FALSE)
correlationTest(df3$espace_visiter,df3$frequence_visiter, "kendall")




