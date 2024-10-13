setwd("L:/BUT/SD/Promo 2023/fbeck/2eme annee/r")
df=read.csv("dpe-v2-logements-existants.csv",header=TRUE)
View(df)
df2=read.csv("dpe-v2-logements-neufs.csv",header=TRUE)
df2$Période_construction="après 2021"
View(df2)
dim(df)
dim(df2)
df$Logement="Ancien"
df2$Logement="Neuf"
col_commune=intersect(colnames(df),colnames(df2))
df3=rbind(df[ , col_commune],df2[ , col_commune ])
class(df3$Date_réception_DPE)
df3$AneeDPE=substr(df3$Date_réception_DPE,1,4)
df3$AneeDPE
df3$Cout5=ifelse(df3$Coût_chauffage +df3$Coût_éclairage+df3$Coût_ECS+df3$Coût_refroidissement+df3$Coût_auxiliaires==df3$Coût_total_5_usages,TRUE,FALSE)
df3$CoutChauffageprct=df3$Coût_chauffage/df3$Coût_total_5_usages*100

by(df3,df3$Etiquette_DPE,nrow)
by(df3,df3$AneeDPE,nrow)
table(df3$Logement)
table(df3$Type_bâtiment)
#5 /
print(table(df3$Période_construction))
mean(df3$Surface_habitable_logement,na.rm = TRUE)
mean(df3$Coût_chauffage,na.rm = TRUE)
quantile(df3$Coût_ECS)
quantile(df3$Coût_ECS,probs = seq(0,1,0.1))
cor(df3$Surface_habitable_logement,df3$Coût_chauffage,use = "complete.obs")
plot(c(df3$Coût_total_5_usages,df3$Coût_chauffage,df3$Coût_éclairage,df3$Coût_ECS,df3$Coût_refroidissement,df3$Coût_auxiliaires,df3$Surface_habitable_logement,df3$Emission_GES_5_usages),ncol(c(df3$Coût_total_5_usages,df3$Coût_chauffage,df3$Coût_éclairage,df3$Coût_ECS,df3$Coût_refroidissement,df3$Coût_auxiliaires,df3$Surface_habitable_logement,df3$Emission_GES_5_usages)))


