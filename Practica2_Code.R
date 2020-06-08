
knitr::opts_chunk$set(echo = TRUE)

library(dplyr)
library(knitr)
library(tidyverse)
library(kableExtra)
library(varhandle)
library(ggplot2)
library(gplots)
library(gridExtra)
require(psych)
library(data.table)
library(rio)

dades_original <- read.csv('C:/Users/nuria/Documents/4. M2.951 - Tipologia i cicle de vida de les dades/Practica2/resultats_arreglo.csv', header=TRUE, sep=";")
#dades_original <- read.csv('C:/Users/ricar/Desktop/Master/tipologia i cicle de vida de les dades/modul2/resultats_arreglo.csv', header=TRUE, sep=";")

dades <- dades_original
colnames(dades)[3] <- "Descripcion"

# informació general de les dades
dim(dades)
head(dades,5)
summary(dades)
str(dades)

# Selecció de les dades que utilitzarem en la nostra pràctica
dades_final<- select(dades, Atleta, Descripcion, Fecha , Marca , F.Nacimiento)
# cal extreure el valor afegit ' A' en la variable marca
dades_final$Marca<-gsub(" A","",dades_final$Marca)

# Ens centrarem en una competició en concret [100m MASC. AL]
dades_final_masc<- filter(dades_final, (Descripcion == '100m MASC. AL'))

# Calculem l'edat de l'atleta
dades_final_masc$anys<- as.integer(format((as.Date(dades_final_masc$F.Nacimiento,"%d/%m/%y")),"%Y"))
year_marca<-as.integer(format((as.Date(dades_final_masc$Fecha,"%d/%m/%y")),"%Y"))
dades_final_masc$anys<- year_marca - dades_final_masc$anys

# Preparem les dades per fer el càlcul de les millores (sigui positiva o negativa )
masc_ord_atl <- with(dades_final_masc, dades_final_masc[order(Atleta, decreasing=FALSE), ])
dades_final_masc$anys = ifelse(dades_final_masc$anys < 0, dades_final_masc$anys+100, dades_final_masc$anys)
# Afegim una columne per calcular la millora sigui positiva o negativa i una informativa per indicar la marca anterior 
dades_final_masc <- cbind(dades_final_masc, millora=NA)
dades_final_masc <- cbind(dades_final_masc, marcaAnt=NA)
dades_final_masc$Fecha_f <- as.Date(dades_final_masc$Fecha, "%d/%m/%y")
results_atl_final_m<-dades_final_masc
results_atl_final_m = results_atl_final_m[FALSE,]

# Dades agrupades per atleta
n_atletes <- group_by(dades_final_masc, Atleta)
result=summarise(n_atletes,num = n())

# Ara mirem els seus resultats
total=dim(result)[1]

for (i in (1:total)) { 
  atleta_act <- result[[i,1]] 
  results_atl_act <- filter(dades_final_masc, (Atleta == atleta_act))
# Ordenem per data de la prova  
  results_atl_act=arrange(results_atl_act, desc(results_atl_act$Fecha_f))
  total_sub=(dim(results_atl_act)[1])
  if (total_sub >1) {
    for (j in (1:(total_sub - 1))) {  
      results_atl_act[j,7] <-as.numeric(as.character(results_atl_act[j,4])) -as.numeric(as.character(results_atl_act[(j+1),4]))
	  results_atl_act[j,8] <-as.numeric(as.character(results_atl_act[(j+1),4]))
    }
  }
# Adjuntem el subset als resultats finals
results_atl_final_m=rbind(results_atl_final_m,results_atl_act)
}

# A partir dels resultats analitzem amb un boxplot els valors que poden ser atípics
# Ens quedem només amb els registres on hi ha informació de millora (positiva o negativa)
results_atl_final_m_amb_millores<-results_atl_final_m[results_atl_final_m$millora != 'NA',]

boxplot(results_atl_final_m_amb_millores$millora ~ results_atl_final_m_amb_millores$anys,
main="Millora segons edat sexe masculí",
xlab="Edat",
ylab="millora", col='red'
)

boxplot( results_atl_final_m_amb_millores$anys,
main="Millora segons edat sexe masculí",
xlab="Edat",
ylab="millora", col='red'
)

# més detall de outliers
impute_outliers <- function(x, removeNA = TRUE){
  quantiles <- quantile(x, c(0.30, 0.70), na.rm = removeNA)
  print (quantiles)
  x[x<quantiles[1]] <- mean(x, na.rm = removeNA)
  x[x>quantiles[2]] <- median(x, na.rm = removeNA)
  x
}

results_atl_final_m_amb_millores_v2<-results_atl_final_m_amb_millores[-which(is.na(results_atl_final_m_amb_millores$anys)),]
imputed_data <- impute_outliers(results_atl_final_m_amb_millores_v2$anys)

result_final<-results_atl_final_m_amb_millores_v2[!is.na(results_atl_final_m_amb_millores_v2$Atleta),]

par(mfrow = c(1,2))
result_final<-boxplot(result_final$anys, main = "Global",col = 2)
result_imputed<-boxplot(imputed_data, main = "Sense outliers",col=3)

result_final$stats
result_final$conf
result_final$n
result_final$out

result_imputed$stats
result_imputed$conf
result_imputed$n
result_imputed$out

# podem extreure les dades que hem tractat
write.table(results_atl_final_m, "result_final_m_v1.csv", sep=";", dec=".", col.names=TRUE)


results_atl_final_m_amb_millores$Marca<-as.numeric(results_atl_final_m_amb_millores$Marca)
results_atl_final_m_amb_millores$millora<-as.numeric(results_atl_final_m_amb_millores$millora)

millora_plot<- select(results_atl_final_m_amb_millores, millora, Marca, marcaAnt, anys)
plot(millora_plot)

# Mirem les correlacions
results_atl_final_m_amb_millores<-results_atl_final_m_amb_millores[!is.na(results_atl_final_m_amb_millores$Atleta),]
summary(results_atl_final_m_amb_millores)
shapiro.test(results_atl_final_m_amb_millores$Marca)
shapiro.test(results_atl_final_m_amb_millores$millora)
results_atl_final_m_amb_millores<- select(results_atl_final_m_amb_millores, millora, Marca, marcaAnt, anys)
cor(results_atl_final_m_amb_millores,method="spearman")
cor.test(results_atl_final_m_amb_millores$millora,results_atl_final_m_amb_millores$Marca,method="spearman")
cor.test(results_atl_final_m_amb_millores$millora,results_atl_final_m_amb_millores$marcaAnt,method="spearman")
cor.test(results_atl_final_m_amb_millores$millora,results_atl_final_m_amb_millores$anys,method="spearman")
cor.test(results_atl_final_m_amb_millores$marcaAnt,results_atl_final_m_amb_millores$Marca,method="spearman")
millora_plot<- select(results_atl_final_m_amb_millores, millora, Marca, marcaAnt, anys)
plot(millora_plot,pch='.')
plot(millora_plot$anys, millora_plot$millora, pch=4)

# Per facilitar la lectura renombrem la variable de les dades
dades<-results_atl_final_m_amb_millores
#mirem la normalitat
shapiro.test(dades$millora)
shapiro.test(dades$anys)
shapiro.test(dades$marcaAnt)
summary(dades)
#mirem la homogeneïtat
fligner.test(millora ~ anys, data = dades)
fligner.test(millora ~ marcaAnt, data = dades)
#tests estadistics
kruskal.test(millora ~ anys, data = dades)
kruskal.test(millora ~ marcaAnt, data = dades)
#millora_plot<- select(c, millora, marcaAnt, anys)
millora_plot<- select(dades, millora, marcaAnt, anys)
plot(millora_plot,pch='.')
ml=lm(millora ~ anys, data = dades)
summary(ml)
plot(millora ~ anys, data = dades, pch=4)
abline(ml, col="red", lwd=3)
ml=lm(millora ~ marcaAnt, data = dades)
summary(ml)
plot(millora ~ marcaAnt, data = dades, pch=4)
abline(ml, col="red", lwd=3)
ml=lm(formula = millora ~ anys+marcaAnt, data = dades)
summary(ml)

cor.test(dades$millora,dades$anys, method="spearman")
cor.test(dades$millora,dades$marcaAnt, method="spearman")






