dades_original <- read.csv('C:/Users/ricar/Desktop/Master/tipologia i cicle de vida de les dades/modul2/resultats_arreglo.csv', header=TRUE, sep=";")
library(data.table)
dades <- dades_original
colnames(dades)
setnames(dades, "Descripción", "Descripcion")
dim(dades)
head(dades,5)
summary(dades)
str(dades)

dades_final<- select(dades, Atleta, Descripcion, Fecha , Marca , F.Nacimiento)

dades_final$Marca<-gsub(" A","",dades_final$Marca)
#dades_final$Marca<-as.numeric(dades_final$Marca)

# En aquest punt creariem una funció per tal d'executar el mateix codi per cada prova, ara només l'¡immdico ja #que'indiquem ja que volem veure els detalls de les variables per separat. 
#result_prova<-function(prova){
dades_final_masc<- filter(dades_final, (Descripcion == '100m MASC. AL'))
dades_final_masc$anys<- as.integer(format((as.Date(dades_final_masc$F.Nacimiento,"%d/%m/%y")),"%Y"))
year_marca<-as.integer(format((as.Date(dades_final_masc$Fecha,"%d/%m/%y")),"%Y"))


dades_final_masc$anys<- year_marca - dades_final_masc$anys
masc_ord_atl <- with(dades_final_masc, dades_final_masc[order(Atleta, decreasing=FALSE), ])
dades_final_masc$anys = ifelse(dades_final_masc$anys < 0, dades_final_masc$anys+100, dades_final_masc$anys)
# Afegim una columne per calcular la millora sigui positiva o negativa 
dades_final_masc <- cbind(dades_final_masc, millora=NA)
dades_final_masc <- cbind(dades_final_masc, marcaAnt=NA)
dades_final_masc$Fecha_f <- as.Date(dades_final_masc$Fecha, "%d/%m/%y")
results_atl_final_m<-dades_final_masc
results_atl_final_m = results_atl_final_m[FALSE,]

#Dades agrupades per atleta
n_atletes <- group_by(dades_final_masc, Atleta)
result=summarise(n_atletes,num = n())

#  Ara mirem els seus resultats
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

a<-results_atl_final_m[results_atl_final_m$millora != 'NA',]


boxplot(a$millora ~ a$anys,
main="Millora segons edat sexe masculí",
xlab="Edat",
ylab="millora", col='red'
)

library(rio)
#export(results_atl_final_m, "./result_final_m.csv")
write.table(results_atl_final_m, "result_final_m_v1.csv", sep=";", dec=".", col.names=TRUE)

a$Marca<-as.numeric(a$Marca)

a$millora<-as.numeric(a$millora)

millora_plot<- select(a, millora, Marca, marcaAnt, anys)
plot(millora_plot)

b<-a
b<-b[!is.na(b$Atleta),]
summary(b)
shapiro.test(b$Marca)
shapiro.test(b$millora)
b<- select(b, millora, Marca, marcaAnt, anys)
cor(b,method="spearman")
cor.test(b$millora,b$Marca,method="spearman")
cor.test(b$millora,b$marcaAnt,method="spearman")
cor.test(b$millora,b$anys,method="spearman")
cor.test(b$marcaAnt,b$Marca,method="spearman")
millora_plot<- select(b, millora, Marca, marcaAnt, anys)
plot(millora_plot,pch='.')
plot(millora_plot$anys, millora_plot$millora, pch=4)


dades<-b
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
millora_plot<- select(c, millora, marcaAnt, anys)
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


plot(millora_plot$marcaAnt, millora_plot$millora, pch=4)
smoothingSpline = smooth.spline(millora_plot$marcaAnt, millora_plot$millora, spar=0.7)
lines(smoothingSpline, col='red', lwd=3)
boxplot(b$millora)


