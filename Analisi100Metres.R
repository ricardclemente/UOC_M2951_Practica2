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
  #year_act<-as.integer(format(Sys.Date(),"%Y"))

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

#}



#*****************
# Repeteixo el codi per tal de veure totes les dades a l'execució, però això s'hauria de fer la funció i passar el tious # de prova --> 
# result_prova('100m MASC. AL')
# result_prova('100m FEM. AL')  
#*****************  
  dades_final_fem<- (filter(dades_final, (Descripcion == '100m FEM. AL')))
  dades_final_fem$anys<- as.integer(format((as.Date(dades_final_fem$F.Nacimiento,"%d/%m/%y")),"%Y"))
  year_act<-as.integer(format(Sys.Date(),"%Y"))
  dades_final_fem$anys <- year_act - dades_final_fem$anys
  dades_final_fem$anys = ifelse(dades_final_fem$anys < 0, dades_final_fem$anys+100, dades_final_fem$anys)
  dades_final_fem <- cbind(dades_final_fem, millora=0)
  dades_final_fem$Fecha_f <- as.Date(dades_final_fem$Fecha, "%d/%m/%Y")
  results_atl_final_f<-dades_final_fem
  results_atl_final_f = results_atl_final_f[FALSE,]
  
  #Dades agrupades per atleta
  
  n_atletes <- group_by(dades_final_fem, Atleta)
  result=summarise(n_atletes,num = n())

#  Ara mirem els seus resultats
  
  total=dim(result)[1]
  for (i in (1:total)) { 
    atleta_act <- result[[i,1]] 
    results_atl_act <- filter(dades_final_fem, (Atleta == atleta_act))
  # Ordenem per data de la prova  
    results_atl_act=arrange(results_atl_act, desc(results_atl_act$Fecha_f))
    total_sub=(dim(results_atl_act)[1])
    #  j=1
    if (total_sub >1) {
      for (j in (1:(total_sub - 1))) {  
        results_atl_act[j,7] <-as.numeric(as.character(results_atl_act[j,4])) -as.numeric(as.character(results_atl_act[(j+1),4]))
      }
    }
   # Adjuntem el subset als resultats finals
   results_atl_final_f=rbind(results_atl_final_f,results_atl_act)
  }
  
boxplot(results_atl_final_f$millora ~ results_atl_final_f$anys,
main="Millora segons edat sexe femeni",
xlab="Edat",
ylab="millora",col="blue"
)

a<-results_atl_final_m[results_atl_final_m$millora != 'NA',]

boxplot(a$millora ~ a$anys,
main="Millora segons edat sexe masculí",
xlab="Edat",
ylab="millora", col='red'
)




library(rio)
#export(results_atl_final_m, "./result_final_m.csv")
write.table(results_atl_final_m, "result_final_m_v1.csv", sep=";", dec=".", col.names=TRUE)
write.table(results_atl_final_f, "result_final_f_v1.csv", sep=";", dec=".", col.names=TRUE)
fix(dades_final_masc)

a$Marca<-as.numeric(a$Marca)
shapiro.test(a$Marca)

a$millora<-as.numeric(a$millora)
shapiro.test(a$millora)

cor.test(a$millora,a$Marca,method="spearman")
cor.test(a$millora,a$anys,method="spearman")

millora_plot<- select(a, millora, Marca, marcaAnt, anys)
plot(millora_plot)



