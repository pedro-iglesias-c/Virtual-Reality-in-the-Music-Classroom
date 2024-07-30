#Carga librerías necesarias
library(readxl)
library(likert)
library(psych)
library(RColorBrewer)
library(ggplot2)

#Carga de datos
respuestas <- read_excel('Resultados_RV.xlsx')

#Transformacion de categorias tratamiento expiremental
respuestas$Metodologia<- as.factor(respuestas$Metodologia)
levels(respuestas$Metodologia)<-c("VR", "Desktop VR", "2D")

#Calculo de estadísticos descriptivos y tamaño de efecto

matriz_inmersion<- rbind(tapply(respuestas$inmersion, respuestas$Metodologia, mean), 
                         tapply(respuestas$inmersion, respuestas$Metodologia, sd))
matriz_atencion<-rbind(tapply(respuestas$atencion, respuestas$Metodologia, mean), 
                       tapply(respuestas$atencion, respuestas$Metodologia, sd))
matriz_interes<-rbind(tapply(respuestas$interes, respuestas$Metodologia, mean), 
                         tapply(respuestas$interes, respuestas$Metodologia, sd))

#funcion para calcular d de Cohen en grupos iguales

effect_size<-function (m1, m2, sd1, sd2){
  d_Cohen <- (m1-m2)/sqrt((sd1^2+sd2^2)/2)
  return(d_Cohen)
}
#función para calcular d Cohen en cada matriz
# retorna una tabla de contingencia en forma de matriz
tabla_eff<-function (m){
  mat_interna<-matrix(1:9, ncol=3, nrow=3)
  rownames(mat_interna)<-colnames(m)
  colnames(mat_interna)<-colnames(m)
  mat_interna[1,1]<-effect_size(m[1,1], m[1,1], m[2,1], m[2,1])
  mat_interna[1,2]<-effect_size(m[1,1], m[1,2], m[2,1], m[2,2])
  mat_interna[1,3]<-effect_size(m[1,1], m[1,3], m[2,1], m[2,3])
  
  mat_interna[2,1]<-effect_size(m[1,2], m[1,1], m[2,2], m[2,1])
  mat_interna[2,2]<-effect_size(m[1,2], m[1,2], m[2,2], m[2,2])
  mat_interna[2,3]<-effect_size(m[1,2], m[1,3], m[2,2], m[2,3])
  
  mat_interna[3,1]<-effect_size(m[1,3], m[1,1], m[2,3], m[2,1])
  mat_interna[3,2]<-effect_size(m[1,3], m[1,2], m[2,3], m[2,2])
  mat_interna[3,3]<-effect_size(m[1,3], m[1,3], m[2,3], m[2,3])
  print("coeficiente d Cohen para la matriz")
  return(mat_interna)
}

tabla_eff(matriz_inmersion)
tabla_eff(matriz_atencion)
tabla_eff(matriz_interes)

#Graficos de caja para respuestas correctas

ggplot(respuestas, aes(x=Metodologia, y=R_correctas, fill=Metodologia))+
  geom_boxplot()+
  labs(
    title = "Correct responses due to experimental treatment",
    subtitle= " ")+
  labs(x="Experimental treatment", y="Correct responses")+
  stat_boxplot(geom = "errorbar",
               width = 0.15)+
  stat_summary(fun.y=mean, geom="point", shape=8, size=3)+
  scale_fill_brewer(palette="Greys")+
  coord_flip()+
  theme_classic()+
  theme()
 
 

#Coeficiente Alfa de Cronbach (reliability, confiabilidad)

alpha(data.matrix(respuestas[,4:6]),  check.keys = TRUE)

kruskal.test(respuestas$inmersion~respuestas$Metodologia)
kruskal.test(respuestas$atencion~respuestas$Metodologia)
kruskal.test(respuestas$interes~respuestas$Metodologia)

#Compruebo que grupos tienen diferencia estadistica
#p.adjust.method = "bonferroni" da otro resultado
pairwise.wilcox.test(respuestas$inmersion, respuestas$Metodologia,
                     p.adjust.method = "BH")
pairwise.wilcox.test(respuestas$atencion, respuestas$Metodologia,
                     p.adjust.method = "BH")
pairwise.wilcox.test(respuestas$interes, respuestas$Metodologia,
                     p.adjust.method = "BH")

#correlacion de Pearson
cor.test(respuestas$R_correctas, respuestas$inmersion, 
         method = "pearson")
cor.test(respuestas$R_correctas, respuestas$atencion, 
         method = "pearson")
cor.test(respuestas$R_correctas, respuestas$interes, 
         method = "pearson")



#Creación de data frame con datos categoricos categoricos para graficos
respuestas_cat<-respuestas
respuestas_cat$inmersion<- factor(respuestas_cat$inmersion, 
                                  levels= c(1,2,3,4), 
                                  labels= c("Baja","Parcial","Casi Total","Total"))
respuestas_cat$atencion<- factor(respuestas_cat$atencion, 
                                  levels= c(1,2,3,4), 
                                  labels= c("Baja","Parcial","Casi Total","Total"))
respuestas_cat$interes<- factor(respuestas_cat$interes, 
                                  levels= c(1,2,3,4), 
                                  labels= c("Baja","Parcial","Casi Total","Total"))


#Renombro los nombres de las columnas del data.frame para graficar

colnames(respuestas_cat)[4]<-"1.Inmersión"
colnames(respuestas_cat)[5]<-"2.Atención"
colnames(respuestas_cat)[6]<-"3.Interés"




#Transformación en data frame, necesario para aplicar librería Likert
respuestas_likert<-likert(as.data.frame(respuestas_cat[,4:6]))
#Se aplica la función likert al data frame, agrupando or tratamiento experimental
likert_grupos<-likert(as.data.frame(respuestas_cat[,4:6]), grouping = respuestas_cat$Metodologia)
#Se grafican los resultados
plot(likert_grupos, group.order = c("RV","RV Escritorio","2D"), 
     centered=TRUE,  legend.position="right")  +
  scale_fill_manual(values = c("#bdbdbd","#969696","#525252", "#000000" ), #brewer.pal(n=4,"Greys"), 
                    breaks = c("Baja","Parcial","Casi Total","Total"))+
  guides(fill = guide_legend(title="Inmersión\nAtención\nInterés", 
                             reverse = TRUE))+
  labs(title='Motivación', y='Porcentaje')



