#Liberías
library(ggplot2)
library(psych)
library(rgdal)
library(stringr)

#Cargamos ficheros
load("municipios_final.RData")
load("municipios_jaen.RData")
load("municipios_sierra_magina.RData")
load("municipios_jaen_sin_sierra_magina.RData")
load("proyeccion_poblacion.RData")

#Extraemos los municipios andaluces con aproximadamente los mismos habitantes que Jódar
ciudades= municipios[(municipios$`Población Total`>=9000&municipios$`Población Total`<=15000),]
ciudades = ciudades[!is.na(ciudades$provincia),]


ciudades_jaen= jaen[(jaen$`Población Total`>5000&jaen$`Población Total`<15000),]

#**************************TURISMO***************************
#alojamientos turisticos por provincia ciudades 9000 - 15000
ggplot(ciudades,aes(ciudades$Total_alojamientos,ciudades$`Población Total`))+
  geom_point(aes(color=ciudades$provincia),size=6,alpha=0.6)+
  scale_color_manual(values=c("red","black","yellow","gray","purple","green","blue","orange"))+
  labs(title="Andalucía", x="Total de alojamientos", y="Población", color="Provincia" )


#alojamientos turisticos por habitantes provincia de Jaén 5000 a 15000 habitantes
ggplot(ciudades_jaen,aes(ciudades_jaen$Total_alojamientos,ciudades_jaen$`Población Total`))+
  geom_point(aes(color=ciudades_jaen$Territorio),size=6,alpha=0.6)+
  labs(title="Municipios Jaén (5000 y 15000 habitantes)", x="Total de alojamientos", y="Población", color="Provincia" )

boxplot(jaen_sin_sierra_magina$Total_alojamientos, sierra_magina$Total_alojamientos,
        names=c("Total alojamientos turísticos Jaén", "Total alojamientos turísticos Sierra Mágina"))

#Resumén estadístico municipios de Jaén entre 5000 y 15000 habitantes
describe(ciudades_jaen$Total_alojamientos)



#*************************************************************

#********************** POBLACIÖN ****************************

#Mapa densidad de población Andalucía
andalucia.r = municipios[,5]
distancia = dist(andalucia.r,method="euclidean")
agrupacion = hclust(distancia,method="ward.D")
plot(agrupacion,hang=-1)
grupo = cutree(agrupacion,5)
migrupo = cbind(municipios[,c(1:3)],grupo)
mapa = readOGR("mapas", "da02_term_munic", encoding= "UTF-8", use_iconv =TRUE)
migrupo$CODIGO_INE1 = str_pad(migrupo$CODIGO_INE1,5,"left","0")
migrupo$grupo = as.factor(migrupo$grupo)
dibujo = merge(mapa,migrupo,by.x="COD_MUN", by.y="CODIGO_INE1")
spplot(dibujo,"grupo")

#Resumen estadistico de la densidad de población en la provincia de Jaén
describe(jaen$`Densidad de población`)

#Total emigraciones territorio Español por población en Jódar
jodar = municipios[municipios$Territorio=="Jódar",]
jodarmigraciones=jodar$Balance_migraciones/jodar$`Población Total`*100

#Total emigraciones territorio Español por población en Jaén

jaen3 = jaen[!is.na(jaen$Balance_migraciones),]
sumamigraciones=sum(jaen3$Balance_migraciones)
suma_poblacionjaen=sum(jaen3$`Población Total`)
jaenmigraciones=sumamigraciones/suma_poblacionjaen*100

#Total emigraciones territorio Español por población en Sierra Mágina

sierra_magina3 = sierra_magina[!is.na(sierra_magina$Balance_migraciones),]
sumamigraciones=sum(sierra_magina3$Balance_migraciones)
suma_poblacionjaen=sum(sierra_magina3$`Población Total`)
sierra_maginamigraciones=sumamigraciones/suma_poblacionjaen*100

#Total emigraciones territorio Español por población en Andalucía
municipios3 = municipios[!is.na(municipios$Balance_migraciones),]
sumamigraciones=sum(municipios3$Balance_migraciones)
suma_poblacionjaen=sum(municipios3$`Población Total`)
andaluciamigraciones=sumamigraciones/suma_poblacionjaen*100

emigraciones_nacionales=data.frame(jodarmigraciones,sierra_maginamigraciones,jaenmigraciones,andaluciamigraciones)

emigraciones_nacionales
#********************** EMPLEO ******************************

#Mapa Demandantes de empleo por eventualidad agraria por población
andalucia2 = municipios[!is.na(municipios$`Demandantes Total`),]
andalucia.r = andalucia2[,18]/andalucia2$`Población Total`*100
distancia = dist(andalucia.r,method="euclidean")
agrupacion = hclust(distancia,method="ward.D")
plot(agrupacion,hang=-1)
grupo = cutree(agrupacion,3)
migrupo = cbind(andalucia2[,c(1:3)],grupo)
mapa = readOGR("mapas", "da02_term_munic", encoding= "UTF-8", use_iconv =TRUE)
migrupo$CODIGO_INE1 = str_pad(migrupo$CODIGO_INE1,5,"left","0")
migrupo$grupo = as.factor(migrupo$grupo)
dibujo = merge(mapa,migrupo,by.x="COD_MUN", by.y="CODIGO_INE1")
spplot(dibujo,"grupo")


#Demandantes de empleo por trabajos eventuales agrarios Jaén
jaen2 = jaen[!is.na(jaen$`Demandantes Total`),]
jaen.r = jaen2[,18]/jaen2$`Población Total`*100
describe (jaen.r)

#Demandantes de empleo por trabajos eventuales agrarios Jódar
jodar$`Demandantes Total`/jodar$`Población Total`*100

#Número de empresas por densidad de población en Sierra Mágina.
ggplot(sierra_magina,aes(sierra_magina$`Número de empresas`,sierra_magina$`Densidad de población`))+
  geom_point(aes(color=sierra_magina$Territorio),size=6,alpha=0.4)+
  labs(title="Sierra Mágina", x="Numero de Empresas", y="Densidad de Población", color="Municipio" )
describe(ciudades$`Número de empresas`)

#Tasa de empleo andalucia, jaen y comarca sierra mágina
boxplot(municipios$`Tasa de empleo Total`,jaen$`Tasa de empleo Total`,sierra_magina$`Tasa de empleo Total`,
        names=c("Tasa empleo Andalucía", "Tasa empleo Jaén", "tasa empleo Sierra Magina"))

#Mapa número declarantes 
andalucia2 = municipios[!is.na(municipios$`Número de declaraciones`),]
andalucia.r = andalucia2[,20]/andalucia2$`Población Total`*100
distancia = dist(andalucia.r,method="euclidean")
agrupacion = hclust(distancia,method="ward.D")
plot(agrupacion,hang=-1)
grupo = cutree(agrupacion,4)
migrupo = cbind(andalucia2[,c(1:3)],grupo)
mapa = readOGR("mapas", "da02_term_munic", encoding= "UTF-8", use_iconv =TRUE)
migrupo$CODIGO_INE1 = str_pad(migrupo$CODIGO_INE1,5,"left","0")
migrupo$grupo = as.factor(migrupo$grupo)
dibujo = merge(mapa,migrupo,by.x="COD_MUN", by.y="CODIGO_INE1")
spplot(dibujo,"grupo")

jodar$`Número de declaraciones`/jodar$`Población Total`*100
#*************************************************************

#************************Correlación**************************

#Analisis Correlación de las variables a nivel autonómico y local
r = cor(municipios[,-c(1:3,7:8,10:11,13:14,16:17)], use="pair")
corPlot(r,xlas=2)

r2 = cor(sierra_magina[,-c(1:2,6:7,9:10,12:13,15:16)], use="pair")
corPlot(r2,xlas=2)

#**************************************************************