# Cargamos las librerías que vamos a necesitar para nuestra práctica
library(readxl)


# Lectura de los ficheros SIMA
densidad_poblacion = read_excel("densidad_poblacion.xlsx")
emigraciones_exteriores_sexo = read_excel("emigraciones_exteriores_sexo.xlsx")
emigraciones_interiores_sexo = read_excel("emigraciones_interiores.xlsx")
emigraciones_sexo = read_excel("emigraciones_sexo.xlsx")
aloja_turisticos = read_excel("establecimientos_turist_rurales.xlsx")
hoteles = read_excel("hoteles_segun_clase.xlsx")
proyeccion_poblacion = read_excel("indices_proyeccion_poblacion.xlsx")
num_empresas = read_excel("num_empresas.xlsx")
poblacion_edad = read_excel("poblacion_edad_sexo.xlsx")
poblacion_sexo = read_excel("poblacion_sexo.xlsx")
tasa_empleo = read_excel("tasa_empleo_sexo.xlsx")
paro = read_excel("tasa_paro_sexo.xlsx")
TEAS = read_excel("trabajadores_eventuales_agrarios_subsidios_sexo.xlsx")
extension = read_excel("extension.xlsx")
declaraciones_renta = read_excel("declaraciones_renta.xlsx")
inmigraciones_interiores = read_excel("inmigraciones_interiores.xlsx")
inmigraciones_exteriores = read_excel("inmigraciones_exteriores.xlsx")

#Cotejamos ambas variables modificando/eliminando las columnas que no nos aportan informacion o no son coincidentes 
aloja_turisticos= aloja_turisticos[,-2]
emigraciones_exteriores_sexo= emigraciones_exteriores_sexo[,-(2:3)]
emigraciones_interiores_sexo= emigraciones_interiores_sexo[,-(2:3)]
emigraciones_sexo = emigraciones_sexo[,-(2:3)]
hoteles = hoteles[,-2]
num_empresas = num_empresas[,-(2:3)]
poblacion_edad = poblacion_edad[,-2]
poblacion_sexo = poblacion_sexo[,-2]
TEAS = TEAS[,-2]
extension = extension[,-2]
declaraciones_renta = declaraciones_renta[,-1]
declaraciones_renta = declaraciones_renta[declaraciones_renta$CODIGO_INE1>999,]
inmigraciones_exteriores = inmigraciones_exteriores[,-c(1:3,5:6)]
inmigraciones_interiores = inmigraciones_interiores[,-c(1:3,5:6)]


#Generamos el objeto
emigraciones = merge (emigraciones_exteriores_sexo,emigraciones_interiores_sexo)
emigraciones =  emigraciones[,-c(1,3:4,6:7)]
emigraciones$TotalEmigracionesEspaña = rowSums(emigraciones[,2:3],na.rm=T)
inmigraciones = merge (inmigraciones_exteriores, inmigraciones_interiores)
inmigraciones$TotalInmigracionesEspaña = rowSums(inmigraciones[2:3], na.rm=T)*-1
migraciones = merge(emigraciones, inmigraciones)
migraciones$Balance_migraciones = rowSums(migraciones[,c(4,7)])
migraciones = migraciones [,-(2:7)]
municipios = merge(aloja_turisticos,hoteles, all=T)
municipios$Total_alojamientos=rowSums(municipios[,3:7],na.rm=T)
municipios = municipios [,-(3:8)]
municipios = merge(municipios, densidad_poblacion, all=T)
municipios = merge (municipios,num_empresas, all=T)
municipios = merge (municipios,paro,all=T)
municipios = merge (municipios, poblacion_sexo, all=T)
municipios = merge (municipios, tasa_empleo, all=T)
municipios = merge (municipios, TEAS, all=T)
municipios = merge (municipios, extension, all=T)
municipios = merge (municipios, declaraciones_renta, all=T)
municipios = merge(municipios,migraciones, all=T)

#Eliminamos los municipios que son provincia (sin codigo ine)
municipios = municipios[!is.na(municipios$CODIGO_INE1),]
municipios = municipios[!is.na(municipios$`Densidad de población`),]

#Vamos a asignar las provincias a los municipios 
provincia = cut (municipios$CODIGO_INE1, c(4000,4999,11999,14999,18999,21999,23999,29999,41999))
levels(provincia) = c("Almería", "Cadiz","Córdoba", "Granada", "Huelva", "Jaén", "Málaga", "Sevilla")

municipios = cbind(provincia,municipios)
save(municipios,file="municipios_final.RData")

#Cargamos los datos de Jaén
jaen = municipios[municipios$provincia=="Jaén",]
jaen = jaen [,-1]
save (jaen, file="municipios_jaen.RData")

#Cargamos los datos de la comarca de sierra magina
sierra_magina = jaen[jaen$Territorio %in% c("Albanchez de Mágina"
                                            ,"Bedmar y Garcíez",
                                            "Bélmez de la Moraleda",
                                            "Cabra del Santo Cristo",
                                            "Cambil",
                                            "Campillo de Arenas",
                                            "Cárcheles",	
                                            "Huelma",
                                            "Jimena",
                                            "Jódar",
                                            "Larva",
                                            "Noalejo",
                                            "Pegalajar",
                                            "Torres",
                                            "La Guardia",
                                            "Mancha Real"),]

save (sierra_magina, file="municipios_sierra_magina.RData")

#Cargamos los datos de la provincia de Jaén excluyendo a Sierra Mágina

jaen_sin_sierra_magina = jaen[!(jaen$Territorio  %in% c("Albanchez de Mágina"
                                                        ,"Bedmar y Garcíez",
                                                        "Bélmez de la Moraleda",
                                                        "Cabra del Santo Cristo",
                                                        "Cambil",
                                                        "Campillo de Arenas",
                                                        "Cárcheles",	
                                                        "Huelma",
                                                        "Jimena",
                                                        "Jódar",
                                                        "Larva",
                                                        "Noalejo",
                                                        "Pegalajar",
                                                        "Torres",
                                                        "La Guardia",
                                                        "Mancha Real")),]


save (jaen_sin_sierra_magina, file="municipios_jaen_sin_sierra_magina.RData")

save (proyeccion_poblacion, file="proyeccion_poblacion.RData")

rm(municipios)
rm(aloja_turisticos)
rm(emigraciones_exteriores_sexo)
rm(emigraciones_interiores_sexo)
rm(emigraciones_sexo)
rm(hoteles)
rm(num_empresas)
rm(paro)
rm(poblacion_edad)
rm(poblacion_sexo)
rm(proyeccion_poblacion)
rm(tasa_empleo)
rm(TEAS)
rm(densidad_poblacion)
rm(provincia)
rm(extension)
rm(municipios2)
rm(alojamientos)
rm(n_empresas)
rm(emigraciones)
rm(jaen)
rm(declaraciones_renta)
rm(sierra_magina)
rm(jaen_sin_sierra_magina)
rm(migraciones)
rm(inmigraciones)
rm(inmigraciones_exteriores)
rm(inmigraciones_interiores)
