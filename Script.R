## llamar pacman (contiene la función p_load)
require(pacman)
require(caret)
## llamar y/o instalar librerias
p_load(tidyverse,rio,skimr,
       sf, ## datos espaciales
       leaflet, ## visualizaciones
       tmaptools, ## geocodificar
       ggsn, ## map scale bar 
       osmdata) ## packages with census data


library(kableExtra)
library(glmnet)
library(magrittr)
library("caret")

`%notin%` <- Negate(`%in%`)
pckgs <- c("tidyverse", "data.table", "rlang", "readxl")
if (any(pckgs %notin% rownames(installed.packages())==TRUE)){
  install.packages(pckgs, repos = c(CRAN = "http://cloud.r-project.org"))}
#https://github.com/LukaIgnjatovic/Historical_Weather_Data_Scraping/blob/master/Historical_Weather_Data_Scraping.ipynb

#######################################################################
########### Procesamiento de datos ####################################
#######################################################################
#######################################################################

rm(list=ls())

setwd("C:/Users/mrozo/OneDrive - Universidad de los Andes/Maestría/Big Data/final")



###########################
#Base de siniestros viales#
###########################
base <- read.csv2("C:/Users/mrozo/OneDrive - Universidad de los Andes/Maestría/Big Data/final/data/datos (1).txt")

colnames(base)[colnames(base) == "FECHA_OCUR"] ="fecha"

base <- subset(base, select = -c(FID,OBJECTID,FECHA_HORA))
base$fecha <- gsub(x=base$fecha,pattern=" 0:00:00",replacement="",fixed=T)
base$fecha = as.Date(base$fecha, format('%d/%m/%Y'))


######################################################
#Base de condiciones climáticas#######################
######################################################
clima<-read_csv(file="data/bogota.csv")
colnames(clima)[colnames(clima) == "date_time"] ="fecha"
clima$fecha = as.Date(clima$fecha, format('%Y/%m/%d'))

######################################################
#Unión de bases#######################################
######################################################
base_clima<-merge(base,clima,by="fecha")

#Mantener año 2021
base_clima <- subset(base_clima, ANO_OCURRE == 2021)


###########################
#Base de vehiculos#########
###########################
vehiculo<-read_csv(file="data/Siniestralidad.csv")


##########################################
#Unión de bases###########################
##########################################
base_clima_vehiculos<-merge(base_clima,vehiculo,by="FORMULARIO")
base_clima_vehiculos <- subset(base_clima_vehiculos, ANO_OCURRE == 2021)

base_clima_vehiculos <- subset(base_clima_vehiculos, select = -c(OBJECTID,location))
###########################
#Base de causas   #########
###########################

causas<-read_csv(file="data/Siniestralidad (2).csv")

##########################################
#Unión de bases###########################
##########################################

base_clima_vehiculos_causas<-merge(base_clima_vehiculos,causas,by="FORMULARIO")
base_clima_vehiculos_causas <- subset(base_clima_vehiculos_causas, ANO_OCURRE == 2021)

#######################################################################
########### Limpieza de datos      ####################################
#######################################################################
#######################################################################

base_clima_vehiculos_causas <- subset(base_clima_vehiculos_causas, select = -c(CODIGO.y,CODIGO_VEHICULO.y,OBJECTID))

cols <- c("LOCALIDAD","GRAVEDAD", "CLASE_ACC", "CODIGO_VEHICULO.x", "CLASE","SERVICIO","MODALIDAD","ENFUGA","CODIGO_CAUSA","TIPO","TIPO_CAUSA")

base_clima_vehiculos_causas %<>% mutate_at(cols, factor)

base_final<-base_clima_vehiculos_causas


#######################################################################
########### Cargar la base final  ####################################
#######################################################################
#######################################################################


data <- read_csv(file="base_final_proyecto.csv")


data <- st_as_sf(x = data, ## datos
                 coords=c("LONGITUD","LATITUD"), ## coordenadas
                 crs=4326) ## CRS


leaflet() %>% addTiles() %>% addCircleMarkers(data=data[1:50,])



#######################################################################
########### Cargar info de camaras  #################################
#######################################################################
#######################################################################

library(rgdal)
camaras <- readOGR(dsn = "D:/noveno semestre/big data/datos/espacial/camara salva vidas", layer = "Camaras_Salvavidas_Bogota", GDAL1_integer64_policy = TRUE)
camaras <- spTransform(camaras, CRS("+proj=longlat +datum=WGS84 +no_defs"))
camaras = st_as_sf(camaras)



## calcular la distancia a la camara salvavidas más cercana ####
matrix_dist_cam<- st_distance(x=data , y=camaras)
min_dist_cam<- apply(matrix_dist_cam , 1 , min)
data$min_dist_cam = min_dist_cam


#######################################################################
########### Cargar info de semáforos  #################################
#######################################################################
#######################################################################

semaforo <- readOGR(dsn = "D:/noveno semestre/big data/datos/espacial/red semaforos", layer = "Red_Semaforica_de_Bogot%C3%A1_D_C", GDAL1_integer64_policy = TRUE)
semaforo <- spTransform(semaforo, CRS("+proj=longlat +datum=WGS84 +no_defs"))
semaforo = st_as_sf(semaforo)

## calcular la distancia al semáforo mas cercano ####
matrix_dist_sem<- st_distance(x=data , y=semaforo)
min_dist_sem<- apply(matrix_dist_sem , 1 , min)
data$min_dist_sem = min_dist_sem


################################################################
########### Paraderos de SITP  #################################
################################################################
################################################################

sitp <- readOGR(dsn = "D:/noveno semestre/big data/datos/espacial/paraderos Sitp", layer = "Paraderos_SITP_Bogot%C3%A1_D_C", GDAL1_integer64_policy = TRUE)
sitp <- spTransform(sitp, CRS("+proj=longlat +datum=WGS84 +no_defs"))
sitp = st_as_sf(sitp)

## calcular la distancia al paradero sitp mas cercano ####
matrix_dist_sitp<- st_distance(x=data , y=sitp)
min_dist_sitp<- apply(matrix_dist_sitp , 1 , min)
data$min_dist_sitp = min_dist_sitp



################################################################
########### Estado de la vía  #################################
################################################################
################################################################

### limpiar un poco la memoria 
rm(camaras, semaforo, sitp)
rm(matrix_dist_cam, matrix_dist_sem, matrix_dist_sitp)
gc()


vias <- readOGR(dsn = "D:/noveno semestre/big data/datos/espacial/priorización vial", layer = "ModeloPriorizacion", GDAL1_integer64_policy = TRUE)
vias <- spTransform(vias, CRS("+proj=longlat +datum=WGS84 +no_defs"))
vias = st_as_sf(vias)


## selecciono las columnas de interés 

vias = subset(vias, select = c(ANCHOCALZA, AREACALZAD,LONGITUDHO,PUNTAJE_IP, IP_RELATIV))


##HAGO UN SPATIAL JOINT CON LA INFO PROXIMA. 
sf_use_s2(FALSE)
data <- st_join(data, vias, join=st_nearest_feature)



################################################################
########### Rutas de Biciusuarios  #############################
################################################################
################################################################

bici <- readOGR(dsn = "D:/noveno semestre/big data/datos/espacial/red biciusuarios", layer = "RedBiciusuario", GDAL1_integer64_policy = TRUE)
bici <- spTransform(bici, CRS("+proj=longlat +datum=WGS84 +no_defs"))
bici = st_as_sf(bici)

sf_use_s2(TRUE)

## calcular la distancia a la bicirruta mas cercana ####
matrix_dist_bici<- st_distance(x=data , y=bici)
min_dist_bici<- apply(matrix_dist_bici , 1 , min)
data$min_dist_bici = min_dist_bici


################################################################
########### Información restaurantes  #############################
################################################################
################################################################


restaurante = opq(bbox = getbb("Bogotá Colombia")) %>%
  add_osm_feature(key="amenity" , value="restaurant") 

restaurante_sf = restaurante %>% osmdata_sf()

restaurante = restaurante_sf$osm_points %>% select(osm_id,amenity) 


matrix_dist_res <- st_distance(x=data , y=restaurante)
mean_dist_res <- apply(matrix_dist_res , 1 , mean)
data$mean_dist_res = mean_dist_res


#######################################################
### Exportar los datos  ###############################
#######################################################
library(tidyverse)

separated_coord <- data %>%
  mutate(long = unlist(map(data$geometry,1)),
         lat = unlist(map(data$geometry,2)))

separated_coord = subset(separated_coord, select = -c(geometry))

save(separated_coord,file="D:/noveno semestre/big data/trabajo final/final/datos_con_dist.Rds")

write.csv(test, file="base_final_proyecto (trpy).csv",row.names = FALSE)
write.csv(train, file="base_final_proyecto (trainpy).csv",row.names = FALSE)


#######################################################################
########### Limpieza de datos      ####################################
#######################################################################
#######################################################################

base_final<-read_csv(file="base_final_proyecto (1).csv")

base_final <- subset(base_final, select = -c(PK_CALZADA,CODIGO_ACC, ANO_OCURRE, DIRECCION,PLACA,CIV,CODIGO.x, CODIGO_VEHICULO.x, CODIGO_ACCIDENTE,TIPO_CAUSA))

colSums(is.na(base_final))

#########################################################
#Día de la semana/ Semanas del Mes/ Mes/Día #############
#########################################################
library(lubridate)

base_final$dias_semana <- wday(base_final$fecha, week_start=1)
base_final %<>% mutate_at(c("dias_semana"), factor) 


first_day_of_month_wday <- function(dx) {
  day(dx) <- 1
  wday(dx)
}


base_final$semanas_mes<-ceiling((day(base_final$fecha) + first_day_of_month_wday(base_final$fecha) - 1) / 7)
base_final %<>% mutate_at(c("semanas_mes"), factor) 

base_final$mes<-month(base_final$fecha)
base_final %<>% mutate_at(c("mes"), factor) 


base_final$dias <- day(base_final$fecha)
base_final <- subset(base_final, select = -c(FORMULARIO,fecha,NOMBRE,long,lat,geometry))

res <- hms(base_final$sunrise)        # format to 'hours:minutes:seconds'

base_final$sunrise<-hour(res)*60 + minute(res)       # convert hours to minutes, and add minutes


res <- hms(base_final$sunset)        # format to 'hours:minutes:seconds'
base_final$sunset<-hour(res)*60 + minute(res)       # convert hours to minutes, and add minutes




base_final$moonrise <- gsub(x=base_final$moonrise,pattern=" AM",replacement="",fixed=T)
base_final$moonset <- gsub(x=base_final$moonset,pattern=" PM",replacement="",fixed=T)


base_final$moonrise<-as.numeric(hm(base_final$moonrise)) /60

base_final$moonset<-(as.numeric(hm(base_final$moonset))/60)+ (60*12)




base_final <- base_final %>%
  mutate(fatalidad = if_else(GRAVEDAD=="CON MUERTOS",1, 0))

base_final <- subset(base_final, select = -c(GRAVEDAD))


##############################################
#Estadísticas descriptivas ###################
##############################################
ggplot(base_final, aes(x=as.factor(GRAVEDAD), y=visibility)) + 
  geom_boxplot(fill="69b3a2", alpha=0.5) + 
  scale_y_continuous(n.breaks=5,limits = quantile(base_final$visibility, c(0.05, 0.95)))+
  xlab("Gravedad del accidente")+
  ylab("Vsibilidad promedio para el día")

ggplot(base_final, aes(x=as.factor(GRAVEDAD), y=min_dist_cam)) + 
  geom_boxplot(fill="slateblue", alpha=0.5) + 
  scale_y_continuous(n.breaks=5,limits = quantile(base_final$min_dist_cam, c(0.05, 0.95)))+
  xlab("Gravedad del accidente")+
  ylab("Distancia mínima a una cámara de velocidad (metros)")

ggplot(base_final, aes(x=as.factor(GRAVEDAD), y=humidity)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  xlab("Gravedad del accidente")+
  ylab("Humedad")

base_final$numero <- 1
descriptivas <- base_final %>%
  group_by(GRAVEDAD,TIPO_CAUSA) %>%
  summarize(no_obs = n())

library(treemap)

treemap(descriptivas,
        index="GRAVEDAD",
        vSize="no_obs",
        type="index",
        title="Gravedad de Accidentes de Tránsito"
)

treemap(descriptivas,
        index="TIPO_CAUSA",
        vSize="no_obs",
        type="index",
        title="Gravedad de Accidentes de Tránsito"
)

treemap(descriptivas,
        index=c("GRAVEDAD","TIPO_CAUSA"),
        vSize="no_obs",
        type="index",
        title="Gravedad de Accidentes de Tránsito"
)


#SOLO LOS MUERTOS
base_final_1 <- subset(final_base, GRAVEDAD="CON MUERTOS")
descriptivas_1 <- final_base_1 %>%
  group_by(TIPO_CAUSA) %>%
  summarize(no_obs = n())

treemap(descriptivas_1,
        index="TIPO_CAUSA",
        vSize="no_obs",
        type="index",
        title="Gravedad de Accidentes de Tránsito"
)


# Compute percentages
descriptivas_1$fraction = descriptivas_1$no_obs / sum(descriptivas_1$no_obs)

# Compute the cumulative percentages (top of each rectangle)
descriptivas_1$ymax = cumsum(descriptivas_1$fraction)

# Compute the bottom of each rectangle
descriptivas_1$ymin = c(0, head(descriptivas_1$ymax, n=-1))

# Compute label position
descriptivas_1$labelPosition <- (descriptivas_1$ymax + descriptivas_1$ymin) / 2

# Compute a good label
descriptivas_1$label <- paste0(descriptivas_1$fraction)

# Make the plot
ggplot(descriptivas_1, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=TIPO_CAUSA)) +
  geom_rect() +
  scale_fill_brewer(palette=4) +
  coord_polar(theta="y") +
  theme_void() 


##################################################################
#Predicciones espaciales #########################################
##################################################################

data$LOCALIDAD <- data$LOCALIDAD %>% replace_na("KENNEDY")
pred_esp = subset(data, select = c(FORMULARIO, fecha, LOCALIDAD, maxtempC, mintempC, windspeedKmph, humidity, precipMM, tempC, visibility,min_dist_bici,min_dist_cam, min_dist_sem, min_dist_sitp, ANCHOCALZA, AREACALZAD, LONGITUDHO, PUNTAJE_IP, dias_semana, grave, long, lat))

colSums(is.na(pred_esp))

pred_esp_cat <- subset(pred_esp, select = c(LOCALIDAD,dias_semana))
pred_esp_cat <- st_drop_geometry(pred_esp_cat)
pred_esp_cat <- model.matrix( ~.-1, data=pred_esp_cat)
pred_esp_cat <-as.data.frame(pred_esp_cat)

pred_esp_cont <- subset(pred_esp, select = -c(LOCALIDAD,dias_semana))
pred_esp <-cbind(pred_esp_cat,pred_esp_cont)

#pred_esp <- pred_esp %>%
#mutate(LONGITUD = unlist(map(data$geometry,1)),
#LATITUD = unlist(map(data$geometry,2)))

#pred_esp <- st_drop_geometry(pred_esp)
#pred_esp <- subset(pred_esp, select = -c(geometry))

pred_esp <- pred_esp %>% group_by(FORMULARIO) %>% summarise(across(everything(), list(mean = mean)))


pred_con_geometria <- st_as_sf(x = pred_esp, ## datos
                               coords=c("LONGITUD_mean","LATITUD_mean"), ## coordenadas
                               crs=4326) ## CRS


############### crear una grilla de bogota 
library(rgdal)

shape_bog <- readOGR(dsn = "D:/noveno semestre/big data/ROZO/final", layer = "Loca", GDAL1_integer64_policy = TRUE)
shape_bog <- spTransform(shape_bog, CRS("+proj=longlat +datum=WGS84 +no_defs"))
shape_bog <- st_as_sf(shape_bog)

library(tigris)
library(tidyverse)
library(sf)
options(tigris_class = "sf", tigris_use_cache = TRUE)
set.seed(1234)

shape_bog = shape_bog %>% filter(grepl("SUMAPAZ",LocNombre)==FALSE)


g <- shape_bog %>%
  st_make_grid(n=c(80,80)) %>%
  st_intersection(shape_bog) %>%
  st_cast("MULTIPOLYGON") %>%
  st_sf() %>%
  mutate(id = row_number())


plot(g$geometry)
plot(pred_con_geometria$geometry, add = TRUE, col = "red")



coordinates(pred_esp) <- c("long_mean", "lat_mean")
proj4string(pred_esp) <- CRS("+proj=longlat +datum=WGS84")


#############################################
library(data.table)# fast read/write function
library(dplyr)
install.packages("geoR")
library(geoR)
install.packages("gstat")
library(gstat)

pred_esp <- subset(pred_esp, select=-c(FORMULARIO, fecha_mean))

pred_esp <- subset(pred_esp, select=c(grave_mean))


##### forma 2 ######
lzn.vgm <- variogram(grave_mean ~ 1 , data= pred_esp, width=0.1)
lzn.fit = fit.variogram(lzn.vgm, vgm(c("Gau", "Sph", "Mat", "Exp")), fit.kappa = TRUE)

### gráfico del variograma #####
plot(lzn.vgm, lzn.fit, ylab="Semivarianza", xlab="Distancia (Km)") 





##################################################################
#Predicciones factores ###########################################
##################################################################
base_final_cat <- subset(base_final, select = c(CLASE_ACC,CLASE,SERVICIO,TIPO,MODALIDAD,ENFUGA,CODIGO_CAUSA,dias_semana,semanas_mes,mes))
base_final_cat[is.na(base_final_cat)]<-""
base_final_cat <- model.matrix( ~.-1, data=base_final_cat)

base_final_cat <-as.data.frame(base_final_cat)

base_final <- subset(base_final, select = -c(CLASE_ACC,LOCALIDAD,CLASE,SERVICIO,TIPO,MODALIDAD,ENFUGA,CODIGO_CAUSA,TIPO,dias_semana,semanas_mes,mes,moonrise,moonset))
base_final<-cbind(base_final,base_final_cat)



set.seed(420)

sample <- sample(c(TRUE, FALSE), nrow(base_final), replace=TRUE, prob=c(0.7,0.3))

train <- subset(base_final, sample == TRUE)
test <- subset(base_final, sample == FALSE)


###########################################
############ Oversampling #################
###########################################

library(pacman)
library(haven)

p_load(AER, tidyverse, caret, MLmetrics, tidymodels, themis)

x_train<-subset(train, select=-c(fatalidad))
y_train<-subset(train, select=c(fatalidad))
y_train$fatalidad<-as.factor(y_train$fatalidad)


train <- recipe(fatalidad ~ .,data = cbind(x_train,y_train)) %>%
  themis::step_smote(fatalidad, over_ratio = 1) %>%
  prep() %>% 
  bake(new_data = NULL)

prop.table(table(train$fatalidad))

x_train<-subset(train, select=-c(fatalidad))
y_train<-subset(train, select=c(fatalidad))


x_test<-subset(test, select=-c(fatalidad))
y_test<-subset(test, select=c(fatalidad))

write.csv(x_train, file="base_final_proyecto (trainx).csv",row.names = FALSE)
write.csv(y_train, file="base_final_proyecto (trainy).csv",row.names = FALSE)
write.csv(x_test, file="base_final_proyecto (testx).csv",row.names = FALSE)
write.csv(y_test, file="base_final_proyecto (testy).csv",row.names = FALSE)
##############################################
####### Linear model #########################
##############################################

modelo1 <- lm(formula = fatalidad ~ ., data = train)

#Métricas de predicción(dentro y fuera de muestra) 
probs_insample1 <- predict(modelo1, train)
probs_insample1[probs_insample1 < 0] <- 0
probs_insample1[probs_insample1 > 1] <- 1
probs_outsample1 <- predict(modelo1, test)
probs_outsample1[probs_outsample1 < 0] <- 0
probs_outsample1[probs_outsample1 > 1] <- 1

# Convertimos la probabilidad en una predicción
y_hat_insample1 <- as.numeric(probs_insample1 > 0.5)
y_hat_outsample1 <- as.numeric(probs_outsample1 > 0.5)


addLevel <- function(x, newlevel=NULL) {
  if(is.factor(x)) {
    if (is.na(match(newlevel, levels(x))))
      return(factor(x, levels=c(levels(x), newlevel)))
  }
  return(x)
}

y_hat_cm<-as.data.frame(y_hat_insample1)
cm=confusionMatrix(data =addLevel(as.factor(y_hat_cm$y_hat_insample1),1), reference = as.factor(train$fatalidad))

sen_insample1<-as.numeric(cm$byClass[1][1])
spec_insample1<-as.numeric(cm$byClass[2][1])
fpr_insample1<-1-spec_insample1

y_hat_cm<-as.data.frame(y_hat_outsample1)
cm=confusionMatrix(data =addLevel(as.factor(y_hat_cm$y_hat_outsample1),1), reference = as.factor(test$fatalidad))

sen_outsample1<-as.numeric(cm$byClass[1][1])
spec_outsample1<-as.numeric(cm$byClass[2][1])
fpr_outsample1<-1-spec_outsample1


acc_insample1 <- Accuracy(y_pred = y_hat_insample1, y_true = train$fatalidad)
acc_outsample1 <- Accuracy(y_pred = y_hat_outsample1, y_true = test$fatalidad)

pre_insample1 <- Precision(y_pred = y_hat_insample1, y_true = train$fatalidad, positive = 1)
pre_outsample1 <- Precision(y_pred = y_hat_outsample1, y_true = test$fatalidad, positive = 1)

rec_insample1 <- Recall(y_pred = y_hat_insample1, y_true = train$fatalidad, positive = 1)
rec_outsample1 <- Recall(y_pred = y_hat_outsample1, y_true = test$fatalidad, positive = 1)

f1_insample1 <- F1_Score(y_pred = y_hat_insample1, y_true = train$fatalidad, positive = 1)
f1_outsample1 <- F1_Score(y_pred = y_hat_outsample1, y_true = test$fatalidad, positive = 1)

metricas_insample12 <- data.frame(Modelo = "Linear Model", 
                                 "Muestreo" = "Normal", 
                                 "Evaluación" = "Dentro de muestra",
                                 "Sensibility"= sen_insample1,
                                 "Specificity" = spec_insample1,
                                 "FPR" =fpr_insample1,
                                 "Accuracy" = acc_insample1,
                                 "Precision" = pre_insample1,
                                 "Recall" = rec_insample1,
                                 "F1" = f1_insample1)

metricas_outsample12 <- data.frame(Modelo = "Linear Model", 
                                  "Muestreo" = "Normal", 
                                  "Evaluación" = "Fuera de muestra",
                                  "Sensibility"= sen_outsample1,
                                  "Specificity" = spec_outsample1,
                                  "FPR" =fpr_outsample1,
                                  "Accuracy" = acc_outsample1,
                                  "Precision" = pre_outsample1,
                                  "Recall" = rec_outsample1,
                                  "F1" = f1_outsample1)


metricas12 <- bind_rows(metricas_insample12, metricas_outsample12)
metricas <- bind_rows(metricas12)

metricas %>%
  kbl(digits = 2)  %>%
  kable_styling(full_width = T)




##############################################
####### Elastic Net ##########################
##############################################

library(glmnet)
library(kableExtra)

x_train<-as.matrix(subset(train, select=-c(fatalidad)))
y_train<-subset(train, select=c(fatalidad))
y_train$fatalidad<-as.factor(y_train$fatalidad)

glmmod <- cv.glmnet(x_train, y_train$fatalidad, alpha = 1, family="binomial")


x_test<-as.matrix(subset(test, select=-c(fatalidad)))
y_test<-subset(test, select=c(fatalidad))
y_test$fatalidad<-as.factor(y_test$fatalidad)

probs_insample2 <- predict(glmmod, x_train)
probs_insample2[probs_insample2 < 0] <- 0
probs_insample2[probs_insample2 > 1] <- 1
probs_outsample2 <- predict(glmmod, x_test)
probs_outsample2[probs_outsample2 < 0] <- 0
probs_outsample2[probs_outsample2 > 1] <- 1

# Convertimos la probabilidad en una predicción
y_hat_insample2 <- as.numeric(probs_insample2 > 0.5)
y_hat_outsample2 <- as.numeric(probs_outsample2 > 0.5)

y_hat_cm<-as.data.frame(y_hat_insample2)
cm=confusionMatrix(data =as.factor(y_hat_cm$y_hat_insample2), reference = as.factor(train$fatalidad))

sen_insample2<-as.numeric(cm$byClass[1][1])
spec_insample2<-as.numeric(cm$byClass[2][1])
fpr_insample2<-1-spec_insample2

y_hat_cm<-as.data.frame(y_hat_outsample2)
cm=confusionMatrix(data =as.factor(y_hat_cm$y_hat_outsample2), reference = as.factor(test$fatalidad))

sen_outsample2<-as.numeric(cm$byClass[1][1])
spec_outsample2<-as.numeric(cm$byClass[2][1])
fpr_outsample2<-1-spec_outsample2


acc_insample2 <- Accuracy(y_pred = y_hat_insample2, y_true = train$fatalidad)
acc_outsample2 <- Accuracy(y_pred = y_hat_outsample2, y_true = test$fatalidad)

pre_insample2 <- Precision(y_pred = y_hat_insample2, y_true = train$fatalidad, positive = 1)
pre_outsample2 <- Precision(y_pred = y_hat_outsample2, y_true = test$fatalidad, positive = 1)

rec_insample2 <- Recall(y_pred = y_hat_insample2, y_true = train$fatalidad, positive = 1)
rec_outsample2 <- Recall(y_pred = y_hat_outsample2, y_true = test$fatalidad, positive = 1)

f1_insample2 <- F1_Score(y_pred = y_hat_insample2, y_true = train$fatalidad, positive = 1)
f1_outsample2 <- F1_Score(y_pred = y_hat_outsample2, y_true = test$fatalidad, positive = 1)

metricas_insample22 <- data.frame(Modelo = "Elastic Net", 
                                 "Muestreo" = "Normal", 
                                 "Evaluación" = "Dentro de muestra",
                                 "Sensibility"= sen_insample2,
                                 "Specificity" = spec_insample2,
                                 "FPR" =fpr_insample2,
                                 "Accuracy" = acc_insample2,
                                 "Precision" = pre_insample2,
                                 "Recall" = rec_insample2,
                                 "F1" = f1_insample2)

metricas_outsample22 <- data.frame(Modelo = "Elastic Net", 
                                  "Muestreo" = "Normal", 
                                  "Evaluación" = "Fuera de muestra",
                                  "Sensibility"= sen_outsample2,
                                  "Specificity" = spec_outsample2,
                                  "FPR" =fpr_outsample2,
                                  "Accuracy" = acc_outsample2,
                                  "Precision" = pre_outsample2,
                                  "Recall" = rec_outsample2,
                                  "F1" = f1_outsample2)



metricas22 <- bind_rows(metricas_insample22, metricas_outsample22)
metricas <- bind_rows(metricas12, metricas22)

metricas %>%
  kbl(digits = 2)  %>%
  kable_styling(full_width = T)




##############################################
####### Random Forest #########################
##############################################
install.packages("randomForest")
library(randomForest)
fiveStats <- function(...) c(twoClassSummary(...), defaultSummary(...))


fitControl <- trainControl(
  method = 'cv', 
  summaryFunction = fiveStats,     # k-fold cross validation
  number = 5,                      # number of folds
  savePredictions = 'final',       # saves predictions for optimal tuning parameter
  classProbs = T,                  # should class probabilities be returned  # results summary function
) 

train$fatalidad<- ifelse(train$fatalidad==0,"No","Yes")
train$fatalidad <- as.factor(train$fatalidad)

model_rf2 = train(fatalidad ~ ., data=train, method='rf', tuneLength=5, trControl = fitControl, metric="Accuracy")



probs_insample3 <- predict(model_rf2, train)
probs_outsample3 <- predict(model_rf2, test)



# Convertimos la probabilidad en una predicción
y_hat_insample3 <- as.numeric(probs_insample3) - 1
y_hat_outsample3 <- as.numeric(probs_outsample3) -1

train$fatalidad<- ifelse(train$fatalidad=="Yes",1,0)

y_hat_cm<-as.data.frame(y_hat_insample3)
cm=confusionMatrix(data =as.factor(y_hat_cm$y_hat_insample3), reference = as.factor(train$fatalidad))

sen_insample3<-as.numeric(cm$byClass[1][1])
spec_insample3<-as.numeric(cm$byClass[2][1])
fpr_insample3<-1-spec_insample3

y_hat_cm<-as.data.frame(y_hat_outsample3)
cm=confusionMatrix(data =as.factor(y_hat_cm$y_hat_outsample3), reference = as.factor(test$fatalidad))

sen_outsample3<-as.numeric(cm$byClass[1][1])
spec_outsample3<-as.numeric(cm$byClass[2][1])
fpr_outsample3<-1-spec_outsample3

acc_insample3 <- Accuracy(y_pred = y_hat_insample3, y_true = train$fatalidad)
acc_outsample3 <- Accuracy(y_pred = y_hat_outsample3, y_true = test$fatalidad)

pre_insample3 <- Precision(y_pred = y_hat_insample3, y_true = train$fatalidad, positive = 1)
pre_outsample3 <- Precision(y_pred = y_hat_outsample3, y_true = test$fatalidad, positive = 1)

rec_insample3 <- Recall(y_pred = y_hat_insample3, y_true = train$fatalidad, positive = 1)
rec_outsample3 <- Recall(y_pred = y_hat_outsample3, y_true = test$fatalidad, positive = 1)

f1_insample3 <- F1_Score(y_pred = y_hat_insample3, y_true = train$fatalidad, positive = 1)
f1_outsample3 <- F1_Score(y_pred = y_hat_outsample3, y_true = test$fatalidad, positive = 1)

metricas_insample32 <- data.frame(Modelo = "RF", 
                                 "Muestreo" = "SMOTE - Oversampling", 
                                 "Evaluación" = "Dentro de muestra",
                                 "Sensibility"= sen_insample3,
                                 "Specificity" = spec_insample3,
                                 "FPR" =fpr_insample3,
                                 "Accuracy" = acc_insample3,
                                 "Precision" = pre_insample3,
                                 "Recall" = rec_insample3,
                                 "F1" = f1_insample3)

metricas_outsample32 <- data.frame(Modelo = "RF", 
                                  "Muestreo" = "SMOTE - Oversampling", 
                                  "Evaluación" = "Fuera de muestra",
                                  "Sensibility"= sen_outsample3,
                                  "Specificity" = spec_outsample3,
                                  "FPR" =fpr_outsample3,
                                  "Accuracy" = acc_outsample3,
                                  "Precision" = pre_outsample3,
                                  "Recall" = rec_outsample3,
                                  "F1" = f1_outsample3)

metricas32 <- bind_rows(metricas_insample32, metricas_outsample32)
metricas <- bind_rows(metricas1,metricas2,metricas3,metricas4,metricas12, metricas22, metricas32,metricas42)

metricas %>%
  kbl(digits = 2)  %>%
  kable_styling(full_width = T)





model_rf$finalModel$importance
##############################################
####### XGBoost ##############################
##############################################


modelo4<-read_csv(file="xgb_dentro_over.csv")
modelo4_outsample<-read_csv(file="xgb_fuera_over.csv")

probs_insample4<-modelo4$dentro_muestra
probs_outsample4<-modelo4_outsample$fuera_muestra

probs_insample4[probs_insample4 < 0] <- 0
probs_insample4[probs_insample4 > 1] <- 1
probs_outsample4[probs_outsample4 < 0] <- 0
probs_outsample4[probs_outsample4 > 1] <- 1

# Convertimos la probabilidad en una predicción
y_hat_insample4 <- as.numeric(probs_insample4 > 0.5)
y_hat_outsample4 <- as.numeric(probs_outsample4 > 0.5)

y_hat_cm<-as.data.frame(y_hat_insample4)
cm=confusionMatrix(data =as.factor(y_hat_cm$y_hat_insample4), reference = as.factor(train$fatalidad))

sen_insample4<-as.numeric(cm$byClass[1][1])
spec_insample4<-as.numeric(cm$byClass[2][1])
fpr_insample4<-1-spec_insample4

y_hat_cm<-as.data.frame(y_hat_outsample4)
cm=confusionMatrix(data =as.factor(y_hat_cm$y_hat_outsample4), reference = as.factor(test$fatalidad))

sen_outsample4<-as.numeric(cm$byClass[1][1])
spec_outsample4<-as.numeric(cm$byClass[2][1])
fpr_outsample4<-1-spec_outsample4


acc_insample4 <- Accuracy(y_pred = y_hat_insample4, y_true = train$fatalidad)
acc_outsample4 <- Accuracy(y_pred = y_hat_outsample4, y_true = test$fatalidad)

pre_insample4 <- Precision(y_pred = y_hat_insample4, y_true = train$fatalidad, positive = 1)
pre_outsample4 <- Precision(y_pred = y_hat_outsample4, y_true = test$fatalidad, positive = 1)

rec_insample4 <- Recall(y_pred = y_hat_insample4, y_true = train$fatalidad, positive = 1)
rec_outsample4 <- Recall(y_pred = y_hat_outsample4, y_true = test$fatalidad, positive = 1)

f1_insample4 <- F1_Score(y_pred = y_hat_insample4, y_true = train$fatalidad, positive = 1)
f1_outsample4 <- F1_Score(y_pred = y_hat_outsample4, y_true = test$fatalidad, positive = 1)

metricas_insample4 <- data.frame(Modelo = "XGBoost", 
                                  "Muestreo" = "SMOTE - Oversampling", 
                                  "Evaluación" = "Dentro de muestra",
                                  "Sensibility"= sen_insample4,
                                  "Specificity" = spec_insample4,
                                  "FPR" =fpr_insample4,
                                  "Accuracy" = acc_insample4,
                                  "Precision" = pre_insample4,
                                  "Recall" = rec_insample4,
                                  "F1" = f1_insample4)

metricas_outsample4 <- data.frame(Modelo = "XGBoost", 
                                   "Muestreo" = "SMOTE - Oversampling", 
                                   "Evaluación" = "Fuera de muestra",
                                   "Sensibility"= sen_outsample4,
                                   "Specificity" = spec_outsample4,
                                   "FPR" =fpr_outsample4,
                                   "Accuracy" = acc_outsample4,
                                   "Precision" = pre_outsample4,
                                   "Recall" = rec_outsample4,
                                   "F1" = f1_outsample4)



metricas4 <- bind_rows(metricas_insample4, metricas_outsample4)

metricas <- bind_rows(metricas12, metricas22)

metricas %>%
  kbl(digits = 2)  %>%
  kable_styling(full_width = T)




