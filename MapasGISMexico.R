##############################################################################
# Mapas GIS de Mexico                                                        #
# Se presentan dos ejemplos de Mapas GIS de México                           #
# (c) Angel Gonzalez Espinosa 2021                                           #
# aglezespinosa@gmail.com                                                    #
##############################################################################

# Carga de Librerias
library(rgdal)
library(ggplot2)
library(tidyverse)

# Carga de Shape de Mexico
shp = readOGR(dsn=".", layer="México_Estados", use_iconv = TRUE)
shape_estados = readOGR(dsn=".", layer="México_Estados", use_iconv = TRUE)
class(shape_estados)

proj4string(shape_estados)
# Dibuja Mapa Sencillo Mexico
ggplot(data=shape_estados) +  
  geom_polygon(aes(x=long, y=lat, group=group), 
               fill="white",
               color="black") +
  theme_minimal() +
  theme(axis.text = element_blank()) +
  labs(title = "Mapa simple de México")

# Crear Mapa de Marginacion de Mexico
#arch <- "http://www.conapo.gob.mx/work/models/CONAPO/Marginacion/Datos_Abiertos/Entidad_Federativa/Marginacion_por_entidad_2015.csv"
# Carga datos de marginacion
arch <- "Marginacion_por_entidad_2015c.csv"
IM_2015 <- read.csv(arch, header=TRUE, sep=",") 
IM_2015b <- mutate(IM_2015, id=as.character(id))

capas_estados<-inner_join(fortify(shape_estados,by="id"),IM_2015b,by="id")
names(capas_estados)

values<-c('tomato1', 'sandybrown','wheat3','darkolivegreen3','green3')
# Dibuja Mapa
capas_estados %>% 
  mutate(GME=fct_relevel(.$GME,"Muy alto","Alto","Medio","Bajo","Muy bajo")) %>%
  ggplot() +  
  geom_polygon(aes(x=long, 
                   y=lat, 
                   group=group, #El argumento group=group arma grupos tanto para para los polígonos. 
                   color="black",
                   fill=GME)) + 
  theme_void() + 
  theme(plot.title = element_text(size=22),
        legend.key.size = unit(0.5, "cm"),
        legend.position = c(0.8, 0.7)) + 
  scale_fill_manual(values=values) + 
  scale_color_manual(values=c("black")) +
  guides(color = FALSE) +
  labs(title = "Índice de marginación a nivel estatal",
       fill = "Muertes",
       caption = "Fuente: Encuesta Intercensal 2015 (INEGI), \n Índice de marginación por estado 2015 (CONAPO).")


