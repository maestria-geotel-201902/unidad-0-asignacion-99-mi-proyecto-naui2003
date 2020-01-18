
library(sf)
library(sp)
library(tidyverse)
library(spdep)
library(lmtest)
library(tmap)
library(RColorBrewer)
library(ggplot2)
source('lisaclusters.R')

## Las variables con las que desarrollaremos este trabajo son: 
## Viviendas con material de construcción de techo de zinc, como Variable Dependiente y, 
## Centros de Refugio, como Variable Independiente. 

#Cargamos nuestros datos, leyendo la extension siguiente:
vivref <- readRDS('data/vivpersgeom_sf.RDS') %>% 
  dplyr::select(matches(
    'TOPONIMIA|Condición de ocupación|Material Construcción Techo: Zinc|Centros de refugio de la defensa civil en este segmento.*Si$')) %>% 
  mutate(`Total de viviendas`=`Condición de ocupación: Ocupada con personas presentes` + `Condición de ocupación: Desocupada`) %>% 
  mutate_each(funs(PCT=round(./`Total de viviendas`,4)*100), -TOPONIMIA, -geom, -`Total de viviendas`) %>% 
  mutate("Material Construcción Techo: Zinc_PCT log"= log(`Material Construcción Techo: Zinc_PCT`))
vivref
vivref %>% plot(breaks = 'jenks')
vivref %>% mutate(vtotal=
                    `Centros de refugio de la defensa civil en este segmento: Otros: Si`+
                    `Centros de refugio de la defensa civil en este segmento: Centro deportivo: Si`)


vivref.sp <- as_Spatial(vivref)
colnames(vivref.sp@data) <- vivref %>% st_drop_geometry() %>% colnames

row.names(vivref.sp) <- as.character(vivref.sp$TOPONIMIA)

#Utilizamos el criterio queen y de esta manera aplicamos la vecindad por contigüidad.
vivref.nb  <- poly2nb(vivref.sp, queen=TRUE)
summary(vivref.nb)
card(vivref.nb)
sapply(vivref.nb, function(x) x)

#Realizamos un mapa de los vínculos de vecindad y confirmamos si el objeto de vencidad es simétrico.
plot(vivref.sp, border="grey", lwd=0.5)
plot(vivref.nb, coordinates(vivref.sp), add=T)
is.symmetric.nb(vivref.nb)

coords <- coordinates(vivref.sp)
ident <- row.names(vivref.sp)
vivref.nb.k1  <- knn2nb(knearneigh(coords, k = 1), row.names = ident)
summary(vivref.nb.k1)

card(vivref.nb.k1)
sapply(vivref.nb.k1, function(x) x)

plot(vivref.sp, border="grey", lwd=0.5)
plot(vivref.nb.k1, coordinates(viv.sp), add=T)

is.symmetric.nb(vivref.nb.k1)

dist<- unlist(nbdists(vivref.nb.k1, coords))
summary(dist)
hist(dist)
boxplot(dist)

(distmin <- min(dist)) 
(distmax <- max(dist))
indicemin <- which(dist==distmin)
ident[indicemin]
indicemax <- which(dist==distmax)
ident[indicemax]

ident[order(dist)]

vivref.w.W  <- nb2listw(viv.nb)
vivref.w.W
vivref.w.B <- nb2listw(viv.nb, style = 'B')
vivref.w.B 


st_crs(vivref)
crsdestino <- 32619
vivrefutm <- vivref %>% st_transform(crs = crsdestino)


#Variable en su versión original como transformada
coordsxy <- vivref %>%
  st_centroid() %>% 
  mutate(x=unlist(map(geom,1)),
         y=unlist(map(geom,2))) %>%
  select(TOPONIMIA, x, y) %>% 
  st_drop_geometry()
vivrefconxy <- vivref %>%
  inner_join(coordsxy, by = 'TOPONIMIA')
vivrefconxy

#mapas

vivref1 <- tm_shape(vivref)+ 
  tm_fill(col = 'Material Construcción Techo: Zinc_PCT', style = 'kmeans', palette = brewer.pal(9, name = 'Reds')) +
  tm_borders(col = 'grey30', lwd = 0.5)
vivref2 <- tm_shape(vivref) +
  tm_fill(col = 'Material Construcción Techo: Zinc_PCT log', style = 'kmeans', palette = brewer.pal(9, name = 'Reds')) +
  tm_borders(col = 'grey30', lwd = 0.5)
tmap_arrange(vivref1, vivref2)


#comprobar el supuesto de normalidad
vivref %>% st_drop_geometry() %>%
  select(contains('PCT')) %>% 
  gather(variable, valor) %>%
  ggplot() + aes(sample=valor) +
  stat_qq() + stat_qq_line() + theme_bw() +
  theme(text = element_text(size = 14)) +
  facet_wrap(~variable, scales = 'free')

vivref %>% st_drop_geometry() %>%
  gather(variable, valor, -(TOPONIMIA)) %>% group_by(variable) %>%
  summarise(prueba_normalidad=shapiro.test(valor)$p.value)

qqnorm(vivref$`Material Construcción Techo: Zinc_PCT`)
shapiro.test(vivref$`Material Construcción Techo: Zinc_PCT`) 
qqnorm(vivref$`Material Construcción Techo: Zinc_PCT log`) 
shapiro.test(vivref$`Material Construcción Techo: Zinc_PCT log`) 


vivrefconxy %>% lm(`Material Construcción Techo: Zinc_PCT`~x,.) %>% plot(3)
vivrefconxy %>% lm(`Material Construcción Techo: Zinc_PCT`~y,.) %>% plot(3)
vivrefconxy %>% lm(`Material Construcción Techo: Zinc_PCT log`~x,.) %>% plot(3)
vivrefconxy %>% lm(`Material Construcción Techo: Zinc_PCT log`~y,.) %>% plot(3)

match(attr(vivref.w.W$neighbours, "region.id"), vivref$TOPONIMIA)==1:155

(gmoranw <- moran.test(x = vivref$`Material Construcción Techo: Zinc_PCT log`, listw = vivref.w.W))

(gmoranb <- moran.test(x = vivref$`Material Construcción Techo: Zinc_PCT log`, listw = vivref.w.B))

moran.plot(x = vivref$`Material Construcción Techo: Zinc_PCT log`, listw = vivref.w.W)




source('lisaclusters.R')
lisamap(objesp=vivref,
        var = 'Material Construcción Techo: Zinc_PCT',
        pesos = vivref.w.W,
        tituloleyenda = 'Significancia\n("x-y", léase\ncomo "x"\nrodeado de "y"',
        leyenda = T,
        anchuratitulo = 1000,
        tamanotitulo = 16,
        fuentedatos ='CENSO 2010',
        titulomapa = paste0('Clusters LISA, de la variable Total de viviendas'))


#MODELIZACION

varsel <- vivref %>% dplyr::select(
  TOPONIMIA = TOPONIMIA,
  TECHOZINC = `Material Construcción Techo: Zinc_PCT`,
  CENTROEDU = `Centros de refugio de la defensa civil en este segmento: Escuela o Liceo: Si_PCT`,
  CENTROREL = `Centros de refugio de la defensa civil en este segmento: Iglesia: Si_PCT`,
  SALONCOMU = `Centros de refugio de la defensa civil en este segmento: Salón comunal: Si_PCT`,
  DEPORTIVO = `Centros de refugio de la defensa civil en este segmento: Centro deportivo: Si_PCT`,
  DEFENSACIV= `Centros de refugio de la defensa civil en este segmento: Otros: Si_PCT`,
  TOTALVIV = `Total de viviendas`)
varsel

varselpctlog <- varsel %>% mutate_each(
  funs(PCT=round(./TOTALVIV,4)*100,
       PCTLOG=log1p(round(./TOTALVIV,4)*100)),
  -1, -2, -geom, -TOTALVIV)
varselpctlog

(gmoranw <- moran.test(x = varselpctlog$CENTROEDU_PCT, listw = vivref.w.W))

(gmoranb <- moran.test(x = varselpctlog$CENTROEDU_PCT, listw = vivref.w.B))

(gmoranwl <- moran.test(x = varselpctlog$CENTROEDU_PCTLOG, listw = vivref.w.W))

(gmoranbl <- moran.test(x = varselpctlog$CENTROEDU_PCTLOG, listw = vivref.w.B))

# Evaluemos si el supuesto de normalidad se cumple
shapiro.test(varselpctlog$CENTROEDU_PCT)

shapiro.test(varselpctlog$CENTROEDU_PCTLOG)

#Aunque la selección tiene varias variables, de éstas resultan 2 significativas

modlin <- varselpctlog %>% select(contains('_PCTLOG')) %>%
  st_drop_geometry() %>% lm(CENTROEDU_PCTLOG ~ ., .)
modlin %>% summary

modlin %>% bptest


#Ejecutemos el modelo espacial autorregresivo, en este caso, la variate Simultaneous Autorregresive Model.

sar <- varselpctlog %>% select(contains('_PCTLOG')) %>%
  st_drop_geometry() %>%
  spautolm(
    formula = CENTROEDU_PCTLOG ~ .,
    data = .,
    listw = vivref.w.W)
summary(sar)

sar2 <- varselpctlog %>% select(contains('_PCTLOG')) %>%
  st_drop_geometry() %>%
  spautolm(
    formula = CENTROEDU_PCTLOG ~ CENTROREL_PCTLOG + DEFENSACIV_PCTLOG,
    data = .,
    listw = vivref.w.W)
summary(sar2)
