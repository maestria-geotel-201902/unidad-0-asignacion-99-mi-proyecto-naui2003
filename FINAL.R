library(sf)
library(tidyverse)
library(spdep)
library(lmtest)
library(tmap)
library(RColorBrewer)



``{r}
objvdvi <- readRDS('data/vivpersgeom_sf.RDS') %>% 
  dplyr::select(matches(
    'TOPONIMIA|Material Construcción Techo: Zinc|Centros de refugio de la defensa civil en este segmento.*Si$'))
objvdvi %>% plot(breaks = 'jenks')
objvdvi %>% mutate(vtotal=
                     `Centros de refugio de la defensa civil en este segmento: Otros: Si`+
                     `Centros de refugio de la defensa civil en este segmento: Centro deportivo: Si`)

objvdvi %>% 
  gather(variables, valor, -TOPONIMIA, -geom) %>% 
  tm_shape() +
  tm_fill(col = 'valor', style = 'kmeans', palette = brewer.pal(9, name = 'Reds')) +
  tm_borders(col = 'grey30', lwd = 0.3) +
  tm_facets('variables')
tm_shape(vivefuera_sf) +
  tm_fill(col = "Pct vive fuera pais", style = 'jenks', palette = brewer.pal(9, name = 'Reds')) +
  tm_borders(lwd = 0.5)



Ref<- readRDS('data/vivpersgeom_sf.RDS')
Ref %>% dplyr::select(contains('Centros de refugio de la defensa civil en este segmento: Escuela o Liceo: Si')) %>%
  plot(breaks = 'jenks')

viv %>% dplyr::select(contains('Centros de refugio de la defensa civil en este segmento: Escuela o Liceo: no')) %>%
  plosuma <- primervalor + segundovalort(breaks = 'jenks')


viv.sp <- as_Spatial(viv)
colnames(viv.sp@data)[27:33]
colnames(viv.sp@data) <- viv %>% st_drop_geometry() %>% colnames
row.names(viv.sp) <- as.character(viv.sp$TOPONIMIA)
viv.nb  <- poly2nb(viv.sp, queen=TRUE)
summary(viv.nb)

card(viv.nb)
sapply(viv.nb, function(x) x)
plot(viv.sp, border="grey", lwd=0.5)
plot(viv.nb, coordinates(viv.sp), add=T)
is.symmetric.nb(viv.nb)

coords <- coordinates(viv.sp)
ident <- row.names(viv.sp)
viv.nb.k1  <- knn2nb(knearneigh(coords, k = 1), row.names = ident)
summary(viv.nb.k1)

card(viv.nb.k1)
sapply(viv.nb.k1, function(x) x)


plot(viv.sp, border="grey", lwd=0.5)
plot(viv.nb.k1, coordinates(viv.sp), add=T)

is.symmetric.nb(viv.nb.k1)

dist<- unlist(nbdists(viv.nb.k1, coords))
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

viv.w.W  <- nb2listw(viv.nb)
viv.w.W
viv.w.B <- nb2listw(viv.nb, style = 'B')
viv.w.B 

