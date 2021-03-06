library(sf)
library(tidyverse)
library(spdep)
library(lmtest)
library(tmap)
library(RColorBrewer)

vivref <- readRDS('data/vivpersgeom_sf.RDS') %>% 
  dplyr::select(matches(
    'TOPONIMIA|Material Construcción Techo: Zinc|Centros de refugio de la defensa civil en este segmento.*Si$'))
vivref %>% plot(breaks = 'jenks')
vivref %>% mutate(vtotal=
                     `Centros de refugio de la defensa civil en este segmento: Otros: Si`+
                     `Centros de refugio de la defensa civil en este segmento: Centro deportivo: Si`)
vivref %>% 
  gather(variables, valor, -TOPONIMIA, -geom) %>% 
  tm_shape() +
  tm_fill(col = 'valor', style = 'kmeans', palette = brewer.pal(9, name = 'Reds')) +
  tm_borders(col = 'grey30', lwd = 0.3) +
  tm_facets('variables')
tm_shape(vivefuera_sf) +
  tm_fill(col = "Pct vive fuera pais", style = 'jenks', palette = brewer.pal(9, name = 'Reds')) +
  tm_borders(lwd = 0.5)


vivref.sp <- as_Spatial(vivref)
colnames(vivref.sp@data) <- vivref %>% st_drop_geometry() %>% colnames
row.names(vivref.sp) <- as.character(vivref.sp$TOPONIMIA)
vivref.nb  <- poly2nb(vivref.sp, queen=TRUE)
summary(vivref.nb)

card(vivref.nb)
sapply(vivref.nb, function(x) x)
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

vivref %>% 
  gather(variables, valor, -TOPONIMIA, -geom) %>% 
  tm_shape() +
  tm_fill(col = 'valor', style = 'kmeans', palette = brewer.pal(9, name = 'Reds')) +
  tm_borders(col = 'grey30', lwd = 0.3) +
  tm_facets('variables')
tm_shape() +
  tm_fill(col = "Material Construcción Techo: Zinc", style = 'jenks', palette = brewer.pal(9, name = 'Reds')) +
  tm_borders(lwd = 0.5)
