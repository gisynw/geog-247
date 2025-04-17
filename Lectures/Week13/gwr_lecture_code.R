library(spgwr)

if (packageVersion("spData") >= "2.3.2") {
  NY8a <- sf::st_read(system.file("shapes/NY8_utm18.gpkg", package="spData"))
} else {
  NY8a <- sf::st_read(system.file("shapes/NY8_bna_utm18.gpkg", package="spData"))
  sf::st_crs(NY8a) <- "EPSG:32618"
  NY8a$Cases <- NY8a$TRACTCAS
}
NY8 <- as(NY8a, "Spatial")

plot(NY8)
