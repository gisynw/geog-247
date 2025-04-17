library(terra)
getwd()

##single band
# Blue
b2 <- rast('rs/LC08_044034_20170614_B2.tif')
#number of cells
ncell(b2)
#ros and columns
dim(b2)
# spatial resolution
res(b2)

# Number of layers (bands in remote sensing jargon)
nlyr(b2)

# Green
b3 <- rast('rs/LC08_044034_20170614_B3.tif')
# Red
b4 <- rast('rs/LC08_044034_20170614_B4.tif')
# Near Infrared (NIR)
b5 <- rast('rs/LC08_044034_20170614_B5.tif')

##Single band and composite maps
par(mfrow = c(2,2))
plot(b2, main = "Blue", col = gray(0:100 / 100))
plot(b3, main = "Green", col = gray(0:100 / 100))
plot(b4, main = "Red", col = gray(0:100 / 100))
plot(b5, main = "NIR", col = gray(0:100 / 100))

##True (or naturla) color
par(mfrow = c(1,1))
landsatRGB <- c(b5, b4, b3)
plotRGB(landsatRGB, stretch = "lin")

filenames <- paste0('rs/LC08_044034_20170614_B', 1:11, ".tif")
filenames
landsat <- rast(filenames)
landsat

nlyr(landsat)

##Relation between bands
pairs(landsat[[4:5]], main = "Red versus NIR")

pairs(landsat[[1:2]], main = "Red versus NIR")

## Principal Component analysis
set.seed(1)
sr <- spatSample(landsat, 10000)
plot(sr[,c(4,5)], main = "NIR-Red plot")

pca <- prcomp(sr, scale = TRUE)
pca
screeplot(pca)
