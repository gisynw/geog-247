# task 1
library(terra);library(ggplot2);library(dplyr);library(raster);library(RStoolbox)  # For remote sensing analysis
library(landsat);library(gridExtra)
filenames <- paste0('rs/LC08_044034_20170614_B', 1:9, ".tif")
landsat <- rast(filenames)
landsat
# vals <- values(landsat, na.rm = TRUE)
# cov_matrix <- cov(vals)
# task 2
plotRGB(landsat, r = 4, g = 3, b = 2, stretch = "lin", main = "False Color Composite (NIR)")

## task 3
summary_stats <- global(landsat, fun = c("min", "max", "mean", "sd"))
summary_stats

landsat_pca <- rasterPCA(landsat)

# Examine the summary of the PCA results
summary(landsat_pca$model)

# Look at the loadings (how much each original band contributes to each PC)
print(landsat_pca$model$loadings)

# Find out how much variance is explained by each component
explained_var <- landsat_pca$model$sdev^2 / sum(landsat_pca$model$sdev^2)
print(paste("Variance explained by each PC:", paste(round(explained_var * 100, 2), "%", collapse = ", ")))

##task 4
par(mfrow = c(2, 2))
plot(landsat_pca$map$PC1, main = "Principal Component 1", col = gray.colors(100))
plot(landsat_pca$map$PC2, main = "Principal Component 2", col = gray.colors(100))
plot(landsat_pca$map$PC3, main = "Principal Component 3", col = gray.colors(100))
plot(landsat_pca$map$PC4, main = "Principal Component 4", col = gray.colors(100))
##Task 5
# Create an RGB composite using the first three
par(mfrow = c(1, 1))
plotRGB(landsat_pca$map, r = 1, g = 2, b = 3, stretch = "lin", 
        main = "RGB Composite of First Three Principal Components")

