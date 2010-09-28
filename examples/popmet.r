# popmet.r
# Genotypic distance metrics for GP, test cases
# 2010 Oliver Flasch
#

require(rgp)
require(MASS)

populationDistanceMatrix <- function(p) {
  d <- normInducedFunctionDistance(exprVisitationLength)
  customDist(p, d)
}

populationPlotMDS <- function(p) {
  dm <- populationDistanceMatrix(p)
  dmp <- abs(dm + rnorm(1, sd = 0.01)) # pertube and remove negative values
  fit <- isoMDS(dmp, k=2)
  plot(fit$points[,1], fit$points[,2], main = "Nonmetric MDS of Genotypic Distance")
}

populationWardClustering <- function(p, k = 5) {
  d <- populationDistanceMatrix(p)
  fit <- hclust(d, method = "ward")
  cutree(fit, k)
}

populationPlotWardClusteringDendrogram <- function(p, k = 5) {
  groups <- populationWardClustering(p, k) 
  plot(fit)
  rect.hclust(fit, k, border = "red")
}

populationPlotWardClusteringMDS <- function(p, k = 5) {
  dm <- populationDistanceMatrix(p)
  dmp <- abs(dm + rnorm(1, sd = 0.01)) # pertube and remove negative values
  groups <- cutree(hclust(dm, method = "ward"), k)
  fit <- isoMDS(dmp, k=2)
  x <- fit$points[,1]
  y <- fit$points[,2]
  plot(x, y, type = "n", main = "Ward Clustering by Genotypic Distance, Nonmetric MDS")
  text(x, y, labels = groups, cex = 0.7)
}
