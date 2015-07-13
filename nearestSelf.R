


imp <- read.table('test_points.csv', sep=',', header=T)
imp$w <- 1


nearestSelf <- function(df, gr = NULL){
  require(FNN)
  if(nrow(gr)>0){
    df[get.knnx(df, gr, 2)$nn.index[, 2],]
    
  }
  df[get.knnx(df, df, 2)$nn.index[, 2],]
}


gravityWeight <- function(p1, p2){
  # takes two vectors of weightings for p1 and p2 and calculates 
  # new point at x distance based on weights
  d <- distVincentyEllipsoid(p1[, c(1:2)], p2[, c(1:2)])
  b <- finalBearing(p1[, c(1:2)], p2[, c(1:2)])
  
  x <- p2[, 3] * d / (p1[, 3] + p2[, 3])
  
  mid <- as.data.frame(destPoint(p1, b, x))
  mid$w <- p1[, 3] + p2[, 3]
  mid
}


gravity <- function(df, n){
  require(geosphere)
  
  for (i in 1:n){
    
    NN <- nearestSelf(df, gr)
    NN$w <- 1
    df <- gravityWeight(df, NN)
    
  }
  df
}

mid <- gravity(imp, 2000)
par(mfrow=c(2, 2))
plot(imp[, 1:2], pch=1)
points(mid[, 1:2], col='blue', pch=19)
par(mfrow=c(1, 1))









