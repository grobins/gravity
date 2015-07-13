


imp <- read.table('test_points.csv', sep=',', header=T)
imp$w <- 1


nearestSelf <- function(df, gr = NULL, n){
  require(FNN)
  if(nrow(gr)>0){
    df[get.knnx(df, gr, n)$nn.index[, n],]
  } else {
    df[get.knnx(df, df, n)$nn.index[, n],]
  }
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
    
    if (i==1){
      
      NN <- nearestSelf(df, df, 2)
      NN$w <- 1
      gr <- gravityWeight(df, NN)
      
    } else {
      
      
      NN <- nearestSelf(df, gr, 2)
      NN$w <- gr[, 3]
      gr <- gravityWeight(df, NN)
      
      
    }
    

  }
  print(gr[1,])
  gr
}



mid <- gravity(imp, 1000)
plot(imp[, 1:2], pch=1)
points(mid[, 1:2], col='blue', pch=19)
points(nearestSelf(mid, mid, 2)[, 1:2], col='green')

par(mfrow=c(2, 2))
par(mfrow=c(1, 1))









