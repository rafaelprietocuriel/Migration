
#### project for measuring the scaling of migration

add.alpha <- function(col, alpha=1){
  if(missing(col))
    stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2, 
        function(x) 
          rgb(x[1], x[2], x[3], alpha=alpha))  
}
T <- 4 #size
Full <- read.csv("FullModelsResults20180525.csv")
Cities <- read.csv("CitiesModelsResults.radiation.csv", 
                   header = T, stringsAsFactors=F, sep= ",")


#### cities graph
colGravity <- add.alpha("blue", .5)
colScaling <- add.alpha("orange", .8)
colRadiation <- add.alpha("red", .8)
colSD <- add.alpha("black", .5)



#### outflow
par(mar = c(1,1,1,1))
plot(-200, xlim = c(7.2,13), ylim = c(6.5, 13.2),
      xaxt = "n", yaxt = "n")
points(log(Cities$true.outflow), log(Cities$radiation.outflow), 
       lwd = 3, pch = 21, col = 1, bg = colRadiation, cex = T)
points(log(Cities$true.outflow), log(Cities$Gravity.1.outflow), 
       lwd = 3, pch = 21, col = 1, bg = colGravity, cex = T)

#points(log(Cities$true.outflow), log(Cities$scaling.distance.inflow), 
#       lwd = 3, pch = 21, col = 1, bg = colSD, cex = T)

points(log(Cities$true.outflow), log(Cities$scaling.gravity.inflow), 
       lwd = 3, pch = 21, col = 1, bg = colScaling, cex = T)

  #points(log(Cities$true_outflow), log(Cities$outflow_gravity_distance), col = 4)
points(c(7.3,13), c(7.3, 13), type = "l", col = 1, lwd = 35)
points(c(7.3,13), c(7.3, 13), type = "l", col = "yellow", lwd = 25)





#### Inflow
par(mar = c(1,1,1,1))
plot(-200, xlim = c(7.2,13), ylim = c(6.5, 13.2),
     xaxt = "n", yaxt = "n")
points(log(Cities$true.outflow), log(Cities$radiation.inflow), 
       lwd = 3, pch = 21, col = 1, bg = colRadiation, cex = T)
points(log(Cities$true.inflow), log(Cities$Gravity1.inflow), 
       lwd = 3, pch = 21, col = 1, bg = colGravity, cex = T )
points(log(Cities$true.inflow), log(Cities$scaling.gravity.inflow), 
       lwd = 3, pch = 21, col = 1, bg = colScaling, cex = T)
#points(log(Cities$true_inflow), log(Cities$inflow_gravity_distance), col = 4)
points(c(7.3,13), c(7.3, 13), type = "l", col = 1, lwd = 35)
points(c(7.3,13), c(7.3, 13), type = "l", col = "yellow", lwd = 25)





colGravity <- add.alpha("blue", .5)
colScaling <- add.alpha("orange", .5)
colRadiation <- add.alpha("red", .5)
colSD <- add.alpha("black", .5)

#### full model graph
par(mar = c(1,1,1,1))
S <- 1.2
plot(-200, xlim = c(0,10), ylim = c(0, 10.0),
     xaxt = "n", yaxt = "n")
points(log(Full$migration +1), log(Full$flux_gravity+1), cex = S,
       pch = 21, col = colGravity, bg = colGravity)
points(log(Full$migration +1), log(Full$flux_radiation +1), cex = S,
       pch = 21, col = colRadiation, bg = colRadiation)
points(log(Full$migration +1), log(Full$flux_scaling +1), cex = S,
       pch = 21, col = colScaling, bg = colScaling)
points(log(Full$migration +1), log(Full$flux_scaling_distance +1), cex = 1,
       pch = 21, col = colScaling, bg = colSD)

points(c(-0.3,10), c(-0.3, 10), type = "l", col = 1, lwd = 17)
points(c(-0.3,10), c(-0.3, 10), type = "l", col = "yellow", lwd = 12)

