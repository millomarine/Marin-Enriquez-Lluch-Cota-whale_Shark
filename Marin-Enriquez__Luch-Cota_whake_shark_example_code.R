library(data.table)
library(maps)
library(ggplot2)
#### load whale shark occurrences ####
tiba <- fread("0021542-260623161305970.csv", header=T)
names(tiba)
plot(tiba$decimalLongitude, tiba$decimalLatitude, asp=1, pch=19)
tiba <- tiba[,c(22,23,28,31:33)] #select desired columns
en_tierra <- readRDS("en_tierra.rds") #load points on ground
tiba <- tiba[!en_tierra, ] #remove points on ground and outside the polygon
points(tiba$decimalLongitude, tiba$decimalLatitude, col="blue")
map(database = "world", add=T, fill=T, col="khaki")#simple map for location

#### load processed daily oceanographic data ####
ocean <- readRDS("ocean_daily.rds")
names(ocean)
head(ocean)
range(ocean$fecha)
plot(ocean$vel_mean, type="l") #plot time series of mean velocity

#### load oceanographic data the dates of each observation ###
tiba_ocean <- readRDS("daily_occurr_oceanogr.rds")
head(tiba_ocean) 
tiba_ocean$date
summary(tiba_ocean$vel_mean) #summary of mean velocities on the occurrence dates

#### PCA ####
##### PCA diferenciado #### 
vars <- c("vel_mean","KE_mean",
          "vort_mean","vort_sd","vort_IDC",
          "div_mean","div_sd","div_IDC")
ocean_daily <- ocean[complete.cases(ocean),]

X <- ocean_daily[, vars]

pca <- prcomp(X, center = TRUE, scale. = TRUE)
summary(pca) #explained variance
avail <- data.frame(pca$x[,1:2])
avail$Group <- "Available"

shark <- predict(pca, newdata = tiba_ocean[, ..vars]) #prediction using the oceanographic data of the dates of the occurrences

shark <- data.frame(shark[,1:2])
shark$Group <- "Sighting"

plot.dat <- rbind(avail, shark)

centroids <- aggregate(cbind(PC1,PC2)~Group,
                       plot.dat,
                       mean)
x11()
ggplot(plot.dat,
       aes(PC1, PC2,
           colour=Group,
           fill=Group))+
  
  geom_point(alpha=.25,
             size=1)+
  
  stat_ellipse(level=.95,
               geom="polygon",
               alpha=.20,
               linewidth=.8)+
  
  geom_point(data=centroids,
             aes(PC1,PC2),
             shape=4,
             size=5,
             linewidth=1.5)+
  
  theme_classic(base_size=15)+
  
  labs(x=paste0("PC1 (",
                round(100*summary(pca)$importance[2,1],1),
                "%)"),
       y=paste0("PC2 (",
                round(100*summary(pca)$importance[2,2],1),
                "%)"))
round(pca$rotation[,1:2], 2)

#### climatologies of the daily first two PCs ####
pc1mo <- tapply(pca$x[,1], month(ocean_daily$fecha), mean)
pc2mo <- tapply(pca$x[,2], month(ocean_daily$fecha), mean)
plot(pc1mo, type="l", xaxt="n", lwd=2)
axis(side=1, 1:12, labels=month.abb)
lines(pc2mo, lty=2, lwd=2)

#### density plots ####
x11()
par(mfrow=c(1,2));hist(pca$x[,1], probability = T, col="white", border="white", ylim=c(0,0.4), xlim=c(-13,5),main="", xlab="PC1 score", cex.axis=1.3, cex.lab=1.3)
tiba_ocean <- as.data.frame(tiba_ocean)
pred1 <- predict(pca,newdata=tiba_ocean[,vars])[,1]
lines(density(pca$x[,1]), lwd=2, col="red")
lines(density(pred1), lwd=2, col="blue")
legend("topleft", c("Available", "Sightings"), lwd=c(2,2),, col=c("red","blue"), bty="n");box()
arrows(x0= median(pca$x[,1]),y0=-2, x1=median(pca$x[,1]), y1=0.01, length=0, angle=0, col="red", lwd=2)
arrows(x0= median(pred1),y0=-2, x1=median(pred1), y1=0.01, length=0, angle=0, col="blue", lwd=2)


pred2 <- predict(pca,newdata=tiba_ocean[,vars])[,2]
hist(pca$x[,2], probability = T, col="white", border="white", ylim=c(0,0.4), xlim=c(-13,5),main="", xlab="PC2 score", cex.axis=1.3, cex.lab=1.3);lines(density(pca$x[,1]), lwd=2, col="red")
lines(density(pred2), lwd=2, col="blue");box()
arrows(x0= median(pca$x[,2]),y0=-2, x1=median(pca$x[,2]), y1=0.01, length=0, angle=0, col="red", lwd=2)
arrows(x0= median(pred2),y0=-2, x1=median(pred2), y1=0.01, length=0, angle=0, col="blue", lwd=2)

#Wilcoxon and KS tests
#prueba KS
ks.test(pca$x[,1], pred1)
wilcox.test(pred1, pca$x[,1])

ks.test(pca$x[,2], pred2)
wilcox.test(pred2, pca$x[,2])
