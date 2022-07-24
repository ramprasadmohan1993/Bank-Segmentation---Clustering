getwd()
setwd("D:/R_wd")
########################################################################################################
#                                                                                                      #
#                         Purpose:       ML Project - Clustering                                       #
#                                                                                                      #
#                         Author:        Ramprasad                                                     #
#                         Contact:       89396 ******                                                  #
#                         Client:        Great Learning                                                #
#                                                                                                      #
#                         Code created:  2020-08-16                                                    #
#                         Last updated:  2020-08-16                                                    #
#                         Source:        D:/R_wd                                                       #
#                                                                                                      #
########################################################################################################

pacman::p_load(readr,dendextend,ggplot2,cluster,funModeling,NbClust,DataExplorer,psych,MLmetrics,plotly,mvoutlier,factoextra,summarytools)

bank <- read.csv(file.choose(),header = TRUE)

class(bank)
head(bank)

bank$spending <- (bank$spending * 1000)
bank$advance_payments <- (bank$advance_payments * 100)
bank$current_balance <- (bank$current_balance * 1000)
bank$credit_limit <- (bank$credit_limit * 10000)
bank$min_payment_amt <- (bank$min_payment_amt * 100)
bank$max_spent_in_single_shopping <- (bank$max_spent_in_single_shopping * 1000)

summarytools::view(dfSummary(bank))

DataExplorer::plot_missing(bank)
funModeling::plot_num(bank)
psych::pairs.panels(bank,hist.col = "blue")
mvoutlier::uni.plot(bank,symb = TRUE)
boxplot(bank,horizontal = TRUE)

bankscaled <- scale(bank,center = TRUE,scale = TRUE)
head(bankscaled)

mvoutlier::uni.plot(bankscaled,symb = TRUE)
boxplot(bankscaled)


##############################################################################################################################
#                                                                                                                            #
#                                          Hierarchical Clustering part                                                      #
#                                                                                                                            #
##############################################################################################################################
## First step in hierarchical clustering is get pairwise distance ##

distance <- dist(bankscaled,method = "euclidean")
summary(distance)

### --------------------------------- Method to assess distance metric and linkage method----------------------#####

a <- (Complete <- agnes(bankscaled,method = "complete",metric = "euclidean"))$ac
b<- (Complete <- agnes(bankscaled,method = "average",metric = "euclidean"))$ac
c<- (Complete <- agnes(bankscaled,method = "ward",metric = "euclidean"))$ac
d<- (Complete <- agnes(bankscaled,method = "weighted",metric = "euclidean"))$ac

e<- (Complete <- agnes(bankscaled,method = "complete",metric = "manhattan"))$ac
f<- (Complete <- agnes(bankscaled,method = "average",metric = "manhattan"))$ac
g<- (Complete <- agnes(bankscaled,method = "ward",metric = "manhattan"))$ac
h<- (Complete <- agnes(bankscaled,method = "weighted",metric = "manhattan"))$ac

i<-(Complete <- agnes(bankscaled,method = "complete",metric = "minkowski"))$ac
j<-(Complete <- agnes(bankscaled,method = "average",metric = "minkowski"))$ac
k<-(Complete <- agnes(bankscaled,method = "ward",metric = "minkowski"))$ac
l<-(Complete <- agnes(bankscaled,method = "weighted",metric = "minkowski"))$ac

temp <- c(a,b,c,d,e,f,g,h,i,j,k,l)

plot(temp,ylab = "agglomerative coefficients", main = "Plot to determine best distance - linkage method",xlab="Methods index",col="Red",pch=16)
lines(temp,col="blue")
abline(v=3,col="green",lty=3)


## ----------------building hclust model ------------------------------###

hierarclust <- hclust(distance,method = "ward.D")

hierarclustagnes <- agnes(x = bankscaled,stand = TRUE,metric = "euclidean",method = "average")
fviz_dend(hierarclustagnes)

summary(hierarclust)

plot(hierarclust)

## Using Nbclust -------------------------------------------------------------------------------------------####


hierarnbcl <- NbClust(data = bankscaled,distance = "euclidean",min.nc = 2,max.nc = 10,method = "ward.D")

table(hierarnbcl$Best.nc[1,])

barplot(table(hierarnbcl$Best.nc[1,]))

### -------------------------------------------------------------------------------------------------------####

clusterheights<- hierarclust$height

clusterheights<-sort(clusterheights,decreasing = TRUE)

plot(clusterheights,col="blue")
qplot(clusterheights)


# Now to visualize how clusters would look like ###

plot(hierarclust)
rect.hclust(hierarclust,k = 3,border = c("red","blue","green"))

clusters <- cutree(hierarclust,k = 3)

newbank <- cbind(bank,clusters)

## visualizing the clusters ###

cluster::clusplot(newbank[,-8],newbank$clusters,color = TRUE,shade=TRUE,labels=2,lines=1,main="Hierarcical cluster plot")
factoextra::fviz_cluster(hierarclust,data = bankscaled,ellipse.type = "convex",palette = "jco",repel = TRUE)
with(newbank,pairs(newbank[,-8], col = c(1:3)[newbank$clusters],upper.panel = NULL))

## Next do the profiling for the clusters which is IMPORTANT STEP###

aggr <- aggregate(newbank[,-c(8)],list(newbank$clusters),mean)

aggr

sil <- silhouette(newbank$clusters,dist = dist(newbank))
plot(sil,col=c("blue","red","chartreuse3"))
factoextra::fviz_silhouette(sil,label = FALSE, print.summary = TRUE)

summary(sil)
class(sil)


##############################################################################################################
#                                                                                                            #
#                               K - MEANS CLUSTERING                                                         #
#                                                                                                            #
#############################################################################################################

factoextra::fviz_nbclust(bankscaled, kmeans,method = "wss")
factoextra::fviz_nbclust(bankscaled, kmeans,method = "silhouette")
factoextra::fviz_nbclust(bankscaled, kmeans,method = "gap_stat")

#--------------------------Silhoutte score part-------------------------------------------------------------######

silhttescore <- function(k)
{
  km <- kmeans(bankscaled,centers = k,nstart = 50)
  ss <- silhouette(km$cluster, dist(bankscaled))
  mean(ss[,3])
}
k <- 2:10
avgsil <- sapply(k,silhttescore)
plot(k,type = 'b', avgsil,xlab = "Number of clusters",ylab = "Average Silhouette Scores",main="Silhoutte Score plot")
abline(v=5,lty=5)

##-------------------------------WSS plot------------------------------------------------------------------####


wssplot <- function(data , nc = no.of.clusters, seed = 123)
{
  wss<- c()
  for (i in 1:nc)
  {
    set.seed(seed)
    wskm <- kmeans(data,centers = i)
    wss[i] <- wskm$withinss
  }
plot(1:nc, wss, type = "b",xlab = "Number of Clusters",ylab = "Within Sum of Squares",main = "WSS Plot")
abline(v=3,lty=5)
}

wssplot(bankscaled,nc = 8)

### ----------------------Confirming Using Nbclust--------------------------------------------#######

kmnsnb <- NbClust(bankscaled,min.nc = 2,max.nc = 15,method = "kmeans")

table(kmnsnb$Best.nc[1,])

barplot(table(kmnsnb$Best.nc[1,]))

##------------------------------------------------------------------------------------------------######

set.seed(123)
kmeans.final <- kmeans(x = bankscaled,centers = 3,nstart = 50,iter.max = 100)


factoextra::fviz_cluster(kmeans.final,data = bankscaled,ellipse.type = "convex",palette = "jco",repel = TRUE)
cluster::clusplot(bank.kmeans[,-8],bank.kmeans$ClusterMapping,color = TRUE,shade=TRUE,labels=2,lines=1,main="K-means cluster plot")
with(bank.kmeans,pairs(bank.kmeans[,-8], col = c(1:3)[bank.kmeans$ClusterMapping],upper.panel = NULL))

### --------------------- K - means profiling --------------------------------------###########3

bank.kmeans <- bank

bank.kmeans$ClusterMapping <- kmeans.final$cluster

silkmeans <- silhouette(bank.kmeans$ClusterMapping,dist = dist(bank.kmeans))
factoextra::fviz_silhouette(silkmeans)
plot(silkmeans,col=c("blue","red","chartreuse3"))

aggrkmeans <- aggregate(bank.kmeans[,-c(8)],list(bank.kmeans$ClusterMapping),mean)


kmeans.group1 <- subset(bank.kmeans, bank.kmeans$ClusterMapping ==1 )
kmeans.group2 <- subset(bank.kmeans, bank.kmeans$ClusterMapping ==2 )
kmeans.group3 <- subset(bank.kmeans, bank.kmeans$ClusterMapping ==3 )

bank.kmeans$ClusterMapping <- as.character(bank.kmeans$ClusterMapping)

esquisse::esquisser()
