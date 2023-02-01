#Import the data  
speeches<-read.table("output_knime_v5.csv",sep="")

#Packages required
library(compositions)

speechesvf<-speeches
speechesvf[1:4]<-list(NULL) #the first four columnns are not informative

##-----------------------------------------------------------------------------------------------------------------------------##
##Exploratory Data Analysis
#Barplot
barplot(acomp(speechesvf),xlim=c(0,11))

#Elipses Plot from the mean and variance 
opar<-par(mar=c(3,3,1,1))
mean=acomp(c(1,2,3))
variance=ilrvar2clr(matrix(c(2,-1.5,-1.5,2),ncol=2))
plot(mean,phc=".")
for(p in c(0.5,1:9,9.5)/10){r=sqrt(qchisq(p=p,df=2))
+ ellipses(mean,variance,r,col="grey")}
xr=rnorm.acomp(n=30,mean=mean,var=variance)
plot(xr,add=TRUE,pch=19,cex=0.5)

#Ternary Diagram Matrices for Multidimensional Compositions
plot(speechesvfec,cex=0.5)
speechesvfec=acomp(speechesvf[,c(4,6,7,9,10)])
r=sqrt(qchisq(p=0.95,df=2))
mn=mean(speechesvfec)
vr=var(speechesvfec)
ellipses(mean=mn,var=vr,r=r,lwd=3,col="gray")

##-----------------------------------------------------------------------------------------------------------------------------##
## Compositional Clustering Analysis

"
    acomp - A class providing the means to analyse compositions in the philosophical framework of the Aitchison Simplex
    dist - Calculates a distance matrix from a data set
    hclust - Hierarchical cluster analysis. The way it performs the clustering can be specified using the method argument 
    locator - Reads the position of the graphics cursor when the mouse button is pressed.
    cutree - Cuts the result from hclust, into several groups either by specifying the desired number(s) of groups or the cut height(s).
"

#Ward Method
xc=acomp(speechesvf)
dd=dist(xc)
hc=hclust(dd,method="ward.D2")
# plot(hc)

h=locator(1)$y #grab y coordinate
rect.hclust(hc,h=h)
gw=cutree(hc,h=h)
# plot(speechesvfec,col=gw)

"----"

#Average linkage method
xc=acomp(speechesvf)
dd=dist(xc)
hc=hclust(dd,method="average")
# plot(hc)

h=locator(1)$y #grab y coordinate
rect.hclust(hc,h=h)
ga=cutree(hc,h=h)
# plot(speechesvfec,col=ga) 

"----"

#Complete linkage method      
xc=acomp(speechesvf)
dd=dist(xc)
hc=hclust(dd,method="complete")
# plot(hc)

h=locator(1)$y #grab y coordinate
rect.hclust(hc,h=h)
gc=cutree(hc,h=h)
plot(speechesvfec,col=gc)   

#Compositional Q-Mode Cluster Analysis
dd=as.dist(variation(x))
hc=hclust(dd,method="ward")
plot(hc)   

#Evaluting the similarity between methods.
"
adjustedTandIndex - Computes the adjusted Rand index comparing two classifications
"
adjustedRandIndex(gc,ga)
adjustedRandIndex(gw,ga)
adjustedRandIndex(gc,gw)

##-----------------------------------------------------------------------------------------------------------------------------##
##Symbolic Data Analysis

#Packages required
install.packages("Clamix")
library("Clamix")

#Import the data 
output_knime_v8 <- read_excel("C:/Users/Toshiba/OneDrive/Ambiente de Trabalho/2 ano/Tese/output knime v8.xlsx", 
+     sheet = "Sheet1")

# Create a Datalist
datalist<-list("M"=output_knime_v8[1],"F"=output_knime_v8[2:10])

# Create a Dataset (object of symData) from a named list of dataframes 
dataset<-create.symData(datalist,"pDist")

# Symbolic Data Clustering
"
hclustSO - Compute adapted hierarchical clustering for modal multi valued modal symbolic data
"
res<-hclustSO(dataset,type="d1w")
# plot(res)

#Determine the number of clusters making use of the Within Sum of Squares Plot
wss<-(nrow(output_knime_v8)-1)*sum(apply(output_knime_v8,2,var))
for (i in 2:15)wss[i]<- sum(hclustSO(dataset)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
+      ylab="Within groups sum of squares")


#Determine the number of clusters making use of Calinski Harabaz method

#Packages required
library(dplyr)
library(tidyr)

WSS <- function( data, groups )
" 
    Args:
    data - The data
    groups -  The clusters 

    Returns:
    WSS - Within Sum of Squares object 
"

{ k <- max(groups) 
   # loop through each groups (clusters) and obtain its within sum squared error 
    total <- lapply( 1:k, function(k)
    {
        # extract the data point within the cluster        cluster <- subset( data, groups == k)
         cluster <- subset( data, groups == k )
         distance <- Distance(cluster)
         return(distance)
     }) %>% unlist()
     
     return( sum(total) )
 }


Distance <- function(cluster)
" 
    Args:
    cluster - cluster defined above

    Returns:
    Distance - Returns the sum squared distance between every point and the center of that cluster
"

 {
     # the center of the cluster, mean of all the points
     center <- colMeans(cluster)
     
    # calculate the sum squared error between every point and the center of that cluster 
     distance <- apply( cluster, 1, function(row)
     {
         sum( ( row - center )^2 )
     }) %>% sum()
     
     return(distance)
 }

CHCriterion <- function( data, kmax, clustermethod, ...  )
"
    Args:
    data - dataset to be considered
    kmax - Maximum cluster number, calculates the Calinski-Harabasz index from 2 cluster to kmax
    clustermethod - kmeanspp or hclust 

    Returns:
    Data and plot
"
 {
     if( !clustermethod %in% c( "kmeanspp", "hclust" ) )
         stop( "method must be one of 'kmeanspp' or 'hclust'" )
     
     # total sum squared error (independent with the number of cluster k)
     tss <- Distance( cluster = data )
     
    # initialize a numeric vector storing the score
     wss <- numeric(kmax)
     
     # k starts from 2, cluster 1 is meaningless
     if( clustermethod == "kmeanspp" )
     {
         for( k in 2:kmax )
         {
             results <- Kmeanspp( data, k, ... )
             wss[k]  <- results$tot.withinss 
         }		
     }else # "hclust"
     {
         d <- dist( data, method = "euclidean" )
         clustering <- hclust( d, ... )
         for( k in 2:kmax )
         {
             groups <- cutree( clustering, k )
             wss[k] <- WSS( data = data, groups =  groups )
         }
     }		
     
     # between sum of square
     bss <- tss - wss[-1]
     
    # cluster count start from 2! 
     numerator <- bss / ( 1:(kmax-1) )
     denominator <- wss[-1] / ( nrow(data) - 2:kmax )
     
     criteria <- data.frame( k = 2:kmax,
                             CHIndex = numerator / denominator,
                             wss = wss[-1] )
    
    # convert to long format for plotting 
     criteria_long <- gather( criteria, "index", "value", -1 )
     
     plot <- ggplot( criteria_long, aes( k, value, color = index ) ) + 
         geom_line() + geom_point( aes( shape = index ), size = 3 ) +
         facet_wrap( ~ index, scale = "free_y" ) + 
         guides( color = FALSE, shape = FALSE )
     
     return( list( data = criteria, 
                   plot = plot ) )
 }

# CH Criterion computation considering different parameters
criteria <- CHCriterion(data = output_knime_v8_scale, kmax = 10, 
+                         clustermethod = "hclust", method = "ward.D")

criteria <- CHCriterion(data = output_knime_v8_scale, kmax = 6, 
+                         clustermethod = "hclust", method = "single")
criteria$plot
criteria <- CHCriterion(data = output_knime_v8_scale, kmax = 6, 
+                         clustermethod = "hclust", method = "average")
criteria$plot
criteria <- CHCriterion(data = output_knime_v8_scale, kmax = 6, 
+                         clustermethod = "hclust", method = "complete")
criteria$plot



#Determine the number of clusters making use of the Silhouette method
"
    Args: 
    x - matrix of the data
    k - the number of clusters to be generated.
    hc_method - the agglomeration method to be used 
    hc_metric - character string specifying the metric to be used for calculating dissimilarities between observations
"
Ward_HC<-function(x,k){hcut(x,k, hc_method ="ward.D2" , hc_metric="euclidian")}
#Determines and visualize the optimal number of clusters using Silhouette method
fviz_nbclust(as.matrix(output_knime_v8_scale), Ward_HC, method = "silhouette",k.max=20)
#k=10 

Average_HC<-function(x,k){hcut(x,k, hc_method ="average" , hc_metric="euclidian")}
#Determines and visualize the optimal number of clusters using Silhouette method
fviz_nbclust(as.matrix(output_knime_v8_scale), Average_HC, method = "silhouette",k.max=20)
#k=10 

Complete_HC<-function(x,k){hcut(x,k, hc_method ="complete" , hc_metric="euclidian")}
#Determines and visualize the optimal number of clusters using Silhouette method
fviz_nbclust(as.matrix(output_knime_v8_scale), complete_HC, method = "silhouette",k.max=20)
#K=2 

