nALLcl_1520_dtm_int_gmf<- ALLcl_1520_dtm_int_gmf[complete.cases(ALLcl_1520_dtm_int_gmf[,6:7]),]

w1520 <- dcast(ALLcl_1520_dtm_int_gmf, X ~ Source, value.var="avgabsdev")

wcavga$CLUST <- wideavgabs$classification

selclust <- wcavga[complete.cases(wcavga[,2:4,17]),]

wcavga$CLUST <- wideavgabs$classification



> cent1520.temp<-cent1520[setdiff(colnames(cent1520),c())]
> selclust.temp<-selclust[setdiff(colnames(selclust),c())]
> data.merged<-merge(cent1520.temp,selclust.temp,by.x=c("X"),by.y=c("X"),incomparables = NA,all.x =F,all.y =F)
> rm(list=c("cent1520.temp","selclust.temp"))
> write.csv(data.merged,'/home/xub12/Clust_Test/test/cluster_out2.csv')




ggplot (cluster_out2, aes (x = X, y = Y, colour = as.factor(CLUST2))+ geom_point(shape=1)

pcaclust.stand <- as.data.frame(scale(cluster_out2[,7:21]))

sapply(pcaclust.stand,sd) #now, standard deviations are 1

#If we use prcomp() function, we indicate 'scale=TRUE' to use correlation matrix
pca <- prcomp(pcaclust.stand,scale=T)
#it is just the same that: prcomp(iris[,1:4],scale=T) and prcomp(pcaclust.stand)
#similar with princomp(): princomp(pcaclust.stand, cor=T)
pca
summary(pca)
#This gives us the standard deviation of each component, and the proportion of variance explained by each component.
#The standard deviation is stored in (see 'str(pca)'):
pca$sdev

plot(pca)
#biplot of first two principal components
biplot(pca,cex=0.8)
abline(h = 0, v = 0, lty = 2, col = 8)
# Do some things that will print only to console
sink("pca_out1.txt")
# Do my pca codecolnames(com  )
print(t(pca$rotation[,1:3]), digits =2)
print("This is my first pca output")
sink()

componentspca = pca$rotation
sorted.loadings = componentspca[order(componentspca[,1]),1]
Main = "Loadings PCA1"
xlabs="Variable Loadings"
dotplot(sorted.loadings, main=Main, xlabs=xlabs, cex=1.5, col='red')


componentspca = pca$rotation
sorted.loadings2 = componentspca[order(componentspca[,2]),2]
Main2 = "Loadings PCA2"
xlabs="Variable Loadings"
dotplot(sorted.loadings2, main=Main2, xlabs=xlabs, cex=1.5, col='red')


componentspca = pca$rotation
sorted.loadings2 = componentspca[order(componentspca[,3]),3]
Main3 = "Loadings PCA3"
xlabs="Variable Loadings"
dotplot(sorted.loadings2, main=Main3, xlabs=xlabs, cex=1.5, col='red')

colnames(cluster_out2)

cluster_out2$CLUST2 <- as.factor(cluster_out2$CLUST2)

treeclust <- rpart(CLUST2 ~ DTM2p4m_anl_par_poly_15mr20msp  + Gmf0p78mCL_anl_par_poly_15mr20msp + Gmf2p4mCL_anl_par_poly_15mr20msp  + Gmf5mCL_anl_par_poly_15mr20msp + Int0p78mCL_anl_par_poly_15mr20msp + Int5mCL_anl_par_poly_15mr20msp, data=cluster_out2, method="class")

plot(treeclust)

wideall_1520 <- recast(ALLcl_1520_dtm_int_gmf, X ~ Source + variable, measure.var=c("min", "max", "avg", "var", "sdev", "avgabsdev"))

wideall_1520 <- wideall_1520[complete.cases(wideall_1520[,6:8]),]

######"DTM0p78m_anl_par_poly_15mr20msp", "DTM1p1m_anl_par_poly_15mr20msp", "DTM1p7m_anl_par_poly_15mr20msp", "DTM2p4m_anl_par_poly_15mr20msp", "DTM5m_anl_par_poly_15mr20msp", "Gmf0p78mCL_anl_par_poly_15mr20msp", "Gmf1p1mCL_anl_par_poly_15mr20msp", "Gmf1p7mCL_anl_par_poly_15mr20msp", "Gmf2p4mCL_anl_par_poly_15mr20msp", "Gmf5mCL_anl_par_poly_15mr20msp", "Int0p78mCL_anl_par_poly_15mr20msp", "Int1p1mCL_anl_par_poly_15mr20msp", "Int1p7mCL_anl_par_poly_15mr20msp", "Int2p4mCL_anl_par_poly_15mr20msp", "Int5mCL_anl_par_poly_15mr20msp")

###############################################################################
#' Replace NA, NaN, Inf values
#'
#' This function will replace all NA, NaN, Inf with given values
#'
#' @param x data to check for NA, NaN, Inf
#' @param y values(s) to be used in place of NA, NaN, Inf
#'
#' @return updated data
#'
#' @examples
#' \dontrun{ 
#' ifna(c(1,NA,2,Inf,3), 4)
#' }
#' @export 
###############################################################################
ifna <- function
(
  x,	# check x for NA, NaN, Inf
  y	# if found replace with y
) 
{ 	
  return(iif(is.na(x) | is.nan(x) | is.infinite(x), y, x))
}

#' @export 
fast.na.omit <- function
(
  x
) 
{
  x[!is.na(x)]
}