### STAT 5703 - FINAL PROJECT 
# HEART DATASET

suppressPackageStartupMessages(library(kmed))
suppressPackageStartupMessages(library(VGAM))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(FactoMineR))
suppressPackageStartupMessages(library(factoextra))
suppressPackageStartupMessages(library(clustMixType))
suppressPackageStartupMessages(library(ISLR))
suppressPackageStartupMessages(library(randomForest))
suppressPackageStartupMessages(library(rtkore))
suppressPackageStartupMessages(library(Rcpp))


# Data manipulation #### Patric this is the same as what you gave me so you don't have to copy this
data = heart
data$class = as.factor(ifelse(data$class>0,1,0)) # Convert to binary problem then factor
data$sex<-as.factor(as.numeric((data$sex)))
data$fbs<-as.factor(as.numeric((data$fbs)))
data$exang<-as.factor(as.numeric((data$exang)))


####### FAMD #######
palette<-c("#D16103","#4E84C4") # Colour palette for consistency

heart.famd = FAMD(data, sup.var = 14, ncp = 20, graph = FALSE) # Creates FAMD coordinates
summary(heart.famd)
heart.eig <- get_eigenvalue(heart.famd) # Eigenvalues
head(heart.eig, 10) 

fviz_screeplot(heart.famd, main='') 

fviz(heart.famd, "var") # Variable plot
fviz(heart.famd, "ind", habillage = 'class', label = 'none') +
  scale_color_manual(values = palette) #Individuals plot

# individual coordinates for dimensions
famd.coords = data.frame(heart.famd$ind$coord)


####### K-Prototype ######
set.seed(123)
heart.kproto = data |> select(-class) # removing class (response) variable

k = 10 # Max number of clusters to try
kproto.wss = data.frame(matrix(data=NA, nrow = k, ncol=2)) # Empty df for WSS results
colnames(kproto.wss) = c('clusters', 'wss')

# Calculate WSS for 2-k number of clusters 
for (i in 2:k){ 
    kproto.wss[i,1] = i
    kproto.wss[i,2] <- sum(kproto(heart.kproto, i)$withinss)
}

# WSS graph
kproto.wss |>
  ggplot(aes(x=clusters, y=wss)) + 
  geom_point() + 
  geom_line() + 
  scale_x_continuous(breaks=1:k) +
  xlab('Number of Clusters') + ylab('Within Sum of Squares')


##### Plotting of clusters ####

num.clust = function(k){
  clust.temp = kproto(heart.kproto, k)
  
  clust = cbind(famd.coords[,1:2], data.frame(clust.temp$cluster), data$class)
  colnames(clust) = c('Dim.1', 'Dim.2', 'Cluster', 'output')
  clust$Cluster = factor(clust$Cluster)
  
  clust.plot = clust |>
    ggplot(aes(x=Dim.1, y=Dim.2, col=Cluster))  +
    geom_vline(xintercept=0) +
    geom_hline(yintercept=0) +
    geom_point() + 
    ggtitle('8 Clusters')
  
  return(clust.plot)
}

num.clust(8)




