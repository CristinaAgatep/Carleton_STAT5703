### STAT 5703 - FINAL PROJECT 
# HEART DATASET

# orange = low risk of heart attack
# blue = high risk of heart attack

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
head(heart.eig, 10) ##### Patric: Check these and make sure they are the same as what is in my document

fviz_screeplot(heart.famd, main='') ###### You can check these too

#### I still can't figure out what the triangles are doing!!!! There are 16 of them. 
# If I can't figure it out worst case scen I'll just plot it using ggplot and remove the triangles...
fviz_famd_ind(heart.famd, label = "none", 
              habillage = "class", palette = palette, # color by groups 
              repel = TRUE, alpha.ind = 0.5) + 
  theme(text = element_text(size=20), axis.text.x = element_text(size=20), axis.text.y = element_text(size=20))


fviz(heart.famd, "var") # Variable plot
fviz(heart.famd, "ind", habillage = 'class', label = 'none') #Individuals plot

# individual coordinates for dimensions
famd.coords = data.frame(heart.famd$ind$coord)


####### K-Prototype ######
set.seed(123)
heart.kproto = data |> select(-class)

k = 10 # Max number of clusters to try
kproto.wss = data.frame(matrix(data=NA, nrow = k, ncol=2))
colnames(kproto.wss) = c('clusters', 'wss')

for (i in 2:k){ 
    kproto.wss[i,1] = i
    kproto.wss[i,2] <- sum(kproto(heart.kproto, i)$withinss)
}

kproto.wss |>
  ggplot(aes(x=clusters, y=wss)) + 
  geom_point() + 
  geom_line() + 
  scale_x_continuous(breaks=1:k) +
  xlab('Number of Clusters') + ylab('Within Sum of Squares')

# FAMD coordinates for each dimension
famd.coords = data.frame(heart.famd$ind$coord)

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
    scale_fill_manual(values = palette)
  
  return(clust.plot)
}

num.clust(2)



