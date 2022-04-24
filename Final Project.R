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

data = read.csv('/Users/cristinaagatep/Desktop/School/STAT5703/Assignments/Research Project/heart.csv',
                      header=TRUE)

summary(data) # Summarize to look for discrepancies

# Note that caa has 5 values of '4' which don't correspond to anything in the metadata, we should get rid of these guys
table(data$caa)
# Same with 2 individuals with values of 0 under thall, we eliminate these individuals since this is meaningless, and we want to avoid imputation
table(data$thall)

data<-data[data$caa!=4,]
data<-data[data$thall!=0,] # Get's rid of undesirable values

data$cp<-as.factor(data$cp)
data$restecg<-as.factor(data$restecg)
data$slp<-as.factor(data$slp)
data$thall<-as.factor(data$thall)

# Convert the binary factors as well
data$sex<-as.factor(data$sex)
data$fbs<-as.factor(data$fbs)
data$exng<-as.factor(data$exng)
data$output<-as.factor(data$output) # Convert the output to factor as well


####### FAMD #######

heart.famd = FAMD(data, sup.var = 14, ncp = 20, graph = FALSE)
summary(heart.famd)
heart.eig <- get_eigenvalue(heart.famd)
head(heart.eig, 10)

fviz_screeplot(heart.famd)

fviz_famd_ind(heart.famd, label = "none", 
              habillage = "output", palette = c("red", "blue"), # color by groups 
              repel = TRUE, alpha.ind = 0.5) + 
  theme(text = element_text(size=20), axis.text.x = element_text(size=20), axis.text.y = element_text(size=20))

fviz(heart.famd, "var")
fviz(heart.famd, "ind", habillage = 'output', label = 'none')


####### K-Prototype ######
set.seed(123)
heart.kproto = data |> select(-output)

k = 10 # Number of clusters
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


famd.coords = data.frame(heart.famd$ind$coord)

k = 3
clust.temp = kproto(heart.kproto, k)

clust = cbind(famd.coords[,1:2], data.frame(clust.temp$cluster), data$output)
colnames(clust) = c('Dim.1', 'Dim.2', 'Cluster', 'output')
clust$Cluster = factor(clust$Cluster)

clust |>
  ggplot(aes(x=Dim.1, y=Dim.2, col=Cluster)) +
  geom_point()



######## FAMD supervised ########
set.seed(100)
test_indicies<-sample(c(1:296),ceiling(0.2*296),replace = F)

famd.set = cbind(famd.coords[,1:7], data$output) |>
  rename(output = `data$output` )

test_set<-famd.set[test_indicies,]
train_set<-famd.set[-test_indicies,]
dim(test_set) # Should have 60 rows
dim(train_set) # Should have 297-60 = 237 rows
intersect(rownames(test_set),rownames(train_set)) # Should be empty



model1<-glm(output ~ .,data = train_set,family = binomial)
summary(model1)

table(pred = predict(model1,type='response')>0.5,obs = train_set$output)



set.seed(105)
model6.1.famd = randomForest(output ~ ., data=train_set, importance=T)
varImpPlot(model6.1.famd) ## FIGURE

pred.mod6.1.famd<-predict(model6.1.famd,newdata=test_set,type="class")
table(predictions = pred.mod6.1.famd,observed = test_set[,8])


