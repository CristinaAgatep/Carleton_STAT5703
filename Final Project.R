### STAT 5703 - FINAL PROJECT 
# HEART DATASET

# convert slp to factor


suppressPackageStartupMessages(library(kmed))
suppressPackageStartupMessages(library(VGAM))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(FactoMineR))
suppressPackageStartupMessages(library(factoextra))


heart.data = heart |>
  mutate(class.bin = ifelse(class == 0, 0, 1)) |>
  select(-class)

# an eigenvalue > 1 indicates that the principal component (PCs) 
# accounts for more variance than accounted by one of the original 
# variables in standardized data (N.B. This holds true only when the 
# data are standardized.)

str(heart.data)

heart.famd = FAMD(heart.data, sup.var = 14, ncp = 20, graph = FALSE)
heart.eig <- get_eigenvalue(heart.famd)
head(heart.eig)

fviz_screeplot(heart.famd)

fviz_famd_ind(heart.famd, label = "none", 
              habillage = "class.bin", palette = c("#00AFBB", "#FC4E07"), # color by groups 
              repel = TRUE, alpha.ind = 0.5) + 
  theme(text = element_text(size=20), axis.text.x = element_text(size=20), axis.text.y = element_text(size=20))





categorical_features_idx = [2, 3, 4, 9]

kproto = KPrototypes(n_clusters=2, verbose=2, max_iter=20).fit(mark_array, categorical=categorical_features_idx)



