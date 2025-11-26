library(readr)
library(NbClust)

#iris
table(iris$Species)
df <- iris[-5]
df <- scale(df)

set.seed(1)
NbClust(df, distance = "euclidean", min.nc = 2, max.nc = 10, 
        method = "kmeans", index = "ch")$Best.nc

NbClust(df, distance = "euclidean", min.nc = 2, max.nc = 10, 
        method = "kmeans", index = "db")$Best.nc

NbClust(df, distance = "euclidean", min.nc = 2, max.nc = 10, 
        method = "kmeans", index = "dunn")$Best.nc

NbClust(df, distance = "euclidean", min.nc = 2, max.nc = 10, 
        method = "kmeans", index = "silhouette")$Best.nc

k_ASI(as.data.frame(df), kmin = 2, kmax = 10, 
      algorithm = "kmeans")$Best_nc


#seeds
seeds <- read_table("real_dataset/seeds_dataset.txt", 
                    col_names = FALSE)

table(seeds[8])
df <- seeds[-8]
cor(df)
df_pca <- prcomp(df, center = TRUE, scale. = TRUE) # 2 pc is ok
summary(df_pca)
df <- predict(df_pca)[,1:2]

df <- scale(df)

set.seed(1)
NbClust(df, distance = "euclidean", min.nc = 2, max.nc = 10, 
        method = "kmeans", index = "ch")$Best.nc

NbClust(df, distance = "euclidean", min.nc = 2, max.nc = 10, 
        method = "kmeans", index = "db")$Best.nc

NbClust(df, distance = "euclidean", min.nc = 2, max.nc = 10, 
        method = "kmeans", index = "dunn")$Best.nc

NbClust(df, distance = "euclidean", min.nc = 2, max.nc = 10, 
        method = "kmeans", index = "silhouette")$Best.nc

k_ASI(as.data.frame(df), kmin = 2, kmax = 10, 
      algorithm = "kmeans")$Best_nc

#Wholesale customers
wholesale <- read_csv("real_dataset/Wholesale customers data.csv", 
                      col_names = TRUE)
table(wholesale[2])
df <- wholesale[-2]
cor(df)
df_pca <- prcomp(df, center = TRUE, scale. = TRUE) # 3 pc is ok
summary(df_pca)
df <- predict(df_pca)[,1:3]

df <- scale(df)

set.seed(1)
NbClust(df, distance = "euclidean", min.nc = 2, max.nc = 10, 
        method = "kmeans", index = "ch")$Best.nc

NbClust(df, distance = "euclidean", min.nc = 2, max.nc = 10, 
        method = "kmeans", index = "db")$Best.nc

NbClust(df, distance = "euclidean", min.nc = 2, max.nc = 10, 
        method = "kmeans", index = "dunn")$Best.nc

NbClust(df, distance = "euclidean", min.nc = 2, max.nc = 10, 
        method = "kmeans", index = "silhouette")$Best.nc

k_ASI(as.data.frame(df), kmin = 2, kmax = 10, 
      algorithm = "kmeans")$Best_nc


#wine
wine <- read_csv("real_dataset/wine.data", 
                 col_names = FALSE)

table(wine[,1])
df <- wine[-1]
df <- scale(df)

set.seed(1)
NbClust(df, distance = "euclidean", min.nc = 2, max.nc = 10, 
        method = "kmeans", index = "ch")$Best.nc

NbClust(df, distance = "euclidean", min.nc = 2, max.nc = 10, 
        method = "kmeans", index = "db")$Best.nc

NbClust(df, distance = "euclidean", min.nc = 2, max.nc = 10, 
        method = "kmeans", index = "dunn")$Best.nc

NbClust(df, distance = "euclidean", min.nc = 2, max.nc = 10, 
        method = "kmeans", index = "silhouette")$Best.nc

k_ASI(as.data.frame(df), kmin = 2, kmax = 10, 
      algorithm = "kmeans")$Best_nc


# Page Blocks
page <- read.table("real_dataset/page-blocks.data",header = F,
                   sep="")

table(page$V11)
df <- page[-11]
df <- df[complete.cases(df),]
cor(df)
df_pca <- prcomp(df, center = TRUE, scale. = TRUE) # 4 pc is ok
summary(df_pca)
df <- predict(df_pca)[,1:4]

df <- scale(df)

set.seed(1)
NbClust(df, distance = "euclidean", min.nc = 2, max.nc = 10, 
        method = "kmeans", index = "ch")$Best.nc

NbClust(df, distance = "euclidean", min.nc = 2, max.nc = 10, 
        method = "kmeans", index = "db")$Best.nc

NbClust(df, distance = "euclidean", min.nc = 2, max.nc = 10, 
        method = "kmeans", index = "dunn")$Best.nc

NbClust(df, distance = "euclidean", min.nc = 2, max.nc = 10, 
        method = "kmeans", index = "silhouette")$Best.nc

k_ASI(as.data.frame(df), kmin = 2, kmax = 10, 
      algorithm = "kmeans")$Best_nc

#breast tissue
breastissue <- read_table("real_dataset/breastissue.data", 
                          col_names = T)

table(breastissue$Class)
df <- breastissue[-1]

cor(df)
df_pca <- prcomp(df, center = TRUE, scale. = TRUE) # 3 pc is ok
summary(df_pca)
df <- predict(df_pca)[,1:3]

df <- scale(df)

set.seed(1)
NbClust(df, distance = "euclidean", min.nc = 2, max.nc = 10, 
        method = "kmeans", index = "ch")$Best.nc

NbClust(df, distance = "euclidean", min.nc = 2, max.nc = 10, 
        method = "kmeans", index = "db")$Best.nc

NbClust(df, distance = "euclidean", min.nc = 2, max.nc = 10, 
        method = "kmeans", index = "dunn")$Best.nc

NbClust(df, distance = "euclidean", min.nc = 2, max.nc = 10, 
        method = "kmeans", index = "silhouette")$Best.nc

k_ASI(as.data.frame(df), kmin = 2, kmax = 10, 
      algorithm = "kmeans")$Best_nc

#glass identification
glass <- read_csv("real_dataset/glass.data", 
                  col_names = FALSE)

table(glass$X11)
df <- glass[-c(1,11)]
df <- scale(df)

set.seed(1)
NbClust(df, distance = "euclidean", min.nc = 2, max.nc = 10, 
        method = "kmeans", index = "ch")$Best.nc

NbClust(df, distance = "euclidean", min.nc = 2, max.nc = 10, 
        method = "kmeans", index = "db")$Best.nc

NbClust(df, distance = "euclidean", min.nc = 2, max.nc = 10, 
        method = "kmeans", index = "dunn")$Best.nc

NbClust(df, distance = "euclidean", min.nc = 2, max.nc = 10, 
        method = "kmeans", index = "silhouette")$Best.nc

k_ASI(as.data.frame(df), kmin = 2, kmax = 10, 
      algorithm = "kmeans")$Best_nc

#ecoli
ecoli <- read_table("real_dataset/ecoli.data", 
                    col_names = FALSE)
colnames(ecoli) <- c("squence_name","mcg","gvh","lip","chg","aac","alm1","alm2","site")
table(ecoli$site)
df <- ecoli[-c(1,9)]

df <- scale(df)

set.seed(1)
NbClust(df, distance = "euclidean", min.nc = 2, max.nc = 10, 
        method = "kmeans", index = "ch")$Best.nc

NbClust(df, distance = "euclidean", min.nc = 2, max.nc = 10, 
        method = "kmeans", index = "db")$Best.nc

NbClust(df, distance = "euclidean", min.nc = 2, max.nc = 10, 
        method = "kmeans", index = "dunn")$Best.nc

NbClust(df, distance = "euclidean", min.nc = 2, max.nc = 10, 
        method = "kmeans", index = "silhouette")$Best.nc

k_ASI(as.data.frame(df), kmin = 2, kmax = 10, 
          algorithm = "kmeans")$Best_nc

#yeast
yeast <- read_table("real_dataset/yeast.data", 
                 col_names = FALSE)

table(yeast$X10)
df <- yeast[-c(1,10)]
cor(df)
df_pca <- prcomp(df, center = TRUE, scale. = TRUE) # 4 pc is ok
summary(df_pca)
df <- predict(df_pca)[,1:4]

set.seed(1)
NbClust(df, distance = "euclidean", min.nc = 2, max.nc = 10, 
        method = "kmeans", index = "ch")$Best.nc

NbClust(df, distance = "euclidean", min.nc = 2, max.nc = 10, 
        method = "kmeans", index = "db")$Best.nc

NbClust(df, distance = "euclidean", min.nc = 2, max.nc = 10, 
        method = "kmeans", index = "dunn")$Best.nc

NbClust(df, distance = "euclidean", min.nc = 2, max.nc = 10, 
        method = "kmeans", index = "silhouette")$Best.nc

k_ASI(as.data.frame(df), kmin = 2, kmax = 10, 
          algorithm = "kmeans")$Best_nc


#Pen-based Recognition of Handwritten Digits
digits <- read_csv("real_dataset/pendigits.tra", 
                      col_names = F)

table(digits$X17)
df <- digits[-17]
df <- df[complete.cases(df),]
cor(df)
df_pca <- prcomp(df, center = TRUE, scale. = TRUE) # 5 pc is ok
summary(df_pca)
df <- predict(df_pca)[,1:5]

df <- scale(df)

set.seed(1)
NbClust(df, distance = "euclidean", min.nc = 2, max.nc = 10, 
        method = "kmeans", index = "ch")$Best.nc

NbClust(df, distance = "euclidean", min.nc = 2, max.nc = 10, 
        method = "kmeans", index = "db")$Best.nc

NbClust(df, distance = "euclidean", min.nc = 2, max.nc = 10, 
        method = "kmeans", index = "dunn")$Best.nc

NbClust(df, distance = "euclidean", min.nc = 2, max.nc = 10, 
        method = "kmeans", index = "silhouette")$Best.nc

k_ASI(as.data.frame(df), kmin = 2, kmax = 10, 
          algorithm = "kmeans")$Best_nc

