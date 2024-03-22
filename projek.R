data("USArrests")
str(USArrests)
View(USArrests)


dat <- read.table("D:/Datasets/kemiskinan.txt", header = TRUE)
View(dat)
df <- na.omit(dat)
str(df)
summary(df)

#Scaling/Standardizing the data
dfs <- scale(df)
head(dfs, n=3)

# Dissimilarity matrix
d <- dist(dfs, method = "euclidean")
# Hierarchical clustering using Complete Linkage
hc1 <- hclust(d, method = "average" )
# Plot the obtained dendrogram
plot(hc1, cex = 0.6, hang = -1)
rect.hclust(hc1, k = 4, border = 2:5)

#Compute cophentic distance
res.coph <- cophenetic(hc1)
# Correlation between cophenetic distance and the original distance
cor(d, res.coph)

fviz_dend(hc1, k = 4, # Cut in four groups
          cex = 0.5, # label size
          k_colors = c("#2E9FDF", "#006400", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE, # Add rectangle around groups
          rect_border = c("#2E9FDF", "#006400", "#E7B800", "#FC4E07"),
          rect_fill = TRUE)


library(igraph)
fviz_dend(hc1, k = 4, k_colors = "jco",
            type = "circular", repel = TRUE)

library(factoextra)
fviz_nbclust(dfs, FUN = hcut, method = "wss")

grp <- cutree(hc1, k = 4)
fviz_cluster(list(data = df, cluster = grp),
               palette = c("#2E9FDF", "#006400","#E7B800", "#FC4E07"),
               ellipse.type = "convex", # Concentration ellipse
               repel = TRUE, # Avoid label overplotting (slow)
               show.clust.cent = FALSE, ggtheme = theme_minimal())

hc1 <- hclust(d, method = "complete" )
grp <- cutree(hc1, k = 4)
library(dplyr)
df_cl <- mutate(df, cluster = grp)
count(df_cl,cluster)

# arrange by cluster
df_cl_sort <- arrange(df_cl, cluster)
df_cl_sort
df_cl_sort %>%
  group_by(cluster) %>%
  summarise_all(list(name = mean))
write.xlsx(df_cl_sort, file="D:/kluster.csv",row.names = FALSE)
write.table(df_cl_sort, file="D:/kluster.txt",row.names = FALSE)

#Non-Hierarchical Clusteringdengan algoritma K-Means 
dat1 <- read.table("D:/Datasets/kemiskinanjateng.txt", header = TRUE)
View(dat1)
str(dat1)
summary(dat1[,-c(1)])
summary(dat)
library (psych)
describe(dat)

Z <- scale(dat1[,-c(1)])
head(Z)

library(factoextra)
library(FactoMineR)
fit.pca <- PCA(dat, scale.unit = TRUE, ncp = 5, graph = TRUE)
eig.val <- get_eigenvalue(fit.pca)
eig.val

set.seed(2021)
clust.4means<-kmeans(x = dat, centers = 4, nstart=50)
clust.4means

library(factoextra)
# Elbow method
fviz_nbclust(dat, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2) + 
  labs(subtitle = "Elbow method") 
aggregate(dat, by=list(cluster=clust.4means$cluster), mean)

dd <- cbind(dat, cluster = clust.4means$cluster)
dd 

fviz_cluster(clust.4means, data = dat,
             palette = c("#2E9FDF", "#006400","#E7B800", "#FC4E07"),
             ellipse.type = "euclid", # Concentration ellipse
             star.plot = TRUE, # Add segments from centroids to items
             repel = TRUE, # Avoid label overplotting (slow)
             ggtheme = theme_minimal())
aggregate(dat, by=list(cluster=clust.4means$cluster), mean)


###PETA###
KAB <- readRDS("D:/Datasets/gadm36_IDN_2_sp.rds")
JATENG <- KAB[KAB$NAME_1 == "Jawa Tengah",]
kab.jateng <- JATENG$NAME_2
kab.jateng
write.csv(kab.jateng, file="D:/Datasets/kab.jateng.csv",row.names = FALSE)

datkabjateng <- read.csv(file="D:/Datasets/kab.jateng.kemiskinan.csv")
library(ggplot2)
library(raster)
plot(JATENG, col = c("#2E9FDF", "#006400","#E7B800", "#FC4E07")[datkabjateng$cluster], axes = 
    TRUE, cex = 0.25,border = "black")
text(JATENG, datkabjateng$kabupaten, cex = 0.6)
legend("bottomright", legend = c("Klaster 1", "Klaster 2", "Klaster 3", "Klaster 4"), col = c("#2E9FDF", "#006400","#E7B800", "#FC4E07"),
         inset=.02, fill= c("#2E9FDF", "#006400","#E7B800", "#FC4E07"), cex = 0.7, bty = "n")
library(prettymapr)
addnortharrow(pos = "topright",scale = 0.5,padin=c(0.55,0.15))
addscalebar()
