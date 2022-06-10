# Install Library
install.packages("data.table")
install.packages("factoextra")
install.packages("magrittr")
install.packages("dplyr")
install.packages("ggpubr")
install.packages("MASS")

# Library yang digunakan
library(data.table)
library(factoextra)
library(magrittr)
library(dplyr)
library(ggpubr)
library(MASS)


###############################################################################


# Input Data #
WHR <- fread("D:\\Fakhri\\Kuliah\\Jadwal dan Materi Kuliah\\Tahun Kedua\\Semester 4\\Hal Lain\\Metode Kuantitatif\\TA-Metkuan\\2019.csv") # Ganti file path nya
WHR_v2 <- WHR[,c(2:9)]
WHR_Fix <- data.frame(WHR_v2, row.names = 1)


###############################################################################


# PCA #
WHR.pca <- prcomp(WHR_Fix, center=TRUE, scale. = TRUE)
summary(WHR.pca)

pcnya <- predict(WHR.pca, newdata=WHR_Fix)
View(pcnya)


###############################################################################


# Biplot #
WHR.biplot <- biplot(WHR.pca, scale = 0)


###############################################################################


# Cluster Optimal
fviz_nbclust(WHR_v2[,2:8], hcut, method = "silhouette") # Cluster Optimal = 2


###############################################################################


# K-Means #
# K-Means
(kmeans.hasil.pca <- kmeans(WHR.pca$x,2))

plot(WHR.pca$x, col = kmeans.hasil.pca$cluster)
points(kmeans.hasil.pca$centers[,c("PC1", "PC2")], col = 1:3, pch = 8, cex = 2)


###############################################################################


# MDS #
# Compute MDS
# Metode 1
mds <- WHR_Fix %>%
  dist() %>%
  cmdscale() %>%
  #isoMDS() %>%
  #sammon() %>%
  #.$points %>%
  as_tibble()
colnames(mds) <- c("Dim.1", "Dim.2")

# Metode 2
mds <- WHR_Fix %>%
  dist() %>%
  #cmdscale() %>%
  isoMDS() %>%
  #sammon() %>%
  .$points %>%
  as_tibble()
colnames(mds) <- c("Dim.1", "Dim.2")

# Metode 3
mds <- WHR_Fix %>%
  dist() %>%
  #cmdscale() %>%
  #isoMDS() %>%
  sammon() %>%
  .$points %>%
  as_tibble()
colnames(mds) <- c("Dim.1", "Dim.2")

## PENTING: Lakukan untuk setiap metode ##
# Plot MDS 
# K-means clustering
clust <- kmeans(mds,2)$cluster %>%
  as.factor()
mds <- mds %>%
  mutate(groups = clust) 

# Plot and color by groups
ggscatter(mds, x = "Dim.1", y = "Dim.2",
          label = rownames(WHR_Fix),
          color = "groups",
          palette = "jco",
          size = 1,
          ellipse = TRUE,
          ellipse.type = "convex",
          repel = TRUE)


###############################################################################


# Hierarchical Clustering #
# Jarak antar data
data_jarak = dist(WHR_v2[,2:8])
data_jarak

# Perbandingan korelasi antar metode hirarki
# Single #
hc = hclust(data_jarak,"single")
d2 = cophenetic(hc)
cor.sing = cor(data_jarak,d2)
cor.sing

# Average #
hc = hclust(data_jarak,"ave")
d2 = cophenetic(hc)
cor.ave = cor(data_jarak,d2)
cor.ave

# Complete #
hc = hclust(data_jarak,"complete")
d2 = cophenetic(hc)
cor.comp = cor(data_jarak,d2)
cor.comp

## PENTING: Menggunakan metode average karena nilainya lebih besar ##

# Analisis cluster dgn hirarki average 
hirarki.ave = hclust(data_jarak, method = "ave")
hirarki.ave

# Dendogram 
plot(hirarki.ave, labels = WHR_v2$`Country or region`)

# mengelompokkan data pada dendogram (k=2)
rect.hclust(hirarki.ave, k=2, border = 2:3)

# Anggota cluster
# Average #
hasil.cut = cutree(hirarki.ave,2)
table(hasil.cut)

rownames(WHR_v2)[hasil.cut==1]
rownames(WHR_v2)[hasil.cut==2]


###############################################################################