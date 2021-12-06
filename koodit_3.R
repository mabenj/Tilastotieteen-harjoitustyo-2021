library(foreign)
ht4.dat<-read.spss("pankkiotos2020.sav", to.data.frame=TRUE)
attach(ht4.dat)

set.seed(518467)
oma.otos4<-ht4.dat[sample(nrow(ht4.dat), 1500), ]
attach(oma.otos4)

# poistetaan toimeksianto_a_kpl_luok
drops <- c("toimeksianto_a_kpl_luok")
oma.otos5 <- oma.otos4[ , !(names(oma.otos4) %in% drops)]
       
# korrelaatiokertoimet
data.kor <- cor(oma.otos5, method="pearson", use="complete.obs")

# pääkomponenttianalyysi
pca <- prcomp(data.kor, center=TRUE, scale=TRUE)
pca
summary(pca)

# Valitaan kymmenen pääkomponenttia, promax-rotaatio
pca.chosen <- pca$rotation [ ,1:10]
pca.promax <- promax(pca.chosen)
pca.promax

# K-means klusterointi pääkomponenttipistemäärillä
km = pca.chosen[,c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)]
set.seed(518467)

# 2 klusteria
set.seed(518467)
km2 = kmeans(km, 2, nstart=100)
km2

#3 klusteria
set.seed(518467)
km3 = kmeans(km, 3, nstart=100)
km3

#4 klusteria
set.seed(518467)
km4 = kmeans(km, 4, nstart=100)
km4

#5 klusteria
set.seed(518467)
km5 = kmeans(km, 5, nstart=100)
km5
