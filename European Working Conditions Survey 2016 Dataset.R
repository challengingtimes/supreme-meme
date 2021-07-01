#----Part 1---------------------------------------------------------------------------------------------------
library(GGally) 
library(tidyverse) 
library(ggfortify)
library(factoextra) 
library(kableExtra)

setwd('C:/Users/User/Documents/Machine Learning/Coursework')
ewc.data <- read.csv("Part 1 EWCS_2016.csv", sep = ",", header = TRUE, stringsAsFactors=T)
ewc.data[,][ewc.data[, ,] == -999] <- NA
c <- complete.cases(ewc.data)
ewc.data2 <- ewc.data[c,]
eWc.data2.scaled <- scale(ewc.data2)

ewc.data2 %>% count(Q2a, sort = TRUE)

#----Data Visualisation-----------------------------
# corr plot
ggcorr(eWc.data2.scaled, label = TRUE, label_alpha = TRUE, mid = "#D4B9DA", high = "#40004B")

# Questionnaire responses
options(repr.plot.width = 6, repr.plot.height = 6)
ggplot(gather(ewc.data2[, 3:11]), aes(value)) + 
  geom_histogram(stat = "count",color = "purple1", fill = "#C994C7")+ facet_wrap(~ key, scales = 'free_x')

#-------PCA--------------------------------------------------------------------
pc.entire <- prcomp(eWc.data2.scaled)
summary(pc.entire)
fviz_eig(pc.entire, barcolor = "purple", barfill = "#C994C7", main = "Scree Plot of Entire Dataset")

pc.matrix <- function(loading, comp.sdev){loading*comp.sdev}
loading <- pc.entire$rotation
comp.sdev <- pc.entire$sdev
pc.var <- t(apply(loading, 1, pc.matrix, comp.sdev))
pc.var[,1:2]

# PCA biplot
autoplot(pc.entire, colour ='Q90f', loadings = TRUE, loadings.label = TRUE, loadings.label.size = 5)

#--------K-means Clustering-----------------------------------------------------
set.seed(2021)
wss <- sapply(1:20, function(k){kmeans(eWc.data2.scaled, k, iter.max = 15)$tot.withinss})
fviz_nbclust(eWc.data2.scaled, kmeans, method = "wss") + geom_vline(xintercept = 2, linetype = 2)
k1 <- kmeans(eWc.data2.scaled, centers = 2, nstart = 20)

# number of observations in clusters
k1.clusters <- k1$cluster
table(k1.clusters) # 2778 in first cluster, 4869 in second

# mean of all variables in the clusters
sub.group <- k1.clusters
kmean.clusters <- ewc.data2 %>% mutate(Cluster = sub.group) %>% group_by(Cluster) %>% summarise_all("mean")
kable_styling(kable(kmean.clusters),c("striped","bordered"), full_width = T)

autoplot(k1, data = eWc.data2.scaled) + scale_color_manual(values=c('#6A51A3','#A50F15'))

## Chi-square test for differences between these two clusters
# cluster 1 and 2 results
k1results <- data.frame(ewc.data2$Q2a, ewc.data2$Q2b, ewc.data2$Q87a, ewc.data2$Q87b, ewc.data2$Q87c, 
                        ewc.data2$Q87d, ewc.data2$Q87e, ewc.data2$Q90a, ewc.data2$Q90b, ewc.data2$Q90c, 
                        ewc.data2$Q90f, k1$cluster)

cluster1 <- subset(k1results, k1$cluster == 1)
cluster2 <- subset(k1results, k1$cluster == 2)

M <- as.matrix(table(cluster1$ewc.data2.Q90f))
p.null <- as.vector(prop.table(table(cluster2$ewc.data2.Q90f)))
chisq.test(M, p = p.null)

#-------------Hierachical Clustering----------------------------------------------
hc.comp <- hclust(dist(eWc.data2.scaled), method = "complete")
hc.avg <- hclust(dist(eWc.data2.scaled), method = "average")
hc.sg <- hclust(dist(eWc.data2.scaled), method = "single")

par(mfrow = c(1, 3))
plot(hc.comp, main = "Complete Linkage", xlab = "", sub = "", cex = 0.9)
plot(hc.avg, main = "Average Linkage", xlab = "", sub = "", cex = 0.9)
plot(hc.sg, main = "Single Linkage", xlab = "", sub = "", cex = 0.9)

sum(cutree(hc.comp, 2)== 2) # Cluster 1 have 1220 cases, cluster 2 has 6427 cases
sum(cutree(hc.avg, 2)== 2) # Cluster 2 only have 29 cases, cluster 1 has 7618 cases
sum(cutree(hc.sg, 2)== 2) # Cluster 2 only have 1 case, cluster 1 has 7646 cases

# getting the cluster means for the variables
sub.group.hc <- cutree(hc.comp, 2)
hmean.clusters <- ewc.data2 %>% mutate(Cluster = sub.group.hc) %>% group_by(Cluster) %>% summarise_all("mean") # mean of all variables in the cluster
kable_styling(kable(hmean.clusters),c("striped","bordered"), full_width = T)

# test if both clusters are similar based on Q90f
# cluster 1 and 2 results
hc.results <- data.frame(ewc.data2$Q2a, ewc.data2$Q2b, ewc.data2$Q87a, ewc.data2$Q87b, ewc.data2$Q87c, 
                        ewc.data2$Q87d, ewc.data2$Q87e, ewc.data2$Q90a, ewc.data2$Q90b, ewc.data2$Q90c, 
                        ewc.data2$Q90f, cutree(hc.comp, 2))

hc.cluster1 <- subset(hc.results, cutree(hc.comp, 2) == 1)
hc.cluster2 <- subset(hc.results, cutree(hc.comp, 2) == 2)

# Chi square test to test if both clusters are independent
M.hc <- as.matrix(table(hc.cluster1$ewc.data2.Q90f))
p.null.hc <- as.vector(prop.table(table(hc.cluster2$ewc.data2.Q90f)))
chisq.test(M.hc, p = p.null.hc)
