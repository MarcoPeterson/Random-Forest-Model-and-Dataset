packages <- installed.packages()  
lapply(packages[1:500], require, character.only = TRUE)  

myconn <- DBI::dbConnect(odbc::odbc(), "cradlepoint", uid="MPETERSON", pwd='Welcome1@') 
mydata <- DBI::dbGetQuery(myconn,"USE WAREHOUSE PROD_MGMT_XS")
original_ds <- DBI::dbGetQuery(myconn,
                               "select  *
from PROD_MGMT_DB.ML.POLL_DATASET_REGRESSION")

head(original_ds)
str(original_ds)

original_ds2 <- data.frame(original_ds$ACCOUNT_AGE_IN_MONTHS, original_ds$POLL_COUNT, original_ds$INDUSTRY ,original_ds$NUM_ROUTERS, round(original_ds$AVERAGE_POLL_RESPONSE_FOR_ACCOUNT), original_ds$NUM_EVENTS, round(original_ds$NUM_HOURS/24), original_ds$TOTAL_LOGINS)
names(original_ds)  <- c("ACCOUNT_AGE_MONTHS", "POLL_COUNT", "INDUSTRY", "NUM_ROUTERS", "AVERAGE_POLL", "NUM_EVENTS", "NUM_DAYS", "TOTAL_LOGINS")
names(original_ds2)  <- c("ACCOUNT_AGE_MONTHS", "POLL_COUNT", "INDUSTRY", "NUM_ROUTERS", "AVERAGE_POLL", "NUM_EVENTS", "NUM_DAYS", "TOTAL_LOGINS")

head(original_ds2)

original_ds2

head(original_ds)
str(original_ds)

original_ds2$INDUSTRY[is.na(original_ds2$INDUSTRY)] <- "Unknown"
original_ds$INDUSTRY[is.na(original_ds$INDUSTRY)] <-0

original_dsD

teens <- read.csv(url("https://raw.githubusercontent.com/PacktPublishing/Machine-Learning-with-R-Third-Edition/master/Chapter09/snsdata.csv"))

interests <-teens[5:40]

interest

str(teens)
str(interest)

interests_z <- as.data.frame(lapply(interests, scale))


summary(interests$basketball)
summary(interests_z$basketball)

interests_z


original_dataset_z <- as.data.frame(lapply(original_ds2, scale))

original_dataset_z

summary(original_ds$AVERAGE_POLL)
summary(original_dataset_z$AVERAGE_POLL)

set.seed(2345)

orginal_clusters <- kmeans(original_dataset_z, 3)


fviz_cluster(res.km2, data = original_ds2[, -3], 
palette = c("#2E9FDF", "#00AFBB", "#E7B800"),  
             geom = "point", 
             ellipse.type = "convex",
outlier.color = "black", outlier.shape = 19,
             ggtheme = theme_bw() 
) 

help(factoextra)


iris <- read.csv(url("https://gist.githubusercontent.com/curran/a08a1080b88344b0c8a7/raw/0e7a9b0a5d22642a06d3d5b9bcbad9890c8ee534/iris.csv")) 
df <- iris 
df
res.km <- kmeans(scale(df[, -5]), 3, nstart = 25)

res.km2 <-kmeans(scale(original_ds2[, -3]), 3, nstart = 25)

res.km
res.km2

original_ds2


prop.table(table(original_ds2$INDUSTRY))

count(original_ds2$INDUSTRY)

set.seed(123)

# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(orignal_ds_2, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")



df <- na.omit(original_ds2)
df


fviz_nbclust(df, kmeans, method = "wss")





res.pca <- PCA(scale(original_ds2[, -3]), scale.unit = TRUE, ncp = 7, graph = TRUE)

res.pca


display_pc(pca_result)

summary(res.pca)



original_ds2


get_eig(res.pca)


fviz_screeplot(res.pca, addlabels = TRUE, ylim = c(0, 50))

var <- get_pca_var(res.pca)
var

head(var$coord)
fviz_pca_var(res.pca, col.var = "black")

fviz_pca_var(res.pca, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping
)


fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)


ind <- get_pca_ind(res.pca)
ind
head(ind$coord)


fviz_pca_ind(res.pca, col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping (slow if many points)
)
fviz_pca_biplot(res.pca, repel = TRUE)


fviz_pca_ind(res.pca,
             label = "none", # hide individual labels
             habillage = iris$Species, # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE # Concentration ellipses
)


fviz_mca_var(res.mca, repel = TRUE)





data("USArrests")
df <- scale(USArrests)

original_ds2


df
# 2. Compute k-means
set.seed(123)
km.res <- kmeans(scale(USArrests), 4, nstart = 25)
res.km2 <-kmeans(scale(original_ds2[, -3]), 3, nstart = 25)
# 3. Visualize
library("factoextra")
fviz_cluster(km.res, data = df,
             palette = c("#00AFBB","#2E9FDF", "#E7B800", "#FC4E07"),
             ggtheme = theme_minimal(),
             main = "Partitioning Clustering Plot"
)

newdata <- data.frame(original_ds2, row.names = "INDUSTRY")

rownames(original_ds2) <-c("INDUSTRY")


ggplot(original_ds2,aes(x,y)) +
  stat_voronoi(geom="path") +
  geom_point()

summary(res.pca)
****************
commonality(res.pca)


res.pca$call

display_pc(res.pca)

loadings.pcarot = varimax(res.pca$var$coord)$loadings

res.pca$var$coord = loadings.pcarot

plot(res.pca, choix = "var")


ggbiplot(res.pca, labels = rownames(original_ds2))


ggbiplot(res.pca,ellipse=TRUE,  labels=rownames(res.pca), groups=res.pca.IN)


groups <- as.factor(original_ds2$INDUSTRY[1:106])

groups


fviz_pca_ind(res.pca,
             col.ind = groups, # color by groups
             # palette = c("#00AFBB",  "#FC4E07"),
             addEllipses = TRUE, # Concentration ellipses
             ellipse.type = "confidence",
             legend.title = "Groups",
             repel = TRUE
)


fviz_pca_ind(res.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)



fviz_cluster(res.km2, data = df[, -3],
             palette = c("#2E9FDF", "#00AFBB", "#E7B800"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)




install.packages("devtools")

library(devtools)

install_github("vqv/ggbiplot")

library(ggbiplot)