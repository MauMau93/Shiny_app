# This is a file used to study the data

ColClasses=c(rep("numeric",9))
DATOS=read.csv2("diabetes.csv",sep = ",",header = T, colClasses = ColClasses, dec = "." )
summary(DATOS)

#Taking out NAs and converting variable into factor
data <- DATOS[2:8]
data[data==0] <-  NA
DATOS[2:8] <- data
DATOS <- mice(DATOS,m=1,method="pmm")
DATOS <- complete(DATOS)
attach(DATOS)
DATOS$Outcome <- as.factor(DATOS$Outcome)

variables <- colnames(DATOS %>% select(-Outcome))

DATOS <- read.csv(url("http://archive.ics.uci.edu/ml/machine-learning-databases/pima-indians-diabetes/"))


library(scatterplot3d)
scatterplot3d(DATOS[,2:4], pch=20, color=rainbow(3)[DATOS$cluster])

cluster <- kmeans(DATOS, centers = 4)
fviz_cluster(cluster, DATOS)
library(scatterplot3d)
scatterplot3d(DATOS[,2:8], pch=20, color=rainbow(3)[km$cluster])

kmeans(Glucose,BloodPressure, centers = 4)

runGitHub("dtwclust","asardaes")
            
