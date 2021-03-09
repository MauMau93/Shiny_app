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

install.packages(c("devtools"))
devtools::install_github("ldurazo/kaggler")
library(readr)
library(kaggler)
kgl_auth(creds_file = 'kaggle.json')
response <- kgl_datasets_download_all(owner_dataset = "UCI Machine Learning")

download.file(response[["url"]], "data/temp.zip", mode="wb")
DATOS <- read_csv("https://www.kaggle.com/uciml/pima-indians-diabetes-database")

datasets <- kgl_datasets_list()
datasets
