# STATISTICAL ANALYSIS R PROJECT

install.packages("ade4")
library(ade4)
library(vegan)

?aravo

# importing datasets
aravo <- read.csv("data/aravo.csv")
aravo_env <- read.table("data/aravo_env.txt",
                        sep = " ",
                        header = T)

# counting observations, species and environmental variables
observations <- nrow(aravo)
species <- ncol(aravo)
env_variables <- ncol(aravo_env)

# checking structure
str(aravo)
str(aravo_env)

# checking ZooD levels and transforming it into a factor
unique(aravo_env$ZoogD)
aravo_env$ZoogD <- factor(aravo_env$ZoogD,
                          levels = c("no", "some", "high"),
                          ordered = T)

# identify NA values
index_na <- which(is.na(aravo_env), arr.ind = T)
index_na <- index_na[, 1]

# remove NA values
aravo_env <- na.omit(aravo_env)
aravo <- aravo[-index_na, ]


