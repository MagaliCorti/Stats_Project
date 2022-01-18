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

unique(aravo_env$Form)
aravo_env$Form <- factor(aravo_env$Form,
                         ordered = T,
                         labels = c("Convexity", "Convex slope", "Right slope","Concave slope", "Concavity"))

# identify NA values
index_na <- which(is.na(aravo_env), arr.ind = T)
index_na <- index_na[, 1]

# remove NA values
aravo_env <- na.omit(aravo_env)
aravo <- aravo[-index_na, ]

# making summary statistics
summary(aravo_env)

# exporting datasets without NAs values
write.table(x = aravo_env, file = "outputs/aravo_env_mod.txt")
write.table(x = aravo, file = "outputs/aravo_mod.txt")


# environmental variable distribution

hist(aravo_env$Aspect,
     main = "Relative south aspect distribution",
     xlab = "Aspect")

hist(aravo_env$Slope,
     main = "Slope inclination distribution",
     xlab = "Slope (degrees)")

# exporting barplot
png(filename = "outputs/form_barplot.png", 
    width = 4000,
    height = 3000,
    res = 300)
barplot(table(aravo_env$Form),
        main = "Microtopographic landform distribution",
        xlab = "Microtopographic landform")
dev.off()

#exporting histogram
png(filename = "outputs/snow_hist.png", 
    width = 4000,
    height = 3000,
    res = 300)
hist(aravo_env$Snow,
     main = "Mean snowmelt date distribution",
     xlab = "Mean snowmelt date (Julian day)")
dev.off()

hist(aravo_env$PhysD,
     main = "Physical disturbance distribution",
     xlab = "Percentage of unvegetated soil")

barplot(table(aravo_env$ZoogD),
        main = "Zoogenic disturbance distribution",
        xlab = "Quantity of unvegetated soil")

plot(Slope ~ PhysD,
     data = aravo_env,
     xlab = "Percentage of unvegetated soil",
     ylab = "Degrees of Inclination")

boxplot(aravo_env$PhysD,
        main = "Percentage of unvegetated soil")

# converting community matrix in presence/absence matrix
aravo <- decostand(aravo, method = "pa")

# exporting presence/absence matrix
write.csv(x = aravo, file = "outputs/aravo_pa.csv")

# compute species richness and store it into aravo_env dataframe
aravo_env$sr <- specnumber(aravo)

# another way to compute sr
# calculate_sr <- function(df){
#  sr <- vector()
#  for (i in 1:nrow(df)) {
#    sr[i] <- sum(df[i, ] > 0)
#  }
#  return(sr)
#}

# find min and max values of sr
summary(sr)

# visualize with a boxplot the species richness distribution with respect to Form
boxplot(sr ~ Form,
        data = aravo_env,
        main = "Species richness distribution with respect to Form",
        xlab = "Microtopographic landform index",
        ylab = "Species Richness",
        ylim = c(0,30))

plot(sr ~ Aspect,
     data = aravo_env,
     main = "Species Richness distribution in respect with Aspect",
     type = "p",
     xlab = "Relative south aspect",
     ylab = "Species Richness",
     ylim = c(0,30))

plot(sr ~ Slope,
     data = aravo_env,
     main = "Species Richness distribution in respect with Slope",
     type = "p",
     xlab = "Degrees of Inclination",
     ylab = "Species Richness",
     ylim = c(0,30))

plot(sr ~ Snow,
     data = aravo_env, 
     main = "Species Richness distribution in respect with Snow",
     type = "p",
     xlab = "Mean sonowmelt date (Julian day)",
     ylab = "Species Richness",
     ylim = c(0,30))

plot(sr ~ PhysD,
     data = aravo_env,
     main = "Species Richness distribution in respect with PhysD",
     type = "p",
     xlab = "Percentage of unvegetated soil",
     ylab = "Species Richness",
     ylim = c(0,30))

cor.test(aravo_env$Aspect, aravo_env$sr)

cor.test(aravo_env$Slope, aravo_env$sr)

cor.test(aravo_env$PhysD, aravo_env$sr)

cor.test(aravo_env$Snow, aravo_env$sr)

mod_snow <- lm(sr ~ Snow, data = aravo_env)
summary(mod_snow)
str(mod_snow)
abline(mod_snow$coefficients[1], mod_snow$coefficients[2])



