# STATISTICAL ANALYSIS R PROJECT

# installing the packages needed for the analysis and recalling the libraries
install.packages("ade4")
install.packages("vegan")
library(ade4)
library(vegan)


# checking the datasets' documentation
?aravo


# importing datasets
aravo <- read.csv("data/aravo.csv")
aravo_env <- read.table("data/aravo_env.txt",
                        sep = " ",
                        header = T)

# sep = " " to explicit that the field are separated by a white space
# header = T to make first row column titles


# counting observations, species and environmental variables
observations <- nrow(aravo)            # number of observation = number of rows
species <- ncol(aravo)                 # number of species = number of columns of the community matrix
env_variables <- ncol(aravo_env)       # number of environmental variables = number of columns of the environmental variables dataframe

# checking data structure
str(aravo)
str(aravo_env)


# checking ZoogD levels
unique(aravo_env$ZoogD)
# transforming ZoogD into an ordered factor with levels
aravo_env$ZoogD <- factor(aravo_env$ZoogD,
                          levels = c("no", "some", "high"),
                          ordered = T)
# checking Form levels
unique(aravo_env$Form)
# transforming Form into an ordered factor with 5 levels and labelling them
aravo_env$Form <- factor(aravo_env$Form,
                         ordered = T,
                         labels = c("Convexity", "Convex slope", "Right slope","Concave slope", "Concavity"))


# identify presence of NAs values
is.na(aravo) # no missing values in aravo dataset
is.na(aravo_env) # presence of NAs in aravo_env dataset

# identify the location of NAs values in aravo_env dataset
index_na <- which(is.na(aravo_env), arr.ind = T) # with the argument arr.ind = T array indices are returned
index_na
index_na <- index_na[, 1] # subetting the first column (containig the row numebers of NAs)

# remove NA values
aravo_env <- na.omit(aravo_env)  # omitting the rows containing NAs in aravo_env
aravo <- aravo[-index_na, ]      # omitting the corrispondig rows in aravo, using index_na


# making summary statistics
summary(aravo_env)
# information about  Min - 1st Qu. - Median - Mean - 3rd Qu. - Max of the numerical variables
# information about number of observations for each levels of the categorical variables


# exporting datasets without NAs values
write.table(x = aravo_env, file = "outputs/aravo_env_mod.txt")
write.table(x = aravo, file = "outputs/aravo_mod.txt")


# graphically exploring the univariate distribution of the environmental variables 
# plotting histograms of the univariate distribution for the numerical variables
hist(aravo_env$Aspect,
     main = "Relative south aspect distribution",
     xlab = "Aspect")

hist(aravo_env$Slope,
     main = "Slope inclination distribution",
     xlab = "Slope (degrees)")

hist(aravo_env$Snow,
     main = "Mean snowmelt date distribution",
     xlab = "Mean snowmelt date (Julian day)")

hist(aravo_env$PhysD,
     main = "Physical disturbance distribution",
     xlab = "Percentage of unvegetated soil")

# plotting barplots of the univariate distribution for the categorical variables
barplot(table(aravo_env$Form),
        main = "Microtopographic landform distribution",
        xlab = "Microtopographic landform")

barplot(table(aravo_env$ZoogD),
        main = "Zoogenic disturbance distribution",
        xlab = "Quantity of unvegetated soil")


# exporting Snow histogram in .png format
png(filename = "outputs/snow_hist.png", 
    width = 4000,
    height = 3000,
    res = 300)
hist(aravo_env$Snow,
     main = "Mean snowmelt date distribution",
     xlab = "Mean snowmelt date (Julian day)")
dev.off()

# exporting Form barplot in .png format
png(filename = "outputs/form_barplot.png", 
    width = 4000,
    height = 3000,
    res = 300)
barplot(table(aravo_env$Form),
        main = "Microtopographic landform distribution",
        xlab = "Microtopographic landform")
dev.off()


# converting community matrix in presence/absence matrix with the decostand function of the vegan package
aravo <- decostand(aravo, method = "pa")

# exporting presence/absence matrix in .csv format
write.csv(x = aravo, file = "outputs/aravo_pa.csv")


# compute species richness using spcenumber function of the vegan package
sr <- specnumber(aravo) 
# store sr into aravo_env dataframe as a new column
aravo_env$sr <- sr

# another way to compute sr
# calculate_sr <- function(aravo){
#  sr <- vector()
#  for (i in 1:nrow(df)) {
#    sr[i] <- sum(aravo[i, ] > 0)
#  }
#  return(sr)
#}


# finding min, max and mean values of sr using the summury function
summary(sr)


# visualize with a boxplot the species richness distribution with respect to Form
boxplot(sr ~ Form,
        data = aravo_env,
        main = "Species richness distribution with respect to Form",
        xlab = "Microtopographic landform index",
        ylab = "Species Richness",
        ylim = c(0,30))


# plot the distribution of species richness in respect with numerical experimental variables
plot(sr ~ Aspect,
     data = aravo_env,
     main = "Species Richness distribution in respect with Aspect",
     xlab = "Relative south aspect",
     ylab = "Species Richness",
     ylim = c(0,30),
     cex = 0.5)

plot(sr ~ Slope,
     data = aravo_env,
     main = "Species Richness distribution in respect with Slope",
     xlab = "Degrees of Inclination",
     ylab = "Species Richness",
     ylim = c(0,30),
     cex = 0.5)

plot(sr ~ Snow,
     data = aravo_env, 
     main = "Species Richness distribution in respect with Snow",
     xlab = "Mean sonowmelt date (Julian day)",
     ylab = "Species Richness",
     ylim = c(0,30),
     cex = 0.5)

plot(sr ~ PhysD,
     data = aravo_env,
     main = "Species Richness distribution in respect with PhysD",
     xlab = "Percentage of unvegetated soil",
     ylab = "Species Richness",
     ylim = c(0,30),
     cex = 0.5)


# testing the correlation between species richness and the numerical environmental variables using cor.test function
cor.test(aravo_env$Aspect, aravo_env$sr) 
# null hypothesis = correlation equal to 0
# two sided alternative = correlation not equal to 0
# p-value = 0.4496 -> not signoficant (p-value > 0.1 -> no evidence against H0)
# cor = 0.08986083
# the p-value is not significant, meaning there is no correlation between Aspect and sr

cor.test(aravo_env$Slope, aravo_env$sr)
# null hypothesis = correlation equal to 0
# two sided alternative = correlation not equal to 0
# p-value = 0.000908 -> very significant (p-value < 0.01 -> strong evidence against H0)
# cor = 0.3801656
# significant positive correlation between Slope and sr

cor.test(aravo_env$Snow, aravo_env$sr)
# null hypothesis = correlation equal to 0
# two sided alternative = correlation not equal to 0
# p-value = 1.163e-07 -> very significant (p-value < 0.01 -> strong evidence against H0)
# cor = -0.5731328 
# very significant negative correlation between Snow and sr

cor.test(aravo_env$PhysD, aravo_env$sr)
# null hypothesis = correlation equal to 0
# two sided alternative = correlation not equal to 0
# p-value = 0.005676 -> very significant (p-value < 0.01 -> strong evidence against H0)
# cor = 0.3206664 
# significant positive correlation between PhysD and sr


# Linear regression models on environmental variables significantly correlated with species richness
# using lm function to construct the linear regression model

# linear regression model of Slope variable
mod_slope <- lm(sr ~ Slope, data = aravo_env)
summary(mod_slope)
# The residuals values look normally distributed
# Both the estimated coefficients ([1] intercept, [2] slope) are very significant since they have a Pr(>|t|) < 0.01
# Adjusted R-squared: 0.1325, not very high meaning that only 13% of the variance of species richness is explained by the linear model 
# p-value: 0.000908 -> very low value meaning the model is significant

# Scatterplot with the regression line
plot(sr ~ Slope,
     data = aravo_env,
     main = "Species Richness distribution in respect with Slope",
     xlab = "Degrees of Inclination",
     ylab = "Species Richness",
     ylim = c(0,30),
     cex = 0.5)
abline(mod_slope$coefficients[1], mod_slope$coefficients[2], col = "red") # adding the line representing the linear regression model, having as intercept the value of coefficients[1], and as slope the value of coefficients[2]

# linear regression model of Snow variable
mod_snow <- lm(sr ~ Snow, data = aravo_env)
summary(mod_snow)
# The residuals values look normally distributed
# Both the estimated coefficients ([1] intercept, [2] slope) are very significant since they have a Pr(>|t|) < 0.01
# Adjusted R-squared: 0.319, moderate value meaning that 32% of the variance of species richness is explained by the linear model 
# p-value: 1.163e-07 -> very low value meaning the model is very significant

# Scatterplot with the regression line
plot(sr ~ Snow,
     data = aravo_env, 
     main = "Species Richness distribution in respect with Snow",
     xlab = "Mean sonowmelt date (Julian day)",
     ylab = "Species Richness",
     ylim = c(0,30),
     cex = 0.5)
abline(mod_snow$coefficients[1], mod_snow$coefficients[2], col = "red") 

# linear regression model of PhysD variable
mod_physd <- lm(sr ~ PhysD, data = aravo_env)
summary(mod_physd)
# The residuals values look normally distributed
# Both the estimated coefficients ([1] intercept, [2] slope) are very significant since they have a Pr(>|t|) < 0.01
# Adjusted R-squared: 0.09019, very low value meaning that only 9% of the variance of species richness is explained by the linear model 
# p-value: 0.005676 -> very low value meaning the model is significant

# Scatterplot with the regression line
plot(sr ~ PhysD,
     data = aravo_env,
     main = "Species Richness distribution in respect with PhysD",
     xlab = "Percentage of unvegetated soil",
     ylab = "Species Richness",
     ylim = c(0,30),
     cex = 0.5)
abline(mod_physd$coefficients[1], mod_physd$coefficients[2], col = "red")




