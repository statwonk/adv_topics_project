# Advanced topics project
# Authors: Chris Peters & Chengfei Lu
# Purpose: Advanced topics class project
# https://github.com/cpeter9/adv_topics_project
# Link to data: http://archive.ics.uci.edu/ml/datasets/Concrete+Compressive+Strength

data <- read.csv("Concrete_Data.csv") # this line enters data

# Look at the data with this command
# str(data)

names(data) <- c("cement", "slag", "fly_ash", "water", "plasticizer", "course_aggregate", "fine_aggregate", "age", "comp_strength") # this renames variables

head(data, 3) # This shows you the first 'x' number of rows of the data


#############################
# Visualization and graphing
############################

# If you do not have ggplot2 install, please uncomment this line and install:
# install.packages("ggplot2")
library(ggplot2)

# Simple pairs analysis
pairs(data) # many non-linear and weak relationships

# A look at how each variable is associated with comp_strength
# install.packages("reshape2")
library(reshape2)

df <- melt(data, c("comp_strength")) # transforms data, comp_strength by every other variable, this is a useful form for plotting

# Plot of each variable against comp_strength
ggplot(df, aes(x = comp_strength, y = value)) +
  geom_smooth(colour = "red", size = 2) +
  geom_point(alpha = 0.2) +
  facet_wrap(~ variable, scale = "free")

##########
# Variable transformation
#########

# Does logging help? Result: logging 'age' variable may be appropriate, not helpful for other variables
df$value <- log(df$value)

ggplot(df, aes(x = comp_strength, y = value)) +
  geom_smooth(colour = "red", size = 2) +
  geom_point(alpha = 0.2) +
  facet_wrap(~ variable, scale = "free")

################################
# Modelling
###############################



############
# Random Survival Forest
############

# install.packages("randomForestSRC")
# install.packages("survival")
library(randomForestSRC)
library(survival)

# All variables are not censored, they have "died", thus censor == 1
data$censor <- 1

rsf_tree <- rfsrc(Surv(comp_strength, censor) ~ ., data = data, ntree = 100)

# A look at variable relationships
plot.variable(rsf_tree) # some interesting relationships appear

plot(rsf_tree)

###
# How well does this model predict?
###

pct_training <- 0.9

train <- sample(1:nrow(data), round(nrow(data) * pct_training))
  rsf_tree <- rfsrc(Surv(comp_strength, censor) ~ ., data = data[train, ], ntree = 500)

preds_rsf <- predict(rsf_tree, data[-train, ])

plot(data[-train, "comp_strength"], preds_rsf$predicted)

preds_rsf_model <- lm(data[-train, "comp_strength"] ~ preds_rsf$predicted)

preds_rsf <- predict(preds_rsf_model, data[-train, ])

mse_rsf <- sum((data[-train, "comp_strength"] - preds_rsf) ^ 2) / length(preds_rsf)

plot(data[-train, "comp_strength"], preds_rsf)

rsf_output <- as.data.frame(list(actuals = data[-train, "comp_strength"], rsf_preds = preds_rsf), stringsAsFactors = FALSE)


############
# Random Forest
############

# install.packages("randomForest")
rf_tree <- randomForest(comp_strength ~ ., data = data[ , -length(data)], ntree = 500)

# A look at variable relationships
# partialPlot(tree, x.var = "water", pred.data = data, add = FALSE) # some interesting relationships appear

varImpPlot(rf_tree)

###
# How well does this model predict?
###
preds_rf <- predict(rf_tree, data[-train, -length(data)])

plot(data[-train, "comp_strength"], preds_rf)

mse_rf <- sum((data[-train, "comp_strength"] - preds_rf) ^ 2) / length(preds_rf)

rf_output <- as.data.frame(list(rf_preds = preds_rf), stringsAsFactors = FALSE)


########
# OLS
#######
ols <- lm(comp_strength ~ ., data = data[ , -length(data)])

preds_ols <- predict(ols, data[-train, -length(data)])

plot(data[-train, "comp_strength"], preds_ols)

mse_ols <- sum((data[-train, "comp_strength"] - preds_ols) ^ 2) / length(preds_ols)

ols_output <- as.data.frame(list(ols_preds = preds_ols), stringsAsFactors = FALSE)

################
# Model comparisons
################
output_combo <- cbind(rsf_output, rf_output, ols_output)
output_combo <- melt(output_combo, "actuals")

ggplot(output_combo, aes(x = actuals, y = value, colour = variable)) +
  geom_smooth() +
  geom_abline(intercept = 0, slope = 1)


