# Advanced topics project
# Authors: Chris Peters & Chengfei Lu
# Purpose: Advanced topics class project
# https://github.com/cpeter9/adv_topics_project
# Link to data: http://archive.ics.uci.edu/ml/datasets/Concrete+Compressive+Strength

# If you do not have ggplot2 install, please uncomment this line and install:
# install.packages("ggplot2")
library(ggplot2)

# install.packages("mboost")
library(mboost)

# A look at how each variable is associated with comp_strength
# install.packages("reshape2")
library(reshape2)


# install.packages("MBA")
library(MBA)

# install.packages("rgl")
library(rgl)

# install.packages("akima")
library(akima)

# install.packages("randomForest")
library(randomForest)


data <- read.csv("Concrete_Data.csv") # this line enters data

# Look at the data with this command
# str(data)

# Rename variables
names(data) <- c("cement", "slag", "fly_ash", "water", "plasticizer", "coarse_aggregate", "fine_aggregate", "age", "comp_strength") # this renames variables

#############################
# Visualization and graphing
############################

# Simple pairs analysis
# pairs(data) # many non-linear and weak relationships

df <- melt(data, c("comp_strength")) # transforms data, comp_strength by every other variable, this is a useful form for plotting

# Plot of each variable against comp_strength
ggplot(df, aes(x = comp_strength, y = value)) +
  geom_smooth(colour = "red", size = 2) +
  geom_point(alpha = 0.2) +
  facet_wrap(~ variable, scale = "free") +
  theme(strip.text.x = element_text(size = 14))

##########
# Variable transformation
#########

# Does logging help? Result: logging 'age' variable may be appropriate, not helpful for other variables
df$value <- log(df$value)

ggplot(df, aes(x = comp_strength, y = value)) +
  geom_smooth(colour = "red", size = 2) +
  geom_point(alpha = 0.2) +
  facet_wrap(~ variable, scale = "free") +
  theme(strip.text.x = element_text(size = 14))


var1 <- data$age
var2 <- data$cement

output <- mba.surf(data.frame(list( x = var1, y = var2, z = data$comp_strength)), no.X = 100, no.Y = 100, h = 4)

# persp3d(output$xyz.est$x, output$xyz.est$x, output$xyz.est$z, aspect=c(1, 1, 0.5), col = "lightblue",
#         xlab = "X", ylab = "Y", zlab = "Sinc( r )")




# data
rgl.spheres(var1 , data$comp_strength , var2, 3, color="red")
rgl.bbox()
# bivariate linear interpolation
# interp:

# interp surface:
rgl.surface(output$xyz.est$x, output$xyz.est$y, output$xyz.est$z, color = "green", alpha = c(0.7), back = "lines")

################################
# Modelling
###############################

############
# Random Forest
############

pct_training <- 0.9

train <- sample(1:nrow(data), round(nrow(data) * pct_training))


rf_tree <- randomForest(comp_strength ~ ., data = data[train, ], ntree = 500, importance = TRUE)

# A look at variable relationships
partialPlot(rf_tree, x.var = "cement", pred.data = data, add = FALSE) # some interesting relationships appear

varImpPlot(rf_tree)

preds_rf <- predict(rf_tree, data[-train, -length(data)])

plot(data[-train, "comp_strength"], preds_rf)

mse_rf <- sum((data[-train, "comp_strength"] - preds_rf) ^ 2) / length(preds_rf)

rf_output <- as.data.frame(list(rf_preds = preds_rf), stringsAsFactors = FALSE)


########
# OLS
#######

ols <- lm(comp_strength ~ ., data = data)

preds_ols <- predict(ols, data[-train, -length(data)])

plot(data[-train, "comp_strength"], preds_ols)

mse_ols <- sum((data[-train, "comp_strength"] - preds_ols) ^ 2) / length(preds_ols)

ols_output <- as.data.frame(list(ols_preds = preds_ols), stringsAsFactors = FALSE)


###########################
# P-splines
###########################

gam_model <- gamboost(comp_strength ~ cement + slag + fly_ash + water + plasticizer +
                        coarse_aggregate + fine_aggregate + age,
                        baselearner = "bbs", data = data)
layout(matrix(1:3, ncol = 3))
plot(gam_model[mstop(AIC(gam_model, "corrected"))], ask = FALSE)

preds_gam <- predict(gam_model, data[-train, -length(data)])

plot(data[-train, "comp_strength"], preds_gam)

mse_gam <- sum((data[-train, "comp_strength"] - preds_gam) ^ 2) / length(preds_gam)

gam_output <- as.data.frame(list(gam_preds = preds_gam), stringsAsFactors = FALSE)

################
# Model comparisons
################

output_combo <- cbind(ols_output, rf_output, gam_output, data[-train, "comp_strength"])
  names(output_combo)[length(output_combo)] <- "actuals"
output_combo <- melt(output_combo, "actuals")

ggplot(output_combo, aes(x = actuals, y = value, colour = variable)) +
  geom_line(size = 2, alpha = 0.5) + 
  # geom_smooth(method = "loess", span = 0.05) +
  geom_abline(intercept = 0, slope = 1) +
  scale_x_continuous(limits = c(0, 80)) +
  scale_y_continuous(limits = c(0, 80))



