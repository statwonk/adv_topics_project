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


data <- read.csv("Concrete_Data.csv") # this line enters data

# Look at the data with this command
# str(data)

names(data) <- c("cement", "slag", "fly_ash", "water", "plasticizer", "coarse_aggregate", "fine_aggregate", "age", "comp_strength") # this renames variables

head(data, 3) # This shows you the first 'x' number of rows of the data


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

# install.packages("MBA")
library(MBA)

var1 <- data$age
var2 <- data$cement

output <- mba.surf(data.frame(list( x = var1, y = var2, z = data$comp_strength)), no.X = 100, no.Y = 100, h = 4)

# persp3d(output$xyz.est$x, output$xyz.est$x, output$xyz.est$z, aspect=c(1, 1, 0.5), col = "lightblue",
#         xlab = "X", ylab = "Y", zlab = "Sinc( r )")


library(rgl)

# install.packages("akima")
library(akima)

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

# install.packages("randomForest")
library(randomForest)
rf_tree <- randomForest(comp_strength ~ ., data = data[train, ], ntree = 500, importance = TRUE)

# A look at variable relationships
partialPlot(rf_tree, x.var = "cement", pred.data = data, add = FALSE) # some interesting relationships appear

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
  geom_smooth() + 
  stat_density2d(geom = "tile", aes(alpha = ..density..), contour = FALSE) +
  geom_abline(intercept = 0, slope = 1)



data(LIDAR)
mba.int <- mba.surf(LIDAR, 300, 300, extend=TRUE)$xyz.est
## Not run:
##Image plot
image(mba.int, xaxs="r", yaxs="r")
##Perspective plot
persp(mba.int, theta = 135, phi = 30, col = "green3", scale = FALSE,
      ltheta = -120, shade = 0.75, expand = 10, border = NA, box = FALSE)
##For a good time I recommend using rgl
library(rgl)
ex <- 10
x <- mba.int[[1]]
y <- mba.int[[2]]
z <- ex*mba.int[[3]]
zlim <- range(z)
zlen <- zlim[2] - zlim[1] + 1
colorlut <- heat.colors(as.integer(zlen))
col <- colorlut[ z-zlim[1]+1 ]
open3d()
surface3d(x, y, z, color=col, back="lines")


