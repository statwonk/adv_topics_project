# Advanced topics project
# Authors: Chris Peters & Chengfei Lu
# Purpose: Advanced topics class project
# https://github.com/cpeter9/adv_topics_project
# Link to data: http://archive.ics.uci.edu/ml/datasets/Concrete+Compressive+Strength

data <- read.csv("Concrete_Data.csv") # this line enters data

# Look at the data with this command
str(data)

names(data)[1:3] <- c("Name_1", "Name_2", "Name_3") # this renames variables

head(data, 3) # This shows you the first 'x' number of rows of the data

# this is comments from Chengfei


# This is my comment, this comment was written by Chris
#hist(data$Name_1)

