###############################################################
# Done by : Boon Tze Suen                                     #
# UOW ID  : 7311898                                           #
# Done on : 06 Feb 2023                                       #
###############################################################

# Install library
#install.packages(c("kohonen", "ggplot2", "maptools", "sp", "reshape2", "rgeos"))
#install.packages("Hmisc")

# Import
library(ggplot2)
library(kohonen)
library(sp)
library(maptools)
library(rgeos) # Interface to Geometry engine
library(MASS)  # Function and datasets to support Venables & Ripley 
library(Hmisc) # Contains many function useful for data analysis

# Colour palette definition make pretty
pretty_palette <- c("#1f77b4", '#ff7f0e', '#2ca02c', '#d62728',
                             '#9467bd', '#8c564b', '#e377c2')
                             
# Load the data into Data_Frame

df <- read.csv("process.csv")

# Function describe(), provide information about the data set
describe(df)

str(df)


# to find attribute that have correlation to "credit_rating", 
# we perfrom a function call cor(), followed by SOM map to analyse
# heatmap and see how easy it is to cluster the data.

corTable = abs(cor(df, y = df$sri2010_cool))
corTable = corTable[order(corTable, decreasing = TRUE),,drop = FALSE]
head(corTable,11)



corTable = abs(cor(df, y = df$sri2010_heat))
corTable = corTable[order(corTable, decreasing = TRUE),,drop = FALSE]
head(corTable,11)