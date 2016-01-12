# Part 3: Visualizing the data
#--------------------------------------
# author: Laura Gabrysiak
# date: 20150709
# description: 
#--------------------------------------
setwd("C:/.../code_r_solow")
# Procedure:
# 1. load data
# 2. sort data
# 3. prepare data
# 4. iterate data
# 5. heatmap
#--------------------------------------
# 
#--------------------------------------
install.packages("ggplot2")
install.packages("gplots")
#--------------------------------------
library(ggplot2)
library(gplots)
library(reshape)
library(zoo)
#--------------------------------------
require(graphics)
require(grDevices)
#--------------------------------------
#
#--------------------------------------
# 1. load data

oecd.viz <- read.csv("C:/Users/Laura/Dropbox/Uni/MSc WiInf HU/6HS 4FS (SoSe 2015) (UHHU Project, Florida)/1-SPL_Statistical Programming Languages (QM 3LP)/Project_Solow/code_r_solow/oecd.csv", sep=",")
colnames(oecd.viz)[1] <- "country"

# 2. sort data\
oecd.viz <- oecd.viz[order(oecd.viz$country),]

# 3. prepare data : Get rid of first column (Copy country names to row names)
row.names(oecd.viz) <- oecd.viz$country

#check dim(oecd.viz)
oecd.viz <- oecd.viz[,2:42]

# 4. convert data to matrix
oecd.viz <- as.matrix(oecd.viz)
#transpose data.frame
oecd.viz <- t(oecd.viz)

# 5. heatmap
#oecd.viz.heatmap <- heatmap(oecd.viz, Rowv=NA, Colv=NA, col = cm.colors(256), scale="column", margins=c(5,10))
oecd.viz.heatmap <- heatmap(oecd.viz, Rowv=NA, Colv=NA, col = cm.colors(256), scale="column", margins=c(5,10))
