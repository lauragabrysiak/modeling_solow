#------------------------------------------------------
# Author: Laura GG
# Date: 20150416
# Description: Code creates two matrix with the time frames 
# (1960-1985) and (1986-2010). 
#------------------------------------------------------

# Part 1: Reading table and extracting OECD Countries

# 1.1-Read PWT table
pwt <- read.csv("C:/Users/Laura/Dropbox/Uni/MSc WiInf HU/6HS 4FS (SoSe 2015)/1-SPL_Statistical Programming Languages (QM 3LP)/SPL_paper_latex/data/pwt80.pop.csv")

# 1.2-Select specific columns by name
pwt.pop <- subset(pwt, select=c(country,year,pop))

# 1.3-Select OECD Countries
#----------------------------------------------------------------------------------------------
#c("Australia","Austria","Belgium","Canada","Chile", "Czech_","Denmark","Finland","France",
#"Germany","Greece","Ireland","Italy","Japan","Netherlands","New Zealand","Norway","Portugal",
#"Spain","Sweden","Switzerland","Turkey","United Kingdom","United States of America")
#pwt.oecd.data <- pwt.pop[which(pwt.pop$country == c("Australia",...,"United Kingdom","United States of America")),]
#----------------------------------------------------------------------------------------------
pwt.oecd.data <- subset(pwt.pop, pwt.pop[[1]] %in% c("Australia","Austria","Belgium","Canada","Chile", "Czech Republic","Denmark","Finland","France","Germany","Greece","Ireland","Italy","Japan","Netherlands","New Zealand","Norway","Portugal","Spain","Sweden","Switzerland","Turkey","United Kingdom","United States of America"), drop = TRUE)
pwt.oecd.1 <- subset(pwt.oecd.data, pwt.oecd.data[[2]] %in% c("1960":"2000"), drop = TRUE)
# 1.4-Converting data frame(s) into matrix
pwt.oecd <- as.matrix(pwt.oecd.1)
edit(pwt.oecd)


# Part 2: Creating matrix country/year (reshaping data)
#----------------------------------------------------------------------------------------------
#install.packages("reshape")
library(reshape)

#Melting data
pwt.oecd.md <- melt(pwt.oecd.1, id=c("country", "year")) #why the timeframes are lost?
edit(pwt.oecd.md)
#Casting data into new structure
pwt.oecd.xs <- cast(pwt.oecd.md, year ~ country)
oecd <- as.matrix(pwt.oecd.xs)
oecd.t <- t(oecd)
edit(oecd.t)

# Part 3: Visualizing the data
#----------------------------------------------------------------------------------------------
#install.packages("ggplot2")
#install.packages("gplots")
library(ggplot2)
library(gplots)
require(graphics); require(grDevices)

#--------------------------------------
# Try#1
#x <- as.matrix(oecd.t)
#rc <- rainbow(nrow(x), start = 0, end = .3)
#cc <- rainbow(ncol(x), start = 0, end = .3)
#
#hv <- heatmap(oecd.t, scale = "column", ColSideColors = cc, RowSideColors = rc, margins = c(5,10), xlab = "years", ylab =  "country", main = "OECD_population (1960-2010)")
# + scale_fill_gradient(low = "white", + high = "steelblue")
# the two re-ordering index vectors
#utils::str(hv) 
#--------------------------------------

quantile.range <- quantile(oecd.t, probs = seq(1, 1, 0.01), na.rm = TRUE)
palette.breaks <- seq(quantile.range["5%"], quantile.range["95%"],0.1)

# use http://colorbrewer2.org/ to find optimal divergent color palette (or set own)
color.palette <- colorRampPalette(c("#FC8D59", "#FFFFBF", "#91CF60"))(length(palette.breaks) - 1)

#Heatmap definition
#heatmap(oecd.t, scale = "row", trace = "years", col = color.palette, margins = c(5,10), xlab = "years", ylab = "OECD countries", main = "OECD Population (1960-2010)")
heatmap.2(oecd.t, col = color.palette, scale = "row", trace = "none", margins = c(5,10), xlab = "years", ylab = "OECD countries", main = "OECD Population (1960-2010)")

