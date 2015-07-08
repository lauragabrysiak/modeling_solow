#------------------------------------------------------
# Author: Laura GG
# Date: 20150416
# Description: Code creates two matrix with the time frames 
# (1960-1985) and (1986-2010). 
#------------------------------------------------------
#OECD Countries: Australia, Austria, Belgium, Canada, Chile, Czech Republic, Denmark, Estonia, Finland, France, Germany, Greece, Iceland, Ireland, Israel, Italy, Japan, Luxembourg, Mexico, Netherlands, New Zealand, Norway, Poland, Portugal, Republic of Korea, Slovakia, Slovenia, Spain, Sweden, Switzerland, Turkey, United Kingdom, United States
#OPEC Countries: Algeria, Angola, Ecuador, Iran, Iraq, Kuwait, Libya, Nigeria, Qatar, Saudi Arabia, United Arab Emirates, Venezuela (wikipedia)

# 1-Read WDI table
wdi <- read.csv("C:/Users/Laura/Dropbox/Uni/MSc WiInf HU/6HS 4FS (SoSe 2015)/Statistical Programming Languages (QM 3LP)/SPL_paper_latex/data/sp.pop.1564.to.zs_Indicator_en_excel_v3.csv")

# 2-Droping up specific columns by name
wdi.clean <- subset(wdi, select=-c(X2014,Country.Code,Indicator.Name,Indicator.Code))

# 3-Extracting OECD countries:
wdi.oecd.data <- subset(wdi.clean, wdi.clean[[1]] %in% c("Australia","Austria","Belgium","Canada","Chile", "Czech Republic","Denmark","Finland","France","Germany","Greece","Ireland","Italy","Japan","Netherlands","New Zealand","Norway","Portugal","Spain","Sweden","Switzerland","Turkey","United Kingdom","United States"), drop = TRUE)
wdi.oecd <- as.matrix(wdi.oecd.data)

# 4-Creating WDI subsets (data.frame)
wdi.oecd.1 <- subset(wdi.oecd, select=c(Country.Name,X1960:X1985))
wdi.oecd.2 <- subset(wdi.oecd, select=c(Country.Name,X1986:X2010))   

# 4-Converting data frames into matrix
#wdi.1 <- as.matrix(wdi.1.data)
#wdi.2 <- as.matrix(wdi.2.data)


