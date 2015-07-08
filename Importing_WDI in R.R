setwd("C:/Users/Justus/Dropbox/SPL")

install.packages("WDI")
library("WDI")

data <- WDI(country = "all", indicator = "SL.TLF.SECO.ZS",
    start = 1960, end = 1985, extra = FALSE, cache = NULL)

school <- read.csv2("C:/Users/Justus/Dropbox/SPL/Data/UNESCO_School_Secondary.csv")

wdi.sec <- wdi[wdi$Indicator.Code=="SL.TLF.PRIM.ZS",] # obtaining a proxy for SCHOOL-variable 
# (percent of working age population with secondary school education)

write.table(school,file="UNESCO_SCHOOL.csv",sep=";",dec=",",col.names=TRUE,row.names=FALSE)


school <- wdi[wdi$Indicator=="Gross enrolment ratio, secondary, both sexes (%)",]

