setwd("C:/Users/Justus/Dropbox/SPL")
getwd()

###### Resembling the Wood Regression using the dataset provided
###### First Step: Resembling the 1992 original regression, including Human Capital component
#---------------------------------------------------------------------------------------------

install.packages("pwt8")
library("pwt8")
data("pwt8.0")
pwt <- pwt8.0

# Sample: OECD countries, omitting Germany since no available data on secondary schooling

countries <- c("Australia","Austria","Belgium","Canada","Denmark","Finland","France",
             "Greece","Ireland","Italy","Japan","Netherlands","New Zealand","Norway","Portugal",
             "Spain","Sweden","Switzerland","Turkey","United Kingdom","United States of America")

# Time Span: 1970-1985

years <- c(1970:1985)

###### Defining the independent variables (X.1 and X.2)
#----------------------------------------------------------------------------------------------

# Computing s as share of gross capital formation at current PPP

s.inv <- numeric()
for (i in countries)
{s.inv[i]=mean((pwt[(pwt$country==i)&(pwt$year>="1970")&(pwt$year<="1985"),])$csh_i)}
mean.s.inv <- as.matrix(s.inv,nrow=length(countries),ncol=1)
print(mean.s.inv)

X.1 <- log(mean.s.inv)

# Defining n (average number of persons engaged in millions between 1960 - 1985 on the basis
# of WDR working-age tables) + g + delta (the latter two variables are assumed to be equal to 0.05 for all economies)

# 1.1-Creating a matrix of population data for OECD countries 1960 to 1985

# 1.2-Select specific columns by name
pwt.pop <- subset(pwt, select=c(country,year,pop))

# 1.3-Select relevant countries from rows and sort data alphabetically (by countries)
pwt.sample.data <- subset(pwt.pop, pwt.pop[[1]] %in% countries, drop = TRUE)
pwt.sample.data$country <- as.character(pwt.sample.data$country)
pwt.sample.sort <- pwt.sample.data[order(pwt.sample.data$country),]

# 1.4-Converting data frame(s) into matrix
pwt.sample <- as.matrix(pwt.sample.sort)

# 1.5-Creating matrix country/year (reshaping data)
install.packages("reshape")
library(reshape)

# 1.6-Melting data
pwt.sample.md <- melt(pwt.sample.sort, id=c("country","year"))

# 1.7-Casting data into new structure
pwt.sample.xs <- cast(pwt.sample.md, year ~ country)
pop.sample <- as.matrix(pwt.sample.xs)
sample.t <- t(pop.sample)

# 1.8-Selecting relevant time-span
pop.fitted <- sample.t[,colnames(sample.t)%in%years] 
              
# Getting the share of working-age population from World Bank data

# 2.1-Read WDI table
wdi <- read.csv("C:/Users/Justus/Dropbox/SPL/Data/sp.pop.1564.to.zs_Indicator_en_excel_v4.csv")

# 2.2-Droping up specific columns by name
wdi.clean <- subset(wdi, select=-c(X2014,Country.Code,Indicator.Name,Indicator.Code))

# 2.3-Selecting relevant countries
wdi.countries <- subset(wdi.clean, wdi.clean[[1]] %in% countries, drop = TRUE)
wdi.countries <- wdi.countries[order(wdi.countries$Country.Name),]    # ordering alphabetically

# 2.4-Adjusting the data.frame so that it has the same format as "pop.fitted" and selecting the relevant time-span
wdi.countries$Country.Name <- NULL    # dropping column containing country names as strings
rownames(wdi.countries) <- c(countries)
colnames(wdi.countries) <- c(1960:2013)

# 2.5-Creating WDI subsets for relevant time-span (data.frame)
wdi.data <- wdi.countries[,colnames(wdi.countries)%in%years]

# 2.6-Converting data frames into matrix creating shares from absolute numbers
wdi.dat <- as.numeric(as.matrix(wdi.data))
wdi.matrix <- matrix(wdi.dat,nrow=length(countries),ncol=length(years))

share.fitted <- wdi.matrix*0.01

# 3.1-Computing labour force variable by merging total population and share of working-age population
emp <- pop.fitted*share.fitted
rownames(emp) <- c(countries)
colnames(emp) <- c(years)

# Constructing n (average working age population growth between 1960 and 1985) 

emp.t <- t(emp)

n <- numeric()
for (i in 1:length(countries)){n[i]=mean(diff(log(emp.t[,i])))}
n.growth <- as.matrix(n)
rownames(n.growth) <- c(countries)
colnames(n.growth) <- c("n.growth")

X.2 <- log(n.growth+0.05)
print(X.2)

# Computing SCHOOL-Variable: Original: fraction of eligible population (12 to 17) enrolled in secondary school
# from UNESCO data base, multiplied by the fraction of working-age population that is of
# school age (15 to 19)
# Our Approach, following Wood (2013): total percentage of secondary school enrollment 
# from UNESCO Education statistics divided by share of working age population

# 4.1-read data downloaded from Wood-Homepage
library(foreign)
MRW <- read.dta("C:/Users/Justus/Dropbox/SPL/Data/Datasets_Wood/MRW.dta")
TTW <- read.dta("C:/Users/Justus/Dropbox/SPL/Data/Datasets_Wood/TTW.dta")

# 4.2-Select relevant countries from Wood sample MRW (by countries), select time span
school <- MRW[MRW$country==countries,]$school

school <- 

X.3 <- school
print(X.3)

###### Defining the dependent variable Y/L (real GDP in 1985 divided by working age population in that year)
#----------------------------------------------------------------------------------------------- 
# For Y

y <- numeric()
for (i in countries)
{y[i]=(pwt[(pwt$country==i)&(pwt$year=="1985"),])$rgdpna}
y.1985 <- as.matrix(y,nrow=length(country),ncol=1)
print(y.1985)

# Using emp in 1985 for L

l.1985 <- emp[,length(years)]
l.1985 <- matrix(l.1985,nrow=length(l.1985),ncol=1)
rownames(l.1985) <- c(countries)

# Computing ln(Y/L)

Y.1 <- log(y.1985/l.1985) 
print(Y.1)

###### Setting up the regression
#-----------------------------------------------------------------------------------------

install.packages("lmtest") 
library("lmtest")

regr <- lm(Y.1~X.1+X.2+X.3)
qqnorm(residuals(regr))  # adjusting for possibly heteroscedastic residuals
qqline(residuals(regr))

output.regr <- coeftest(regr)
print(output.regr)


# Aside: Style of regression output

install.packages("stargazer")
library("stargazer")

stargazer(regr,type="text", dep.var.labels=c("log GDP per working-age person in 1985", 
                                             covariate.labels=c("ln(I/GDP)", "ln(n+g+delta)"),out="default.htm"),intercept.bottom=FALSE)



