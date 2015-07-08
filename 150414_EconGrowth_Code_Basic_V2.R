setwd("C:/Users/Justus/Dropbox/SPL")
getwd()

install.packages("pwt8")
library("pwt8")
data("pwt8.0")

pwt <- pwt8.0 

###### Old data (resembling the 1992 original regression)

# Setting up the right-handside of the regression
# defining s: 1-share of household consumption-share of government consumption

hh <- numeric()
for (i in c("Australia","Austria","Belgium","Canada","Denmark","Finland","France","Germany",
            "Greece","Ireland","Italy","Japan","Netherlands","New Zealand","Norway","Portugal",
            "Spain","Sweden","Switzerland","Turkey","United Kingdom","United States of America"))
{hh[i]=mean((pwt[(pwt$country==i)&(pwt$year>"1960")&(pwt$year<"1985"),])$csh_c)}
mean.hh <- as.matrix(hh,nrow=22,ncol=1)
colnames(mean.hh) <- c("hh.bar")
print(mean.hh)

gov <- numeric()
for (i in c("Australia","Austria","Belgium","Canada","Denmark","Finland","France","Germany",
            "Greece","Ireland","Italy","Japan","Netherlands","New Zealand","Norway","Portugal",
            "Spain","Sweden","Switzerland","Turkey","United Kingdom","United States of America"))
{gov[i]=mean((pwt[(pwt$country==i)&(pwt$year>"1960")&(pwt$year<"1985"),])$csh_g)}
mean.gov <- as.matrix(gov,nrow=22,ncol=1)
colnames(mean.gov) <- c("gov.bar")
print(mean.gov)

mean.s <- (1-mean.hh+mean.gov)

X.1 <- log(mean.s)

# defining n (average number of persons engaged in millions between 1960 - 1985) + g + delta
# (the latter two variables are assumed to be equal to 0.05 for all economies)

n <- numeric()
for (i in c("Australia","Austria","Belgium","Canada","Denmark","Finland","France","Germany",
            "Greece","Ireland","Italy","Japan","Netherlands","New Zealand","Norway","Portugal",
            "Spain","Sweden","Switzerland","Turkey","United Kingdom","United States of America"))
{n[i]=mean(diff(log((pwt[(pwt$country==i)&(pwt$year>"1960")&(pwt$year<"1985"),])$emp)))}
mean.n <- as.matrix(n,nrow=22,ncol=1)
colnames(mean.n) <- c("n.bar")
print(mean.n)

X.2 <- log(mean.n+0.05)
print(X.2)

# defining Y/L (real GDP in 1985 divided by working age population in that year)
# For Y

y <- numeric()
for (i in c("Australia","Austria","Belgium","Canada","Denmark","Finland","France","Germany",
            "Greece","Ireland","Italy","Japan","Netherlands","New Zealand","Norway","Portugal",
            "Spain","Sweden","Switzerland","Turkey","United Kingdom","United States of America"))
{y[i]=(pwt[(pwt$country==i)&(pwt$year=="1985"),])$rgdpe}
y.1985 <- as.matrix(y,nrow=22,ncol=1)
colnames(y.1985) <- c("Y.1985")
print(y.1985)

# For L (working age population data obtained from world development report)

l <- numeric()
for (i in c("Australia","Austria","Belgium","Canada","Denmark","Finland","France","Germany",
            "Greece","Ireland","Italy","Japan","Netherlands","New Zealand","Norway","Portugal",
            "Spain","Sweden","Switzerland","Turkey","United Kingdom","United States of America"))
{l[i]=(pwt[(pwt$country==i)&(pwt$year=="1985"),])$emp}
l.1985 <- as.matrix(l,nrow=22,ncol=1)

w.share <- c(0.67,0.68,0.67,0.68,0.67,0.68,0.66,0.69,0.66,0.61,0.68,0.69,0.69,0.66,0.65,0.65,
                0.66,0.65,0.69,0.58,0.66,0.66)

wdr <- read.table(file.choose(),sep=",",dec=".") # set this up later


l.1985 <- 
colnames(l.1985) <- c("L.1985")
print(l.1985)

# Computing ln(Y/L)

Y.1 <- log(y.1985/l.1985) 
print(Y.1)

# Setting up the regression

regr <- lm(Y.1~X.1+X.2)
output.regr <- summary(regr)
print(output.regr)
plot(mean.s,y.1985)
abline(regr)



###### Estimating the new model with old OECD sample (see Excel Grouping 1992) 
# Setting up the right-handside of the regression
# defining s - csh_i used as a proxy for s (Shares of gross capital formation at current PPPs)

hh <- numeric()
for (i in c("Australia","Austria","Belgium","Canada","Denmark","Finland","France","Germany",
            "Greece","Ireland","Italy","Japan","Netherlands","New Zealand","Norway","Portugal",
            "Spain","Sweden","Switzerland","Turkey","United Kingdom","United States of America"))
{hh[i]=mean((pwt[(pwt$country==i)&(pwt$year>"1985")&(pwt$year<"2011"),])$csh_c)}
mean.hh <- as.matrix(hh,nrow=22,ncol=1)
colnames(mean.hh) <- c("hh.bar")
print(mean.hh)

gov <- numeric()
for (i in c("Australia","Austria","Belgium","Canada","Denmark","Finland","France","Germany",
            "Greece","Ireland","Italy","Japan","Netherlands","New Zealand","Norway","Portugal",
            "Spain","Sweden","Switzerland","Turkey","United Kingdom","United States of America"))
{gov[i]=mean((pwt[(pwt$country==i)&(pwt$year>"1985")&(pwt$year<"2011"),])$csh_g)}
mean.gov <- as.matrix(gov,nrow=22,ncol=1)
colnames(mean.gov) <- c("gov.bar")
print(mean.gov)

mean.s <- (1-mean.hh+mean.gov)

X.1 <- log(mean.s)

# defining n (average number of persons engaged in millions between 1985 - 2010)

n <- numeric()
for (i in c("Australia","Austria","Belgium","Canada","Denmark","Finland","France","Germany",
  "Greece","Ireland","Italy","Japan","Netherlands","New Zealand","Norway","Portugal",
  "Spain","Sweden","Switzerland","Turkey","United Kingdom","United States of America"))
{n[i]=mean(diff(log((pwt[(pwt$country==i)&(pwt$year>"1985")&(pwt$year<"2011"),])$emp)))}
mean.n <- as.matrix(n,nrow=22,ncol=1)
colnames(mean.n) <- c("n.bar")
print(mean.n)

# defining g (average growth rate between 1985 -2010) ##1986-2010??

g <- numeric()
for (i in c("Australia","Austria","Belgium","Canada","Denmark","Finland","France","Germany","Greece","Ireland","Italy","Japan","Netherlands","New Zealand","Norway","Portugal","Spain","Sweden","Switzerland","Turkey","United Kingdom","United States of America"))
{g[i]=mean(diff(log((pwt[(pwt$country==i)&(pwt$year>"1985")&(pwt$year<"2011"),])$rgdpna)))}
mean.g <- as.matrix(g,nrow=22,ncol=1)
colnames(mean.g) <- c("g.bar")
print(mean.g)

# building a vector of constants (equal to 0.03 for delta) for depreciation

delta.constant <- rep(0.03,22)
print(delta.constant)

# Merging factors for regression input right-handside (X.2)

X.2 <- log(mean.n+mean.g+delta.constant)
print(X.2)

# defining Y/L (real GDP in 2010 divided by emp in that year)
# For Y

y <- numeric()
for (i in c("Australia","Austria","Belgium","Canada","Denmark","Finland","France","Germany",
            "Greece","Ireland","Italy","Japan","Netherlands","New Zealand","Norway","Portugal",
            "Spain","Sweden","Switzerland","Turkey","United Kingdom","United States of America"))
{y[i]=(pwt[(pwt$country==i)&(pwt$year=="2010"),])$rgdpe}
y.2010 <- as.matrix(y,nrow=22,ncol=1)
colnames(y.2010) <- c("Y.2010")
print(y.2010)

# For L

l <- numeric()
for (i in c("Australia","Austria","Belgium","Canada","Denmark","Finland","France","Germany",
            "Greece","Ireland","Italy","Japan","Netherlands","New Zealand","Norway","Portugal",
            "Spain","Sweden","Switzerland","Turkey","United Kingdom","United States of America"))
{l[i]=(pwt[(pwt$country==i)&(pwt$year=="2010"),])$emp}
l.2010 <- as.matrix(l,nrow=22,ncol=1)
colnames(l.2010) <- c("L.2010")
print(l.2010)

# Computing ln(Y/L)

Y.1 <- log(y.2010/l.2010) 
print(Y.1)

# Setting up the regression

regr <- lm(Y.1~X.1+X.2)
output.regr <- summary(regr)
print(output.regr)

plot(mean.s,y.2010)
abline(regr)



###### Estimating the new model with old OECD sample (see Excel Grouping 1992) and traditional approximation of s
# Setting up the right-handside of the regression
# defining s - csh_i used as a proxy for s (Shares of gross capital formation at current PPPs)

s <- numeric()
for (i in c("Australia","Austria","Belgium","Canada","Denmark","Finland","France","Germany",
            "Greece","Ireland","Italy","Japan","Netherlands","New Zealand","Norway","Portugal",
            "Spain","Sweden","Switzerland","Turkey","United Kingdom","United States of America"))
{s[i]=mean((pwt[(pwt$country==i)&(pwt$year>"1985")&(pwt$year<"2011"),])$csh_i)}
mean.s <- as.matrix(s,nrow=22,ncol=1)
colnames(mean.s) <- c("n.bar")
print(mean.s)

X.1 <- log(mean.s)

# defining n (average number of persons engaged in millions between 1985 - 2010)

n <- numeric()
for (i in c("Australia","Austria","Belgium","Canada","Denmark","Finland","France","Germany",
            
            
            "Greece","Ireland","Italy","Japan","Netherlands","New Zealand","Norway","Portugal",
            "Spain","Sweden","Switzerland","Turkey","United Kingdom","United States of America"))
{n[i]=mean(diff(log((pwt[(pwt$country==i)&(pwt$year>"1985")&(pwt$year<"2011"),])$emp)))}
mean.n <- as.matrix(n,nrow=22,ncol=1)
colnames(mean.n) <- c("n.bar")
print(mean.n)

# defining g (average growth rate between 1985 -2010)

g <- numeric()
for (i in c("Australia","Austria","Belgium","Canada","Denmark","Finland","France","Germany","Greece","Ireland","Italy","Japan","Netherlands","New Zealand","Norway","Portugal","Spain","Sweden","Switzerland","Turkey","United Kingdom","United States of America"))
{g[i]=mean(diff(log((pwt[(pwt$country==i)&(pwt$year>"1985")&(pwt$year<"2011"),])$rgdpna)))}
mean.g <- as.matrix(g,nrow=22,ncol=1)
colnames(mean.g) <- c("g.bar")
print(mean.g)

# building a vector of constants (equal to 0.03 for delta) for depreciation

delta.constant <- rep(0.03,22)
print(delta.constant)

# Merging factors for regression input right-handside (X.2)

X.2 <- log(mean.n+mean.g+delta.constant)
print(X.2)

# defining Y/L (real GDP in 2010 divided by emp in that year)
# For Y

y <- numeric()
for (i in c("Australia","Austria","Belgium","Canada","Denmark","Finland","France","Germany",
            "Greece","Ireland","Italy","Japan","Netherlands","New Zealand","Norway","Portugal",
            "Spain","Sweden","Switzerland","Turkey","United Kingdom","United States of America"))
{y[i]=(pwt[(pwt$country==i)&(pwt$year=="2010"),])$rgdpe}
y.2010 <- as.matrix(y,nrow=22,ncol=1)
colnames(y.2010) <- c("Y.2010")
print(y.2010)

# For L

l <- numeric()
for (i in c("Australia","Austria","Belgium","Canada","Denmark","Finland","France","Germany",
            "Greece","Ireland","Italy","Japan","Netherlands","New Zealand","Norway","Portugal",
            "Spain","Sweden","Switzerland","Turkey","United Kingdom","United States of America"))
{l[i]=(pwt[(pwt$country==i)&(pwt$year=="2010"),])$emp}
l.2010 <- as.matrix(l,nrow=22,ncol=1)
colnames(l.2010) <- c("L.2010")
print(l.2010)

# Computing ln(Y/L)

Y.1 <- log(y.2010/l.2010) 
print(Y.1)

# Setting up the regression

regr <- lm(Y.1~X.1+X.2)
output.regr <- summary(regr)
print(output.regr)

plot(mean.s,y.2010)
abline(regr)




###### Old data (resembling the 1992 original regression)
# Setting up the right-handside of the regression
# defining s: 1-share of household consumption-share of government consumption

hh <- numeric()
for (i in c("Australia","Austria","Belgium","Canada","Denmark","Finland","France","Germany",
            "Greece","Ireland","Italy","Japan","Netherlands","New Zealand","Norway","Portugal",
            "Spain","Sweden","Switzerland","Turkey","United Kingdom","United States of America"))
{hh[i]=mean((pwt[(pwt$country==i)&(pwt$year>"1960")&(pwt$year<"1985"),])$csh_c)}
mean.hh <- as.matrix(hh,nrow=22,ncol=1)
colnames(mean.hh) <- c("hh.bar")
print(mean.hh)

gov <- numeric()
for (i in c("Australia","Austria","Belgium","Canada","Denmark","Finland","France","Germany",
            "Greece","Ireland","Italy","Japan","Netherlands","New Zealand","Norway","Portugal",
            "Spain","Sweden","Switzerland","Turkey","United Kingdom","United States of America"))
{gov[i]=mean((pwt[(pwt$country==i)&(pwt$year>"1960")&(pwt$year<"1985"),])$csh_g)}
mean.gov <- as.matrix(gov,nrow=22,ncol=1)
colnames(mean.gov) <- c("gov.bar")
print(mean.gov)

mean.s <- (1-mean.hh+mean.gov)

X.1 <- log(mean.s)

# defining n (average number of persons engaged in millions between 1960 - 1985)

n <- numeric()
for (i in c("Australia","Austria","Belgium","Canada","Denmark","Finland","France","Germany",
            "Greece","Ireland","Italy","Japan","Netherlands","New Zealand","Norway","Portugal",
            "Spain","Sweden","Switzerland","Turkey","United Kingdom","United States of America"))
{n[i]=mean(diff(log((pwt[(pwt$country==i)&(pwt$year>"1960")&(pwt$year<"1985"),])$emp)))}
mean.n <- as.matrix(n,nrow=22,ncol=1)
colnames(mean.n) <- c("n.bar")
print(mean.n)

# defining g (average growth rate between 1960 - 1985)

g <- numeric()
for (i in c("Australia","Austria","Belgium","Canada","Denmark","Finland","France","Germany","Greece","Ireland","Italy","Japan","Netherlands","New Zealand","Norway","Portugal","Spain","Sweden","Switzerland","Turkey","United Kingdom","United States of America"))
{g[i]=mean(diff(log((pwt[(pwt$country==i)&(pwt$year>"1960")&(pwt$year<"1985"),])$rgdpna)))}
mean.g <- as.matrix(g,nrow=22,ncol=1)
colnames(mean.g) <- c("g.bar")
print(mean.g)

# building a vector of constants (equal to 0.03 for delta) for depreciation

delta.constant <- rep(0.03,22)
print(delta.constant)

# Merging factors for regression input right-handside (X.2)

X.2 <- log(mean.n+mean.g+delta.constant)
print(X.2)

# defining Y/L (real GDP in 2010 divided by emp in that year)
# For Y

y <- numeric()
for (i in c("Australia","Austria","Belgium","Canada","Denmark","Finland","France","Germany",
            "Greece","Ireland","Italy","Japan","Netherlands","New Zealand","Norway","Portugal",
            "Spain","Sweden","Switzerland","Turkey","United Kingdom","United States of America"))
{y[i]=(pwt[(pwt$country==i)&(pwt$year=="1985"),])$rgdpe}
y.1985 <- as.matrix(y,nrow=22,ncol=1)
colnames(y.1985) <- c("Y.1985")
print(y.1985)

# For L

l <- numeric()
for (i in c("Australia","Austria","Belgium","Canada","Denmark","Finland","France","Germany",
            "Greece","Ireland","Italy","Japan","Netherlands","New Zealand","Norway","Portugal",
            "Spain","Sweden","Switzerland","Turkey","United Kingdom","United States of America"))
{l[i]=(pwt[(pwt$country==i)&(pwt$year=="1985"),])$emp}
l.1985 <- as.matrix(l,nrow=22,ncol=1)
colnames(l.1985) <- c("L.1985")
print(l.1985)

# Computing ln(Y/L)

Y.1 <- log(y.1985/l.1985) 
print(Y.1)

# Setting up the regression

regr <- lm(Y.1~X.1+X.2)
output.regr <- summary(regr)
print(output.regr)
plot(mean.s,y.1985)
abline(regr)
