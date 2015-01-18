### R code from vignette source 'CItools.Rnw'

###################################################
### code chunk number 1: class-ProcesControl
###################################################
library(CItools)
ctr <- ProcessControl()
names(ctr)


###################################################
### code chunk number 2: class-compind
###################################################
library(CItools)
data(cip)
head(cip)
index_cip = compind(country~., time="year", for.period = 2004:2010, data=cip,
    ISOcode='ISOcode', ISOtype='UN',
    ctr=ProcessControl(weights=c("fixed", 1/6, 1/6, 1/12, 1/12, 1/12, 1/12, 1/6, 1/6), aggregation="linear"))

class(index_cip)
names(index_cip)


###################################################
### code chunk number 3: plot-radial-1
###################################################
library(CItools)
data(cip)
head(cip)
## compute Composite index (eight-indicators linear) for a range of years
##  ISOcode specifies the column name containing ISOCode.
##  ISOtype specifies what ISO code it is. In CIP, ISO code is numeric hence UN
##
index_cip1 = compind(country~., time="year", for.period = 2010, data=cip,
    ISOcode='ISOcode', ISOtype='UN',
    ctr=ProcessControl(weights=c("fixed", 1/6, 1/6, 1/12, 1/12, 1/12, 1/12, 1/6, 1/6),
    aggregation="linear"))

plot(index_cip1, which = 'radial plot', entity=c('India', 'Japan', 'United Kingdom'))



###################################################
### code chunk number 4: plot-radial-2
###################################################
## compute Composite index (eight-indicators linear) for a range of years
index_cip = compind(country~., time="year", for.period = 2004:2010, data=cip,
    ISOcode='ISOcode', ISOtype='UN',
    ctr=ProcessControl(weights=c("fixed", 1/6, 1/6, 1/12, 1/12, 1/12, 1/12, 1/6, 1/6),
    aggregation="linear"))

plot(index_cip, which='radial plot', entity = 'Japan')


