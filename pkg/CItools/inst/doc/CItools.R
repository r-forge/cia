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

index_cip = compind(country~., time="year", for.period = 2005:2010, data=cip,
    ctr=ProcessControl(weights=c("fixed",1/6,1/6,1/12,1/12,1/12,1/12,1/6,1/6),
        aggregation="linear"))
class(index_cip)
names(index_cip)


###################################################
### code chunk number 3: plot-bar-1
###################################################
## compute Composite index (eight-indicators linear) for one year
##
index_cip1 = compind(country~., time="year", for.period = 2010, data=cip,
    ctr=ProcessControl(weights=c("fixed",1/6,1/6,1/12,1/12,1/12,1/12,1/6,1/6),
    aggregation="linear"))

## default plot for composite indicator - bar plot for only one year
plot(index_cip1)


###################################################
### code chunk number 4: plot-bar-2
###################################################
## Bar plot for composite indicator, one year and several selected entities
plot(index_cip1, entity=c("India", "Japan", "United Kingdom",
    "Bulgaria", "Austria", "China"))


###################################################
### code chunk number 5: plot-bar-3
###################################################
## Bar plot for composite indicator, one year and several
##  selected entities.
##  The option used is to highlight the selected countries
##  on the global graph
plot(index_cip1, which="bar2", entity=c("India", "Japan",
    "United Kingdom", "Bulgaria", "Austria", "China"))


###################################################
### code chunk number 6: plot-bar-4
###################################################
## Bar plot for composite indicator, one year and
##  several selected entities
##  A criterion (30% highly ranked countries according to CIP)
##  is used to select the countries
##
plot(index_cip1, entity.num="first 30 percent")


###################################################
### code chunk number 7: plot-bar-5
###################################################
## Bar plot for composite indicator, one year and several
##  selected entities
## A criterion (30% highly ranked countries according to CIP)
##  is used to select the countries
## Title, X-label and Y-label are selected by the user
##
plot(index_cip1, entity.num="first 30 percent", xlab="Country",
ylab="CIP index", title="CIP index of the 30% highly ranked countries.")


###################################################
### code chunk number 8: plot-bar-6
###################################################
## Bar plot for composite indicator, one year and
##  several selected entities
##  A criterion (30% lowest ranked countries according to
##  the CIP index, in descending order) is used to select the countries
## Title, X-label and Y-label are selected by the user
##
plot(index_cip1, entity.num="last 30 percent", sort="descending",
    xlab="Country", ylab="CIP index",
    title="CIP index of the 30% lowest ranked countries.")


###################################################
### code chunk number 9: plot-line-1
###################################################
## default plot for composite indicator - line plot for more than one year
plot(index_cip)


###################################################
### code chunk number 10: plot-line-2
###################################################
plot(index_cip, entity=c("Austria", "India", "Bulgaria", "Oman"),
ylab="CIP Index", xlab="", title="CIP index 2005-2010 for the EU-15 countries")


###################################################
### code chunk number 11: plot-line-3
###################################################
mycode <- getCountryCode("EU-15")       # get the code of the group EU-15
mygroup <- getCountryGroup(mycode)      # get the list of (UN) codes of the countries in the group EU-15
myct <- getCountryName(mygroup)         # get the names of the countries in the group
myct
plot(index_cip, entity=myct, ylab="CIP Index", xlab="",
title="CIP index 2005-2010 for the EU-15 countries")


###################################################
### code chunk number 12: plot-radial-1
###################################################
plot(index_cip1, which = 'radial', entity=c('India', 'Japan', 'United Kingdom'))


###################################################
### code chunk number 13: plot-radial-2
###################################################
plot(index_cip, which='radial', entity = 'Japan')


###################################################
### code chunk number 14: plot-map-1
###################################################
plot(index_cip1, which="map")
box()


###################################################
### code chunk number 15: plot-map-2
###################################################
plot(index_cip1, which="map", entity=c("Austria", "Australia", "India", "United States of America"))
box()


###################################################
### code chunk number 16: plot-map-3
###################################################
plot(index_cip1, which="map", entity.num="first 30 percent")
box()


