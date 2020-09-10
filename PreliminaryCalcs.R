# WARNING:  THIS IS A HACK
# IT IS CODE HASTELY CRAFTED TO ILLUSTRATE THE USE OF RECURSIVE PARTITIONING
# This is a record of ALL that was done.
# There are many plots used to guide the development.
# Predictions from the GHG measures to all records can be found by looking for "pred" below.
# The plots displayed in the report are at the end.

User <- "DFA"
#User <- "BAT"
if(User == "DFA"){
    CurrentDirectory <- "/Users/david/Documents/CONSULT/Knights"
}
datafile <- "2016 carbon data modelling"

# Read data
raw <- read.csv(paste(CurrentDirectory,"/",datafile,".csv",sep=""))

# See the variables
colnames(raw)

# Select some for analysis
Subsector <- as.character(raw$RBICS.subsector.Code)
TotalGHG <- raw$Total.GHG.emssion..1..2.
Scope1 <- raw$GHG.Scope.1
Scope2 <- raw$GHG.Scope.2
Revenue <- as.numeric(as.character(raw$PPP..Revenue))
Employees <- as.numeric(as.character(raw$Number.of.employees))
FixedAssets <-  as.numeric(as.character(raw$Gross.Fixed.Assets..PPP...millions.))
IntangibleAssets <- as.numeric(as.character(raw$Disclosed.Intangible.Assets..PPP...millions.))
TotalPower <- as.numeric(as.character(raw$TOTAL_POWER_GENERATED))
Country <- as.character(raw$Headquarterscountry)

Used <- !is.na(TotalGHG)  # Data available

# Some preliminary plots

hist(log(TotalGHG[Used]))

plot( log(TotalPower[Used]),log(TotalGHG[Used]))
plot( log(Employees[Used]),log(TotalGHG[Used]))
plot( log(Revenue[Used]),log(TotalGHG[Used]))

# Normalize by revenue

TotalGHGbyRevenue <- TotalGHG / Revenue
EmployeesByRevenue <- Employees / Revenue
AssetsByRevenue <- FixedAssets / Revenue
IntangiblesByRevenue <- IntangibleAssets / Revenue
TotalPowerByRevenue <- TotalPower / Revenue

plot(log(EmployeesByRevenue[Used]), log(TotalGHGbyRevenue[Used]))
plot(log(AssetsByRevenue[Used]), log(TotalGHGbyRevenue[Used]))

# Now look at subsectors
GHGbySubsectorsplit <- split(log(TotalGHGbyRevenue[Used]), Subsector[Used])
hist(sapply(GHGbySubsectorsplit,length))
hist(sapply(GHGbySubsectorsplit,median))


# Order Subsectors by median GHG
MedSectorGHGsplit <- sapply(GHGbySubsectorsplit,median,na.rm=TRUE)
MedSectorGHGsplit[is.na(MedSectorGHGsplit)] <- min(MedSectorGHGsplit,na.rm=TRUE)

OrderMeds <- order(MedSectorGHGsplit)
RankMeds <- rank(MedSectorGHGsplit,ties.method = "random")
SubsectorRank <- unsplit(RankMeds,Subsector[Used])
plot(SubsectorRank, log(TotalGHGbyRevenue[Used]))

# Create variables for analysis

LUTotGHGbyRev <- log(TotalGHG / Revenue)[Used]
LUEmpByRev <- log(Employees / Revenue)[Used]
LUAssetsByRev <- log(FixedAssets / Revenue)[Used]
LUIntangByRev <- log(IntangibleAssets / Revenue)[Used]
LUTotPowByRev <- log(TotalPower / Revenue)[Used]

library(rpart)

fit0 <- rpart(LUTotGHGbyRev ~ LUEmpByRev + LUAssetsByRev + LUIntangByRev + LUTotPowByRev + SubsectorRank,cp=0.005)
plot(fit0)
#text(fit0, use.n = TRUE, all = TRUE,cex=0,5)
text(fit0,splits=TRUE,cex=0.5,digits=2,use.n=TRUE)

# Order Counties by median GHG

GHGbyCountrysplit <- split(log(TotalGHGbyRevenue[Used]), Country[Used])

MedCountryGHGsplit <- sapply(GHGbyCountrysplit,median,na.rm=TRUE)
MedCountryGHGsplit[is.na(MedCountryGHGsplit)] <- min(MedCountryGHGsplit,na.rm=TRUE)

OrderMeds <- order(MedCountryGHGsplit)
RankMeds <- rank(MedCountryGHGsplit,ties.method = "random")
CountryRank <- unsplit(RankMeds,Country[Used])
plot(CountryRank, log(TotalGHGbyRevenue[Used]))

fit1 <- rpart(LUTotGHGbyRev ~ LUEmpByRev + LUAssetsByRev + LUIntangByRev + LUTotPowByRev + SubsectorRank + CountryRank,cp=0.005)
plot(fit1)
#text(fit0, use.n = TRUE, all = TRUE,cex=0,5)
text(fit1,splits=TRUE,cex=0.5,digits=2,use.n=TRUE)

plot(fit1,branch=0.6,compress=T,uniform=T)
#text(fit0, use.n = TRUE, all = TRUE,cex=0,5)
text(fit1,splits=TRUE,cex=0.5,digits=2,use.n=TRUE)

plot(fit1$frame$yval[fit1$where], LUTotGHGbyRev[!is.na(LUTotGHGbyRev)])

fit2 <- rpart(LUTotGHGbyRev ~ LUEmpByRev + LUAssetsByRev + LUIntangByRev + LUTotPowByRev + SubsectorRank + CountryRank, minbucket = 5,cp=0.00001)

plot(fit2$frame$yval[fit2$where], LUTotGHGbyRev[!is.na(LUTotGHGbyRev)])

fit3 <- rpart(LUTotGHGbyRev ~ LUEmpByRev + LUAssetsByRev + LUIntangByRev + LUTotPowByRev + SubsectorRank + CountryRank, minbucket = 5,cp=0.00001)

# Now fit all

# Extend SubsectorRank

SubsectorRankA <- rep(0,length(Used))
# split used subsectors by rank
ssbr <- split(Subsector[Used],SubsectorRank)
srmbr <- split(SubsectorRank,SubsectorRank)
first <- function(x){
    x[1]
}
rsmbr <- sapply(srmbr,first)
for(i in 1: length(ssbr)){
    SubsectorRankA[Subsector %in% ssbr[[i]]] <- rsmbr[i]
}

# for all in split,

# Extend Country Rank
CountryRankA <- rep(0,length(Used))
# split used Countrys by rank
ssbr <- split(Country[Used],CountryRank)
srmbr <- split(CountryRank,CountryRank)
first <- function(x){
    x[1]
}
rsmbr <- sapply(srmbr,first)
for(i in 1: length(ssbr)){
    CountryRankA[Country %in% ssbr[[i]]] <- rsmbr[i]
}

LUTotGHGbyRevA <- log(TotalGHG / Revenue)
LUEmpByRevA <- log(Employees / Revenue)
LUAssetsByRevA <- log(FixedAssets / Revenue)
LUIntangByRevA <- log(IntangibleAssets / Revenue)
LUTotPowByRevA <- log(TotalPower / Revenue)

fit3 <- rpart(LUTotGHGbyRevA ~ LUEmpByRevA + LUAssetsByRevA + LUIntangByRevA + LUTotPowByRevA + SubsectorRankA + CountryRankA, minbucket = 5,cp=0.01,weights = Used)

plot(fit3$frame$yval[fit3$where], LUTotGHGbyRev[!is.na(LUTotGHGbyRev)])

fit4 <- rpart(LUTotGHGbyRevA ~ LUEmpByRevA , minbucket = 5,cp=0.0001,weights = Used)

fit4 <- rpart(LUTotGHGbyRevA ~ LUEmpByRevA + LUAssetsByRevA + LUIntangByRevA + LUTotPowByRevA + SubsectorRankA + CountryRankA, minbucket = 5,cp=0.000001,weights = Used)

par(mfrow=c(2,1))

plot(fit4$frame$yval[fit4$where], LUTotGHGbyRev[!is.na(LUTotGHGbyRev)])

# Compute fitted values for all the records

newdata <- list(LUEmpByRevA = LUEmpByRevA,LUAssetsByRevA = LUAssetsByRevA,LUIntangByRevA=LUIntangByRevA, LUTotPowByRevA = LUTotPowByRevA, SubsectorRankA = SubsectorRankA,CountryRankA=CountryRankA )
pred <- predict(fit4,newdata)

#Check predictions match fitted values
junk <- (pred[Used])[!is.na(LUTotGHGbyRev)] - fit4$frame$yval[fit4$where]

plot(fit4$frame$yval[fit4$where],junk)

# Miscellaneous other plots
par(mfrow=c(1,1))
fit4 <- rpart(LUTotGHGbyRevA ~ LUEmpByRevA + LUAssetsByRevA + LUIntangByRevA + LUTotPowByRevA + SubsectorRankA + CountryRankA, minbucket = 5,cp=0.000001,weights = Used)
#fit4 <- rpart(LUTotGHGbyRevA ~ LUEmpByRevA + LUAssetsByRevA + LUIntangByRevA + LUTotPowByRevA + SubsectorRankA + CountryRankA, minbucket = 5,cp=0.01,weights = Used)
fit4 <- rpart(LUTotGHGbyRevA ~ LUAssetsByRevA + SubsectorRankA , minbucket = 5,cp=0.005,weights = Used)

plot(fit4$frame$yval[fit4$where], LUTotGHGbyRev[!is.na(LUTotGHGbyRev)],pch=".")

plot(SubsectorRankA[Used], LUAssetsByRevA[Used],pch=".")
plot(SubsectorRankA[Used], fit4$frame$yval[fit4$where],pch=".")

# Now save the good plots

plotfile <- paste(CurrentDirectory,"/PreliminaryPlots%03d.pdf",sep="")
pdf(file=plotfile, height = 4, width = 6,onefile=FALSE)

fit4 <- rpart(LUTotGHGbyRevA ~ LUAssetsByRevA + SubsectorRankA , minbucket = 5,cp=0.005,weights = Used)
main="GHG fit by Subsector and Assets"
y <- fit4$frame$yval
GoodUsed <- !is.na(LUTotGHGbyRev)
Y <- LUTotGHGbyRevA[Used][GoodUsed]

x1 <- SubsectorRankA[Used][GoodUsed]
x2 <- LUAssetsByRevA[Used][GoodUsed]
x1name <- "Subsector"
x2name <- "Assets"
eps1 <- 0.01 * (max(x1,na.rm = TRUE) - min(x1,na.rm=TRUE))
eps2 <- 0.01 * (max(x2,na.rm = TRUE) - min(x2,na.rm=TRUE))
loc <- fit4$where
uniloc <-unique(loc)
col = terrain.colors(floor(length(y) * 1.25))

par(mfrow=c(1,1))

plot(x1,x2,cex=0.5,
    col=col[rank(y,ties.method="first")[loc]],
    main= main,
    xlab= x1name,
    ylab= x2name)
for( i in 1:length(uniloc)){
    these <-  (loc == uniloc[i])
    #if(sum(these,na.rm=TRUE) > 0){
    if(length(these) > 0){
        mx1<- min(x1[these],na.rm=TRUE) - eps1
        Mx1 <- max(x1[these],na.rm=TRUE) + eps1
        mx2<- min(x2[these],na.rm=TRUE) - eps2
        Mx2 <- max(x2[these],na.rm=TRUE) + eps2
        lines(c(mx1,mx1,Mx1,Mx1,mx1), c(mx2,Mx2,Mx2,mx2,mx2),
        col=col[rank(y,ties.method="first")[uniloc[i]]])
        print(c(i, uniloc[i],mx1,Mx1,mx2,Mx2))
    }
}

fit5 <- rpart(LUTotGHGbyRevA ~ LUEmpByRevA + LUAssetsByRevA + LUIntangByRevA + LUTotPowByRevA + SubsectorRankA + CountryRankA, minbucket = 5,cp=0.000001,weights = Used)
y <- fit5$frame$yval
main="GHG fit by Many variables"
GoodUsed <- !is.na(LUTotGHGbyRev)
Y <- LUTotGHGbyRevA[Used][GoodUsed]
x1 <- SubsectorRankA[Used][GoodUsed]
x2 <- LUAssetsByRevA[Used][GoodUsed]
x1name <- "Subsector"
x2name <- "Assets"
eps1 <- 0.01 * (max(x1,na.rm = TRUE) - min(x1,na.rm=TRUE))
eps2 <- 0.01 * (max(x2,na.rm = TRUE) - min(x2,na.rm=TRUE))
loc <- fit5$where
uniloc <-unique(loc)
col = terrain.colors(floor(length(y) * 1.25))

par(mfrow=c(1,1))

plot( y[loc],Y,cex=0.5,
    main=main,
    xlab="Fitted Values",
    ylab="GHG",
    #col=terrain.colors(floor(length(Y) * 1.25))[rank(Y,ties.method="first")]
    col=col[rank(y,ties.method="first")[loc]]
)

plot(x1,x2,cex=0.5,
col=col[rank(y,ties.method="first")[loc]],
main= main,
xlab= x1name,
ylab= x2name)
for( i in 1:length(uniloc)){
    these <-  (loc == uniloc[i])
    #if(sum(these,na.rm=TRUE) > 0){
    if(length(these) > 0){
        mx1<- min(x1[these],na.rm=TRUE) - eps1
        Mx1 <- max(x1[these],na.rm=TRUE) + eps1
        mx2<- min(x2[these],na.rm=TRUE) - eps2
        Mx2 <- max(x2[these],na.rm=TRUE) + eps2
        lines(c(mx1,mx1,Mx1,Mx1,mx1), c(mx2,Mx2,Mx2,mx2,mx2),
        col=col[rank(y,ties.method="first")[uniloc[i]]])
        print(c(i, uniloc[i],mx1,Mx1,mx2,Mx2))
    }
}
dev.off()






























