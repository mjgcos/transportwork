#Exercise to reassign all transport revenue to fuel taxes.
#2 scenarios, one in which Q is fixed, one where Q responds to P.
#Assumptions: 1. No cross-substitution from petrol to diesel or vice versa (shares stay fixed).
# 2. 

setwd("~/OECD/Decomposition")
oecd <- read.csv("oecd.csv")
#All values in domestic currency.
elast <- read.csv("elasticities lookup.csv")
comb <- merge(oecd, elast)

#Test A: Fuel taxes required in absence of demand effects.Simple test.
#Given fixed Q and P excluding tax, find required tax to satisfy total transport revenue.

TestA <- comb[,c(2,13,17:26)]
TestA$Country <- as.character(TestA$Country) 
TestA$PRev <- TestA$Transport.Revenue*TestA$Petrol.Share
TestA$DRev <- TestA$Transport.Revenue*TestA$Diesel.Share
TestA$PTax <- TestA$PRev / TestA$Petrol.Quantity
TestA$DTax <- TestA$DRev / TestA$Diesel.Quantity

TestA[1,2] - (TestA[1,5]*TestA[1,15]+TestA[1,10]*TestA[1,16])

print(c(TestA$Country, "tp", TestA$PTax,"td", TestA$DTax))

#Test B: Including Demand effects for simple (shares equal) model.
#Elasticites - need to look at effects on the extreme, plus cross elasticities.

TestB <- comb[,c(2,13,17:26)]

for(i in 1:nrow(TestB)){
  cy <- as.character(TestB[i,1]) #Country
  tp <- TestB[i,7] #Tax per unit of petrol
  td <- TestB[i,12] #Tax per unit of diesel
  qp <- TestB[i,5] #Quantity of petrol
  qd <- TestB[i,10] #Quantity of diesel
  tr <- TestB[i,2] #Total transport revenue (target)
  ep <- TestB[i,3] #Own-price elasticity of petrol demand
  ed <- TestB[i,8] #Own-price elasticity of diesel demand
  pp <- TestB[i,6] #Price of petrol (including tax)
  pd <- TestB[i,11] #Price of diesel (including tax)
  ps <- TestB[i,4] #Petrol share of fuel quantity
  ds <- 1 - ps #Diesel share of fuel quantity
  bpp <- pp - tp #Base price of petrol (excluding tax)
  bpd <- pd - td #Base price of diesel (excluding tax)
  d <- 1 #Difference from initial total revenue. Defined as 1 to start loop (target = 0, revenue neutrality)
  while(abs(d) > 0){
    tp <- (ps*tr)/qp #Tax per unit required to acount for petrol's share of tr 
    td <- (ds*tr)/qd #Tax per unit required to acount for diesel's share of tr
    ppo <- pp #Price of petrol with previous iteration's tax
    pdo <- pd #Price of diesel with previous iteration's tax
    pp <- bpp + tp #New price of petrol
    pd <- bpd + td #New price of diesel
    qp <- qp*(1 + ep*((pp-ppo)/ppo)) #New quantity of petrol
    qd <- qd*(1 + ed*((pd-pdo)/pdo)) #New quantity of diesel
    x <- qp*tp + qd*td #New total revenue
    d <- tr - x #Difference between new total revenue and target (loop ends when this reaches 0) 
  }
  print(c(cy, "tp", tp, "td", td)) #Output - Will look to place in a new dataframe.
}

#Test C: variable elasticities
#Given the large changes in price advocated by Test B, fixed elasticities not reasonable.
#Assume 3 reigimes. 1) Point elasticity measured in study for original price and below. Holds until p2 = 1.5*p1.
#2) 0.5*e for 1.5*p1 to 1.5*p2
#3) p3 > 1.5*p2, perfectly price inelastic (e=0)


TestC <- comb[,c(2,13,17:26)]

a <- 1.5 #Stage 1-2 dilineator (multiplied by bpp/bpd)
b <- 1.5 #Stage 2-3 delineator (interact with a)
c <- 0.5 #State (2) multiplier of ep/ed

for(i in 1:nrow(TestC)){
  cy <- as.character(TestC[i,1]) #Country
  tp <- TestC[i,7] #Tax per unit of petrol
  td <- TestC[i,12] #Tax per unit of diesel
  qp <- TestC[i,5] #Quantity of petrol
  qd <- TestC[i,10] #Quantity of diesel
  tr <- TestC[i,2] #Total transport revenue (target)
  epo <- TestC[i,3] #Own-price elasticity of petrol demand - original
  edo <- TestC[i,8] #Own-price elasticity of diesel demand - original
  ep <- 1
  ed <- 1
  pp <- TestC[i,6] #Price of petrol (including tax)
  pd <- TestC[i,11] #Price of diesel (including tax)
  ps <- TestC[i,4] #Petrol share of fuel quantity
  ds <- 1 - ps #Diesel share of fuel quantity
  bpp <- pp - tp #Base price of petrol (excluding tax)
  bpd <- pd - td #Base price of diesel (excluding tax)
  d <- 1 #Difference from initial total revenue. Defined as 1 to start loop (target = 0, revenue neutrality)
  while(abs(d) > 0){
    tp <- (ps*tr)/qp #Tax per unit required to acount for petrol's share of tr 
    td <- (ds*tr)/qd #Tax per unit required to acount for diesel's share of tr
    ppo <- pp #Price of petrol with previous iteration's tax
    pdo <- pd #Price of diesel with previous iteration's tax
    pp <- bpp + tp #New price of petrol
    pd <- bpd + td #New price of diesel
    ep <- ifelse(pp > a*bpp, ifelse(pp > a*b*bpp, 0, c*epo), epo)
    ed <- ifelse(pd > a*bpd, ifelse(pd > a*b*bpd, 0, c*edo), edo)
    qp <- qp*(1 + ep*((pp-ppo)/ppo)) #New quantity of petrol
    qd <- qd*(1 + ed*((pd-pdo)/pdo)) #New quantity of diesel
    x <- qp*tp + qd*td #New total revenue
    d <- tr - x #Difference between new total revenue and target (loop ends when this reaches 0) 
  }
  print(c(cy, "tp", tp, "td", td)) #Output - Will look to place in a new dataframe.
}
