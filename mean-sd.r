library(readxl)
data <- read_excel("/rkdrive/ku-research/sleep-covid/draft/rk-coded-preprocessed-data-final.xlsx")

age <-data$Age[1:667]
mage <-mean(age)
rmage <-round(mage, digits = 1)
sdage <- sd(age)
rsdage <- round(sdage, digits = 1)
print(rmage)
print(rsdage)

f1 <- data$F1[1:667]
mf1 <- mean(f1)
rmf1 <- round(mf1, digits = 1)
sdf1 <- sd(f1)
rsdf1 <-round(sdf1, digits = 1)
print(rmf1)
print(rsdf1)

f2 <- data$F2[1:667]
mf2 <- mean(f2)
rmf2 <- round(mf2, digits = 1)
sdf2 <- sd(f2)
rsdf2 <-round(sdf2, digits = 1)
print(rmf2)
print(rsdf2)

psstot <- data$PSSSUM[1:667]
mpsstot <- mean(psstot)
rmpsstot <- round(mpsstot, digits = 1)
sdpsstot <- sd(psstot)
rsdpsstot <-round(sdpsstot, digits = 1)
print(rmpsstot)
print(rsdpsstot)

gadtot <- data$GADSUM[1:667]
mgadtot <- mean(gadtot)
rmgadtot <- round(mgadtot, digits = 1)
sdgadtot <- sd(gadtot)
rsdgadtot <- round(sdgadtot, digits = 1)
print(rmgadtot)
print(rsdgadtot)