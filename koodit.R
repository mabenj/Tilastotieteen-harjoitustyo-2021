library(foreign)
ht1.dat <- read.spss("elinolo2020.sav", to.data.frame = TRUE)
attach(ht1.dat)

set.seed(518467)
oma.otos1 <- ht1.dat[sample(nrow(ht1.dat), 700),]
attach(oma.otos1)

################# Varianssianalyysi ######################

library(dplyr)
miehet.dat <- select(filter(oma.otos1, supu=="mies"), c(supu, ahtas, pala))
naiset.dat <- select(filter(oma.otos1, supu=="nainen"), c(supu, ahtas, pala))

# Shapiro-Wilk -testit
with(miehet.dat, tapply(pala, list(ahtas), shapiro.test))
with(naiset.dat, tapply(pala, list(ahtas), shapiro.test))

# laattikko-jana-kuviot
attach (miehet.dat)
boxplot(pala~ahtas)
detach (miehet.dat)

attach (naiset.dat)
boxplot(pala~ahtas)
detach (naiset.dat)

# hajontojen yhtäsuuruustestaus
attach(oma.otos1)
library(car)
leveneTest(pala~supu*ahtas)

#Kaksisuuntainen varianssianalyysi
anova(lm(pala~supu*ahtas))




################# Regressiomalli ######################

# sirontakuvio, kotitalouden kuluttajayksiköiden lukumäärä
plot(rkyks, pala)
abline(lm(pala~rkyks))

# sirontakuvio, asumismenot
plot(asmenot, pala)
abline(lm(pala~asmenot))

# sirontakuvio, asumismenot
plot(alaika, pala)
abline(lm(pala~alaika))


# korrelaatiokertoimet
cor.test(rkyks, pala, method="pearson")
cor.test(rkyks, pala, method="spearman", exact=FALSE)
cor.test(asmenot, pala, method="pearson")
cor.test(asmenot, pala, method="spearman", exact=FALSE)
cor.test(alaika, pala, method="pearson")
cor.test(alaika, pala, method="spearman", exact=FALSE)


# kolmen selittäjän regressiomalli
lm.pala <- lm(pala~rkyks+asmenot+alaika)
summary(lm.pala)
