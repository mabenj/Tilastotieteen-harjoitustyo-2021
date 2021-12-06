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

# sirontakuvio, alueella asumisaika
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



################# Toistomittausmalli ######################

library(foreign)
ht2.dat<-read.spss("Toistomittausaineisto2020.sav", to.data.frame=TRUE)
attach(ht2.dat)

set.seed(518467)
oma.otos2<-ht2.dat[sample(nrow(ht2.dat), 600), ]
attach(oma.otos2)

# Shapiro wilkin testit
shapiro.test(Functional_M2)
shapiro.test(Functional_M1)

# luodaan sopivan muotoinen data
library(dplyr)
filtered_data <- na.omit(select(oma.otos2, patient, Functional_M1, Functional_M2, D2))
filtered_data <- with(filtered_data, filtered_data[order(patient), ])
filtered_data <- filtered_data[!duplicated(filtered_data$patient), ]

M1_patient <- list(select(filtered_data, patient))
M1_value <- list(select(filtered_data, Functional_M1))
M1_Sex <- list(select(filtered_data, D2))
M1 <- do.call(rbind.data.frame, Map('c', M1_patient, M1_value, M1_Sex))
M1['Functional'] = 'M1'
names(M1)[names(M1) == 'Functional_M1'] <- 'Functional_Value'

M2_patient <- list(select(filtered_data, patient))
M2_value <- list(select(filtered_data, Functional_M2))
M2_Sex <- list(select(filtered_data, D2))
M2 <- do.call(rbind.data.frame, Map('c', M2_patient, M2_value, M2_Sex))
M2['Functional'] = 'M2'
names(M2)[names(M2) == 'Functional_M2'] <- 'Functional_Value'

data <- rbind(M1, M2)

# Friedmanin testi
attach(data)
friedman.test(Functional_Value ~ Functional | patient, data=data)
detach(data)

# Wilcoxonin testi
attach(oma.otos2)
wilcox.test(Functional_M1, Functional_M2, paired = TRUE)


# sukupuolet erikseen

# normaalijakaumatestit
with(oma.otos2, tapply(Functional_M1, D2, shapiro.test))
with(oma.otos2, tapply(Functional_M2, D2, shapiro.test))

# toistettujen mittausten varianssianalyysi
attach(data)
summary(aov(Functional_Value ~ D2 * Functional + Error(patient / Functional), data=data))
detach(data)
