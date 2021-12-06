library(foreign)
ht3.dat<-read.spss("EK2011.sav", to.data.frame=TRUE)
attach(ht3.dat)

set.seed(518467)
oma.otos3<-ht3.dat[sample(nrow(ht3.dat), 900), ]
attach(oma.otos3)


############ Kategoristen vastemuuttujien mallitus #####################

# -------------- Muuttujien riippuvuusrakenne --------------------------

# frekvenssitaulut
table(d2)
table(d32)
table(k23)

# ristiintaulu
ftable(table(d2, d32, k23))

# loglineaarinen mallitus
library(MASS)
mytable <- xtabs(~ d2 + d32 + k23, data=oma.otos3)

# ehdollinen riippumattomuusmalli:
malli <- loglm(~ d2 + k23 + d32 + d2*k23+d32, mytable)
malli

# standardoidut jäännökset
stdres = residuals(malli, "pearson")
summary(stdres)
 
# mallin jatkotarkastelu
taulu1 <- table(d2, k23)
prop.table(taulu1, 1)
taulu2 <- table(d32, k23)
prop.table(taulu2, 1)

#loglineaarinen binäärinen regressio
tyottomyys <- glm(d32 ~ d1 + d2, data=oma.otos3, family=binomial)
summary(tyottomyys)


# Odds ratiot
exp(cbind(OR=coef(tyottomyys), confint(tyottomyys)))

# Negelgerke selitysaste
install.packages("fmsb")
library(fmsb)
data.nagel <- NagelkerkeR2(tyottomyys)
data.nagel
