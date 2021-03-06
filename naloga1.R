#######################################################################################

library(lattice)
library(plyr)
library(Rmisc)
library(gplots)
library(plotrix)
library(ggplot2)

#######################################################################################

setwd("~/Documents/FMF MAT dodiplomski/FMF 3. letnik/Statistika/Stat_projekt")

# Uvoz tabele s podatki
data <- read.table("kibergrad.csv", header = TRUE, sep = ",")

N <- nrow(data) # stevilo vseh podatkov
n <- 200 # velikost vzorca 

# Oznaka _V se nanasa na vzorec, oznaka _P pa na populacijo 

########################################## A ##########################################

# Enostavni slucajni vzorec 200 ljudi
index <- sample(1:nrow(data), n)
vzorec <- data[index,]

# Rabili bomo samo stolpec z izobrazbo

izobrazba_V <- vzorec$IZOBRAZBA

# Delez vodij gospodinjstva v vzorcu, ki nimajo srednjesolske izobrazbe -> ocena

delez_V <- length(izobrazba_V[izobrazba_V <= 38]) / n
delez_V

########################################## B ##########################################

# Ocena standardne napake glede na vzorec iz točke ### A ###

napaka_V <- sqrt((delez_V * (1 - delez_V) /  n) * (1 - (n - 1) / (N - 1)))
napaka_V

# Interval zaupanja glede na oceno napake -> 95% interval zaupanja -> alpha = 0.05

alpha = 0.05
koef = qnorm(1 - alpha / 2)
sp_meja <- delez_V - koef * napaka_V
zg_meja <- delez_V + koef * napaka_V

sp_meja
zg_meja

########################################## C ##########################################

# Vzorcni delez iz ### A ### in ocenjeno standardno napako iz ### B ### 
# primerjamo s populacijskim deležem in pravo standardno napako

# Delez vodij gospodinjstva v vzorcu iz ## A ##, ki nimajo srednjesolske izobrazbe

izobrazba_P <- data$IZOBRAZBA

delez_P <- length(izobrazba_P[izobrazba_P <= 38]) / N
delez_P

napaka_P <- sqrt((delez_P * (1 - delez_P) / n) * (1 - (n - 1) / (N - 1)))
napaka_P

# Razlika med P in V

abs(delez_P - delez_V)
abs(napaka_P - napaka_V)

########################################## D ##########################################

# Poleg enost sl vzorca iz ### A ###, vzamemo se 99 sl vzorcev po 200 ljudi.
# Ponovimo iste izracune kot zgoraj, le da za vseh 100 oz. 99 vzorcev:
  # Delez
  # Standardna napaka
  # Interval zaupanja kot spodnjo in zgornjo mejo

###

# Delez vodij gospodinjstva v vzorcu, ki nimajo srednjesolske izobrazbe za vseh 100 vzorcev 
delezi_200 <- delez_V
for (i in 2:100) {
  index_i <- sample(1:nrow(data), n)
  vzorec_i <- data[index_i,]
  izobrazba_i <- vzorec_i$IZOBRAZBA
  delez_i <- length(izobrazba_i[izobrazba_i <= 38]) / n 
  delezi_200 <- c(delezi_200, delez_i)
}

# Standardne napake za vseh 100 vzorcev
napaka_200 <- sqrt((delezi_200[1] * (1 - delezi_200[1]) /  n) * (1 - (n - 1) / (N - 1)))
for (i in 2:100) {
  napaka_i <- sqrt((delezi_200[i] * (1 - delezi_200[i]) /  n) * (1 - (n - 1) / (N - 1)))
  napaka_200 <- c(napaka_200, napaka_i)
}

# Spodnja meja za interval zaupanja
sp_meja_200 <- delezi_200[1] - koef * napaka_200[1] 
for (i in 2:100) {
  sp_i <- delezi_200[i] - koef * napaka_200[i]
  sp_meja_200 <- c(sp_meja_200, sp_i)
}

# Zgornja meja za interval zaupanja
zg_meja_200 <- delezi_200[1] + koef * napaka_200[1] 
for (i in 2:100) {
  zg_i <- delezi_200[i] + koef * napaka_200[i]
  zg_meja_200 <- c(zg_meja_200, zg_i)
}

###

# Zdruzimo podatke v tabelo
podatki_200 <- cbind(sp_meja_200, zg_meja_200)


# Koliko intervalov zaupanja pokrije populacijski delez: 
# popul delez mora biti torej vecji od spodnje meje in hkrati manjsi od zgornje.

pokritje_200 <- 0
for (i in 1:100) {
  if (podatki_200[,1][i] < delez_P && podatki_200[,2][i] > delez_P) {
    pokritje_200 <- pokritje_200 + 1
  }
}

pokritje_200

########################################## E ##########################################

# Standardni odklon in napaka za 100 vzorcev po 200 družin

povpr <- mean(delezi_200)

standardni_odklon_200 <- 0
for (i in 1:100) {
  standardni_odklon_200 <- standardni_odklon_200 + (delezi_200[i]  - povpr)^2
}
standardni_odklon_200 <- sqrt(standardni_odklon_200 / 100)

standardni_odklon_200
napaka_P

abs(standardni_odklon_200 - napaka_P)

########################################## F ##########################################

# 100 enostavnih slucajnih vzorcev po 800 ljudi
# Naredimo izračune:
  # Delez
  # Standardna napaka
  # Interval zaupanja kot spodnjo in zgornjo mejo

###

m <- 800 # velikost vzorca 

index_m <- sample(1:nrow(data), m)
vzorec_m <- data[index_m,]
izobrazba_m <- vzorec_m$IZOBRAZBA
delez_m <- length(izobrazba_m[izobrazba_m <= 38]) / m

# Izracunamo deleze
delezi_800 <- delez_m
for (i in 2:100){
  index_i <- sample(1:nrow(data), m)
  vzorec_i <- data[index_i,]
  izobrazba_i <- vzorec_i$IZOBRAZBA
  delez_i <- length(izobrazba_i[izobrazba_i <= 38]) / m 
  delezi_800 <- c(delezi_800, delez_i)
}

# Standardne napake za vseh 100 vzorcev
napaka_800 <- sqrt((delezi_800[1] * (1 - delezi_800[1]) /  n) * (1 - (n - 1) / (N - 1)))
for (i in 2:100){
  napaka_i <- sqrt((delezi_800[i] * (1 - delezi_800[i]) /  n) * (1 - (n - 1) / (N - 1)))
  napaka_800 <- c(napaka_800, napaka_i)
}

# Spodnja meja za interval zaupanja
sp_meja_800 <- delezi_800[1] - koef * napaka_800[1] 
for (i in 2:100){
  sp_i <- delezi_800[i] - koef * napaka_800[i]
  sp_meja_800 <- c(sp_meja_800, sp_i)
}

# Zgornja meja za interval zaupanja
zg_meja_800 <- delezi_800[1] + koef * napaka_800[1] 
for (i in 2:100){
  zg_i <- delezi_800[i] + koef * napaka_800[i]
  zg_meja_800 <- c(zg_meja_800, zg_i)
}

###

# Zdruzimo podatke v tabelo
podatki_800 <- cbind(sp_meja_800, zg_meja_800)

# Koliko intervalov zaupanja pokrije populacijski delez: 
# popul delez mora biti torej vecji od spodnje meje in hkrati manjsi od zgornje.

pokritje_800 <- 0
for (i in 1:100) {
  if (podatki_800[,1][i] < delez_P && podatki_800[,2][i] > delez_P) {
    pokritje_800 <- pokritje_800 + 1
  }
}

pokritje_800

###
###

# Standardni odklon in standardna napaka za 100 vzorcev po 800 družin

povpr1 <- mean(delezi_800)

standardni_odklon_800 <- 0
for (i in 1:100) {
  standardni_odklon_800 <- standardni_odklon_800 + (delezi_800[i] - povpr1)^2
}
standardni_odklon_800 <- sqrt(standardni_odklon_800 / 100)

standardni_odklon_800


napaka_m <- sqrt((delez_P * (1 - delez_P) / m) * (1 - (m - 1) / (N - 1)))
napaka_m

abs(standardni_odklon_800 - napaka_m)

#######################################################################################
