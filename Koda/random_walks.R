library(rgl)
library(scatterplot3d)
library(ggplot2)
library(threed)
library(animation)
library(plotly)
library(htmlwidgets)
library(igraph)
library(dplyr)
library(ImageMagick)

# za simetricen
sim_sprehod <- function(p, spodnja_meja, zgornja_meja){
  vektor_vrednosti <- c(0)
  vsota <- 0
  spodaj <- 0 
  zgoraj <- 0
  while (vsota > spodnja_meja & vsota < zgornja_meja){
    c <- c(-1,1)
    a <- sample(c, 1)
    vsota <- vsota + a
    vektor_vrednosti <- c(vektor_vrednosti, vsota)
    if (a == -1){
      spodaj <- spodaj + a
    } else {
      zgoraj <- zgoraj + a
    }
  }
  vrednosti <- list(vektor_vrednosti, spodaj, zgoraj)
  return(vrednosti)
}

## 1 sprehod ## 
vektor_vrednosti <- c(0)
vsota <- 0
p = 0.50
spodnja_meja <- -5
zgornja_meja <- 5
set.seed(15)
seznam1 <- replicate(1, sim_sprehod(p, spodnja_meja, zgornja_meja))
max_length <- max(sapply(seznam1, length))

plot(c(1, max_length), c(spodnja_meja, zgornja_meja), type = "n",
     main="Random walk", xlab="cas oz. stevilo korakov", ylab="vrednost")
for (i in 1:1) {
  lines(1:length(seznam1[[i]]), seznam1[[i]])
}
abline(h = 0, lty = "dashed")
abline(h = spodnja_meja, lwd = 2)
abline(h = zgornja_meja, lwd = 2)
seznam1[[1]]
## 1 sprehod ##

prestej <- function(seznam_ne_sim, spodnja_meja_ne_sim, zgornja_meja_ne_sim){
  # je šel zgoraj in kok korakov je rabu
  zgoraj <- c()
  # je šel spodaj in kok korakov je rabu
  spodaj <- c()
  for (i in seq(1, length(seznam_ne_sim), by = 3)){
    pot <- seznam_ne_sim[[i]]
    pot
    stevilo_korakov <- abs(seznam_ne_sim[[i+1]])+ abs(seznam_ne_sim[[i+2]])
    stevilo_korakov
    if (pot[length(pot)]==zgornja_meja_ne_sim){
      zgoraj <- c(zgoraj,stevilo_korakov)
      zgoraj
    } else {
      spodaj <- c(spodaj,stevilo_korakov)
      spodaj
    }
  }
  vrednosti <- list(spodaj, zgoraj)
  return(vrednosti)
}

prestej_skupaj <- function(seznam_ne_sim, spodnja_meja_ne_sim, zgornja_meja_ne_sim){
  skupaj <- c()
  for (i in seq(1, length(seznam_ne_sim), by = 3)){
    pot <- seznam_ne_sim[[i]]
    pot
    stevilo_korakov <- abs(seznam_ne_sim[[i+1]])+ abs(seznam_ne_sim[[i+2]])
    stevilo_korakov
    skupaj <- c(skupaj,stevilo_korakov)
  }
  return(skupaj)
}

######### za barve #######################
colorPicker <- function(vsota, max_length,
                        ls_color = c(178, 34, 34), ll_color = c(255, 204, 0),
                        us_color = c(0, 0, 102), ul_color = c(102, 204, 225)) {
  l <- length(vsota)
  if (vsota[l] < 0) {
    rgb_values <- (ls_color + (ll_color - ls_color) * l / max_length) / 255
  } else {
    rgb_values <- (us_color + (ul_color - us_color) * l / max_length) / 255
  }
  rgb(rgb_values[1], rgb_values[2], rgb_values[3])
}
######### za barve #######################

################################## SIMETRIČEN SPREHOD #######################
vektor_vrednosti <- c(0)
vsota <- 0
p = 0.50
spodnja_meja <- -25
zgornja_meja <- 25
a <- sim_sprehod(p,spodnja_meja,zgornja_meja)
N <- 10000
set.seed(77)
seznam <- replicate(N, sim_sprehod(p, spodnja_meja, zgornja_meja))
max_length <- max(sapply(seznam, length))
# make plot
plot(c(1, max_length), c(spodnja_meja, zgornja_meja), type = "n",
     main="Random walks", xlab="cas oz. stevilo korakov", ylab="vrednost")
for (i in 1:N) {
  lines(1:length(seznam[[i]]), seznam[[i]],
        col = colorPicker(seznam[[i]], max_length), lwd = 0.5)
}
abline(h = 0, lty = "dashed")
abline(h = spodnja_meja, lwd = 2)
abline(h = zgornja_meja, lwd = 2)

#poglejmo si verjetnosti: 

verjetnost_da_prej_dosezemo_zgornjo_mejo <- function(seznam){
  stevilo_sprehodov <- length(seznam)/3
  koncal_zgoraj <- 0
  for (i in seq(2, length(seznam), by = 3)) {
      if (abs(seznam[[i]]) - seznam[[i+1]] < 0){
        koncal_zgoraj <- koncal_zgoraj + 1 
      }
  }
  iskana_verjetnost <- koncal_zgoraj / stevilo_sprehodov
  return(iskana_verjetnost)
}
verjetnost_da_prej_dosezemo_zgornjo_mejo(seznam)
#narišimo verjetnost 

# stevilo simulacij: 
x <- c(1:N)
x
y <- c()
y
for (i in seq(3, length(seznam), by = 3)) {
  b <- seznam[1:i]
  p <- verjetnost_da_prej_dosezemo_zgornjo_mejo(b)
  y <- c(y,p)
}
vrednosti_verjetnosti <- data.frame(x,y)
plot(x,y,
     type="l", lwd=1, xlab="Število simulacij", ylab="Verjetnost")
abline(h = abs(spodnja_meja)/(abs(spodnja_meja)+zgornja_meja), lty = "dashed")

stevilo_korakov_sim <- prestej(seznam, spodnja_meja, zgornja_meja)

ttt_sim <- table(stevilo_korakov_sim[[1]])
tttt_sim <- table(stevilo_korakov_sim[[2]])
View(ttt_sim)
View(tttt_sim)

# za histogram
zadet_spodaj <- stevilo_korakov_sim[[1]]
hist(zadet_spodaj,
     main="Število sprehodov, da so dosegli spodnjo mejo",
     xlab="Število korakov",breaks= 100)
zadet_zgoraj <- stevilo_korakov_sim[[2]]
hist(zadet_zgoraj,
     main="Število sprehodov, da so dosegli zgornjo mejo",
     xlab="Število korakov",breaks= 100)

stevilo_korakov_sim

skupaj <- prestej_skupaj(seznam, spodnja_meja, zgornja_meja)
hist(skupaj,
     main="Število sprehodov, da so dosegli zgornjo/spodnjo mejo",
     xlab="Število korakov",breaks= 100)

povprecje_korakov <- table(skupaj)
vrednosti <- as.numeric(names(povprecje_korakov))
dobi_frekvenco  <- function(povprecje_korakov) {
  frekvenca <- c()
  for(i in 1:length(povprecje_korakov)){
    frekvenca <- c(frekvenca,povprecje_korakov[[i]])
  }
  return(frekvenca)
}
frekvenca <- dobi_frekvenco(povprecje_korakov)

stevec <- sum(vrednosti * frekvenca)
imenovalec <- sum(frekvenca)
povprecno_stevilo_korakov <- stevec/imenovalec

################################## SIMETRIČEN SPREHOD #######################

########### pričakovano število korakov: simulacije #########################

################################## SIMETRIČEN SPREHOD #######################
vektor_vrednosti <- c(0)
vsota <- 0
p = 0.50
spodnja_meja <- -17
zgornja_meja <- 20
a <- sim_sprehod(p,spodnja_meja,zgornja_meja)
N <- 10000
seznam <- replicate(N, sim_sprehod(p, spodnja_meja, zgornja_meja))
stevilo_korakov_sim <- prestej(seznam, spodnja_meja, zgornja_meja)

skupaj <- prestej_skupaj(seznam, spodnja_meja, zgornja_meja)
povprecje_korakov <- table(skupaj)
vrednosti <- as.numeric(names(povprecje_korakov))
dobi_frekvenco  <- function(povprecje_korakov) {
  frekvenca <- c()
  for(i in 1:length(povprecje_korakov)){
    frekvenca <- c(frekvenca,povprecje_korakov[[i]])
  }
  return(frekvenca)
}
frekvenca <- dobi_frekvenco(povprecje_korakov)

stevec <- sum(vrednosti * frekvenca)
imenovalec <- sum(frekvenca)
povprecno_stevilo_korakov <- stevec/imenovalec
povprecno_stevilo_korakov

########### pričakovano število korakov: simulacije #########################
################################## NESIMETRIČEN SPREHOD #####################

nesim_sprehod <- function(p_gor, spodnja_meja, zgornja_meja){
  vektor_vrednosti <- c(0)
  vsota <- 0
  spodaj <- 0 
  zgoraj <- 0
  while (vsota > spodnja_meja & vsota < zgornja_meja){
    #  ifelse(test, yes, no)
    a <- ifelse(runif(1) < p_gor, 1, -1)
    vsota <- vsota + a
    vektor_vrednosti <- c(vektor_vrednosti, vsota)
    if (a == -1){
      spodaj <- spodaj + a
    } else {
      zgoraj <- zgoraj + a
    }
  }
  vrednosti <- list(vektor_vrednosti, spodaj, zgoraj)
  return(vrednosti)
}

vektor_vrednosti_ne_sim <- c(0)
vsota_ne_sim <- 0
p_gor = 0.75
spodnja_meja_ne_sim <- -4
zgornja_meja_ne_sim <- 36
a <- nesim_sprehod(p_gor,spodnja_meja_ne_sim,zgornja_meja_ne_sim)
N <- 10000
set.seed(53)
seznam_ne_sim <- replicate(N, nesim_sprehod(p_gor,spodnja_meja_ne_sim,zgornja_meja_ne_sim))
max_length <- max(sapply(seznam_ne_sim, length))
# make plot
plot(c(1, max_length), c(spodnja_meja_ne_sim, zgornja_meja_ne_sim), type = "n",
     main="Random walks", xlab="cas oz. stevilo korakov", ylab="vrednost")
for (i in 1:N) {
  lines(1:length(seznam_ne_sim[[i]]), seznam_ne_sim[[i]],
        col = colorPicker(seznam_ne_sim[[i]], max_length), lwd = 0.5)
}
abline(h = 0, lty = "dashed")
abline(h = spodnja_meja_ne_sim, lwd = 2)
abline(h = zgornja_meja_ne_sim, lwd = 2)

verjetnost_da_prej_dosezemo_zgornjo_mejo(seznam_ne_sim)
#narišimo verjetnost 


kolikokrat_dosezemo_zgornjo_mejo <- function(seznam_ne_sim, spodnja_meja_ne_sim, zgornja_meja_ne_sim){
  dosezena_zgornja <- 0
  for (i in seq(1, length(seznam_ne_sim), by = 3)){
    a <- length(seznam_ne_sim[[i]])
    b <- seznam_ne_sim[[i]]
    c <- b[a]
    if (c == zgornja_meja_ne_sim){
      dosezena_zgornja <- dosezena_zgornja +1
    }
  }
  return(dosezena_zgornja)
}

kolikokrat_dosezemo_zgornjo_mejo(seznam_ne_sim, spodnja_meja_ne_sim, zgornja_meja_ne_sim)


stevilo_korakov_ne_sim <- prestej(seznam_ne_sim, spodnja_meja_ne_sim, zgornja_meja_ne_sim)

ttt_ne_sim <- table(stevilo_korakov_ne_sim[[1]])
tttt_ne_sim <- table(stevilo_korakov_ne_sim[[2]])
View(ttt_ne_sim)
View(tttt_ne_sim)

# stevilo simulacij: 
x <- c(1:N)
x
y <- c()
y
for (i in seq(3, length(seznam_ne_sim), by = 3)) {
  b <- seznam_ne_sim[1:i]
  p <- verjetnost_da_prej_dosezemo_zgornjo_mejo(b)
  y <- c(y,p)
}
vrednosti_verjetnostiseznam_ne_sim <- data.frame(x,y)
plot(x,y,
     type="l", lwd=1, xlab="Število simulacij", ylab="Verjetnost")
abline(h = (1-((1-p_gor)/(p_gor))^(abs(spodnja_meja_ne_sim)))/(1-((1-p_gor)/(p_gor))^(abs(spodnja_meja_ne_sim)+zgornja_meja_ne_sim)), 
       lty = "dashed")

# tocno ta primer: 80/81 ~ 0.987654321 --> izkaže se, da formula deluje 

vektor_vrednosti <- c(0)
vsota <- 0
p_gor = 0.61
spodnja_meja_ne_sim <- -5
zgornja_meja_ne_sim <- 25
N <- 10000
seznam_ne_sim <- replicate(N, nesim_sprehod(p_gor,spodnja_meja_ne_sim,zgornja_meja_ne_sim))
stevilo_korakov_sim <- prestej(seznam_ne_sim, spodnja_meja_ne_sim, zgornja_meja_ne_sim)

skupaj <- prestej_skupaj(seznam_ne_sim, spodnja_meja_ne_sim, zgornja_meja_ne_sim)
povprecje_korakov <- table(skupaj)
vrednosti <- as.numeric(names(povprecje_korakov))
dobi_frekvenco  <- function(povprecje_korakov) {
  frekvenca <- c()
  for(i in 1:length(povprecje_korakov)){
    frekvenca <- c(frekvenca,povprecje_korakov[[i]])
  }
  return(frekvenca)
}
frekvenca <- dobi_frekvenco(povprecje_korakov)

stevec <- sum(vrednosti * frekvenca)
imenovalec <- sum(frekvenca)
povprecno_stevilo_korakov <- stevec/imenovalec
povprecno_stevilo_korakov





################################## NESIMETRIČEN SPREHOD #####################

################################## POVPREČNI ČAS ZADETKA #####################

povprecje <- function(seznam){
  stevilo_poti <- length(seznam)
  vsota <- 0
  for (i in seq(1, stevilo_poti, 3)){
    vsota = vsota + (length(seznam[[i]])-1)
  }
  e = vsota / length(seq(1, stevilo_poti, 3))
  return(e)
}
st_korakov <- povprecje(seznam)
st_korakov_ne_sim <- povprecje(seznam_ne_sim)

################################## POVPREČNI ČAS ZADETKA #####################

################################## SLUČAJNI SPREHOD V Z^2 ####################
sprehod_po_ravnini_simetricen <- function(stevilo_sprehodov, stevilo_korakov){
  require(ggplot2)
  polozaj <- matrix(ncol = 2)
  polozaj
  for (x in 1:stevilo_sprehodov) {
    x
    i <- 1
    sprehajalec <- matrix(c(0,0), nrow = stevilo_korakov+1, ncol = 2, byrow = T)
    sprehajalec
    while(i  <= stevilo_korakov){
      # odlocmo se najprej, a gremo levo/desno/gor/dol
      # 1 - levo 
      # 2 - desno
      # 3 - dol 
      # 4 - gor
      smeri <- c(1,2,3,4)
      kam <- sample(smeri, 1)
      kam # pobriši
      # ?e je 0 se spreminja x os:
      sprehajalec[i+1,1] <- sprehajalec[i,1] #treba je nastavit na enga vnaprej 
      sprehajalec[i+1,2] <- sprehajalec[i,2] #treba je nastavit na enga vnaprej
      #sprehajalec[i,3] <- x
      sprehajalec
      if (kam == 1){
        sprehajalec[i+1,1] <- sprehajalec[i,1] - 1
        i = i + 1
      } else if (kam == 2){
        sprehajalec[i+1,1] <- sprehajalec[i,1] + 1
        i = i + 1
      } else if (kam == 3){
        sprehajalec[i+1,2] <- sprehajalec[i,2] - 1
        i = i + 1
      } else if (kam == 4){
        sprehajalec[i+1,2] <- sprehajalec[i,2] + 1
        i = i + 1
      }
    }
    sprehajalec
    polozaj <- rbind(polozaj, sprehajalec)
    #polozaj[length(polozaj)/3,3] <- x
    polozaj
    x = x + 1
    x
  }
  v <- c(0)
  for (x in 1:stevilo_sprehodov){
    v <- c(v,rep(x,stevilo_korakov+1))
  }
  v <- v[2:length(v)]
  colnames(polozaj) <- c("x" , "y") # imena stolpcev 
  polozaj <- as.data.frame(polozaj)
  polozaj <- polozaj[2:nrow(polozaj),]
  polozaj <- cbind(polozaj,pot = factor(v))
  return(polozaj)
}
sprehod_v_ravnini <- sprehod_po_ravnini_simetricen(1, 100)
p <- ggplot(sprehod_v_ravnini, aes(x = x, y = y, colour = pot))
p <- p + geom_path()
p

ravnina_sprehod <- sprehod_po_ravnini_simetricen(1, 50)

require(animation)
ani.options(interval = .25)
saveGIF(
  {for(i in seq(2,25,1)){
    gp <- ggplot(ravnina_sprehod, aes(x=x, y=y, col = pot)) + geom_path(alpha = .5)
    gp <- gp + geom_point(x=ravnina_sprehod[i,1], y=ravnina_sprehod[i,2], col = "red")
    print(gp)
  }},
  movie.name = "ravnina_sprehod.gif", interval = .25, nmax =25, ani.width = 600, ani.height = 600,
  outdir = getwd()
)

ravnina_sprehod <- sprehod_po_ravnini_simetricen(10, 1000)
prvi_kvadrat <- ravnina_sprehod
ravnina_sprehod <- sprehod_po_ravnini_simetricen(10, 1000)
drugi_kvadrat <- ravnina_sprehod
ravnina_sprehod <- sprehod_po_ravnini_simetricen(10, 1000)
tretji_kvadrat <- ravnina_sprehod
ravnina_sprehod <- sprehod_po_ravnini_simetricen(10, 1000)
cetrti_kvadrat <- ravnina_sprehod

prvi_kvadrat <- cbind(prvi_kvadrat, group = factor("gr1"))
drugi_kvadrat <- cbind(drugi_kvadrat, group = factor("gr2"))
tretji_kvadrat <- cbind(tretji_kvadrat, group = factor("gr3"))
cetrti_kvadrat <- cbind(cetrti_kvadrat, group = factor("gr4"))

stirka <- rbind(prvi_kvadrat, drugi_kvadrat, tretji_kvadrat, cetrti_kvadrat)

ggplot(stirka, aes(x=x, y=y, col = pot)) + geom_path() +facet_wrap(facets = ~group, nrow = 2, ncol = 2)

#sprehod v R^3 
sprehod_v_prostoru_simetricen <- function(stevilo_korakov,plot = TRUE){
  polozaj <- matrix(ncol = 3)
  polozaj
  i <- 1
  sprehajalec <- matrix(c(0,0,0), nrow = stevilo_korakov+1, ncol = 3, byrow = T)
  sprehajalec
  while(i  <= stevilo_korakov){
    # odlocmo se najprej, a gremo levo/desno/gor/dol
    # 1 - levo 
    # 2 - desno
    # 3 - dol 
    # 4 - gor
    # 5 - nazaj
    # 6 - naprej
    smeri <- c(1,2,3,4,5,6)
    kam <- sample(smeri, 1)
    kam # pobriši
    # ?e je 0 se spreminja x os:
    sprehajalec[i+1,1] <- sprehajalec[i,1] #treba je nastavit na enga vnaprej 
    sprehajalec[i+1,2] <- sprehajalec[i,2] #treba je nastavit na enga vnaprej
    sprehajalec[i+1,3] <- sprehajalec[i,3]
    #sprehajalec[i,3] <- x
    sprehajalec
    if (kam == 1){
      sprehajalec[i+1,1] <- sprehajalec[i,1] - 1
      i = i + 1
    } else if (kam == 2){
      sprehajalec[i+1,1] <- sprehajalec[i,1] + 1
      i = i + 1
    } else if (kam == 3){
      sprehajalec[i+1,2] <- sprehajalec[i,2] - 1
      i = i + 1
    } else if (kam == 4){
      sprehajalec[i+1,2] <- sprehajalec[i,2] + 1
      i = i + 1
    } else if (kam == 5){
      sprehajalec[i+1,3] <- sprehajalec[i,3] - 1
      i = i + 1
    } else if (kam == 6){
      sprehajalec[i+1,3] <- sprehajalec[i,3] + 1
      i = i + 1
    }
  }
  sprehajalec
  polozaj <- rbind(polozaj, sprehajalec)
  #polozaj[length(polozaj)/3,3] <- x
  polozaj
  v <- c(0)
  v <- c(v,rep(1,stevilo_korakov+1))
  v <- v[2:length(v)]
  colnames(polozaj) <- c("x" , "y", "z") # imena stolpcev 
  polozaj <- as.data.frame(polozaj)
  polozaj <- polozaj[2:nrow(polozaj),]
  polozaj <- cbind(polozaj,pot = factor(v))
  return(polozaj)
}

prostor_sprehod <- sprehod_v_prostoru_simetricen(100)
prostor_sprehod

fig = plot_ly(prostor_sprehod, x = ~x, y = ~y, z = ~z, type = 'scatter3d', mode = 'lines+markers',
              opacity = 2, line = list(width = 1, color = ~pot, colorscale = 'Viridis'),
              marker = list(size = 2, color = ~pot, colorscale = 'Viridis'))

fig
saveWidget(fig, "Slike_animacije/sprehod_v_prostoru.html")

####################################### kocka ################################

# začnemo v oglišču A
kocka <- graph(
  c("A", "B","B", "A", 
    "A", "D", "D", "A",
    "A", "E", "E", "A",
    "B", "F", "F", "B",
    "B", "C", "C", "B",
    "D", "C", "C", "D",
    "D", "H", "H", "D",
    "E", "F", "F", "E",
    "E", "H", "H", "E",
    "F", "G", "G", "F",
    "C", "G", "G", "C",
    "H", "G", "G", "H")
)
# začnemo v oglišču 1

kocka <- graph(
  c(1, 2,2, 1, 
    1, 4, 4, 1,
    1, 5, 5, 1,
    2, 6, 6, 2,
    2, 3, 3, 2,
    4, 3, 3, 4,
    4, 8, 8, 4,
    5, 6, 6, 5,
    5, 8, 8, 5,
    6, 7, 7, 6,
    3, 7, 7, 3,
    8, 7, 7, 8)
)
plot(kocka)


#pričakovan čas vrnitve v začetno oglišče
sprehod_v_kocki <- function(){
  pot <- c(1)
  sosed <- c(2,4,5)
  kam <- sample(sosed, 1)
  pot <- c(pot,kam)
  pot
  while (kam != 1) {
    if (kam == 2){
      sosedi <- c(1,3,6)
      kam <- sample(sosedi, 1)
      kam
      pot <- c(pot,kam)
    } else if (kam == 3){
      sosedi <- c(2,4,7)
      kam <- sample(sosedi, 1)
      kam
      pot <- c(pot,kam)
    } else if (kam == 4){
      sosedi <- c(1,3,8)
      kam <- sample(sosedi, 1)
      kam
      pot <- c(pot,kam)
    } else if (kam == 5){
      sosedi <- c(1,6,8)
      kam <- sample(sosedi, 1)
      kam
      pot <- c(pot,kam)
    } else if (kam == 6){
      sosedi <- c(2,5,7)
      kam <- sample(sosedi, 1)
      kam
      pot <- c(pot,kam)
    } else if (kam == 7){
      sosedi <- c(3,6,8)
      kam <- sample(sosedi, 1)
      kam
      pot <- c(pot,kam)
    } else {
      sosedi <- c(4,5,7)
      kam <- sample(sosedi, 1)
      kam
      pot <- c(pot,kam)
    }
  }
  return(pot)
}

povpecen_cas_vrnitve_kocka <- function(st_simulacij){
  vsota <- 0 
  for (i in 1:st_simulacij){
    vsota <- vsota + (length(sprehod_v_kocki())-1)
  }
  rezultat <- vsota / st_simulacij
  return(rezultat)
}

# povprečen čas obiska vseh oglišč

obisci_vse_kocka <- function(){
  pot <- c(1)
  sosed <- c(2,4,5)
  kam <- sample(sosed, 1)
  pot <- c(pot,kam)
  while (length(rle(sort(pot))$values) < 8) {
    if (kam == 2){
      sosedi <- c(1,3,6)
      kam <- sample(sosedi, 1)
      pot <- c(pot,kam)
    } else if (kam == 3){
      sosedi <- c(2,4,7)
      kam <- sample(sosedi, 1)
      pot <- c(pot,kam)
    } else if (kam == 4){
      sosedi <- c(1,3,8)
      kam <- sample(sosedi, 1)
      pot <- c(pot,kam)
    } else if (kam == 5){
      sosedi <- c(1,6,8)
      kam <- sample(sosedi, 1)
      pot <- c(pot,kam)
    } else if (kam == 6){
      sosedi <- c(2,5,7)
      kam <- sample(sosedi, 1)
      pot <- c(pot,kam)
    } else if (kam == 7){
      sosedi <- c(3,6,8)
      kam <- sample(sosedi, 1)
      pot <- c(pot,kam)
    } else {
      sosedi <- c(4,5,7)
      kam <- sample(sosedi, 1)
      pot <- c(pot,kam)
    }
  }
  return(pot)
}

povpecen_cas_obiska_vseh_kocka <- function(st_simulacij){
  vsota <- 0 
  for (i in 1:st_simulacij){
    vsota <- vsota + (length(obisci_vse_kocka())-1)
  }
  rezultat <- vsota / st_simulacij
  return(rezultat)
}


## teserakt objekt z 16 ogli��i in 32 robovi 

teserarkt <- graph(
  c(1, 2, 2, 1, 
    1, 4, 4, 1,
    1, 5, 5, 1,
    1, 13, 13, 1,
    2, 3, 3, 2, 
    2, 6, 6, 2, 
    2, 14, 14, 2, 
    3, 4, 4, 3, 
    3, 7, 7, 3, 
    3, 15, 15, 3, 
    4, 8, 8, 4,
    4, 16, 16, 4,
    5, 6, 6, 5, 
    5, 8, 8, 5, 
    5, 9, 9, 5,
    6, 7, 7, 6,
    6, 10, 10, 6, 
    7, 8, 8, 7,
    8, 12, 12, 8,
    9, 10, 10, 9,
    9, 12, 12, 9,
    9, 13, 13, 9,
    10, 11, 11, 10, 
    10, 14, 14, 10, 
    11, 12, 12, 11, 
    11, 15, 15, 11,
    12, 16, 16, 12,
    13, 14, 14, 13,
    13, 16, 16, 13, 
    14, 15, 15, 14,
    15, 16, 16, 15
  )
)
plot(teserarkt)

sprehod_v_teserarkt <- function(){
  pot <- c(1)
  sosed <- c(2,4,5,13)
  kam <- sample(sosed, 1)
  pot <- c(pot,kam)
  pot
  while (kam != 1) {
    if (kam == 2){
      sosedi <- c(1,3,6,14)
      kam <- sample(sosedi, 1)
      kam
      pot <- c(pot,kam)
    } else if (kam == 3){
      sosedi <- c(2,4,7,15)
      kam <- sample(sosedi, 1)
      kam
      pot <- c(pot,kam)
    } else if (kam == 4){
      sosedi <- c(1,3,8,16)
      kam <- sample(sosedi, 1)
      kam
      pot <- c(pot,kam)
    } else if (kam == 5){
      sosedi <- c(1,6,8,9)
      kam <- sample(sosedi, 1)
      kam
      pot <- c(pot,kam)
    } else if (kam == 6){
      sosedi <- c(2,5,7,10)
      kam <- sample(sosedi, 1)
      kam
      pot <- c(pot,kam)
    } else if (kam == 7){
      sosedi <- c(3,6,8,11)
      kam <- sample(sosedi, 1)
      kam
      pot <- c(pot,kam)
    } else if (kam == 8){
      sosedi <- c(4,5,7,12)
      kam <- sample(sosedi, 1)
      kam
      pot <- c(pot,kam)
    } else if (kam == 9){
      sosedi <- c(5,10,12,13)
      kam <- sample(sosedi, 1)
      kam
      pot <- c(pot,kam)
    } else if (kam == 10){
      sosedi <- c(6,9,11,14)
      kam <- sample(sosedi, 1)
      kam
      pot <- c(pot,kam)
    } else if (kam == 11){
      sosedi <- c(7,10,12,15)
      kam <- sample(sosedi, 1)
      kam
      pot <- c(pot,kam)
    } else if (kam == 12){
      sosedi <- c(8,9,11,16)
      kam <- sample(sosedi, 1)
      kam
      pot <- c(pot,kam)
    } else if (kam == 13){
      sosedi <- c(1,9,14,16)
      kam <- sample(sosedi, 1)
      kam
      pot <- c(pot,kam)
    } else if (kam == 14){
      sosedi <- c(2,10,13,15)
      kam <- sample(sosedi, 1)
      kam
      pot <- c(pot,kam)
    } else if (kam == 15){
      sosedi <- c(3,11,14,16)
      kam <- sample(sosedi, 1)
      kam
      pot <- c(pot,kam)
    } else {
      sosedi <- c(4,12,13,15)
      kam <- sample(sosedi, 1)
      kam
      pot <- c(pot,kam)
    }
  }
  return(pot)
}

povpecen_cas_vrnitve_tesarakt <- function(st_simulacij){
  vsota <- 0 
  for (i in 1:st_simulacij){
    vsota <- vsota + (length(sprehod_v_teserarkt())-1)
  }
  rezultat <- vsota / st_simulacij
  return(rezultat)
}

## povpre�en �as obiska vseh ogli�� 

obisci_vse_teserarkt <- function(){
  pot <- c(1)
  sosed <- c(2,4,5)
  kam <- sample(sosed, 1)
  pot <- c(pot,kam)
  while (length(rle(sort(pot))$values) < 16) {
    if (kam == 2){
      sosedi <- c(1,3,6,14)
      kam <- sample(sosedi, 1)
      kam
      pot <- c(pot,kam)
    } else if (kam == 3){
      sosedi <- c(2,4,7,15)
      kam <- sample(sosedi, 1)
      kam
      pot <- c(pot,kam)
    } else if (kam == 4){
      sosedi <- c(1,3,8,16)
      kam <- sample(sosedi, 1)
      kam
      pot <- c(pot,kam)
    } else if (kam == 5){
      sosedi <- c(1,6,8,9)
      kam <- sample(sosedi, 1)
      kam
      pot <- c(pot,kam)
    } else if (kam == 6){
      sosedi <- c(2,5,7,10)
      kam <- sample(sosedi, 1)
      kam
      pot <- c(pot,kam)
    } else if (kam == 7){
      sosedi <- c(3,6,8,11)
      kam <- sample(sosedi, 1)
      kam
      pot <- c(pot,kam)
    } else if (kam == 8){
      sosedi <- c(4,5,7,12)
      kam <- sample(sosedi, 1)
      kam
      pot <- c(pot,kam)
    } else if (kam == 9){
      sosedi <- c(5,10,12,13)
      kam <- sample(sosedi, 1)
      kam
      pot <- c(pot,kam)
    } else if (kam == 10){
      sosedi <- c(6,9,11,14)
      kam <- sample(sosedi, 1)
      kam
      pot <- c(pot,kam)
    } else if (kam == 11){
      sosedi <- c(7,10,12,15)
      kam <- sample(sosedi, 1)
      kam
      pot <- c(pot,kam)
    } else if (kam == 12){
      sosedi <- c(8,9,11,16)
      kam <- sample(sosedi, 1)
      kam
      pot <- c(pot,kam)
    } else if (kam == 13){
      sosedi <- c(1,9,14,16)
      kam <- sample(sosedi, 1)
      kam
      pot <- c(pot,kam)
    } else if (kam == 14){
      sosedi <- c(2,10,13,15)
      kam <- sample(sosedi, 1)
      kam
      pot <- c(pot,kam)
    } else if (kam == 15){
      sosedi <- c(3,11,14,16)
      kam <- sample(sosedi, 1)
      kam
      pot <- c(pot,kam)
    } else {
      sosedi <- c(4,12,13,15)
      kam <- sample(sosedi, 1)
      kam
      pot <- c(pot,kam)
    }
  }
  return(pot)
}

povpecen_cas_obiska_vseh_teserarkt <- function(st_simulacij){
  vsota <- 0 
  for (i in 1:st_simulacij){
    vsota <- vsota + (length(obisci_vse_teserarkt())-1)
  }
  rezultat <- vsota / st_simulacij
  return(rezultat)
}

############ piramida ##################

########## 3 strana ####################
piramida3 <- graph(
  c(1, 2, 2, 1,
    1, 3, 3, 1,
    1, 4, 4, 1,
    2, 3, 3, 2,
    2, 4, 4, 2,
    3, 4, 4, 3
  )
)
plot(piramida3)


sprehod_v_piramida3 <- function(){
  pot <- c(1)
  sosed <- c(2,3,4)
  kam <- sample(sosed, 1)
  pot <- c(pot,kam)
  pot
  while (kam != 1) {
    if (kam == 2){
      sosedi <- c(1,3,4)
      kam <- sample(sosedi, 1)
      kam
      pot <- c(pot,kam)
    } else if (kam == 3){
      sosedi <- c(1,2,4)
      kam <- sample(sosedi, 1)
      kam
      pot <- c(pot,kam)
    }  else {
      sosedi <- c(1,2,3)
      kam <- sample(sosedi, 1)
      kam
      pot <- c(pot,kam)
    }
  }
  return(pot)
}

povpecen_cas_vrnitve_piramida3 <- function(st_simulacij){
  vsota <- 0 
  for (i in 1:st_simulacij){
    vsota <- vsota + (length(sprehod_v_piramida3())-1)
  }
  rezultat <- vsota / st_simulacij
  return(rezultat)
}

## povpre�en �as obiska vseh ogli�� 

obisci_vse_piramida3 <- function(){
  pot <- c(1)
  sosed <- c(2,3,4)
  kam <- sample(sosed, 1)
  pot <- c(pot,kam)
  while (length(rle(sort(pot))$values) < 4) {
    while (kam != 1) {
      if (kam == 2){
        sosedi <- c(1,3,4)
        kam <- sample(sosedi, 1)
        kam
        pot <- c(pot,kam)
      } else if (kam == 3){
        sosedi <- c(1,2,4)
        kam <- sample(sosedi, 1)
        kam
        pot <- c(pot,kam)
      }  else {
        sosedi <- c(1,2,3)
        kam <- sample(sosedi, 1)
        kam
        pot <- c(pot,kam)
      }
  }
  return(pot)
  }
}

povpecen_cas_obiska_vseh_piramida3<- function(st_simulacij){
  vsota <- 0 
  for (i in 1:st_simulacij){
    vsota <- vsota + (length(obisci_vse_piramida3())-1)
  }
  rezultat <- vsota / st_simulacij
  return(rezultat)
}
########## 3 strana ####################

########## 4 strana ####################

piramida4 <- graph(
  c(1, 2, 2, 1, 
    1, 4, 4, 1,
    1, 5, 5, 1,
    2, 3, 3, 2,
    2, 5, 5, 2,
    3, 4, 4, 3, 
    3, 5, 5, 3, 
    4, 5, 5, 4
    )
)
plot(piramida4)

sprehod_v_piramida4 <- function(){
  pot <- c(1)
  sosed <- c(2,4,5)
  kam <- sample(sosed, 1)
  pot <- c(pot,kam)
  pot
  while (kam != 1) {
    if (kam == 2){
      sosedi <- c(1,3,5)
      kam <- sample(sosedi, 1)
      kam
      pot <- c(pot,kam)
    } else if (kam == 3){
      sosedi <- c(2,4,5)
      kam <- sample(sosedi, 1)
      kam
      pot <- c(pot,kam)
    } 
    else if (kam == 4){
      sosedi <- c(1,3,5)
      kam <- sample(sosedi, 1)
      kam
      pot <- c(pot,kam)
    } else {
      sosedi <- c(1,2,3,4)
      kam <- sample(sosedi, 1)
      kam
      pot <- c(pot,kam)
    }
  }
  return(pot)
}

povpecen_cas_vrnitve_piramida4 <- function(st_simulacij){
  vsota <- 0 
  for (i in 1:st_simulacij){
    vsota <- vsota + (length(sprehod_v_piramida4())-1)
  }
  rezultat <- vsota / st_simulacij
  return(rezultat)
}

## povpre�en �as obiska vseh ogli�� 

obisci_vse_piramida4 <- function(){
  pot <- c(1)
  sosed <- c(2,4,5)
  kam <- sample(sosed, 1)
  pot <- c(pot,kam)
  while (length(rle(sort(pot))$values) < 5) {
    while (kam != 1) {
      if (kam == 2){
        sosedi <- c(1,3,5)
        kam <- sample(sosedi, 1)
        kam
        pot <- c(pot,kam)
      } else if (kam == 3){
        sosedi <- c(2,4,5)
        kam <- sample(sosedi, 1)
        kam
        pot <- c(pot,kam)
      } 
      else if (kam == 4){
        sosedi <- c(1,3,5)
        kam <- sample(sosedi, 1)
        kam
        pot <- c(pot,kam)
      } else {
        sosedi <- c(1,2,3,4)
        kam <- sample(sosedi, 1)
        kam
        pot <- c(pot,kam)
      }
    }
    return(pot)
  }
}

povpecen_cas_obiska_vseh_piramida4 <- function(st_simulacij){
  vsota <- 0 
  for (i in 1:st_simulacij){
    vsota <- vsota + (length(obisci_vse_piramida4())-1)
  }
  rezultat <- vsota / st_simulacij
  return(rezultat)
}


########## 4 strana ####################

