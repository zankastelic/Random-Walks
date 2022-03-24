library(rgl)
library(scatterplot3d)
library(ggplot2)
library(threed)
library(animation)

# za simetricen
sim_sprehod <- function(p, spodnja_meja, zgornja_meja) {
  vektor_vrednosti <- c(0)
  vsota <- 0
  while (vsota > spodnja_meja & vsota < zgornja_meja) {
    c <- c(-1,1)
    vsota <- vsota + sample(c, 1)
    vektor_vrednosti <- c(vektor_vrednosti, vsota)
  }
  vektor_vrednosti}

nesim_sprehod <- function(p, spodnja_meja, zgornja_meja) {
  vektor_vrednosti <- c(0)
  vsota <- 0
  while (vsota > spodnja_meja & vsota < zgornja_meja) {
    vsota <- vsota + ifelse(runif(1) < p, 1, -1)
    vektor_vrednosti <- c(vektor_vrednosti, vsota)
  }
  vektor_vrednosti}

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

vektor_vrednosti <- c(0)
vsota <- 0
p = 0.50
spodnja_meja <- -50
zgornja_meja <- 50
a <- sim_sprehod(p,spodnja_meja,zgornja_meja)
N <- 100
set.seed(77)
seznam <- replicate(N, sim_sprehod(p, spodnja_meja, zgornja_meja))
max_length <- max(sapply(seznam, length))
# make plot
plot(c(1, max_length), c(spodnja_meja, zgornja_meja), type = "n",
     main="Random walks", xlab="cas", ylab="vrednost")
for (i in 1:N) {
  lines(1:length(seznam[[i]]), seznam[[i]],
        col = colorPicker(seznam[[i]], max_length), lwd = 0.5)
}
abline(h = 0, lty = "dashed")
abline(h = spodnja_meja, lwd = 2)
abline(h = zgornja_meja, lwd = 2)

# povprecni_cas zadetka 
povprecje <- function(seznam){
  stevilo_poti <- length(seznam)
  vsota <- 0
  for (i in 1: stevilo_poti){
    vsota = vsota + length(seznam[[i]])
  }
  e = vsota / stevilo_poti
  e
}

# slucajni sprehod v R^2
sprehod_po_ravnini_simetricen <- function(stevilo_sprehodov, stevilo_korakov,plot = TRUE){
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
  if(plot){
    require(ggplot2)
    p <- ggplot(polozaj, aes(x = x, y = y, colour = pot))
    p <- p + geom_path()
    print(p)
  }
  return(polozaj)
}


ploskev_sprehod <- sprehod_po_ravnini_simetricen(4, 10000)

require(animation)
ani.options(interval = .25)
saveGIF(
  {for(i in seq(2,25,1)){
    gp <- ggplot(ploskev_sprehod, aes(x=x, y=y, col = pot)) + geom_path(alpha = .5)
    gp <- gp + geom_point(x=ploskev_sprehod[i,1], y=ploskev_sprehod[i,2], col = "red")
    print(gp)
  }},
  movie.name = "ploskev_sprehod.gif", interval = .25, nmax =25, ani.width = 600, ani.height = 600,
  outdir = getwd()
)

ploskev_sprehod <- sprehod_po_ravnini_simetricen(10, 1000)
prvi_kvadrat <- ploskev_sprehod
ploskev_sprehod <- sprehod_po_ravnini_simetricen(10, 1000)
drugi_kvadrat <- ploskev_sprehod
ploskev_sprehod <- sprehod_po_ravnini_simetricen(10, 1000)
tretji_kvadrat <- ploskev_sprehod
ploskev_sprehod <- sprehod_po_ravnini_simetricen(10, 1000)
cetrti_kvadrat <- ploskev_sprehod

prvi_kvadrat <- cbind(prvi_kvadrat, group = factor("gr1"))
drugi_kvadrat <- cbind(drugi_kvadrat, group = factor("gr2"))
tretji_kvadrat <- cbind(tretji_kvadrat, group = factor("gr3"))
cetrti_kvadrat <- cbind(cetrti_kvadrat, group = factor("gr4"))

stirka <- rbind(prvi_kvadrat, drugi_kvadrat, tretji_kvadrat, cetrti_kvadrat)

ggplot(stirka, aes(x=x, y=y, col = pot)) + geom_path() +facet_wrap(facets = ~group, nrow = 2, ncol = 2)

#sprehod v R^3 
sprehod_v_prostoru_simetricen <- function(stevilo_sprehodov, stevilo_korakov,plot = TRUE){
  require(ggplot2)
  polozaj <- matrix(ncol = 3)
  polozaj
  for (x in 1:stevilo_sprehodov) {
    x
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
    x = x + 1
    x
  }
  v <- c(0)
  for (x in 1:stevilo_sprehodov){
    v <- c(v,rep(x,stevilo_korakov+1))
  }
  v <- v[2:length(v)]
  colnames(polozaj) <- c("x" , "y", "z") # imena stolpcev 
  polozaj <- as.data.frame(polozaj)
  polozaj <- polozaj[2:nrow(polozaj),]
  polozaj <- cbind(polozaj,pot = factor(v))
  polozaj
  if(plot){
    require(ggplot2)
    p <- ggplot(polozaj, aes(x = x, y = y, z=z,colour = pot))
    p <- p + geom_path()
    print(p)
  }
  return(polozaj)
}




# kocka

