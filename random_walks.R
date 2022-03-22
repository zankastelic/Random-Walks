library(rgl)


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

# kocka



