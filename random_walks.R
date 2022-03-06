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

vektor_vrednosti <- c(0)
vsota <- 0
p = 0.50
spodnja_meja <- -10
zgornja_meja <- 10
a <- sprehod(p,spodnja_meja,zgornja_meja)
N <- 10
seznam <- replicate(N, sim_sprehod(p, spodnja_meja, zgornja_meja))

max_length <- max(sapply(seznam, length))
# make plot
plot(c(1, max_length), c(spodnja_meja, zgornja_meja), type = "n",
     main="Random walks", xlab="cas", ylab="vrednost")
for (i in 1:N) {
  lines(1:length(seznam[[i]]), seznam[[i]])
}
abline(h = 0, lty = "dashed")
abline(h = spodnja_meja, lwd = 2)
abline(h = zgornja_meja, lwd = 2)
```
