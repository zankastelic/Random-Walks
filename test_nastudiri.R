# test 
library(plotly)
library(htmlwidgets)

# https://plotly.com/r/3d-scatter-plots/

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

#fig1 <- plot_ly(prostor_sprehod, x = ~x, y = ~y, z = ~z, color = ~pot, colors = c('#BF382A'))
#fig1 <- fig1 %>% add_markers()
#fig1 <- fig1 %>% layout(scene = list(xaxis = list(title = 'x'),
                                   #yaxis = list(title = 'y'),
                                   #zaxis = list(title = 'z')))

fig = plot_ly(prostor_sprehod, x = ~x, y = ~y, z = ~z, type = 'scatter3d', mode = 'lines+markers',
              opacity = 2, line = list(width = 1, color = ~pot, colorscale = 'Viridis'),
              marker = list(size = 2, color = ~pot, colorscale = 'Viridis'))

fig
saveWidget(fig, "sprehod_v_prostoru.html")
