MESES = c ("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")

nPOPULACAO = 50

graficoFACs = function (serieH, p, lag) {
  series = leituraArquivos (p)
  inicializaGrafico (serieH, lag)
  cores = rainbow (nPOPULACAO)
  p = 1:length (p)
  lapply (p, function (x)
             graficoFAC (series[[x]], lag, cores[x]))
  graficoFAC (serieH, lag, 'black')
}

leituraArquivos = function (p) {
  nomes = sapply (p, function (x)
                     paste0 ("serie_", x, ".csv"))
  series = lapply (nomes, function (x)
                          read.csv (x, header = TRUE, sep = ";", dec = ","))
  series = lapply (series, function (x)
                           as.matrix (x[-1]))
  return (series)
}

inicializaGrafico = function (serieH, lag) {
  par (lwd = 1, col= 'black')
  plot (NA, main = paste ("Autocorrelacoes mensais - lag", lag), xlim = c (1,12), ylim = c (-1, 1), xlab = "", ylab = "", axes = F, type = "n")
  axis (1, 1:12, MESES)
  axis (2, -1:1, c (-1, 0, 1))
  par (lty = 1)
  lines (1:12, rep(0, 12))
  intConfianca = 1.96 / sqrt (length (serieH) / 12)
  par (lty = 2)
  lines (1:12, rep(intConfianca, 12))
  lines (1:12, rep(-intConfianca, 12))
  box ()
}

graficoFAC = function (serie, lag, cor) {
  fac = correlograma (serie, lag, F)[(lag + 1), ]
  par (col = cor, lty = 1)
  points (fac, type = "o", pch = 20)
}

################################################
correlograma = function(serie, lagMax, grafico) {
  
  mPer = apply(serie, 2, mean)
  dpPer = apply(serie, 2, sd)
  serie = t((t(serie) - mPer))
  serieV = as.vector(t(serie))
  n = length(serie)/12
  intConfianca = 1.96 / sqrt(n)
  fac = matrix(1, nrow = lagMax+1, ncol = 12)
  
  if (lagMax > n) {
    print("Lag maior que a serie!")
    return();
  }
  
  for(lag in 1:lagMax) {
    serieLag = serieV[-(1:lag)] 
    serieAux = serieV[1:((n*12)-lag)]
    product = c(rep(0, lag), serieLag * serieAux)           
    serie = matrix(product, ncol = 12, byrow = T)
    
    for(mes in 1:12) {
      meslag = (mes - lag - 1) %% 12 + 1
      fac[(lag+1), mes] = sum(serie[, mes]) / ((n-1) * dpPer[mes] * dpPer[meslag])
    }
  }
  
  if(grafico) {
    plot(1, type = "n", xlab = "Periodo", xaxt="n", ylab = "Lag", xlim = c(1, 25), ylim = c(0, lagMax))
    axis(side = 1, at=c(2,4,6,8,10,12,14,16,18,20,22,24), 
         labels=c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez"))
    for(mes in 1:12) {
      segments((mes*2), 0, (mes*2), lagMax)
      segments((mes*2) + intConfianca, 0, (mes*2) + intConfianca, lagMax, lty = 2)
      segments((mes*2) - intConfianca, 0, (mes*2) - intConfianca, lagMax, lty = 2)
      for(lag in 0:lagMax) {
        segments((mes*2), lag, ((mes*2) + fac[(lag+1), mes]), lag)
      }
    }
  }
  return(fac)
}

lagMensalSignificativo = function (serie) {
  intConfianca = 1.96 / sqrt (length (serie) / 12)
  lagMensal = 12
  while (lagMensal < (length (serie))) {
    facMensal = correlograma (serie, lagMensal, F)[-1, ]
    facMensal = facMensal >= intConfianca
    lagMensalMin = apply (facMensal, 2, sum)
    lagMensalMin = min (lagMensalMin)
    if (lagMensalMin == lagMensal)
      lagMensal = lagMensal + 12
    else
      return (lagMensalMin)
  }
}

