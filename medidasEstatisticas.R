medidas = function (serieH, p) {
  series = leituraArquivos (p)
  q = 1:length (p)
  
  saida = lapply (q, function (x)
                     escreve (serieH, series[[x]], p[x]))
}

escreve = function (serieH, serieS, ordem) {
  mediaMensalH = apply (serieH, 2, mean)
  mediaAnualH = mean (serieH)
  sdMensalH = apply (serieH, 2, sd)
  sdAnualH = sd (serieH)
  facMensalH = correlograma (serieH, 12, F)[-1, ]
  facAnualH = correlogramaAnual (serieH, 12)[-1]
  mediaMensalS = apply (serieS, 2, mean)
  mediaAnualS = mean (serieS)
  sdMensalS = apply (serieS, 2, sd)
  sdAnualS = sd (serieS)
  facMensalS = correlograma (serieS, 12, F)[-1, ]
  facAnualS = correlogramaAnual (serieS, 12)[-1]
  
  dados = c (mediaMensalH, mediaMensalS, sdMensalH, sdMensalS, facMensalH, facMensalS,
             rep (mediaAnualH, 12), rep (mediaAnualS, 12), rep (sdAnualH, 12), rep (sdAnualS, 12), facAnualH, facAnualS)
  dados = t (matrix (dados, ncol = 34))
  dados = data.frame (dados)
  rownames (dados) = c ("MediaH", "DesvioH", "MediaS", "DesvioS", 
                        "FACmensalH_lag1", "FACmensalH_lag2", "FACmensalH_lag3", "FACmensalH_lag4", "FACmensalH_lag5", "FACmensalH_lag6",
                        "FACmensalH_lag7", "FACmensalH_lag8", "FACmensalH_lag9", "FACmensalH_lag10", "FACmensalH_lag11", "FACmensalH_lag12",
                        "FACmensalS_lag1", "FACmensalS_lag2", "FACmensalS_lag3", "FACmensalS_lag4", "FACmensalS_lag5", "FACmensalS_lag6",
                        "FACmensalS_lag7", "FACmensalS_lag8", "FACmensalS_lag9", "FACmensalS_lag10", "FACmensalS_lag11", "FACmensalS_lag12",
                        "MediaAnualH", "DesvioAnualH", "MediaAnualS", "DesvioAnualS", "FACanualH", "FACanualS")
  colnames (dados) = c (1:12)
  write.csv2 (dados, paste0 ("Medidas_", ordem, ".csv"))
  
  return (T)
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

correlogramaAnual = function (serie, lagMax) {
  serieAnual = apply (serie, 1, sum)
  facAnual = acf (serieAnual, lag.max = lagMax, type = c("correlation"), plot = F, na.action = na.pass)
  facAnual = as.numeric (facAnual$acf)
  
  return (facAnual)
}

lagAnualSignificativo = function (serie) {
  intConfianca = 1.96 / sqrt (length (serie) / 12)
  lagAnual = 12
  while (lagAnual < (length (serie) / 12)) {
    facAnual = correlogramaAnual (serie, lagAnual)[-1]
    lagAnualMin = sum (facAnual >= intConfianca)
    if (lagAnualMin == lagAnual)
      lagAnual = lagAnual + 12
    else
      return (lagAnualMin)
  }
}

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
         labels= c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez"))
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