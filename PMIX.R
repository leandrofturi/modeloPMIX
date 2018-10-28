PMIX = function(dados, parametros, nS){
  
  #TRATAMENTO DOS DADOS DE ENTRADA
  leitura = read.table(dados, header = T, sep = ";", dec = ",")
  leitura = leitura[, -1]
  serieH = matrix(leitura, ncol = 12, byrow = T)
  nH = length(serieH)/12
  mPerH = apply(serieH, 2, mean)
  dpPerH = apply(serieH, 2, sd)
  varPerH = apply(serieH, 2, var)
  
  serieHN = log(serieH)
  
  serieHN = t((t(serieHN) - apply(serieHN, 2, mean)) / apply(serieHN, 2, sd))
  
  facHN = correlograma(serieHN, n/4, F)
  
  return(serieHN)
}

correlograma = function(serie, lagMax, grafico){
  
  #CORRELOGRAMA DE UMA SERIE
  mPer = apply(serie, 2, mean)
  dpPer = apply(serie, 2, sd)
  serie = t((t(serie) - mPer))
  serieV = as.vector(t(serie))
  n = length(serie)/12
  intConfianca = 1.96 / sqrt(n)
  fac = matrix(1, nrow = lagMax+1, ncol = 12)
  
  if(lagMax > n){
    print("Lag maior que a serie!")
    return();
  }
  
  for(lag in 1:lagMax){
    serieLag = serieV[-(1:lag)] 
    serieAux = serieV[1:((n*12)-lag)]
    product = c(rep(0, lag), serieLag * serieAux)           
    serie = matrix(product, ncol = 12, byrow = T)
    
    for(mes in 1:12){
      meslag = (mes - lag - 1) %% 12 + 1
      fac[(lag+1), mes] = sum(serie[, mes]) / ((n-1) * dpPer[mes] * dpPer[meslag])
    }
  }
  
  if(grafico){
    plot(1, type = "n", xlab = "PERIODO", xaxt="n", ylab = "LAG", xlim = c(1, 25), ylim = c(0, lagMax))
    axis(side = 1, at=c(2,4,6,8,10,12,14,16,18,20,22,24), 
         labels=c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez"))
    for(mes in 1:12){
      segments((mes*2), 0, (mes*2), lagMax)
      segments((mes*2) + intConfianca, 0, (mes*2) + intConfianca, lagMax, lty = 2)
      segments((mes*2) - intConfianca, 0, (mes*2) - intConfianca, lagMax, lty = 2)
      for(lag in 0:lagMax){
        segments((mes*2), lag, ((mes*2) + fac[(lag+1), mes]), lag)
      }
    }
  }
  return(fac)
}