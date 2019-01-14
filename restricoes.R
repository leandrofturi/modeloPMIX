source('cenarioSint.R')
source('inicializacao.R')

inicializaPop = function (serie, lags) {
  populacao = matrix (numeric(1), ncol = 12*PRECISAO, nrow = TAM_POPULACAO)
  
  n = 1
  while (n <= TAM_POPULACAO) {
    individuo = round (runif (12*PRECISAO, 0, 1))
    
    media = media (serie, individuo, lags)
    desvioPadrao = desvioPadrao (serie, individuo, lags)
    
    if (! ((is.nan (media)) || (is.nan (desvioPadrao)) || (is.infinite(media)) || (is.infinite(desvioPadrao)))) {
      if ((media > -0.6) && (media < 0.4) && (desvioPadrao > 4) && (desvioPadrao < 18)) {
        print (n)
        populacao[n, ] = individuo
        n = n + 1
      }
      else
        print ("Descartando individuo...")
    }
  }
  return (populacao)
}


media = function (serie, individuo, lags) {
  parametros = cortaParametros (individuo)
  
  dpRes = residuos (serie, parametros, lags)$dpRes
  serieSint = serieSint (parametros, dpRes, lags, 1000)
  mMensal = apply (serieSint, 2, mean)
  
  return (sum (mMensal))
}

desvioPadrao = function (serie, individuo, lags) {
  parametros = cortaParametros (individuo)
  
  dpRes = residuos (serie, parametros, lags)$dpRes
  serieSint = serieSint (parametros, dpRes, lags, 1000)
  dpMensal = apply (serieSint, 2, sd)
  
  return (sum (dpMensal))
}