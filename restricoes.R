source('cenarioSint.R')
source('sumQuadRes.R')
source('inicializacao.R')

inicializaPop = function (serie, lags) {
  populacao = matrix (numeric(1), ncol = 12*PRECISAO*sum(lags), nrow = TAM_POPULACAO)
  
  n = 1
  while (n <= TAM_POPULACAO) {
    individuo = round (runif (12*PRECISAO*sum(lags), 0, 1))
    
    parametros = cortaParametros (individuo)
    dpRes = residuos (serie, parametros, lags)$dpRes
    serieSint = serieSint (parametros, dpRes, lags, 10000)
    media = sum (apply (serieSint, 2, mean))
    desvioPadrao = sum (apply (serieSint, 2, sd))
    
    
    sumQuadRes = residuos (serie, parametros, lags)$somRes
    
    if (! ((is.nan (media)) || (is.nan (desvioPadrao)) || (is.infinite(media)) || (is.infinite(desvioPadrao)))) {
      if ((media > -0.7) && (media < 0.3) && (desvioPadrao > 7) && (desvioPadrao < 15) && (sumQuadRes < 400)) {
        print (n)
        populacao[n, ] = individuo
        n = n + 1
      }
      #else
        #print ("Descartando individuo...")
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