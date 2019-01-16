source('cenarioSint.R')
source('sumQuadRes.R')
source('inicializacao.R')

testeIndividuo = function (individuo, serie, lags) {
  parametros = cortaParametros (individuo)
  dpRes = residuos (serie, parametros, lags)$dpRes
  serieSint = serieSint (parametros, dpRes, lags, 10000)
  media = sum (apply (serieSint, 2, mean))
  desvioPadrao = sum (apply (serieSint, 2, sd))
  
  sumQuadRes = residuos (serie, parametros, lags)$somRes
  
  if (! ((is.nan (media)) || (is.nan (desvioPadrao)) || (is.infinite(media)) || (is.infinite(desvioPadrao)))) {
    if (sumQuadRes < 600)
      return (TRUE)
    else
      return (FALSE)
  }
  else
    return (FALSE)
}

inicializaPop = function (serie, lags) {
  populacao = matrix (numeric(1), ncol = 12*PRECISAO*sum(lags), nrow = TAM_POPULACAO)
  
  n = 1
  while (n <= TAM_POPULACAO) {
    individuo = round (runif (12*PRECISAO*sum(lags), 0, 1))
    
    if (testeIndividuo (individuo, serie, lags)) {
      print (n)
      populacao[n, ] = individuo
      n = n + 1
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