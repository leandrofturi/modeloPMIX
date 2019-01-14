source('cenarioSint.R')
source('inicializacao.R')

media = function (serie, individuo, lags) {
  parametros = cortaParametros (individuo)
  print(parametros)
  
  dpRes = residuos (serie, parametros, lags)$dpRes
  serieSint = serieSint (parametros, dpRes, lags, length(serie)/12)
  mMensal = apply (serieSint, 2, mean)
  
  return (mMensal)
}

desvioPadrao = function (serie, individuo, lags) {
  parametros = cortaParametros (individuo)
  print(parametros)
  
  dpRes = residuos (serie, parametros, lags)$dpRes
  serieSint = serieSint (parametros, dpRes, lags, length(serie)/12)
  dpMensal = apply (serieSint, 2, sd)
  
  return (dpMensal)
}