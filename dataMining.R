agrupamento = function (entrada, lags, populacao, nS, nGrupos) { #nGrupos = cores
  print ("Efetuando agrupamento...")
  populacaoTotal = completaPopulacao (entrada, lags, populacao, nS, (nGrupos*length (populacao)))
  distancias = lapply (populacaoTotal, function (x)
                                       distEuclidiana (x, populacaoTotal))
  grupos = KNN (populacaoTotal, distancias, length (populacao))
  
  return (grupos)
}

distEuclidiana = function (individuo, populacao) {
  dist = sapply (populacao, function (x)
                             sqrt (sum ((x$individuo - individuo$individuo)^2)))
  return (dist)
}

KNN = function (populacao, distancias, k) {
  proximos = lapply (distancias, function (x)
                                 order (x)[1:k])
  grupos = lapply (proximos, function (x)
                             populacao[x])
  return (grupos)
  
}