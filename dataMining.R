agrupamento = function (entrada, lags, populacao, nGerada, nS, nGrupos) { #nGrupos = cores
  distancias = lapply (populacao, function (x)
                                  distEuclidiana (x, populacao))
  posDist = order (sapply (distancias, sum))
  centroides = populacao[posDist[1:nGrupos]]
  
  populacaoTotal = completaPopulacao (entrada, lags, populacao, nS, nGerada)
  grupos = KNN (populacaoTotal, centroides)
  
  return (grupos)
}

distEuclidiana = function (individuo, populacao) {
  dist = sapply (populacao, function (x)
                             sqrt (sum ((x$individuo - individuo$individuo)^2)))
  return (dist)
}

KNN = function (populacao, centroides) {
  distancias = lapply (centroides, function (x)
                                  distEuclidiana (x, populacao))
  posDist = lapply (distancias, order)
  grupos = lapply (posDist, function (x) 
                            populacao[1:nPOPULACAO])
  
  return (grupos)
}