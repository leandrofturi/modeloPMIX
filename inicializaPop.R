source ('avaliacao.R')

geraIndividuo = function (entrada, lags, nS) {
  nINDIVIDUO = sum (lags) * 12
  individuoMIN = -1
  individuoMAX = 1
  individuo = runif (nINDIVIDUO, individuoMIN, individuoMAX)
  momentos = momentos (entrada, individuo, lags, nS)
  
  while (estouro (momentos)) {
    individuo = runif (nINDIVIDUO, individuoMIN, individuoMAX)
    momentos = momentos (entrada, individuo, lags, nS)
  }
  avaliacao = avaliacao (entrada, momentos)
  
  final = list (individuo = individuo, serie = momentos$serie, avaliacao = avaliacao)
  return (final)
}

geraPopulacao = function (entrada, lags, nS) {
  p = 1:nPOPULACAO
  
  populacao = list ()
  populacao = parLapply (cl, p, function (x)
                                geraIndividuo (entrada, lags, nS))
  
  return (populacao)
}

geraCruzamento = function (entrada, lags, populacao, nS, Pc, Pm) {
  p = 1:nPOPULACAO
  
  novaPopulacao = list ()
  novaPopulacao = parLapply (cl, p, function (x)
                                    cruzamentoBLX (entrada, lags, populacao, nS, Pc))
  
  novaPopulacao = parLapply (cl, p, function (x)
                                    mutacao (novaPopulacao[[x]], entrada, lags, novaPopulacao, nS, Pm))
  
  return (novaPopulacao)
}

cruzamentoBLX = function (entrada, lags, populacao, nS, Pc) {
  nINDIVIDUO = sum (lags) * 12
  
  ALFA = 0.5
  beta = runif (nINDIVIDUO, -ALFA, 1 + ALFA)
  homog = runif (nINDIVIDUO, 0, 1) <= Pc
  
  pais = torneio (3)
  pai1 = populacao[[pais[1]]]$individuo
  pai2 = populacao[[pais[2]]]$individuo
  
  filho = pai1 + homog*beta*(pai2 - pai1)
  momentos = momentos (entrada, filho, lags, nS)
  
  while (estouro (momentos)) {
    beta = runif (nINDIVIDUO, -ALFA, 1 + ALFA)
    homog = runif (nINDIVIDUO, 0, 1) >= Pc
    filho = pai1 + homog*beta*(pai2 - pai1)
    momentos = momentos (entrada, filho, lags, nS)
  }
  avaliacao = avaliacao (entrada, momentos)
  
  final = list (individuo = filho, serie = momentos$serie, avaliacao = avaliacao)
  return (final)
}

mutacao = function (individuo, entrada, lags, populacao, nS, Pm) {
  if ((runif (1, 0, 1)) < Pm)
    individuo = geraIndividuo (entrada, lags, nS)
  
  return (individuo)
}

torneio = function (nPossibilidades) {
  possiveis = sample (nPOPULACAO, nPossibilidades, replace = F)
  possiveis = sort (possiveis)
  possiveis = possiveis[-(3:nPossibilidades)]
  
  return (possiveis)
}