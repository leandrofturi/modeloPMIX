crossoverBLX = function (pai1, pai2) {
  ALFA = 0.5
  beta = runif (nINDIVIDUO, -ALFA, 1+ALFA)
  homog = round (runif (nINDIVIDUO, 0, 1))
  
  filho = pai1 + homog*beta*(pai2 - pai1)
  return (filho)
}

mutacao = function (individuo) {
  r = runif (1, 0, 1)
  novoIndividuo = r*(individuoMAX - individuoMIN)
  
  return (novoIndividuo)
}

FNS = function (populacao, avaliacoes) {
  
}

CDA = function (individuos, avaliacoes) {
  
}