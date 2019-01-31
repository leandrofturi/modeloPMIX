crossoverBLX = function (pai1, pai2) {
  ALFA = 0.5
  beta = runif (nINDIVIDUO, -ALFA, 1+ALFA)
  homog = round (runif (nINDIVIDUO, 0, 1))
  
  filho = pai1 + homog*beta*(pai2 - pai1)
  return (filho)
}

mutacao = function (individuo) {
  novoIndividuo = runif (1, individuoMIN, individuoMAX)
  
  return (novoIndividuo)
}