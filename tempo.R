<<<<<<< HEAD
source ('entrada.R')
source ('inicializaPop.R')
source ('mecanismos.R')



NSGA = function (dados, lags) {
  serieHN = entrada (dados)$serieHN
  pop = geraPopulacao (serieHN, lags, T, NA)
  populacaoTotal = matrix (numeric (0), ncol = nINDIVIDUO, nrow = 2*nPOPULACAO)
  avaliacaoTotal = list ()
  populacao = matrix (numeric (0), ncol = nINDIVIDUO, nrow = nPOPULACAO)
  avaliacao = list ()
  
  for (tempo in 1:500) {
    novaPop = geraPopulacao (serieHN, lags, F, pop)
    populacaoTotal[(1:nPOPULACAO), ] = pop$populacao
    populacaoTotal[((nPOPULACAO+1):(2*nPOPULACAO)), ] = novaPop$populacao
    avaliacaoTotal[(1:nPOPULACAO)] = pop$avaliacao
    avaliacaoTotal[((nPOPULACAO+1):(2*nPOPULACAO))] = novaPop$avaliacao
    
    popTotal = list (populacao = populacaoTotal, avaliacao = avaliacaoTotal)
    popTotal = CCO (popTotal)
    
    populacao = popTotal$populacao[(1:nPOPULACAO), ]
    avaliacao = popTotal$avaliacao[1:nPOPULACAO]
    pop = list (populacao = populacao, avaliacao = avaliacao)
  }
  
  pop = CCO (pop)
  return (pop)
=======
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
>>>>>>> a15340e948bdac63077d5101b05d29f2e0be66c5
}