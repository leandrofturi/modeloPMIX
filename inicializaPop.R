source ('avaliacao.R')

nPOPULACAO = 5
nINDIVIDUO = 0
individuoMIN = -2
individuoMAX = 2

geraIndividuo = function (nINDIVIDUO) {
  individuo = runif (nINDIVIDUO, individuoMIN, individuoMAX)
  
  return (individuo)
}

geraPopulacao = function (serieHN, lags, inicio, pop) {
  nINDIVIDUO <<- (sum (lags))*12
  populacao = matrix (numeric(1), ncol = nINDIVIDUO, nrow = nPOPULACAO)
  avaliacao = list ()

  p = 0
  print("Formando populacao...")
  
  while (p < nPOPULACAO) {
    if (inicio)
      novoIndividuo = geraIndividuo (nINDIVIDUO)
    else {
      pais = selecao (pop)
      novoIndividuo = crossoverBLX (pop$populacao[pais[1]], pop$populacao[pais[2]])
      probMut = runif (1, 0, 1)
      if (probMut <= 0.05) {
        novoIndividuo = mutacao (novoIndividuo)
        print ("mutacao")
      }
    }
    
    if ((prod(novoIndividuo) <= 1) && (prod(novoIndividuo) >= -1)) { # TESTE DO PRODUTO
      momentos = momentos (serieHN, novoIndividuo, lags)
      
      if (!((is.nan (momentos$media)) || (is.nan (momentos$dp)) || (is.infinite (momentos$media)) || (is.infinite (momentos$dp)))) {
        p = p + 1
        populacao[p, ] = novoIndividuo
        av = avaliacao (serieHN, momentos)
        avaliacao[[p]] = av
        print (p)
      }
    }
  }
  final = list (populacao = populacao, avaliacao = avaliacao)
  return (final)
}

crossoverBLX = function (pai1, pai2) {
  ALFA = 0.5
  beta = runif (nINDIVIDUO, -ALFA, 1+ALFA)
  homog = round (runif (nINDIVIDUO, 0, 1))
  
  filho = pai1 + homog*beta*(pai2 - pai1)
  return (filho)
}

mutacao = function (individuo) {
  novoIndividuo = runif (nINDIVIDUO, individuoMIN, individuoMAX)
  
  return (novoIndividuo)
}

selecao = function (pop) {
  possiveis = sample ((1:nPOPULACAO), 2, replace = F)
  
  return (possiveis)
}

#pop$avalioacao[[1]]$media
#sapply(pop$avaliacao, function(x) (x$media)) pega todos e coloca numa matriz