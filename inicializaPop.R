source ('avaliacao.R')

nPOPULACAO = 50
nINDIVIDUO = 0
individuoMIN = -2
individuoMAX = 2

geraIndividuo = function (nINDIVIDUO) {
  individuo = runif (nINDIVIDUO, individuoMIN, individuoMAX)
  
  return (individuo)
}

geraPopulacao = function (serieHN, lags) {
  nINDIVIDUO <<- (sum (lags))*12
  populacao = matrix (numeric(1), ncol = nINDIVIDUO, nrow = nPOPULACAO)
  avaliacao = list ()

  p = 0
  print("Formando populacao...")
  
  while (p < nPOPULACAO) {
    novoIndividuo = geraIndividuo (nINDIVIDUO)
    
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

#pop$avalioacao[[1]]$media
#sapply(pop$avaliacao, function(x) (x$media)) pega todos e coloca numa matriz