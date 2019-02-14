source ('entrada.R')
source ('inicializaPop.R')
source ('mecanismos.R')

tempoMAX = 10000

NSGA = function (dados, lags) {
  series = entrada (dados)
  pop = geraPopulacao (series, lags, T, NA)
  nINDIVIDUO = (sum (lags))*12
  populacaoTotal = matrix (numeric (0), ncol = nINDIVIDUO, nrow = 2*nPOPULACAO)
  avaliacaoTotal = list ()
  populacao = matrix (numeric (0), ncol = nINDIVIDUO, nrow = nPOPULACAO)
  avaliacao = list ()
  ciclo = T
  tempo = 0
  
  while ((ciclo) && (tempo <= tempoMAX)) {
    novaPop = geraPopulacao (series, lags, F, pop)
    populacaoTotal[(1:nPOPULACAO), ] = pop$populacao
    populacaoTotal[((nPOPULACAO+1):(2*nPOPULACAO)), ] = novaPop$populacao
    avaliacaoTotal[(1:nPOPULACAO)] = pop$avaliacao
    avaliacaoTotal[((nPOPULACAO+1):(2*nPOPULACAO))] = novaPop$avaliacao
    
    popTotal = list (populacao = populacaoTotal, avaliacao = avaliacaoTotal)
    popTotal = CCO (popTotal)
    
    populacao = popTotal$populacao[(1:nPOPULACAO), ]
    avaliacao = popTotal$avaliacao[1:nPOPULACAO]
    pop = list (populacao = populacao, avaliacao = avaliacao)
    tempo = tempo + 1
    print (paste ("ciclo", tempo))
    
    if ((MAPEdiferenca (populacao)) < 0.002)
      ciclo = F
  }
  
  somResFinal = sapply (pop$avaliacao, function(x) (x$somRes))
  pop$populacao = pop$populacao[order (somResFinal), ]
  pop$avaliacao = pop$avaliacao[order (somResFinal)]
  sink ("resultado.txt")
  print (pop)
  sink ()
  
  #tabelaserieS = data.frame (melhorSerie)
  #rownames(tabelaserieS) = c(1:10000)
  #colnames(tabelaserieS) = c("JANEIRO", "FEVEREIRO", "MARCO", "ABRIL", "MAIO", "JUNHO", "JULHO", "AGOSTO", "SETEMBRO", "OUTUBRO", "NOVEMBRO", "DEZEMBRO")
  #write.csv2(tabelaserieS, "Serie Sintetica.csv")
  
  return ( )
}

MAPEdiferenca = function (populacao) {
  individuo = populacao [(round (runif (1, 1, nPOPULACAO))), ]
  diferencas = abs (t ((t (populacao) - individuo) / individuo))
  MAPEdif = sum (diferencas) / length (populacao)
  if (max (is.na (MAPEdif) || is.nan (MAPEdif) || is.infinite (MAPEdif)))
    return (1)
  else
    return (MAPEdif)
}