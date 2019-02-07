source ('entrada.R')
source ('inicializaPop.R')
source ('mecanismos.R')
source ('cenarioSint.R')
source ('sumQuadRes.R')

NSGA = function (dados, lags) {
  serieHN = entrada (dados)$serieHN
  pop = geraPopulacao (serieHN, lags, T, NA)
  populacaoTotal = matrix (numeric (0), ncol = nINDIVIDUO, nrow = 2*nPOPULACAO)
  avaliacaoTotal = list ()
  populacao = matrix (numeric (0), ncol = nINDIVIDUO, nrow = nPOPULACAO)
  avaliacao = list ()
  
  for (tempo in 1:1000) {
    novaPop = geraPopulacao (serieHN, lags, F, pop)
    populacaoTotal[(1:nPOPULACAO), ] = pop$populacao
    populacaoTotal[((nPOPULACAO+1):(2*nPOPULACAO)), ] = novaPop$populacao
    avaliacaoTotal[(1:nPOPULACAO)] = pop$avaliacao
    avaliacaoTotal[((nPOPULACAO+1):(2*nPOPULACAO))] = novaPop$avaliacao
    
    popTotal = list (populacao = populacaoTotal, avaliacao = avaliacaoTotal)
    popTotal = CCO (popTotal)
    
    ciclos = paste ("ciclo", tempo)
    print (ciclos)
    populacao = popTotal$populacao[(1:nPOPULACAO), ]
    avaliacao = popTotal$avaliacao[1:nPOPULACAO]
    pop = list (populacao = populacao, avaliacao = avaliacao)
  }
  pop = CCO (pop)
  sink ("resultado.txt") 
  print (pop)
  sink ()
  
  parametros = pop$populacao[1, ]
  dpRes = residuos (serieHN, parametros, lags)$dpRes
  melhorSerie = serieSint (parametros, dpRes, lags, 10000)
  
  tabelaserieS = data.frame(melhorSerie)
  rownames(tabelaserieS) = c(1:10000)
  colnames(tabelaserieS) = c("JANEIRO", "FEVEREIRO", "MARCO", "ABRIL", "MAIO", "JUNHO", "JULHO", "AGOSTO", "SETEMBRO", "OUTUBRO", "NOVEMBRO", "DEZEMBRO")
  write.csv2(tabelaserieS, "Serie Sintetica.csv")
  
  final = list (pop = pop, melhorSerie = melhorSerie)
  return (final)
}