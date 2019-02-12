source ('entrada.R')
source ('inicializaPop.R')
source ('mecanismos.R')
source ('cenarioSint.R')
source ('sumQuadRes.R')

ciclo = 0
tempoMAX = 10000

NSGA = function (dados, lags) {
  serieHN <<- entrada (dados)$serieHN
  nH <<- length (serieHN) / 12
  pop = geraPopulacao (serieHN, lags, T, NA)
  populacaoTotal = matrix (numeric (0), ncol = nINDIVIDUO, nrow = 2*nPOPULACAO)
  avaliacaoTotal = list ()
  populacao = matrix (numeric (0), ncol = nINDIVIDUO, nrow = nPOPULACAO)
  avaliacao = list ()
  
  for (tempo in 1:tempoMAX) {
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
    
    ciclo <<- tempo
    #rankAtual = FNS (pop)
    #if (max (rankAtual == 1)) {
      #print ("todos na mesma fronteira!")
      #tempo = tempoMAX + 1
    #}
  }
  pop = CCO (pop)
  sink ("resultado.txt") 
  print (pop)
  sink ()
  
  resultadosFinais <<- pop
  
  parametros = pop$populacao[1, ]
  dpRes = residuos (serieHN, parametros, lags)$dpRes
  melhorSerie <<- serieSint (parametros, dpRes, lags, nH)
  tabelaserieS = data.frame (melhorSerie)
  rownames(tabelaserieS) = c(1:nH)
  colnames(tabelaserieS) = c("JANEIRO", "FEVEREIRO", "MARCO", "ABRIL", "MAIO", "JUNHO", "JULHO", "AGOSTO", "SETEMBRO", "OUTUBRO", "NOVEMBRO", "DEZEMBRO")
  write.csv2(tabelaserieS, "Serie Sintetica.csv")
  
  pop$avaliacao = pop$avaliacao[1:100]
  medias = sum (sapply (pop$avaliacao, function(x) (x$media))) / 100
  desvios = sum (sapply (pop$avaliacao, function(x) (x$dp))) / 100
  facAnuais = sum (sapply (pop$avaliacao, function(x) (x$facAnual))) / 100
  facMensais = sum (sapply (pop$avaliacao, function(x) (x$facMensal))) / 100
  somRes = sum (sapply (pop$avaliacao, function(x) (x$somRes))) / 100
  
  sink ("medias100.txt")
  print (medias)
  print (desvios)
  print (facAnuais)
  print (facMensais)
  print (somRes)
  sink ()
  
  pop$avaliacao = pop$avaliacao[1:50]
  medias = sum (sapply (pop$avaliacao, function(x) (x$media))) / 50
  desvios = sum (sapply (pop$avaliacao, function(x) (x$dp))) / 50
  facAnuais = sum (sapply (pop$avaliacao, function(x) (x$facAnual))) / 50
  facMensais = sum (sapply (pop$avaliacao, function(x) (x$facMensal))) / 50
  somRes = sum (sapply (pop$avaliacao, function(x) (x$somRes))) / 50
  
  sink ("medias50.txt")
  print (medias)
  print (desvios)
  print (facAnuais)
  print (facMensais)
  print (somRes)
  sink ()
  
  pop$avaliacao = pop$avaliacao[1:20]
  medias = sum (sapply (pop$avaliacao, function(x) (x$media))) / 20
  desvios = sum (sapply (pop$avaliacao, function(x) (x$dp))) / 20
  facAnuais = sum (sapply (pop$avaliacao, function(x) (x$facAnual))) / 20
  facMensais = sum (sapply (pop$avaliacao, function(x) (x$facMensal))) / 20
  somRes = sum (sapply (pop$avaliacao, function(x) (x$somRes))) / 20
  
  sink ("medias20.txt")
  print (medias)
  print (desvios)
  print (facAnuais)
  print (facMensais)
  print (somRes)
  sink ()
  
  pop$avaliacao = pop$avaliacao[1:10]
  medias = sum (sapply (pop$avaliacao, function(x) (x$media))) / 10
  desvios = sum (sapply (pop$avaliacao, function(x) (x$dp))) / 10
  facAnuais = sum (sapply (pop$avaliacao, function(x) (x$facAnual))) / 10
  facMensais = sum (sapply (pop$avaliacao, function(x) (x$facMensal))) / 10
  somRes = sum (sapply (pop$avaliacao, function(x) (x$somRes))) / 10
  
  sink ("medias10.txt")
  print (medias)
  print (desvios)
  print (facAnuais)
  print (facMensais)
  print (somRes)
  sink ()
  
  return (pop)
}