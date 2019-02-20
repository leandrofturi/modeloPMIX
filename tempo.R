source ("entrada.R")
source ('inicializaPop.R')
source ('mecanismos.R')

#PARAMETROS ALGORITMO GENETICO
nPOPULACAO <<- 50
cicloMAX <<- 10000
MAPEdiferencaMAX <<- 0.002

#PARAMETROS FUNCAO OBJETIVO
nSINTETICA <<- 10000
probCRUZAMENTO <<- 0.8
probMUTACAO <<- 0.05

require ('parallel')
cores = detectCores () - 2
cl = makeCluster (cores)
clusterExport (cl, list ("geraIndividuo", "cruzamentoBLX", "mutacao", "torneio", "momentos", "avaliacao", "estouro",
                         "FNS", "dominanciaCompleta", "CDA", "distancia", "FNSfac", "dominanciaCompletaFac", "residuos",
                         "entrada", "correlograma", "correlogramaAnual", "lagAnualSignificativo", "lagMensalSignificativo", "serieSint",
                         "nPOPULACAO", "cicloMAX", "MAPEdiferencaMAX", "nSINTETICA", "probCRUZAMENTO", "probMUTACAO"))


NSGA = function (dados, lags) {
  entrada = entrada (dados)
  print ("Formando populacao inicial...")
  populacao = geraPopulacao (entrada, lags, nSINTETICA)
  populacao = CCO (populacao)
  diversidade = TRUE
  avaliacaoAutocorrelacao = FALSE
  ciclo <<- 0
  
  while ((ciclo < cicloMAX) && (diversidade)) {
    ciclo <<- ciclo + 1
    #if ((ciclo %% 1000) == 0) print (paste ("ciclo", ciclo))
    print (paste ("ciclo", ciclo))
    
    novaPopulalacao = geraCruzamento (entrada, lags, populacao, nSINTETICA, probCRUZAMENTO, probMUTACAO)
    populacaoTotal = c (populacao, novaPopulalacao)
    if (avaliacaoAutocorrelacao)
      populacaoTotal = CCOfac (populacaoTotal)
    else
        populacaoTotal = CCO (populacaoTotal)
    
    populacao = populacaoTotal[1:nPOPULACAO]
    rank = FNS (populacao)
    
    if (max (rank) == 0) {
      avaliacaoAutocorrelacao = TRUE
      if (MAPEdiferenca(populacao) <= MAPEdiferencaMAX)
        diversidade = FALSE
    }
  }
  
  diretorio = getwd ()
  
  estacao = substr (dados, start = 1, stop = (nchar (dados)-4))
  if (! (dir.exists (estacao)))
    dir.create (file.path (estacao))
  setwd (estacao)
  data = format (Sys.time (), "%F %Hh%M")
  ordem = paste0 ("PMIX(", lags[1], ",", lags[2], ",", lags[3], ",", lags[4], ") ", data)
  if (! (dir.exists (ordem)))
    dir.create (file.path (ordem))
  setwd (ordem)
  if (! (dir.exists ("series")))
    dir.create (file.path ("series"))
  
  arquivoParametros (populacao, lags)
  arquivoAvaliacoes (populacao)
  p = 1:nPOPULACAO
  parLapply (cl, p, function (x)
                    arquivosSeries (populacao[[x]], x))
  setwd (diretorio)

  stopCluster (cl)
}

MAPEdiferenca = function (populacao) {
  a = round (runif (1, 1, nPOPULACAO))
  individuo = populacao[[a]]$individuo
  
  MAPE = parLapply (cl, populacao, function (x)
                                   abs ((individuo - x$individuo) / individuo))
  MAPEdif = sum (unlist (MAPE)) / (nPOPULACAO * (length (individuo)))
  
  if (is.finite (MAPEdif))
    return (MAPEdif)
  else
    return (1)
}

arquivosSeries = function (individuo, p) {
  nome = paste0 ("series/serie_", p, ".csv")
  serie = data.frame (individuo$serie)
  rownames (serie) = c (1:nSINTETICA)
  colnames (serie) = c ("JANEIRO", "FEVEREIRO", "MARCO", "ABRIL", "MAIO", "JUNHO", "JULHO", "AGOSTO", "SETEMBRO", "OUTUBRO", "NOVEMBRO", "DEZEMBRO")
  write.csv2 (serie, nome)
}

arquivoParametros = function (populacao, lags) {
  parametros = t (sapply (populacao, function (x)
                                     x$individuo))
  parametros = data.frame (parametros)
  rownames (parametros) = c (1:nPOPULACAO)
  colnames (parametros) = rep (c ("JANEIRO", "FEVEREIRO", "MARCO", "ABRIL", "MAIO", "JUNHO", "JULHO", "AGOSTO", "SETEMBRO", "OUTUBRO", "NOVEMBRO", "DEZEMBRO"), sum (lags))
  write.csv2 (parametros, "parametros.csv")
}

arquivoAvaliacoes = function (populacao) {
  avaliacoes = sapply (populacao, function (x)
                                  x$avaliacao)
  avaliacoes = t (matrix (as.numeric (avaliacoes), ncol = length (populacao)))
  avaliacoes = data.frame (avaliacoes)
  rownames (avaliacoes) = c (1:nPOPULACAO)
  colnames (avaliacoes) = c("MAPEmedia", "MAPEdp", "MAPEfacAnual", "MAPEfacMensal", "SomRes")
  write.csv2 (avaliacoes, "avaliacoes.csv")
}