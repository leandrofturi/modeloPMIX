source ("entrada.R")
source ('inicializaPop.R')
source ('mecanismos.R')
source ('PMIX.R')
source ('tempo.R')
source ('dataMining.R')
source ('lexicografica.R')
source ('graficoSeries.R')
source ('graficoFAC_Anual.R')
source ('graficoFAC_Mensal.R')

#PARAMETROS ALGORITMO GENETICO
nPOPULACAO <<- 30
cicloMAX <<- 10000
MAPEdiferencaMAX <<- 0.15
MAPEavaliacao <<- 0.2

#PARAMETROS FUNCAO OBJETIVO
nSINTETICA <<- 10000
probCRUZAMENTO <<- 0.8
probMUTACAO <<- 0.05

lagSIGNIFICATIVO <<- T
lagANUAL <<- 1
lagMENSAL <<- 1

gerarPOWELL <<- F

#PARAMETROS AVALIACAO
TOLERANCIAS <<- c (0.2, 0.1, 0.2, 0.05, 0.05)
PESOS <<- c (3, 1, 5, 5, 1)


require ('parallel')
cores = detectCores () - 2
cores = max (1, cores)
cl = makeCluster (1)
clusterExport (cl, list ("geraPinicial", "powell", "tamanhoPasso", "iteracoesMAX", "EPS",
                         "geraIndividuo", "avaliaIndividuo", "cruzamentoBLX", "torneio",
                         "momentos", "avaliacao", "estouro",
                         "FNS", "dominanciaCompleta", "CDA", "distancia",
                         "entrada", "correlograma", "correlogramaAnual", "lagSIGNIFICATIVO", "lagANUAL", "lagMENSAL", "residuos", "serieSint",
                         "agrupamento", "distEuclidiana", "KNN",
                         "GA",
                         "nPOPULACAO", "cicloMAX", "MAPEdiferencaMAX", "nSINTETICA", "probCRUZAMENTO", "probMUTACAO"))

COMPLETO = function (dados) {
  lags = list ( )
  lags[[1]] = c (1,0,0,0)
  lags[[2]] = c (1,0,1,0)
  lags[[3]] = c (1,1,0,0)
  lags[[4]] = c (1,1,1,0)
  lags[[5]] = c (1,1,1,1)
  lags[[6]] = c (2,0,0,0)
  lags[[7]] = c (2,0,1,0)
  lags[[8]] = c (2,1,0,0)
  lags[[9]] = c (2,1,1,0)
  lags[[10]] = c (2,1,1,1)
  lags[[11]] = c (2,2,2,2)
  lags[[12]] = c (3,2,2,1)
  
  p = 1:length(lags)
  
  lapply (p, function (x)
             NSGA (dados, lags[[x]], x))
  
  stopCluster (cl)
}