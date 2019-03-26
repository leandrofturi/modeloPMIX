source ("entrada.R")
source ('inicializaPop.R')
source ('mecanismos.R')
source ('PMIX.R')

#PARAMETROS ALGORITMO GENETICO
nPOPULACAO <<- 50
cicloMAX <<- 1000
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

require ('parallel')
cores = detectCores () - 2
cores = max (1, cores)
cl = makeCluster (1)
clusterExport (cl, list ("geraPinicial", "powell", "tamanhoPasso", "iteracoesMAX", "EPS",
                         "geraIndividuo", "avaliaIndividuo", "cruzamentoBLX", "torneio",
                         "momentos", "avaliacao", "estouro",
                         "FNS", "dominanciaCompleta", "CDA", "distancia",
                         "entrada", "correlograma", "correlogramaAnual", "lagSIGNIFICATIVO", "lagANUAL", "lagMENSAL", "residuos", "serieSint",
                         "nPOPULACAO", "cicloMAX", "MAPEdiferencaMAX", "nSINTETICA", "probCRUZAMENTO", "probMUTACAO"))

NSGA = function (dados, lags) {
  inicio = format (Sys.time (), "%F %Hh%M")
  entrada = entrada (dados)
  print ("Formando populacao inicial...")
  
  if (gerarPOWELL) {
    saidasPMIX = PMIXs (dados, lags, 25)
    parametrosIniciais = sapply (saidasPMIX, function (x) x$parametros)
    parametrosIniciais = t (parametrosIniciais)
  }
  
  else {
    parametrosIniciais = read.csv ("parametrosIniciais.csv", header = TRUE, sep = ";", dec = ",")
    parametrosIniciais = parametrosIniciais[-(1:3)]
    parametrosIniciais = as.vector (as.matrix (parametrosIniciais))
    parametrosIniciais = matrix (parametrosIniciais, ncol = (sum (lags))*12)
  }

  populacao = geraPopulacao (entrada, lags, nSINTETICA, parametrosIniciais)
  populacao = CCO (populacao)
  ciclo <<- 0
  
  while ((ciclo < cicloMAX) && (MAPEdiferenca (populacao) > MAPEdiferencaMAX) && (melhorIndividuo (populacao) > MAPEavaliacao)) {
    ciclo <<- ciclo + 1
    print (ciclo)
    
    novaPopulalacao = geraCruzamento (entrada, lags, populacao, nSINTETICA, probCRUZAMENTO, probMUTACAO)
    populacaoTotal = c(populacao, novaPopulalacao)
    populacaoTotal = CCO (populacaoTotal)
    populacao = populacaoTotal[1:nPOPULACAO]
  }
  
  populacao = melhorIndividuo (populacao)
  stopCluster (cl)
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
  lapply (p, function (x)
             arquivosSeries (populacao[[x]], x))
  setwd (diretorio)

  fim = format (Sys.time (), "%F %Hh%M")
	
  inicio = paste ("inicio:", inicio)
  fim = paste ("fim:", fim)
  print (inicio)
  print (fim)
}

MAPEdiferenca = function (populacao) {
  a = round (runif (1, 1, nPOPULACAO))
  individuo = populacao[[a]]$individuo
  
  MAPE = lapply (populacao, function (x)
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
  colnames (serie) = c ("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")
  write.csv2 (serie, nome)
}

arquivoParametros = function (populacao, lags) {
  parametros = t (sapply (populacao, function (x)
                                     x$individuo))
  parametros = data.frame (parametros)
  rownames (parametros) = c (1:nPOPULACAO)
  colnames (parametros) = rep (c ("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez"), sum (lags))
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
