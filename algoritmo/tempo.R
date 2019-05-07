NSGA = function (dados, lags, dadosPowell) {
  inicio = Sys.time ( )
  
  entrada = entrada (dados)
  
  if (gerarPOWELL) {
    saidasPMIX = PMIXs (dados, lags, nPOPULACAO, ordem)
    parametrosIniciais = sapply (saidasPMIX, function (x) x$parametros)
    parametrosIniciais = t (parametrosIniciais)
  }
  else {
    parametrosIniciais = read.csv (dadosPowell, header = TRUE, sep = ";", dec = ",")
    parametrosIniciais = parametrosIniciais[-(1:3)]
    parametrosIniciais = as.vector (as.matrix (parametrosIniciais))
    parametrosIniciais = matrix (parametrosIniciais, ncol = (sum (lags))*12)
  }

  populacao = geraPopulacao (entrada, lags, nSINTETICA, parametrosIniciais, nPOPULACAO)
  populacao = CCO (populacao)
  ciclo = 0
  
  while ((ciclo < cicloMAX) && (MAPEdiferenca (populacao) > MAPEdiferencaMAX)) {
    ciclo = ciclo + 1
    
    novaPopulalacao = geraCruzamento (entrada, lags, populacao, nSINTETICA, probCRUZAMENTO, probMUTACAO, nPOPULACAO)
    populacaoTotal = c (populacao, novaPopulalacao)
    populacaoTotal = CCO (populacaoTotal)
    populacao = populacaoTotal[1:nPOPULACAO]
  }
  populacao = lexicografia (populacao, TOLERANCIAS, PESOS)
  
  fim = Sys.time ( )
  duracao = as.numeric (difftime (fim, inicio))

  arqParametros = arquivoParametros (populacao, lags)
  arqAvaliacoes = arquivoAvaliacoes (populacao)
  p = 1:nPOPULACAO
  arqSeries = lapply (p, function (x)
                         arquivosSeries (populacao[[x]], x))
  
  final = list (arqParametros = arqParametros, arqAvaliacoes = arqAvaliacoes, arqSeries = arqSeries, duracao = duracao)
  return (final)
}

NSGAagrupado = function (dados, lags, ordem, nGrupos) {
  inicio = format (Sys.time (), "%F %Hh%M")
  entrada = entrada (dados)
  #print ("Formando populacao inicial...")
  
  if (gerarPOWELL) {
    saidasPMIX = PMIXs (dados, lags, nPOPULACAO, ordem)
    parametrosIniciais = sapply (saidasPMIX, function (x) x$parametros)
    parametrosIniciais = t (parametrosIniciais)
  }
  
  else {
    parametrosIniciais = read.csv (paste0 ("parametrosIniciais_", ordem, ".csv"), header = TRUE, sep = ";", dec = ",")
    parametrosIniciais = parametrosIniciais[-(1:3)]
    parametrosIniciais = as.vector (as.matrix (parametrosIniciais))
    parametrosIniciais = matrix (parametrosIniciais, ncol = (sum (lags))*12)
  }
  
  populacao = geraPopulacao (entrada, lags, nSINTETICA, parametrosIniciais, nPOPULACAO)
  nGerada = (nGrupos-1)*nPOPULACAO
  #print ("Efetuando agrupamento...")
  grupos = agrupamento (entrada, lags, populacao, nGerada, nSINTETICA, nGrupos)
  #print ("Agrupamento completo!")
  
  rodadas = 0
  while ((rodadas <= 100) && (MAPEdiferenca (populacao) > MAPEdiferencaMAX)) {
    rodadas = rodadas + 1
    print (rodadas)
    
    resultadosPorGrupo = lapply (grupos, function (x)
                                         AG (entrada, lags, x))
    populacao = list ( )
    for (c in (1:nGrupos))
      populacao = c (populacao, resultadosPorGrupo[[c]])
    
    grupos = agrupamento (entrada, lags, populacao, 0, nSINTETICA, nGrupos)
  }
  
  populacao = lexicografia (populacao, TOLERANCIAS, PESOS)
  
  stopCluster (cl)
  
  populacao = list ()
  for (c in (1:nGrupos))
    populacao = c (populacao, grupos[[c]])
  
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
  
  fim = format (Sys.time (), "%F %Hh%M")
  inicio = paste ("inicio:", inicio)
  fim = paste ("fim:", fim)
  
  sink ("tempoExecucao.txt")
  print (inicio)
  print (fim)
  print (paste ("rodadas:", rodadas))
  sink ( )
  
  arquivoParametros (populacao, lags)
  arquivoAvaliacoes (populacao)
  p = 1:length (populacao)
  lapply (p, function (x)
    arquivosSeries (populacao[[x]], x))
  
  setwd (diretorio)
}

AG = function (entrada, lags, populacao) {
  ciclo = 0
  
  while ((ciclo < (round (cicloMAX/10))) && (MAPEdiferenca (populacao) > MAPEdiferencaMAX)) {
    ciclo = ciclo + 1
    
    novaPopulalacao = geraCruzamento (entrada, lags, populacao, nSINTETICA, probCRUZAMENTO, probMUTACAO, nPOPULACAO)
    populacaoTotal = c (populacao, novaPopulalacao)
    populacaoTotal = CCO (populacaoTotal)
    populacao = populacaoTotal[1:nPOPULACAO]
  }
  
  return (populacao)
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

arquivosSeries = function (parte, p) {
  serie = data.frame (parte$serie)
  rownames (serie) = c (1:nSINTETICA)
  colnames (serie) = c ("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez")
  return (serie)
}

arquivoParametros = function (populacao, lags) {
  parametros = t (sapply (populacao, function (x) x$individuo))
  parametros = data.frame (parametros)
  rownames (parametros) = c (1:length (populacao))
  colnames (parametros) = rep (c ("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez"), sum (lags))
  return (parametros)
}

arquivoAvaliacoes = function (populacao) {
  avaliacoes = sapply (populacao, function (x)
                                  x$avaliacao)
  avaliacoes = t (matrix (as.numeric (avaliacoes), ncol = length (populacao)))
  avaliacoes = data.frame (avaliacoes)
  rownames (avaliacoes) = c (1:length (populacao))
  colnames (avaliacoes) = c("MAPEmedia", "MAPEdp", "MAPEfacAnual", "MAPEfacMensal", "SomRes")
  return (avaliacoes)
}
