source('sumQuadRes.R')

nPOPULACAO = 50
individuoMIN = -2
individuoMAX = 2

geraIndividuo = function (nIndividuo) {
  individuo = runif (nIndividuo, individuoMIN, individuoMAX)
  
  return (individuo)
}

geraPopulacao = function (serie, lags) {
  nIndividuo = (sum (lags))*12
  populacao = matrix (numeric(1), ncol = nIndividuo, nrow = nPOPULACAO)
  somResPop = numeric (nPOPULACAO)
  mediaPop = matrix (numeric(1), ncol = 12, nrow = nPOPULACAO)
  dpPop = matrix (numeric(1), ncol = 12, nrow = nPOPULACAO)
  
  p = 0
  print("Formando populacao...")
  
  while (p < nPOPULACAO) {
    novoIndividuo = geraIndividuo (nIndividuo)
    
    if (prod(novoIndividuo) <= 1) { # TESTE DO PRODUTO
      momentos = momentos (serie, lags, novoIndividuo)
      media = momentos$media
      dp = momentos$dp
      somRes = momentos$somRes
      
      if (!((is.nan (media)) || (is.nan (dp)) || (is.infinite (media)) || (is.infinite (dp)))) {
        p = p + 1
        populacao[p, ] = novoIndividuo
        somResPop[p] = somRes
        mediaPop[p, ] = media
        dpPop[p, ] = dp
        print (p)
      }
    }
  }
  avInicialPop = list(somRes = somResPop, media = mediaPop, dp = dpPop)
  final = list (avaliacao = avInicialPop, populacao = populacao)
  return (final)
}

momentos = function (serie, lags, parametros) {
  residuos = residuos (serie, parametros, lags)
  dpRes = residuos$dpRes
  serieSint = serieSint (parametros, dpRes, lags, 1000)
  
  media = apply(serieSint, 2, mean)
  dp = media = apply(serieSint, 2, sd)
  somRes = residuos$somRes
  
  final = list (media = media, dp = dp, somRes = somRes)
  
  return (final)
}





