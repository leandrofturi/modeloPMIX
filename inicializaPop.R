source ('avaliacao.R')

nPOPULACAO = 50

geraIndividuo = function (nINDIVIDUO) {
  individuoMIN = -1
  individuoMAX = 1
  individuo = runif (nINDIVIDUO, individuoMIN, individuoMAX)
  
  return (individuo)
}

geraPopulacao = function (series, lags, inicio, pop) {
  nINDIVIDUO = (sum (lags))*12
  populacao = matrix (numeric(1), ncol = nINDIVIDUO, nrow = nPOPULACAO)

  avaliacao = list ()
  excluidos = 0
  p = 0
  iniciarMomentos = T
  print("Formando populacao...")
  
  while (p < nPOPULACAO) {
    
###########################################################################################
    
    if (inicio && iniciarMomentos) {
      novoIndividuo = paramPhi (series$serieHN, lags[1])$phi
      novoIndividuo = c (novoIndividuo, rep (0, 12*sum (lags[2] + lags[3] + lags[4])))
      p = p + 1
      momentos = momentos (series, novoIndividuo, lags)
      if (! (max (estouro (momentos$media)) || (estouro (momentos$dp)) || (estouro (momentos$facAnual)) || (estouro (momentos$facMensal)))) {
        av = avaliacao (series, momentos)
        populacao[p, ] = novoIndividuo
        avaliacao[[p]] = av
      }
      iniciarMomentos = F
    }
    
###########################################################################################
    
    if (inicio)
      novoIndividuo = geraIndividuo (nINDIVIDUO)
    
    else {
      pais = selecao (pop)
      novoIndividuo = crossoverBLX (pop$populacao[pais[1], ], pop$populacao[pais[2], ])
      
      probMut = runif (1, 0, 1)
      if (probMut <= 0.01) {
        novoIndividuo = mutacao (novoIndividuo)
      }
    }

    momentos = momentos (series, novoIndividuo, lags)
    if (! (max (estouro (momentos$media)) || (estouro (momentos$dp)) || (estouro (momentos$facAnual)) || (estouro (momentos$facMensal)))) {
        av = avaliacao (series, momentos)
        p = p + 1
        print (p)
        populacao[p, ] = novoIndividuo
        avaliacao[[p]] = av
      }
    else
      excluidos = excluidos + 1
  }
  
  if (inicio)
    print (paste ("excluidos", excluidos))
  
  final = list (populacao = populacao, avaliacao = avaliacao)
  return (final)
}

crossoverBLX = function (pai1, pai2) {
  ALFA = 0.5
  beta = runif ((length (pai1)), -ALFA, 1+ALFA)
  homog = round (runif ((length (pai1)), 0, 1))
  
  filho = pai1 + homog*beta*(pai2 - pai1)
  return (filho)
}

mutacao = function (individuo) {
  novoIndividuo = geraIndividuo (length (individuo))
  
  return (novoIndividuo)
}

selecao = function (pop) {
  possiveis = sample (nPOPULACAO, 3, replace = F)
  possiveis = sort (possiveis)
  possiveis = possiveis[-3]
  return (possiveis)
}

###########################################################################################

source ('correlograma.R')
paramPhi = function (serieHN, p)
{
  fac = correlograma (serieHN, p, F)
  serie = as.vector (t (serieHN))
  n = length (serieHN)/12
  
  tablePhi = matrix (numeric (1), nrow = p, ncol = 12)
  varEpsilon = rep (1, 12)
  
  for (mes in 1:12) {
    Pk = matrix (numeric (1), nrow = p, ncol = p)
    rok = rep (1, p)
    I = matrix (numeric (1), nrow = p, ncol = p)
    
    for (j in 1:p){
      meslag = (mes - j - 1) %% 12 + 1
      for(i in j:p){
        k = abs (j - i) + 1
        Pk[i, j] = fac[k, meslag]
        
        if (i == j){
          I[i, j] = 1
        }
      }
    }
    Pk = Pk + t (Pk) - I
    rok = fac[-1, mes]
    tablePhi[, mes] = solve (Pk, rok)
    
    varEpsilon[mes] = 1 - sum (tablePhi[, mes] * fac[-1, mes])
  }
  dpRes = sqrt (varEpsilon)
  
  final = list (phi = tablePhi, dpRes = dpRes)
  return (final)
}