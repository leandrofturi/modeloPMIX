source ('avaliacao.R')

nPOPULACAO = 500
nINDIVIDUO = 0
individuoMIN = -1
individuoMAX = 1
intervaloMedia = 0.1
intervaloDesvio = 0.5

geraIndividuo = function (nINDIVIDUO) {
  individuo = runif (nINDIVIDUO, individuoMIN, individuoMAX)
  
  return (individuo)
}

geraPopulacao = function (serieHN, lags, inicio, pop) {
  nINDIVIDUO <<- (sum (lags))*12
  populacao = matrix (numeric(1), ncol = nINDIVIDUO, nrow = nPOPULACAO)
  if (inicio) {
    lagAnualSignificativo (serieHN)
    lagMensalSignificativo (serieHN)
  }
  avaliacao = list ()
  excluidos = 0
  p = 0
  iniciarMomentos = T
  print("Formando populacao...")
  
  while (p < nPOPULACAO) {
###########################################################################################
    if (inicio && iniciarMomentos) {
      novoIndividuo = paramPhi (serieHN, lags[1])$phi
      p = p + 1
      momentos = momentos (serieHN, novoIndividuo, lags)
      av = avaliacao (serieHN, momentos)
      populacao[p, ] = novoIndividuo
      avaliacao[[p]] = av
      print (p)
      iniciarMomentos = F
    }
    
###########################################################################################
    
    if (inicio)
      novoIndividuo = geraIndividuo (nINDIVIDUO)
    else {
      pais = selecao (pop)
      novoIndividuo = crossoverBLX (pop$populacao[pais[1]], pop$populacao[pais[2]])
      probMut = runif (1, 0, 1)
      if (probMut <= 0.01) {
        novoIndividuo = mutacao (novoIndividuo)
        print ("mutacao")
      }
    }

    if ((prod (novoIndividuo) <= 1) && (prod (novoIndividuo) >= -1)) {
      momentos = momentos (serieHN, novoIndividuo, lags)
    
      if (!((is.nan (momentos$media)) || (is.nan (momentos$dp)) || (is.infinite (momentos$media)) || (is.infinite (momentos$dp)))) {
       #if (min ((momentos$media > (0 - intervaloMedia)) & (momentos$media < intervaloMedia) &
                #(momentos$dp > (1 - intervaloDesvio)) & (momentos$dp < (1 + intervaloDesvio)))) {
          av = avaliacao (serieHN, momentos)
          p = p + 1
          populacao[p, ] = novoIndividuo
          avaliacao[[p]] = av
          print (p)       
       #}
      #else
        #excluidos = excluidos + 1
      }
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
  beta = runif (nINDIVIDUO, -ALFA, 1+ALFA)
  homog = round (runif (nINDIVIDUO, 0, 1))
  
  filho = pai1 + homog*beta*(pai2 - pai1)
  return (filho)
}

mutacao = function (individuo) {
  novoIndividuo = runif (nINDIVIDUO, individuoMIN, individuoMAX)
  
  return (novoIndividuo)
}

selecao = function (pop) {
  possiveis = sample(nPOPULACAO, 3, replace = F)
  possiveis = sort (possiveis)
  possiveis = possiveis[-3]
  return (possiveis)
}

#pop$avalioacao[[1]]$media
#sapply(pop$avaliacao, function(x) (x$media)) pega todos e coloca numa matriz

source ('correlograma.R')
paramPhi = function (serieHN, p)
{
  fac = correlograma (serieHN, p, F)
  serie = as.vector (t (serieHN))
  n = length (serieHN)/12
  
  tablePhi = matrix(numeric (1), nrow = p, ncol = 12)
  varEpsilon = rep (1, 12)
  
  for (mes in 1:12) {
    Pk = matrix (numeric (1), nrow = p, ncol = p)
    rok = rep (1, p)
    I = matrix (numeric (1), nrow = p, ncol = p)
    
    for(j in 1:p){
      meslag = (mes - j - 1) %% 12 + 1
      for(i in j:p){
        k = abs (j - i) + 1
        Pk[i, j] = fac[k, meslag]
        
        if(i == j){
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