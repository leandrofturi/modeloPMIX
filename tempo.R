source('inicializacao.R')

roleta = function (avaliacoes) {
  giro = runif (1,0,1)
  giro = giro
  avaliacoes = avaliacoes / (sum (avaliacoes))
  
  for (i in (1:(length(avaliacoes)-1))) {
    if (avaliacoes[i] >= giro)
      return (i)
    
    else
      avaliacoes[i+1] = avaliacoes[i] + avaliacoes[i+1]
  }
}

cruzamento = function (pai1, pai2) {
  ponto1 = round (runif (1, 2, (TAM_INDIVIDUO-1)))
  ponto2 = round (runif (1, (ponto1+1), TAM_INDIVIDUO))
  
  filho1 = c(pai1[1:(ponto1-1)], pai2[ponto1:(ponto2-1)], pai1[ponto2:TAM_INDIVIDUO])
  filho2 = c(pai2[1:(ponto1-1)], pai1[ponto1:(ponto2-1)], pai2[ponto2:TAM_INDIVIDUO])
  
  final = list (filho1 = filho1, filho2 = filho2)
  return (final)
}

mutacao = function (individuo) {
  troca = runif (1,0,1)
  
  if (troca <= 0.005) {
    pos = runif (1,0,TAM_INDIVIDUO)
    if (individuo[pos] == 0) individuo[pos] = 1
    else individuo[pos] = 0
  }
  
  return (individuo)
}

GA = function (dados, lags) {
  qntParam = sum(lags)
  tamIndividuo = qntParam*12*PRECISAO
  TAM_INDIVIDUO <<- tamIndividuo
  populacao = inicializaPop (tamIndividuo)
  serie = entrada (dados)$serieHN
  
  avaliacoes = avaliacao (serie, lags, populacao)
  
  for (t in (1:10)) {
    p1 = roleta (avaliacoes)
    p2 = roleta (avaliacoes)
    print(p1)
    print(p2)
    
    novaGeracao = cruzamento (populacao[p1, ], populacao[p2, ])
    filho1 = novaGeracao$filho1
    filho2 = novaGeracao$filho2
    
    filho1 = mutacao (filho1)
    filho2 = mutacao (filho2)
    
    populacao[p1, ] = filho1
    populacao[p2, ] = filho2
    
    avaliacoes = avaliacao (serie, lags, populacao)
    #print(max(avaliacoes))
    
  }
}

avaliacao = function (serie, lags, populacao) {
  
  popParam = t (apply (populacao, 1, cortaParametros))
  avaliacao = apply (popParam, 1, function (parametros) 
    residuos (serie, parametros, lags)$somRes)
  
  avaliacao = 10000 / (avaliacao + 10)
  
  return (avaliacao)
}


