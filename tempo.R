source('restricoes.R')
source('sumQuadRes.R')
source('cenarioSint.R')

roleta = function (avaliacoes) {
  giro = runif (1,0,1)
  avaliacoes = sort (avaliacoes / (sum (avaliacoes)))
  
  for (i in (1:length(avaliacoes))) {
    if (avaliacoes[i] >= giro)
      return (i)
    
    else
      avaliacoes[i+1] = avaliacoes[i] + avaliacoes[i+1]
  }
}

cruzamentoDoisPontos = function (pai1, pai2) { #NAO E UTIL
  ponto1 = round (runif (1, 2, (TAM_INDIVIDUO-1)))
  ponto2 = round (runif (1, (ponto1+1), TAM_INDIVIDUO))
  
  filho1 = c(pai1[1:(ponto1-1)], pai2[ponto1:(ponto2-1)], pai1[ponto2:TAM_INDIVIDUO])
  filho2 = c(pai2[1:(ponto1-1)], pai1[ponto1:(ponto2-1)], pai2[ponto2:TAM_INDIVIDUO])
  
  final = list (filho1 = filho1, filho2 = filho2)
  return (final)
}

cruzamentoUniforme = function (pai1, pai2) {
  probCruzamento = runif (1, 0, 1)
  if (probCruzamento > 0.75) {
    final = list (filho1 = pai1, filho2 = pai2)
    return (final)
  }
    
  pontos = round (runif (TAM_INDIVIDUO, 0, 1))
  
  filho1 = numeric (TAM_INDIVIDUO)
  filho2 = numeric (TAM_INDIVIDUO)
  for (i in (1 : TAM_INDIVIDUO)) {
    if (pontos[i] == 1) {
      filho1[i] = pai2[i]
      filho2[i] = pai1[i]
    }
    else {
      filho1[i] = pai1[i]
      filho2[i] = pai2[i]
    }
  }
  
  final = list (filho1 = filho1, filho2 = filho2)
  return (final)
}

mutacao = function (individuo) {
  troca = runif (1,0,1)
  
  if (troca <= 0.004) {
    pos = runif (1,1,TAM_INDIVIDUO)
    individuo[pos] = 1 - individuo[pos]
  }
  
  return (individuo)
}

GA = function (dados, lags) {
  qntParam = sum(lags)
  tamIndividuo = qntParam*12*PRECISAO
  TAM_INDIVIDUO <<- tamIndividuo
  serie = entrada (dados)$serieHN
  populacao = inicializaPop (serie, lags)
  
  avaliacoes = avaliacao (serie, lags, populacao)
  
  for (t in (1:500)) {
    p1 = roleta (avaliacoes)
    p2 = roleta (avaliacoes)
    
    novaGeracao = cruzamentoUniforme (populacao[p1, ], populacao[p2, ])
    filho1 = novaGeracao$filho1
    filho2 = novaGeracao$filho2
    
    filho1 = mutacao (filho1)
    filho2 = mutacao (filho2)
    
    populacao[p1, ] = filho1
    populacao[p2, ] = filho2
    
    avaliacoes = avaliacao (serie, lags, populacao)

    print(max (avaliacoes))
  }
}

BRKGA = function (dados, lags) {
  qntParam = sum(lags)
  tamIndividuo = qntParam*12*PRECISAO
  TAM_INDIVIDUO <<- tamIndividuo
  serie = entrada (dados)$serieHN
  populacao = inicializaPop (serie, lags)
  
  avaliacoes = avaliacao (serie, lags, populacao)
  
  novaPop = populacao
  for (t in (1:500)) {
    ordemAv = order (avaliacoes, decreasing = T)
    p = round (0.6*TAM_POPULACAO)
    novaPop[1:p, ] = populacao[ordemAv[1:p], ]
    p = p + 1
    while (p <= TAM_POPULACAO) {
      posPai = roleta (avaliacoes[ordemAv[1:(round (0.6*TAM_POPULACAO))]])
      pai1 = populacao[posPai, ]
      posPai = roleta (avaliacoes)
      pai2 = populacao[posPai, ]
      
      filhos = cruzamentoUniforme (pai1, pai2)
      filho1 = mutacao (filhos$filho1)
      filho2 = mutacao (filhos$filho2)
      
      novaPop[p, ] = filho1
      novaPop[p + 1, ] = filho2
      
      p = p + 2
    }
    
    avaliacoes = avaliacao (serie, lags, populacao)
    print (t)
  }
  ordemAv = order (avaliacoes, decreasing = T)
  paramFinal = cortaParametros (populacao[ordemAv[1], ])
  
  sumRes = residuos (serie, paramFinal, lags)
  sumQuadRes = sumRes$somRes
  dpRes = sumRes$dpRes
  
  serieSint = serieSint (paramFinal, dpRes, lags, 10000)
  media = apply(serieSint, 2, mean)
  dp = apply(serieSint, 2, sd)
  
  final = list (sumQuadRes = sumQuadRes, dpRes = dpRes, media = media, dp = dp)
  
  return (final)
}

avaliacao = function (serie, lags, populacao) {
  
  popParam = t (apply (populacao, 1, cortaParametros))
  avaliacao = apply (popParam, 1, function (parametros) 
                                    residuos (serie, parametros, lags)$somRes)
  
  avaliacao = 1 / (avaliacao + 10)
  
  return (avaliacao)
}


