FNS = function (populacao) {
  npIndividual = sapply (populacao, function (x)
                                    dominanciaCompleta (x, populacao))
  np = apply (npIndividual, 1, sum)
  np = np - 1
  
  return (np)
}

dominanciaCompleta = function (individuo, populacao) {
  SpIndividual = sapply (populacao, function (x)
    if ((individuo$avaliacao$media <= x$avaliacao$media) && (individuo$avaliacao$dp <= x$avaliacao$dp) &&
        (individuo$avaliacao$facAnual <= x$avaliacao$facAnual) && (individuo$avaliacao$facMensal <= x$avaliacao$facMensal) &&
        (individuo$avaliacao$somRes <= x$avaliacao$somRes)) {
      return (1)
    }
    else
      return (0))
}

CDA = function (populacao) {
  nINDIVIDUO = length (populacao[[1]]$individuo)
  n = length (populacao)
  
  if (n > 2) {
    avaliacoes = sapply (populacao, function (x) x$avaliacao)
    avaliacoes = matrix (as.numeric (avaliacoes), ncol = n)
    p = 1:((length (avaliacoes)) / n)
    distancias = t (sapply (p, function (x)
                               distancia (avaliacoes[x, ], n)))
    
    diversidade = apply (distancias, 2, sum)
    diversidade = order (diversidade, decreasing = T)
  }
  else
    diversidade = 1:n

  return (diversidade)
}

distancia = function (avaliacoes, n) {
  
  dist = numeric (n)
  ord = order (avaliacoes)
  avMax = max (avaliacoes)
  avMin = min (avaliacoes)
  
  for (i in (2:(n-1))) {
    dist[ord[i]] = dist[ord[i-1]] + ((avaliacoes[ord[i+1]] - avaliacoes[ord[i-1]]) / (avMax - avMin))
  }
  
  dist[ord[1]] = dist[ord[n]] = Inf
  return (dist)
}

CCO = function (populacao) {
  np = FNS (populacao)
  #print (np)
  p = sort (unique (np))
  fronteiras = sapply (p, function (x)
                          which (np %in% x))
  populacaoFronteiras = lapply (fronteiras, function (x)
                                            populacao[x])
  diversidadeFronteiras = lapply (populacaoFronteiras, CDA)
  nindividuos = sapply (diversidadeFronteiras, length)
  nindividuos = c(0, nindividuos[1:(length (nindividuos)-1)])
  diversidadeFNS = order (np)
  
  #return  (diversidade)
  
}