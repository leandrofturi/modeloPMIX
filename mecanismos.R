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

CDA = function (populacao, n) {
  p = 1:n
  nINDIVIDUO = length (populacao[[1]]$individuo)
  
  avaliacoes = sapply (populacao, function (x) x$avaliacao)
  
  return (avaliacoes)
}

distancia = function (avaliacoes) {
  n = length (avaliacoes)
  dist = numeric (n)
  
  ord = order (avaliacoes)
  avMax = max (avaliacoes)
  avMin = min (avaliacoes)
  
  for (i in (2:(n-1))) {
    dist[ord[i]] = dist[ord[i-1]] + ((avaliacoes[ord[i+1]] - avaliacoes[ord[i-1]]) / (avMax - avMin))
  }
  
  dist[ord[1]] = dist[ord[n]] = sum (avaliacoes)
  return (dist)
}