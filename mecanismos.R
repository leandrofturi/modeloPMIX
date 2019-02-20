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
  p = sort (unique (np))
  fronteiras = sapply (p, function (x)
                          which (np %in% x))
  populacaoFronteiras = lapply (fronteiras, function (x)
                                            populacao[x])
  diversidadeFronteiras = lapply (populacaoFronteiras, CDA)
  nindividuos = sapply (diversidadeFronteiras, length)
  nindividuosAc = sapply ((1:(length (p))), function (x)
                                            sum (nindividuos[1:x]))
  nindividuosAc = nindividuosAc - nindividuos
  nindividuos = unlist (mapply (rep, times = nindividuos, x = nindividuosAc))
  diversidadeFronteiras = unlist (diversidadeFronteiras)
  diversidadeFronteiras = diversidadeFronteiras + nindividuos 
  
  diversidade = order (np)
  diversidade = diversidade[diversidadeFronteiras]
  
  populacao = populacao[diversidade]
  
  return  (populacao)
}

#AVALIAR SOMENTE AUTOCORRELACOES
FNSfac = function (populacao) {
  npIndividual = sapply (populacao, function (x)
                                    dominanciaCompletaFac (x, populacao))
  np = apply (npIndividual, 1, sum)
  np = np - 1
  
  return (np)
}

dominanciaCompletaFac = function (individuo, populacao) {
  SpIndividual = sapply (populacao, function (x)
    if ((individuo$avaliacao$facAnual <= x$avaliacao$facAnual) && (individuo$avaliacao$facMensal <= x$avaliacao$facMensal)) {
      return (1)
    }
    else
      return (0))
}

CCOfac = function (populacao) {
  np = FNSfac (populacao)
  p = sort (unique (np))
  fronteiras = sapply (p, function (x)
    which (np %in% x))
  populacaoFronteiras = lapply (fronteiras, function (x)
    populacao[x])
  diversidadeFronteiras = lapply (populacaoFronteiras, CDA)
  nindividuos = sapply (diversidadeFronteiras, length)
  nindividuosAc = sapply ((1:(length (p))), function (x)
    sum (nindividuos[1:x]))
  nindividuosAc = nindividuosAc - nindividuos
  nindividuos = unlist (mapply (rep, times = nindividuos, x = nindividuosAc))
  diversidadeFronteiras = unlist (diversidadeFronteiras)
  diversidadeFronteiras = diversidadeFronteiras + nindividuos
  
  diversidade = order (np)
  diversidade = diversidade[diversidadeFronteiras]
  
  populacao = populacao[diversidade]
  
  return  (populacao)
}