source ('avaliacao.R')

FNS = function (pop) {
  npIndividual = sapply (pop$avaliacao, function (x)
                                        dominanciaCompleta (x, pop))
  np = apply (npIndividual, 1, sum)
  np = np-1
  rank = ranqueamento (np)
  
  return (rank)
}

dominanciaCompleta = function (avaliacoes, pop) {
  SpIndividual = sapply (pop$avaliacao, function (x)
    if ((avaliacoes$media <= x$media) && (avaliacoes$dp <= x$dp) && (avaliacoes$facAnual <= x$facAnual) && (avaliacoes$facMensal <= x$facMensal) && (avaliacoes$somRes <= x$somRes)) {
      return (1)
    }
    else
      return (0))
}

ranqueamento = function (np) {
  fr = order (np)
  rank = numeric (length (np))
  r = 1
  rank[fr[1]] = r
  
  for (p in (2:(length (np)))) {
    if (np[fr[p-1]] == np[fr[p]])
      rank[fr[p]] = r
    else {
      r = r+1
      rank[fr[p]] = r
    }
  }
  return (rank)
}

CDA = function (pop) {
  nINDIVIDUO = length (pop$populacao[1, ])
  n  = length (pop$populacao) / nINDIVIDUO
  CD = numeric (n)
  diversidade = numeric (n)
  nObjetivos = 5
  CDmat = matrix (numeric (0), ncol = n, nrow = nObjetivos)
  
  medias = sapply (pop$avaliacao, function(x) (x$media))
  desvios = sapply (pop$avaliacao, function(x) (x$dp))
  facAnuais = sapply (pop$avaliacao, function(x) (x$facAnual))
  facMensais = sapply (pop$avaliacao, function(x) (x$facMensal))
  somRes = sapply (pop$avaliacao, function(x) (x$somRes))
  
  objetivos = matrix (c(medias, desvios, facAnuais, facMensais, somRes), ncol = n, nrow = nObjetivos, byrow = T)
  CDmat = t (apply (objetivos, 1, function (x) distancia (x)))
  CD = apply (CDmat, 2, sum)
  
  return (CD)
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

CCO = function (pop) {
  nINDIVIDUO = length (pop$populacao[1, ])
  rank = FNS (pop)
  if (length (rank) <= 2) {
    return (pop)
  }
  diversidade = order (rank)
  
  contRank = 1
  inicioRank = 1
  
  for (r in (2:(length (rank)))) {
    if (rank[diversidade[r-1]] == rank[diversidade[r]])
      contRank = contRank+1
    else {
      if (contRank > 2) {
        populacaoRank = matrix (numeric (0), ncol = nINDIVIDUO, nrow = contRank)
        populacaoRank = pop$populacao[diversidade[inicioRank:(r-1)], ]
        avaliacaoRank = list ()
        avaliacaoRank = pop$avaliacao[diversidade[inicioRank:(r-1)]]
        popRank = list (populacao = populacaoRank, avaliacao = avaliacaoRank)
        rankOrd = CDA (popRank)
        rankOrd = order (rankOrd, decreasing = T)
        diversidade[inicioRank:(r-1)] = diversidade[inicioRank:(r-1)][rankOrd]
      }
      inicioRank = r
      contRank = 1
    }
  }
  populacaoOrd = pop$populacao[diversidade, ]
  avaliacaoOrd = pop$avaliacao[diversidade]
  
  novoPop = list (populacao = populacaoOrd, avaliacao = avaliacaoOrd)
  return (novoPop)
}