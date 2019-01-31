source ('avaliacao.R')

FNS = function (pop) {
  npIndividual = sapply (pop$avaliacao, function (x)
    dominanciaCompleta (x, pop))
  np = apply (npIndividual, 1, sum)
  rank = ranqueamento (np)
  
  return (rank)
}

dominanciaCompleta = function (avaliacoes, pop) {
  SpIndividual = sapply (pop$avaliacao, function (x)
    if ((avaliacoes$media < x$media) && (avaliacoes$dp < x$dp) && 
        (avaliacoes$facAnual < x$facAnual) && (avaliacoes$facMensal < x$facMensal) && 
        (avaliacoes$somRes < x$somRes)) {
      return (1)
    }
    else
      return (0))
}

ranqueamento = function (np) {
  fr = order (np)
  rank = numeric (nPOPULACAO)
  r = 1
  rank[fr[1]] = r
  
  for (p in (2:nPOPULACAO)) {
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
  dist = sapply(pop$avaliacao, function (x)
    distEuclidiana (x, pop))
  dist = t (dist)
  return (dist)
}

distEuclidiana = function (avaliacoes, pop) {
  dists = sapply(pop$avaliacao, function (x) 
    sqrt (sum ((avaliacoes$media - x$media)^2 + (avaliacoes$dp - x$dp)^2 + 
                 (avaliacoes$facAnual - x$facAnual)^2 + (avaliacoes$facMensal - x$facMensal)^2 + 
                 (avaliacoes$somRes - x$somRes)^2)))
  return (dists)
}

CCO = function (pop) {
  rank = FNS (pop)
  dist = CDA (pop)
  r = order (rank)
  diversidade = numeric (nPOPULACAO)
  d = numeric (nPOPULACAO)
  nivel = 1
  i = 1
  
  for (p in (1:5)) {
    if (rank[r[p]] == nivel) {
      d[r[p]] = dist[r[p]]
      print (d[r[p]])
    }
    d = order (d, na.last = T, decreasing = T)
    
  }
  print (diversidade)
  
  #populacaoOrd = pop$populacao[diversidade, ]
  #avaliacaoOrd = pop$avaliacao[diversidade]
  
  #novoPop = list (populacao = populacaoOrd, avaliacao = avaliacaoOrd)
  #return (novoPop)
}