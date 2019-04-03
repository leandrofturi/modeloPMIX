lexicografia = function (populacao, tolerancias, pesos) {
  p = 1:5
  ordem = lapply (p, function (x)
                     ordenamento (populacao, tolerancias[x], x))
  melhores = t (sapply (ordem, function (x)
                               apply (x, 1, sum)))
  melhores = melhores*pesos
  diversidade = order (apply (melhores, 2, sum), decreasing = T)
  
  populacao = populacao[diversidade]
  
  return (populacao)
}

ordenamento = function (populacao, tolerancia, criterio) {
  np = sapply (populacao, function (x)
                          dominancia (x, tolerancia, criterio, populacao))
  return (np)
}

dominancia = function (individuo, tolerancia, criterio, populacao) {
  sapply (populacao, function (x)
                     if (((unlist (x$avaliacao[criterio]))*(1-tolerancia)) < (unlist (individuo$avaliacao[criterio])))
                      return (1)
                     else
                       return (0))
}
#cada 1 nas linhas significa que aquele e melhor que o correspondente na coluna