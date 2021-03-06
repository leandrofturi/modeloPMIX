source ('cenarioSintetico.R')
source ('correlograma.R')
source ('sumQuadRes.R')

# FUNCOES DE AVALIACAO PARA O ALGORITMO GENETICO
#
# entrada: Saida da funcao entrada
# parametros: Vetor contendo parametros phi, tht, PHI, THT. Parametros de ordem zero sao omitidos.
# lags: Vetor contendo as ordens dos parametros do modelo.
# nS: Tamanho da serie sintetica

momentos = function (entrada, parametros, lags, nS) {
  residuos = residuos (entrada$serieHN, parametros, lags)
  dpRes = residuos$dpRes
  serieSint = serieSintetica (parametros, dpRes, lags, nS)
  serieSint = t ((t (serieSint) * entrada$dpHL) + entrada$mediaHL)
  serieSint = exp (serieSint)
  
  media = apply (serieSint, 2, mean)
  dp = apply (serieSint, 2, sd)
  serieAnual = apply (serieSint, 1, sum)
  facAnual = autocorrelacaoAnual (serieAnual, entrada$lagAnual)[-1]
  facMensal = autocorrelacaoMensal (serieSint, entrada$lagMensal)[-1, ]
  somRes = residuos$somRes
  
  final = list (serie = serieSint, media = media, dp = dp, facAnual = facAnual, facMensal = facMensal, somRes = somRes)
  return (final)
}

# entrada: Saida da funcao entrada
# momentosS: Saida da funcao momentos

avaliacao = function (entrada, momentosS) {
  mediaH = entrada$mediaH
  dpH = entrada$dpH
  # UTILIZA PARA O CALCULO SOMEBTE LAGS SIGNIFICATIVOS
  facAnualH = entrada$facAnual[-1]
  facAnualH = facAnualH[-((entrada$lagAnual + 1):12)]
  facMensalH = entrada$facMensal[-1, ]
  facMensalH = facMensalH[-((entrada$lagMensal + 1):12), ]
  # CALCULO DO MAPE
  MAPEMedia = sum (abs ((mediaH - momentosS$media) / mediaH)) / 12
  MAPEDesvio = sum (abs ((dpH - momentosS$dp)) / dpH) / 12
  #TRATA AS AUTOCORRELACOES COMO VETORES
  MAPEFacAnual = sum (abs ((facAnualH - momentosS$facAnual) / facAnualH)) / entrada$lagAnual
  MAPEFacMensal = sum (abs ((facMensalH - momentosS$facMensal) / facMensalH)) / ((entrada$lagMensal)*12)
  somRes = momentosS$somRes
  
  final = list (media = MAPEMedia, dp = MAPEDesvio, facAnual = MAPEFacAnual, facMensal = MAPEFacMensal, somRes = somRes)
  return (final)
}

# VERIFICA SE HOUVE ESTOURO DE ALGUM MOMENTO CALCULADO
estouro = function (momentos) {
  if ((min (is.finite (momentos$media))) &&
      (min (is.finite (momentos$dp))) &&
      (min (is.finite (momentos$facAnual))) &&
      (min (is.finite (momentos$facMensal))) &&
      (min (is.finite (momentos$somRes))))
    return (FALSE)
  
  return (TRUE)
}
