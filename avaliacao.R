source ('cenarioSint.R')
source ('correlograma.R')
source ('sumQuadRes.R')

momentos = function (entrada, parametros, lags, nS) {
  residuos = residuos (entrada$serieHN, parametros, lags)
  dpRes = residuos$dpRes
  serieSint = serieSint (parametros, dpRes, lags, nS)
  serieSint = t ((t (serieSint) * entrada$dpHL) + entrada$mediaHL)
  serieSint = exp (serieSint)
  
  media = apply (serieSint, 2, mean)
  dp = apply (serieSint, 2, sd)
  facAnual = correlogramaAnual (serieSint, entrada$lagAnual)[-1]
  facMensal = correlograma (serieSint, entrada$lagMensal, F)[-1, ]
  somRes = residuos$somRes
  
  final = list (media = media, dp = dp, facAnual = facAnual, facMensal = facMensal, somRes = somRes)
  return (final)
}

avaliacao = function (entrada, momentosS) {
  mediaH = entrada$mediaH
  dpH = entrada$dpH
  facAnualH = entrada$facAnual
  facMensalH = entrada$facMensal
  MAPEMedia = sum (abs ((mediaH - momentosS$media) / mediaH)) / 12
  MAPEDesvio = sum (abs ((dpH - momentosS$dp)) / dpH) / 12
  MAPEFacAnual = sum (abs ((facAnualH - momentosS$facAnual) / facAnualH)) / entrada$lagAnual
  MAPEFacMensal = sum (abs ((facMensalH - momentosS$facMensal) / facMensalH)) / (entrada$lagMensal)*12
  somRes = momentosS$somRes
  
  final = list (media = MAPEMedia, dp = MAPEDesvio, facAnual = MAPEFacAnual, facMensal = MAPEFacMensal, somRes = somRes)
  return (final)
}

lagAnualSignificativo = function (serie) {
  intConfianca = 1.96 / sqrt (length (serie) / 12)
  lagAnual = 12
  while (lagAnual < (length (serie) / 12)) {
    facAnual = correlogramaAnual (serie, lagAnual)[-1]
    lagAnualMin = sum (facAnual >= intConfianca)
    if (lagAnualMin == lagAnual)
      lagAnual = lagAnual + 12
    else
      return (lagAnualMin)
  }
}

lagMensalSignificativo = function (serie) {
  intConfianca = 1.96 / sqrt (length (serie) / 12)
  lagMensal = 12
  while (lagMensal < (length (serie))) {
    facMensal = correlograma (serie, lagMensal, F)[-1, ]
    facMensal = facMensal >= intConfianca
    lagMensalMin = apply (facMensal, 2, sum)
    lagMensalMin = min (lagMensalMin)
    if (lagMensalMin == lagMensal)
      lagMensal = lagMensal + 12
    else
      return (lagMensalMin)
  }
}

estouro = function (momentos) {
  if ((min (is.finite (momentos$media))) &&
      (min (is.finite (momentos$dp))) &&
      (min (is.finite (momentos$facAnual))) &&
      (min (is.finite (momentos$facMensal))) &&
      (min (is.finite (momentos$somRes))))
    return (FALSE)
  
  return (TRUE)
}