source ('sumQuadRes.R')
source ('cenarioSint.R')
source ('correlograma.R')

lagANUAL = 5
lagMENSAL = 3

momentos = function (serie, parametros, lags) {
  residuos = residuos (serie, parametros, lags)
  dpRes = residuos$dpRes
  serieSint = serieSint (parametros, dpRes, lags, 10000)
  
  media = apply(serieSint, 2, mean)
  dp = apply(serieSint, 2, sd)
  somRes = residuos$somRes
  facMensal = correlograma (serieSint, lagMENSAL, F)[-1, ]
  facAnual = correlogramaAnual (serieSint, lagANUAL)[-1]
  
  final = list (media = media, dp = dp, somRes = somRes, facMensal = facMensal, facAnual = facAnual)
  
  return (final)
}

avaliacao = function (serieHN, avaliacoesInd) {
  mediaH = apply (serieHN, 2, mean)
  dpH = apply (serieHN, 2, sd)
  facMensalH = correlograma (serieHN, lagMENSAL, F)[-1, ]
  facAnualH = correlogramaAnual (serieHN, lagANUAL)[-1]
  
  funcMedia = sum (1 / (1 + abs (avaliacoesInd$media))) / 12
  funcDesvio = sum (1 / (1 + abs (1 - avaliacoesInd$dp))) / 12
  funcFacAnual = sum (1 / (1 + abs (facAnualH - avaliacoesInd$facAnual))) / lagANUAL
  funcFacMensal = sum (1 / (1 + abs (facMensalH - avaliacoesInd$facMensal))) / (lagMENSAL*12)
  somRes = 1 / (1 + avaliacoesInd$somRes)
  
  final = list (media = funcMedia, dp = funcDesvio, facAnual = funcFacAnual, facMensal = funcFacMensal, somRes = somRes)
  
  return (final)
}
