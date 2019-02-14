source ('cenarioSint.R')
source ('sumQuadRes.R')
source ('correlograma.R')

momentos = function (series, parametros, lags) {
  residuos = residuos (series$serieHN, parametros, lags)
  dpRes = residuos$dpRes
  serieSint = serieSint (parametros, dpRes, lags, (length (series$serieH))/12)
  serieSint = t ((t (serieSint) * series$dpHL) + series$mediaHL)
  serieSint = exp (serieSint)
  
  media = apply (serieSint, 2, mean)
  dp = apply (serieSint, 2, sd)
  somRes = residuos$somRes
  lagAnual = lagAnualSignificativo (series$serieH)
  facAnual = correlogramaAnual (serieSint, lagAnual)[-1]
  lagMensal = lagMensalSignificativo (series$serieH)
  facMensal = correlograma (serieSint, lagMensal, F)[-1, ]
  
  final = list (media = media, dp = dp, somRes = somRes, facAnual = facAnual, facMensal = facMensal)
  return (final)
}

avaliacao = function (series, avaliacoesInd) {
  mediaH = apply (series$serieH, 2, mean)
  dpH = apply (series$serieH, 2, sd)
  lagAnual = length (avaliacoesInd$facAnual)
  lagMensal = length (avaliacoesInd$facMensal) / 12
  facAnualH = correlogramaAnual (series$serieH, lagAnual)[-1]
  facMensalH = correlograma (series$serieH, lagMensal, F)[-1, ]
  
  MAPEMedia = sum (abs ((mediaH - avaliacoesInd$media) / mediaH)) / 12
  MAPEDesvio = sum (abs ((dpH - avaliacoesInd$dp)) / dpH) / 12
  MAPEFacAnual = sum (abs ((facAnualH - avaliacoesInd$facAnual) / facAnualH)) / lagAnual
  MAPEFacMensal = sum (abs ((facMensalH - avaliacoesInd$facMensal) / facMensalH)) / (lagMensal*12)
  somRes = avaliacoesInd$somRes
  
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

estouro = function (momento) {
  if (max (is.na (momento) || is.nan (momento) || is.infinite (momento)))
    return (TRUE)
  else
    return (FALSE)
}