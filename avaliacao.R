source ('sumQuadRes.R')
source ('cenarioSint.R')
source ('correlograma.R')

LagANUAL = 1
LagMENSAL = 1

momentos = function (serie, parametros, lags) {
  residuos = residuos (serie, parametros, lags)
  dpRes = residuos$dpRes
  serieSint = serieSint (parametros, dpRes, lags, 10000)
  
  media = apply(serieSint, 2, mean)
  dp = apply(serieSint, 2, sd)
  somRes = residuos$somRes
  facMensal = correlograma (serieSint, LagMENSAL, F)[-1, ]
  facAnual = correlogramaAnual (serieSint, LagANUAL)[-1]
  
  final = list (media = media, dp = dp, somRes = somRes, facMensal = facMensal, facAnual = facAnual)
  
  return (final)
}

avaliacao = function (serieHN, avaliacoesInd) {
  #mediaH = apply (serieHN, 2, mean)
  #dpH = apply (serieHN, 2, sd)
  facMensalH = correlograma (serieHN, lagMENSAL, F)[-1, ]
  facAnualH = correlogramaAnual (serieHN, lagANUAL)[-1]
  
  #MAPEMedia = sum (abs (avaliacoesInd$media)) / 12
  #MAPEDesvio = sum (abs (avaliacoesInd$dp - 1)) / 12
  MAPEFacAnual = sum (abs ((facAnualH - avaliacoesInd$facAnual) / facAnualH)) / LagANUAL
  MAPEFacMensal = sum (abs ((facMensalH - avaliacoesInd$facMensal) / facMensalH)) / (LagMENSAL*12)
  somRes = avaliacoesInd$somRes
  
  final = list (facAnual = MAPEFacAnual, facMensal = MAPEFacMensal, somRes = somRes)
  
  return (final)
}

lagAnualSignificativo = function (serieHN) {
  intConfianca = 1.96 / sqrt (length (serieHN) / 12)
  LagANUAL <<- 12
  while (lagANUAL < (length (serieHN) / 12)) {
    facAnualH = correlogramaAnual (serieHN, LagANUAL)[-1]
    lagAnual = sum (facAnualH >= intConfianca)
    if (lagAnual == LagANUAL)
      LagANUAL <<- LagANUAL + 12
    else {
      LagANUAL <<- lagAnual
      return ()
    }
  }
}

lagMensalSignificativo = function (serieHN) {
  intConfianca = 1.96 / sqrt (length (serieHN) / 12)
  LagMENSAL <<- 12
  while (lagANUAL < (length (serieHN))) {
    facMensalH = correlograma (serieHN, LagMENSAL, F)[-1, ]
    facMensalH = facMensalH >= intConfianca
    lagMensal = apply (facAnualH, 2, sum)
    lagMensalMin = min (lagMensal)
    if (lagMensalMin == LagMENSAL)
      LagMENSAL <<- LagMENSAL + 12
    else {
      LagMENSAL <<- lagMensalMin
      return ()
    }
  }
}


