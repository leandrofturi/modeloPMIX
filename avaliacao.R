source ('sumQuadRes.R')
source ('cenarioSint.R')
source ('correlograma.R')

lagANUAL = 3
lagMENSAL = 5

momentos = function (serie, parametros, lags) {
  residuos = residuos (serie, parametros, lags)
  dpRes = residuos$dpRes
  serieSint = serieSint (parametros, dpRes, lags, 10000)
  
  media = apply(serieSint, 2, mean)
  dp = apply(serieSint, 2, sd)
  somRes = residuos$somRes
  facMensal = correlograma (serieSint, lagMENSAL, F)
  facAnual = correlogramaAnual (serieSint, lagANUAL)
  
  final = list (media = media, dp = dp, somRes = somRes, facMensal = facMensal, facAnual = facAnual)
  
  return (final)
}

multiObjetivo = function (dados, populacao, avaliacoes) {
  serieHN = entrada (dados)$serieHN
  mediaH = apply(serieHN, 2, mean)
  dpH = apply(serieHN, 2, sd)
  facMensalH = correlograma (serieHN, lagMENSAL, F)
  facAnualH = correlogramaAnual (serieHN, lagANUAL)
  
  medias = sapply(pop$avaliacao, function(x)
                                 1 / (1 + abs (x$media)))
  desvios = sapply(pop$avaliacao, function(x)
                                  2 / (1 + abs (x$dp)))
  facAnuais = sapply(pop$avaliacao, function(x)
                                    (1 + abs (facAnualH)) / (1 + abs (x$facAnual)))
  facMensais = sapply(pop$avaliacao, function(x)
                                     (1 + abs (facMensalH)) / (1 + abs (x$facMensal)))
  somRes = sapply(pop$avaliacao, function(x)
                                 1 / (1 + x$somRes))
  
  final = list (media = medias, dp = desvios, facAnual = facAnuais, facMensal = facMensais, somRes = somRes)
  
  return (final)
}

#sapply(pop$avaliacao, function(x) (x$media)) pega todos e coloca numa matriz
#pop$avalioacao[[1]]$media
