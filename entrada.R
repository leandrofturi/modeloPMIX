source ('correlograma.R')

entrada = function (dados) {
  leitura = read.table (dados, header = T, sep = ";", dec = ",")
  leitura = leitura[, -1]
  serieH = matrix (leitura, ncol = 12, byrow = T)
  mediaH = apply (serieH, 2, mean)
  dpH = apply (serieH, 2, sd)
  nH = length (serieH) / 12
  
  serieHL = log (serieH)
  mediaHL = apply (serieHL, 2, mean)
  dpHL = apply (serieHL, 2, sd)
  
  serieHN = t ((t (serieHL) - mediaHL) / dpHL)
  
  lagAnual = lagAnualSignificativo (serieH)
  lagMensal = lagMensalSignificativo (serieH)
  facAnualH = correlogramaAnual (serieH, lagAnual)[-1]
  facMensalH = correlograma (serieH, lagMensal, F)[-1, ]
  
  final = list (serieH = serieH, mediaH = mediaH, dpH = dpH, lagAnual = lagAnual, lagMensal = lagMensal, facAnualH = facAnualH, facMensalH = facMensalH,
                serieHN = serieHN, mediaHL = mediaHL, dpHL = dpHL)
  return (final)
}