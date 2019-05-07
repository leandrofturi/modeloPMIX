source ('modelo/correlograma.R')

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
  
  if (lagSIGNIFICATIVO) {
    lagANUAL <<- lagAnualSignificativo (serieH)
    lagMENSAL <<- lagMensalSignificativo (serieH)
  }
  facAnualH = correlogramaAnual (serieH, lagANUAL)[-1]
  facMensalH = correlograma (serieH, lagMENSAL, F)[-1, ]
  
  final = list (serieH = serieH, mediaH = mediaH, dpH = dpH, lagAnual = lagANUAL, lagMensal = lagMENSAL, facAnualH = facAnualH, facMensalH = facMensalH,
                serieHN = serieHN, mediaHL = mediaHL, dpHL = dpHL)
  return (final)
}