entrada = function (dados) {
  leitura = read.table (dados, header = T, sep = ";", dec = ",")
  leitura = leitura[, -1]
  serieH = matrix (leitura, ncol = 12, byrow = T)
  nH = length (serieH) / 12
  
  serieHL = log (serieH)
  mediaHL = apply (serieHL, 2, mean)
  dpHL = apply (serieHL, 2, sd)
  
  serieHN = t ((t (serieHL) - mediaHL) / dpHL)
  
  final = list (serieH = serieH, serieHN = serieHN, mediaHL = mediaHL, dpHL = dpHL)
  return (final)
}