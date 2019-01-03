entrada = function(dados) {
  
  leitura = read.table(dados, header = T, sep = ";", dec = ",")
  leitura = leitura[, -1]
  serieH = matrix(leitura, ncol = 12, byrow = T)
  nH = length(serieH)/12
  
  serieHN = log(serieH)
  mPerHN = apply(serieHN, 2, mean)
  dpPerHN = apply(serieHN, 2, sd)
  
  serieHN = t((t(serieHN) - mPerHN) / dpPerHN)
  
  final = list(serieH = serieH, serieHN = serieHN, mediaHN = mPerHN, dpHN = dpPerHN)
  return (final)
}