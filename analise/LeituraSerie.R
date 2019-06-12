leitura = function (fileSeries) {
  series = lapply (fileSeries, function (x)
                               read.csv2 (x, header = T, sep = ";", dec = ","))
  series = lapply (series, function (x)
                           x[, -1])
  series = lapply (series, as.matrix)
  return (series)
}

tabelaCorrelogramaMensal = function (serieH, serieS) {
  facH = autocorrelacaoMensal (serieH, 12)
  facS = autocorrelacaoMensal (serieS, 12)
  
  fac = matrix (0, ncol = 24, nrow = 12)
  i = 1
  for (m in (1:12)) {
    fac[, i] = facH[, m]
    fac[, i+1] = facS[, m]
    i = i + 1
  }
}