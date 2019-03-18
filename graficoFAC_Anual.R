nPOPULACAO = 50

graficoFACs = function (serieH, p, lagMax) {
  series = leituraArquivos ( )
  inicializaGrafico (serieH, lagMax)
  cores = rainbow (nPOPULACAO)
  lapply (p, function (x)
             graficoFAC (series[[x]], lagMax, cores[x]))
  graficoFAC (serieH, lagMax, 'black')
}

leituraArquivos = function ( ) {
  p = 1:nPOPULACAO
  nomes = sapply (p, function (x)
    paste0 ("serie_", x, ".csv"))
  series = lapply (p, function (x)
    read.csv (nomes[x], header = TRUE, sep = ";", dec = ","))
  series = lapply (p, function (x)
    as.matrix (series[[x]][-1]))
  return (series)
}

inicializaGrafico = function (serieH, lagMax) {
  par (lwd = 1, col= 'black')
  plot (NA, main = "Autocorrelacoes anuais", xlim = c (1, lagMax), ylim = c (-1, 1), xlab = "", ylab = "", axes = F, type = "n")
  axis (1, 1:lagMax, 1:lagMax)
  axis (2, -1:1, c (-1, 0, 1))
  par (lty = 1)
  lines (1:12, rep(0, 12))
  intConfianca = 1.96 / sqrt (length (serieH) / 12)
  par (lty = 2)
  lines (1:12, rep(intConfianca, 12))
  lines (1:12, rep(-intConfianca, 12))
  box ()
}

graficoFAC = function (serie, lagMax, cor) {
  fac = correlogramaAnual (serie, lagMax)[-1]
  par (col = cor, lty = 1)
  points (fac, type = "o", pch = 20)
}

correlogramaAnual = function (serie, lagMax) {
  serieAnual = apply (serie, 1, sum)
  facAnual = acf (serieAnual, lag.max = lagMax, type = c("correlation"), plot = F, na.action = na.pass)
  facAnual = as.numeric (facAnual$acf)
  
  return (facAnual)
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