volumeUtil = function (dados, Pregularizacao, M) {
  serie = leituraSerie (dados)
  nS = length (serie)
  
  if (M)
    periodo = rep (c (31,28,31,30,31,30,31,31,30,31,30,31), (nS/12))
  else
    periodo = rep (365, nS)
  
  periodo = periodo*24*60*60
  tempoAcumulado = sapply (1:nS, function (x)
                                 sum (periodo[1:x]))
  volumeAcumulado = (serie - (mean (serie)*Pregularizacao))*tempoAcumulado
  
  pico = NULL
  i = 2
  while (i < (nS-1)) {
    if ((volumeAcumulado[i] > volumeAcumulado[i-1]) && (volumeAcumulado[i] > volumeAcumulado[i+1])) {
      j = i+1
      while (j < (nS-1)) {
        if (((volumeAcumulado[j] > volumeAcumulado[j-1]) && (volumeAcumulado[j] > volumeAcumulado[j+1])) && 
            (volumeAcumulado[j] > volumeAcumulado[i])) {
          pico = c (pico, volumeAcumulado[i] - min (volumeAcumulado[i:j]))
          i = j
          j = nS
        }
        j = j+1
      }
    }
    i = i+1
  }
  
  final = list (picos = pico, picoMax = max (pico))
  return (final)
}

leituraSerie = function (dados) {
  leitura = read.table (dados, header = TRUE, sep = ";", dec = ",")
  leitura = leitura[, -1]
  serie = as.matrix (leitura)
    
  serie = as.vector (t (serie))
  return (serie)
}
