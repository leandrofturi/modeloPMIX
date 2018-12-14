PMIX = function (dados, lags, n, seriePlot) {
  
  #FUNCAO GERAL DO MODELO PMIX
  
  numParametros = sum(lags)*12
  Pinicial = rep(0,numParametros)
  #Pinicial = c(rep(1,12),rep(0,12),rep(1,12),rep(0,12))
  
  entrada = entrada(dados)
  serieHN = entrada$serieHN
  serieH = entrada$serieH
  mediaH = apply(serieH, 2, mean)
  dpH = apply(serieH, 2, sd)
  mediaHN = entrada$mediaHN
  dpHN = entrada$dpHN
  facH = correlograma(serieHN, 12, F)
  intervalop = 1.96 / sqrt(length(serieH)/12)
  
  otimizacao = powell(serieHN, lags, Pinicial)
  parametros = otimizacao$parametros
  ciclos = otimizacao$ciclos
  somRes = otimizacao$somRes
  paramResiduos = residuos(serieHN, parametros, lags)
  dpRes = paramResiduos$dpRes
  ciclos = otimizacao$ciclos
  
  serieSN = serieSint(parametros, dpRes, lags, n)
  serieSN = t((t(serieSN) * dpHN) + mediaHN)
  serieS = exp(serieSN)
  mediaS = apply(serieS, 2, mean)
  dpS = apply(serieS, 2, sd)
  facS = correlograma(serieS, 12, F)
  
  p = lags[1]
  q = lags[2]
  P = lags[3]
  Q = lags[4]
  data = format(Sys.time(), "%F %Hh%M")
  data = paste0("PMIX (", p, ", ", q, ", ", P, ", ", Q, ") ", data)
  dir.create(file.path("./", data))
  
  limInf = 0
  limSup = 0
  if (p > 0) {
    limInf = 1
    limSup = 12*p
    phi = matrix(parametros[limInf : limSup], ncol = 12, byrow = T)
  }
  else {
    p = 1
    phi = matrix(rep(0,12), ncol = 12, byrow = T)
  }
  
  if (q > 0) {
    limInf = limSup + 1
    limSup = limInf + 12*q - 1
    tht = matrix(parametros[limInf : limSup], ncol = 12, byrow = T)
  }
  else {
    q = 1
    tht = matrix(rep(0,12), ncol = 12, byrow = T)
  }
  
  if (P > 0) {
    limInf = limSup + 1
    limSup = limInf + 12*P - 1
    PHI = matrix(parametros[limInf : limSup], ncol = 12, byrow = T)
  }
  else {
    P = 1
    PHI = matrix(rep(0,12), ncol = 12, byrow = T)
  }
  
  if (Q > 0) {
    limInf = limSup + 1
    limSup = limInf + 12*Q - 1
    THT = matrix(parametros[limInf : limSup], ncol = 12, byrow = T)
  }
  else {
    Q = 1
    THT = matrix(rep(0,12), ncol = 12, byrow = T)
  }
  
  
  #GERACAO DOS ARQUIVOS DE SAIDA
  
  meses = c("JANEIRO", "FEVEREIRO", "MARCO", "ABRIL", "MAIO", "JUNHO", "JULHO", "AGOSTO", "SETEMBRO", "OUTUBRO", "NOVEMBRO", "DEZEMBRO")
  facNome = paste("AUTOCOR_PER", (0:12))
  phiNome = paste("phi", (1:p))
  PHINome = paste("PHI", (1:P))
  thtNome = paste("tht", (1:q))
  THTNome = paste("THT", (1:Q))
  
  tabelaDadosH = data.frame(facH)
  rownames(tabelaDadosH) = c(facNome)
  colnames(tabelaDadosH) = meses
  write.csv2(tabelaDadosH, paste0(data, "/Autocorrelacao Historica.csv"))
  
  tabelaDadosS = data.frame(facS)
  rownames(tabelaDadosS) = c(facNome)
  colnames(tabelaDadosS) = meses
  write.csv2(tabelaDadosS, paste0(data, "/Autocorrelacao Sintetica.csv"))
  
  tabelaParametros = data.frame(mediaH, dpH, mediaS, dpS, t(phi), t(PHI), t(tht), t(THT), dpRes)
  rownames(tabelaParametros) = meses
  colnames(tabelaParametros) = c("MEDIA HIST", "SD HIST", "MEDIA SINT", "SD SINT", phiNome, PHINome, thtNome, THTNome, "dpRes")
  write.csv2(tabelaParametros, paste0(data, "/Parametros.csv"))
  
  tabelaserieS = data.frame(serieS)
  rownames(tabelaserieS) = c(1:n)
  colnames(tabelaserieS) = meses
  write.csv2(tabelaserieS, paste0(data, "/Serie Sintetica.csv"))
  
  if (seriePlot){
    par(lwd = 0.5, col = 'grey')
    plot(mediaS, col= 'red', xlim = c(1,12), ylim = c(0, max(serieS)),
         xlab = "Periodo (mensal)", ylab = "Vazao (m^3/s)", type = "n")
    
    for(ano in 1:n){
      points(1:12, serieS[ano, ])
    }
    par(col = 'black')
    legend("topright", c("Dados Historicos", "Dados Sinteticos"), bty = "n", col = c('Blue', 'Red'), pch = 19, cex=0.8)
    par(lwd = 2)
    lines(1:12, mediaH, col= 'blue')
    lines(1:12, mediaS, col= 'red')
    par(lwd = 0.5)
    lines(1:12, mediaH + dpH, col= 'blue', lty = 2)
    lines(1:12, mediaH - dpH, col= 'blue', lty = 2)
    lines(1:12, mediaS + dpS, col= 'red', lty = 2)
    lines(1:12, mediaS - dpS, col= 'red', lty = 2)
  }
  
  final = list(serieH = serieH, serieS = serieS, mediaH = mediaH, mediaS = mediaS, dpH = dpH, dpS = dpS, ciclos = ciclos)
  return (final)
}