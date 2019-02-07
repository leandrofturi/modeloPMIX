serieSint = function(parametros, dpRes, lags, n) {
  
  nTotal = 12*n
  p = lags[1]
  q = lags[2]
  P = lags[3]
  Q = lags[4]
  lagMax = 12*(max(P, Q) + 1)
  
  limInf = 0
  limSup = 0
  if (p > 0) {
    limInf = 1
    limSup = 12*p
    phi = matrix(parametros[limInf : limSup], ncol = 12, byrow = T)
  }
  if (q > 0) {
    limInf = limSup + 1
    limSup = limInf + 12*q - 1
    tht = matrix(parametros[limInf : limSup], ncol = 12, byrow = T)
  }
  if (P > 0) {
    limInf = limSup + 1
    limSup = limInf + 12*P - 1
    PHI = matrix(parametros[limInf : limSup], ncol = 12, byrow = T)
  }
  if (Q > 0) {
    limInf = limSup + 1
    limSup = limInf + 12*Q - 1
    THT = matrix(parametros[limInf : limSup], ncol = 12, byrow = T)
  }
  
  serieS = matrix(numeric(0), ncol = 12, nrow = n)
  serieSV = numeric(nTotal)
  
  residuoS = matrix(rnorm(nTotal), ncol = 12, nrow = n)
  meanAleat = apply(residuoS, 2, mean)
  sdAleat = apply(residuoS, 2, sd)
  residuoS = t((t(residuoS) - meanAleat) / sdAleat)
  residuoS = t(t(residuoS) * dpRes)
  residuoSV = numeric(nTotal)
  residuoSV = as.vector(t(residuoS))
  
  for(t in (lagMax : nTotal)) {
    mes = t %% 12
    if(mes == 0) mes = 12
    
    autoMensal = 0
    autoAnual = 0
    autoMensalAnual = 0
    mmMensal = 0
    mmAnual = 0
    mmMensalAnual = 0
    
    if (p > 0) {
      for(i in 1 : p){
        tlag = t - i
        autoMensal = autoMensal + phi[i, mes]*serieSV[tlag]
        
        if (P > 0) {
          for(k in 1 : P){
            vtlag = (t - k*12) - i
            autoMensalAnual = autoMensalAnual + phi[i, mes]*PHI[k, mes]*serieSV[vtlag]
          }
        }
      }
    }
    if (P > 0) {
      for(j in 1 : P){
        vlag = t - j*12
        autoAnual = autoAnual + PHI[j, mes]*serieSV[vlag]
      }
    }
    if (q > 0) {
      for(l in 1 : q){
        tlag = t - l
        mmMensal = mmMensal + tht[l, mes]*residuoSV[tlag]
        if (Q > 0) {
          for(n in 1 : Q){
            vtlag = (t - n*12) - l
            mmMensalAnual = mmMensalAnual + tht[l, mes]*THT[n, mes]*residuoSV[vtlag]
          }
        }
      }
    }
    
    if (Q > 0) {
      for(m in 1 : Q){
        vlag = t - m*12
        mmAnual = mmAnual + THT[m, mes]*residuoSV[vlag]
      }
    }
    serieSV[t] = autoMensal + autoAnual - autoMensalAnual - mmMensal - mmAnual + mmMensalAnual + residuoSV[t]
  }
  serieS = matrix(serieSV, ncol = 12, byrow = T)
  
  return(serieS)
}

seriePlot = function (serieH, serieS) {
  mediaH = apply (serieH, 2, mean)
  dpH = apply (serieH, 2, sd)
  mediaS = apply (serieS, 2, mean)
  dpS = apply (serieS, 2, sd)
  n = length (serieS) / 12
  
  par(lwd = 0.5, col = 'grey')
  plot(mediaS, col= 'red', xlim = c(1,12), ylim = c(0, max(serieS)),
       xlab = "Período (mensal)", ylab = "Vazão (m^3/s)", type = "n")
    
    for(ano in 1:n){
      points(1:12, serieS[ano, ])
    }
    par(col = 'black')
    legend("topright", c("Dados Históricos", "Dados Sintéticos"), bty = "n", col = c('Blue', 'Red'), pch = 19, cex=0.8)
    par(lwd = 2)
    lines(1:12, mediaH, col= 'blue')
    lines(1:12, mediaS, col= 'red')
    par(lwd = 0.5)
    lines(1:12, mediaH + dpH, col= 'blue', lty = 2)
    lines(1:12, mediaH - dpH, col= 'blue', lty = 2)
    lines(1:12, mediaS + dpS, col= 'red', lty = 2)
    lines(1:12, mediaS - dpS, col= 'red', lty = 2)
}