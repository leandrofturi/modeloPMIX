entrada = function(dados) {
  
  #TRATAMENTO DOS DADOS DE ENTRADA
  
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

correlograma = function(serie, lagMax, grafico) {
  
  #CORRELOGRAMA DE UMA SERIE
  
  mPer = apply(serie, 2, mean)
  dpPer = apply(serie, 2, sd)
  serie = t((t(serie) - mPer))
  serieV = as.vector(t(serie))
  n = length(serie)/12
  intConfianca = 1.96 / sqrt(n)
  fac = matrix(1, nrow = lagMax+1, ncol = 12)
  
  if (lagMax > n) {
    print("Lag maior que a serie!")
    return();
  }
  
  for(lag in 1:lagMax) {
    serieLag = serieV[-(1:lag)] 
    serieAux = serieV[1:((n*12)-lag)]
    product = c(rep(0, lag), serieLag * serieAux)           
    serie = matrix(product, ncol = 12, byrow = T)
    
    for(mes in 1:12) {
      meslag = (mes - lag - 1) %% 12 + 1
      fac[(lag+1), mes] = sum(serie[, mes]) / ((n-1) * dpPer[mes] * dpPer[meslag])
    }
  }
  
  if(grafico) {
    plot(1, type = "n", xlab = "Período", xaxt="n", ylab = "Lag", xlim = c(1, 25), ylim = c(0, lagMax))
    axis(side = 1, at=c(2,4,6,8,10,12,14,16,18,20,22,24), 
         labels=c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez"))
    for(mes in 1:12) {
      segments((mes*2), 0, (mes*2), lagMax)
      segments((mes*2) + intConfianca, 0, (mes*2) + intConfianca, lagMax, lty = 2)
      segments((mes*2) - intConfianca, 0, (mes*2) - intConfianca, lagMax, lty = 2)
      for(lag in 0:lagMax) {
        segments((mes*2), lag, ((mes*2) + fac[(lag+1), mes]), lag)
      }
    }
  }
  return(fac)
}

residuos = function(serie, parametros, lags) {
  
  #CALCULO DOS RESIDUOS E DO DESVIO-PADRAO
  
  nTotal = length(serie)
  p = lags[1]
  q = lags[2]
  P = lags[3]
  Q = lags[4]
  lagMax = 12*(max(P, Q) + 1)
  
  serieV = numeric(nTotal)
  serieV = as.vector(t(serie))
  
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
  
  residuoV = numeric(nTotal)
  dpRes = numeric(12)
  
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
        autoMensal = autoMensal + phi[i, mes]*serieV[tlag]
        
        if (P > 0) {
          for(k in 1 : P){
            vtlag = (t - k*12) - i
            autoMensalAnual = autoMensalAnual + phi[i, mes]*PHI[k, mes]*serieV[vtlag]
          }
        }
      }
    }
    if (P > 0) {
      for(j in 1 : P){
        vlag = t - j*12
        autoAnual = autoAnual + PHI[j, mes]*serieV[vlag]
      }
    }
    if (q > 0) {
      for(l in 1 : q){
        tlag = t - l
        mmMensal = mmMensal + tht[l, mes]*residuoV[tlag]
        if (Q > 0) {
          for(n in 1 : Q){
            vtlag = (t - n*12) - l
            mmMensalAnual = mmMensalAnual + tht[l, mes]*THT[n, mes]*residuoV[vtlag]
          }
        }
      }
    }
    
    if (Q > 0) {
      for(m in 1 : Q){
        vlag = t - m*12
        mmAnual = mmAnual + THT[m, mes]*residuoV[vlag]
      }
    }
    residuoV[t] = serieV[t] - autoMensal - autoAnual + autoMensalAnual + mmMensal + mmAnual - mmMensalAnual
  }
  residuo = matrix(residuoV, ncol = 12, byrow = T)
  
  dpRes = apply(residuo, 2, sd)
  somQuadRes = sum(residuo^2)
  
  final = list(somRes = somQuadRes, dpRes = dpRes)
  return(final)
}

powell = function (serie, lags, Pinicial) {
  
  #ALGORITMO DE POWELL
  MAXITERACOES = 100
  eps = 10^-5
  
  p = lags[1]
  q = lags[2]
  P = lags[3]
  Q = lags[4]
  ordem = sum(lags)
  
  P0 = Pinicial
  passo = matrix(numeric(1), ncol = (ordem*12), nrow = (ordem*12))
  for (i in 1 : (ordem*12)) passo[i, i] = 1
  E0 = residuos(serie, P0, lags)$somRes
  Pi = P0
  Ei = E0
  
  for (ciclos in (1 : MAXITERACOES)) {
    Pdec = 1
    delta = 0
    print(E0)
    for (i in (1 : (ordem*12))) {
      Eiant = Ei
      busca = buscaLinear(serie, lags, Pi, passo[, i])
      Pi = busca$ponto
      Ei = busca$E
      
      if (Eiant - Ei > delta) {
        delta = Eiant - Ei
        Pdec = i
      }
    }
    if (abs(Ei - E0) < eps) {
      final = list(parametros = Pi, SomRes = Ei, ciclos = ciclos)
      return (final)
    }
    Pe = 2*Pi - P0
    Ee = residuos(serie, Pe, lags)$somRes
    if (Ee < E0) {
      if (2*(E0 - 2*Ei + Ee)*((E0 - Ei - delta)^2) < ((E0 - Ee)^2)*delta) {
        S = Pi - P0
        busca = buscaLinear(serie, lags, S, Pi)
        Ps = busca$ponto
        Es = busca$E
        if (Pdec < ordem*12) passo[, (Pdec:((ordem*12)-1))] = passo[, ((Pdec+1):(ordem*12))]
        passo[, (ordem*12)] = S
        Pi = Ps
        Ei = Es
      }
    }
    if (Ei > E0) {
      final = list(parametros = Pi, SomRes = Ei, ciclos = ciclos)
      return (final)
    }
    P0 = Pi
    E0 = Ei
  }
  final = list(parametros = Pi, SomRes = Ei, ciclos = ciclos)
  return (final)
}


secaoAurea = function (serie, lags, ponto, passo) {
  
  #METODO DA SECAO AUREA PARA OBTENCAO DE TRES PONTOS BONS DENTRO DO INTERVALO BUSCADO
  
  OURO = (sqrt(5) - 1) / 2
  OUROlim = 100
  
  a = -1
  b = 0
  
  novoPonto = ponto + (a * passo)
  Ea = residuos(serie, novoPonto, lags)$somRes
  novoPonto = ponto + (b * passo)
  Eb = residuos(serie, novoPonto, lags)$somRes
  intervalo = list(a = a, b = b, Ea = Ea, Eb = Eb)
  
  if (Eb > Ea) {
    a = intervalo$b
    b = intervalo$a
    Ea = intervalo$Eb
    Eb = intervalo$Ea
  }
  
  c = b + (1 + OURO)*(b - a)
  novoPonto = ponto + (c * passo)
  Ec = residuos(serie, novoPonto, lags)$somRes
  
  while (Eb > Ec) {
    finalAnt = list(a = a, Ea = Ea, b = b, Eb = Eb, c = c, Ec = Ec)
    for (ciclos in 1:1000) {
      r = (b - a)*(Eb - Ec)
      q = (b - c)*(Eb - Ea)
      if ((q - r) >= 0) div = 2 * (max(abs(q - r), 10^-20))
      else div = (-2) * (max(abs(q - r), 10^-20))
      u = b - ((b - c)*q - (b - a)*r) / div
      limu = b + OUROlim*(c - b)
      
      if ((b - u)*(u - c) > 0) {
        novoPonto = ponto + (u * passo)
        Eu = residuos(serie, novoPonto, lags)$somRes
        if (Eu < Ec) {
          a = b
          b = u
          Ea = Eb
          Eb = Eu
          final = list(a = a, Ea = Ea, b = b, Eb = Eb, c = c, Ec = Ec)
          return (final)
        }
        else if (Eu > Eb) {
          c = u
          Ec = Eu
          final = list(a = a, Ea = Ea, b = b, Eb = Eb, c = c, Ec = Ec)
          return (final)
        }
        u = c + (1 + OURO)*(c - b)
        novoPonto = ponto + (u * passo)
        Eu = residuos(serie, novoPonto, lags)$somRes
      }
      else if ((c - u)*(u - limu) > 0) {
        novoPonto = ponto + (u * passo)
        Eu = residuos(serie, novoPonto, lags)$somRes
        if (Eu < Ec) {
          b = c
          c = u
          u = c + (1 + OURO)*(c - b)
          Eb =  Ec
          Ec = Eu
          novoPonto = ponto + (u * passo)
          Eu = residuos(serie, novoPonto, lags)$somRes
        }
        else if ((u - limu)*(limu - c) >= 0) {
          u = limu
          novoPonto = ponto + (u * passo)
          Eu = residuos(serie, novoPonto, lags)$somRes
        }
        else {
          u = c + (1 + OURO)*(c - b)
          novoPonto = ponto + (u * passo)
          Eu = residuos(serie, novoPonto, lags)$somRes 
        }
        a = b
        b = c
        c = u
        Ea = Eb
        Eb = Ec
        Ec = Eu
      }
    }
    return (finalAnt)
  }
  final = list(a = a, Ea = Ea, b = b, Eb = Eb, c = c, Ec = Ec)
  return (final)
}

metBrent = function (serie, lags, ponto, passo, buscaDourada) {
  
  #METODO DE BRENT PARA OBTENCAO DO TAMANHO DO PASSO
  
  OURO = (sqrt(5) - 1) / 2
  MAXIT = 100
  ab = buscaDourada$a
  bb = buscaDourada$b
  Ebb = buscaDourada$Eb
  cb = buscaDourada$c
  if (ab < cb) {
    a = ab
    b = cb
  }
  else {
    a = cb
    b = ab
  }
  e = 0
  x = bb
  xmin = x
  w = bb
  v = bb
  Ex = Ebb
  Ew = Ebb
  Ev = Ebb
  
  for (iteracoes in 1:MAXIT) {
    xm = (a + b) / 2
    tol1 = 2*10^-6 * abs(x) + 10^-10
    tol2 = 2 * tol1
    if (abs(x - xm) <= (tol2 - 0.5*(b - a))) {
      xmin = x
      final = list(ponto = xmin, Ex = Ex)
      return (final)
    }
    if (abs(e) > tol1) {
      r = (x - w)*(Ex - Ev)
      q = (x - v)*(Ex - Ew)
      p = (x - v)*q - (x - w) * r
      q = 2*(q - r)
      if (q > 0) p = (-1) * p
      q = abs (q)
      etemp = e
      e = d
      
      if ((abs(p) > abs(0.5 * q * etemp)) || (p <= q*(a - x)) || (p >= q * (b - x))) {
        if (x >= xm) e = a - x
        else e = b - x
        d = (1 - OURO)*e
      }
      else {
        d = p/q
        u = x + d
        if (((u - a) < tol2) || ((b - u)) < tol2) {
          if ((x - xm) >= 0) d = tol1
          else d = (-1)*tol1
        }
      }
    }
    else {
      if (x >= xm) e = a - x
      else e = b - x
      d = (1 - OURO) * e
    }
    if (abs(d) >= tol1) u = x + d
    else {
      if (d >= 0) u = x + tol1
      else u = x - tol1
    }
    novoPonto = ponto + (u * passo)
    Eu = residuos(serie, novoPonto, lags)$somRes
    if (Eu <= Ex) {
      if (u >= x) a = x
      else b = x
      v = w
      w = x
      x = u
      Ev = Ew
      Ew = Ex
      Ex = Eu
    }
    else {
      if (u < x) a = u
      else b = u
      if ((Eu <= Ew) || (w == x)) {
        v = w
        w = u
        Ev = Ew
        Ew = Eu
      }
      else if ((Eu <= Ev) || (v == x) || (v == w)) {
        v = u
        Ev = Eu
      }
    }
  }
  print("Maximo de iteracoes atingidas")
  novoPonto = ponto + (xmin * passo)
  Ex = residuos(serie, novoPonto, lags)$somRes
  final = list(ponto = xmin, Ex = Ex)
  return (final)
}

buscaLinear = function (serie, lags, ponto, passo) {
  
  #FUNCAO GERAL PARA O CALCULO DO TAMANHO DO PASSO
  
  buscaDourada = secaoAurea(serie, lags, ponto, passo)
  metBrent = metBrent(serie, lags, ponto, passo, buscaDourada)
  
  passoMin = metBrent$ponto
  novoPonto = ponto + (passoMin*passo)
  Emin = metBrent$Ex
  final = list(ponto = novoPonto, E = Emin)
  return (final)
}

serieSint = function(parametros, dpRes, lags, n) {
  
  #GERACAO DA SERIE SINTETICA
  
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
  
  final = list(serieH = serieH, serieS = serieS, mediaH = mediaH, mediaS = mediaS, dpH = dpH, dpS = dpS, ciclos = ciclos)
  return (final)
}