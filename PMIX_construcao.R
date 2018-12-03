entrada = function(dados){
  
  #TRATAMENTO DOS DADOS DE ENTRADA
  leitura = read.table(dados, header = T, sep = ";", dec = ",")
  leitura = leitura[, -1]
  serieH = matrix(leitura, ncol = 12, byrow = T)
  nH = length(serieH)/12
  mPerH = apply(serieH, 2, mean)
  dpPerH = apply(serieH, 2, sd)
  varPerH = apply(serieH, 2, var)
  
  serieHN = log(serieH)
  
  serieHN = t((t(serieHN) - apply(serieHN, 2, mean)) / apply(serieHN, 2, sd))
  
  facHN = correlograma(serieHN, nH/4, F)
  
  return(serieHN)
}

correlograma = function(serie, lagMax, grafico){
  
  #CORRELOGRAMA DE UMA SERIE
  mPer = apply(serie, 2, mean)
  dpPer = apply(serie, 2, sd)
  serie = t((t(serie) - mPer))
  serieV = as.vector(t(serie))
  n = length(serie)/12
  intConfianca = 1.96 / sqrt(n)
  fac = matrix(1, nrow = lagMax+1, ncol = 12)
  
  if(lagMax > n){
    print("Lag maior que a serie!")
    return();
  }
  
  for(lag in 1:lagMax){
    serieLag = serieV[-(1:lag)] 
    serieAux = serieV[1:((n*12)-lag)]
    product = c(rep(0, lag), serieLag * serieAux)           
    serie = matrix(product, ncol = 12, byrow = T)
    
    for(mes in 1:12){
      meslag = (mes - lag - 1) %% 12 + 1
      fac[(lag+1), mes] = sum(serie[, mes]) / ((n-1) * dpPer[mes] * dpPer[meslag])
    }
  }
  
  if(grafico){
    plot(1, type = "n", xlab = "PERIODO", xaxt="n", ylab = "LAG", xlim = c(1, 25), ylim = c(0, lagMax))
    axis(side = 1, at=c(2,4,6,8,10,12,14,16,18,20,22,24), 
         labels=c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov", "Dez"))
    for(mes in 1:12){
      segments((mes*2), 0, (mes*2), lagMax)
      segments((mes*2) + intConfianca, 0, (mes*2) + intConfianca, lagMax, lty = 2)
      segments((mes*2) - intConfianca, 0, (mes*2) - intConfianca, lagMax, lty = 2)
      for(lag in 0:lagMax){
        segments((mes*2), lag, ((mes*2) + fac[(lag+1), mes]), lag)
      }
    }
  }
  return(fac)
}

residuos = function(serie, parametros, lags){
  nyrs = length(serie)/12
  p = lags[1]
  P = lags[2]
  q = lags[3]
  Q = lags[4]
  lagMax = max(p, P, q, Q)
  
  limInf = 1
  limSup = 12*p
  phi = matrix(parametros[limInf : limSup], ncol = 12, byrow = T)
  limInf = limSup + 1
  limSup = limInf + 12*q - 1
  tht = matrix(parametros[limInf : limSup], ncol = 12, byrow = T)
  limInf = 12*q + limInf
  limSup = limInf + 12*P - 1
  PHI = matrix(parametros[limInf : limSup], ncol = 12, byrow = T)
  limInf = 12*P + limInf
  limSup = limInf + 12*Q - 1
  THT = matrix(parametros[limInf : limSup], ncol = 12, byrow = T)
  
  residuo = matrix(0, ncol = 12, nrow = nyrs)
  dpRes = numeric(12)
  autoMensal = numeric(1)
  autoAnual = numeric(1)
  autoMensalAnual = numeric(1)
  mmMensal = numeric(1)
  mmAnual = numeric(1)
  mmMensalAnual = numeric(1)

  
  for(v in lagMax+1 : (nyrs - 1)){
    for(t in 1 : 12){
      autoMensal = 0
      autoAnual = 0
      autoMensalAnual = 0
      mmMensal = 0
      mmAnual = 0
      mmMensalAnual = 0
      
      if (p > 0) {
        for(i in 1 : p){
          tlag = (t - i - 1) %% 12 + 1
          autoMensal = autoMensal + phi[i, t] * serie[v, tlag]
            
          if (P > 0) {
            for(k in 1 : P){
                vlag = v - k
                autoMensalAnual = autoMensalAnual + phi[i, t] * PHI[k, t] * serie[vlag, tlag]
            }
          }
        }
      }
      if (P > 0) {
        for(j in 1 : P){
          vlag = v - j
          autoAnual = autoAnual + PHI[j, t] * serie[vlag, t]
        }
      }
      residuo[v, t] = serie[v, t] - autoMensal - autoAnual + autoMensalAnual
      
      if (q > 0) {
        for(l in 1 : q){
          tlag = (t - l - 1) %% 12 + 1
          mmMensal = mmMensal + tht[l, t] * residuo[v, tlag]
          
          if (Q > 0) {
            for(n in 1 : Q){
              vlag = v - n
              mmMensalAnual = mmMensalAnual + tht[l, t] * THT[n, t] * residuo[vlag, tlag]
            }
          }
        }
      }
      
      if (Q > 0) {
        for(m in 1 : Q){
          vlag = v - m
          mmAnual = mmAnual + THT[m, t] * residuo[vlag, t]
        }
      }
      residuo[v, t] = residuo[v, t] + mmMensal + mmAnual - mmMensalAnual
    }
  }
  dpRes = apply(residuo, 2, sd)
  somQuadRes = sum(residuo ^ 2)
  
  final = list(somRes = somQuadRes, dpRes = dpRes)
  return(final)
}


otimizacao = function(serie, lags){
  nyrs = length(serie)/12
  p = lags[1]
  P = lags[2]
  q = lags[3]
  Q = lags[4]
  ordem = sum(lags)
  
  passo = matrix(numeric(1), ncol = (ordem*12), nrow = (ordem*12))
  P0 = numeric(ordem*12)
  E0 = numeric(1)
  Pi = numeric(ordem*12)
  Eiant = numeric(1)
  Ei = numeric(1)
  Pdec = numeric(1)
  delta = numeric(1)
  Pe = numeric(ordem * 12)
  Ee = numeric(1)
  sp = numeric(ordem*12)
  
  difRes = 10
  ciclos = 0
  for (i in 1 : (ordem*12)) passo[i, i] = 1
  P0 = rep(0, (ordem * 12))
  P0 = c(rep(1, (p*12)), rep(0, (q*12)), rep(1, (P*12)), rep(0, (Q*12)))
  
  E0 = residuos(serie, P0, lags)$somRes
  
  while ((difRes > 10 ^-6) && (ciclos <= 100)) {
      delta = 0
    
    for (i in 1 : (ordem*12)) {
      if (i == 1) {
        Pi = P0
        Eiant = E0
      }
      else Eiant = Ei
      busca = buscaLinear(serie, lags, Pi, passo[, i])
      Pi = busca$ponto
      Ei = busca$E
      
      if ((abs(Ei - Eiant)) > delta) {
        delta = abs(Ei - Eiant)
        Pdec = i
      }
    }
    sp = Pi - P0
    Pe = 2*Pi - P0
    Ee = residuos(serie, Pe, lags)$somRes
    
    if (Ee < E0) {
      if  ((2*(E0 - 2*Ei + Ee)*((E0 - Ei - delta)^2)) < (((E0 - Ee)^2)*delta)) {
        if (Pdec < ordem*12)  passo[, (Pdec:((ordem*12)-1))] = passo[, ((Pdec+1):(ordem*12))]
        passo[, (ordem*12)] = sp
        busca = buscaLinear(serie, lags, Pi, sp)
        Pi = busca$ponto
        Ei = busca$E
      }
    }
    difRes = abs(Ei - E0)
    E0 = Ei
    P0 = Pi
    print(E0) #######################################################################
    
    ciclos = ciclos + 1
  }
  dpRes = residuos(serie, P0, lags)$dpRes
  final = list(ponto = P0, dpRes = dpRes)
  return (final)
}


secaoAurea = function (serie, lags, ponto, passo) {
  #OURO = (sqrt(5) - 1) / 2
  OURO = 5
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
  final = list(a = a, Ea = Ea, b = b, Eb = Eb, c = c, Ec = Ec)
  return (final)
}

metBrent = function (serie, lags, ponto, passo, buscaDourada) {
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
    tol1 = 2*10^-10 * abs(x) + 10^-10
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
  buscaDourada = secaoAurea(serie, lags, ponto, passo)
  metBrent = metBrent(serie, lags, ponto, passo, buscaDourada)
  
  passoMin = metBrent$ponto
  novoPonto = ponto + (passoMin*passo)
  Emin = metBrent$Ex
  final = list(ponto = novoPonto, E = Emin)
  return (final)
}

serieSint = function(serie, lags, n) {
  
  otimizados = otimizacao(serie, lags)
  parametros = otimizados$ponto
  dpRes = otimizados$dpRes
  #parametros = c(0.883429, 0.704449, 0.775853, 0.360193, 0.841805, 0.879542, 0.855069, 0.9339, 0.958456, 0.927165, 0.897832, 0.844401,
  #              0.337275, -0.17647, 0.146482, -0.361297, 0.006054, 0.122416, -0.256529, -0.189037, -0.403412, 0.442112,-0.316306, -0.27799,
  #               0.697471, -0.547037, -0.681417, 0.162175, -0.588748, 0.678744, 0.876987, 0.710564, 0.164374, -0.975155, 0.41605, 0.953006,
  #               0.835418, -1.325291, -0.445427, 0.021704, -0.73116, 0.431762, 0.651614, 0.568605, 0.144673, -1.364278, 0.861876, 1.332262)
  #dpRes = c(0.62, 0.53, 0.68, 0.8, 0.51, 0.36, 0.22, 0.14, 0.22, 0.2, 0.37, 0.36)
    
  p = lags[1]
  P = lags[2]
  q = lags[3]
  Q = lags[4]
  lagMax = max(p, P, q, Q)
  
  limInf = 1
  limSup = 12*p
  phi = matrix(parametros[limInf : limSup], ncol = 12, byrow = T)
  print(phi)
  limInf = limSup + 1
  limSup = limInf + 12*q - 1
  tht = matrix(parametros[limInf : limSup], ncol = 12, byrow = T)
  print(tht)
  limInf = 12*q + limInf
  limSup = limInf + 12*P - 1
  PHI = matrix(parametros[limInf : limSup], ncol = 12, byrow = T)
  print(PHI)
  limInf = 12*P + limInf
  limSup = limInf + 12*Q - 1
  THT = matrix(parametros[limInf : limSup], ncol = 12, byrow = T)
  print(THT)
  
  serieS = matrix(numeric(1), ncol = 12, nrow = n)
  dpH = numeric(12)
  dpS = numeric(12)
  mediaH = numeric(12)
  mediaS = numeric(12)
  
  residuoS = matrix(rnorm(n*12), ncol = 12, nrow = n)
  meanAleat = apply(residuoS, 2, mean)
  sdAleat = apply(residuoS, 2, sd)
  residuoS = t((t(residuoS) - meanAleat) / sdAleat)
  residuoS = t(t(residuoS) * dpRes)
  
  autoMensal = numeric(1)
  autoAnual = numeric(1)
  autoMensalAnual = numeric(1)
  mmMensal = numeric(1)
  mmAnual = numeric(1)
  mmMensalAnual = numeric(1)
  
  for(v in lagMax+1 : (n - 1)){
    for(t in 1 : 12){
      autoMensal = 0
      autoAnual = 0
      autoMensalAnual = 0
      mmMensal = 0
      mmAnual = 0
      mmMensalAnual = 0
      
      if (p > 0) {
        for(i in 1 : p){
          tlag = (t - i - 1) %% 12 + 1
          autoMensal = autoMensal + phi[i, t] * serieS[v, tlag]
          
          if (P > 0) {
            for(k in 1 : P){
              vlag = v - k
              autoMensalAnual = autoMensalAnual + phi[i, t] * PHI[k, t] * serieS[vlag, tlag]
            }
          }
        }
      }
      
      if (P > 0) {
        for(j in 1 : P){
          vlag = v - j
          autoAnual = autoAnual + PHI[j, t] * serieS[vlag, t]
        }
      }
      
      if (q > 0) {
        for(l in 1 : q){
          tlag = (t - l - 1) %% 12 + 1
          mmMensal = mmMensal + tht[l, t] * residuoS[v, tlag]
          
          if (Q > 0) {
            for(n in 1 : Q){
              vlag = v - n
              mmMensalAnual = mmMensalAnual + tht[l, t] * THT[n, t] * residuoS[vlag, tlag]
            }
          }
        }
      }
      
      if (Q > 0) {
        for(m in 1 : Q){
          vlag = v - m
          mmAnual = mmAnual + THT[m, t] * residuoS[vlag, t]
        }
      }
      
      serieS[v, t] = residuoS[v, t] + autoMensal + autoAnual - autoMensalAnual - mmMensal - mmAnual + mmMensalAnual
    }
  }
  
  dpH = apply(serie, 2, sd)
  dpS = apply(serieS, 2, sd)
  mediaH = apply(serie, 2, mean)
  mediaS = apply(serieS, 2, mean)
  
  final = list(dpH = dpH, dpS = dpS, mediaH = mediaH, mediaS = mediaS)
  return(final)
}
