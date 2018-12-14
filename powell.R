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
  
  MAXITERACOES = 500
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

buscaLinear = function (serie, lags, ponto, passo) {
  
  #FUNCAO GERAL PARA O CALCULO DO TAMANHO DO PASSO
  
  buscaDourada = secaoAurea(serie, lags, ponto, passo)
  metBrent = metBrent(serie, lags, ponto, passo, buscaDourada)
  
  passoMin = metBrent$passo
  novoPonto = ponto + (passoMin*passo)
  Emin = metBrent$Ex
  final = list(ponto = novoPonto, E = Emin)
  return (final)
}

secaoAurea = function (serie, lags, ponto, passo) {
  
  #METODO DA SECAO AUREA PARA OBTENCAO DE TRES PONTOS DENTRO DO INTERVALO BUSCADO
  
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
      final = list(passo = xmin, Ex = Ex)
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
  final = list(passo = xmin, Ex = Ex)
  return (final)
}