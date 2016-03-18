#Ejercicio 1
############a)
prueba_M_varDesc_dist = function(x, m0,alpha) {
  n = length(x)
  media = mean(x)
  Sc = sd(x)
  (z = (media - m0)*sqrt(n)/Sc)
  (RR = qnorm(alpha/2,lower.tail = FALSE))
  return (c(z,RR))
}
(datos_aire = read.table("aire.txt", header = T))
Temp=factor(datos_aire[,2])
oxido_azufre=datos_aire[,9]
OxAz_otono=oxido_azufre[Temp=="otono"]

prueba_M_varDesc_dist(OxAz_otono,2,0.1)

###########b)
prueba_diffM_varDesc_dist = function(x,x2,alpha) {
  n = length(x)
  n2 = length(x2)
  media = mean(x)
  media2 = mean(x2)
  Scsq = var(x)
  Sc2sq = var(x2)
  z = (media - media2)/sqrt((Scsq/n)+(Sc2sq/n2))
  RR = qnorm(alpha/2,lower.tail = FALSE)
  return (c(z,RR))
}

Ne = datos_aire[,6]
Ne_primavera = Ne[Temp=="primavera"]
Ne_invierno = Ne[Temp == "invierno"]

(B=prueba_diffM_varDesc_dist(Ne_primavera,Ne_invierno,0.05))
p_valorB=(2*pnorm(B[1],lower.tail = FALSE))
###########c)
prueba_iguVar_mayor = function(x,x2,alpha) {
  n = length(x)
  n2 = length(x2)
  media = mean(x)
  media2 = mean(x2)
  Scsq = var(x)
  Sc2sq = var(x2)
  Sp=sqrt(((n-1)*Scsq + (n2-1)*Sc2sq)/(n+n2-2))
  z = (sqrt(Scsq) - sqrt(Sc2sq))/((Sp)*(sqrt((1/2*n)+(1/2*n2))))
  RR = qnorm(alpha,lower.tail = FALSE)
  return (c(z,RR))
}
O3 = datos_aire[,7]
O3_otono = O3[Temp=="otono"]
O3_verano = O3[Temp=="verano"]

(C = prueba_iguVar_mayor(O3_otono,O3_verano,0.05))
(p_valorC=(pnorm(C[1],lower.tail = FALSE)))
 ##Ejercicio 2
lambda=(sum(datos_accidentes[,1]*datos_accidentes[,2])/315)
(Pi = ((exp(-lambda))*(lambda^(datos_accidentes[,1])))/factorial(datos_accidentes[,1]))
n=315
(nPio=Pi*n)
(estadistico_chi = sum(((datos_accidentes[,2]-nPio)^2)/nPio))
(RR = qchisq(0.05,length(Pi)-2,lower.tail=FALSE))
