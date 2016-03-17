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

prueba_diffM_varDesc_dist(Ne_primavera,Ne_invierno,0.05)

###########c)
prueba_iguVar_mayor = function(x,x2,alpha) {
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
