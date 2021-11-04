#*******************************************************
# Práctica de ordenador del Tema 5
#*******************************************************

#************ Actividad 3

# H_0: sigma^2 <= 0.045
# H_a: sigma^2 > 0.045

datos3 = c(0.69, 1.04, 0.39, 0.37, 0.64, 0.73, 0.69, 1.04, 0.83, 1.00, 0.19, 0.61, 0.42, 0.20, 0.79)
n = length(datos3)
sc2 = var(datos3)
sigma02 = 0.045
est = (n-1)*sc2/sigma02; est 

# p-valor
# Para tomar la cola izquierda hacemos 1 - la prob
1 - pchisq(est, df=n-1)

# Rechazamos H_0. Existen evidencias estadíssticamente significativas
# de que la varianza poblacional es mayor que 0.045. En efecto, podemos afirmar que
# sigma^2 es mayor que 0.045

library(TeachingDemos)
sigma.test(datos3, sigmasq=sigma02, alternative = "greater")



#************* Actividad 4

# H_0: p <= 0.4
# H_a: p > 0.4

exitos = 50
n = 100
p0= 0.4

pgorro = exitos/n
est = (pgorro - p0)/sqrt(p0*(1-p0)/n); est

pvalor = 1 - pnorm(est, mean=0, sd=1); pvalor
# (El p-valor es menor que alfa, que es 0.05 => rechazamos H_0)

# Rechazamos H_0. Existen evidencias estadísticamente significativas de que la proporción de pacientes
# curados es mayor que 0.4 y, por lo tanto, el medicamento es eficaz.

prop.test(exitos, n, p=p0, alternative="greater")
# En este test apreciamos una pequeña diferencia debido a una corrección por continuidad (al fin y 
# al cabo, estamos aproximando una binomial por una normal)

# Obtenemos el mismo resultado cambiando correct:
prop.test(exitos, n, p=p0, alternative="greater", correct=FALSE)

# El intervalo de confianza está dirigido hacia el lado de la hipótesis alternativa