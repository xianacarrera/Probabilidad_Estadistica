#*********************************
# Tema 6
#*********************************

#*********** Ejemplo 3

# H0: sigma1^2 = sigma2^2
# Ha: sigma1^2 != sigma2^2

A = c(45.2, 48.3, 50.6, 43.3, 49.7, 46.6, 44.9, 42.0, 52.1, 48.3)
B = c(44.3, 46.9, 51.0, 47.5, 48.6, 47.2, 49.1)

varA = var(A); varA
varB = var(B); varB

est = varA/varB; est

# Para calcular el pvalor, primero tenemos que ver cuál de las colas 
# es más pequeña
n1=length(A); n1
n2=length(B); n2

# Calculo la probabilidad a la izquierda
pf(est, df1=n1-1, df2=n2-1)
# Me da 0.85, por lo que la cola más pequeña es la derecha

pval = 2*(1-pf(est, df1=n1-1, df2=n2-1)); pval

# pval=0.28 -> Aceptamos H0 
# Conclusión: No existen evidencias estadísticamente significativas
# de que las varianzas del contenido en sodio entre las botellas de 
# ambas empresas sea diferente.


# Forma automática
# ratio (3º argumento es el ratio hipotético entre las varianzas) por
# defecto vale 1.
var.test(A, B, alternative="two.sided")
          



#************ Actividad 4 - Ejemplo 4

# H0: p1 >= p2
# H1: p1 < p2

n1 = 200
n2 = 150
jov = 58
ad = 36

pgorro1 = jov/n1; pgorro1
pgorro2 = ad/n2; pgorro2

# Calculamos el pgorro conjunto
pgorro = (n1*pgorro1 + n2*pgorro2) / (n1+n2); pgorro

# Denominador del estadístico
error_tipico = sqrt(pgorro*(1-pgorro)*(1/n1+1/n2)); error_tipico

# Estadístico de contraste
est=(pgorro1 - pgorro2)/error_tipico; est

# p-valor
pnorm(est)

# Acepto H0
# No existen evidencias estadísticamente significativas 
# que avalen la teoría del granjero


# Automáticamente
# Pasamos los éxitos de las muestras como un vector
# Los tamaños muestrales los indicamos también como un vector
prop.test(c(jov, ad), c(n1, n2), alternative="less")
# Esta salida es bastante diferente a la nuestra por la corrección por continuidad
# que realiza R

# Sin corrrección por continuidad:
prop.test(c(jov, ad), c(n1, n2), alternative="less", correct=FALSE)
est^2    # X-squared que sale en la salida




