#****************************************************
# Tema 6
#****************************************************

# Mercedes

#********************** Actividad 1
# Tema 6 - Ejemplo 1

# Muestras emparejadas
# La estrategia será calcular las diferencias y utilizarlas como una nueva variable.

# V = "tiempo de reacción visual"
# A = "tiempo de reacción auditiva"

# H0: muV <= muA
# Ha: muV > muA 

# Rechazamos para valores grandes del estadístico

# Introducimos los datos
V = c(166, 208, 240, 181, 206, 193, 233, 216, 196, 183, 164, 233, 198, 197, 216)
A = c(162, 212, 203, 166, 239, 202, 185, 170, 207, 198, 178, 142, 187, 164, 161)

# Estadístico de contraste
D = V-A; D

# Estadístico observado
n = length(D); n
Tobs = mean(D)/(sd(D)/sqrt(n)); Tobs


# p-valor
p.valor = pt(-Tobs, df=n-1)
# pt (y todas las funciones de este tipo) da siempre la cola izquierda por defecto
# Tenemos que coger el simétrico

# Rechazamos para 5% y para 10%, pero no para el 1%
# Evitar las frases de "significativo pero no mucho". No son matemáticamente precisas.

# Supongamos que hemos fijado alfa = 5%.
# En tal caso, existen evidencias estadísticamente significativas a favor de la hipótesis
# alternativa. Hemos demostrado que la media de las diferencias es mayor que 0, esto es, que el tiempo
# medio de reacción visual es mayor que el tiempo medio de reacción auditivo.


t.test(D, mu = 0, alternative = "greater")


# EN EL EXAMEN FINAL PUEDE HABER UNA SALIDA DE R Y QUE NOS PREGUNTEN SOBRE LA MISMA
# Por ejemplo, cuál es el tamaño de la muestra? -> df + 1  = 14 + 1 = 15
# ¿Cuál es el contraste que se está haciendo? -> Mayor que media = 0
# Mean of x es la media muestral


# t.test también nos permite hacer contrastes para la diferencia de medias de dos 
# variables normales
# Tengo que indicar si son muestras apareadas o independientes
t.test(V, A, paired = T, alternative="greater")

# Si en el examen pusiesen esta muestra, tendríamos que identificar que son muestras emparejadas,
# el tipo de contraste y decir su conclusión.
# En este caso, como estamos trabajando con la diferencia, pone "true difference in means".
# Con la salida de antes no hay forma de saber si las muestras son emparejadas. Aquí lo pone
# explícitamente.





#********************** Actividad 2
# Ejemplo 2

# Muestras independientes
# Siempre que tengamos un tamaño de muestra distinto, no pueden ser emparejadas
# Eso no quiere decir que mismo tamaño => emparejadas

# Como no nos dicen nada, nos ponemos en el contexto más general: 
# comparación de medias en muestras independientes con varianzas desconocidas y DISTINTAS.

# A="longitud de los peces del lago A" (cm)
# B="longitud de los peces del lago B" (cm)

# H0: muA <= muB
# Ha: muA > muB

A = c(12, 17, 8, 13, 16, 14, 18, 15, 16, 15)
B = c(12, 11, 15, 14, 16, 13, 15)

# Muestra del lago A
xbarra.A = mean(A); xbarra.A
Sc2.A = var(A); Sc2.A  # cuasivarianza muestral
Sc.A = sd(A); Sc.A     # cuasidesviación típica muestral
nA = length(A); nA

# Muestra del lago B
xbarra.B = mean(B); xbarra.B
Sc2.B = var(B); Sc2.B  # cuasivarianza muestral
Sc.B = sd(B); Sc.B     # cuasidesviación típica muestral
nB = length(B); nB


# Estadístico observado
Tobs = (xbarra.A - xbarra.B)/sqrt(Sc2.A/nA+Sc2.B/nB); Tobs

((Sc2.A/nA)+(Sc2.B/nB))^2/((Sc2.A/nA)^2/(nA-1)+(Sc2.B/nB)^2/(nB-1))
gamma = 15    # El entero más próximo


p.valor = 1 - pt(Tobs, df=gamma); p.valor

# Conclusión: Como el p-valor es mayor que los niveles de 
# significación habituales (10%, 5%, 1%), entonces para todos ellos
# vamos a aceptar la H0. Es decir, no tenemos evidencias estadísticamente
# significativas a favor de Ha y por tanto ASUMIMOS que la media de los peces
# del lago A es menor o igual que la longitud media de los peces del lago B.


# Función de R
t.test(A, B, paired = F, var.equal = F)
# x siempre será lo que pongamos en primer lugar, e y lo que pongamos en el segundo
# En realidad, lo que hace R es algo distinto a nuestro procedimiento
# Coge como gamma el valor 14.895 (ya que en sí el estadístico es una T aproximadamente).
# En el examen del año pasado se preguntó sobre esto, y habría que decir que tendríamos 
# que buscar en la tabla con el valor más cercano, 15.


# Cuidado! Si no pongo el mu que quiero contrastar, asume que es 0
# Y si no ponemos la alternativa, asume que es bilateral
# De hecho, en versiones antiguas de R, si lo escribíamos mal, como "les" en lugar de "less", 
# cogía el valor por defecto ("two.sided").