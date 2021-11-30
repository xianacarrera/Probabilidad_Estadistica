#*****************************************
# Distribuciones en R
#*****************************************


# Funciones básicas
# d -> densidad
# p -> distribución
# q -> cuantil
# r -> números aleatorios

# Función cuantil

# Es la inversa (siempre que exista) de la función de distribución.

# A cada probabilidad le asigna el punto (x) cuya probabilidad acumulada (F(x)) es ese valor (esa probabilidad).

# Esto es, si F(x0) = p0, cuantil(p0) = x0.

# Función de distribución -> a cada punto le asigna una probabilidad.
# Función cuantil -> a cada probabilidad le asigna un punto.


#***********************************************
# Actividad 1
#***********************************************

#*********** a) Representación de funciones de densidad

# La normal vive (su probabilidad es significativamente mayor que 0) entre la media +- 3 o 4 desviaciones típicas

# Para abarcar a todas las normales del ejercicio, tomamos [-8,8]

x = seq(-8, 8, len=1000)
y1 = dnorm(x, mean=0, sd=1)     # Nótese que para la normal estándar también valdría dnorm(x)
y2 = dnorm(x, mean=0, sd=2)
y3 = dnorm(x, mean=2, sd=1)
y4 = dnorm(x, mean=-3, sd=0.5)

# type = "l" para unir los puntos con una línea
plot(x, y1, type="l", lwd=2, col=2, ylim=c(0, 1))

# Para no crear un nuevo gráfico, cambiamos plot por una función gráfica de segundo nivel -> lines
lines(x, y2, lwd=2, col=3)
lines(x, y3, lwd=2, col=4)
lines(x, y4, lwd=2, col=5)

# Como la 4ª es bastante picuda, ampliamos los límites en plot con ylim
# Nótese que la 4ª queda bastante rugosa por lo picuda que es. Podemos coger más puntos para suavizar.

legend("topright", legend=c("N(0,1)", "N(0,4)", "N(2,1)", "N(-3, 0.5^2)"),
       col=2:5, lwd=2, bty="n")
# bty = "n" quita la línea negra del cuadro de la leyenda


# Otra opción sería utilizar la función curve para pintar la función directamente
# Curve es de 1º nivel, pero se puede convertir en 2º nivel con add=T

curve(dnorm(x,mean=0, sd=1), xlim=c(-8,8), col=2)   # Aquí x puede ser cualquier cosa
curve(dnorm(x,mean=0, sd=2), xlim=c(-8,8), add=T, col=3)


# La función matplot permite representar las 4 funciones de golpe si agrupamos los datos en una matriz
# ****** Leer sobre matplot y probar
matplot(x, cbind(y1, y2, y3, y4), type="l", col=c(2,3,4,5), lty=1:5)
legend("topright", legend=c("N(0,1)", "N(0,4)", "N(2,1)", "N(-3, 0.5^2)"),
       col=2:5, lwd=2, lty =1:4, bty="n")



#******** b)

# P(Z < 1.52)
pnorm(1.52, mean=0, sd=1)

# P(Z > 2)
1-pnorm(2, mean=0, sd=1)

# P(-1 < Z < 2)
pnorm(2, mean=0, sd=1) - pnorm(-1, mean=0, sd=1)
pnorm(2) - pnorm(-1)


#******** c)

# P(Z <= z0) = 0.87
qnorm(0.87)

# P(Z > z0) = 0.05
qnorm(1-0.05)

# P(|Z| > z0) = 0.01
# Son las dos colas
qnorm(0.995)    
-qnorm(0.005)   # qnorm(0.005) me da -z0






#***********************************************
# Actividad 2
#***********************************************

# Una ji-cuadrado de m grados de libertad se construye sumando m normales estándar **independientes** al cuadrado
# X = Z1^2 + Z2^2 + ... + Zm^2

xx = seq(0, 20, len=1000)
yy1=dchisq(xx, df=3)   # 3 grados de libertad
yy2=dchisq(xx, df=5)
yy3=dchisq(xx, df=8)

plot(xx, yy1, type="l", lwd=2, col=2)
lines(xx, yy2, lwd=2, col=3)
lines(xx, yy3, lwd=2, col=4)


# expression permite escribir algunas expresiones matemáticas sencillas
# [] -> subíndice
# ^ -> superíndice
# También se pueden escribir letras griegas
legend(17, 0.25, legend=c(expression(chi[3]^2), 
                            expression(chi[5]^2), 
                            expression(chi[8]^2)),
       col=2:5, lwd=2, bty="n")



#*********** b)

# P(chi^2_5 < 6)
pchisq(6, df=5)

# P(chi^2_5 > 2)
1-pchisq(2, df=5)

# P(1 < chi^2_5 < 7)
pchisq(7, df=5) - pchisq(1, df=5)



#************ c)

# P(chi^2_5 <= x0) = 0.87
qchisq(0.87, df=5)

# P(chi^2_5 > x0) = 0.05
qchisq(1-0.05, df=5)


abline(v=qchisq(0.95, df=5))    # Recta vertical
abline(h=0)                     # Recta horizontal
