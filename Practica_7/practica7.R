#*******************************************************
# Práctica de ordenador del Tema 5
#*******************************************************


#*******************************************************
# Actividad 1
#*******************************************************

# X = "tiempo de permanencia postoperatoria" (días)
# X € N(mu, 7^2)

# H_0: mu >= 17
# H_1: mu < 17   (queremos demostrar que el tiempo de espera es menor que el que había)

# Nivel de significación. Como no nos lo dan, tomamos alfa = 0.05

# Estadístico de contraste -> T = (X_barra - mu_0) / (desv_tip/sqrt(n))€N(0,1)

# Región de rechazo -> valores muy alejados de la hipótesis nula (en este caso, muy pequeños)
# Cola izquierda (hasta -z_alfa)
# Región de rechazo = (-inf, -z_alfa] = (-inf, -1.65]

# Evaluación del estadístico en la muestra
# Deja de ser una variable aleatoria (T), para convertirse en un número determinístico (t)
# t = 

# p-valor
# P(T <= t) = P(Z < t) 
#       si < alfa => Rechazo H0
#       si > alfa => No rechazo H0


#**********************************************

# Nota: Recomendable escribir las hipótesis como comentarios

# H_0: mu >= 17
# H_1: mu < 17


mu0=17
sigma=7
datos=c(3, 5, 12, 7, 22, 6, 2, 18, 9, 8, 20, 15, 3, 36, 38, 43)


#****************** Cálculos "a mano"
n=length(datos);n
xbarra=mean(datos);xbarra
est=(xbarra-mu0)/(sigma/sqrt(n));est
nivel_critico=pnorm(est);nivel_critico

# Conclusión: No rechazo H_0. No existen evidencias estadísticamente significativas
# de que el protocolo sea efectivo. (No existen pruebas significativas suficientes
# para afirmar que el protocolo es efectivo).


#***************** Cálculos automatizados

# Cargamos la librería con la función z.test
library(TeachingDemos)
z.test(datos, stdev=sigma, mu=mu0, alternative="less")

# Alternative (H_a) puede tomar 3 valores: less/greater/two.sided
# z = -0.89286 es el valor del estadístico
# Hay varias posibilidades para el intervalo de confianza. El que nos da la función
# es tomando el 95% de probabilidad desde el extremo izquierdo (podríamos cogerlo
# desde la derecha de la gráfica, central), ya que este es el lado de la alternativa.




#*******************************************************
# Actividad 2
#*******************************************************

# Apuntes en hoja (28*10)


mu0 = 24
datos2=c(23.5, 24.6, 26.5, 23.0, 28.3, 27.9, 22.4, 22.8, 21.8, 24.7,
        23.4, 23.7, 25.3, 24.8, 26.0, 22.9, 25.7, 24.2, 22.1, 25.3)

#******** Cálculos "a mano"
n=length(datos2);n
xbarra=mean(datos2);xbarra
sc=sd(datos2);sc
est=(xbarra-mu0)/(sc/sqrt(n));est

#nivel_critico=pt(-est, df=n-1);nivel_critico
1-pt(est, df=n-1)       # Quizás mejor esta opción

# Conclusión: No rechazo H_0. No existen evidencias estadísticamente significativas
# de que la longitud axial media sea mayor que 24mm, por tanto asumimos que mu <= 24.


#******** Cálculos automáticos
t.test(datos2, mu=mu0, alternative="greater")
# Nos da un intervalo de confianza hacia el lado del la alternativa






