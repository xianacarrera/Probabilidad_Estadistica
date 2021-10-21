
#*********************************** PRACTICA TEMA 4 ***********************************************

#*********************************************
# Actividad 2
#*********************************************

#************ a)

# X = "Nº de latidos por minuto"
# X€N(mu, 10^2)

# Media con varianza conocida

datos = c(78, 95, 70, 97, 81, 85, 102, 75, 78, 85, 115, 80, 98, 101, 92)
n = length(datos)

xbarra = mean(datos)
sigma=10

nivel = 0.99
alfa = 1 - nivel
z=qnorm(1 - alfa/2); z

ext_inf = xbarra - z*sigma/sqrt(n)
ext_sup = xbarra + z*sigma/sqrt(n)
int = c(ext_inf, ext_sup); int

# En R existe una función que nos permite calcular automáticamente la media
# con varianza conocida
# Tenemos que descargar un paquete, TeachingDemos
# install.packages("TeachingDemos")
library(TeachingDemos)

# Pasamos datos, la varianza (no se toma la muestral) y el nivel de confianza
z.test(datos, stdev=sigma, conf.level = nivel)
# z es la evaluación del estadístico en la muestra




#*********************************************
# Actividad 3
#*********************************************

# X = "Peso de los niños" (g) 
# X € N(mu, sigma^2)

datos2 = c(2750, 3316, 3969, 2211, 2806, 4195, 3061, 3827, 3572, 3430)


#******** Nota -> Sobre Sc^2 y S^2
# La esperanza de Sc^2 es sigma^2         => estimador insesgado
#         ya que al calcular valores, vamos a estar centrados en la varianza
# La esperanza de S^2 es (n-1)*sigma^2/n  => estimador sesgado
#         ya que al calcular valores, estamos ligeramente desplazados


xbarra=mean(datos2);xbarra 
sc=sd(datos2);sc           

n = length(datos2)

nivel = 0.95
alfa = 1-nivel

# Calculamos el cuantil
t=qt(1-alfa/2, df=n-1)    # El número de grados de libertad es n-1
t

ext_inf = xbarra - t*sc/sqrt(n)
ext_sup = xbarra + t*sc/sqrt(n)
int = c(ext_inf, ext_sup); int


# Esta función sí está en un paquete normal, porque lo común es que no conozcamos
# ni media ni varianza
t.test(datos2, conf.level = nivel)


# Si nos dan esta salida y nos preguntan el tamaño muestral, basta ver el valor de df
# Como df = 9, el tamaño muestral será n = 9+1 = 10



#*********************************************
# Actividad 4
#*********************************************

# X = "nivel de hemoglobina en sangre" (g/dl)
# X € N(mu, sigma^2)

# Pivote € Ji-cuadrado de n-1 grados de libertad
# (Si la media es conocida, usamos una ji-cuadrado de n grados de libertad)

# Como la ji no es simétrica, hay que calcular ambos cuantiles

datos3 = c(15.6, 14.8, 14.4, 16.6, 13.8, 14.0, 17.3, 17.4, 18.6, 16.2, 14.7, 15.7, 16.4, 13.9, 14.8, 17.5)

nivel = 0.95
alfa = 1 - nivel

sc2=var(datos3)
chi1=qchisq(alfa/2, df=n-1)      # Cuantil grande
chi2=qchisq(1-alfa/2, df=n-1)    # Cuantil pequeño

ei = (n-1)*sc2/chi2
es = (n-1)*sc2/chi1
int3 = c(ei, es); int     # Intervalo de confianza para la varianza
sqrt(int3)                # Intervalo de confianza para la desviación típica


# En este caso también tenemos que cargar un paquete
library(TeachingDemos)
sigma.test(datos, conf.level = nivel)    # Varianza

# Desviación típica
sqrt(sigma.test(datos, conf.level = nivel)$conf.int)







