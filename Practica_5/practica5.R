#*********************************************
# Intervalo de confianza para una proporción
#*********************************************

# Un estadístico es cualquier función de la muestra (sumarle 3, hacerle la media...)
# Un estimador es un estadístico que tiene por objeto estimar un parámetro
# Todos los estimadores son estadísticos, pero el recíproco no es cierto

# El pivote es un estadístico (es combinación de estadísticos)
# El pivote se aproxima por una distribución normal estándar (en realidad sigue una binomial,
# pero lo aproximamos con una normal)

# La probabilidad que queda entre -z_alfa/2 y z_alfa/2 es 1-alfa, es decir,
# P (-z_alfa/2 < Z < z_alfa/2) = 1 - alfa 
# Sustituyo la variable Z por el pivote, que sigue una normal, y despejo




#*********************************************
# Tema 4 - Ejemplo 1
#*********************************************

# X = "¿Es la cápsula defectuosa?"
# X € Ber(p)

# n = tamaño muestral = 200
# éxitos = 9

# Estimación puntual de la proporción
# p_gorro = 9/200

# Intervalo de confianza para p
# (p_gorro - z_alfa*sqrt(...), p_gorro + z_alfa*sqrt(...))

n = 200
def = 9
pgorro = def/n; pgorro         # Proporción muestral
# El 4.5% de las cápsulas de la muestra son defectuosas


nivel = 0.95   # Nivel de confianza
alfa = 1 - nivel

# Quiero z_alfa/2. A su izquierda deja probabilidad alfa/2 + 1 - alfa = 1 - alfa/2
z = qnorm(1 - alfa/2); z    

ext_inf = pgorro - z*sqrt(pgorro*(1 - pgorro)/n); ext_inf
ext_sup = pgorro + z*sqrt(pgorro*(1 - pgorro)/n); ext_sup
int = c(ext_inf, ext_sup); int



# Podemos automatizar con prop.test (que, además, no aproxima con pgorro)
prop.test(x=def, n=n, conf.level = nivel)

# El intervalo que construimos en clase podría salirse del intervalo [0,1], mientras
# que el que construye R (el de R, llamado intervalo de Wilson, es más correcto)




#******************
t=z^2/n
ext_inf2=(pgorro+t/2)/(1+t) - (sqrt(pgorro*(1-pgorro)*t+t^2/4))/(1+t)
ext_sup2=(pgorro+t/2)/(1+t) + (sqrt(pgorro*(1-pgorro)*t+t^2/4))/(1+t)
int2 = c(ext_inf2, ext_sup2); int2


# El prop.test hace una corrección por continuidad. Cuando aproximamos una binomial por una normal,
# pasamos de una discreta a una continua. La discreta acumula las probabilidades en valores concretos,
# mientras que la normal los "expande" un poco. Por consiguiente, cuando buscamos una probabilidad,
# ampliamos intervalo por izquierda y/o derecha con 0.5. Podemos eliminar dicha corrección de
# prop.test:
prop.test(x=def, n=n, conf.level = nivel, correct = FALSE)


# La interpretación de un nivel de confianza es que, en un 95% de las ocasiones, p estará
# dentro del intervalo. En el 5% restante, no.

# shiny app