#********************************************
# Prática de repaso de los temas 4, 5 y 6
#********************************************


#************* Actividad 2

# Apartado a)

muestra =c(9.8, 10.4, 10.6, 9.6, 9.7, 9.9, 10.9, 11.1, 9.6, 10.2)
mean(muestra)       # media muestral


# Apartado b)

n = length(muestra)
sc2 = (n-1)/n*var(muestra); sc2    # Varianza


# Apartado c)

nivel = 0.94
alfa = 1 - nivel
sc = sd(muestra)
xbarra = mean(muestra)
t=qt(1 - alfa/2, df=n-1); t

ext_inf = xbarra - t*sc/sqrt(n)
ext_sup = xbarra + t*sc/sqrt(n)
int = c(ext_inf, ext_sup); int

t.test(muestra, conf.level=nivel)


# Apartado d)
muestra2 = c(7.4, 8.2, 7.8, 8.3, 8.0, 7.0, 8.3, 8.5, 7.1, 9.5, 9.4, 9.4)

nivel = 0.05

# H_0: mu1 <= mu2
# H_a: mu1 > mu2

xbarra.2 = mean(muestra2); xbarra.2
n2=length(muestra2); n2
sc12 = var(muestra); sc12
sc22 = var(muestra2); sc22

est = (xbarra - xbarra.2)/sqrt(sc12/n+sc22/n2); est


gamma = ((sc12/n)+(sc22/n2))^2/((sc12/n)^2/(n-1)+(sc22/n2)^2/(n2-1)); gamma

pvalor= 1-pt(est, df=gamma); pvalor

# Rechazamos H_0. Existen evidencias estadísticamente significativas
# a un nivel de significación del 5% de que la nueva sede es más rápida
  
t.test(muestra, muestra2, alternative = "greater", var.equal = FALSE, paired = FALSE)



# Cómo redondear en R
round(1.2); round(1.7)
ceiling(1.2); ceiling(1.7)
floor(1.2); floor(1.7)
