#*****************************************
# Repaso de las prácticas 1, 2 y 3
#*****************************************


#***********************************************
# Actividad 2
#***********************************************

# ******************* a)

# Para coger las dos normales, tomaremos una rejilla en [-4,20] (para dejar un poco de espacio)???

x=seq(-4, 20, len=1000)
y1=dnorm(x, mean=0, sd=1)
y2=dnorm(x, mean=7, sd=2)
y3=dchisq(x, df=6)

plot(x, y1, type="l", lwd=2, col=2)
lines(x, y2, col=3, lwd=2)
lines(x, y3, col=3, lwd=2)

# Nótese que R utiliza una ligera alteración: representa la ji-cuadrado para valores negativos,
# cuando en realidad ahí no está definida

legend("topright", legend=c("N(0,1)", "N(7,2^2)", expression(chi[6]^2)),
       col=2:4, lwd=2, bty="n")



# ******************** b)

# P(Z <= 1.5)
pnorm(1.5, mean=0, sd=1)

# P(Z > 0.5)
1-pnorm(0.5, mean=0, sd=1)

# P(Z > -2)
pnorm(2, mean=0, sd=1) # Por simetría
# Ahora bien, R también admite valores negativos. Podemos hacerlo directamente
pnorm(-2, mean=0, sd=1)


# P(Z<=0)=0.57
dnorm(0.57, mean=0, sd=1)


# P(|Z|>z0)=0.1
# Tenemos 0.05 en cada cola.
# Nos queremos quedar con todo menos la cola de la derecha, porque ahí marcamos z0
qnorm(0.95, mean=0, sd=1)



#***********************************************
# Actividad 1
#***********************************************

dat <- read.table("decathlon.txt", header=T, dec=".", sep=" ")

# Para comprobar que está bien leído, debemos hacer:
head(dat)    # Vemos que está bien cargado
dim(dat)     # Vemos que la longitud de los datos es la indicada


# Para acceder a los datos de la primera variable:
# a) dat es un data.frame. Por tanto, podemos usar dat$
# b) Otra opción es verlo como una matriz

dat$Cien_m
dat[,1]

# Tercer individuo
dat[3,1]
# Con la sintaxis de data.frame sería dat$Cien_m[3]


#************* Vector de medias

# Aplicamos mean por columnas
apply(dat, 2, mean)

# Otra opción
colMeans(dat)


#************ Desviaciones típicas normales

# 2 detalles importantes:
# a) R calcula la cuasidesviación típica
# b) Hay que corregir usando la raíz cuadrada

n=dim(dat)
apply(dat, 2, sd) * sqrt((n-1)/n)


#************ Matriz de covarianza

cov(dat)        # Matriz simétrica y definida positiva


#************ Gráfico

par(mfrow=c(1,2))    # 2 ventanas


# Tenemos que centrar los datos -> restar la media
med = colMeans(dat)
xc = dat$Cien_m - med[1]
yc = dat$Salto_long - med[2]
plot(xc, yc, pch=16, main="Datos centrados", asp=1)
# asp = 1 para mantener los ejes



# Estandarizaicón multivariante
Sc = cov(dat)
auto=eigen(Sc); auto     # Contiene los autovalores de la matriz de covarianzas

# Se accede usando $,[] o usando [[]]
valores = auto$values
v = auto$vectors

# Ahora voy a construir la matriz sigma^(-1/2)
# sigma = matriz de autovectores * diag(autovalores) * matriz de autovectores traspuesta
# sigma^(-1/2) = matriz de autovectores * (diag(autovalores))^(-1/2) * matriz de autovectores traspuesta
Sc12 = v %*% diag(1/sqrt(valores)) %*% t(v)

# t(v) es la trapuesta de v

# No es que le esté dando la vuelta al producto, es por las dimensiones???
datestm = cbind(xc,yc)%*%Sc12

# Usamos asp = 1 para mantener los ejes
plot(datestm, pch=16, main="Datos estandarizados multi", asp=1)

# Se mantiene la forma del diagrama de dispersión siempre y cuando mantengamos la proporcionalidad??????
# Con la uni sí y la multi no ??? Quéeee



par(mfrow=c(1,1))    # Volvemos a la ventana 1x1
  


