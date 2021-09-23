#*****************************************************************
# Actividad 1
#*****************************************************************

#************ Lectura de datos

# Para leer un archivo .txt -> read.table
# Argumentos:
# 1) Nombre del archivo, incluida su extensión, entre comillas
# 2) header (T/F) -> Indica si hay encabezado o no
# 3) sep -> ¿qué elemento/símbolo se utliza para separar las columnas?
# 4) dec -> ¿qué símbolo marca los decimales?

datos = read.table("familias.txt", header=T, sep=" ", dec=".")

# Cuando leemos datos, debemos hacer siempre un head
head(datos)


#************ Vector de medias

# El vector de medias tendrá 4 componentes, una por cada variable (las columnas)
# Utilizamos la función apply a modo de sustituto de un bucle
# 1 -> filas, 2 -> columnas, 3 -> profundidad (si tuviéramos 3 dim)
apply(datos, 2, mean)

# Otra opción -> utilizar la función colMeans
colMeans(datos)


#************ Matriz de covarianzas

# La covarianza puede tomar cualquier valor real, tanto positivo como negativo

cov(datos)

# Diagonal -> varianzas
# Fuera de la diagonal -> covarianzas entre pares

cov(datos[,1:2], datos[,3:4])   # También vale cov(datos[,c(1,2)],datos[,c(3,4)])


#*********** Matriz de correlaciones

cor(datos)

# Para redondear el resultado -> función round
# 5 o más -> unidad superior

round(cor(datos), 3)


# Sin usar la función cor:
# Hacemos un producto matricial donde la matriz de covarianzas la multiplicamos por
# izquierda y derecha por la misma matriz: una matriz diagonal con los inversos de las
# desviaciones típicas

# Obtengo las desviaciones típicas
# IMP -> R calcula la cuasivarianza y cuasidesviación típica (divide por n-1 en lugar de n)
n = dim(datos)[1]
v = apply(datos, 2, sd) * sqrt((n-1)/n); v

# Tenemos que corregir también cov(datos) para que en la diagonal tenga las varianzas y no las
# cuasivarianzas.
datos.corregidos = cov(datos)
diag(datos.corregidos) <- apply(datos, 2, var) * (n-1)/n; datos.corregidos

diag(1/v)%*%datos.corregidos%*%diag(1/v)




#*****************************************************************
# Actividad 2
#*****************************************************************

datos2 = read.table("hipertension.txt", header=T, sep=" ", dec=".")
head(datos2)
dim(datos2)

#***************** Vector de medias
med = colMeans(datos2)

#***************** Matriz de covarianzas
cov(datos2)

#***************** Diagramas de dispersión
plot(datos2$edad, datos2$PAM, pch=19)

# Figura apuntes
par(mfrow = c(2,2))   # 2 filas, 2 columnas. Importante resetear después

plot(datos2)
points(med[1], med[2], col="blue", pch=16)

par(mfrow = c(1,1))
