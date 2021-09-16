#*************************************************************
# Ejercicio 1
#*************************************************************

# X_1 = "Nº de caras en los dos primeros lanzamientos"
# X_2 = "Nº de caras en los tres lanzamientos"

# X = (X_1, X_2)
# soporte = {(0,0), (0,1), (1,1), (1,2), (2,2), (2,3)}

# P((X_1,X_2) = (0,0)) = 1/8


# Lo primero que deberíamos hacer al comenzar un script es elegir el working directory. Opciones:
# 1) setwd("C:/...")
# 2) CTRL + MAYUS + H
# 3) 3 puntos en panel inferior derecho.


#********************* a)

# Creamos dos vectores, uno para cada uno de los ejes
xx=c(0,0,1,1,2,2)
yy=c(0,1,1,2,2,3)

# Los representamos con plot
# Elegimos el tipo de punto con pch (8 -> asterisco)
plot(xx,yy, pch=8, xlab="Nº de caras en los 2 primeros lanzamientos",
     ylab="Nº de caras en 3 lanzamientos", main="Diagrama de dispersión")

# Ahora queremos añadir una etiqueta con la probabilidad a cada punto
# Usamos la función text pasándole las coordenadas de los puntos y las labels (en orden)
text(xx, yy+0.2, labels=c("1/8", "1/8", "1/4", "1/4", "1/8", "1/8"))

# Problema -> ha superpuesto los puntos con el texto
# Vamos a definir un nuevo vector con los puntos desplazados ligeramente a la derecha
# Los últimos están a la izquierda para que no se vayan del gráfico 
xxd = c(0.1, 0.1, 1.1, 1.1, 1.9, 1.9)


# Funciones de primer nivel -> crean una ventana gráfica nueva
# Funciones de segundo nivel -> sobreescriben en una ventana gráfica ya existente

text(xxd, yy, labels=c("1/8", "1/8", "1/4", "1/4", "1/8", "1/8"))



#********************* b)

# Creación de matrices. Opciones:
# 1) Crearla directamente con el comando matrix
# 2) Crear filas o columnas y pegarlas con rbind o cbind, respectivamente


conj=matrix(c(1/8, 0, 0, 1/8, 1/4, 0, 0, 1/4, 1/8, 0, 0, 1/8), nrow=3, ncol=4)
conj


#************* c) Marginales. 
# 1) Usar rowSums o colSums para sumar filas o columnas


marg_X1 = rowSums(conj); marg_X1
marg_X2 = colSums(conj); marg_X2

# Ahora lo pegamos a la matriz con rbind y cbind
rbind(cbind(conj,marg_X1), c(marg_X2, 1))
# Al vector marg_X2 le tenemos que añadir un elemento (el total) para que
# coincida en longitud con el resto


# 2) Usando addmargins directamente
addmargins(conj)




#************* d) Condicionadas

# Fundamentalmente, tenemos que dividir los valores de la matriz por la marginal correspondiente
# Para eso, construimos una matriz diagonal con los inversos de la marginal

# Para la condicionada de X1 a X2, multiplicamos por la derecha
# Para la condicionada de X2 a X1, tendríamos que multiplicar por la izquierda

cond_X1_a_X2=conj%*%diag(1/marg_X2)     # %*% para multiplicación matricial
cond_X1_a_X2




# Guardar con encoding UTF-8



