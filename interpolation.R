#--------------------------------
# RETO 2
# 
#--------------------------------
# Integrantes
# - Andres Camilo Giraldo Gil
# - Erika Alejandra Gonzalez
# - Leonel Steven Londono
#--------------------------------
# Analisis Numerico
#--------------------------------


#Instalacion Librerias Requeridas
#--------------------------------

#install.packages("bezier")
#install.packages("gridBezier")
#install.packages("PolynomF")
#install.packages("rgl")
#install.packages("plot3D")



#Incluir las librerias requeridas
#--------------------------------

require(bezier)
require(gridBezier)
require(PolynomF)
require(rgl)
require(plot3D)

#FUNCIONES
#--------------------------------


#Funcion que calcula los puntos que tendra que un cuadrante en todo el espacio
#--------------------------------
calcularProfundidadMortero <- function(puntos,zMax, dX, dY, dZ, longitud){
  
  finalDelCiclo <- FALSE
  
  auxX= puntos[,1]
  auxY= puntos[,2]
  auxZ= puntos[,3]
  
  nuevasCoordenadasX <- length(auxX) + 1
  nuevasCoordenadasY <- 1
  
  decrementoX <- dX
  decrementoY <- dY
  decrementoZ <- zMax - dZ
  
  
  while(!finalDelCiclo){
    
    if((auxX[nuevasCoordenadasY] - decrementoX) >= 0){
      auxX[nuevasCoordenadasX] <- auxX[nuevasCoordenadasY] - decrementoX
    }
    
    if((auxY[nuevasCoordenadasY] - decrementoY) >= 0){
      auxY[nuevasCoordenadasX] <- auxY[nuevasCoordenadasY] - decrementoY
    }else
    {
      auxY[nuevasCoordenadasX] = 0
      auxX[nuevasCoordenadasX] =  auxX[nuevasCoordenadasX - 1]
    }
    
    auxZ[nuevasCoordenadasX] = decrementoZ
    nuevasCoordenadasX = nuevasCoordenadasX + 1
    nuevasCoordenadasY = nuevasCoordenadasY + 1
    
    if(nuevasCoordenadasY == 101)
    {
      nuevasCoordenadasY = 1
      if(nuevasCoordenadasX >= longitud){
        decrementoZ = decrementoZ - 0.0000007
      }
      else if(nuevasCoordenadasX >= 19000){
        decrementoZ = decrementoZ - 0.006
      }
      else if(nuevasCoordenadasX >= 18000){
        decrementoZ = decrementoZ - 0.005
      }
      else if(nuevasCoordenadasX >= 17000){
        decrementoZ = decrementoZ - 0.004
      }
      else if(nuevasCoordenadasX >= 16000){
        decrementoZ = decrementoZ - 0.003
      }
      else if(nuevasCoordenadasX >= 15000){
        decrementoZ = decrementoZ - 0.002
      }
      else if(nuevasCoordenadasX < 15000){
        decrementoZ = decrementoZ - dZ
      }
      
      decrementoX = decrementoX + dX
      decrementoY = decrementoY + dY
    }
    
    if(nuevasCoordenadasX == 40000){
      finalDelCiclo <- TRUE
    }
    
  }
  
  nuevosPuntos <- rbind(auxX, auxY, auxZ)
  
  return (nuevosPuntos)
}

#Funcion que calcula los puntos de un mortero en base a otro usando splines
#--------------------------------
calcularPuntosNuevoMortero <- function(cuadrantePX, cuadrantePY, cuadrantepZ, dZ, zMax, longitud){
  finalDelCiclo <- FALSE
  nuevasCoordenadas <- 1
  morteroNuevoX <- c()
  morteroNuevoY <- c()
  morteroNuevoZ <- c()
  
  while(!finalDelCiclo){
    auxX <- cuadrantePX[nuevasCoordenadas:(nuevasCoordenadas+99)]
    auxY <- cuadrantePY[nuevasCoordenadas:(nuevasCoordenadas+99)]
    
    auxXX <- c(auxX[1])
    auxYY <- c(auxY[1])
    auxSegundoCiclo <- sample(4:10,1)
    
    contadorSegundoCiclo <- auxSegundoCiclo
    
    while(TRUE){
      if(contadorSegundoCiclo >=100){
        break;
      }
      auxXX <- c(auxXX, auxX[contadorSegundoCiclo])
      auxYY <- c(auxYY, auxY[contadorSegundoCiclo])
      contadorSegundoCiclo <- contadorSegundoCiclo + auxSegundoCiclo
    }
    
    auxXX <- c(auxXX, auxX[100])
    auxYY <- c(auxYY, auxY[100])
    
    splineMortero <- spline(auxXX, auxYY)
    
    morteroNuevoX <- c(morteroNuevoX, splineMortero$x)
    morteroNuevoY <- c(morteroNuevoY, splineMortero$y)
    auxZ <- 1
    while (auxZ <= length(splineMortero$x)){
      morteroNuevoZ <- c(morteroNuevoZ, zMax)
      #Creo que aqui van los if de los limites para que de la curvatura de aabajo pero no estoy segurdo
      auxZ <- auxZ + 1
    }
    zMax <- zMax - dZ
    nuevasCoordenadas <- nuevasCoordenadas + 100
    
    auxXX <- c()
    auxYY <- c()
    
    if((nuevasCoordenadas+99) > longitud){
      finalDelCiclo <- TRUE
    }
  }
  
  nuevoMortero <- rbind(morteroNuevoX, morteroNuevoY, morteroNuevoZ)
  
  return (nuevoMortero)
  
}


#Funcion que calcula el error de los puntos respecto 
#--------------------------------
calcularError <- function(zMax,zMin, cuadranteOriginal, cuadranteNuevo){
  
  finDelCiclo <- FALSE
  coordenadaZ <- zMax
  distanciasOriginales <- c()
  distanciasNuevas <- c()
  errorRelativo <- c()
  errorAbsoluto <- c()
  
  cat("\tDistancia del punto Original\t Distancia del punto Nuevo \t Error absoluto\t Error Relativo\n")
  
  cuadranteOriginalX <- cuadranteOriginal[1,]
  cuadranteOriginalY <- cuadranteOriginal[2,]
  cuadranteOriginalZ <- cuadranteOriginal[3,]
  
  cuadranteNuevoX <- cuadranteNuevo[1,] 
  cuadranteNuevoY <- cuadranteNuevo[2,]
  cuadranteNuevoZ <- cuadranteNuevo[3,]
  
  
  for (i in 1:length(cuadranteOriginalX)) {
    distanciasOriginales[i] <- sqrt((cuadranteOriginalX[i]^2)
                                    +(cuadranteOriginalY[i]^2)
                                    +(cuadranteOriginalZ[i]^2))
  }
  
  
  for(i in 1:length(cuadranteNuevoX)){
    distanciasNuevas[i] <- sqrt((cuadranteNuevoX[i]^2)
                                +(cuadranteNuevoY[i]^2)
                                +(cuadranteNuevoZ[i]^2))
  }
  
  
  for (i in distanciasNuevas) {
    errorAbsoluto[i] <-abs( distanciasOriginales[i] - distanciasNuevas[i] )
    errorRelativo[i] <- abs( distanciasOriginales[i] - distanciasNuevas[i] )/distanciasOriginales[i]
    cat("\t", distanciasOriginales[i], 
        "\t", distanciasNuevas[i], 
        "\t", errorAbsoluto[i], 
        "\t", errorRelativo[i],
        "\n")
  }

  
  retorno <- rbind(distanciasOriginales,distanciasNuevas, errorAbsoluto, errorRelativo)
  
  return(retorno)
    
}

#Funcion que grafica los cuadrantes que recibe
#--------------------------------
graficarFigura <- function(cuadrantePX, cuadrantePY, cuadrantePZ, cuadranteNX, cuadranteNY, color){
  
  plot3d(cuadrantePX,cuadrantePY, cuadrantePZ, type = "l", lwd = 10, col = color,
         xlab = "x", ylab="y", zlab="z", xlim = c(-10,10), ylim = c(-10,10) , zlim = c(0,5))
  
  plot3d(cuadranteNX,cuadrantePY, cuadrantePZ, type = "l", lwd = 10, col = color,
         xlab = "x", ylab="y", zlab="z", xlim = c(-10,10), ylim = c(-10,10) , zlim = c(0,5))
  
  plot3d(cuadranteNX, cuadranteNY, cuadrantePZ, type = "l", lwd = 10, col = color,
         xlab = "x", ylab="y", zlab="z", xlim = c(-10,10), ylim = c(-10,10) , zlim = c(0,5))
  
  plot3d(cuadrantePX,cuadranteNY, cuadrantePZ, type = "l", lwd = 10, col = color,
         xlab = "x", ylab="y", zlab="z", xlim = c(-10,10), ylim = c(-10,10) , zlim = c(0,5))
}


#Declaracion de puntos de control de un cuadrante
#--------------------------------

t <- seq(0,4, length=100)



coordenadasX <- c(0,0.99,4.78,8.91,9)
coordenadasY <- c(9,8.95,7.63,1.24,0)
coordenadasZ <- c(rep(4,5))

#Matriz con los puntos de control

p <- matrix(rbind(coordenadasX,coordenadasY,coordenadasZ), 
            nrow=5, ncol=3, byrow=TRUE)

puntosDeBezier <- bezier(t=t, p=p, deg =1)




#Se imprime la grafica del cuadrante vista desde arriba
#--------------------------------

xAux <- p[,1]
yAux <- p[,2]
p
plot(xAux, yAux, type = "l")

#Se calculan y grafican los cuadrantes del mortero
#--------------------------------

cuadrante <- calcularProfundidadMortero(puntosDeBezier, 4,0.02,0.02,0.01, 25000)

cuadrantePositivoX <- cuadrante[1,]
cuadrantePositivoY <- cuadrante[2,]
cuadrantePositivoZ <- cuadrante[3,]

cuadranteNegativoX=-1*cuadrantePositivoX
cuadranteNegativoY=-1*cuadrantePositivoY

graficarFigura(cuadrantePositivoX, cuadrantePositivoY,
               cuadrantePositivoZ,cuadranteNegativoX,
               cuadranteNegativoY, "blue")

#Se calcula y se grafica el grosor de las paredes del mortero
#--------------------------------

cuadrantePared <- calcularProfundidadMortero(puntosDeBezier, 4,0.002,0.002,0.001, 300)

cuadranteParedX <- cuadrantePared[1,]
cuadranteParedY <- cuadrantePared[2,]
cuadranteParedZ <- cuadrantePared[3,]

cuadranteParedNX=-1*cuadranteParedX
cuadranteParedNY=-1*cuadranteParedY

graficarFigura(cuadranteParedX, cuadranteParedY,
               cuadranteParedZ,cuadranteParedNX,
               cuadranteParedNY, "blue")

#Se calculan y grafican los cuadrantes del mortero interno
#--------------------------------

cuadranteInternoPositivoX <- cuadrante[1,]
cuadranteInternoPositivoY <- cuadrante[2,]
cuadranteInternoPositivoZ <- cuadrante[3,]

cuadranteInternoNegativoX=-1*cuadranteInternoPositivoX
cuadranteInternoNegativoY=-1*cuadranteInternoPositivoY



cuadranteInternoPositivoX <- cuadranteInternoPositivoX-1
cuadranteInternoPositivoY <- cuadranteInternoPositivoY-1

cuadranteInternoNegativoX <- cuadranteInternoNegativoX+1
cuadranteInternoNegativoY <- cuadranteInternoNegativoY+1


graficarFigura(cuadranteInternoPositivoX,cuadranteInternoPositivoY,
               cuadranteInternoPositivoZ,cuadranteInternoNegativoX,
               cuadranteInternoNegativoY, "gray")

#Se obtiene un nuevo mortero con los puntos del anterior y se grafica
#--------------------------------

cuadranteMorteroNuevo <- calcularPuntosNuevoMortero(cuadrante[1,],cuadrante[2,], cuadrante[3,],0.01, 4, 25000)

cuadranteMNX <- cuadranteMorteroNuevo[1,]
cuadranteMNY <- cuadranteMorteroNuevo[2,]
cuadranteMNZ <- cuadranteMorteroNuevo[3,]

cuadranteMNNX=-1*cuadranteMNX
cuadranteMNNY=-1*cuadranteMNY

graficarFigura(cuadranteMNX, cuadranteMNY,
               cuadranteMNZ,cuadranteMNNX,
               cuadranteMNNY, "green")

#Se calcula los errores en base a todos los puntos
#--------------------------------

distancias <- calcularError(4,1,cuadrante,cuadranteMorteroNuevo)

errorAbsolutoPromedio <- 0
errorRelativoPromedio <- 0
for(i in length(distancias[3,])){
  errorAbsolutoPromedio <- errorAbsolutoPromedio + distancias[3,i]
  errorRelativoPromedio <- errorRelativoPromedio + distancias[4,i]
}

errorAbsolutoPromedio <- errorAbsolutoPromedio/length(distancias[3,])
errorRelativoPromedio <- errorRelativoPromedio/length(distancias[4,])

cat("\n\n")
cat("Error Absoluto Promedio: ", errorAbsolutoPromedio, "\n")
cat("Error Relativo Promedio",errorRelativoPromedio,"\n")
