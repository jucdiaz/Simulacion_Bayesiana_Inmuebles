###### funciones Utiles

arregla_min_max_igual <- function(row){
  if(row[5] >= row[7]){
    row[5] <- row[5] - ((row[6] - 0) / 2)
    row[7] <- row[7] + ((row[6] - 0) / 2)
  }
  return(row)
}

#################

n_equ <- function(x, alpha = 0.05){
  n1 <- NULL
  for(i in 1:nrow(x)){
    if(x$maximo[i] > x$minimo[i]){
      n <- round((4*qnorm(alpha)^2*x$medio[i]*(1 - x$medio[i]))/(x$maximo[i] - x$minimo[i])^2)
      n1 <- rbind(n1,n)
    }
  }
  return(n1)
}

###################

ajuste.beta <- function(teta,valores= valores){
  alfa<-teta[1]
  beta<-teta[2]
  cuantil0.05<-valores[1]
  cuantil0.95<-valores[3]
  media<-valores[2]
  cuant1.teo<-qbeta(0.05,alfa,beta)
  cuant2.teo<-qbeta(0.95,alfa,beta)
  media.teo<-alfa/(alfa+beta)
  res<-(cuantil0.05-cuant1.teo)^2
  +(cuantil0.95-cuant2.teo)^2
  +(media-media.teo)^2
  return(res)
}

######################

par_betas <- function(x){
  alfas1 <- NULL
  for(i in 1:nrow(x)){
    valores <- x[i, c("minimo", "medio", "maximo")]
    alfas <- optim(c(1,1),ajuste.beta,method="L-BFGS-B",
                   lower=c(1,1)/1000000,upper=c(10,10),
                   valores = valores)[[1]]
    alfas1 <- rbind(alfas1,alfas)}
  return(alfas1)
}

#####################

simulador_base <- function(base, n_exp){
  base_simulada <- NULL
  for(i in 1:nrow(base)){
    pi_a <- rbeta(n = 1, base$alfa[i], base$beta[i])
    #print(paste0("El escenario ", as.character(base$ï..Numero.personas[i])," ",
    #as.character(base$area[i]), " con probabilidad de 1 de: ", as.character(pi_a)))
    ys <- sample(c(1,0), prob = c(pi_a, 1-pi_a), n_exp, replace = T)
    
    xs <- base[i,1:4]
    j = 1
    while(j <= (n_exp - 1)){
      xs <- rbind(xs, base[i,1:4])
      j = j + 1
    }
    
    base_aux <- as.data.frame(cbind(var_rpta = ys, xs))
    base_simulada <- as.data.frame(rbind(base_simulada, base_aux))
  }
  
  return(base_simulada)
}

#############################################


# metodologia propuesta
#
# Se leen los puntos de diseño dados por el experto

data <- read.table("Elicitacion_onboarding_v2.csv", sep = ';', header = TRUE)
head(data)

Niveles <- data[, c(1,3,4,5)]
Niveles<- Niveles[!duplicated(Niveles),]

minimo <- data[data$nivel == 'minimo', 7]
medio <- data[data$nivel == 'medio', 7]
maximo <- data[data$nivel == 'maximo', 7]

(puntos <- cbind(Niveles, minimo, medio, maximo))


# arreglo posibles errrores de la base
puntos <- as.data.frame(t(apply(puntos, 1, arregla_min_max_igual)))

# Calculo n equivalente
(n_exp <- min(n_equ(puntos, alpha = 0.01)))

# ajuste de la distribucion beta
parametros_beta <- matrix(unlist(par_betas(x = puntos)), ncol = 2, byrow = F)
colnames(parametros_beta) <- c("alfa","beta")
(puntos1 <- cbind(puntos,parametros_beta))

base_simulada_final <- NULL
for(i in 1:1000){
  base_aux <- simulador_base(puntos1, n_exp)
  base_simulada_final <- as.data.frame(rbind(base_simulada_final, base_aux))
  if(i == 1){
    write.table(base_aux,"Base_simulada_onboarding4.csv", sep = ";", row.names = FALSE)
  }else{
    write.table(base_aux,"Base_simulada_onboarding4.csv", append = TRUE, sep = ";", row.names = FALSE, col.names = FALSE)
  }
  print(paste0("Termina iteración: ", as.character(i)))
}

