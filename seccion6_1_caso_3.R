library(tidyverse)
library(partykit)
library(caret)

load("EPH_final.Rda")
rm(actualizar_ingreso, ingreso_medio)

# Función para calcular RMSE
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# Convierto a logaritmo
bases_eph$Ingreso <- log(bases_eph$Ingreso)

# Reduzco la cantidad de variables
bases_eph <- bases_eph %>%
  select(-c(CODUSU, NRO_HOGAR, COMPONENTE, MAS_500, Reg_Tenencia, Resid_5_años, NIVEL_ED_p))

# Elimino la respuesta 9 en nacimiento
bases_eph$Nacimiento[bases_eph$Nacimiento=="9"] <- "1"
bases_eph$Nacimiento <- as.factor(as.numeric(bases_eph$Nacimiento))

# Divido las variables continuas en base a la mediana
m <- median(bases_eph$n_trabaj_hogar)
bases_eph$n_trabaj_hogar_np <- ifelse(bases_eph$n_trabaj_hogar<=m,0,1)
m <- median(bases_eph$Miembros_hogar)
bases_eph$Miembros_hogar_np <- ifelse(bases_eph$Miembros_hogar<=m,0,1)
m <- median(bases_eph$Menores_10)
bases_eph$Menores_10_np <- ifelse(bases_eph$Menores_10<=m,0,1)
bases_eph$n_trabaj_hogar_np <- as.factor(bases_eph$n_trabaj_hogar_np)
bases_eph$Miembros_hogar_np <- as.factor(bases_eph$Miembros_hogar_np)
bases_eph$Menores_10_np <- as.factor(bases_eph$Menores_10_np)

### Estimación GBA
rmses_gba <- sapply(1:10,function(i){
  df <- bases_eph %>% filter(Trimestre == i, REGION=="1") %>%
    select(-c(Trimestre, REGION))
  ## Train y test sets
  set.seed(1)
  test_index <- createDataPartition(y = df$Ingreso, times = 1, p = 0.333, list = FALSE)
  train <- df[-test_index,]
  test <- df[test_index,]
  train2 <- train %>% select(-c(n_trabaj_hogar_np, Menores_10_np, Miembros_hogar_np))
  train3 <- train %>% select(-c(n_trabaj_hogar, Menores_10, Miembros_hogar))
  
  ## Estimaciones
  fit_p <- lm(Ingreso ~ ., data = train2)
  fit_np <- train3 %>% group_by(Genero, Nacimiento, n_trabaj_hogar_np, Menores_10_np, Miembros_hogar_np) %>%
    summarise(y_hat_np = mean(Ingreso))
  set.seed(1)
  fit_rfc <- cforest(Ingreso ~ ., data = train2, ntree = 500, mtry = 3, control = ctree_control(mincriterion = .1))
  
  # Bootstrapping
  set.seed(1)
  errores <- replicate(200, {
    indices <- sample(1:nrow(test), size=nrow(test), replace = TRUE)
    testb <- test[indices,]
    
    y_hat_p <- predict(fit_p, newdata=testb)
    y_hat_np <- testb %>%
      left_join(fit_np, by=c("Genero", "Nacimiento", "n_trabaj_hogar_np", "Menores_10_np", "Miembros_hogar_np"))
    m <- mean(y_hat_np$Ingreso, na.rm = TRUE)
    y_hat_np$y_hat_np[is.na(y_hat_np$y_hat_np)] <- m
    y_hat_rf <- predict(fit_rfc, newdata=testb)
    
    rmse1 <- RMSE(testb$Ingreso, y_hat_p)
    rmse2 <- RMSE(y_hat_np$Ingreso, y_hat_np$y_hat_np)
    rmse3 <- RMSE(testb$Ingreso, y_hat_rf)
    
    p_rfc <- rmse1/rmse3
    np_rfc <- rmse2/rmse3
    
    newlist <- list("param"=p_rfc,
                    "noparam"=np_rfc)
  })
  
  p <- unlist(errores[1,])
  np <- unlist(errores[2,])
  
  newlist <- c(p,np)
  
  return(newlist)
})

rmse_comp <- data.frame(Region=rep("GBA",2000),
                        Trimestre=c(rep(1,200),rep(2,200),rep(3,200),rep(4,200),
                                    rep(5,200),rep(6,200),rep(7,200),rep(8,200),
                                    rep(9,200),rep(10,200)),
                        Observacion=rep(c(1:200),10),
                        Parametrico=c(rmses_gba[1:200,1],rmses_gba[1:200,2],rmses_gba[1:200,3],
                                      rmses_gba[1:200,4],rmses_gba[1:200,5],rmses_gba[1:200,6],
                                      rmses_gba[1:200,7],rmses_gba[1:200,8],rmses_gba[1:200,9],
                                      rmses_gba[1:200,10]),
                        NO_Parametrico=c(rmses_gba[201:400,1],rmses_gba[201:400,2],rmses_gba[201:400,3],
                                      rmses_gba[201:400,4],rmses_gba[201:400,5],rmses_gba[201:400,6],
                                      rmses_gba[201:400,7],rmses_gba[201:400,8],rmses_gba[201:400,9],
                                      rmses_gba[201:400,10]),
                        N = rep(NA,2000),
                        p = rep(NA,100))


### Estimación NOA
rmses_noa <- sapply(1:10,function(i){
  df <- bases_eph %>% filter(Trimestre == i, REGION=="40") %>%
    select(-c(Trimestre, REGION))
  ## Train y test sets
  set.seed(1)
  test_index <- createDataPartition(y = df$Ingreso, times = 1, p = 0.333, list = FALSE)
  train <- df[-test_index,]
  test <- df[test_index,]
  train2 <- train %>% select(-c(n_trabaj_hogar_np, Menores_10_np, Miembros_hogar_np))
  train3 <- train %>% select(-c(n_trabaj_hogar, Menores_10, Miembros_hogar))
  
  ## Estimaciones
  fit_p <- lm(Ingreso ~ ., data = train2)
  fit_np <- train3 %>% group_by(Genero, Nacimiento, n_trabaj_hogar_np, Menores_10_np, Miembros_hogar_np) %>%
    summarise(y_hat_np = mean(Ingreso))
  set.seed(1)
  fit_rfc <- cforest(Ingreso ~ ., data = train2, ntree = 500, mtry = 3, control = ctree_control(mincriterion = .1))
  
  # Bootstrapping
  set.seed(1)
  errores <- replicate(200, {
    indices <- sample(1:nrow(test), size=nrow(test), replace = TRUE)
    testb <- test[indices,]
    
    y_hat_p <- predict(fit_p, newdata=testb)
    y_hat_np <- testb %>%
      left_join(fit_np, by=c("Genero", "Nacimiento", "n_trabaj_hogar_np", "Menores_10_np", "Miembros_hogar_np"))
    m <- mean(y_hat_np$Ingreso, na.rm = TRUE)
    y_hat_np$y_hat_np[is.na(y_hat_np$y_hat_np)] <- m
    y_hat_rf <- predict(fit_rfc, newdata=testb)
    
    rmse1 <- RMSE(testb$Ingreso, y_hat_p)
    rmse2 <- RMSE(y_hat_np$Ingreso, y_hat_np$y_hat_np)
    rmse3 <- RMSE(testb$Ingreso, y_hat_rf)
    
    p_rfc <- rmse1/rmse3
    np_rfc <- rmse2/rmse3
    
    newlist <- list("param"=p_rfc,
                    "noparam"=np_rfc)
  })
  
  p <- unlist(errores[1,])
  np <- unlist(errores[2,])
  
  newlist <- c(p,np)
  
  return(newlist)
})

rmse_comp2 <- data.frame(Region=rep("NOA",2000),
                        Trimestre=c(rep(1,200),rep(2,200),rep(3,200),rep(4,200),
                                    rep(5,200),rep(6,200),rep(7,200),rep(8,200),
                                    rep(9,200),rep(10,200)),
                        Observacion=rep(c(1:200),10),
                        Parametrico=c(rmses_noa[1:200,1],rmses_noa[1:200,2],rmses_noa[1:200,3],
                                      rmses_noa[1:200,4],rmses_noa[1:200,5],rmses_noa[1:200,6],
                                      rmses_noa[1:200,7],rmses_noa[1:200,8],rmses_noa[1:200,9],
                                      rmses_noa[1:200,10]),
                        NO_Parametrico=c(rmses_noa[201:400,1],rmses_noa[201:400,2],rmses_noa[201:400,3],
                                         rmses_noa[201:400,4],rmses_noa[201:400,5],rmses_noa[201:400,6],
                                         rmses_noa[201:400,7],rmses_noa[201:400,8],rmses_noa[201:400,9],
                                         rmses_noa[201:400,10]),
                        N = rep(NA,2000),
                        p = rep(NA,100))

rmses <- rbind(rmse_comp,rmse_comp2)
rm(rmse_comp, rmse_comp2)

### Estimación NEA
rmses_nea <- sapply(1:10,function(i){
  df <- bases_eph %>% filter(Trimestre == i, REGION=="41") %>%
    select(-c(Trimestre, REGION))
  ## Train y test sets
  set.seed(1)
  test_index <- createDataPartition(y = df$Ingreso, times = 1, p = 0.333, list = FALSE)
  train <- df[-test_index,]
  test <- df[test_index,]
  train2 <- train %>% select(-c(n_trabaj_hogar_np, Menores_10_np, Miembros_hogar_np))
  train3 <- train %>% select(-c(n_trabaj_hogar, Menores_10, Miembros_hogar))
  
  ## Estimaciones
  fit_p <- lm(Ingreso ~ ., data = train2)
  fit_np <- train3 %>% group_by(Genero, Nacimiento, n_trabaj_hogar_np, Menores_10_np, Miembros_hogar_np) %>%
    summarise(y_hat_np = mean(Ingreso))
  set.seed(1)
  fit_rfc <- cforest(Ingreso ~ ., data = train2, ntree = 500, mtry = 3, control = ctree_control(mincriterion = .1))
  
  # Bootstrapping
  set.seed(1)
  errores <- replicate(200, {
    indices <- sample(1:nrow(test), size=nrow(test), replace = TRUE)
    testb <- test[indices,]
    
    y_hat_p <- predict(fit_p, newdata=testb)
    y_hat_np <- testb %>%
      left_join(fit_np, by=c("Genero", "Nacimiento", "n_trabaj_hogar_np", "Menores_10_np", "Miembros_hogar_np"))
    m <- mean(y_hat_np$Ingreso, na.rm = TRUE)
    y_hat_np$y_hat_np[is.na(y_hat_np$y_hat_np)] <- m
    y_hat_rf <- predict(fit_rfc, newdata=testb)
    
    rmse1 <- RMSE(testb$Ingreso, y_hat_p)
    rmse2 <- RMSE(y_hat_np$Ingreso, y_hat_np$y_hat_np)
    rmse3 <- RMSE(testb$Ingreso, y_hat_rf)
    
    p_rfc <- rmse1/rmse3
    np_rfc <- rmse2/rmse3
    
    newlist <- list("param"=p_rfc,
                    "noparam"=np_rfc)
  })
  
  p <- unlist(errores[1,])
  np <- unlist(errores[2,])
  
  newlist <- c(p,np)
  
  return(newlist)
})

rmse_comp <- data.frame(Region=rep("NEA",2000),
                         Trimestre=c(rep(1,200),rep(2,200),rep(3,200),rep(4,200),
                                     rep(5,200),rep(6,200),rep(7,200),rep(8,200),
                                     rep(9,200),rep(10,200)),
                         Observacion=rep(c(1:200),10),
                         Parametrico=c(rmses_nea[1:200,1],rmses_nea[1:200,2],rmses_nea[1:200,3],
                                       rmses_nea[1:200,4],rmses_nea[1:200,5],rmses_nea[1:200,6],
                                       rmses_nea[1:200,7],rmses_nea[1:200,8],rmses_nea[1:200,9],
                                       rmses_nea[1:200,10]),
                         NO_Parametrico=c(rmses_nea[201:400,1],rmses_nea[201:400,2],rmses_nea[201:400,3],
                                          rmses_nea[201:400,4],rmses_nea[201:400,5],rmses_nea[201:400,6],
                                          rmses_nea[201:400,7],rmses_nea[201:400,8],rmses_nea[201:400,9],
                                          rmses_nea[201:400,10]),
                         N = rep(NA,2000),
                         p = rep(NA,100))

rmses <- rbind(rmses,rmse_comp)


### Estimación Cuyo
rmses_cuyo <- sapply(1:10,function(i){
  df <- bases_eph %>% filter(Trimestre == i, REGION=="42") %>%
    select(-c(Trimestre, REGION))
  ## Train y test sets
  set.seed(1)
  test_index <- createDataPartition(y = df$Ingreso, times = 1, p = 0.333, list = FALSE)
  train <- df[-test_index,]
  test <- df[test_index,]
  train2 <- train %>% select(-c(n_trabaj_hogar_np, Menores_10_np, Miembros_hogar_np))
  train3 <- train %>% select(-c(n_trabaj_hogar, Menores_10, Miembros_hogar))
  
  ## Estimaciones
  fit_p <- lm(Ingreso ~ ., data = train2)
  fit_np <- train3 %>% group_by(Genero, Nacimiento, n_trabaj_hogar_np, Menores_10_np, Miembros_hogar_np) %>%
    summarise(y_hat_np = mean(Ingreso))
  set.seed(1)
  fit_rfc <- cforest(Ingreso ~ ., data = train2, ntree = 500, mtry = 3, control = ctree_control(mincriterion = .1))
  
  # Bootstrapping
  set.seed(1)
  errores <- replicate(200, {
    indices <- sample(1:nrow(test), size=nrow(test), replace = TRUE)
    testb <- test[indices,]
    
    y_hat_p <- predict(fit_p, newdata=testb)
    y_hat_np <- testb %>%
      left_join(fit_np, by=c("Genero", "Nacimiento", "n_trabaj_hogar_np", "Menores_10_np", "Miembros_hogar_np"))
    m <- mean(y_hat_np$Ingreso, na.rm = TRUE)
    y_hat_np$y_hat_np[is.na(y_hat_np$y_hat_np)] <- m
    y_hat_rf <- predict(fit_rfc, newdata=testb)
    
    rmse1 <- RMSE(testb$Ingreso, y_hat_p)
    rmse2 <- RMSE(y_hat_np$Ingreso, y_hat_np$y_hat_np)
    rmse3 <- RMSE(testb$Ingreso, y_hat_rf)
    
    p_rfc <- rmse1/rmse3
    np_rfc <- rmse2/rmse3
    
    newlist <- list("param"=p_rfc,
                    "noparam"=np_rfc)
  })
  
  p <- unlist(errores[1,])
  np <- unlist(errores[2,])
  
  newlist <- c(p,np)
  
  return(newlist)
})

rmse_comp <- data.frame(Region=rep("Cuyo",2000),
                        Trimestre=c(rep(1,200),rep(2,200),rep(3,200),rep(4,200),
                                    rep(5,200),rep(6,200),rep(7,200),rep(8,200),
                                    rep(9,200),rep(10,200)),
                        Observacion=rep(c(1:200),10),
                        Parametrico=c(rmses_cuyo[1:200,1],rmses_cuyo[1:200,2],rmses_cuyo[1:200,3],
                                      rmses_cuyo[1:200,4],rmses_cuyo[1:200,5],rmses_cuyo[1:200,6],
                                      rmses_cuyo[1:200,7],rmses_cuyo[1:200,8],rmses_cuyo[1:200,9],
                                      rmses_cuyo[1:200,10]),
                        NO_Parametrico=c(rmses_cuyo[201:400,1],rmses_cuyo[201:400,2],rmses_cuyo[201:400,3],
                                         rmses_cuyo[201:400,4],rmses_cuyo[201:400,5],rmses_cuyo[201:400,6],
                                         rmses_cuyo[201:400,7],rmses_cuyo[201:400,8],rmses_cuyo[201:400,9],
                                         rmses_cuyo[201:400,10]),
                        N = rep(NA,2000),
                        p = rep(NA,100))

rmses <- rbind(rmses,rmse_comp)

rm(rmse_comp)

### Estimación Pampeana
rmses_pamp <- sapply(1:10,function(i){
  df <- bases_eph %>% filter(Trimestre == i, REGION=="43") %>%
    select(-c(Trimestre, REGION))
  ## Train y test sets
  set.seed(1)
  test_index <- createDataPartition(y = df$Ingreso, times = 1, p = 0.333, list = FALSE)
  train <- df[-test_index,]
  test <- df[test_index,]
  train2 <- train %>% select(-c(n_trabaj_hogar_np, Menores_10_np, Miembros_hogar_np))
  train3 <- train %>% select(-c(n_trabaj_hogar, Menores_10, Miembros_hogar))
  
  ## Estimaciones
  fit_p <- lm(Ingreso ~ ., data = train2)
  fit_np <- train3 %>% group_by(Genero, Nacimiento, n_trabaj_hogar_np, Menores_10_np, Miembros_hogar_np) %>%
    summarise(y_hat_np = mean(Ingreso))
  set.seed(1)
  fit_rfc <- cforest(Ingreso ~ ., data = train2, ntree = 500, mtry = 3, control = ctree_control(mincriterion = .1))
  
  # Bootstrapping
  set.seed(1)
  errores <- replicate(200, {
    indices <- sample(1:nrow(test), size=nrow(test), replace = TRUE)
    testb <- test[indices,]
    
    y_hat_p <- predict(fit_p, newdata=testb)
    y_hat_np <- testb %>%
      left_join(fit_np, by=c("Genero", "Nacimiento", "n_trabaj_hogar_np", "Menores_10_np", "Miembros_hogar_np"))
    m <- mean(y_hat_np$Ingreso, na.rm = TRUE)
    y_hat_np$y_hat_np[is.na(y_hat_np$y_hat_np)] <- m
    y_hat_rf <- predict(fit_rfc, newdata=testb)
    
    rmse1 <- RMSE(testb$Ingreso, y_hat_p)
    rmse2 <- RMSE(y_hat_np$Ingreso, y_hat_np$y_hat_np)
    rmse3 <- RMSE(testb$Ingreso, y_hat_rf)
    
    p_rfc <- rmse1/rmse3
    np_rfc <- rmse2/rmse3
    
    newlist <- list("param"=p_rfc,
                    "noparam"=np_rfc)
  })
  
  p <- unlist(errores[1,])
  np <- unlist(errores[2,])
  
  newlist <- c(p,np)
  
  return(newlist)
})

rmse_comp <- data.frame(Region=rep("Pampeana",2000),
                        Trimestre=c(rep(1,200),rep(2,200),rep(3,200),rep(4,200),
                                    rep(5,200),rep(6,200),rep(7,200),rep(8,200),
                                    rep(9,200),rep(10,200)),
                        Observacion=rep(c(1:200),10),
                        Parametrico=c(rmses_pamp[1:200,1],rmses_pamp[1:200,2],rmses_pamp[1:200,3],
                                      rmses_pamp[1:200,4],rmses_pamp[1:200,5],rmses_pamp[1:200,6],
                                      rmses_pamp[1:200,7],rmses_pamp[1:200,8],rmses_pamp[1:200,9],
                                      rmses_pamp[1:200,10]),
                        NO_Parametrico=c(rmses_pamp[201:400,1],rmses_pamp[201:400,2],rmses_pamp[201:400,3],
                                         rmses_pamp[201:400,4],rmses_pamp[201:400,5],rmses_pamp[201:400,6],
                                         rmses_pamp[201:400,7],rmses_pamp[201:400,8],rmses_pamp[201:400,9],
                                         rmses_pamp[201:400,10]),
                        N = rep(NA,2000),
                        p = rep(NA,100))

rmses <- rbind(rmses,rmse_comp)

rm(rmse_comp)

### Estimación Patagonia
rmses_pata <- sapply(1:10,function(i){
  df <- bases_eph %>% filter(Trimestre == i, REGION=="44") %>%
    select(-c(Trimestre, REGION))
  ## Train y test sets
  set.seed(1)
  test_index <- createDataPartition(y = df$Ingreso, times = 1, p = 0.333, list = FALSE)
  train <- df[-test_index,]
  test <- df[test_index,]
  train2 <- train %>% select(-c(n_trabaj_hogar_np, Menores_10_np, Miembros_hogar_np))
  train3 <- train %>% select(-c(n_trabaj_hogar, Menores_10, Miembros_hogar))
  
  ## Estimaciones
  fit_p <- lm(Ingreso ~ ., data = train2)
  fit_np <- train3 %>% group_by(Genero, Nacimiento, n_trabaj_hogar_np, Menores_10_np, Miembros_hogar_np) %>%
    summarise(y_hat_np = mean(Ingreso))
  set.seed(1)
  fit_rfc <- cforest(Ingreso ~ ., data = train2, ntree = 500, mtry = 3, control = ctree_control(mincriterion = .1))
  
  # Bootstrapping
  set.seed(1)
  errores <- replicate(200, {
    indices <- sample(1:nrow(test), size=nrow(test), replace = TRUE)
    testb <- test[indices,]
    
    y_hat_p <- predict(fit_p, newdata=testb)
    y_hat_np <- testb %>%
      left_join(fit_np, by=c("Genero", "Nacimiento", "n_trabaj_hogar_np", "Menores_10_np", "Miembros_hogar_np"))
    m <- mean(y_hat_np$Ingreso, na.rm = TRUE)
    y_hat_np$y_hat_np[is.na(y_hat_np$y_hat_np)] <- m
    y_hat_rf <- predict(fit_rfc, newdata=testb)
    
    rmse1 <- RMSE(testb$Ingreso, y_hat_p)
    rmse2 <- RMSE(y_hat_np$Ingreso, y_hat_np$y_hat_np)
    rmse3 <- RMSE(testb$Ingreso, y_hat_rf)
    
    p_rfc <- rmse1/rmse3
    np_rfc <- rmse2/rmse3
    
    newlist <- list("param"=p_rfc,
                    "noparam"=np_rfc)
  })
  
  p <- unlist(errores[1,])
  np <- unlist(errores[2,])
  
  newlist <- c(p,np)
  
  return(newlist)
})

rmse_comp <- data.frame(Region=rep("Patagonia",2000),
                        Trimestre=c(rep(1,200),rep(2,200),rep(3,200),rep(4,200),
                                    rep(5,200),rep(6,200),rep(7,200),rep(8,200),
                                    rep(9,200),rep(10,200)),
                        Observacion=rep(c(1:200),10),
                        Parametrico=c(rmses_pata[1:200,1],rmses_pata[1:200,2],rmses_pata[1:200,3],
                                      rmses_pata[1:200,4],rmses_pata[1:200,5],rmses_pata[1:200,6],
                                      rmses_pata[1:200,7],rmses_pata[1:200,8],rmses_pata[1:200,9],
                                      rmses_pata[1:200,10]),
                        NO_Parametrico=c(rmses_pata[201:400,1],rmses_pata[201:400,2],rmses_pata[201:400,3],
                                         rmses_pata[201:400,4],rmses_pata[201:400,5],rmses_pata[201:400,6],
                                         rmses_pata[201:400,7],rmses_pata[201:400,8],rmses_pata[201:400,9],
                                         rmses_pata[201:400,10]),
                        N = rep(NA,2000),
                        p = rep(NA,100))

rmses <- rbind(rmses,rmse_comp)

rm(rmse_comp)

N <- bases_eph %>% group_by(REGION, Trimestre) %>% summarise(N=n()) %>% ungroup()
summary(N$N)

res_param <- rmses %>% group_by(Region, Trimestre) %>%
  filter(Parametrico>1) %>%
  summarise(porc = n()/200) %>%
  ungroup()

sum(res_param$porc==1)/60
sum(res_param$porc<1 & res_param$porc>=.75)/60
sum(res_param$porc<.75 & res_param$porc>=.5)/60
sum(res_param$porc<.5 & res_param$porc>=.25)/60
sum(res_param$porc<.25)/60

res_noparam <- rmses %>% group_by(Region, Trimestre) %>%
  filter(NO_Parametrico>1) %>%
  summarise(porc = n()/200) %>%
  ungroup() %>%
  arrange(desc(porc))

sum(res_noparam$porc==1)/60
sum(res_noparam$porc<1 & res_noparam$porc>=.75)/60
sum(res_noparam$porc<.75 & res_noparam$porc>=.5)/60
sum(res_noparam$porc<.5 & res_noparam$porc>=.25)/60
sum(res_noparam$porc<.25)/60

np <- rmses %>%
  select(-c(N,p))
np$Region <- as.character(np$Region)
names(N) <- c("Region", "Trimestre", "N")
N$Region <- as.character(N$Region)
N$Region[N$Region=="1"] <- "GBA"
N$Region[N$Region=="40"] <- "NOA"
N$Region[N$Region=="41"] <- "NEA"
N$Region[N$Region=="42"] <- "Cuyo"
N$Region[N$Region=="43"] <- "Pampeana"
N$Region[N$Region=="44"] <- "Patagonia"
np <- left_join(np, N, by=c("Region", "Trimestre"))

np$np <- np$N/9
np %>% select(-c(Region, Trimestre, Observacion, N)) %>%
  gather(Metodo, RMSE, -c(np)) %>%
  ggplot(aes(x=np, y=RMSE, color=Metodo)) +
  geom_point()

names(rmses) <- c("Region","Trimestre","Observacion","Paramétrico","No paramétrico","N","p")
rmses %>% filter(Region=="GBA")  %>%
  gather(Metodo, Error_relativo, -c(Region, Trimestre, Observacion, N, p)) %>%
  ggplot(aes(group=Trimestre)) +
  geom_boxplot(aes(x=Trimestre, y=Error_relativo)) +
  facet_wrap(.~Metodo) +
  ylab("RMSE / RMSE(Bosque)") +
  geom_hline(yintercept = 1, linetype="dotted") +
  theme_classic() +
  theme(text = element_text(size=20)) +
  scale_x_continuous(breaks = c(1:10))

rmses %>% filter(Region=="NOA")  %>%
  gather(Metodo, Error_relativo, -c(Region, Trimestre, Observacion, N, p)) %>%
  ggplot(aes(group=Trimestre)) +
  geom_boxplot(aes(x=Trimestre, y=Error_relativo)) +
  facet_wrap(.~Metodo) +
  ylab("RMSE / RMSE(Bosque)") +
  geom_hline(yintercept = 1, linetype="dotted") +
  theme_classic() +
  theme(text = element_text(size=20)) +
  scale_x_continuous(breaks = c(1:10))

rmses %>% filter(Region=="NEA")  %>%
  gather(Metodo, Error_relativo, -c(Region, Trimestre, Observacion, N, p)) %>%
  ggplot(aes(group=Trimestre)) +
  geom_boxplot(aes(x=Trimestre, y=Error_relativo)) +
  facet_wrap(.~Metodo) +
  ylab("RMSE / RMSE(Bosque)") +
  geom_hline(yintercept = 1, linetype="dotted") +
  theme_classic() +
  theme(text = element_text(size=20)) +
  scale_x_continuous(breaks = c(1:10))

rmses %>% filter(Region=="Cuyo")  %>%
  gather(Metodo, Error_relativo, -c(Region, Trimestre, Observacion, N, p)) %>%
  ggplot(aes(group=Trimestre)) +
  geom_boxplot(aes(x=Trimestre, y=Error_relativo)) +
  facet_wrap(.~Metodo) +
  ylab("RMSE / RMSE(Bosque)") +
  geom_hline(yintercept = 1, linetype="dotted") +
  theme_classic() +
  theme(text = element_text(size=20)) +
  scale_x_continuous(breaks = c(1:10))

rmses %>% filter(Region=="Pampeana")  %>%
  gather(Metodo, Error_relativo, -c(Region, Trimestre, Observacion, N, p)) %>%
  ggplot(aes(group=Trimestre)) +
  geom_boxplot(aes(x=Trimestre, y=Error_relativo)) +
  facet_wrap(.~Metodo) +
  ylab("RMSE / RMSE(Bosque)") +
  geom_hline(yintercept = 1, linetype="dotted") +
  theme_classic() +
  theme(text = element_text(size=20)) +
  scale_x_continuous(breaks = c(1:10))

rmses %>% filter(Region=="Patagonia")  %>%
  gather(Metodo, Error_relativo, -c(Region, Trimestre, Observacion, N, p)) %>%
  ggplot(aes(group=Trimestre)) +
  geom_boxplot(aes(x=Trimestre, y=Error_relativo)) +
  facet_wrap(.~Metodo) +
  ylab("RMSE / RMSE(Bosque)") +
  geom_hline(yintercept = 1, linetype="dotted") +
  theme_classic() +
  theme(text = element_text(size=20)) +
  scale_x_continuous(breaks = c(1:10))


