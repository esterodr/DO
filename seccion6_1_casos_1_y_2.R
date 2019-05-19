library(tidyverse)
library(partykit)
library(caret)

load("EPH_final.Rda")

# Función para calcular RMSE
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

names(actualizar_ingreso) <- c("Trimestre", "factor")
bases_eph <- bases_eph %>%
  left_join(actualizar_ingreso, by="Trimestre") %>%
  mutate(Ingreso = Ingreso*factor) %>%
  group_by(CODUSU, NRO_HOGAR, COMPONENTE) %>%
  filter(Trimestre==max(Trimestre)) %>%
  ungroup() %>%
  filter(!is.na(NIVEL_ED_p)) %>%
  select(-c(CODUSU, NRO_HOGAR, COMPONENTE, Trimestre, factor))


# Convierto a logaritmo
bases_eph$Ingreso <- log(bases_eph$Ingreso)


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

### Estimación
df <- bases_eph 
  ## Train y test sets
  set.seed(1)
  test_index <- createDataPartition(y = df$Ingreso, times = 1, p = 0.333, list = FALSE)
  train <- df[-test_index,]
  test <- df[test_index,]
  train2 <- train %>% select(-c(n_trabaj_hogar_np, Menores_10_np, Miembros_hogar_np))
  train3 <- train %>% select(-c(n_trabaj_hogar, Menores_10, Miembros_hogar))
  
  ## Estimaciones
  fit_p <- lm(Ingreso ~ ., data = train2)
  fit_np <- train3 %>% group_by(Genero, Nacimiento, n_trabaj_hogar_np, Menores_10_np, Miembros_hogar_np,
                                MAS_500, REGION, Resid_5_años, NIVEL_ED_p, Reg_Tenencia) %>%
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
      left_join(fit_np, by=c("Genero", "Nacimiento", "n_trabaj_hogar_np", "Menores_10_np", "Miembros_hogar_np",
                             "MAS_500", "REGION", "Resid_5_años", "NIVEL_ED_p", "Reg_Tenencia"))
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
  

rmse_comp <- data.frame(Observacion=c(1:200),
                        Parametrico=newlist[1:200],
                        NO_Parametrico=newlist[201:400],
                        N = rep(NA,200),
                        p = rep(NA,200))

rmse_comp %>% 
  gather(Metodo, Error_relativo, -c(Observacion, N, p)) %>%
  ggplot() +
  geom_boxplot(aes(x=Metodo, y=Error_relativo)) +
  ylab("RMSE / RMSE(RF)") +
  geom_hline(yintercept = 1) +
  theme_classic() +
  theme(text = element_text(size=20))

save.image(file="comparaciones_total.Rda")

rmse_comp$N <- rep(nrow(bases_eph),200)
rmse_comp$p <- rep(length(fit_p$coefficients),200)

save.image(file="comparaciones_total.Rda")


### Lo mismo con menos variables

bases_eph2 <- bases_eph %>%
     select(-c(MAS_500, Reg_Tenencia, Resid_5_años))

### Estimación
df <- bases_eph2 
## Train y test sets
set.seed(1)
test_index <- createDataPartition(y = df$Ingreso, times = 1, p = 0.333, list = FALSE)
train <- df[-test_index,]
test <- df[test_index,]
train2 <- train %>% select(-c(n_trabaj_hogar_np, Menores_10_np, Miembros_hogar_np))
train3 <- train %>% select(-c(n_trabaj_hogar, Menores_10, Miembros_hogar))

## Estimaciones
fit_p2 <- lm(Ingreso ~ ., data = train2)
fit_np2 <- train3 %>% group_by(Genero, Nacimiento, n_trabaj_hogar_np, Menores_10_np, Miembros_hogar_np,
                              REGION, NIVEL_ED_p) %>%
  summarise(y_hat_np = mean(Ingreso))
set.seed(1)
fit_rfc2 <- cforest(Ingreso ~ ., data = train2, ntree = 500, mtry = 3, control = ctree_control(mincriterion = .1))

# Bootstrapping
set.seed(1)
errores2 <- replicate(200, {
  indices <- sample(1:nrow(test), size=nrow(test), replace = TRUE)
  testb <- test[indices,]
  
  y_hat_p2 <- predict(fit_p2, newdata=testb)
  y_hat_np2 <- testb %>%
    left_join(fit_np2, by=c("Genero", "Nacimiento", "n_trabaj_hogar_np", "Menores_10_np", "Miembros_hogar_np",
                           "REGION", "NIVEL_ED_p"))
  m <- mean(y_hat_np2$Ingreso, na.rm = TRUE)
  y_hat_np2$y_hat_np[is.na(y_hat_np2$y_hat_np)] <- m
  y_hat_rf2 <- predict(fit_rfc2, newdata=testb)
  
  rmse1 <- RMSE(testb$Ingreso, y_hat_p2)
  rmse2 <- RMSE(y_hat_np2$Ingreso, y_hat_np2$y_hat_np)
  rmse3 <- RMSE(testb$Ingreso, y_hat_rf2)
  
  p_rfc2 <- rmse1/rmse3
  np_rfc2 <- rmse2/rmse3
  
  newlist2 <- list("param"=p_rfc2,
                  "noparam"=np_rfc2)
})

p2 <- unlist(errores2[1,])
np2 <- unlist(errores2[2,])

newlist2 <- c(p2,np2)

rmse_comp2 <- data.frame(Observacion=c(1:200),
                        Parametrico=newlist2[1:200],
                        NO_Parametrico=newlist2[201:400],
                        N = rep(NA,200),
                        p = rep(NA,200))

rmse_comp2 %>% 
  gather(Metodo, Error_relativo, -c(Observacion, N, p)) %>%
  ggplot() +
  geom_boxplot(aes(x=Metodo, y=Error_relativo)) +
  ylab("RMSE / RMSE(RF)") +
  geom_hline(yintercept = 1) +
  theme_classic() +
  theme(text = element_text(size=20))

rmse_comp2$N <- rep(nrow(bases_eph2),200)
rmse_comp2$p <- rep(length(fit_p2$coefficients),200)

rmse_comp <- rbind(rmse_comp, rmse_comp2)

save.image(file="comparaciones_total.Rda")

names(rmse_comp) <- c("Observacion", "Paramétrico", "No paramétrico", "N", "p")

rmse_comp %>% 
  filter(p==33) %>%
  gather(Metodo, Error_relativo, -c(Observacion, N, p)) %>%
  ggplot(aes(x=Metodo, y=Error_relativo, color=Metodo)) +
  geom_boxplot(color = "grey60", outlier.alpha = 0) +
  geom_point(size = 3, alpha = 0.15) +
  ylab("RMSE / RMSE(Bosque)") +
  xlab("") +
  coord_flip() +
  geom_hline(yintercept = 1, linetype="dotted") +
  theme_classic() +
  theme(text = element_text(size=40),
        legend.position = "none")

rmse_comp %>% 
  filter(p==20) %>%
  gather(Metodo, Error_relativo, -c(Observacion, N, p)) %>%
  ggplot(aes(x=Metodo, y=Error_relativo, color=Metodo)) +
  geom_boxplot(color = "grey60", outlier.alpha = 0) +
  geom_point(size = 3, alpha = 0.15) +
  ylab("RMSE / RMSE(Bosque)") +
  xlab("") +
  coord_flip() +
  geom_hline(yintercept = 1, linetype="dotted") +
  theme_classic() +
  theme(text = element_text(size=40),
        legend.position = "none")