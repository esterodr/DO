load("EPH_final.Rda")
library(tidyverse)
library(party)
library(ineq)

rm(actualizar_ingreso, ingreso_medio)

region <- c("1", "40", "41", "42", "43", "44")

bases_eph <- bases_eph %>%
  select(-c(CODUSU, NRO_HOGAR, COMPONENTE, NIVEL_ED_p))

## GBA
base_gba <- bases_eph %>% filter(REGION==region[1])
#1
base_trim <- base_gba %>% filter(Trimestre==1) %>%
  select(-c(REGION, Trimestre)) 
fit_gba_1 <- ctree(Ingreso ~ ., data = base_trim, controls = ctree_control(mincriterion = 0.99))
set.seed(1)
fit_gba_rf_1 <- cforest(Ingreso ~ ., data = base_trim, controls=cforest_control(ntree = 500, mincriterion = 0.99))
y_hat_gba_1 <- predict(fit_gba_rf_1, base_gba, OOB=TRUE)
desigualdad_gba_1 <- ineq(y_hat_gba_1,type="Gini")
aux <- varimp(fit_gba_rf_1)
varimp_gba_1 <- data.frame(Variable=names(aux), Importancia=aux)
#2
base_trim <- base_gba %>% filter(Trimestre==2) %>%
  select(-c(REGION, Trimestre)) 
fit_gba_2 <- ctree(Ingreso ~ ., data = base_trim, controls = ctree_control(mincriterion = 0.99))
set.seed(1)
fit_gba_rf_2 <- cforest(Ingreso ~ ., data = base_trim, controls=cforest_control(ntree = 500, mincriterion = 0.99))
y_hat_gba_2 <- predict(fit_gba_rf_2, base_gba, OOB=TRUE)
desigualdad_gba_2 <- ineq(y_hat_gba_2,type="Gini")
aux <- varimp(fit_gba_rf_2)
varimp_gba_2 <- data.frame(Variable=names(aux), Importancia=aux)
#3
base_trim <- base_gba %>% filter(Trimestre==3) %>%
  select(-c(REGION, Trimestre)) 
fit_gba_3 <- ctree(Ingreso ~ ., data = base_trim, controls = ctree_control(mincriterion = 0.99))
set.seed(1)
fit_gba_rf_3 <- cforest(Ingreso ~ ., data = base_trim, controls=cforest_control(ntree = 500, mincriterion = 0.99))
y_hat_gba_3 <- predict(fit_gba_rf_3, base_gba, OOB=TRUE)
desigualdad_gba_3 <- ineq(y_hat_gba_3,type="Gini")
aux <- varimp(fit_gba_rf_3)
varimp_gba_3 <- data.frame(Variable=names(aux), Importancia=aux)
#4
base_trim <- base_gba %>% filter(Trimestre==4) %>%
  select(-c(REGION, Trimestre)) 
fit_gba_4 <- ctree(Ingreso ~ ., data = base_trim, controls = ctree_control(mincriterion = 0.99))
set.seed(1)
fit_gba_rf_4 <- cforest(Ingreso ~ ., data = base_trim, controls=cforest_control(ntree = 500, mincriterion = 0.99))
y_hat_gba_4 <- predict(fit_gba_rf_4, base_gba, OOB=TRUE)
desigualdad_gba_4 <- ineq(y_hat_gba_4,type="Gini")
aux <- varimp(fit_gba_rf_4)
varimp_gba_4 <- data.frame(Variable=names(aux), Importancia=aux)
#5
base_trim <- base_gba %>% filter(Trimestre==5) %>%
  select(-c(REGION, Trimestre)) 
fit_gba_5 <- ctree(Ingreso ~ ., data = base_trim, controls = ctree_control(mincriterion = 0.99))
set.seed(1)
fit_gba_rf_5 <- cforest(Ingreso ~ ., data = base_trim, controls=cforest_control(ntree = 500, mincriterion = 0.99))
y_hat_gba_5 <- predict(fit_gba_rf_5, base_gba, OOB=TRUE)
desigualdad_gba_5 <- ineq(y_hat_gba_5,type="Gini")
aux <- varimp(fit_gba_rf_5)
varimp_gba_5 <- data.frame(Variable=names(aux), Importancia=aux)
#6
base_trim <- base_gba %>% filter(Trimestre==6) %>%
  select(-c(REGION, Trimestre)) 
fit_gba_6 <- ctree(Ingreso ~ ., data = base_trim, controls = ctree_control(mincriterion = 0.99))
set.seed(1)
fit_gba_rf_6 <- cforest(Ingreso ~ ., data = base_trim, controls=cforest_control(ntree = 500, mincriterion = 0.99))
y_hat_gba_6 <- predict(fit_gba_rf_6, base_gba, OOB=TRUE)
desigualdad_gba_6 <- ineq(y_hat_gba_6,type="Gini")
aux <- varimp(fit_gba_rf_6)
varimp_gba_6 <- data.frame(Variable=names(aux), Importancia=aux)
#7
base_trim <- base_gba %>% filter(Trimestre==7) %>%
  select(-c(REGION, Trimestre)) 
fit_gba_7 <- ctree(Ingreso ~ ., data = base_trim, controls = ctree_control(mincriterion = 0.99))
set.seed(1)
fit_gba_rf_7 <- cforest(Ingreso ~ ., data = base_trim, controls=cforest_control(ntree = 500, mincriterion = 0.99))
y_hat_gba_7 <- predict(fit_gba_rf_7, base_gba, OOB=TRUE)
desigualdad_gba_7 <- ineq(y_hat_gba_7,type="Gini")
aux <- varimp(fit_gba_rf_7)
varimp_gba_7 <- data.frame(Variable=names(aux), Importancia=aux)
#8
base_trim <- base_gba %>% filter(Trimestre==8) %>%
  select(-c(REGION, Trimestre)) 
fit_gba_8 <- ctree(Ingreso ~ ., data = base_trim, controls = ctree_control(mincriterion = 0.99))
set.seed(1)
fit_gba_rf_8 <- cforest(Ingreso ~ ., data = base_trim, controls=cforest_control(ntree = 500, mincriterion = 0.99))
y_hat_gba_8 <- predict(fit_gba_rf_8, base_gba, OOB=TRUE)
desigualdad_gba_8 <- ineq(y_hat_gba_8,type="Gini")
aux <- varimp(fit_gba_rf_8)
varimp_gba_8 <- data.frame(Variable=names(aux), Importancia=aux)
#9
base_trim <- base_gba %>% filter(Trimestre==9) %>%
  select(-c(REGION, Trimestre)) 
fit_gba_9 <- ctree(Ingreso ~ ., data = base_trim, controls = ctree_control(mincriterion = 0.99))
set.seed(1)
fit_gba_rf_9 <- cforest(Ingreso ~ ., data = base_trim, controls=cforest_control(ntree = 500, mincriterion = 0.99))
y_hat_gba_9 <- predict(fit_gba_rf_9, base_gba, OOB=TRUE)
desigualdad_gba_9 <- ineq(y_hat_gba_9,type="Gini")
aux <- varimp(fit_gba_rf_9)
varimp_gba_9 <- data.frame(Variable=names(aux), Importancia=aux)
#10
base_trim <- base_gba %>% filter(Trimestre==10) %>%
  select(-c(REGION, Trimestre)) 
fit_gba_10 <- ctree(Ingreso ~ ., data = base_trim, controls = ctree_control(mincriterion = 0.99))
set.seed(1)
fit_gba_rf_10 <- cforest(Ingreso ~ ., data = base_trim, controls=cforest_control(ntree = 500, mincriterion = 0.99))
y_hat_gba_10 <- predict(fit_gba_rf_10, base_gba, OOB=TRUE)
desigualdad_gba_10 <- ineq(y_hat_gba_10,type="Gini")
aux <- varimp(fit_gba_rf_10)
varimp_gba_10 <- data.frame(Variable=names(aux), Importancia=aux)

varimp_gba <- data.frame(Trimestre=c(rep(1,8),rep(2,8),rep(3,8),rep(4,8),rep(5,8),
                         rep(6,8),rep(7,8),rep(8,8),rep(9,8),rep(10,8)),
                         Variable=c(as.character(varimp_gba_1$Variable),as.character(varimp_gba_2$Variable),
                                    as.character(varimp_gba_3$Variable),as.character(varimp_gba_4$Variable),
                                    as.character(varimp_gba_5$Variable),as.character(varimp_gba_6$Variable),
                                    as.character(varimp_gba_7$Variable),as.character(varimp_gba_8$Variable),
                                    as.character(varimp_gba_9$Variable),as.character(varimp_gba_10$Variable)),
                         Importancia=c(varimp_gba_1$Importancia,varimp_gba_2$Importancia,
                                       varimp_gba_3$Importancia,varimp_gba_4$Importancia,
                                       varimp_gba_5$Importancia,varimp_gba_6$Importancia,
                                       varimp_gba_7$Importancia,varimp_gba_8$Importancia,
                                       varimp_gba_9$Importancia,varimp_gba_10$Importancia))
rm(varimp_gba_1, varimp_gba_2,varimp_gba_3, varimp_gba_4,varimp_gba_5, varimp_gba_6,
   varimp_gba_7, varimp_gba_8,varimp_gba_9, varimp_gba_10)
desigualdad_gba <- data.frame(Trimestre=c(1:10),
                              Gini=c(desigualdad_gba_1, desigualdad_gba_2, desigualdad_gba_3,
                                     desigualdad_gba_4, desigualdad_gba_5, desigualdad_gba_6,
                                     desigualdad_gba_7, desigualdad_gba_8, desigualdad_gba_9,
                                     desigualdad_gba_10))
rm(desigualdad_gba_1, desigualdad_gba_2, desigualdad_gba_3,
   desigualdad_gba_4, desigualdad_gba_5, desigualdad_gba_6,
   desigualdad_gba_7, desigualdad_gba_8, desigualdad_gba_9,
   desigualdad_gba_10)
save.image("trimestrales_gba.Rda")

rm(list=ls())

#### NOA
load("EPH_final.Rda")

rm(actualizar_ingreso, ingreso_medio)

region <- c("1", "40", "41", "42", "43", "44")

bases_eph <- bases_eph %>%
  select(-c(CODUSU, NRO_HOGAR, COMPONENTE, NIVEL_ED_p))

base_noa <- bases_eph %>% filter(REGION==region[2])
#1
base_trim <- base_noa %>% filter(Trimestre==1) %>%
  select(-c(REGION, Trimestre)) 
fit_noa_1 <- ctree(Ingreso ~ ., data = base_trim, controls = ctree_control(mincriterion = 0.99))
set.seed(1)
fit_noa_rf_1 <- cforest(Ingreso ~ ., data = base_trim, controls=cforest_control(ntree = 500, mincriterion = 0.99))
y_hat_noa_1 <- predict(fit_noa_rf_1, base_noa, OOB=TRUE)
desigualdad_noa_1 <- ineq(y_hat_noa_1,type="Gini")
aux <- varimp(fit_noa_rf_1)
varimp_noa_1 <- data.frame(Variable=names(aux), Importancia=aux)
#2
base_trim <- base_noa %>% filter(Trimestre==2) %>%
  select(-c(REGION, Trimestre)) 
fit_noa_2 <- ctree(Ingreso ~ ., data = base_trim, controls = ctree_control(mincriterion = 0.99))
set.seed(1)
fit_noa_rf_2 <- cforest(Ingreso ~ ., data = base_trim, controls=cforest_control(ntree = 500, mincriterion = 0.99))
y_hat_noa_2 <- predict(fit_noa_rf_2, base_noa, OOB=TRUE)
desigualdad_noa_2 <- ineq(y_hat_noa_2,type="Gini")
aux <- varimp(fit_noa_rf_2)
varimp_noa_2 <- data.frame(Variable=names(aux), Importancia=aux)
#3
base_trim <- base_noa %>% filter(Trimestre==3) %>%
  select(-c(REGION, Trimestre)) 
fit_noa_3 <- ctree(Ingreso ~ ., data = base_trim, controls = ctree_control(mincriterion = 0.99))
set.seed(1)
fit_noa_rf_3 <- cforest(Ingreso ~ ., data = base_trim, controls=cforest_control(ntree = 500, mincriterion = 0.99))
y_hat_noa_3 <- predict(fit_noa_rf_3, base_noa, OOB=TRUE)
desigualdad_noa_3 <- ineq(y_hat_noa_3,type="Gini")
aux <- varimp(fit_noa_rf_3)
varimp_noa_3 <- data.frame(Variable=names(aux), Importancia=aux)
#4
base_trim <- base_noa %>% filter(Trimestre==4) %>%
  select(-c(REGION, Trimestre)) 
fit_noa_4 <- ctree(Ingreso ~ ., data = base_trim, controls = ctree_control(mincriterion = 0.99))
set.seed(1)
fit_noa_rf_4 <- cforest(Ingreso ~ ., data = base_trim, controls=cforest_control(ntree = 500, mincriterion = 0.99))
y_hat_noa_4 <- predict(fit_noa_rf_4, base_noa, OOB=TRUE)
desigualdad_noa_4 <- ineq(y_hat_noa_4,type="Gini")
aux <- varimp(fit_noa_rf_4)
varimp_noa_4 <- data.frame(Variable=names(aux), Importancia=aux)
#5
base_trim <- base_noa %>% filter(Trimestre==5) %>%
  select(-c(REGION, Trimestre)) 
fit_noa_5 <- ctree(Ingreso ~ ., data = base_trim, controls = ctree_control(mincriterion = 0.99))
set.seed(1)
fit_noa_rf_5 <- cforest(Ingreso ~ ., data = base_trim, controls=cforest_control(ntree = 500, mincriterion = 0.99))
y_hat_noa_5 <- predict(fit_noa_rf_5, base_noa, OOB=TRUE)
desigualdad_noa_5 <- ineq(y_hat_noa_5,type="Gini")
aux <- varimp(fit_noa_rf_5)
varimp_noa_5 <- data.frame(Variable=names(aux), Importancia=aux)
#6
base_trim <- base_noa %>% filter(Trimestre==6) %>%
  select(-c(REGION, Trimestre)) 
fit_noa_6 <- ctree(Ingreso ~ ., data = base_trim, controls = ctree_control(mincriterion = 0.99))
set.seed(1)
fit_noa_rf_6 <- cforest(Ingreso ~ ., data = base_trim, controls=cforest_control(ntree = 500, mincriterion = 0.99))
y_hat_noa_6 <- predict(fit_noa_rf_6, base_noa, OOB=TRUE)
desigualdad_noa_6 <- ineq(y_hat_noa_6,type="Gini")
aux <- varimp(fit_noa_rf_6)
varimp_noa_6 <- data.frame(Variable=names(aux), Importancia=aux)
#7
base_trim <- base_noa %>% filter(Trimestre==7) %>%
  select(-c(REGION, Trimestre)) 
fit_noa_7 <- ctree(Ingreso ~ ., data = base_trim, controls = ctree_control(mincriterion = 0.99))
set.seed(1)
fit_noa_rf_7 <- cforest(Ingreso ~ ., data = base_trim, controls=cforest_control(ntree = 500, mincriterion = 0.99))
y_hat_noa_7 <- predict(fit_noa_rf_7, base_noa, OOB=TRUE)
desigualdad_noa_7 <- ineq(y_hat_noa_7,type="Gini")
aux <- varimp(fit_noa_rf_7)
varimp_noa_7 <- data.frame(Variable=names(aux), Importancia=aux)
#8
base_trim <- base_noa %>% filter(Trimestre==8) %>%
  select(-c(REGION, Trimestre)) 
fit_noa_8 <- ctree(Ingreso ~ ., data = base_trim, controls = ctree_control(mincriterion = 0.99))
set.seed(1)
fit_noa_rf_8 <- cforest(Ingreso ~ ., data = base_trim, controls=cforest_control(ntree = 500, mincriterion = 0.99))
y_hat_noa_8 <- predict(fit_noa_rf_8, base_noa, OOB=TRUE)
desigualdad_noa_8 <- ineq(y_hat_noa_8,type="Gini")
aux <- varimp(fit_noa_rf_8)
varimp_noa_8 <- data.frame(Variable=names(aux), Importancia=aux)
#9
base_trim <- base_noa %>% filter(Trimestre==9) %>%
  select(-c(REGION, Trimestre)) 
fit_noa_9 <- ctree(Ingreso ~ ., data = base_trim, controls = ctree_control(mincriterion = 0.99))
set.seed(1)
fit_noa_rf_9 <- cforest(Ingreso ~ ., data = base_trim, controls=cforest_control(ntree = 500, mincriterion = 0.99))
y_hat_noa_9 <- predict(fit_noa_rf_9, base_noa, OOB=TRUE)
desigualdad_noa_9 <- ineq(y_hat_noa_9,type="Gini")
aux <- varimp(fit_noa_rf_9)
varimp_noa_9 <- data.frame(Variable=names(aux), Importancia=aux)
#10
base_trim <- base_noa %>% filter(Trimestre==10) %>%
  select(-c(REGION, Trimestre)) 
fit_noa_10 <- ctree(Ingreso ~ ., data = base_trim, controls = ctree_control(mincriterion = 0.99))
set.seed(1)
fit_noa_rf_10 <- cforest(Ingreso ~ ., data = base_trim, controls=cforest_control(ntree = 500, mincriterion = 0.99))
y_hat_noa_10 <- predict(fit_noa_rf_10, base_noa, OOB=TRUE)
desigualdad_noa_10 <- ineq(y_hat_noa_10,type="Gini")
aux <- varimp(fit_noa_rf_10)
varimp_noa_10 <- data.frame(Variable=names(aux), Importancia=aux)

varimp_noa <- data.frame(Trimestre=c(rep(1,8),rep(2,8),rep(3,8),rep(4,8),rep(5,8),
                                     rep(6,8),rep(7,8),rep(8,8),rep(9,8),rep(10,8)),
                         Variable=c(as.character(varimp_noa_1$Variable),as.character(varimp_noa_2$Variable),
                                    as.character(varimp_noa_3$Variable),as.character(varimp_noa_4$Variable),
                                    as.character(varimp_noa_5$Variable),as.character(varimp_noa_6$Variable),
                                    as.character(varimp_noa_7$Variable),as.character(varimp_noa_8$Variable),
                                    as.character(varimp_noa_9$Variable),as.character(varimp_noa_10$Variable)),
                         Importancia=c(varimp_noa_1$Importancia,varimp_noa_2$Importancia,
                                       varimp_noa_3$Importancia,varimp_noa_4$Importancia,
                                       varimp_noa_5$Importancia,varimp_noa_6$Importancia,
                                       varimp_noa_7$Importancia,varimp_noa_8$Importancia,
                                       varimp_noa_9$Importancia,varimp_noa_10$Importancia))
rm(varimp_noa_1, varimp_noa_2,varimp_noa_3, varimp_noa_4,varimp_noa_5, varimp_noa_6,
   varimp_noa_7, varimp_noa_8,varimp_noa_9, varimp_noa_10)
desigualdad_noa <- data.frame(Trimestre=c(1:10),
                              Gini=c(desigualdad_noa_1, desigualdad_noa_2, desigualdad_noa_3,
                                     desigualdad_noa_4, desigualdad_noa_5, desigualdad_noa_6,
                                     desigualdad_noa_7, desigualdad_noa_8, desigualdad_noa_9,
                                     desigualdad_noa_10))
rm(desigualdad_noa_1, desigualdad_noa_2, desigualdad_noa_3,
   desigualdad_noa_4, desigualdad_noa_5, desigualdad_noa_6,
   desigualdad_noa_7, desigualdad_noa_8, desigualdad_noa_9,
   desigualdad_noa_10)
save.image("trimestrales_noa.Rda")

rm(list=ls())

#### NEA
load("EPH_final.Rda")

rm(actualizar_ingreso, ingreso_medio)

region <- c("1", "40", "41", "42", "43", "44")

bases_eph <- bases_eph %>%
  select(-c(CODUSU, NRO_HOGAR, COMPONENTE, NIVEL_ED_p))

base_nea <- bases_eph %>% filter(REGION==region[3])
#1
base_trim <- base_nea %>% filter(Trimestre==1) %>%
  select(-c(REGION, Trimestre)) 
fit_nea_1 <- ctree(Ingreso ~ ., data = base_trim, controls = ctree_control(mincriterion = 0.99))
set.seed(1)
fit_nea_rf_1 <- cforest(Ingreso ~ ., data = base_trim, controls=cforest_control(ntree = 500, mincriterion = 0.99))
y_hat_nea_1 <- predict(fit_nea_rf_1, base_nea, OOB=TRUE)
desigualdad_nea_1 <- ineq(y_hat_nea_1,type="Gini")
aux <- varimp(fit_nea_rf_1)
varimp_nea_1 <- data.frame(Variable=names(aux), Importancia=aux)
#2
base_trim <- base_nea %>% filter(Trimestre==2) %>%
  select(-c(REGION, Trimestre)) 
fit_nea_2 <- ctree(Ingreso ~ ., data = base_trim, controls = ctree_control(mincriterion = 0.99))
set.seed(1)
fit_nea_rf_2 <- cforest(Ingreso ~ ., data = base_trim, controls=cforest_control(ntree = 500, mincriterion = 0.99))
y_hat_nea_2 <- predict(fit_nea_rf_2, base_nea, OOB=TRUE)
desigualdad_nea_2 <- ineq(y_hat_nea_2,type="Gini")
aux <- varimp(fit_nea_rf_2)
varimp_nea_2 <- data.frame(Variable=names(aux), Importancia=aux)
#3
base_trim <- base_nea %>% filter(Trimestre==3) %>%
  select(-c(REGION, Trimestre)) 
fit_nea_3 <- ctree(Ingreso ~ ., data = base_trim, controls = ctree_control(mincriterion = 0.99))
set.seed(1)
fit_nea_rf_3 <- cforest(Ingreso ~ ., data = base_trim, controls=cforest_control(ntree = 500, mincriterion = 0.99))
y_hat_nea_3 <- predict(fit_nea_rf_3, base_nea, OOB=TRUE)
desigualdad_nea_3 <- ineq(y_hat_nea_3,type="Gini")
aux <- varimp(fit_nea_rf_3)
varimp_nea_3 <- data.frame(Variable=names(aux), Importancia=aux)
#4
base_trim <- base_nea %>% filter(Trimestre==4) %>%
  select(-c(REGION, Trimestre)) 
fit_nea_4 <- ctree(Ingreso ~ ., data = base_trim, controls = ctree_control(mincriterion = 0.99))
set.seed(1)
fit_nea_rf_4 <- cforest(Ingreso ~ ., data = base_trim, controls=cforest_control(ntree = 500, mincriterion = 0.99))
y_hat_nea_4 <- predict(fit_nea_rf_4, base_nea, OOB=TRUE)
desigualdad_nea_4 <- ineq(y_hat_nea_4,type="Gini")
aux <- varimp(fit_nea_rf_4)
varimp_nea_4 <- data.frame(Variable=names(aux), Importancia=aux)
#5
base_trim <- base_nea %>% filter(Trimestre==5) %>%
  select(-c(REGION, Trimestre)) 
fit_nea_5 <- ctree(Ingreso ~ ., data = base_trim, controls = ctree_control(mincriterion = 0.99))
set.seed(1)
fit_nea_rf_5 <- cforest(Ingreso ~ ., data = base_trim, controls=cforest_control(ntree = 500, mincriterion = 0.99))
y_hat_nea_5 <- predict(fit_nea_rf_5, base_nea, OOB=TRUE)
desigualdad_nea_5 <- ineq(y_hat_nea_5,type="Gini")
aux <- varimp(fit_nea_rf_5)
varimp_nea_5 <- data.frame(Variable=names(aux), Importancia=aux)
#6
base_trim <- base_nea %>% filter(Trimestre==6) %>%
  select(-c(REGION, Trimestre)) 
fit_nea_6 <- ctree(Ingreso ~ ., data = base_trim, controls = ctree_control(mincriterion = 0.99))
set.seed(1)
fit_nea_rf_6 <- cforest(Ingreso ~ ., data = base_trim, controls=cforest_control(ntree = 500, mincriterion = 0.99))
y_hat_nea_6 <- predict(fit_nea_rf_6, base_nea, OOB=TRUE)
desigualdad_nea_6 <- ineq(y_hat_nea_6,type="Gini")
aux <- varimp(fit_nea_rf_6)
varimp_nea_6 <- data.frame(Variable=names(aux), Importancia=aux)
#7
base_trim <- base_nea %>% filter(Trimestre==7) %>%
  select(-c(REGION, Trimestre)) 
fit_nea_7 <- ctree(Ingreso ~ ., data = base_trim, controls = ctree_control(mincriterion = 0.99))
set.seed(1)
fit_nea_rf_7 <- cforest(Ingreso ~ ., data = base_trim, controls=cforest_control(ntree = 500, mincriterion = 0.99))
y_hat_nea_7 <- predict(fit_nea_rf_7, base_nea, OOB=TRUE)
desigualdad_nea_7 <- ineq(y_hat_nea_7,type="Gini")
aux <- varimp(fit_nea_rf_7)
varimp_nea_7 <- data.frame(Variable=names(aux), Importancia=aux)
#8
base_trim <- base_nea %>% filter(Trimestre==8) %>%
  select(-c(REGION, Trimestre)) 
fit_nea_8 <- ctree(Ingreso ~ ., data = base_trim, controls = ctree_control(mincriterion = 0.99))
set.seed(1)
fit_nea_rf_8 <- cforest(Ingreso ~ ., data = base_trim, controls=cforest_control(ntree = 500, mincriterion = 0.99))
y_hat_nea_8 <- predict(fit_nea_rf_8, base_nea, OOB=TRUE)
desigualdad_nea_8 <- ineq(y_hat_nea_8,type="Gini")
aux <- varimp(fit_nea_rf_8)
varimp_nea_8 <- data.frame(Variable=names(aux), Importancia=aux)
#9
base_trim <- base_nea %>% filter(Trimestre==9) %>%
  select(-c(REGION, Trimestre)) 
fit_nea_9 <- ctree(Ingreso ~ ., data = base_trim, controls = ctree_control(mincriterion = 0.99))
set.seed(1)
fit_nea_rf_9 <- cforest(Ingreso ~ ., data = base_trim, controls=cforest_control(ntree = 500, mincriterion = 0.99))
y_hat_nea_9 <- predict(fit_nea_rf_9, base_nea, OOB=TRUE)
desigualdad_nea_9 <- ineq(y_hat_nea_9,type="Gini")
aux <- varimp(fit_nea_rf_9)
varimp_nea_9 <- data.frame(Variable=names(aux), Importancia=aux)
#10
base_trim <- base_nea %>% filter(Trimestre==10) %>%
  select(-c(REGION, Trimestre)) 
fit_nea_10 <- ctree(Ingreso ~ ., data = base_trim, controls = ctree_control(mincriterion = 0.99))
set.seed(1)
fit_nea_rf_10 <- cforest(Ingreso ~ ., data = base_trim, controls=cforest_control(ntree = 500, mincriterion = 0.99))
y_hat_nea_10 <- predict(fit_nea_rf_10, base_nea, OOB=TRUE)
desigualdad_nea_10 <- ineq(y_hat_nea_10,type="Gini")
aux <- varimp(fit_nea_rf_10)
varimp_nea_10 <- data.frame(Variable=names(aux), Importancia=aux)

varimp_nea <- data.frame(Trimestre=c(rep(1,8),rep(2,8),rep(3,8),rep(4,8),rep(5,8),
                                     rep(6,8),rep(7,8),rep(8,8),rep(9,8),rep(10,8)),
                         Variable=c(as.character(varimp_nea_1$Variable),as.character(varimp_nea_2$Variable),
                                    as.character(varimp_nea_3$Variable),as.character(varimp_nea_4$Variable),
                                    as.character(varimp_nea_5$Variable),as.character(varimp_nea_6$Variable),
                                    as.character(varimp_nea_7$Variable),as.character(varimp_nea_8$Variable),
                                    as.character(varimp_nea_9$Variable),as.character(varimp_nea_10$Variable)),
                         Importancia=c(varimp_nea_1$Importancia,varimp_nea_2$Importancia,
                                       varimp_nea_3$Importancia,varimp_nea_4$Importancia,
                                       varimp_nea_5$Importancia,varimp_nea_6$Importancia,
                                       varimp_nea_7$Importancia,varimp_nea_8$Importancia,
                                       varimp_nea_9$Importancia,varimp_nea_10$Importancia))
rm(varimp_nea_1, varimp_nea_2,varimp_nea_3, varimp_nea_4,varimp_nea_5, varimp_nea_6,
   varimp_nea_7, varimp_nea_8,varimp_nea_9, varimp_nea_10)
desigualdad_nea <- data.frame(Trimestre=c(1:10),
                              Gini=c(desigualdad_nea_1, desigualdad_nea_2, desigualdad_nea_3,
                                     desigualdad_nea_4, desigualdad_nea_5, desigualdad_nea_6,
                                     desigualdad_nea_7, desigualdad_nea_8, desigualdad_nea_9,
                                     desigualdad_nea_10))
rm(desigualdad_nea_1, desigualdad_nea_2, desigualdad_nea_3,
   desigualdad_nea_4, desigualdad_nea_5, desigualdad_nea_6,
   desigualdad_nea_7, desigualdad_nea_8, desigualdad_nea_9,
   desigualdad_nea_10)
save.image("trimestrales_nea.Rda")

rm(list=ls())

#### Cuyo
load("EPH_final.Rda")

rm(actualizar_ingreso, ingreso_medio)

region <- c("1", "40", "41", "42", "43", "44")

bases_eph <- bases_eph %>%
  select(-c(CODUSU, NRO_HOGAR, COMPONENTE, NIVEL_ED_p))

base_cuyo <- bases_eph %>% filter(REGION==region[4])
#1
base_trim <- base_cuyo %>% filter(Trimestre==1) %>%
  select(-c(REGION, Trimestre)) 
fit_cuyo_1 <- ctree(Ingreso ~ ., data = base_trim, controls = ctree_control(mincriterion = 0.99))
set.seed(1)
fit_cuyo_rf_1 <- cforest(Ingreso ~ ., data = base_trim, controls=cforest_control(ntree = 500, mincriterion = 0.99))
y_hat_cuyo_1 <- predict(fit_cuyo_rf_1, base_cuyo, OOB=TRUE)
desigualdad_cuyo_1 <- ineq(y_hat_cuyo_1,type="Gini")
aux <- varimp(fit_cuyo_rf_1)
varimp_cuyo_1 <- data.frame(Variable=names(aux), Importancia=aux)
#2
base_trim <- base_cuyo %>% filter(Trimestre==2) %>%
  select(-c(REGION, Trimestre)) 
fit_cuyo_2 <- ctree(Ingreso ~ ., data = base_trim, controls = ctree_control(mincriterion = 0.99))
set.seed(1)
fit_cuyo_rf_2 <- cforest(Ingreso ~ ., data = base_trim, controls=cforest_control(ntree = 500, mincriterion = 0.99))
y_hat_cuyo_2 <- predict(fit_cuyo_rf_2, base_cuyo, OOB=TRUE)
desigualdad_cuyo_2 <- ineq(y_hat_cuyo_2,type="Gini")
aux <- varimp(fit_cuyo_rf_2)
varimp_cuyo_2 <- data.frame(Variable=names(aux), Importancia=aux)
#3
base_trim <- base_cuyo %>% filter(Trimestre==3) %>%
  select(-c(REGION, Trimestre)) 
fit_cuyo_3 <- ctree(Ingreso ~ ., data = base_trim, controls = ctree_control(mincriterion = 0.99))
set.seed(1)
fit_cuyo_rf_3 <- cforest(Ingreso ~ ., data = base_trim, controls=cforest_control(ntree = 500, mincriterion = 0.99))
y_hat_cuyo_3 <- predict(fit_cuyo_rf_3, base_cuyo, OOB=TRUE)
desigualdad_cuyo_3 <- ineq(y_hat_cuyo_3,type="Gini")
aux <- varimp(fit_cuyo_rf_3)
varimp_cuyo_3 <- data.frame(Variable=names(aux), Importancia=aux)
#4
base_trim <- base_cuyo %>% filter(Trimestre==4) %>%
  select(-c(REGION, Trimestre)) 
fit_cuyo_4 <- ctree(Ingreso ~ ., data = base_trim, controls = ctree_control(mincriterion = 0.99))
set.seed(1)
fit_cuyo_rf_4 <- cforest(Ingreso ~ ., data = base_trim, controls=cforest_control(ntree = 500, mincriterion = 0.99))
y_hat_cuyo_4 <- predict(fit_cuyo_rf_4, base_cuyo, OOB=TRUE)
desigualdad_cuyo_4 <- ineq(y_hat_cuyo_4,type="Gini")
aux <- varimp(fit_cuyo_rf_4)
varimp_cuyo_4 <- data.frame(Variable=names(aux), Importancia=aux)
#5
base_trim <- base_cuyo %>% filter(Trimestre==5) %>%
  select(-c(REGION, Trimestre)) 
fit_cuyo_5 <- ctree(Ingreso ~ ., data = base_trim, controls = ctree_control(mincriterion = 0.99))
set.seed(1)
fit_cuyo_rf_5 <- cforest(Ingreso ~ ., data = base_trim, controls=cforest_control(ntree = 500, mincriterion = 0.99))
y_hat_cuyo_5 <- predict(fit_cuyo_rf_5, base_cuyo, OOB=TRUE)
desigualdad_cuyo_5 <- ineq(y_hat_cuyo_5,type="Gini")
aux <- varimp(fit_cuyo_rf_5)
varimp_cuyo_5 <- data.frame(Variable=names(aux), Importancia=aux)
#6
base_trim <- base_cuyo %>% filter(Trimestre==6) %>%
  select(-c(REGION, Trimestre)) 
fit_cuyo_6 <- ctree(Ingreso ~ ., data = base_trim, controls = ctree_control(mincriterion = 0.99))
set.seed(1)
fit_cuyo_rf_6 <- cforest(Ingreso ~ ., data = base_trim, controls=cforest_control(ntree = 500, mincriterion = 0.99))
y_hat_cuyo_6 <- predict(fit_cuyo_rf_6, base_cuyo, OOB=TRUE)
desigualdad_cuyo_6 <- ineq(y_hat_cuyo_6,type="Gini")
aux <- varimp(fit_cuyo_rf_6)
varimp_cuyo_6 <- data.frame(Variable=names(aux), Importancia=aux)
#7
base_trim <- base_cuyo %>% filter(Trimestre==7) %>%
  select(-c(REGION, Trimestre)) 
fit_cuyo_7 <- ctree(Ingreso ~ ., data = base_trim, controls = ctree_control(mincriterion = 0.99))
set.seed(1)
fit_cuyo_rf_7 <- cforest(Ingreso ~ ., data = base_trim, controls=cforest_control(ntree = 500, mincriterion = 0.99))
y_hat_cuyo_7 <- predict(fit_cuyo_rf_7, base_cuyo, OOB=TRUE)
desigualdad_cuyo_7 <- ineq(y_hat_cuyo_7,type="Gini")
aux <- varimp(fit_cuyo_rf_7)
varimp_cuyo_7 <- data.frame(Variable=names(aux), Importancia=aux)
#8
base_trim <- base_cuyo %>% filter(Trimestre==8) %>%
  select(-c(REGION, Trimestre)) 
fit_cuyo_8 <- ctree(Ingreso ~ ., data = base_trim, controls = ctree_control(mincriterion = 0.99))
set.seed(1)
fit_cuyo_rf_8 <- cforest(Ingreso ~ ., data = base_trim, controls=cforest_control(ntree = 500, mincriterion = 0.99))
y_hat_cuyo_8 <- predict(fit_cuyo_rf_8, base_cuyo, OOB=TRUE)
desigualdad_cuyo_8 <- ineq(y_hat_cuyo_8,type="Gini")
aux <- varimp(fit_cuyo_rf_8)
varimp_cuyo_8 <- data.frame(Variable=names(aux), Importancia=aux)
#9
base_trim <- base_cuyo %>% filter(Trimestre==9) %>%
  select(-c(REGION, Trimestre)) 
fit_cuyo_9 <- ctree(Ingreso ~ ., data = base_trim, controls = ctree_control(mincriterion = 0.99))
set.seed(1)
fit_cuyo_rf_9 <- cforest(Ingreso ~ ., data = base_trim, controls=cforest_control(ntree = 500, mincriterion = 0.99))
y_hat_cuyo_9 <- predict(fit_cuyo_rf_9, base_cuyo, OOB=TRUE)
desigualdad_cuyo_9 <- ineq(y_hat_cuyo_9,type="Gini")
aux <- varimp(fit_cuyo_rf_9)
varimp_cuyo_9 <- data.frame(Variable=names(aux), Importancia=aux)
#10
base_trim <- base_cuyo %>% filter(Trimestre==10) %>%
  select(-c(REGION, Trimestre)) 
fit_cuyo_10 <- ctree(Ingreso ~ ., data = base_trim, controls = ctree_control(mincriterion = 0.99))
set.seed(1)
fit_cuyo_rf_10 <- cforest(Ingreso ~ ., data = base_trim, controls=cforest_control(ntree = 500, mincriterion = 0.99))
y_hat_cuyo_10 <- predict(fit_cuyo_rf_10, base_cuyo, OOB=TRUE)
desigualdad_cuyo_10 <- ineq(y_hat_cuyo_10,type="Gini")
aux <- varimp(fit_cuyo_rf_10)
varimp_cuyo_10 <- data.frame(Variable=names(aux), Importancia=aux)

varimp_cuyo <- data.frame(Trimestre=c(rep(1,8),rep(2,8),rep(3,8),rep(4,8),rep(5,8),
                                     rep(6,8),rep(7,8),rep(8,8),rep(9,8),rep(10,8)),
                         Variable=c(as.character(varimp_cuyo_1$Variable),as.character(varimp_cuyo_2$Variable),
                                    as.character(varimp_cuyo_3$Variable),as.character(varimp_cuyo_4$Variable),
                                    as.character(varimp_cuyo_5$Variable),as.character(varimp_cuyo_6$Variable),
                                    as.character(varimp_cuyo_7$Variable),as.character(varimp_cuyo_8$Variable),
                                    as.character(varimp_cuyo_9$Variable),as.character(varimp_cuyo_10$Variable)),
                         Importancia=c(varimp_cuyo_1$Importancia,varimp_cuyo_2$Importancia,
                                       varimp_cuyo_3$Importancia,varimp_cuyo_4$Importancia,
                                       varimp_cuyo_5$Importancia,varimp_cuyo_6$Importancia,
                                       varimp_cuyo_7$Importancia,varimp_cuyo_8$Importancia,
                                       varimp_cuyo_9$Importancia,varimp_cuyo_10$Importancia))
rm(varimp_cuyo_1, varimp_cuyo_2,varimp_cuyo_3, varimp_cuyo_4,varimp_cuyo_5, varimp_cuyo_6,
   varimp_cuyo_7, varimp_cuyo_8,varimp_cuyo_9, varimp_cuyo_10)
desigualdad_cuyo <- data.frame(Trimestre=c(1:10),
                              Gini=c(desigualdad_cuyo_1, desigualdad_cuyo_2, desigualdad_cuyo_3,
                                     desigualdad_cuyo_4, desigualdad_cuyo_5, desigualdad_cuyo_6,
                                     desigualdad_cuyo_7, desigualdad_cuyo_8, desigualdad_cuyo_9,
                                     desigualdad_cuyo_10))
rm(desigualdad_cuyo_1, desigualdad_cuyo_2, desigualdad_cuyo_3,
   desigualdad_cuyo_4, desigualdad_cuyo_5, desigualdad_cuyo_6,
   desigualdad_cuyo_7, desigualdad_cuyo_8, desigualdad_cuyo_9,
   desigualdad_cuyo_10)
save.image("trimestrales_cuyo.Rda")

rm(list=ls())

#### Pampeana
load("EPH_final.Rda")

rm(actualizar_ingreso, ingreso_medio)

region <- c("1", "40", "41", "42", "43", "44")

bases_eph <- bases_eph %>%
  select(-c(CODUSU, NRO_HOGAR, COMPONENTE, NIVEL_ED_p))

base_pamp <- bases_eph %>% filter(REGION==region[5])
#1
base_trim <- base_pamp %>% filter(Trimestre==1) %>%
  select(-c(REGION, Trimestre)) 
fit_pamp_1 <- ctree(Ingreso ~ ., data = base_trim, controls = ctree_control(mincriterion = 0.99))
set.seed(1)
fit_pamp_rf_1 <- cforest(Ingreso ~ ., data = base_trim, controls=cforest_control(ntree = 500, mincriterion = 0.99))
y_hat_pamp_1 <- predict(fit_pamp_rf_1, base_pamp, OOB=TRUE)
desigualdad_pamp_1 <- ineq(y_hat_pamp_1,type="Gini")
aux <- varimp(fit_pamp_rf_1)
varimp_pamp_1 <- data.frame(Variable=names(aux), Importancia=aux)
#2
base_trim <- base_pamp %>% filter(Trimestre==2) %>%
  select(-c(REGION, Trimestre)) 
fit_pamp_2 <- ctree(Ingreso ~ ., data = base_trim, controls = ctree_control(mincriterion = 0.99))
set.seed(1)
fit_pamp_rf_2 <- cforest(Ingreso ~ ., data = base_trim, controls=cforest_control(ntree = 500, mincriterion = 0.99))
y_hat_pamp_2 <- predict(fit_pamp_rf_2, base_pamp, OOB=TRUE)
desigualdad_pamp_2 <- ineq(y_hat_pamp_2,type="Gini")
aux <- varimp(fit_pamp_rf_2)
varimp_pamp_2 <- data.frame(Variable=names(aux), Importancia=aux)
#3
base_trim <- base_pamp %>% filter(Trimestre==3) %>%
  select(-c(REGION, Trimestre)) 
fit_pamp_3 <- ctree(Ingreso ~ ., data = base_trim, controls = ctree_control(mincriterion = 0.99))
set.seed(1)
fit_pamp_rf_3 <- cforest(Ingreso ~ ., data = base_trim, controls=cforest_control(ntree = 500, mincriterion = 0.99))
y_hat_pamp_3 <- predict(fit_pamp_rf_3, base_pamp, OOB=TRUE)
desigualdad_pamp_3 <- ineq(y_hat_pamp_3,type="Gini")
aux <- varimp(fit_pamp_rf_3)
varimp_pamp_3 <- data.frame(Variable=names(aux), Importancia=aux)
#4
base_trim <- base_pamp %>% filter(Trimestre==4) %>%
  select(-c(REGION, Trimestre)) 
fit_pamp_4 <- ctree(Ingreso ~ ., data = base_trim, controls = ctree_control(mincriterion = 0.99))
set.seed(1)
fit_pamp_rf_4 <- cforest(Ingreso ~ ., data = base_trim, controls=cforest_control(ntree = 500, mincriterion = 0.99))
y_hat_pamp_4 <- predict(fit_pamp_rf_4, base_pamp, OOB=TRUE)
desigualdad_pamp_4 <- ineq(y_hat_pamp_4,type="Gini")
aux <- varimp(fit_pamp_rf_4)
varimp_pamp_4 <- data.frame(Variable=names(aux), Importancia=aux)
#5
base_trim <- base_pamp %>% filter(Trimestre==5) %>%
  select(-c(REGION, Trimestre)) 
fit_pamp_5 <- ctree(Ingreso ~ ., data = base_trim, controls = ctree_control(mincriterion = 0.99))
set.seed(1)
fit_pamp_rf_5 <- cforest(Ingreso ~ ., data = base_trim, controls=cforest_control(ntree = 500, mincriterion = 0.99))
y_hat_pamp_5 <- predict(fit_pamp_rf_5, base_pamp, OOB=TRUE)
desigualdad_pamp_5 <- ineq(y_hat_pamp_5,type="Gini")
aux <- varimp(fit_pamp_rf_5)
varimp_pamp_5 <- data.frame(Variable=names(aux), Importancia=aux)
#6
base_trim <- base_pamp %>% filter(Trimestre==6) %>%
  select(-c(REGION, Trimestre)) 
fit_pamp_6 <- ctree(Ingreso ~ ., data = base_trim, controls = ctree_control(mincriterion = 0.99))
set.seed(1)
fit_pamp_rf_6 <- cforest(Ingreso ~ ., data = base_trim, controls=cforest_control(ntree = 500, mincriterion = 0.99))
y_hat_pamp_6 <- predict(fit_pamp_rf_6, base_pamp, OOB=TRUE)
desigualdad_pamp_6 <- ineq(y_hat_pamp_6,type="Gini")
aux <- varimp(fit_pamp_rf_6)
varimp_pamp_6 <- data.frame(Variable=names(aux), Importancia=aux)
#7
base_trim <- base_pamp %>% filter(Trimestre==7) %>%
  select(-c(REGION, Trimestre)) 
fit_pamp_7 <- ctree(Ingreso ~ ., data = base_trim, controls = ctree_control(mincriterion = 0.99))
set.seed(1)
fit_pamp_rf_7 <- cforest(Ingreso ~ ., data = base_trim, controls=cforest_control(ntree = 500, mincriterion = 0.99))
y_hat_pamp_7 <- predict(fit_pamp_rf_7, base_pamp, OOB=TRUE)
desigualdad_pamp_7 <- ineq(y_hat_pamp_7,type="Gini")
aux <- varimp(fit_pamp_rf_7)
varimp_pamp_7 <- data.frame(Variable=names(aux), Importancia=aux)
#8
base_trim <- base_pamp %>% filter(Trimestre==8) %>%
  select(-c(REGION, Trimestre)) 
fit_pamp_8 <- ctree(Ingreso ~ ., data = base_trim, controls = ctree_control(mincriterion = 0.99))
set.seed(1)
fit_pamp_rf_8 <- cforest(Ingreso ~ ., data = base_trim, controls=cforest_control(ntree = 500, mincriterion = 0.99))
y_hat_pamp_8 <- predict(fit_pamp_rf_8, base_pamp, OOB=TRUE)
desigualdad_pamp_8 <- ineq(y_hat_pamp_8,type="Gini")
aux <- varimp(fit_pamp_rf_8)
varimp_pamp_8 <- data.frame(Variable=names(aux), Importancia=aux)
#9
base_trim <- base_pamp %>% filter(Trimestre==9) %>%
  select(-c(REGION, Trimestre)) 
fit_pamp_9 <- ctree(Ingreso ~ ., data = base_trim, controls = ctree_control(mincriterion = 0.99))
set.seed(1)
fit_pamp_rf_9 <- cforest(Ingreso ~ ., data = base_trim, controls=cforest_control(ntree = 500, mincriterion = 0.99))
y_hat_pamp_9 <- predict(fit_pamp_rf_9, base_pamp, OOB=TRUE)
desigualdad_pamp_9 <- ineq(y_hat_pamp_9,type="Gini")
aux <- varimp(fit_pamp_rf_9)
varimp_pamp_9 <- data.frame(Variable=names(aux), Importancia=aux)
#10
base_trim <- base_pamp %>% filter(Trimestre==10) %>%
  select(-c(REGION, Trimestre)) 
fit_pamp_10 <- ctree(Ingreso ~ ., data = base_trim, controls = ctree_control(mincriterion = 0.99))
set.seed(1)
fit_pamp_rf_10 <- cforest(Ingreso ~ ., data = base_trim, controls=cforest_control(ntree = 500, mincriterion = 0.99))
y_hat_pamp_10 <- predict(fit_pamp_rf_10, base_pamp, OOB=TRUE)
desigualdad_pamp_10 <- ineq(y_hat_pamp_10,type="Gini")
aux <- varimp(fit_pamp_rf_10)
varimp_pamp_10 <- data.frame(Variable=names(aux), Importancia=aux)

varimp_pamp <- data.frame(Trimestre=c(rep(1,8),rep(2,8),rep(3,8),rep(4,8),rep(5,8),
                                     rep(6,8),rep(7,8),rep(8,8),rep(9,8),rep(10,8)),
                         Variable=c(as.character(varimp_pamp_1$Variable),as.character(varimp_pamp_2$Variable),
                                    as.character(varimp_pamp_3$Variable),as.character(varimp_pamp_4$Variable),
                                    as.character(varimp_pamp_5$Variable),as.character(varimp_pamp_6$Variable),
                                    as.character(varimp_pamp_7$Variable),as.character(varimp_pamp_8$Variable),
                                    as.character(varimp_pamp_9$Variable),as.character(varimp_pamp_10$Variable)),
                         Importancia=c(varimp_pamp_1$Importancia,varimp_pamp_2$Importancia,
                                       varimp_pamp_3$Importancia,varimp_pamp_4$Importancia,
                                       varimp_pamp_5$Importancia,varimp_pamp_6$Importancia,
                                       varimp_pamp_7$Importancia,varimp_pamp_8$Importancia,
                                       varimp_pamp_9$Importancia,varimp_pamp_10$Importancia))
rm(varimp_pamp_1, varimp_pamp_2,varimp_pamp_3, varimp_pamp_4,varimp_pamp_5, varimp_pamp_6,
   varimp_pamp_7, varimp_pamp_8,varimp_pamp_9, varimp_pamp_10)
desigualdad_pamp <- data.frame(Trimestre=c(1:10),
                              Gini=c(desigualdad_pamp_1, desigualdad_pamp_2, desigualdad_pamp_3,
                                     desigualdad_pamp_4, desigualdad_pamp_5, desigualdad_pamp_6,
                                     desigualdad_pamp_7, desigualdad_pamp_8, desigualdad_pamp_9,
                                     desigualdad_pamp_10))
rm(desigualdad_pamp_1, desigualdad_pamp_2, desigualdad_pamp_3,
   desigualdad_pamp_4, desigualdad_pamp_5, desigualdad_pamp_6,
   desigualdad_pamp_7, desigualdad_pamp_8, desigualdad_pamp_9,
   desigualdad_pamp_10)
save.image("trimestrales_pamp.Rda")

rm(list=ls())

#### Patagonia
load("EPH_final.Rda")

rm(actualizar_ingreso, ingreso_medio)

region <- c("1", "40", "41", "42", "43", "44")

bases_eph <- bases_eph %>%
  select(-c(CODUSU, NRO_HOGAR, COMPONENTE, NIVEL_ED_p))

base_pata <- bases_eph %>% filter(REGION==region[6])
#1
base_trim <- base_pata %>% filter(Trimestre==1) %>%
  select(-c(REGION, Trimestre)) 
fit_pata_1 <- ctree(Ingreso ~ ., data = base_trim, controls = ctree_control(mincriterion = 0.99))
set.seed(1)
fit_pata_rf_1 <- cforest(Ingreso ~ ., data = base_trim, controls=cforest_control(ntree = 500, mincriterion = 0.99))
y_hat_pata_1 <- predict(fit_pata_rf_1, base_pata, OOB=TRUE)
desigualdad_pata_1 <- ineq(y_hat_pata_1,type="Gini")
aux <- varimp(fit_pata_rf_1)
varimp_pata_1 <- data.frame(Variable=names(aux), Importancia=aux)
#2
base_trim <- base_pata %>% filter(Trimestre==2) %>%
  select(-c(REGION, Trimestre)) 
fit_pata_2 <- ctree(Ingreso ~ ., data = base_trim, controls = ctree_control(mincriterion = 0.99))
set.seed(1)
fit_pata_rf_2 <- cforest(Ingreso ~ ., data = base_trim, controls=cforest_control(ntree = 500, mincriterion = 0.99))
y_hat_pata_2 <- predict(fit_pata_rf_2, base_pata, OOB=TRUE)
desigualdad_pata_2 <- ineq(y_hat_pata_2,type="Gini")
aux <- varimp(fit_pata_rf_2)
varimp_pata_2 <- data.frame(Variable=names(aux), Importancia=aux)
#3
base_trim <- base_pata %>% filter(Trimestre==3) %>%
  select(-c(REGION, Trimestre)) 
fit_pata_3 <- ctree(Ingreso ~ ., data = base_trim, controls = ctree_control(mincriterion = 0.99))
set.seed(1)
fit_pata_rf_3 <- cforest(Ingreso ~ ., data = base_trim, controls=cforest_control(ntree = 500, mincriterion = 0.99))
y_hat_pata_3 <- predict(fit_pata_rf_3, base_pata, OOB=TRUE)
desigualdad_pata_3 <- ineq(y_hat_pata_3,type="Gini")
aux <- varimp(fit_pata_rf_3)
varimp_pata_3 <- data.frame(Variable=names(aux), Importancia=aux)
#4
base_trim <- base_pata %>% filter(Trimestre==4) %>%
  select(-c(REGION, Trimestre)) 
fit_pata_4 <- ctree(Ingreso ~ ., data = base_trim, controls = ctree_control(mincriterion = 0.99))
set.seed(1)
fit_pata_rf_4 <- cforest(Ingreso ~ ., data = base_trim, controls=cforest_control(ntree = 500, mincriterion = 0.99))
y_hat_pata_4 <- predict(fit_pata_rf_4, base_pata, OOB=TRUE)
desigualdad_pata_4 <- ineq(y_hat_pata_4,type="Gini")
aux <- varimp(fit_pata_rf_4)
varimp_pata_4 <- data.frame(Variable=names(aux), Importancia=aux)
#5
base_trim <- base_pata %>% filter(Trimestre==5) %>%
  select(-c(REGION, Trimestre)) 
fit_pata_5 <- ctree(Ingreso ~ ., data = base_trim, controls = ctree_control(mincriterion = 0.99))
set.seed(1)
fit_pata_rf_5 <- cforest(Ingreso ~ ., data = base_trim, controls=cforest_control(ntree = 500, mincriterion = 0.99))
y_hat_pata_5 <- predict(fit_pata_rf_5, base_pata, OOB=TRUE)
desigualdad_pata_5 <- ineq(y_hat_pata_5,type="Gini")
aux <- varimp(fit_pata_rf_5)
varimp_pata_5 <- data.frame(Variable=names(aux), Importancia=aux)
#6
base_trim <- base_pata %>% filter(Trimestre==6) %>%
  select(-c(REGION, Trimestre)) 
fit_pata_6 <- ctree(Ingreso ~ ., data = base_trim, controls = ctree_control(mincriterion = 0.99))
set.seed(1)
fit_pata_rf_6 <- cforest(Ingreso ~ ., data = base_trim, controls=cforest_control(ntree = 500, mincriterion = 0.99))
y_hat_pata_6 <- predict(fit_pata_rf_6, base_pata, OOB=TRUE)
desigualdad_pata_6 <- ineq(y_hat_pata_6,type="Gini")
aux <- varimp(fit_pata_rf_6)
varimp_pata_6 <- data.frame(Variable=names(aux), Importancia=aux)
#7
base_trim <- base_pata %>% filter(Trimestre==7) %>%
  select(-c(REGION, Trimestre)) 
fit_pata_7 <- ctree(Ingreso ~ ., data = base_trim, controls = ctree_control(mincriterion = 0.99))
set.seed(1)
fit_pata_rf_7 <- cforest(Ingreso ~ ., data = base_trim, controls=cforest_control(ntree = 500, mincriterion = 0.99))
y_hat_pata_7 <- predict(fit_pata_rf_7, base_pata, OOB=TRUE)
desigualdad_pata_7 <- ineq(y_hat_pata_7,type="Gini")
aux <- varimp(fit_pata_rf_7)
varimp_pata_7 <- data.frame(Variable=names(aux), Importancia=aux)
#8
base_trim <- base_pata %>% filter(Trimestre==8) %>%
  select(-c(REGION, Trimestre)) 
fit_pata_8 <- ctree(Ingreso ~ ., data = base_trim, controls = ctree_control(mincriterion = 0.99))
set.seed(1)
fit_pata_rf_8 <- cforest(Ingreso ~ ., data = base_trim, controls=cforest_control(ntree = 500, mincriterion = 0.99))
y_hat_pata_8 <- predict(fit_pata_rf_8, base_pata, OOB=TRUE)
desigualdad_pata_8 <- ineq(y_hat_pata_8,type="Gini")
aux <- varimp(fit_pata_rf_8)
varimp_pata_8 <- data.frame(Variable=names(aux), Importancia=aux)
#9
base_trim <- base_pata %>% filter(Trimestre==9) %>%
  select(-c(REGION, Trimestre)) 
fit_pata_9 <- ctree(Ingreso ~ ., data = base_trim, controls = ctree_control(mincriterion = 0.99))
set.seed(1)
fit_pata_rf_9 <- cforest(Ingreso ~ ., data = base_trim, controls=cforest_control(ntree = 500, mincriterion = 0.99))
y_hat_pata_9 <- predict(fit_pata_rf_9, base_pata, OOB=TRUE)
desigualdad_pata_9 <- ineq(y_hat_pata_9,type="Gini")
aux <- varimp(fit_pata_rf_9)
varimp_pata_9 <- data.frame(Variable=names(aux), Importancia=aux)
#10
base_trim <- base_pata %>% filter(Trimestre==10) %>%
  select(-c(REGION, Trimestre)) 
fit_pata_10 <- ctree(Ingreso ~ ., data = base_trim, controls = ctree_control(mincriterion = 0.99))
set.seed(1)
fit_pata_rf_10 <- cforest(Ingreso ~ ., data = base_trim, controls=cforest_control(ntree = 500, mincriterion = 0.99))
y_hat_pata_10 <- predict(fit_pata_rf_10, base_pata, OOB=TRUE)
desigualdad_pata_10 <- ineq(y_hat_pata_10,type="Gini")
aux <- varimp(fit_pata_rf_10)
varimp_pata_10 <- data.frame(Variable=names(aux), Importancia=aux)

varimp_pata <- data.frame(Trimestre=c(rep(1,8),rep(2,8),rep(3,8),rep(4,8),rep(5,8),
                                     rep(6,8),rep(7,8),rep(8,8),rep(9,8),rep(10,8)),
                         Variable=c(as.character(varimp_pata_1$Variable),as.character(varimp_pata_2$Variable),
                                    as.character(varimp_pata_3$Variable),as.character(varimp_pata_4$Variable),
                                    as.character(varimp_pata_5$Variable),as.character(varimp_pata_6$Variable),
                                    as.character(varimp_pata_7$Variable),as.character(varimp_pata_8$Variable),
                                    as.character(varimp_pata_9$Variable),as.character(varimp_pata_10$Variable)),
                         Importancia=c(varimp_pata_1$Importancia,varimp_pata_2$Importancia,
                                       varimp_pata_3$Importancia,varimp_pata_4$Importancia,
                                       varimp_pata_5$Importancia,varimp_pata_6$Importancia,
                                       varimp_pata_7$Importancia,varimp_pata_8$Importancia,
                                       varimp_pata_9$Importancia,varimp_pata_10$Importancia))
rm(varimp_pata_1, varimp_pata_2,varimp_pata_3, varimp_pata_4,varimp_pata_5, varimp_pata_6,
   varimp_pata_7, varimp_pata_8,varimp_pata_9, varimp_pata_10)
desigualdad_pata <- data.frame(Trimestre=c(1:10),
                              Gini=c(desigualdad_pata_1, desigualdad_pata_2, desigualdad_pata_3,
                                     desigualdad_pata_4, desigualdad_pata_5, desigualdad_pata_6,
                                     desigualdad_pata_7, desigualdad_pata_8, desigualdad_pata_9,
                                     desigualdad_pata_10))
rm(desigualdad_pata_1, desigualdad_pata_2, desigualdad_pata_3,
   desigualdad_pata_4, desigualdad_pata_5, desigualdad_pata_6,
   desigualdad_pata_7, desigualdad_pata_8, desigualdad_pata_9,
   desigualdad_pata_10)
save.image("trimestrales_pata.Rda")

load("trimestrales_gba.Rda")
plot(fit_gba_10, type="simple")

rm(list=ls())
load("trimestrales_noa.Rda")
plot(fit_noa_10, type="simple")

rm(list=ls())
load("trimestrales_nea.Rda")
plot(fit_nea_10, type="simple")

rm(list=ls())
load("trimestrales_cuyo.Rda")
plot(fit_cuyo_10, type="simple")

rm(list=ls())
load("trimestrales_pamp.Rda")
plot(fit_pamp_10, type="simple")

rm(list=ls())
load("trimestrales_pata.Rda")
plot(fit_pata_10, type="simple")

rm(list=ls())
load("trimestrales_gba.Rda")
desigualdad_gba$Region <- "GBA"
save(desigualdad_gba, file="ginis.Rda")
rm(list=ls())
load("trimestrales_noa.Rda")
load("ginis.Rda")
desigualdad_noa$Region <- "NOA"
desigualdad <- rbind(desigualdad_gba, desigualdad_noa)
save(desigualdad, file="ginis.Rda")
rm(list=ls())
load("trimestrales_nea.Rda")
load("ginis.Rda")
desigualdad_nea$Region <- "NEA"
desigualdad <- rbind(desigualdad, desigualdad_nea)
save(desigualdad, file="ginis.Rda")
rm(list=ls())
load("trimestrales_cuyo.Rda")
load("ginis.Rda")
desigualdad_cuyo$Region <- "Cuyo"
desigualdad <- rbind(desigualdad, desigualdad_cuyo)
save(desigualdad, file="ginis.Rda")
rm(list=ls())
load("trimestrales_pamp.Rda")
load("ginis.Rda")
desigualdad_pamp$Region <- "Pampeana"
desigualdad <- rbind(desigualdad, desigualdad_pamp)
save(desigualdad, file="ginis.Rda")
rm(list=ls())
load("trimestrales_pata.Rda")
load("ginis.Rda")
desigualdad_pata$Region <- "Patagonia"
desigualdad <- rbind(desigualdad, desigualdad_pata)
save(desigualdad, file="ginis.Rda")

rm(list=ls())
load("ginis.Rda")
desigualdad %>% ggplot(aes(x=Trimestre, y=Gini, color=Region)) +
  geom_line(size=2) +
  theme_classic() +
  scale_x_continuous(breaks = c(1:10)) +
  theme(text = element_text(size=30))

desigualdad %>% ggplot(aes(x=Region, y=Gini)) +
  geom_boxplot() +
  theme_classic() +
  theme(text = element_text(size=30))

desigualdad %>% group_by(Region) %>% summarise(media=mean(Gini), sd=sd(Gini))

rm(list=ls())
load("trimestrales_gba.Rda")
varimp_gba %>%
  group_by(Trimestre) %>%
  mutate(Importancia=100*Importancia/max(Importancia)) %>%
  ungroup() %>%
  group_by(Variable) %>%
  summarise(Importancia=mean(Importancia)) %>%
  ungroup() %>%
  ggplot(aes(x=Variable, y=Importancia)) +
  geom_col(fill="blue") +
  coord_flip() +
  labs(title= "GBA") +
  theme_classic() +
  theme(text = element_text(size=30))
rm(list=ls())
load("trimestrales_noa.Rda")
varimp_noa %>%
  group_by(Trimestre) %>%
  mutate(Importancia=100*Importancia/max(Importancia)) %>%
  ungroup() %>%
  group_by(Variable) %>%
  summarise(Importancia=mean(Importancia)) %>%
  ungroup() %>%
  ggplot(aes(x=Variable, y=Importancia)) +
  geom_col(fill="blue") +
  coord_flip() +
  labs(title= "NOA") +
  theme_classic() +
  theme(text = element_text(size=30))
rm(list=ls())
load("trimestrales_nea.Rda")
varimp_nea %>%
  group_by(Trimestre) %>%
  mutate(Importancia=100*Importancia/max(Importancia)) %>%
  ungroup() %>%
  group_by(Variable) %>%
  summarise(Importancia=mean(Importancia)) %>%
  ungroup() %>%
  ggplot(aes(x=Variable, y=Importancia)) +
  geom_col(fill="blue") +
  coord_flip() +
  labs(title= "NEA") +
  theme_classic() +
  theme(text = element_text(size=30))
rm(list=ls())
load("trimestrales_cuyo.Rda")
varimp_cuyo %>%
  group_by(Trimestre) %>%
  mutate(Importancia=100*Importancia/max(Importancia)) %>%
  ungroup() %>%
  group_by(Variable) %>%
  summarise(Importancia=mean(Importancia)) %>%
  ungroup() %>%
  ggplot(aes(x=Variable, y=Importancia)) +
  geom_col(fill="blue") +
  coord_flip() +
  labs(title= "Cuyo") +
  theme_classic() +
  theme(text = element_text(size=30))
rm(list=ls())
load("trimestrales_pamp.Rda")
varimp_pamp %>%
  group_by(Trimestre) %>%
  mutate(Importancia=100*Importancia/max(Importancia)) %>%
  ungroup() %>%
  group_by(Variable) %>%
  summarise(Importancia=mean(Importancia)) %>%
  ungroup() %>%
  ggplot(aes(x=Variable, y=Importancia)) +
  geom_col(fill="blue") +
  coord_flip() +
  labs(title= "Pampeana") +
  theme_classic() +
  theme(text = element_text(size=30))
rm(list=ls())
load("trimestrales_pata.Rda")
varimp_pata %>%
  group_by(Trimestre) %>%
  mutate(Importancia=100*Importancia/max(Importancia)) %>%
  ungroup() %>%
  group_by(Variable) %>%
  summarise(Importancia=mean(Importancia)) %>%
  ungroup() %>%
  ggplot(aes(x=Variable, y=Importancia)) +
  geom_col(fill="blue") +
  coord_flip() +
  labs(title= "Patagonia") +
  theme_classic() +
  theme(text = element_text(size=30))