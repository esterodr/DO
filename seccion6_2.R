load("EPH_final.Rda")
library(tidyverse)
library(party)
library(ineq)

rm(ingreso_medio)

base_intergeneracional <- bases_eph

names(actualizar_ingreso) <- c("Trimestre", "factor")
base_intergeneracional <- base_intergeneracional %>%
  left_join(actualizar_ingreso, by="Trimestre") %>%
  mutate(Ingreso=Ingreso*factor)

base_intergeneracional <- base_intergeneracional %>%
  filter(!is.na(NIVEL_ED_p)) %>%
  select(-factor) %>%
  group_by(CODUSU, NRO_HOGAR, COMPONENTE) %>%
  filter(Trimestre==max(Trimestre)) %>%
  ungroup() %>%
  select(-c(NRO_HOGAR, COMPONENTE, CODUSU, Trimestre))

base_intergeneracional %>%
  group_by(REGION) %>%
  summarise(n())

base <- base_intergeneracional

fit <- ctree(Ingreso ~ ., data = base, controls = ctree_control(mincriterion = 0.99))
plot(fit, type="simple")

set.seed(1)
fit_rf <- cforest(Ingreso ~ ., data = base, controls=cforest_control(ntree = 500, mincriterion = 0.99))
y_hat <- predict(fit_rf, base, OOB=TRUE)
desigualdad <- ineq(y_hat,type="Gini")
aux <- varimp(fit_rf)
varimp <- data.frame(Variable=names(aux), Importancia=aux)
varimp %>%
  mutate(Importancia=100*Importancia/max(Importancia)) %>%
  mutate(Variable = fct_reorder(Variable, Importancia)) %>%
  ggplot(aes(x=Variable, y=Importancia)) +
  geom_col(fill="cyan") +
  labs(x = NULL, y = "Importancia de las variables") + 
  coord_flip() +
  theme_classic() +
  theme(text = element_text(size=30))

##GBA
base <- base_intergeneracional %>% filter(REGION=="1") %>% select(-REGION)
fit_gba <- ctree(Ingreso ~ ., data = base, controls = ctree_control(mincriterion = 0.90))
plot(fit_gba, type="simple")
set.seed(1)
fit_gba_rf <- cforest(Ingreso ~ ., data = base, controls=cforest_control(ntree = 500, mincriterion = 0.99))
y_hat_gba <- predict(fit_gba_rf, base, OOB=TRUE)
desigualdad_gba <- ineq(y_hat_gba,type="Gini")
aux <- varimp(fit_gba_rf)
varimp_gba <- data.frame(Variable=names(aux), Importancia=aux)
varimp_gba %>%
  mutate(Importancia=100*Importancia/max(Importancia)) %>%
  mutate(Variable = fct_reorder(Variable, Importancia)) %>%
  ggplot(aes(x=Variable, y=Importancia)) +
  geom_col(fill="cyan") +
  labs(x = NULL, y = NULL) + 
  coord_flip() +
  theme_classic() +
  theme(text = element_text(size=45))

##NOA
base <- base_intergeneracional %>% filter(REGION=="40") %>% select(-REGION)
fit_noa <- ctree(Ingreso ~ ., data = base, controls = ctree_control(mincriterion = 0.90))
plot(fit_noa, type="simple")
set.seed(1)
fit_noa_rf <- cforest(Ingreso ~ ., data = base, controls=cforest_control(ntree = 500, mincriterion = 0.99))
y_hat_noa <- predict(fit_noa_rf, base, OOB=TRUE)
desigualdad_noa <- ineq(y_hat_noa,type="Gini")
aux <- varimp(fit_noa_rf)
varimp_noa <- data.frame(Variable=names(aux), Importancia=aux)
varimp_noa %>%
  mutate(Importancia=100*Importancia/max(Importancia)) %>%
  mutate(Variable = fct_reorder(Variable, Importancia)) %>%
  ggplot(aes(x=Variable, y=Importancia)) +
  geom_col(fill="cyan") +
  labs(x = NULL, y = NULL) + 
  coord_flip() +
  theme_classic() +
  theme(text = element_text(size=45))

##NEA
base <- base_intergeneracional %>% filter(REGION=="41") %>% select(-REGION)
fit_nea <- ctree(Ingreso ~ ., data = base, controls = ctree_control(mincriterion = 0.90))
plot(fit_nea, type="simple")
set.seed(1)
fit_nea_rf <- cforest(Ingreso ~ ., data = base, controls=cforest_control(ntree = 500, mincriterion = 0.99))
y_hat_nea <- predict(fit_nea_rf, base, OOB=TRUE)
desigualdad_nea <- ineq(y_hat_nea,type="Gini")
aux <- varimp(fit_nea_rf)
varimp_nea <- data.frame(Variable=names(aux), Importancia=aux)
varimp_nea %>%
  mutate(Importancia=100*Importancia/max(Importancia)) %>%
  mutate(Variable = fct_reorder(Variable, Importancia)) %>%
  ggplot(aes(x=Variable, y=Importancia)) +
  geom_col(fill="cyan") +
  labs(x = NULL, y = NULL) + 
  coord_flip() +
  theme_classic() +
  theme(text = element_text(size=45))

##Cuyo
base <- base_intergeneracional %>% filter(REGION=="42") %>% select(-REGION)
fit_cuyo <- ctree(Ingreso ~ ., data = base, controls = ctree_control(mincriterion = 0.90))
plot(fit_cuyo, type="simple")
set.seed(1)
fit_cuyo_rf <- cforest(Ingreso ~ ., data = base, controls=cforest_control(ntree = 500, mincriterion = 0.99))
y_hat_cuyo <- predict(fit_cuyo_rf, base, OOB=TRUE)
desigualdad_cuyo <- ineq(y_hat_cuyo,type="Gini")
aux <- varimp(fit_cuyo_rf)
varimp_cuyo <- data.frame(Variable=names(aux), Importancia=aux)
varimp_cuyo %>%
  mutate(Importancia=100*Importancia/max(Importancia)) %>%
  mutate(Variable = fct_reorder(Variable, Importancia)) %>%
  ggplot(aes(x=Variable, y=Importancia)) +
  geom_col(fill="cyan") +
  labs(x = NULL, y = NULL) + 
  coord_flip() +
  theme_classic() +
  theme(text = element_text(size=45))

##Pampeana
base <- base_intergeneracional %>% filter(REGION=="43") %>% select(-REGION)
fit_pamp <- ctree(Ingreso ~ ., data = base, controls = ctree_control(mincriterion = 0.90))
plot(fit_pamp, type="simple")
set.seed(1)
fit_pamp_rf <- cforest(Ingreso ~ ., data = base, controls=cforest_control(ntree = 500, mincriterion = 0.99))
y_hat_pamp <- predict(fit_pamp_rf, base, OOB=TRUE)
desigualdad_pamp <- ineq(y_hat_pamp,type="Gini")
aux <- varimp(fit_pamp_rf)
varimp_pamp <- data.frame(Variable=names(aux), Importancia=aux)
varimp_pamp %>%
  mutate(Importancia=100*Importancia/max(Importancia)) %>%
  mutate(Variable = fct_reorder(Variable, Importancia)) %>%
  ggplot(aes(x=Variable, y=Importancia)) +
  geom_col(fill="cyan") +
  labs(x = NULL, y = NULL) + 
  coord_flip() +
  theme_classic() +
  theme(text = element_text(size=45))

##Patagonia
base <- base_intergeneracional %>% filter(REGION=="44") %>% select(-REGION)
fit_pata <- ctree(Ingreso ~ ., data = base, controls = ctree_control(mincriterion = 0.90))
plot(fit_pata, type="simple")
set.seed(1)
fit_pata_rf <- cforest(Ingreso ~ ., data = base, controls=cforest_control(ntree = 500, mincriterion = 0.99))
y_hat_pata <- predict(fit_pata_rf, base, OOB=TRUE)
desigualdad_pata <- ineq(y_hat_pata,type="Gini")
aux <- varimp(fit_pata_rf)
varimp_pata <- data.frame(Variable=names(aux), Importancia=aux)
varimp_pata %>%
  mutate(Importancia=100*Importancia/max(Importancia)) %>%
  mutate(Variable = fct_reorder(Variable, Importancia)) %>%
  ggplot(aes(x=Variable, y=Importancia)) +
  geom_col(fill="cyan") +
  labs(x = NULL, y = NULL) + 
  coord_flip() +
  theme_classic() +
  theme(text = element_text(size=45))
