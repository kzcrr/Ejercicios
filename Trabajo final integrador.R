#LIBRERÍAS

library(tidyverse)
library(sf)
library(patchwork)
library(tidymodels)
library(GDAtools) 
library(ggdendro)
library(dendextend)
library(dplyr)
library(kknn)
library(ggplot2)
library(themis)
library(vip)
library(parsnip)
library(baguette)
library(rpart.plot)
library(readxl)
library(tictoc)
library(reldist)

#PREPROCESAMIENTO

#1.BASES

vuln <- read_csv('./data/vuln_sanit.csv')

trans <- read_xlsx('./data/trans.xlsx')

trans <- trans %>% rename(link= `Código de radio...1`, dist_transporte=`Hogares con transporte público a menos de 300 metros`)

cond_vida<- read_csv('./data/radios_hogar.csv')

cond_vida  <- cond_vida %>% rename(link=radio)

eph <- read_csv('./data/env_eph.csv') 

#como notamos inconsictencias en la base de la eph, antes de joinearla con el resto de las bases tuvimos que realizar algunos cambios:
#los radios considerados  NAs en "aglomerado" que pertenecian a CABA fueron consideradas zonas urbanas, por esto indicamos que aquellos casos que sean na en aglomerado
#y na en localidade sean categorizados como "urbano" (incluimos como condición la variable localidade pq esta indica si el radio es de zona urbana/rural) 

sum(is.na(eph$localidade))

eph$aglomerado <- with(eph, ifelse(is.na(aglomerado) & is.na(localidade), "urbano", aglomerado))

radios_eph <- eph %>% 
  mutate(link = paste0(codprov, coddepto, frac2010, radio2010)) %>% 
  select(id: aglomerado, link)

data <- vuln %>% left_join(cond_vida) %>% left_join(radios_eph)

poblacion <- read_xlsx('./data/ind_radios.xlsx')

poblacion  <- poblacion %>% rename(link= `Código de radio.`)

data <- data %>% left_join(poblacion, by = "link")

data <- data %>% 
  left_join(trans %>% select(link, dist_transporte), by = "link")


#2. VARIABLES

#2.1 Hogares con transporte público a menos de 300 metros

summary(data$dist_transporte)

data$missing_hogares <- ifelse(is.na(data$dist_transporte), "Missing", "Non-missing")

ggplot(data, aes(x = missing_hogares, fill = missing_hogares)) +
  geom_bar() +
  scale_fill_manual(values = c("Missing" = "red", "Non-missing" = "green")) +
  labs(x = "Valores faltantes en Hogares", y = "Frecuencia", fill = "Estado") +
  ggtitle("Distribución de valores faltantes en Hogares") +
  theme_minimal()

ggplot(data, aes(x = missing_hogares, fill = missing_hogares)) +
  geom_bar() +
  facet_wrap(~ provincia ) +
  scale_fill_manual(values = c("Missing" = "red", "Non-missing" = "green")) +
  labs(x = "Valores faltantes en Hogares", y = "Frecuencia", fill = "Estado") +
  ggtitle("Distribución de valores faltantes en Hogares por provincia") +
  theme_minimal()

  data %>%
  drop_na %>%
  group_by(provincia) %>%
  summarize(media_distancias = mean(dist_transporte))

#observamos que los NAs no presentaban patrones sistemáticos y no se encontraban concentrados en determinadas categorías.si bien habían más casos de NAs en
#BA y Corrientes, estas provincias no concentraban la mayorías o casi todos lo casos (solo 59 de 125). por esto, tomamos la decisión de imputar los
#NAs según la media de "Hogares con transporte público a menos de 300 metros" para cada provincia específica
 
data %>%
  group_by(provincia) %>%
  summarize(NAs = sum(is.na(dist_transporte)))

#imputación NAs por media de cada provincia

media_por_provincia <- data %>%
  group_by(provincia) %>%
  summarize(media_hogares_tp = mean(dist_transporte, na.rm = TRUE))

data <- data %>%
  left_join(media_por_provincia, by = "provincia") %>%
  mutate(dist_transporte = ifelse(is.na(dist_transporte), media_hogares_tp, dist_transporte)) %>%
  select(-media_hogares_tp)


#2.2 Aglomerado urbano/zona rural los radios considerados  NAs en "aglomerado" fueron indicados como zona rural y el resto como zona urbana

data$aglomerado <- ifelse(is.na(data$aglomerado), "zona rural", "zona urbana")

#2.3 Densidad poblacional 

data <- data %>% st_as_sf(wkt = "geometry")

data <- data %>%
  mutate(area = st_area(.) / 1000000) %>% 
  mutate(densidad = `Población total` / area)

sum(is.na(data$densidad))

#como son pocos casos de NAs los dropeamos

densidad_complete <- complete.cases(data$densidad)  
data <- data[densidad_complete, ] 

#2.4 Imputación de missing en variables de accesibilidad

data <- data %>% mutate(across(starts_with("tpo_"), ~if_else(is.na(.x), 
                                                             quantile(.x, prob=0.9, na.rm=TRUE),
                                                             .x)))

#3. SPLIT

data  <- data %>%
  st_set_geometry(NULL)

set.seed(123)
split <- initial_split(data)
train <- training(split)
test <- testing(split)

#4. PCA sobre train
#4.1 pca condiciones habitacionales

recipe_pca <- train %>% 
  select(banio_uso_exclusivo:inodoro_con_cadena) %>%
  recipe(~.) %>%
  step_normalize(all_numeric()) %>%
  step_pca(all_numeric(), num_comp= 2)

pca_estimates <-  prep(recipe_pca, training = train)
pca_data <- bake(pca_estimates, train)

tidy(pca_estimates, 2, type="coef") %>%
  filter(component %in% c("PC1", "PC2")) %>%
  ggplot(aes(value, terms, fill=terms)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label=round(value,2))) +
  labs(title="Cargas factoriales (comp. 1 y 2)",
       x="Valor",
       y="Variable") + 
  facet_wrap(~component, nrow=1) +
  theme_minimal() 

tidy(pca_estimates, 2, type="coef") %>%
  filter(component %in% c("PC1", "PC2")) %>%
  ggplot(aes(-1*value, terms, fill=terms)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label=round(value,2))) +
  labs(title="Cargas factoriales (comp. 1 y 2)",
       x="Valor",
       y="Variable") + 
  facet_wrap(~component, nrow=1) +
  theme_minimal()

tidy(pca_estimates, 2, type="variance") %>%
  filter(terms=="percent variance") %>%
  mutate(component=paste0("PC", component)) %>%
  ggplot(aes(x=component,y=value,group=terms)) +
  geom_col() +
  ylim(0,100) +
  labs(title="Porcentaje de varianza",
       x="Componentes",
       y="Valor") + 
  theme_minimal() 

train <- train  %>%
  bind_cols(pca_data) %>% 
  rename(Condiciones_Habitacionales = PC1) %>% 
  select(-PC2) %>%
  mutate(Condiciones_Habitacionales = -1*Condiciones_Habitacionales)

#4.2 pca acceso a servicios de salud

train <- train  %>%
  select(link, aglomerado, densidad, dist_transporte, tpo_ctro_salud, tpo_hospital, tpo_posta, Condiciones_Habitacionales)

recipe_pca <- train %>% 
  select(tpo_hospital, tpo_ctro_salud, tpo_posta) %>%
  recipe(~.) %>%
  step_normalize(all_numeric()) %>%
  step_pca(all_numeric(), num_comp= 2)

pca_estimates <-  prep(recipe_pca, training =  train %>%
                         select(tpo_hospital, tpo_ctro_salud, tpo_posta))

pca_data <- bake(pca_estimates,  train %>%
                   select(tpo_hospital, tpo_ctro_salud, tpo_posta))

tidy(pca_estimates, 2, type="coef") %>%
  filter(component %in% c("PC1", "PC2")) %>%
  ggplot(aes(value, terms, fill=terms)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label=round(value,2))) +
  labs(title="Cargas factoriales (comp. 1 y 2)",
       x="Valor",
       y="Variable") + 
  facet_wrap(~component, nrow=1) +
  theme_minimal() 

tidy(pca_estimates, 2, type="variance") %>%
  filter(terms=="percent variance") %>%
  mutate(component=paste0("PC", component)) %>%
  ggplot(aes(x=component,y=value,group=terms)) +
  geom_col() +
  ylim(0,100) +
  labs(title="Porcentaje de varianza",
       x="Componentes",
       y="Valor") + 
  theme_minimal()

train <- train %>% 
  bind_cols(pca_data) %>%
  rename(Acceso_Salud = PC1) %>%
  select(-PC2)


#MEDIANA

train$Acceso_Salud <- as.numeric(train$Acceso_Salud)
mediana_train <- median(train$Acceso_Salud)
hist(train$Acceso_Salud, main = "Histograma de Acceso a la Salud", xlab = "Valores", ylab = "Frecuencia")
abline(v = mediana_train, col = "red")
legend("topright", legend = "Mediana", col = "red", lwd = 1)


#acceso a salud en dos grupos

train <- train %>%
  mutate(acceso_dicot = ntile(Acceso_Salud, 2)) %>%
  mutate(acceso_dicot = as.factor(case_when(
    acceso_dicot == 1 ~ "1. Bajo",
    acceso_dicot == 2 ~ "2. Alto")
  ))


#5. PCA sobre test

#5.1 pca condiciones habitacionales

recipe_pca <- test %>% 
  select(banio_uso_exclusivo:inodoro_con_cadena) %>%
  recipe(~.) %>%
  step_normalize(all_numeric()) %>%
  step_pca(all_numeric(), num_comp= 2)

pca_estimates <-  prep(recipe_pca, training = test)
pca_data <- bake(pca_estimates, test)

tidy(pca_estimates, 2, type="coef") %>%
  filter(component %in% c("PC1", "PC2")) %>%
  ggplot(aes(value, terms, fill=terms)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label=round(value,2))) +
  labs(title="Cargas factoriales (comp. 1 y 2)",
       x="Valor",
       y="Variable") + 
  facet_wrap(~component, nrow=1) +
  theme_minimal() 

tidy(pca_estimates, 2, type="coef") %>%
  filter(component %in% c("PC1", "PC2")) %>%
  ggplot(aes(-1*value, terms, fill=terms)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label=round(value,2))) +
  labs(title="Cargas factoriales (comp. 1 y 2)",
       x="Valor",
       y="Variable") + 
  facet_wrap(~component, nrow=1) +
  theme_minimal()

tidy(pca_estimates, 2, type="variance") %>%
  filter(terms=="percent variance") %>%
  mutate(component=paste0("PC", component)) %>%
  ggplot(aes(x=component,y=value,group=terms)) +
  geom_col() +
  ylim(0,100) +
  labs(title="Porcentaje de varianza",
       x="Componentes",
       y="Valor") + 
  theme_minimal() 

test <- test  %>%
  bind_cols(pca_data) %>% 
  rename(Condiciones_Habitacionales = PC1) %>% 
  select(-PC2) %>%
  mutate(Condiciones_Habitacionales = -1*Condiciones_Habitacionales)

#5.2 pca acceso a servicios de salud

test <- test  %>%
  select(link, aglomerado, densidad, dist_transporte, tpo_ctro_salud, tpo_hospital, tpo_posta, Condiciones_Habitacionales)

recipe_pca <- test %>% 
  select(tpo_hospital, tpo_ctro_salud, tpo_posta) %>%
  recipe(~.) %>%
  step_normalize(all_numeric()) %>%
  step_pca(all_numeric(), num_comp= 2)

pca_estimates <-  prep(recipe_pca, training =  test %>% 
                         select(tpo_hospital, tpo_ctro_salud, tpo_posta))

pca_data <- bake(pca_estimates,  test %>%  
                   select(tpo_hospital, tpo_ctro_salud, tpo_posta))

tidy(pca_estimates, 2, type="coef") %>%
  filter(component %in% c("PC1", "PC2")) %>%
  ggplot(aes(value, terms, fill=terms)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label=round(value,2))) +
  labs(title="Cargas factoriales (comp. 1 y 2)",
       x="Valor",
       y="Variable") + 
  facet_wrap(~component, nrow=1) +
  theme_minimal() 

tidy(pca_estimates, 2, type="variance") %>%
  filter(terms=="percent variance") %>%
  mutate(component=paste0("PC", component)) %>%
  ggplot(aes(x=component,y=value,group=terms)) +
  geom_col() +
  ylim(0,100) +
  labs(title="Porcentaje de varianza",
       x="Componentes",
       y="Valor") + 
  theme_minimal()


test <- test %>% 
  bind_cols(pca_data)%>%
  rename(Acceso_Salud = PC1) %>%
  select(-PC2)

#acceso a salud en dos grupos

test <- test %>%
  mutate(acceso_dicot = ntile(Acceso_Salud, 2)) %>%
  mutate(acceso_dicot = as.factor(case_when(
    acceso_dicot == 1 ~ "1. Bajo",
    acceso_dicot == 2 ~ "2. Alto")
  ))



#6.train y test models

train_cart <- train %>% select(link, densidad, dist_transporte, Condiciones_Habitacionales, aglomerado, acceso_dicot) %>% drop_na()
train_cart <- train_cart %>%
  mutate(aglomerado=as.factor(aglomerado))


test_cart <- test %>% select(link, densidad, dist_transporte, Condiciones_Habitacionales, aglomerado, acceso_dicot) %>% drop_na()
test_cart <- test_cart %>%
  mutate(aglomerado=as.factor(aglomerado))

#7. ARBOL SIMPLE CART

recipe <- recipe(acceso_dicot ~ ., data = train_cart)%>%
  update_role(link, new_role = "id")

wf <- workflow() %>%
  add_recipe(recipe)

#hiperparámetros

tree_spec <- decision_tree(  
  cost_complexity = tune(),
  tree_depth = tune(),
  min_n = tune()
) %>%
  set_engine("rpart") %>%
  set_mode("classification")

tree_spec %>% translate()

tree_wf <- wf %>%
  add_model (tree_spec)

set.seed(1912)

tree_grid <- grid_regular(
  cost_complexity(),
  tree_depth( range = c(1, 15)),
  min_n(),
  levels = 4)

tree_grid

#cv

set.seed(789)
folds <- vfold_cv(train, v = 5)

tidy(folds)

#entrenamos

doParallel::registerDoParallel()
set.seed(345)
tree_rs <- tree_wf %>% 
  tune_grid(
    resamples = folds,
    grid = tree_grid,
    metrics = metric_set(roc_auc, precision, 
                         recall, f_meas)
  )


#metricas

collect_metrics(tree_rs)

autoplot(tree_rs) 

show_best(tree_rs, "roc_auc")

#mejor modelo

best_model <- select_best(tree_rs, "roc_auc")

final_tree <- finalize_model(tree_spec, best_model)

final_tree

final_fit <- tree_wf %>% update_model(final_tree) %>% fit(train_cart)

final_fit

write_rds(final_fit, './data/cart_final_train.rds') #guardamos el modelo final entrenado

extract_fit_parsnip(final_fit) %>%
  vip(geom = "col") + theme_minimal()

rpart.plot(extract_fit_parsnip(final_fit)$fit)

#TEST 
test_cart <- final_fit %>%
  predict(test_cart) %>%
  bind_cols(test_cart, .)

class_metrics <- metric_set(precision, accuracy, recall, f_meas)

class_metrics(test_cart, truth = acceso_dicot, estimate = .pred_class)

matriz_confusion <- conf_mat(test_cart, truth = acceso_dicot, estimate = .pred_class)

matriz_confusion

test_cart <- final_fit %>%
  predict(test_cart, type = "prob") %>%
  bind_cols(test_cart, .)

test_cart %>% select(acceso_dicot:.pred_class)


#curva roc

test_cart %>% 
  roc_curve(truth = acceso_dicot, `.pred_1. Bajo`) %>% 
  autoplot()

test_cart %>% 
  roc_curve(truth = acceso_dicot, `.pred_2. Alto`)%>% 
  autoplot()

test_cart %>% 
  roc_auc(truth = acceso_dicot, `.pred_1. Bajo`)
test_cart %>% 
  roc_auc(truth = acceso_dicot, `.pred_2. Alto`)
#Random Forest

Train_rf <- train %>% select(link, densidad, dist_transporte, Condiciones_Habitacionales, aglomerado, acceso_dicot) %>% drop_na()
Train_rf <- Train_rf %>%
  mutate(aglomerado=as.factor(aglomerado))

test_rf <- test %>% 
  select(link, densidad, dist_transporte, Condiciones_Habitacionales, aglomerado, acceso_dicot) %>%
  drop_na()

test_rf <- test_rf %>%
  mutate(aglomerado=as.factor(aglomerado))


recipe <- recipe(acceso_dicot ~ ., data = Train_rf)%>%
  update_role(link, new_role = "id") 

wf <- workflow() %>%
  add_recipe(recipe)

rf_spec <- rand_forest(
  mtry = tune(),
  min_n = tune()
) %>%
  set_mode("classification") %>%
  set_engine("ranger")

rf_spec <- rand_forest(
  trees = 800,
  mtry = tune(),
  min_n = tune()
) %>%
  set_mode("classification") %>%
  set_engine("ranger")

rf_spec %>% translate()

tree_rf <- wf %>%
  add_model(rf_spec)

wf <- workflow() %>%
  add_recipe(recipe) %>%
  add_model(rf_spec)


#Hiperparametros manuales

tree_grid <- expand.grid(
  mtry = seq(1,10,5),
  min_n = seq(1,20,5))

set.seed(1912)
folds <- vfold_cv(Train_rf, v = 5)

tuned_rf <- tune_grid(
  wf,
  resamples = folds,
  grid = tree_grid,
  metrics = metric_set(roc_auc, f_meas, precision, recall))

autoplot(tuned_rf)

best_model_tuned<-select_best(tuned_rf, "roc_auc")
final_model_rf_tuned <- finalize_model(rf_spec, best_model_tuned)

final_model_rf <- finalize_model(rf_spec, best_model_tuned)
final_fit_rf <- wf %>%
  update_model(final_model_rf) %>%
  fit(Train_rf)

write_rds(final_fit_rf, './data/rf_final_train.rds') #guardamos el arbol final

#Test arbol 

test_rf <- final_fit_rf %>%
  predict(test_rf) %>%
  bind_cols(., test_rf)

test_rf <- predict(final_fit_rf, test_rf, type = "prob") %>%
  bind_cols(test_rf, .)

class_metrics <- metric_set(accuracy, f_meas, precision, recall)

test_rf <- test_rf %>% 
  mutate(acceso_dicot = as.factor(acceso_dicot))

roc_auc(test_rf, truth = acceso_dicot, ".pred_1. Bajo") %>% 
  bind_rows(class_metrics(test_rf, truth = acceso_dicot, estimate = .pred_class))

#Boosting

#receta

train_boost <- train %>% select (link, aglomerado, densidad, acceso_dicot, Condiciones_Habitacionales, dist_transporte) %>%
  drop_na()

train_boost <- train_boost %>% 
  mutate(aglomerado=as.factor(aglomerado))

test_boost <- test %>%
  select (link, dist_transporte, Condiciones_Habitacionales, acceso_dicot, densidad, aglomerado) %>%
  drop_na()

test_boost <- test_boost %>% 
  mutate(aglomerado=as.factor(aglomerado))

#Hacemos la recipe

recipe <- recipe(acceso_dicot ~ ., data = train_boost)%>%
  update_role(link, new_role = "id") 

wf <- workflow() %>%
  add_recipe(recipe)

##

bt_spec <- boost_tree(
  trees = tune(), 
  min_n = tune ()
) %>% 
  set_engine("C5.0") %>%
  set_mode("classification")

bt_spec %>% translate()

tune_wf <- wf %>%
  add_model(bt_spec)

set.seed(912)

folds <- vfold_cv(train)

tictoc::tic()
tune_params <- tune_wf %>%
  tune_grid(folds,
            metrics = metric_set(precision, recall,
                                 roc_auc, f_meas),
            grid = 15)
tictoc::toc() 

best_model <- select_best(tune_params, metric = "roc_auc")
final_model <- finalize_model(bt_spec, best_model)

tree_boost <- wf %>%
  update_model(final_model)

boost_final_train <- tree_boost %>% fit(train_boost)

boost_final_train

write_rds(boost_final_train, './data/boost_final_train.rds') 

#Test

boost_test <- boost_final_train %>%
  predict(test_boost) %>%
  bind_cols(., test_boost)

boost_test <- predict(boost_final_train, test_boost, type = "prob") %>%
  bind_cols(boost_test, .)

class_metrics <- metric_set(precision, recall,
                            accuracy, f_meas)

boost_metrics <- roc_auc(boost_test, truth = acceso_dicot, ".pred_1. Bajo") %>%
  bind_rows(class_metrics(boost_test, truth = acceso_dicot, estimate = .pred_class)) %>%
  mutate(model = "Boosting")

boost_metrics


#ensamble

datasets <- list(test_cart, boost_test, test_rf)

class_metrics <- metric_set(precision, recall,
                            accuracy, f_meas)

metricas <- function(dataset, model_name){
  
  metrics <-  roc_auc(dataset, truth = acceso_dicot, ".pred_1. Bajo") %>%
    bind_rows(class_metrics(dataset, truth = acceso_dicot, estimate = .pred_class))
  
  return(metrics)
}

metrics_eval <- datasets %>% 
  map_dfr(metricas, .id = "model")

metrics_eval <- metrics_eval %>% 
  mutate_at(vars(model),
            ~as.factor(
              case_when(
                . %in% "1" ~ "Árbol de decisión",
                . %in% "2" ~ "Boosting",
                . %in% "3" ~ "Random Forest")
            ))

ggplot(metrics_eval, aes(x = .metric, y = .estimate, fill = model))+
  geom_col(position = "dodge")+
  scale_fill_viridis_d()+
  theme_minimal()

####################################################################################

#MAPAS


#cargamos de nuevo las bases porque vamos a usar variables que no contienen las recategorizaciones hechas para los árboles


vuln <- read_csv('./data/vuln_sanit.csv')

trans <- read_xlsx('./data/trans.xlsx')

trans <- trans %>% rename(link= `Código de radio...1`, dist_transporte=`Hogares con transporte público a menos de 300 metros`)

cond_vida<- read_csv('./data/radios_hogar.csv')

cond_vida  <- cond_vida %>% rename(link=radio)

eph <- read_csv('./data/env_eph.csv') 

eph$aglomerado <- with(eph, ifelse(is.na(aglomerado) & is.na(localidade), "urbano", aglomerado))

radios_eph <- eph %>% 
  mutate(link = paste0(codprov, coddepto, frac2010, radio2010)) %>% 
  select(id: aglomerado, link)

data <- vuln %>% left_join(cond_vida) %>% left_join(radios_eph)

poblacion <- read_xlsx('./data/ind_radios.xlsx')

poblacion  <- poblacion %>% rename(link= `Código de radio.`)

data <- data %>% left_join(poblacion, by = "link")

data <- data %>% 
  left_join(trans %>% select(link, dist_transporte), by = "link")

#imputación de missing en variables de accesibilidad

data <- data %>% mutate(across(starts_with("tpo_"), ~if_else(is.na(.x), 
                                                             quantile(.x, prob=0.9, na.rm=TRUE),
                                                             .x)))
#PCA acceso sobre toda la base completa

data <- data  %>%
  select(link, aglomerado, provincia, tpo_ctro_salud, tpo_hospital, tpo_posta)

recipe_pca <- data %>% 
  select(tpo_hospital, tpo_ctro_salud, tpo_posta) %>%
  recipe(~.) %>%
  step_normalize(all_numeric()) %>%
  step_pca(all_numeric(), num_comp= 2)

pca_estimates <-  prep(recipe_pca, training =  data %>%  
                         select(tpo_hospital, tpo_ctro_salud, tpo_posta))
pca_data <- bake(pca_estimates,  data %>% 
                   select(tpo_hospital, tpo_ctro_salud, tpo_posta))

tidy(pca_estimates, 2, type="coef") %>%
  filter(component %in% c("PC1", "PC2")) %>%
  ggplot(aes(value, terms, fill=terms)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label=round(value,2))) +
  labs(title="Cargas factoriales (comp. 1 y 2)",
       x="Valor",
       y="Variable") + 
  facet_wrap(~component, nrow=1) +
  theme_minimal() 

tidy(pca_estimates, 2, type="variance") %>%
  filter(terms=="percent variance") %>%
  mutate(component=paste0("PC", component)) %>%
  ggplot(aes(x=component,y=value,group=terms)) +
  geom_col() +
  ylim(0,100) +
  labs(title="Porcentaje de varianza",
       x="Componentes",
       y="Valor") + 
  theme_minimal()


data <- data %>%  
  bind_cols(pca_data)%>%
  rename(Acceso_Salud = PC1) %>%
  select(-PC2)

#cargamos base leída como sf 
vuln_2 <- read_sf('./data/vuln_sanit.csv')

vuln_2 <- vuln_2 %>% 
  select(link, geometry)

data <- data %>% left_join(vuln_2, by = "link")


sf <- st_as_sf(data, wkt = "geometry")

#GINI por provincia

data %>%
  group_by(provincia) %>%
  summarize(trans_acceso_Salud = Acceso_Salud - min(Acceso_Salud) + 1) %>%
  summarise(gini_index = gini(trans_acceso_Salud))


#MAPAS CABA

mapa_caba_centros <- ggplot() +
  geom_sf(data = sf[sf$provincia == "Ciudad Autónoma de Buenos Aires", ],
          aes(fill = tpo_ctro_salud, color = tpo_ctro_salud, geometry = geometry),
          size = 0.2) +
  coord_sf() +
  scale_fill_viridis_c(na.value = "black") +
  scale_color_viridis_c(na.value = "black", guide = guide_colorbar(title = "Distancia a centros de salud")) +
  labs(title = "Rango de distancia a centros de salud en CABA") +
  labs(fill = "Distancia a centros de salud") +
  theme_minimal()

mapa_caba_posta <- ggplot() +
  geom_sf(data = sf[sf$provincia == "Ciudad Autónoma de Buenos Aires", ],
          aes(fill = tpo_posta, color = tpo_posta, geometry = geometry),
          size = 0.2) +
  coord_sf() +
  scale_fill_viridis_c(na.value = "black") +
  scale_color_viridis_c(na.value = "black", guide = guide_colorbar(title = "Distancia a postas sanitarias")) +
  labs(title = "Rango de distancia a postas sanitarias en CABA") +
  labs(fill = "Distancia a postas sanitarias") +
  theme_minimal()


mapa_caba_hospital <- ggplot() +
  geom_sf(data = sf[sf$provincia == "Ciudad Autónoma de Buenos Aires", ],
          aes(fill = tpo_hospital, color = tpo_hospital, geometry = geometry),
          size = 0.2) +
  coord_sf() +
  scale_fill_viridis_c(na.value = "black") +
  scale_color_viridis_c(na.value = "black", guide = guide_colorbar(title = "Distancia a hospitales")) +
  labs(title = "Rango de distancia a hospitales en CABA") +
  labs(fill = "Distancia a hospitales") +
  theme_minimal()



mapa_caba <- ggplot() +
  geom_sf(data = sf[sf$provincia == "Ciudad Autónoma de Buenos Aires", ],
          aes(fill = Acceso_Salud, color = Acceso_Salud, geometry = geometry),
          size = 0.2) +
  coord_sf() +
  scale_fill_viridis_c(na.value = "black") +
  scale_color_viridis_c(na.value = "black", guide = guide_colorbar(title = "Nivel de acceso"))+
  labs(title = "Acceso a servicios de salud en CABA") +
  labs(fill = "Nivel de acceso") +
  theme_minimal()


mapas_caba <- mapa_caba + mapa_caba_hospital + mapa_caba_centros + mapa_caba_posta + plot_layout(ncol = 4, nrow = 1)



#MAPA GBA


mapa_gba <- ggplot() +
  geom_sf(data = sf[sf$aglomerado == "Gran Buenos Aires", ],
          aes(fill = Acceso_Salud, color = Acceso_Salud, geometry = geometry),
          size = 0.2) +
  coord_sf() +
  scale_fill_viridis_c(na.value = "black") +
  scale_color_viridis_c(na.value = "black", guide = guide_colorbar(title = "Nivel de acceso"))+
  labs(title = "Acceso a servicios de salud en GBA") +
  labs(fill = "Nivel de acceso") +
  theme_minimal()



#MAPAS NOA, nivel acceso



mapa_chaco <- ggplot() +
  geom_sf(data = sf[sf$provincia == "Chaco", ],
          aes(fill = Acceso_Salud, color = Acceso_Salud, geometry = geometry),
          size = 0.2) +
  coord_sf() +
  scale_fill_viridis_c(na.value = "black") +
  scale_color_viridis_c(na.value = "black", guide = guide_colorbar(title = "Nivel de acceso"))+
  labs(title = "Acceso a servicios de salud en Chaco") +
  labs(fill = "Nivel de acceso") +
  theme_minimal()



mapa_mis <- ggplot() +
  geom_sf(data = sf[sf$provincia == "Misiones", ],
          aes(fill = Acceso_Salud, color = Acceso_Salud, geometry = geometry),
          size = 0.2) +
  coord_sf() +
  scale_fill_viridis_c(na.value = "black") +
  scale_color_viridis_c(na.value = "black", guide = guide_colorbar(title = "Nivel de acceso"))+
  labs(title = "Acceso a servicios de salud en Misiones") +
  labs(fill = "Nivel de acceso") +
  theme_minimal()



mapa_corr <- ggplot() +
  geom_sf(data = sf[sf$provincia == "Corrientes", ],
          aes(fill = Acceso_Salud, color = Acceso_Salud, geometry = geometry),
          size = 0.2) +
  coord_sf() +
  scale_fill_viridis_c(na.value = "black") +
  scale_color_viridis_c(na.value = "black", guide = guide_colorbar(title = "Nivel de acceso"))+
  labs(title = "Acceso a servicios de salud en Corrientes") +
  labs(fill = "Nivel de acceso") +
  theme_minimal()


mapa_for <- ggplot() +
  geom_sf(data = sf[sf$provincia == "Formosa", ],
          aes(fill = Acceso_Salud, color = Acceso_Salud, geometry = geometry),
          size = 0.2) +
  coord_sf() +
  scale_fill_viridis_c(na.value = "black") +
  scale_color_viridis_c(na.value = "black", guide = guide_colorbar(title = "Nivel de acceso"))+
  labs(title = "Acceso a servicios de salud en Formosa") +
  labs(fill = "Nivel de acceso") +
  theme_minimal()

mapas_nea <- mapa_for + mapa_chaco + mapa_corr + mapa_mis + plot_layout(ncol = 4, nrow = 1)



## alto

buenos <- ggplot() +
  geom_sf(data = sf[sf$provincia == "Buenos Aires", ],
          aes(fill = Acceso_Salud, color = Acceso_Salud, geometry = geometry),
          size = 0.2) +
  coord_sf() +
  scale_fill_viridis_c(na.value = "black") +
  scale_color_viridis_c(na.value = "black", guide = guide_colorbar(title = "Nivel de acceso"))+
  labs(title = "Acceso a servicios de salud en Buenos Aires") +
  labs(fill = "Nivel de acceso") +
  theme_minimal()

cata <- ggplot() +
  geom_sf(data = sf[sf$provincia == "Catamarca", ],
          aes(fill = Acceso_Salud, color = Acceso_Salud, geometry = geometry),
          size = 0.2) +
  coord_sf() +
  scale_fill_viridis_c(na.value = "black") +
  scale_color_viridis_c(na.value = "black", guide = guide_colorbar(title = "Nivel de acceso"))+
  labs(title = "Acceso a servicios de salud en Catamarca") +
  labs(fill = "Nivel de acceso") +
  theme_minimal()

cord <- ggplot() +
  geom_sf(data = sf[sf$provincia == "Córdoba", ],
          aes(fill = Acceso_Salud, color = Acceso_Salud, geometry = geometry),
          size = 0.2) +
  coord_sf() +
  scale_fill_viridis_c(na.value = "black") +
  scale_color_viridis_c(na.value = "black", guide = guide_colorbar(title = "Nivel de acceso"))+
  labs(title = "Acceso a servicios de salud en Córdoba") +
  labs(fill = "Nivel de acceso") +
  theme_minimal()


mapas_alto <- buenos + cata + cord + plot_layout(ncol = 3, nrow = 1)



#HIBRIDO

neu <- ggplot() +
  geom_sf(data = sf[sf$provincia == "Neuquén", ],
          aes(fill = Acceso_Salud, color = Acceso_Salud, geometry = geometry),
          size = 0.2) +
  coord_sf() +
  scale_fill_viridis_c(na.value = "black") +
  scale_color_viridis_c(na.value = "black", guide = guide_colorbar(title = "Nivel de acceso"))+
  labs(title = "Acceso a servicios de salud en Neuquén") +
  labs(fill = "Nivel de acceso") +
  theme_minimal()

stgo <- ggplot() +
  geom_sf(data = sf[sf$provincia == "Santiago del Estero", ],
          aes(fill = Acceso_Salud, color = Acceso_Salud, geometry = geometry),
          size = 0.2) +
  coord_sf() +
  scale_fill_viridis_c(na.value = "black") +
  scale_color_viridis_c(na.value = "black", guide = guide_colorbar(title = "Nivel de acceso"))+
  labs(title = "Acceso a servicios de salud en Santiago del Estero") +
  labs(fill = "Nivel de acceso") +
  theme_minimal()

men <- ggplot() +
  geom_sf(data = sf[sf$provincia == "Mendoza", ],
          aes(fill = Acceso_Salud, color = Acceso_Salud, geometry = geometry),
          size = 0.2) +
  coord_sf() +
  scale_fill_viridis_c(na.value = "black") +
  scale_color_viridis_c(na.value = "black", guide = guide_colorbar(title = "Nivel de acceso"))+
  labs(title = "Acceso a servicios de salud en Mendoza") +
  labs(fill = "Nivel de acceso") +
  theme_minimal()

hibridos <- stgo + men + neu + plot_layout(ncol = 3, nrow = 1)


#bajito
sc <- ggplot() +
  geom_sf(data = sf[sf$provincia == "Santa Cruz", ],
          aes(fill = Acceso_Salud, color = Acceso_Salud, geometry = geometry),
          size = 0.2) +
  coord_sf() +
  scale_fill_viridis_c(na.value = "black") +
  scale_color_viridis_c(na.value = "black", guide = guide_colorbar(title = "Nivel de acceso"))+
  labs(title = "Acceso a servicios de salud en Santa Cruz") +
  labs(fill = "Nivel de acceso") +
  theme_minimal()

rn <- ggplot() +
  geom_sf(data = sf[sf$provincia == "Río Negro", ],
          aes(fill = Acceso_Salud, color = Acceso_Salud, geometry = geometry),
          size = 0.2) +
  coord_sf() +
  scale_fill_viridis_c(na.value = "black") +
  scale_color_viridis_c(na.value = "black", guide = guide_colorbar(title = "Nivel de acceso"))+
  labs(title = "Acceso a servicios de salud en Río Negro") +
  labs(fill = "Nivel de acceso") +
  theme_minimal()

Salta <- ggplot() +
  geom_sf(data = sf[sf$provincia == "Salta", ],
          aes(fill = Acceso_Salud, color = Acceso_Salud, geometry = geometry),
          size = 0.2) +
  coord_sf() +
  scale_fill_viridis_c(na.value = "black") +
  scale_color_viridis_c(na.value = "black", guide = guide_colorbar(title = "Nivel de acceso"))+
  labs(title = "Acceso a servicios de salud en Salta") +
  labs(fill = "Nivel de acceso") +
  theme_minimal()

mediobajos <- sc + rn + plot_layout(ncol = 2, nrow = 1)

ejemplos  <- buenos + Salta + men + rn + plot_layout(ncol = 4, nrow = 1)





