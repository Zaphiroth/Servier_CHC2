# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Servier CHC 2
# Purpose:      Imputation of Fujian Venous
# programmer:   Zhe Liu
# Date:         2020-12-23
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Model ----
## model set
fj.model.data <- raw.total %>% 
  mutate(flag = if_else(province == '福建', 1, 0))

fj.model.set <- fj.model.data %>% 
  filter(year %in% c('2018', '2019', '2020'), !(quarter %in% c('2020Q4'))) %>% 
  distinct(date, province, city, district, pchc, packid, sales, flag) %>% 
  group_by(province, city, district, pchc, date, flag) %>% 
  summarise(sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = c(province, city, district, pchc, flag), 
              names_from = date, 
              values_from = sales, 
              values_fill = 0)

## model
fj.train.set <- fj.model.set[fj.model.set$flag == 0, ]
fj.test.set <- fj.model.set[fj.model.set$flag == 1, ]

fj.knn.model <- kknn(flag ~ ., 
                     train = fj.train.set[, -(1:4)], 
                     test = fj.test.set[, -(1:4)], 
                     k = 3, 
                     scale = TRUE)

## model weightage
fj.model.indice <- as.data.frame(fj.knn.model$C) %>% 
  lapply(function(x) {
    fj.train.set$pchc[x]
  }) %>% 
  as.data.frame(col.names = c('pchc_1', 'pchc_2', 'pchc_3')) %>% 
  bind_cols(fj.test.set[, 1:4]) %>% 
  pivot_longer(cols = starts_with('pchc_'), 
               names_to = 'knn_level', 
               values_to = 'knn_pchc')

fj.model.weight <- as.data.frame(fj.knn.model$D) %>% 
  lapply(function(x) {
    1 / (x + 1)
  }) %>% 
  as.data.frame(col.names = c('pchc_1', 'pchc_2', 'pchc_3')) %>% 
  mutate(pchc_1 = pchc_1 / (pchc_1 + pchc_2 + pchc_3),
         pchc_2 = pchc_2 / (pchc_1 + pchc_2 + pchc_3),
         pchc_3 = pchc_3 / (pchc_1 + pchc_2 + pchc_3)) %>% 
  bind_cols(fj.test.set[, 1:4]) %>% 
  pivot_longer(cols = starts_with('pchc_'), 
               names_to = 'knn_level', 
               values_to = 'knn_weight')


##---- Result ----
imp.fj <- fj.model.data %>% 
  filter(flag == 0, market == 'Venous Disease') %>% 
  group_by(year, date, quarter, knn_pchc = pchc, market, packid) %>% 
  summarise(units = sum(units, na.rm = TRUE),
            sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup() %>% 
  inner_join(fj.model.indice, by = 'knn_pchc') %>% 
  left_join(fj.model.weight, 
            by = c('province', 'city', 'district', 'pchc', 'knn_level')) %>% 
  group_by(year, date, quarter, province, city, district, pchc, market, packid) %>% 
  summarise(units = sum(units * knn_weight, na.rm = TRUE),
            sales = sum(sales * knn_weight, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(flag = 1)
