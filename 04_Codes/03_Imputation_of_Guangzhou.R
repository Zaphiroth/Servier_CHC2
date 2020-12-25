# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Servier CHC 2
# Purpose:      Imputation of Guangzhou
# programmer:   Zhe Liu
# Date:         2020-12-25
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Model ----
## missing CHC
kMissingCHC <- c('PCHC01046', 'PCHC01085', 'PCHC01112', 'PCHC01115', 'PCHC01145', 
                 'PCHC01146', 'PCHC01164', 'PCHC01172')

## model set
gz.model.data <- raw.total %>% 
  filter(province == '广东') %>% 
  mutate(flag = if_else(pchc %in% kMissingCHC, 1, 0))

gz.model.set <- gz.model.data %>% 
  filter(year %in% c('2018', '2019'), !(quarter %in% c('2019Q4'))) %>% 
  distinct(date, province, city, district, pchc, packid, sales, flag) %>% 
  group_by(province, city, district, pchc, date, flag) %>% 
  summarise(sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup() %>% 
  arrange(date) %>% 
  pivot_wider(id_cols = c(province, city, district, pchc, flag), 
              names_from = date, 
              values_from = sales, 
              values_fill = 0)

## model
gz.train.set <- gz.model.set[gz.model.set$flag == 0, ]
gz.test.set <- gz.model.set[gz.model.set$flag == 1, ]

gz.knn.model <- kknn(flag ~ ., 
                     train = gz.train.set[, -(1:4)], 
                     test = gz.test.set[, -(1:4)], 
                     k = 3, 
                     scale = TRUE)

## model weightage
gz.model.indice <- as.data.frame(gz.knn.model$C) %>% 
  lapply(function(x) {
    gz.train.set$pchc[x]
  }) %>% 
  as.data.frame(col.names = c('pchc_1', 'pchc_2', 'pchc_3')) %>% 
  bind_cols(gz.test.set[, 1:4]) %>% 
  pivot_longer(cols = starts_with('pchc_'), 
               names_to = 'knn_level', 
               values_to = 'knn_pchc')

gz.model.weight <- as.data.frame(gz.knn.model$D) %>% 
  lapply(function(x) {
    1 / (x + 1)
  }) %>% 
  as.data.frame(col.names = c('pchc_1', 'pchc_2', 'pchc_3')) %>% 
  mutate(pchc_1 = pchc_1 / (pchc_1 + pchc_2 + pchc_3),
         pchc_2 = pchc_2 / (pchc_1 + pchc_2 + pchc_3),
         pchc_3 = pchc_3 / (pchc_1 + pchc_2 + pchc_3)) %>% 
  bind_cols(gz.test.set[, 1:4]) %>% 
  pivot_longer(cols = starts_with('pchc_'), 
               names_to = 'knn_level', 
               values_to = 'knn_weight')


##---- Growth ----
gz.model.growth <- gz.model.data %>% 
  filter(flag == 0) %>% 
  distinct(year, date, quarter, province, city, district, pchc, packid, sales, flag) %>% 
  group_by(knn_pchc = pchc, packid, year, date, quarter) %>% 
  summarise(sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup() %>% 
  inner_join(gz.model.indice, by = 'knn_pchc') %>% 
  left_join(gz.model.weight, 
            by = c('province', 'city', 'district', 'pchc', 'knn_level')) %>% 
  group_by(pchc, packid, year, date, quarter) %>% 
  summarise(sales = sum(sales * knn_weight, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(month = stri_sub(date, 5, 6)) %>% 
  pivot_wider(id_cols = c(pchc, packid, month), 
              names_from = year, 
              values_from = sales, 
              values_fill = 0) %>% 
  mutate(growth_1718 = `2018` / `2017`, 
         growth_1819 = `2019` / `2018`, 
         growth_1920 = `2020` / `2019`) %>% 
  select(pchc, packid, month, `2017` = growth_1718, `2018` = growth_1819, 
         `2019` = growth_1920) %>% 
  pivot_longer(cols = starts_with('20'), 
               names_to = 'year', 
               values_to = 'growth') %>% 
  filter(!is.na(growth), !is.infinite(growth)) %>% 
  mutate(date = stri_paste(year, month)) %>% 
  select(year, date, pchc, packid, growth)


##---- Prediction ----
## predict 2019
gz.predict.sales.19 <- gz.model.data %>% 
  filter(quarter %in% c('2018Q4'), flag == 1) %>% 
  left_join(gz.model.growth, by = c('year', 'date', 'pchc', 'packid')) %>% 
  mutate(growth = if_else(is.na(growth), 1, growth),
         growth = if_else(growth > 3, 3, growth),
         growth = if_else(growth > quantile(growth, 0.9),
                          mean(growth[growth >= quantile(growth, 0.25) & 
                                        growth <= quantile(growth, 0.75)]),
                          growth)) %>% 
  mutate(units_imp = units * growth,
         sales_imp = sales * growth,
         date = gsub('2018', '2019', date),
         quarter = gsub('2018', '2019', quarter),
         year = '2019',
         flag = 1) %>% 
  select(year, date, quarter, province, city, district, pchc, market, packid, 
         units = units_imp, sales = sales_imp, flag)

## predict 2020
gz.predict.sales.20 <- gz.model.data %>% 
  filter(quarter %in% c('2019Q1', '2019Q2'), flag == 1) %>% 
  left_join(gz.model.growth, by = c('year', 'date', 'pchc', 'packid')) %>% 
  mutate(growth = if_else(is.na(growth), 1, growth),
         growth = if_else(growth > 3, 3, growth),
         growth = if_else(growth > quantile(growth, 0.9),
                          mean(growth[growth >= quantile(growth, 0.25) & 
                                        growth <= quantile(growth, 0.75)]),
                          growth)) %>% 
  mutate(units_imp = units * growth,
         sales_imp = sales * growth,
         date = gsub('2019', '2020', date),
         quarter = gsub('2019', '2020', quarter),
         year = '2020',
         flag = 1) %>% 
  select(year, date, quarter, province, city, district, pchc, market, packid, 
         units = units_imp, sales = sales_imp, flag)


##---- Result ----
imp.gz <- bind_rows(gz.predict.sales.19, gz.predict.sales.20) %>% 
  group_by(year, date, quarter, province, city, district, pchc, market, packid, flag) %>% 
  summarise(units = sum(units, na.rm = TRUE),
            sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(units > 0, sales > 0) %>% 
  select(year, date, quarter, province, city, district, pchc, market, packid, units, sales, flag)
