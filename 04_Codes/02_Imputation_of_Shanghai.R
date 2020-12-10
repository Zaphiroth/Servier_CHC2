# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Servier CHC 2
# Purpose:      Imputation of Shanghai
# programmer:   Zhe Liu
# Date:         2020-12-08
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Model ----
## model set
sh.model.data <- raw.total %>% 
  filter(province %in% c('上海', '北京')) %>% 
  mutate(flag = if_else(province == '上海', 1, 0))

sh.model.set <- sh.model.data %>% 
  filter(quarter %in% c('2017Q1', '2017Q2', '2017Q3', '2017Q4', '2018Q1')) %>% 
  group_by(province, city, district, pchc, date, flag) %>% 
  summarise(sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = c(province, city, district, pchc, flag), 
              names_from = date, 
              values_from = sales, 
              values_fill = 0)

## model
sh.train.set <- sh.model.set[sh.model.set$flag == 0, ]
sh.test.set <- sh.model.set[sh.model.set$flag == 1, ]

sh.knn.model <- kknn(flag ~ ., 
                     train = sh.train.set[, -(1:4)], 
                     test = sh.test.set[, -(1:4)], 
                     k = 3, 
                     scale = TRUE)

## model weightage
sh.model.indice <- as.data.frame(sh.knn.model$C) %>% 
  lapply(function(x) {
    sh.train.set$pchc[x]
  }) %>% 
  as.data.frame(col.names = c('pchc_1', 'pchc_2', 'pchc_3')) %>% 
  bind_cols(sh.test.set[, 1:4]) %>% 
  pivot_longer(cols = starts_with('pchc_'), 
               names_to = 'knn_level', 
               values_to = 'knn_pchc')

sh.model.weight <- as.data.frame(sh.knn.model$D) %>% 
  lapply(function(x) {
    1 / (x + 1)
  }) %>% 
  as.data.frame(col.names = c('pchc_1', 'pchc_2', 'pchc_3')) %>% 
  mutate(pchc_1 = pchc_1 / (pchc_1 + pchc_2 + pchc_3),
         pchc_2 = pchc_2 / (pchc_1 + pchc_2 + pchc_3),
         pchc_3 = pchc_3 / (pchc_1 + pchc_2 + pchc_3)) %>% 
  bind_cols(sh.test.set[, 1:4]) %>% 
  pivot_longer(cols = starts_with('pchc_'), 
               names_to = 'knn_level', 
               values_to = 'knn_weight')


##---- Growth ----
sh.model.growth <- sh.model.data %>% 
  filter(flag == 0) %>% 
  group_by(knn_pchc = pchc, packid, year, date, quarter) %>% 
  summarise(sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup() %>% 
  inner_join(sh.model.indice, by = 'knn_pchc') %>% 
  left_join(sh.model.weight, 
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
## predict 2018
sh.predict.sales.18 <- sh.model.data %>% 
  filter(quarter %in% c('2017Q2', '2017Q3', '2017Q4'), flag == 1) %>% 
  left_join(sh.model.growth, by = c('pchc', 'packid', 'year', 'date')) %>% 
  mutate(growth = if_else(is.na(growth), 1, growth),
         growth = if_else(growth > 3, 3, growth),
         growth = if_else(growth > quantile(growth, 0.9),
                          mean(growth[growth >= quantile(growth, 0.25) & 
                                        growth <= quantile(growth, 0.75)]),
                          growth)) %>% 
  mutate(units_imp = units * growth,
         sales_imp = sales * growth,
         date = gsub('2017', '2018', date),
         quarter = gsub('2017', '2018', quarter),
         year = '2018',
         flag = 1) %>% 
  select(year, date, quarter, province, city, district, pchc, packid, 
         units = units_imp, sales = sales_imp, flag)

## predict 2019
sh.predict.sales.19 <- sh.model.data %>% 
  filter(quarter %in% c('2018Q1'), flag == 1) %>% 
  bind_rows(sh.predict.sales.18) %>% 
  left_join(sh.model.growth, by = c('pchc', 'packid', 'year', 'date')) %>% 
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
  select(year, date, quarter, province, city, district, pchc, packid, 
         units = units_imp, sales = sales_imp, flag)

## predict 2020
sh.predict.sales.20 <- sh.predict.sales.19 %>% 
  filter(quarter %in% c('2019Q1', '2019Q2', '2019Q3')) %>% 
  left_join(sh.model.growth, by = c('pchc', 'packid', 'year', 'date')) %>% 
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
  select(year, date, quarter, province, city, district, pchc, packid, 
         units = units_imp, sales = sales_imp, flag)


##---- Result ----
imp.sh <- bind_rows(sh.predict.sales.18, sh.predict.sales.19, sh.predict.sales.20) %>% 
  group_by(year, date, quarter, province, city, district, pchc, packid, flag) %>% 
  summarise(units = sum(units, na.rm = TRUE),
            sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(units > 0, sales > 0) %>% 
  select(year, date, quarter, province, city, district, pchc, packid, units, sales, flag)
