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
  distinct(date, province, city, district, pchc, packid, sales, flag) %>% 
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
## model growth
sh.model.growth <- sh.model.data %>% 
  filter(flag == 0) %>% 
  distinct(year, date, quarter, province, city, district, pchc, packid, sales, flag) %>% 
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
  left_join(sh.model.growth, by = c('year', 'date', 'pchc', 'packid')) %>% 
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
  select(year, date, quarter, province, city, district, pchc, market, packid, 
         units = units_imp, sales = sales_imp, flag)

## predict 2019
sh.predict.sales.19 <- sh.model.data %>% 
  filter(quarter %in% c('2018Q1'), flag == 1) %>% 
  bind_rows(sh.predict.sales.18) %>% 
  left_join(sh.model.growth, by = c('year', 'date', 'pchc', 'packid')) %>% 
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
sh.predict.sales.20 <- sh.predict.sales.19 %>% 
  filter(quarter %in% c('2019Q1', '2019Q2', '2019Q3')) %>% 
  left_join(sh.model.growth, by = c('year', 'date', 'pchc', 'packid')) %>% 
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
imp.sh <- bind_rows(sh.predict.sales.18, sh.predict.sales.19, sh.predict.sales.20) %>% 
  group_by(year, date, quarter, province, city, district, pchc, market, packid, flag) %>% 
  summarise(units = sum(units, na.rm = TRUE),
            sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(units > 0, sales > 0) %>% 
  select(year, date, quarter, province, city, district, pchc, market, packid, units, sales, flag)


##---- CHPA result ----
## CHPA growth
chpa.format <- read.xlsx('05_Internal_Review/ims_chpa_to20Q3_format.xlsx')

chpa.growth <- chpa.format %>% 
  select(-ends_with('UNIT')) %>% 
  pivot_longer(cols = ends_with('RENMINBI'), 
               names_to = 'quarter', 
               values_to = 'sales') %>% 
  mutate(year = stri_sub(quarter, 1, 4), 
         quarter = stri_sub(quarter, 5, 6)) %>% 
  group_by(year, quarter, packid = Pack_Id) %>% 
  summarise(sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(packid) %>% 
  arrange(quarter, year) %>% 
  mutate(growth = lead(sales) / sales) %>% 
  ungroup() %>% 
  filter(year %in% c('2017', '2018', '2019'), quarter != '2019Q4') %>% 
  mutate(quarter = stri_paste(year, quarter)) %>% 
  select(year, quarter, packid, growth_sup = growth)

## predict 2018
set.seed(2018)
sh.predict.181 <- sh.model.data %>% 
  filter(quarter %in% c('2017Q2', '2017Q3', '2017Q4'), flag == 1) %>% 
  left_join(sh.model.growth, by = c('year', 'date', 'pchc', 'packid')) %>% 
  left_join(chpa.growth, by = c('year', 'quarter', 'packid')) %>% 
  mutate(factor = runif(n(), -0.05, 0.05), 
         growth = if_else(is.na(growth), growth_sup + factor, growth),
         growth = if_else(is.na(growth), 1, growth),
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
  select(year, date, quarter, province, city, district, pchc, market, packid, 
         units = units_imp, sales = sales_imp, flag)

## predict 2019
set.seed(2019)
sh.predict.191 <- sh.model.data %>% 
  filter(quarter %in% c('2018Q1'), flag == 1) %>% 
  bind_rows(sh.predict.181) %>% 
  left_join(sh.model.growth, by = c('year', 'date', 'pchc', 'packid')) %>% 
  left_join(chpa.growth, by = c('year', 'quarter', 'packid')) %>% 
  mutate(factor = runif(n(), -0.05, 0.05), 
         growth = if_else(is.na(growth), growth_sup + factor, growth),
         growth = if_else(is.na(growth), 1, growth),
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
set.seed(2020)
sh.predict.201 <- sh.predict.191 %>% 
  filter(quarter %in% c('2019Q1', '2019Q2', '2019Q3')) %>% 
  left_join(sh.model.growth, by = c('year', 'date', 'pchc', 'packid')) %>% 
  left_join(chpa.growth, by = c('year', 'quarter', 'packid')) %>% 
  mutate(factor = runif(n(), -0.05, 0.05), 
         growth = if_else(is.na(growth), growth_sup + factor, growth),
         growth = if_else(is.na(growth), 1, growth),
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

## imp result
imp.sh.update <- bind_rows(sh.predict.181, sh.predict.191, sh.predict.201) %>% 
  filter(market == 'Venous Disease') %>% 
  group_by(year, date, quarter, province, city, district, pchc, market, packid, flag) %>% 
  summarise(units = sum(units, na.rm = TRUE),
            sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup() %>% 
  filter(units > 0, sales > 0) %>% 
  select(year, date, quarter, province, city, district, pchc, market, packid, units, sales, flag) %>% 
  group_by(year, date, quarter, province, city, district, market, packid) %>% 
  summarise(sales = sum(sales, na.rm = TRUE) * 0.5, 
            units = sum(units, na.rm = TRUE) * 0.5) %>% 
  ungroup() %>% 
  mutate(channel = 'CHC', 
         flag_sample = 1, 
         price = sales / units)

## format
sh.format <- FormatServier(proj.price = imp.sh.update, 
                           std.info = std.info, 
                           vbp.info = vbp.info, 
                           city.en = city.en)

sh.update <- sh.format %>% 
  left_join(price.update, by = c('Channel', 'City', 'MKT', 'Molecule_Desc', 
                                 'Prod_Desc', 'Pack_ID', 'Date')) %>% 
  mutate(Units = if_else(!is.na(flag_price), Sales / price, Units), 
         DosageUnits = if_else(!is.na(flag_price), Quantity * Units, DosageUnits), 
         Units_raw = if_else(!is.na(flag_price), Sales_raw / price, Units_raw), 
         DosageUnits_raw = if_else(!is.na(flag_price), Quantity * Units_raw, DosageUnits_raw), 
         Units = round(Units), 
         DosageUnits = round(DosageUnits), 
         Units_raw = round(Units_raw), 
         DosageUnits_raw = round(DosageUnits_raw)) %>% 
  filter(Sales > 0, Units > 0, DosageUnits > 0, 
         !is.infinite(Sales), !is.infinite(Units), !is.infinite(DosageUnits)) %>% 
  filter(!(ATC3 == 'V03B' & !(Prod_Desc %in% kTCM))) %>% 
  filter(City %in% c('National', kTargetCity)) %>% 
  mutate(`Period-MAT` = case_when(
    Date %in% c('2020Q3', '2020Q2', '2020Q1', '2019Q4') ~ 'MAT20Q3', 
    Date %in% c('2019Q3', '2019Q2', '2019Q1', '2018Q4') ~ 'MAT19Q3', 
    TRUE ~ NA_character_
  )) %>% 
  select(Pack_ID, Channel, Province, City, Date, ATC3, ATC4, MKT, Molecule_Desc, 
         Prod_Desc, Pck_Desc, Corp_Desc, Sales, Units, DosageUnits, `Period-MAT`, 
         `CITY-EN`, TherapeuticClsII, TherapeuticClsIII, Prod_CN_Name, Package, 
         Dosage, Quantity, `是否进入带量采购`, `是否是原研`, `是否是中标品种`, 
         `是否是MNC`, `ATC3中文分类`, Sales_raw, Units_raw, DosageUnits_raw, 
         `给药途径`)

write.xlsx(sh.update, '03_Outputs/SH_Venous_Update.xlsx')
