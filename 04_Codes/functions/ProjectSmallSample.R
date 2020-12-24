# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Servier CHC 2
# Purpose:      Small Sample Projection
# programmer:   Zhe Liu
# Date:         2020-12-24
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


ProjectSmallSample <- function(raw.total, 
                               pchc.universe, 
                               small = '上海', 
                               sample = c('上海', '北京')) {
  
  ##---- Shanghai info ----
  pchc.sh <- pchc.universe %>% 
    filter(province %in% sample)
  
  # partition
  pchc.sh.sample <- pchc.sh %>% 
    filter(flag_sample == 1)
  
  pchc.sh.nonsample <- pchc.sh %>% 
    filter(flag_sample == 0, province %in% small)
  
  
  ##---- K-NN model ----
  # set
  train.set <- pchc.sh.sample[, 5:13]
  test.set <- pchc.sh.nonsample[, 5:13]
  
  # model
  knn.model <- kknn(flag_sample ~ ., train = train.set, test = test.set, 
                    k = 3, scale = TRUE)
  
  # weighting
  knn.indice <- as.data.frame(knn.model$C) %>% 
    lapply(function(x) {
      pchc.sh.sample$pchc[x]
    }) %>% 
    as.data.frame(col.names = c("pchc_1", "pchc_2", "pchc_3")) %>% 
    bind_cols(pchc.sh.nonsample[, c("pchc")]) %>% 
    pivot_longer(cols = c(pchc_1, pchc_2, pchc_3), 
                 names_to = 'knn_level', 
                 values_to = 'knn_pchc', 
                 values_drop_na = FALSE)
  
  knn.weight <- as.data.frame(knn.model$D) %>% 
    lapply(function(x) {
      1 / (x + 1)
    }) %>% 
    as.data.frame(col.names = c("pchc_1", "pchc_2", "pchc_3")) %>% 
    mutate(weight_sum = pchc_1 + pchc_2 + pchc_3,
           pchc_1 = pchc_1 / weight_sum,
           pchc_2 = pchc_2 / weight_sum,
           pchc_3 = pchc_3 / weight_sum) %>% 
    bind_cols(pchc.sh.nonsample[, c("pchc")]) %>% 
    select(-weight_sum) %>% 
    pivot_longer(cols = c(pchc_1, pchc_2, pchc_3), 
                 names_to = 'knn_level', 
                 values_to = 'knn_weight', 
                 values_drop_na = FALSE)
  
  
  ##---- Projection ----
  # projection data
  proj.small.data <- raw.total %>% 
    filter(province %in% small) %>% 
    arrange(province, city, district, pchc, packid, date) %>% 
    group_by(pchc, packid, date) %>% 
    summarise(sales = sum(sales, na.rm = TRUE), 
              units = sum(units, na.rm = TRUE)) %>% 
    ungroup()
  
  # est ratio factor
  est.sh <- pchc.sh %>% 
    distinct(pchc, est)
  
  est.ratio <- pchc.sh.nonsample %>% 
    distinct(pchc, province, city, district, flag_sample) %>% 
    left_join(knn.indice, by = 'pchc') %>% 
    left_join(knn.weight, by = c('pchc', 'knn_level')) %>% 
    left_join(est.sh, by = c('knn_pchc' = 'pchc')) %>% 
    rename(knn_est = est) %>% 
    left_join(est.sh, by = 'pchc') %>% 
    group_by(province, city, district, pchc) %>% 
    summarise(knn_est = sum(knn_est * knn_weight, na.rm = TRUE), 
              est = sum(est * knn_weight, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(est_ratio = est / knn_est)
  
  # result
  proj.small.nonsample <- pchc.sh.nonsample %>% 
    distinct(pchc, province, city, district, flag_sample) %>% 
    left_join(knn.indice, by = 'pchc') %>% 
    left_join(knn.weight, by = c('pchc', 'knn_level')) %>% 
    left_join(proj.small.data, by = c('knn_pchc' = 'pchc')) %>% 
    group_by(province, city, district, pchc, packid, date, flag_sample) %>% 
    summarise(knn_sales = sum(sales * knn_weight, na.rm = TRUE), 
              knn_units = sum(units * knn_weight, na.rm = TRUE)) %>% 
    ungroup() %>% 
    left_join(est.ratio, by = c('province', 'city', 'district', 'pchc')) %>% 
    mutate(sales = knn_sales * est_ratio, 
           units = knn_units * est_ratio)
  
  proj.small <- raw.total %>% 
    filter(province %in% small) %>% 
    mutate(flag_sample = 1) %>% 
    bind_rows(proj.small.nonsample) %>% 
    group_by(date, province, city, district, packid, flag_sample) %>% 
    summarise(sales = sum(sales, na.rm = TRUE), 
              units = sum(units, na.rm = TRUE)) %>% 
    ungroup() %>% 
    filter(sales > 0, units > 0)
  
  
  return(proj.small)
}







