# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Servier CHC 2
# Purpose:      Nation Projection
# programmer:   Zhe Liu
# Date:         2020-12-11
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


ProjectNation <- function(proj.sample.total, 
                          pchc.universe, 
                          city.tier) {
  
  ##---- Nation info ----
  ## universe city
  universe.city <- pchc.universe %>% 
    left_join(city.tier, by = "city") %>% 
    mutate(tier = ifelse(is.na(tier), 1, tier)) %>% 
    filter(!is.na(est)) %>% 
    group_by(province, city, district, tier) %>% 
    summarise(est = sum(est, na.rm = TRUE)) %>% 
    ungroup()
  
  ## nation sample
  nation.sample <- proj.sample.total %>% 
    filter(!(province %in% c('北京', '上海'))) %>% 
    left_join(city.tier, by = "city") %>% 
    mutate(tier = ifelse(is.na(tier), 1, tier)) %>% 
    group_by(date, province, city, tier, district, market, packid) %>% 
    summarise(sales = sum(sales, na.rm = TRUE), 
              units = sum(units, na.rm = TRUE)) %>% 
    ungroup() %>% 
    inner_join(universe.city, by = c("province", "city", "district", "tier")) %>% 
    pivot_wider(names_from = date, values_from = c(sales, units))
  
  
  ##---- Projection ----
  ## loop
  proj.nation.list <- vector('list', length = nrow(universe.city))
  
  pb <- txtProgressBar(min = 1, max = nrow(universe.city), initial = 1)
  for (i in 1:nrow(universe.city)) {
    setTxtProgressBar(pb, 1)
    
    proj.nation.list[[i]] <- universe.city[i, ] %>% 
      left_join(nation.sample, by = c("tier")) %>% 
      mutate(est_gap = abs(est.x - est.y)) %>% 
      filter(est_gap <= min(est_gap)) %>% 
      mutate(slope = est.x / est.y, 
             slope = ifelse(is.infinite(slope) | is.na(slope) | is.nan(slope), 
                            1, 
                            slope)) %>% 
      pivot_longer(cols = c(starts_with('sales'), starts_with('units')), 
                   names_to = c('index', 'date'), 
                   names_sep = '_', 
                   values_to = 'value', 
                   values_drop_na = FALSE) %>% 
      pivot_wider(names_from = index, values_from = value) %>% 
      select(date, province = province.x, city = city.x, tier, 
             district = district.x, market, packid, sales, units, est.x, est.y, slope)
  }
  
  ## result
  proj.nation <- bind_rows(proj.nation.list) %>% 
    mutate(slope = if_else(slope > quantile(slope, 0.9), 
                           quantile(slope, 0.9), 
                           slope), 
           sales_m = sales * slope, 
           units_m = units * slope, 
           flag_sample = 0) %>% 
    group_by(date, province, city, district, market, packid, flag_sample) %>% 
    summarise(sales = sum(sales_m, na.rm = TRUE), 
              units = sum(units_m, na.rm = TRUE)) %>% 
    ungroup() %>% 
    filter(sales > 0, units > 0) %>% 
    filter(!(city %in% unique(proj.sample.total$city))) %>% 
    bind_rows(proj.sample.total)
  
  return(proj.nation)
}

