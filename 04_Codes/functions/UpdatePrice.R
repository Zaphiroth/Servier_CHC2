# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Servier CHC 2
# Purpose:      Price
# programmer:   Zhe Liu
# Date:         2020-12-11
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


UpdatePrice <- function(proj.nation, raw.total) {
  
  ##---- Price ----
  ## origin price
  price.origin <- raw.total %>% 
    group_by(packid, quarter, province, city) %>% 
    summarise(sales = sum(sales, na.rm = TRUE),
              units = sum(units, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(price = sales / units) %>% 
    select(-sales, -units)
  
  ## mean price by city year
  price.city <- raw.total %>% 
    group_by(packid, year, province, city) %>% 
    summarise(sales = sum(sales, na.rm = TRUE), 
              units = sum(units, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(price_city = sales / units) %>% 
    select(-sales, -units)
  
  ## mean price by province quarter
  price.province <- raw.total %>% 
    group_by(packid, quarter, province) %>% 
    summarise(sales = sum(sales, na.rm = TRUE),
              units = sum(units, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(price_prov = sales / units) %>% 
    select(-sales, -units)
  
  ## mean price by province year
  price.year <- raw.total %>% 
    group_by(packid, year, province) %>% 
    summarise(sales = sum(sales, na.rm = TRUE), 
              units = sum(units, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(price_year = sales / units) %>% 
    select(-sales, -units)
  
  ## mean price by pack quarter
  price.pack <- raw.total %>% 
    group_by(packid, quarter) %>% 
    summarise(sales = sum(sales, na.rm = TRUE),
              units = sum(units, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(price_pack = sales / units) %>% 
    select(-sales, -units)
  
  ## mean price by pack year
  price.pack.year <- raw.total %>% 
    group_by(packid, year) %>% 
    summarise(sales = sum(sales, na.rm = TRUE),
              units = sum(units, na.rm = TRUE)) %>% 
    ungroup() %>% 
    mutate(price_pack_year = sales / units) %>% 
    select(-sales, -units)
  
  
  ##---- Update ----
  proj.price <- proj.nation %>% 
    mutate(year = stri_sub(date, 1, 4), 
           quarter = stri_sub(date, 5, 6), 
           quarter = ifelse(quarter %in% c("01", "02", "03"), "Q1",
                            ifelse(quarter %in% c("04", "05", "06"), "Q2",
                                   ifelse(quarter %in% c("07", "08", "09"), "Q3",
                                          ifelse(quarter %in% c("10", "11", "12"), "Q4",
                                                 NA_character_)))), 
           quarter = stri_paste(year, quarter)) %>% 
    left_join(price.origin, by = c("province", "city", "quarter", "packid")) %>% 
    left_join(price.city, by = c("province", "city", "year", "packid")) %>% 
    left_join(price.province, by = c("province", "quarter", "packid")) %>% 
    left_join(price.year, by = c("province", "year", "packid")) %>% 
    left_join(price.pack, by = c("quarter", "packid")) %>% 
    left_join(price.pack.year, by = c('year', 'packid')) %>% 
    mutate(price = if_else(is.na(price), price_city, price), 
           price = if_else(is.na(price), price_prov, price), 
           price = if_else(is.na(price), price_year, price), 
           price = if_else(is.na(price), price_pack, price), 
           price = if_else(is.na(price), price_pack_year, price)) %>% 
    mutate(units = sales / price) %>% 
    filter(units > 0, sales > 0, price > 0) %>% 
    select(channel, year, date, quarter, province, city, district, packid, 
           flag_sample, price, units, sales)
  
  return(proj.price)
}