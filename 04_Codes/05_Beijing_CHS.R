# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Servier CHC 2
# Purpose:      Beijing CHS
# programmer:   Zhe Liu
# date:         2020-12-16
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Beijing CHS ----
bj.chs <- bind_rows(raw.servier, raw.venous) %>% 
  filter(Province == '北京市', 
         Year %in% c(2018, 2019, 2020), 
         grepl('服务站', Hospital_Name)) %>% 
  distinct(year = as.character(Year), 
           quarter = Quarter, 
           date = as.character(Month), 
           province = '北京', 
           city = '北京', 
           district = County, 
           hospital = Hospital_Name, 
           atc3 = stri_sub(ATC4_Code, 1, 4), 
           molecule = Molecule_Desc, 
           packid = stri_pad_left(packcode, 7, 0), 
           price = Price, 
           units = Value / Price, 
           sales = Value) %>% 
  mutate(packid = case_when(packid == '0060212' & city %in% c('北京', '南京') ~ '0060206', 
                            packid == '0243304' & city %in% c('北京') ~ '0243302', 
                            packid == '0243306' ~ '0243304', 
                            packid == '1065108' & city %in% c('北京') ~ '1065106', 
                            packid == '1065108' & !(city %in% c('北京')) ~ '1065102', 
                            packid == '3145210' & city %in% c('北京') ~ '3145208', 
                            TRUE ~ packid)) %>% 
  left_join(market.def, by = c('atc3', 'molecule')) %>% 
  filter(!is.na(market)) %>% 
  group_by(date, province, city, district, market, packid) %>% 
  summarise(sales = sum(sales, na.rm = TRUE),
            units = sum(units, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(channel = 'CHS', 
         flag_sample = 1)

