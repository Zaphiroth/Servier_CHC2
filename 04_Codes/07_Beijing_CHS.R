# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Servier CHC 2
# Purpose:      Beijing CHS
# programmer:   Zhe Liu
# date:         2020-12-16
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Readin ----
## Beijing CHS
raw.chs <- map(list.files('02_Inputs/data/CHS', pattern = '*.xlsx', full.names = TRUE), 
               function(x) {
                 read.xlsx(x) %>% 
                   mutate(Year = as.character(Year), 
                          Month = as.character(Month), 
                          Prd_desc_ZB = as.character(Prd_desc_ZB))
               })


##---- Beijing CHS projection ----
bj.chs <- bind_rows(raw.chs) %>% 
  filter(grepl('服务站', Hospital_Name)) %>% 
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
  left_join(market.def, by = c('atc3', 'molecule')) %>% 
  filter(!is.na(market)) %>% 
  group_by(date, province, city, district, packid) %>% 
  summarise(sales = sum(sales, na.rm = TRUE),
            units = sum(units, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(channel = 'CHS', 
         flag_sample = 1)

