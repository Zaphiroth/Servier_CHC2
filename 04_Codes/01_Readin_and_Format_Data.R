# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Servier CHC 2
# Purpose:      Readin
# programmer:   Zhe Liu
# Date:         2020-12-10
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Universe info ----
## PCHC info
pchc.universe <- read.xlsx("02_Inputs/Universe_PCHCCode_20201209.xlsx", sheet = "PCHC")

pchc.mapping1 <- pchc.universe %>% 
  filter(!is.na(`单位名称`), !is.na(PCHC_Code)) %>% 
  group_by(province = `省`, city = `地级市`, district = `区[县/县级市】`, hospital = `单位名称`) %>% 
  summarise(pchc = first(PCHC_Code)) %>% 
  ungroup()

pchc.mapping2 <- pchc.universe %>% 
  filter(!is.na(ZS_Servier.name), !is.na(PCHC_Code)) %>% 
  group_by(province = `省`, city = `地级市`, district = `区[县/县级市】`, hospital = `ZS_Servier.name`) %>% 
  summarise(pchc = first(PCHC_Code)) %>% 
  ungroup()

pchc.mapping3 <- bind_rows(pchc.mapping1, pchc.mapping2) %>% 
  distinct(province, city, district, hospital, pchc)

pchc.mapping4 <- pchc.mapping3 %>% 
  group_by(pchc) %>% 
  summarise(province = first(na.omit(province)),
            city = first(na.omit(city)),
            district = first(na.omit(district))) %>% 
  ungroup()

# pchc.info <- pchc.universe %>% 
#   rename(province = `省`, 
#          city = `地级市`, 
#          district = `区[县/县级市】`, 
#          pchc = PCHC_Code) %>% 
#   group_by(pchc) %>% 
#   summarise(province = first(na.omit(province)), 
#             city = first(na.omit(city)), 
#             district = first(na.omit(district)), 
#             pop = first(na.omit(`人口`)), 
#             pop1 = first(na.omit(`其中：0-14岁人口数`)), 
#             pop2 = first(na.omit(`15-64岁人口数`)), 
#             pop3 = first(na.omit(`65岁及以上人口数`)), 
#             doc = first(na.omit(`2016年执业医师（助理）人数`)), 
#             pat = first(na.omit(`2016年总诊疗人次数`)), 
#             inc = first(na.omit(`2016年药品收入（千元）`)), 
#             est = first(na.omit(`其中：西药药品收入（千元）`))) %>% 
#   ungroup() %>% 
#   filter(!is.na(pop), !is.na(pop1), !is.na(pop2), !is.na(pop3), !is.na(doc), 
#          !is.na(pat), !is.na(inc), !is.na(est))


##---- Raw data ----
## Servier
raw.list <- map(list.files('02_Inputs/data', pattern = '*.xlsx', full.names = TRUE), 
                function(x) {
                  read.xlsx(x) %>% 
                    mutate(Year = as.character(Year), 
                           Month = as.character(Month), 
                           Prd_desc_ZB = as.character(Prd_desc_ZB))
                })

raw.data <- bind_rows(raw.list) %>% 
  distinct(year = as.character(Year), 
           quarter = Quarter, 
           date = as.character(Month), 
           province = gsub('省|市', '', Province), 
           city = if_else(City == "市辖区", "北京", gsub("市", "", City)), 
           district = County, 
           hospital = Hospital_Name, 
           packid = stri_pad_left(packcode, 7, 0), 
           units = if_else(is.na(Volume), Value / Price, Volume), 
           sales = Value) %>% 
  left_join(pchc.mapping3, by = c('province', 'city', 'district', 'hospital')) %>% 
  filter(!is.na(pchc), !is.na(packid)) %>% 
  mutate(packid = if_else(stri_sub(packid, 1, 5) == '47775', 
                          stri_paste('58906', stri_sub(packid, 6, 7)), 
                          packid), 
         packid = if_else(stri_sub(packid, 1, 5) == '06470', 
                          stri_paste('64895', stri_sub(packid, 6, 7)), 
                          packid)) %>% 
  filter(units > 0, sales > 0) %>% 
  select(year, date, quarter, province, city, district, pchc, packid, units, sales)

## Guangzhou
raw.gz1 <- read_feather('02_Inputs/data/广州/Servier_guangzhou_171819_packid_moleinfo.feather') %>% 
  distinct(year = as.character(Year), 
           quarter = Quarter, 
           date = as.character(Month), 
           province = '广东', 
           city = '广州', 
           hospital = Hospital_Name, 
           packid = stri_pad_left(packcode, 7, 0), 
           price = Price, 
           units = if_else(is.na(Volume), Value / Price, Volume), 
           sales = Value)

raw.gz2 <- read.xlsx('02_Inputs/data/广州/gzs 20q1_date.xlsx')
raw.gz3 <- read.xlsx('02_Inputs/data/广州/2jd_date.xlsx')

raw.gz <- raw.gz2 %>% 
  select(-period) %>% 
  mutate(date = as.character(date)) %>% 
  bind_rows(raw.gz3) %>% 
  mutate(quarter_m = stri_sub(date, 5, 6)) %>% 
  distinct(year = stri_sub(date, 1, 4), 
           quarter = ifelse(quarter_m %in% c("01", "02", "03"), 
                            stri_paste(year, "Q1"), 
                            ifelse(quarter_m %in% c("04", "05", "06"), 
                                   stri_paste(year, "Q2"), 
                                   ifelse(quarter_m %in% c("07", "08", "09"), 
                                          stri_paste(year, "Q3"), 
                                          ifelse(quarter_m %in% c("10", "11", "12"), 
                                                 stri_paste(year, "Q4"), 
                                                 year)))), 
           date = stri_sub(date, 1, 6), 
           province = '广东', 
           city = '广州', 
           hospital = name, 
           packid = stri_pad_left(pfc, 7, 0), 
           price = value / unit, 
           units = unit, 
           sales = value) %>% 
  bind_rows(raw.gz1) %>% 
  left_join(pchc.mapping3, by = c('province', 'city', 'hospital')) %>% 
  filter(!is.na(pchc), !is.na(packid)) %>% 
  mutate(packid = if_else(stri_sub(packid, 1, 5) == '47775', 
                          stri_paste('58906', stri_sub(packid, 6, 7)), 
                          packid), 
         packid = if_else(stri_sub(packid, 1, 5) == '06470', 
                          stri_paste('64895', stri_sub(packid, 6, 7)), 
                          packid)) %>% 
  filter(units > 0, sales > 0) %>% 
  select(year, date, quarter, province, city, district, pchc, packid, units, sales)

## Shanghai
raw.sh1 <- read.xlsx('02_Inputs/data/上海/上海_2017.xlsx')
raw.sh2 <- read.xlsx('02_Inputs/data/上海/上海_2018.xlsx')

raw.sh <- bind_rows(raw.sh1, raw.sh2) %>% 
  mutate(quarter_m = stri_sub(Date, 5, 6)) %>% 
  distinct(year = stri_sub(Date, 1, 4), 
           quarter = ifelse(quarter_m %in% c('01', '02', '03'), 
                            stri_paste(year, 'Q1'), 
                            ifelse(quarter_m %in% c('04', '05', '06'), 
                                   stri_paste(year, 'Q2'), 
                                   ifelse(quarter_m %in% c('07', '08', '09'), 
                                          stri_paste(year, 'Q3'), 
                                          ifelse(quarter_m %in% c('10', '11', '12'), 
                                                 stri_paste(year, 'Q4'), 
                                                 year)))), 
           date = as.character(Date), 
           province = '上海', 
           city = '上海', 
           pchc = PCHC, 
           packid = stri_pad_left(pfc, 7, 0), 
           price = value / unit, 
           units = unit, 
           sales = value) %>% 
  mutate(pchc = case_when(pchc == 'PCHC06729' ~ 'PCHC06728', 
                          pchc == 'PCHC06622' ~ 'PCHC06620', 
                          pchc == 'PCHC06645' ~ 'PCHC06644', 
                          pchc == 'PCHC06722' ~ 'PCHC06721', 
                          pchc == 'PCHC06840' ~ 'PCHC06839', 
                          TRUE ~ pchc)) %>% 
  left_join(pchc.mapping4, by = c('province', 'city', 'pchc')) %>% 
  filter(pchc != '#N/A', units > 0, sales > 0) %>% 
  mutate(packid = if_else(stri_sub(packid, 1, 5) == '47775', 
                          stri_paste('58906', stri_sub(packid, 6, 7)), 
                          packid), 
         packid = if_else(stri_sub(packid, 1, 5) == '06470', 
                          stri_paste('64895', stri_sub(packid, 6, 7)), 
                          packid)) %>% 
  select(year, date, quarter, province, city, district, pchc, packid, units, sales)

## total
raw.total <- bind_rows(raw.data, raw.gz, raw.sh) %>% 
  filter(pchc != "#N/A", units > 0, sales > 0) %>% 
  group_by(pchc) %>% 
  mutate(province = first(na.omit(province)), 
         city = first(na.omit(city)), 
         district = first(na.omit(district))) %>% 
  ungroup() %>% 
  group_by(year, date, quarter, province, city, district, pchc, packid) %>% 
  summarise(units = sum(units, na.rm = TRUE), 
            sales = sum(sales, na.rm = TRUE)) %>% 
  ungroup()

write_feather(raw.total, '03_Outputs/Servier_CHC2_Raw.feather')