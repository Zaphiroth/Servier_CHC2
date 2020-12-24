# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Servier CHC 2
# Purpose:      Main
# programmer:   Zhe Liu
# Date:         2020-12-10
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Universe info ----
## target city
kTargetCity <- c('北京', '上海', '杭州', '南京')

## PCHC
pchc.universe <- read.xlsx("02_Inputs/2020_PCHC_Universe更新维护.xlsx", 
                           sheet = "2020 CHC universe", cols = 1:19)

pchc.universe.m <- pchc.universe %>% 
  distinct(province = gsub('省|市', '', `省`), 
           city = gsub('市', '', `地级市`), 
           district = `区[县/县级市]`, 
           pchc = `新版PCHC_Code`, 
           est = `其中：西药药品收入（千元）`) %>% 
  filter(est > 0) %>% 
  group_by(pchc) %>% 
  summarise(province = first(na.omit(province)), 
            city = first(na.omit(city)), 
            district = first(na.omit(district)), 
            est = sum(est, na.rm = TRUE)) %>% 
  ungroup()

hospital.universe <- bind_rows(imp.total, pchc.universe.m) %>% 
  group_by(pchc) %>% 
  summarise(province = first(province),
            city = first(na.omit(city)),
            district = first(na.omit(district)), 
            est = first(na.omit(est))) %>% 
  ungroup() %>% 
  filter(!is.na(province), !is.na(city), !is.na(district), !is.na(est)) %>% 
  mutate(flag_sample = if_else(pchc %in% unique(imp.total$pchc), 1, 0))

## city tier
city.tier <- read.xlsx("02_Inputs/pchc_city_tier.xlsx") %>% 
  group_by(city) %>% 
  mutate(tier = if_else(is.na(city_tier), first(na.omit(city_tier)), city_tier)) %>% 
  ungroup() %>% 
  mutate(tier = if_else(is.na(tier), 5, tier)) %>% 
  distinct(city, tier)


##---- Run Project ----
source('04_Codes/functions/ProjectSample.R', encoding = 'UTF-8')
# source('04_Codes/functions/ProjectSmallSample.R', encoding = 'UTF-8')
source('04_Codes/functions/ProjectNation.R', encoding = 'UTF-8')
source('04_Codes/functions/UpdatePrice.R', encoding = 'UTF-8')

proj.sample <- ProjectSample(raw.total = imp.total, 
                             pchc.universe = hospital.universe)

proj.nation <- ProjectNation(proj.sample.total = proj.sample, 
                             pchc.universe = hospital.universe, 
                             city.tier = city.tier)

proj.cs <- proj.nation %>% 
  mutate(sales = if_else(province == '上海', sales * 0.5, sales), 
         units = if_else(province == '上海', units * 0.5, units)) %>% 
  mutate(channel = 'CHC') %>% 
  bind_rows(bj.chs)

proj.price <- UpdatePrice(proj.nation = proj.cs, 
                          raw.total = imp.total)

write_feather(proj.price, '03_Outputs/Servier_CHC2_Proj.feather')


##---- Format info ----
## standard info
chpa.info <- read.xlsx('02_Inputs/ims_chpa_to20Q3.xlsx', cols = 1:21, startRow = 4) %>%  
  distinct(corp = Corp_Desc, type = MNF_TYPE, atc3 = ATC3_Code, atc4 = ATC4_Code,  
           molecule = Molecule_Desc, product = Prd_desc, pack = Pck_Desc,  
           packid = Pack_ID)

master.info <- read.xlsx('02_Inputs/Product standardization master data-A-S-1211.xlsx') %>% 
  mutate(PACK_ID = gsub('禁用', '', PACK_ID)) %>% 
  distinct(corp = CORP_NAME_EN, type = MNF_TYPE, atc3 = ATC3_CODE, atc4 = ATC4_CODE, 
           atc3_cn = ATC3, molecule = MOLE_NAME_EN, molecule_cn = MOLE_NAME_CH, 
           product = PROD_DESC, product_cn = PROD_NAME_CH, pack = PCK_DESC, 
           route = NFC1_NAME_CH, packid = PACK_ID)

std.info <- bind_rows(chpa.info, master.info) %>% 
  group_by(atc3) %>% 
  mutate(atc3_cn = first(na.omit(atc3_cn))) %>% 
  ungroup() %>% 
  group_by(molecule) %>% 
  mutate(molecule_cn = first(na.omit(molecule_cn))) %>% 
  ungroup() %>% 
  group_by(product) %>% 
  mutate(product_cn = first(na.omit(product_cn))) %>% 
  ungroup() %>% 
  group_by(packid) %>% 
  summarise(corp = first(na.omit(corp)), 
            type = first(na.omit(type)), 
            atc3 = first(na.omit(atc3)), 
            atc4 = first(na.omit(atc4)), 
            atc3_cn = first(na.omit(atc3_cn)), 
            molecule = first(na.omit(molecule)), 
            molecule_cn =first(na.omit(molecule_cn)), 
            product = first(na.omit(product)), 
            product_cn = first(na.omit(product_cn)), 
            pack = first(na.omit(pack)), 
            route = first(na.omit(route))) %>% 
  ungroup()

## VBP info
vbp.info.list <- map(1:4, 
                     function(x) {
                       print(x)
                       read.xlsx('02_Inputs/带量采购中标结果汇总（1-3批）_20201222.xlsx', 
                                 sheet = x) %>% 
                         mutate(`中标时间` = as.character(as.Date(as.numeric(`中标时间`), origin = '1899-12-30')), 
                                `执行日期` = as.character(as.Date(as.numeric(`执行日期`), origin = '1899-12-30')), 
                                sheet = x)
                     })

vbp.nat <- bind_rows(vbp.info.list) %>% 
  mutate(PACK_ID = stri_pad_left(as.numeric(PACK_ID), 7, 0)) %>% 
  filter(!is.na(PACK_ID)) %>% 
  mutate(city = 'National') %>% 
  distinct(city, molecule_cn = `通用名`, packid = PACK_ID)

vbp.city <- bind_rows(vbp.info.list) %>% 
  mutate(PACK_ID = stri_pad_left(as.numeric(PACK_ID), 7, 0)) %>% 
  filter(!is.na(PACK_ID)) %>% 
  filter(`省份` %in% c('北京', '上海', '浙江', '江苏')) %>% 
  mutate(`城市` = case_when(`省份` == '北京' ~ '北京', 
                          `省份` == '上海' ~ '上海', 
                          `省份` == '浙江' ~ '杭州', 
                          `省份` == '江苏' ~ '南京', 
                          TRUE ~ `城市`)) %>% 
  distinct(city = `城市`, molecule_cn = `通用名`, packid = PACK_ID)

vbp.info <- bind_rows(vbp.nat, vbp.city) %>% 
  mutate(`是否是中标品种` = '中标产品')

## city EN
city.en <- read.xlsx("02_Inputs/CityEN.xlsx")


##---- Run format ----
source('04_Codes/functions/FormatServier.R')

servier.city <- FormatServier(proj.price = proj.price, 
                              std.info = std.info, 
                              vbp.info = vbp.info, 
                              city.en = city.en)

servier.result <- servier.city %>% 
  filter(Channel == 'CHC', 
         Sales > 0, Units > 0, DosageUnits > 0) %>% 
  group_by(Pack_ID, Channel, Date, ATC3, ATC4, MKT, Molecule_Desc, Prod_Desc, 
           Pck_Desc, Corp_Desc, TherapeuticClsII, TherapeuticClsIII, Prod_CN_Name, 
           Package, Dosage, Quantity, `ATC3中文分类`, `给药途径`) %>% 
  summarise(Province = 'National', 
            City = 'National', 
            Sales = sum(Sales, na.rm = TRUE), 
            Units = sum(Units, na.rm = TRUE), 
            DosageUnits = sum(DosageUnits, na.rm = TRUE), 
            `CITY-EN` = 'National', 
            Sales_raw = sum(Sales_raw, na.rm = TRUE), 
            Units_raw = sum(Units_raw, na.rm = TRUE), 
            DosageUnits_raw = sum(DosageUnits_raw, na.rm = TRUE), 
            `是否进入带量采购` = first(na.omit(`是否进入带量采购`)), 
            `是否是原研` = first(na.omit(`是否是原研`)), 
            `是否是中标品种` = first(na.omit(`是否是中标品种`)), 
            `是否是MNC` = first(na.omit(`是否是MNC`))) %>% 
  ungroup() %>% 
  bind_rows(servier.city) %>% 
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

write.xlsx(servier.result, '03_Outputs/Servier_CHC2_2018Q1_2020Q3.xlsx')

