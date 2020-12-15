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
  distinct(province = `省`, city = `地级市`, district = `区[县/县级市]`, 
           pchc = `新版PCHC_Code`, est = `其中：西药药品收入（千元）`) %>% 
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

proj.price <- UpdatePrice(proj.nation = proj.nation, 
                          raw.total = imp.total)

write_feather(proj.price, '03_Outputs/Servier_CHC2_Proj.feather')


##---- Format info ----
## zs flag
zs.flag <- read.xlsx("02_Inputs/13城市的招标flag_zs_flag.xlsx") %>% 
  filter(!is.na(`是否是13城市`)) %>% 
  distinct(province = `省`, city = `地级市`, pchc = PCHC_Code, zs_flag)

## 4+7 flag
capital.47 <- read_xlsx("02_Inputs/4+7+省会名单.xlsx") %>% 
  filter(`类别` %in% "4+7城市") %>% 
  mutate(city = gsub("市", "", `城市`)) %>% 
  select(city, `是否是4+7城市` = `类别`)

## bid name
prod.bid <- read_xlsx("02_Inputs/Displayname Mapping.xlsx", sheet = 1) %>% 
  mutate(Pack_ID = stri_pad_left(Pack_ID, 7, 0),
         prodid = stri_sub(Pack_ID, 1, 5),
         `Display Name3 CN` = if_else(`Display Name3 CN` %in% c("中标品规", "非中标品规"), 
                                      "仿制", 
                                      `Display Name3 CN`),
         `Display Name3 CN` = gsub("-2|-1", "", `Display Name3 CN`),
         `Display Name2 CN` = gsub("-2|-1", "", `Display Name2 CN`)) %>% 
  distinct(name1 = `Display Name1 CN`,
           name2 = `Display Name2 CN`,
           name3 = `Display Name3 CN`,
           prodid)

## corporation, ATC3
corp.atc3 <- read_xlsx("02_Inputs/产品性质_chpa 08.23(1).xlsx", sheet = 1)

atc3.cn <- distinct(corp.atc3, atc3 = ATC3_Code, `ATC3中文分类` = `类别`)

molecule.cn <- distinct(corp.atc3, molecule = Molecule_Desc, Molecule_CN = `分子`)

## new profile
packid.profile.raw <- read_xlsx("02_Inputs/packid_prod_20181112.xlsx")

pack.profile <- packid.profile.raw %>% 
  mutate(packid = stri_pad_left(Pack_Id, 7, 0)) %>% 
  distinct(packid, ims_product_cn)

prod.profile <- pack.profile %>%
  mutate(prodid = substr(packid, 1, 5)) %>% 
  distinct(prodid, ims_product_cn1 = ims_product_cn)

## city EN
city.en <- read.xlsx("02_Inputs/CityEN.xlsx")


##---- Run format ----
source('04_Codes/functions/FormatServier.R')

servier.city <- FormatServier(proj.price = proj.price, 
                              market.def = market.def, 
                              city.en = city.en, 
                              capital.47 = capital.47, 
                              prod.bid = prod.bid, 
                              atc3.cn = atc3.cn, 
                              molecule.cn = molecule.cn, 
                              pack.profile = pack.profile, 
                              prod.profile = prod.profile)

servier.result <- servier.city %>% 
  group_by(Pack_ID, Channel, Date, ATC3, MKT, Molecule_Desc, Prod_Desc, 
           Pck_Desc, Corp_Desc, TherapeuticClsII, Prod_CN_Name, Package, Dosage, 
           Quantity, `是否进入带量采购`, `是否是原研`, `是否是中标品种`, 
           `是否是MNC`, `ATC3中文分类`) %>% 
  summarise(Province = 'Nation', 
            City = 'Nation', 
            `CITY-EN` = 'Nation', 
            `是否是4+7城市` = NA_character_, 
            Sales = sum(Sales, na.rm = TRUE), 
            Units = sum(Units, na.rm = TRUE), 
            DosageUnits = sum(DosageUnits, na.rm = TRUE)) %>% 
  ungroup() %>% 
  bind_rows(servier.city) %>% 
  filter(City %in% c('Nation', kTargetCity)) %>% 
  mutate(Sales = if_else(MKT == 'HTN' & ATC3 == 'C07', Sales * 0.75, Sales), 
         Sales = if_else(MKT == 'HTN' & ATC3 == 'C08', Sales * 0.9, Sales), 
         Sales = if_else(MKT == 'IHD' & ATC3 == 'C07', Sales * 0.25, Sales), 
         Sales = if_else(MKT == 'IHD' & ATC3 == 'C08', Sales * 0.1, Sales), 
         Units = if_else(MKT == 'HTN' & ATC3 == 'C07', Units * 0.75, Units), 
         Units = if_else(MKT == 'HTN' & ATC3 == 'C08', Units * 0.9, Units), 
         Units = if_else(MKT == 'IHD' & ATC3 == 'C07', Units * 0.25, Units), 
         Units = if_else(MKT == 'IHD' & ATC3 == 'C08', Units * 0.1, Units), 
         DosageUnits = if_else(MKT == 'HTN' & ATC3 == 'C07', DosageUnits * 0.75, DosageUnits), 
         DosageUnits = if_else(MKT == 'HTN' & ATC3 == 'C08', DosageUnits * 0.9, DosageUnits), 
         DosageUnits = if_else(MKT == 'IHD' & ATC3 == 'C07', DosageUnits * 0.25, DosageUnits), 
         DosageUnits = if_else(MKT == 'IHD' & ATC3 == 'C08', DosageUnits * 0.1, DosageUnits)) %>% 
  mutate(Sales = round(Sales, 2), 
         Units = round(Units), 
         DosageUnits = round(DosageUnits)) %>% 
  select(Pack_ID, Channel, Province, City, Date, ATC3, MKT, Molecule_Desc, 
         Prod_Desc, Corp_Desc, Sales, Units, DosageUnits, `CITY-EN`, 
         TherapeuticClsII, Prod_CN_Name, Package, Dosage, Quantity, 
         `是否是4+7城市`, `是否进入带量采购`, `是否是原研`, `是否是中标品种`, 
         `是否是MNC`, `ATC3中文分类`)

write.xlsx(servier.result, '03_Outputs/Servier_CHC2_2018Q1_2020Q3.xlsx')

