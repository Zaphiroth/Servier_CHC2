# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Servier CHC 2
# Purpose:      Main
# programmer:   Zhe Liu
# Date:         2020-12-10
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Universe info ----
## PCHC
pchc.universe.m <- pchc.universe %>% 
  filter(!is.na(`单位名称`), !is.na(PCHC_Code)) %>% 
  group_by(province = `省`, city = `地级市`, district = `区[县/县级市】`, hospital = `单位名称`) %>% 
  summarise(pchc = first(PCHC_Code), 
            est = first(na.omit(`其中：西药药品收入（千元）`))) %>% 
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

## market definition
market.def <- read_xlsx("02_Inputs/市场分子式明细_chk_20201127.xlsx") %>% 
  distinct(atc3 = ATCIII.Code, molecule = Molecule.Composition.Name, market = TC)


# pchc.universe <- bind_rows(raw.total, pchc.universe1) %>% 
#   group_by(pchc) %>% 
#   summarise(province = first(province),
#             city = first(na.omit(city)),
#             district = first(na.omit(district)), 
#             est = first(na.omit(est))) %>% 
#   ungroup() %>% 
#   filter(!is.na(province), !is.na(city), !is.na(district), !is.na(est)) %>% 
#   mutate(flag_sample = if_else(pchc %in% unique(raw.total$pchc), 1, 0))

# pchc.info <- bind_rows(raw.total, pchc.info) %>% 
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


##---- Run Project ----
source('04_Codes/functions/ProjectSample.R', encoding = 'UTF-8')
# source('04_Codes/functions/ProjectSmallSample.R', encoding = 'UTF-8')
source('04_Codes/functions/ProjectNation.R', encoding = 'UTF-8')
source('04_Codes/functions/UpdatePrice.R', encoding = 'UTF-8')

proj.sample <- ProjectSample(raw.total = imp.total, 
                             pchc.universe = hospital.universe)

# proj.small <- ProjectSmallSample(raw.total = raw.total, 
#                                  pchc.info = pchc.info, 
#                                  small = '上海', 
#                                  sample = c('上海', '北京'))

# proj.sample.total <- proj.sample %>% 
#   filter(!(province %in% '上海')) %>% 
#   bind_rows(proj.small)

proj.nation <- ProjectNation(proj.sample.total = proj.sample, 
                             pchc.universe = hospital.universe, 
                             city.tier = city.tier)

proj.price <- UpdatePrice(proj.nation = proj.nation, 
                          raw.total = raw.total)


##---- Projection rate ----
source('04_Codes/Review.R', encoding = 'UTF-8')

proj.rate <- ReviewFunc(raw.total, 
                        proj.sample.total, 
                        proj.nation, 
                        market.def, 
                        target.city)

servier.proj.rate <- proj.rate %>% 
  left_join(market.def, by = 'packid') %>% 
  filter(!is.na(market)) %>% 
  filter(city %in% target.city) %>% 
  select(quarter, date, province, city, market, molecule, product, packid, 
         starts_with('raw'), starts_with('proj'))

write.xlsx(servier.proj.rate, '05_Internal_Review/Seriver_Projection_Rate.xlsx')


##---- Format info ----
# zs flag
zs.flag.raw <- read.xlsx("02_Inputs/13城市的招标flag_zs_flag.xlsx")

zs.flag <- zs.flag.raw %>% 
  filter(!is.na(`是否是13城市`)) %>% 
  distinct(province = `省`, city = `地级市`, pchc = PCHC_Code, zs_flag)

# corporation
corp.ref <- fread("02_Inputs/cn_corp_ref_201903_1.txt", 
                  stringsAsFactors = FALSE, sep = "|") %>% 
  distinct()

# pack
pack.ref <- fread("02_Inputs/cn_prod_ref_201903_1.txt", 
                  stringsAsFactors = FALSE, sep = "|") %>% 
  distinct() %>% 
  mutate(Pack_Id = stri_pad_left(Pack_Id, 7, 0))

corp.pack <- pack.ref %>% 
  select("Pack_Id", "Pck_Desc", "Corp_ID") %>% 
  distinct() %>% 
  left_join(corp.ref, by = "Corp_ID") %>% 
  mutate(Corp_Desc = if_else(Corp_Desc == "LUYE GROUP", "LVYE GROUP", Corp_Desc)) %>% 
  select(packid = Pack_Id, pack_desc = Pck_Desc, corp_desc = Corp_Desc)

# pack size
pack.size <- pack.ref %>% 
  distinct(packid = Pack_Id, pack_size = PckSize_Desc)

# 4+7 flag
capital.47 <- read_xlsx("02_Inputs/4+7+省会名单.xlsx") %>% 
  filter(`类别` %in% "4+7城市") %>% 
  mutate(city = gsub("市", "", `城市`)) %>% 
  select(city, `是否是4+7城市` = `类别`)

# bid name
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

# corporation, ATC3
corp.atc3 <- read_xlsx("02_Inputs/产品性质_chpa 08.23(1).xlsx", sheet = 1)

corp.type <- distinct(corp.atc3, corp_desc = Corp_Desc, Mnf_Type = `厂家性质`)

atc3.cn <- distinct(corp.atc3, atc3 = ATC3_Code, `ATC3中文分类` = `类别`)

molecule.cn <- distinct(corp.atc3, molecule = Molecule_Desc, Molecule_CN = `分子`)

corp.add <- read_xlsx("02_Inputs/Corp_Info_20200720.xlsx") %>% 
  group_by(corp_desc = Corp_Desc) %>% 
  arrange(MnfType_Desc) %>% 
  summarise(Mnf_Type1 = first(MnfType_Desc)) %>% 
  ungroup() %>% 
  mutate(Mnf_Type1 = if_else(is.na(Mnf_Type1), "Local", Mnf_Type1),
         Mnf_Type1 = if_else(Mnf_Type1 %in% c("Imported", "Joint Venture"), "MNC", Mnf_Type1))

# new profile
packid.profile.raw <- read_xlsx("02_Inputs/packid_prod_20181112.xlsx")

packid.profile <- packid.profile.raw %>% 
  mutate(packid = stri_pad_left(Pack_Id, 7, 0)) %>% 
  distinct(packid, ATC4_Code, ims_product_cn)

prod.profile <- packid.profile %>%
  mutate(prodid = substr(packid, 1, 5)) %>% 
  select(prodid, ATC4_Code1 = ATC4_Code, ims_product_cn1 = ims_product_cn) %>%
  distinct()

# city EN
city.en <- read.xlsx("02_Inputs/CityEN.xlsx")


##---- Run format ----
source('04_Codes/FormatServier.R')

servier.result <- FormatServier(proj.price = proj.price, 
                                target.city = target.city, 
                                market.def = market.def, 
                                corp.pack = corp.pack, 
                                pack.size = pack.size, 
                                capital.47 = capital.47, 
                                prod.bid = prod.bid, 
                                corp.type = corp.type, 
                                atc3.cn = atc3.cn, 
                                molecule.cn = molecule.cn, 
                                corp.add = corp.add, 
                                packid.profile = packid.profile, 
                                prod.profile = prod.profile, 
                                city.en = city.en)

write.xlsx(servier.result, '03_Outputs/Servier_CHC_Result.xlsx')

