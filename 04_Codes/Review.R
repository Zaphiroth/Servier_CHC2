# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  Servier CHC 2
# Purpose:      Review
# programmer:   Zhe Liu
# date:         2020-12-11
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


##---- Check SOP -----
# CHPA
chpa.format <- read.xlsx('05_Internal_Review/ims_chpa_to20Q3_format.xlsx')

servier.chpa <- chpa.format %>% 
  pivot_longer(cols = c(ends_with('UNIT'), ends_with('RENMINBI')), 
               names_to = 'quarter', 
               values_to = 'value') %>% 
  separate(quarter, c('quarter', 'measure'), sep = '_') %>% 
  pivot_wider(id_cols = c(Pack_ID, ATC3_Code, Molecule_Desc, Prd_desc, 
                          Pck_Desc, Corp_Desc, quarter), 
              names_from = measure, 
              values_from = value) %>% 
  left_join(market.def, by = c('ATC3_Code' = 'atc3', 'Molecule_Desc' = 'molecule')) %>% 
  filter(!is.na(market), 
         UNIT > 0, RENMINBI > 0, 
         stri_sub(quarter, 1, 4) %in% c('2018', '2019', '2020')) %>% 
  select(Pack_ID, Date = quarter, ATC3 = ATC3_Code, MKT = market, 
         Molecule_Desc, Prod_Desc = Prd_desc, Pck_Desc, 
         Corp_Desc, Units = UNIT, Sales = RENMINBI) %>% 
  filter(!(ATC3 == 'V03B' & !(Prod_Desc %in% kTCM)))

write.xlsx(servier.chpa, '05_Internal_Review/Servier_CHC2_CHPA_2018Q1_2020Q3.xlsx')


##---- Price ----
price.check <- servier.result %>% 
  mutate(price = round(Sales / Units)) %>% 
  group_by(Channel, City, MKT, Molecule_Desc, Prod_Desc, Pack_ID) %>% 
  mutate(Sales = sum(Sales, na.rm = TRUE)) %>% 
  ungroup() %>% 
  arrange(Channel, City, MKT, -Sales, Date) %>% 
  distinct(Channel, Date, City, MKT, Molecule_Desc, Prod_Desc, Pack_ID, price) %>% 
  pivot_wider(id_cols = c(Channel, City, MKT, Molecule_Desc, Prod_Desc, Pack_ID), 
              names_from = Date, 
              values_from = price)

write.xlsx(price.check, '05_Internal_Review/Servier_CHC2_2018Q1_2020Q3_Price_Check.xlsx')


##---- Market ----
missing.mol <- market.def %>% 
  filter(!(molecule %in% servier.result$Molecule_Desc))

write.xlsx(missing.mol, '05_Internal_Review/Missing_Market_Def.xlsx')


##---- Shanghai ----
servier.sh1 <- read.xlsx('02_Inputs/CHC_MAX_16Q420Q3_1222.xlsx') %>% 
  filter(Province == '上海') %>% 
  group_by(channel = Channel, year = stri_sub(Date, 1, 4), quarter = Date, 
           province = Province, city = City, market = MKT, packid = stri_pad_left(Pack_ID, 7, 0)) %>% 
  summarise(units = sum(Units, na.rm = TRUE), 
            sales = sum(Sales, na.rm = TRUE)) %>% 
  ungroup()

format.sh1 <- FormatServier(proj.price = servier.sh1, 
                            std.info = std.info, 
                            vbp.info = vbp.info, 
                            city.en = city.en)

result.sh1 <- format.sh1 %>% 
  filter(Sales > 0, Units > 0, DosageUnits > 0) %>% 
  mutate(`Period-MAT` = case_when(
    Date %in% c('2020Q3', '2020Q2', '2020Q1', '2019Q4') ~ 'MAT20Q3', 
    Date %in% c('2019Q3', '2019Q2', '2019Q1', '2018Q4') ~ 'MAT19Q3', 
    TRUE ~ NA_character_
  )) %>% 
  select(Pack_ID, Channel, Province, City, Date, ATC3, ATC4, MKT, Molecule_Desc, 
         Prod_Desc, Pck_Desc, Corp_Desc, Sales = Sales_raw, Units = Units_raw, 
         DosageUnits = DosageUnits_raw, `Period-MAT`,`CITY-EN`, TherapeuticClsII, 
         TherapeuticClsIII, Prod_CN_Name, Package, Dosage, Quantity, 
         `是否进入带量采购`, `是否是原研`, `是否是中标品种`, `是否是MNC`, 
         `ATC3中文分类`, `给药途径`) %>% 
  mutate(
    Sales_raw = case_when(
      MKT == 'HTN' & stri_sub(ATC3, 1, 3) == 'C07' ~ Sales / 0.75, 
      MKT == 'IHD' & stri_sub(ATC3, 1, 3) == 'C07' ~ Sales / 0.25, 
      MKT == 'HTN' & stri_sub(ATC3, 1, 3) == 'C08' ~ Sales / 0.9, 
      MKT == 'IHD' & stri_sub(ATC3, 1, 3) == 'C08' ~ Sales / 0.1, 
      TRUE ~ Sales
    ), 
    Units_raw = case_when(
      MKT == 'HTN' & stri_sub(ATC3, 1, 3) == 'C07' ~ Units / 0.75, 
      MKT == 'IHD' & stri_sub(ATC3, 1, 3) == 'C07' ~ Units / 0.25, 
      MKT == 'HTN' & stri_sub(ATC3, 1, 3) == 'C08' ~ Units / 0.9, 
      MKT == 'IHD' & stri_sub(ATC3, 1, 3) == 'C08' ~ Units / 0.1, 
      TRUE ~ Units
    ), 
    DosageUnits_raw = case_when(
      MKT == 'HTN' & stri_sub(ATC3, 1, 3) == 'C07' ~ DosageUnits / 0.75, 
      MKT == 'IHD' & stri_sub(ATC3, 1, 3) == 'C07' ~ DosageUnits / 0.25, 
      MKT == 'HTN' & stri_sub(ATC3, 1, 3) == 'C08' ~ DosageUnits / 0.9, 
      MKT == 'IHD' & stri_sub(ATC3, 1, 3) == 'C08' ~ DosageUnits / 0.1, 
      TRUE ~ DosageUnits
    )
  )

write.xlsx(result.sh1, '03_Outputs/Servier_SH.xlsx')


##---- Market check ----
market.non <- read_xlsx('02_Inputs/痔疮静脉市场确认名单_1214.xlsx', sheet = '未购买')

market.wm <- market.non %>% 
  filter(`中药/西药` == '西药') %>% 
  left_join(chpa.info, by = c('ATCIII Code' = 'atc3', 'Molecule Composition Name' = 'molecule')) %>% 
  mutate(loc = stri_locate_first(pack, regex = "\\d")[,1], 
         package = trimws(stri_sub(pack, 1, loc-1)), 
         Note = case_when(
           package %in% c('CAP', 'SOL UNIT DOS', 'TAB', 'TAB FLM CTD', 'TAB SC') ~ '口服', 
           package %in% c('CRM', 'GEL', 'OINT RECTAL', 'SUPPOS') ~ '外用', 
           package %in% c('AMP', 'INF VIAL DRY', 'INFUSION', 'VIAL DRY') ~ '注射', 
           TRUE ~ NA_character_
         )) %>% 
  group_by(`中药/西药`, `ATCIII Code`, `Molecule Composition Name`, Note) %>% 
  summarise(`MAT06,2020` = sum(`MAT06,2020`, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(prop = `MAT06,2020` / sum(`MAT06,2020`, na.rm = TRUE))

market.tcm <- market.non %>% 
  filter(`中药/西药` == '中药') %>% 
  mutate(Product = stri_paste(stri_sub(Product, 1, 19), stri_sub(Product, -3, -1))) %>% 
  left_join(chpa.info, by = c('ATCIII Code' = 'atc3', 'Molecule Composition Name' = 'molecule', 
                              'Product' = 'product')) %>% 
  mutate(loc = stri_locate_first(pack, regex = "\\d")[,1], 
         package = trimws(stri_sub(pack, 1, loc-1)), 
         Note = case_when(
           package %in% c('CAP', 'GRAN U/DOSE', 'PILL', 'TAB FLM CTD', 'TAB SC') ~ '口服', 
           package %in% c('AEROSOL', 'LIQ', 'LOT', 'OINT', 'OINT U/DOSE', 'SUPPOS') ~ '外用', 
           TRUE ~ NA_character_
         )) %>% 
  group_by(`中药/西药`, `ATCIII Code`, `Molecule Composition Name`, Note) %>% 
  summarise(`MAT06,2020` = sum(`MAT06,2020`, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(prop = `MAT06,2020` / sum(`MAT06,2020`, na.rm = TRUE))

write.xlsx(bind_rows(market.wm, market.tcm), '05_Internal_Review/痔疮静脉市场未购买.xlsx')


##---- Adjustment ----
servier.delivery <- read.xlsx('06_Deliveries/Servier_CHC2_2018Q4_2020Q3_20210106_final_hx.xlsx')

servier.delivery.adj <- servier.delivery %>% 
  filter(!(ATC3 != 'A10S' & `给药途径` == '注射用药')) %>% 
  mutate(MKT = if_else(MKT == 'Diabetes', 'OAD', MKT), 
         Product = trimws(stri_sub(Prod_Desc, 1, -4)), 
         Prod_Desc = if_else(stri_sub(Prod_Desc, -3, -1) == 'J1J', 
                             gsub('J1J', 'JJJ', Prod_Desc), 
                             Prod_Desc), 
         `是否进入带量采购` = if_else(Molecule_Desc %in% vbp.info$molecule, 
                              '4+7分子', NA_character_), 
         Pack_ID = stri_pad_left(Pack_ID, 7, 0)) %>% 
  select(-`是否是中标品种`) %>% 
  left_join(vbp.info, by = c('City' = 'city', 'Molecule_Desc' = 'molecule', 'Pack_ID' = 'packid')) %>% 
  select(Pack_ID, Channel, City, Date, ATC3, ATC4, MKT, Molecule_Desc, 
         Prod_Desc, Pck_Desc, Corp_Desc, Sales, Units, DosageUnits, `QTR-MAT`, 
         `CITY-EN`, TherapeuticClsII, TherapeuticClsIII, Prod_CN_Name, Package, 
         Dosage, Quantity, `是否进入带量采购`, `是否是原研`, `是否是中标品种`, 
         `是否是MNC`, `ATC3中文分类`, Sales_raw, Units_raw, DosageUnits_raw, 
         `给药途径`, Product)

write.xlsx(servier.delivery.adj, '06_Deliveries/Servier_CHC2_2018Q4_2020Q3_final.xlsx')


##---- TCM product CN ----
venous.raw <- read.xlsx('05_Internal_Review/Venous_Disease_MKT.xlsx', sheet = 'CMAX_Updated')
prod.cn <- read.xlsx('02_Inputs/Product standardization master data-A-S-0106_updated.xlsx') %>% 
  distinct(PACK_ID, PROD_NAME_CH, NFC1_NAME_CH)

venous.update <- venous.raw %>% 
  left_join(prod.cn, by = c('Pack_ID' = 'PACK_ID')) %>% 
  mutate(Prod_CN_Name = if_else(is.na(Prod_CN_Name), PROD_NAME_CH, Prod_CN_Name), 
         `给药途径` = if_else(is.na(`给药途径`), NFC1_NAME_CH, `给药途径`), 
         Product = trimws(stri_sub(Prod_Desc, 1, -4))) %>% 
  select(-PROD_NAME_CH, -NFC1_NAME_CH)

write.xlsx(venous.update, '03_Outputs/Venous_Market_Update.xlsx')


##---- Adjustment ----
delivery1 <- read.xlsx('06_Deliveries/Servier_CHC2_2018Q4_2020Q3_20210106_final.xlsx')
delivery2 <- read.xlsx('06_Deliveries/Servier_CHC2_2018Q4_2020Q3_Venous_MKT_20210107_final_m.xlsx')

delivery1.adj <- delivery1 %>% 
  rename(Value = Sales, 
         `category I` = TherapeuticClsII, 
         `category II` = TherapeuticClsIII) %>% 
  mutate(MKT = if_else(MKT == 'OAD', 'Diabetes', MKT), 
         `category II` = if_else(MKT == 'Diabetes', `category I`, `category II`), 
         `category I` = case_when(MKT == 'Diabetes' ~ 'OAD&GLP-1', 
                                  `category I` == 'ANTI-HTN' ~ 'Others', 
                                  `category I` == 'DIURETICS' ~ 'DU', 
                                  `category I` == 'RAASi Plain' ~ 'RAASi PLAIN', 
                                  `category I` == 'NITRITES' ~ 'LAN', 
                                  TRUE ~ `category I`), 
         `category II` = case_when(`category II` == 'A+C FDC' ~ 'A+C', 
                                   `category II` == 'A+D FDC' ~ 'A+D', 
                                   `category II` == 'ACEi PLAIN' ~ 'ACEI', 
                                   `category II` == 'ARB PLAIN' ~ 'ARB', 
                                   `category II` == 'AG Is' ~ 'AGI', 
                                   `category II` == 'BIGUANIDE' ~ 'MET', 
                                   `category II` == 'DPP-IV' ~ 'DPP-4', 
                                   `category II` == 'GLITAZONE' ~ 'TZD', 
                                   `category II` == 'METAGLINIDE' ~ 'GLINIDE', 
                                   `category II` == 'SULPHONYLUREA' ~ 'SU', 
                                   TRUE ~ `category II`))

delivery2.adj <- delivery2 %>% 
  rename(Value = Sales, 
         `category I` = TherapeuticClsII, 
         `category II` = TherapeuticClsIII) %>% 
  mutate(`category II` = case_when(`category I` == 'TCM' & `给药途径` == '口服固体' ~ 'TCM Oral', 
                                   `category I` == 'TCM' & `给药途径` == '外用制剂' ~ 'TCM Topical', 
                                   TRUE ~ NA_character_))

write.xlsx(delivery1.adj, '06_Deliveries/Servier_CHC2_2018Q4_2020Q3_HTN&IHD&Diabetes.xlsx')
write.xlsx(delivery2.adj, '06_Deliveries/Servier_CHC2_2018Q4_2020Q3_Venou.xlsx')





