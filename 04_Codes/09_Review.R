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
  left_join(market.def, by = c('Pack_ID' = 'packid')) %>% 
  filter(!is.na(market), 
         stri_sub(quarter, 1, 4) %in% c('2018', '2019', '2020')) %>% 
  mutate(Prd_desc = trimws(stri_sub(Prd_desc, 1, -4))) %>% 
  select(Pack_ID, Date = quarter, ATC3 = ATC3_Code, MKT = market, Molecule_Desc, 
         Prod_Desc = Prd_desc, Pck_Desc, Corp_Desc, Units = UNIT, 
         Sales = RENMINBI)

write.xlsx(servier.chpa, '05_Internal_Review/Servier_CHC2_CHPA_2018Q1_2020Q3.xlsx')

# Update
chpa.info <- servier.chpa %>% 
  distinct(Pack_ID, ATC3, Molecule_Desc, Prod_Desc, Pck_Desc, Corp_Desc)

delivery.update <- servier.result %>% 
  distinct(Pack_ID, Channel, Province, City, Date, MKT, 
           Sales, Units, DosageUnits) %>% 
  left_join(chpa.info, by = 'Pack_ID') %>% 
  filter(!is.na(ATC3), stri_sub(Date, 1, 4) %in% c('2018', '2019', '2020'))

write.xlsx(delivery.update, '05_Internal_Review/Servier_CHC2_Delivery_Updated.xlsx')




