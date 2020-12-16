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
         UNIT > 0, RENMINBI > 0, 
         stri_sub(quarter, 1, 4) %in% c('2018', '2019', '2020')) %>% 
  mutate(atc3 = stri_sub(atc4, 1, 4)) %>% 
  select(Pack_ID, Date = quarter, ATC3 = atc3, MKT = market, 
         Molecule_Desc = molecule, Prod_Desc = Prd_desc, Pck_Desc = pack, 
         Corp_Desc = corp, Units = UNIT, Sales = RENMINBI)

write.xlsx(servier.chpa, '05_Internal_Review/Servier_CHC2_CHPA_2018Q1_2020Q3.xlsx')

# Update
# chpa.info <- servier.chpa %>% 
#   distinct(Pack_ID, ATC3, Molecule_Desc, Prod_Desc, Pck_Desc, Corp_Desc)

# delivery.update <- servier.result %>% 
#   distinct(Pack_ID, Channel, Province, City, Date, ATC3, MKT, Molecule_Desc, 
#            Prod_Desc, Corp_Desc, Sales, Units, DosageUnits) %>% 
#   left_join(chpa.info, by = 'Pack_ID') %>% 
#   filter(!is.na(ATC3), stri_sub(Date, 1, 4) %in% c('2018', '2019', '2020'))

# write.xlsx(delivery.update, '05_Internal_Review/Servier_CHC2_Delivery_Updated.xlsx')


##---- Price ----
price.check <- servier.result %>% 
  mutate(price = round(Sales / Units)) %>% 
  distinct(Channel, Date, City, MKT, Molecule_Desc, Prod_Desc, Pack_ID, price) %>% 
  pivot_wider(id_cols = c(Channel, City, MKT, Molecule_Desc, Prod_Desc, Pack_ID), 
              names_from = Date, 
              values_from = price)

write.xlsx(price.check, '05_Internal_Review/Servier_CHC2_2018Q1_2020Q3_Price_Check.xlsx')


##---- Market ----
missing.pack <- market.def %>% filter(!(packid %in% raw.total$packid))
missing.mol <- market.def %>% filter(!(stri_sub(packid, 1, 5) %in% stri_sub(raw.total$packid, 1, 5)))

wb <- createWorkbook()
addWorksheet(wb, 'Missing_pack')
addWorksheet(wb, 'Missing_mol')
writeDataTable(wb, 'Missing_pack', missing.pack)
writeDataTable(wb, 'Missing_mol', missing.mol)
saveWorkbook(wb, '05_Internal_Review/Missing_Market_Def.xlsx', overwrite = TRUE)




