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
         Corp_Desc, Units = UNIT, Sales = RENMINBI)

write.xlsx(servier.chpa, '05_Internal_Review/Servier_CHC2_CHPA_2018Q1_2020Q3.xlsx')


##---- Price ----
price.check <- servier.result %>% 
  mutate(price = round(Sales / Units)) %>% 
  distinct(Channel, Date, City, MKT, Molecule_Desc, Prod_Desc, Pack_ID, price) %>% 
  pivot_wider(id_cols = c(Channel, City, MKT, Molecule_Desc, Prod_Desc, Pack_ID), 
              names_from = Date, 
              values_from = price)

write.xlsx(price.check, '05_Internal_Review/Servier_CHC2_2018Q1_2020Q3_Price_Check.xlsx')


##---- Market ----
missing.mol <- market.def %>% 
  filter(!(molecule %in% servier.result$Molecule_Desc))

write.xlsx(missing.mol, '05_Internal_Review/Missing_Market_Def.xlsx')
