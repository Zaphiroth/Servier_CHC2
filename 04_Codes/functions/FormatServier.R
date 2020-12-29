# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  CHC Functionalize
# Purpose:      Format Servier
# programmer:   Zhe Liu
# Date:         2020-12-16
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


FormatServier <- function(proj.price, 
                          std.info, 
                          vbp.info, 
                          city.en) {
  
  ##---- Summary ----
  servier.result <- proj.price %>% 
    group_by(channel, year, quarter, province, city, market, packid) %>% 
    summarise(sales = sum(sales, na.rm = TRUE), 
              units = sum(units, na.rm = TRUE)) %>% 
    ungroup() %>% 
    left_join(city.en, by = 'city') %>% 
    left_join(std.info, by = 'packid') %>% 
    left_join(vbp.info, by = c('city', 'molecule_cn', 'packid')) %>% 
    mutate(`是否进入带量采购` = if_else(molecule_cn %in% unique(vbp.info$molecule_cn), 
                                '4+7分子', NA_character_), 
           `是否是原研` = if_else(type != 'L', '原研', '仿制'), 
           `是否是MNC` = if_else(type != 'L', 'MNC', 'Local')) %>% 
    mutate(first_num_position = stri_locate_first(pack, regex = '\\d')[,1],
           last_space_position = stri_locate_last(pack, regex = '\\s')[,1],
           Package = str_squish(substr(pack, 1, first_num_position - 1)),
           Dosage = str_squish(substr(pack, first_num_position, 
                                      last_space_position - 1)),
           Quantity = as.integer(str_squish(substr(pack, last_space_position, 
                                                   nchar(pack))))) %>% 
    mutate(
      TherapeuticClsII = case_when(
        market == 'HTN' & atc3 == 'C09A' ~ 'RAASi Plain', 
        market == 'HTN' & atc3 == 'C09C' ~ 'RAASi Plain', 
        market == 'HTN' & atc3 == 'C09B' ~ 'RAASi FDC', 
        market == 'HTN' & atc3 == 'C09D' ~ 'RAASi FDC', 
        market == 'HTN' & atc3 == 'C02A' ~ 'ANTI-HTN', 
        market == 'HTN' & atc3 == 'C02B' ~ 'ANTI-HTN', 
        market == 'HTN' & atc3 == 'C02C' ~ 'ANTI-HTN', 
        market == 'HTN' & atc3 == 'C03A' ~ 'DIURETICS', 
        market == 'HTN' & atc3 == 'C07A' ~ 'BB', 
        market == 'HTN' & atc3 == 'C08A' ~ 'CCB', 
        market == 'HTN' & atc3 == 'C08B' ~ 'CCB', 
        market == 'Diabetes' & atc3 == 'A10H' ~ 'SULPHONYLUREA', 
        market == 'Diabetes' & atc3 == 'A10J' ~ 'BIGUANIDE', 
        market == 'Diabetes' & atc3 == 'A10K' ~ 'GLITAZONE', 
        market == 'Diabetes' & atc3 == 'A10L' ~ 'AG Is', 
        market == 'Diabetes' & atc3 == 'A10M' ~ 'METAGLINIDE', 
        market == 'Diabetes' & atc3 == 'A10N' ~ 'DPP-IV', 
        market == 'Diabetes' & atc3 == 'A10P' ~ 'SGLT2', 
        market == 'Diabetes' & atc3 == 'A10S' ~ 'GLP-1', 
        market == 'Diabetes' & atc3 == 'A10X' ~ 'OTHERS', 
        market == 'IHD' & atc3 == 'C07A' ~ 'BB', 
        market == 'IHD' & atc3 == 'C08A' ~ 'CCB', 
        market == 'IHD' & atc3 == 'C08B' ~ 'CCB', 
        market == 'IHD' & atc3 == 'C01E' ~ 'NITRITES', 
        market == 'IHD' & atc3 == 'C01D' & molecule == 'TRIMETAZIDINE' ~ 'TMZ', 
        market == 'IHD' & atc3 == 'C01D' & molecule != 'TRIMETAZIDINE'~ 'OTHERS', 
        market == 'Venous Disease' & atc3 == 'C05A' ~ 'HD Topical', 
        market == 'Venous Disease' & atc3 == 'C05B' ~ 'CVD Topical', 
        market == 'Venous Disease' & atc3 == 'C05C' ~ 'VAD Oral', 
        market == 'Venous Disease' & stri_sub(atc3, 1, 3) == 'V03' ~ 'TCM', 
        TRUE ~ NA_character_
      ), 
      TherapeuticClsIII = case_when(
        atc3 == 'C09A' ~ 'ACEi PLAIN', 
        atc3 == 'C09C' ~ 'ARB PLAIN', 
        atc4 %in% c('C09B3', 'C09D3') ~ 'A+C FDC', 
        atc4 %in% c('C09B1', 'C09D1') ~ 'A+D FDC', 
        TRUE ~ NA_character_
      )
    ) %>% 
    mutate(
      sales_adj = case_when(
        market == 'HTN' & stri_sub(atc3, 1, 3) == 'C07' ~ sales * 0.75, 
        market == 'IHD' & stri_sub(atc3, 1, 3) == 'C07' ~ sales * 0.25, 
        market == 'HTN' & stri_sub(atc3, 1, 3) == 'C08' ~ sales * 0.9, 
        market == 'IHD' & stri_sub(atc3, 1, 3) == 'C08' ~ sales * 0.1, 
        TRUE ~ sales
      ), 
      units_adj = case_when(
        market == 'HTN' & stri_sub(atc3, 1, 3) == 'C07' ~ units * 0.75, 
        market == 'IHD' & stri_sub(atc3, 1, 3) == 'C07' ~ units * 0.25, 
        market == 'HTN' & stri_sub(atc3, 1, 3) == 'C08' ~ units * 0.9, 
        market == 'IHD' & stri_sub(atc3, 1, 3) == 'C08' ~ units * 0.1, 
        TRUE ~ units
      )
    ) %>% 
    mutate(sales_adj = round(sales_adj, 2), 
           units_adj = round(units_adj), 
           dosageunits_adj = round(Quantity * units_adj), 
           sales = round(sales, 2), 
           units = round(units), 
           dosageunits = round(Quantity * units)) %>% 
    select(Pack_ID = packid, 
           Channel = channel, 
           Province = province, 
           City = city, 
           Date = quarter, 
           ATC3 = atc3, 
           ATC4 = atc4, 
           MKT = market, 
           Molecule_Desc = molecule, 
           Prod_Desc = product, 
           Pck_Desc = pack, 
           Corp_Desc = corp, 
           Sales = sales_adj, 
           Units = units_adj, 
           DosageUnits = dosageunits_adj, 
           `CITY-EN`, 
           TherapeuticClsII, 
           TherapeuticClsIII, 
           Prod_CN_Name = product_cn, 
           Package, 
           Dosage, 
           Quantity, 
           `是否进入带量采购`, 
           `是否是原研`, 
           `是否是中标品种`, 
           `是否是MNC`, 
           `ATC3中文分类` = atc3_cn, 
           Sales_raw = sales, 
           Units_raw = units, 
           DosageUnits_raw = dosageunits, 
           `给药途径` = route) %>% 
    arrange(Channel, Date, Province, City, MKT, Pack_ID)
  
  
  return(servier.result)
}


