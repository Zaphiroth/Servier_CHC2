# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  CHC Functionalize
# Purpose:      Format Servier
# programmer:   Zhe Liu
# Date:         2020-07-16
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #


FormatServier <- function(proj.price, market.def, 
                          city.en, capital.47, prod.bid, 
                          atc3.cn, molecule.cn, 
                          pack.profile, prod.profile) {
  
  ##---- Summary ----
  servier.result <- proj.price %>% 
    group_by(year, quarter, province, city, packid) %>% 
    summarise(sales = sum(sales, na.rm = TRUE), 
              units = sum(units, na.rm = TRUE)) %>% 
    ungroup() %>% 
    left_join(market.def, by = 'packid') %>% 
    filter(!is.na(market)) %>% 
    group_by(province, city, year, quarter, market, atc3, atc4, molecule, corp, 
             type, product, pack, packid) %>% 
    summarise(units = sum(units, na.rm = TRUE),
              sales = sum(sales, na.rm = TRUE)) %>% 
    ungroup() %>% 
    left_join(city.en, by = c('city' = 'City')) %>% 
    left_join(capital.47, by = 'city') %>% 
    mutate(prodid = stri_sub(packid, 1, 5)) %>% 
    left_join(prod.bid, by = 'prodid') %>% 
    left_join(atc3.cn, by = 'atc3') %>% 
    left_join(molecule.cn, by = 'molecule') %>% 
    mutate(name1 = if_else(Molecule_CN %in% c("赖诺普利", "卡托普利"), 
                            "4+7分子", 
                            name1), 
           name1 = if_else(name1 == '非4+7分子', NA_character_, name1)) %>% 
    mutate(name2 = if_else(name2 == "非中标产品", NA_character_, name2), 
           name3 = trimws(name3), 
           name3 = ifelse(is.na(name3) & type != "L", "原研", 
                          ifelse(is.na(name3) & type == "L", "仿制", 
                                 name3))) %>% 
    rename(`是否进入带量采购` = name1,
           `是否是中标品种` = name2,
           `是否是原研` = name3,
           `是否是MNC` = type) %>% 
    mutate(first_num_position = stri_locate_first(pack, regex = "\\d")[,1],
           last_space_position = stri_locate_last(pack, regex = "\\s")[,1],
           Package = str_squish(substr(pack, 1, first_num_position - 1)),
           Dosage = str_squish(substr(pack, first_num_position, 
                                      last_space_position - 1)),
           Quantity = as.integer(str_squish(substr(pack, last_space_position, 
                                                   nchar(pack))))) %>% 
    left_join(pack.profile, by = 'packid') %>% 
    left_join(prod.profile, by = 'prodid') %>% 
    mutate(ims_product_cn = if_else(is.na(ims_product_cn), 
                                    ims_product_cn1, 
                                    ims_product_cn)) %>% 
    mutate(
      `TherapeuticClsII` = case_when(
        atc3 == "A10H" ~ "SULPHONYLUREA",
        atc3 == "A10J" ~      "BIGUANIDE",
        atc3 == "A10K" ~     "GLITAZONE",
        atc3 == "A10L" ~     "AGIs",
        atc3 == "A10M" ~    "METAGLINIDE",
        atc3 == "A10N" ~    "DPP-IV",
        atc3 == "A10P" ~     "SGLT2",
        atc3 == "A10S" ~     "GLP-1",
        atc3 == "A10X" ~     "OTHERS",
        atc3 == "C02A" ~     "ANTI-HTN",
        atc3 == "C02B" ~     "ANTI-HTN",
        atc3 == "C02C" ~     "ANTI-HTN",
        atc3 == "C03A" ~     "DIURETICS",
        atc3 == "C07A" ~     "BB",
        atc3 == "C08A" ~     "CCB",
        atc3 == "C08B" ~     "CCB",
        atc3 == "C09A" ~     "ACEi PLAIN",
        atc3 == "C09C" ~     "ARB PLAIN",
        atc3 == "C07A" ~     "BB",
        atc3 == "C08A" ~     "CCB",
        atc3 == "C01E" ~     "NITRITES",
        molecule == "TRIMETAZIDINE" ~   "TMZ",
        atc3 == "C01D" & molecule != "TRIMETAZIDINE" ~  "OTHERS",
        atc4 %in% c("C09B3", "C09D3") ~ "A+C FDC",
        atc4 %in% c("C09B1", "C09D1") ~ "A+D FDC",
        !is.na(atc4) ~ "OTHERS",
        TRUE ~ NA_character_
      ), 
      TherapeuticClsII = case_when(
        market == "HTN" & atc3 == "C09A" ~ "RAASi Plain",
        market == "HTN" & atc3 == "C09C" ~ "RAASi Plain",
        market == "HTN" & atc3 == "C09B" ~ "RAASi FDC",
        market == "HTN" & atc3 == "C09D" ~ "RAASi FDC",
        market == "HTN" & atc3 == "C02A" ~ "ANTI-HTN",
        market == "HTN" & atc3 == "C02B" ~ "ANTI-HTN",
        market == "HTN" & atc3 == "C02C" ~ "ANTI-HTN",
        market == "HTN" & atc3 == "C03A" ~ "DIURETICS",
        market == "HTN" & atc3 == "C07A" ~ "BB",
        market == "HTN" & atc3 == "C08A" ~ "CCB",
        market == "HTN" & atc3 == "C08B" ~ "CCB",
        market == "OAD" & atc3 == "A10H" ~ "SULPHONYLUREA",
        market == "OAD" & atc3 == "A10J" ~ "BIGUANIDE",
        market == "OAD" & atc3 == "A10K" ~ "GLITAZONE",
        market == "OAD" & atc3 == "A10L" ~ "AG Is",
        market == "OAD" & atc3 == "A10M" ~ "METAGLINIDE",
        market == "OAD" & atc3 == "A10N" ~ "DPP-IV",
        market == "OAD" & atc3 == "A10P" ~ "SGLT2",
        market == "OAD" & atc3 == "A10S" ~ "GLP-1",
        market == "OAD" & atc3 == "A10X" ~ "OTHERS",
        market == "IHD" & atc3 == "C07A" ~ "BB",
        market == "IHD" & atc3 == "C08A" ~ "CCB",
        market == "IHD" & atc3 == "C01E" ~ "NITRITES",
        market == "IHD" & atc3 == "C01D" & molecule == "TRIMETAZIDINE" ~ "TMZ",
        market == "IHD" & atc3 == "C01D" & molecule != "TRIMETAZIDINE"~ "OTHERS",
        TRUE ~ NA_character_
      ), 
      channel = 'CHC', 
      dosageunits = Quantity * units
    ) %>% 
    select(Pack_ID = packid, 
           Channel = channel, 
           Province = province, 
           City = city, 
           Date = quarter, 
           ATC3 = atc3, 
           MKT = market, 
           Molecule_Desc = molecule, 
           Prod_Desc = product, 
           Pck_Desc = pack, 
           Corp_Desc = corp, 
           Sales = sales, 
           Units = units, 
           DosageUnits = dosageunits, 
           `CITY-EN`, 
           TherapeuticClsII, 
           Prod_CN_Name = ims_product_cn, 
           Package, 
           Dosage, 
           Quantity, 
           `是否是4+7城市`, 
           `是否进入带量采购`, 
           `是否是原研`, 
           `是否是中标品种`, 
           `是否是MNC`, 
           `ATC3中文分类`) %>% 
    arrange(Channel, Date, Province, City, MKT, Pack_ID)
  
  
  return(servier.result)
}


