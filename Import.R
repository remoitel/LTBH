ltbh_raw <- read_excel("C:/Users/M291711/OneDrive - MerckGroup/SAP/SAP GUI/LTBH_L2_Layout.xlsx") #Layout STAB LTBH plus Lev

#Überflüssige Spalten
wegdamit <- c(  "Material description",
               "Last goods receipt")  # , "Deviations from analytical method", "UD code"


ltbh <- ltbh_raw %>%
  #mutate_at(vars(Manuf), ~as.Date(.)) %>%  #zu Datum
  filter(Lev==1) %>% 
  mutate(month = str_extract(`IL short text`, "\\d+(?= ?M(ona)?t[ e.])"))%>%
  mutate(temp = str_c(str_extract(`IL short text`, "-?\\d+(?= ?°?C[ /,])"),"°C"))%>%  #str_c wie paste, aber besser für NA
  # filter(!is.na("UD code"))%>%  # Leere UD -> entfernen
  select(-all_of(wegdamit)) %>%
  select(-starts_with('Specification'), -starts_with( 'A/R'),-starts_with('Unit'),-starts_with('QC '),
         -contains("No QC")) %>% 
  #select(-"Inspection Lot", -"IL short text",-"Material description"  ) %>%  #optional
  mutate_at(vars(temp), factor) %>%  #Faktorumwandlung
  relocate(any_of(c("temp","month")), .after = Batch) %>%
  
  filter(str_detect(`Inspection Lot`, "^40|^16|^89|^10") ) %>%    #Nur Freigabe und Stabi IL (mit Temp!)
  # filter((str_detect(`Inspection Lot`, "^16|^89") & !is.na(temp)) ) %>%
  filter(!str_detect(Batch, "^VALIDUM")) %>% 
  filter(!str_detect(`IL short text`, "ICP|Hyp|USD") | is.na(`IL short text`) ) %>% # 
  filter(!(str_detect(`Inspection Lot`, "890000133358"))) # 3Mte 40°C -> wiso?

# mutate_at(c("Area% Cyanon, MSC2007362A","Area% any unspecified"),  as.numeric)  %>% 
# mutate( `Area% any unspecified with Cyanon` = `Area% Cyanon, MSC2007362A` +`Area% any unspecified`) %>% 
# rename("Residue on drying" = "Residue on drying (attach report)")

# mutate_at(vars(weight), as.numeric)
# filter(str_detect(Inspection Lot, "^89|^40")) %>%  #IL mit 89 aber ohne Temp. löschen

#filter(!(str_detect(`Inspection Lot`, "^89")& !is.na(temp))) %>%  #IL mit 89 aber ohne Temp. löschen
#filter(!is.na(temp)) %>%  #  na.omit für komplette Datensätze   filter(!is.na(temp)| !is.na(month)) %>%
# select_if(~!all(is.na(.))) # Leere Spalten, die nur NA enthalten löschen

ltbh$month <- as.integer(ltbh$month)
ltbh$temp <- factor(ltbh$temp, levels = c("25°C", "40°C")) #Reihenfolge Faktor

ltbh$month <-replace_na(ltbh$month, 0) #Fehlende Monate gleich Initialwert

lev <-levels(droplevels(ltbh$temp)) # droplevels entfernt unbenutzte Level, die sonst irgendwie drin bleiben....

ltbh0<-ltbh %>% filter(month==0)  %>%  # Spalten mit 0 Monaten mit Temperaturen ergänzen
  select(-temp) %>%
  expand_grid(temp=lev)

ltbh <- ltbh[!ltbh$month==0,]  # Monate mit Null (ohne Temp) aus alter Tabelle entfernen
ltbh_fertig <- rbind(ltbh,ltbh0)  # Kombinieren
ltbh_fertig$month <-as.integer(ltbh_fertig$month)
ltbh <-ltbh_fertig

ltbh_long <- ltbh %>% 
  select(c( "Batch", "Inspection Lot","temp", "month", 
             "Water content", 
            "Assay THFS", "Assay Imp. BSA (260 nm)", "Assay THFS+BSA", "Assay Imp. ABGS", 
            "Assay Imp. THPte", "Assay Imp. DHFS", "Assay Imp. FS", "Individual unknown RC", 
            "Sum unknown RC", "Sum of all RC", "Area% 6R-CH2-THFS")) %>% 
  rename("Individ. unkn. RC" = "Individual unknown RC" ) %>% 
  rename("Sum unkn. RC" = "Sum unknown RC" ) %>% 
  rename("FS" = "Assay Imp. FS" ) %>% 
  rename("DHFS" = "Assay Imp. DHFS" ) %>% 
  rename("THPte" = "Assay Imp. THPte" ) %>% 
  rename("ABGS" = "Assay Imp. ABGS" ) %>% 
  rename("BSA" = "Assay Imp. BSA (260 nm)" ) %>% 
  rename("THFS" = "Assay THFS" ) %>% 
  mutate_at(5:16, as.numeric) %>% 
  pivot_longer(!c(Batch,`Inspection Lot`, month,temp), names_to = "analy_param", values_to = "value") %>% 
  mutate(value = as.numeric(value))

ltbh_long2 <- ltbh %>% 
  select(c(  "Batch", "Inspection Lot","temp", "month", "Appearance color", "Appearance texture", )) %>% 
  pivot_longer(!c(Batch,`Inspection Lot`, month,temp), names_to = "analy_param", values_to = "value") 





