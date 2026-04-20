lmsr_raw <- read_excel("LMSR.xlsx") #Layout STAB lmsr plus Lev

#Überflüssige Spalten
wegdamit <- c(  "Material description", "UD code",
               "Last goods receipt","Operation", "Operation short text", "Physical sample", 
               "Inspection unit number", "Plant", "Material", "Shelf Life Exp. Date", 
               "Order", "Vendor", "Sequence", "Vial", "Seq. Notification", "Weighing No.", 
               "Date", "Time", "Identity retention time HPLC", "Sample shipped for analysis", "Assay Sulfur (ICP-OES)", 
                "UD code")

lmsr <- lmsr_raw %>%
  #mutate_at(vars(Manuf), ~as.Date(.)) %>%  #zu Datum
  filter(Lev==1) %>% 
  mutate(month = case_when(str_detect(`Inspection Lot`,"^89") ~ str_c(str_extract(`IL short text`, "\\d+(?=[ ]{0,2}M(ona?)?t[ e.h,])")),
                           str_detect(`Inspection Lot`,"^16") ~ str_c(str_extract(`IL short text`, "^.{2}"))))%>%
  mutate(temp = str_c(str_extract(`IL short text`, "-?\\d+(?= ?°?C[ /,]?)"),"°C"))%>%
  # filter(!is.na("UD code"))%>%  # Leere UD -> entfernen
  select(-all_of(wegdamit)) %>%
  select(-starts_with('Specification'), -starts_with( 'A/R'),-starts_with('Unit'),-starts_with('QC '),
         -contains("No QC")) %>% 
  #select(-"Inspection Lot", -"IL short text",-"Material description"  ) %>%  #optional
  mutate_at(vars(temp), factor) %>%  #Faktorumwandlung
  relocate(any_of(c("temp","month")), .after = Batch) %>%
  
  filter(str_detect(`Inspection Lot`, "^40|^16|^89|^10|^80") ) %>%    #Nur Freigabe und Stabi IL (mit Temp!)
  mutate(temp = case_when(str_detect(`Inspection Lot`, "^80")  ~ "-20°C",.default = temp))%>% #add months / temp to Retest IL's
  mutate(month = case_when(str_detect(`Inspection Lot`, "^80")  ~ "0",.default = month))%>%
  # filter((str_detect(`Inspection Lot`, "^16|^89") & !is.na(temp)) ) %>%
  filter(!str_detect(Batch, "^VALIDUM")) %>% 
  filter(!str_detect(`IL short text`, "ICP|Hyp|USD|OOS") | is.na(`IL short text`) ) |>   #
  filter(!str_detect(`Inspection Lot`, "890000133358|890000137176|890000137177")) |>  # 3Mte 40°C -> wiso?
  filter(!str_detect(`Inspection Lot`, "10000849642|80000373639|10000849649")) # Restest used instead of release (see mango 23292432)


lmsr$month <- as.integer(lmsr$month)
lmsr$temp <- factor(lmsr$temp, levels = c("-20°C", "5°C","25°C","40°C")) #Reihenfolge Faktor

lmsr$month <-replace_na(lmsr$month, 0) #Fehlende Monate gleich Initialwert

lev <-levels(droplevels(lmsr$temp)) # droplevels entfernt unbenutzte Level, die sonst irgendwie drin bleiben....

lmsr0<-lmsr %>% filter(month==0)  %>%  # Spalten mit 0 Monaten mit Temperaturen ergänzen
  select(-temp) %>%
  expand_grid(temp=lev)

lmsr <- lmsr[!lmsr$month==0,]  # Monate mit Null (ohne Temp) aus alter Tabelle entfernen
lmsr_fertig <- rbind(lmsr,lmsr0)  # Kombinieren
lmsr_fertig$month <-as.integer(lmsr_fertig$month)
lmsr <-lmsr_fertig

lmsr <- rename_with(lmsr, ~ gsub("Assay Imp. ", "", .x, fixed = TRUE))
lmsr <- rename_with(lmsr, ~ gsub("Assay ", "", .x, fixed = TRUE))

lmsr_long <- lmsr %>% 
  select(!c("Lev", "(6R)-5-MeTHFA" , "(6S)-5-MeTHFA","IL short text" )) %>% 
  mutate_at(5:24, as.numeric) %>% 
  pivot_longer(!c(Batch,`Inspection Lot`, month,temp), names_to = "analy_param", values_to = "value") 

lmsr_long2 <- lmsr %>% 
  select(c(  "Batch", "Inspection Lot","temp", "month", "Appearance color", "Appearance texture", )) %>% 
  pivot_longer(!c(Batch,`Inspection Lot`, month,temp), names_to = "analy_param", values_to = "value") 





