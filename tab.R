library(flextable)
library(janitor)

ltbh_tab <- ltbh %>%
  select("Batch", "temp", "month", "Inspection Lot",  
         "Material", "Appearance color", "Appearance texture", "Water content", 
         "Assay THFS", "Assay Imp. BSA (260 nm)", "Assay THFS+BSA", "Assay Imp. ABGS", 
         "Assay Imp. THPte", "Assay Imp. DHFS", "Assay Imp. FS", "Individual unknown RC", 
         "Sum unknown RC", "Sum of all RC", "Area% 6R-CH2-THFS") %>% 
  # filter(temp == "25°C") %>% 
  arrange(Batch, month) %>%
  group_by(Batch) 


ltbh_tab$`Water content` <- round_half_up(as.numeric(ltbh_tab$`Water content`), 2)
ltbh_tab$`Assay Imp. BSA (260 nm)`  <- round_half_up(as.numeric(ltbh_tab$`Assay Imp. BSA (260 nm)`), 2)
ltbh_tab$`Assay THFS`  <- round_half_up(as.numeric(ltbh_tab$`Assay THFS`),2)
ltbh_tab$`Assay THFS+BSA` <- round_half_up(as.numeric(ltbh_tab$`Assay THFS+BSA`), 2)
ltbh_tab$`Area% 6R-CH2-THFS` <- round_half_up(as.numeric(ltbh_tab$`Area% 6R-CH2-THFS`), 2)


ltbh_tab <- as_grouped_data(x = ltbh_tab, groups = c("Batch"))

tab1 <- flextable(ltbh_tab)
tab1 <- set_header_labels(tab1, temp = "Storage Condition", month = "Duration [months]")
tab1 <- theme_vanilla(tab1)
tab1

save_as_docx(tab1, path = "ltbh_tab.docx")


# das selbe für 40°C
ltbh_tab <- ltbh %>%
  select("Batch", "temp", "month", "Appearance color", "Appearance texture", "Assay as is (Q-NMR)","Residue on drying",
         "Purity Chlorcyanon, MSC2487290A", "Area% MSC2492085A", "Area% MSC2519932A",  "Area% MSC1010052A", "Area% Cyanon, MSC2007362A",  
         "Area% MSC2588530A", "Area% MSC2586074A", "Area% MSC2586075A","Area% any unspecified",
         "Area% any unspecified with Cyanon",  "Area% Imp. sum total"  ) %>% 
  filter(temp == "40°C") %>% 
  arrange(Batch, month) %>%
  group_by(Batch) 

ltbh_tab$`Assay as is (Q-NMR)` <- round_half_up(as.numeric(ltbh_tab$`Assay as is (Q-NMR)`), 2)
ltbh_tab$`Purity Chlorcyanon, MSC2487290A` <- round_half_up(as.numeric(ltbh_tab$`Purity Chlorcyanon, MSC2487290A`), 2)
ltbh_tab$`Residue on drying` <- round_half_up(as.numeric(ltbh_tab$`Residue on drying`), 2)
ltbh_tab$`Area% MSC2492085A` <- "< LOQ"
ltbh_tab$`Area% MSC2519932A` <- round_half_up(as.numeric(ltbh_tab$`Area% MSC2519932A`), 3)
ltbh_tab$`Area% MSC1010052A` <- round_half_up(as.numeric(ltbh_tab$`Area% MSC1010052A`), 3)
ltbh_tab$`Area% Cyanon, MSC2007362A` <- round_half_up(as.numeric(ltbh_tab$`Area% Cyanon, MSC2007362A`), 3)
ltbh_tab$`Area% MSC2588530A` <- round_half_up(as.numeric(ltbh_tab$`Area% MSC2588530A`), 3)
ltbh_tab$`Area% MSC2586074A` <- round_half_up(as.numeric(ltbh_tab$`Area% MSC2586074A`), 3)
ltbh_tab$`Area% MSC2586075A` <- round_half_up(as.numeric(ltbh_tab$`Area% MSC2586075A`), 3)
ltbh_tab$`Area% Imp. sum total` <- round_half_up(as.numeric(ltbh_tab$`Area% Imp. sum total`), 3)

ltbh_tab <- as_grouped_data(x = ltbh_tab, groups = c("Batch"))

tab1 <- flextable(ltbh_tab)
tab1 <- set_header_labels(tab1, temp = "Storage Condition", month = "Duration [months]")
tab1 <- theme_vanilla(tab1)
tab1

save_as_docx(tab1, path = "ltbh_tab_40.docx")
