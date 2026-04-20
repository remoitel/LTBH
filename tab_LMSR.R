library(flextable)
library(janitor)

lmsr_tab <- lmsr %>%
  select("Batch", "temp", "month", "Inspection Lot", "5-MeTHFA", "ABGA", "HOMeTHFA", "Mefox", 
"Mefdiamid", "THFA", "Mefguan", "DHFA", "FA", "CH2THFA", "rRt 1.06", 
"MeTHPA", "rRt 1.16", "DiMeTHFA", "sum unknown", "Individual ORC", 
"Sum ORC", "Sum ORC + RC", "(6S)-5-MeTHFA", "(6R)-5-MeTHFA") %>% 
  # filter(temp == "25°C") %>% 
  arrange(Batch, temp, month) %>%
  group_by(Batch) 

lmsr_tab <- lmsr_tab |> mutate_at(c(5:24),  as.numeric) |> 
  mutate(across(5:20, ~ round_half_up(., 4))) |> #grouped columns are not counted using mutate across (!)
  mutate(across(c(4,21,22,23), ~ round_half_up(., 2)))
  
# lmsr_tab$`Water content` <- round_half_up(as.numeric(lmsr_tab$`Water content`), 2)
# lmsr_tab$`Assay Imp. BSA (260 nm)`  <- round_half_up(as.numeric(lmsr_tab$`Assay Imp. BSA (260 nm)`), 2)
# lmsr_tab$`Assay THFS`  <- round_half_up(as.numeric(lmsr_tab$`Assay THFS`),2)
# lmsr_tab$`Assay THFS+BSA` <- round_half_up(as.numeric(lmsr_tab$`Assay THFS+BSA`), 2)
# lmsr_tab$`Area% 6R-CH2-THFS` <- round_half_up(as.numeric(lmsr_tab$`Area% 6R-CH2-THFS`), 2)

lmsr_tab <- as_grouped_data(x = lmsr_tab, groups = c("Batch"))

tab1 <- flextable(lmsr_tab)
tab1 <- set_header_labels(tab1, temp = "Storage Condition", month = "Duration [months]")
tab1 <- theme_vanilla(tab1)
tab1

save_as_docx(tab1, path = "lmsr_tab.docx")


# das selbe für 40°C
lmsr_tab <- ltbh %>%
  select("Batch", "temp", "month", "Appearance color", "Appearance texture", "Assay as is (Q-NMR)","Residue on drying",
         "Purity Chlorcyanon, MSC2487290A", "Area% MSC2492085A", "Area% MSC2519932A",  "Area% MSC1010052A", "Area% Cyanon, MSC2007362A",  
         "Area% MSC2588530A", "Area% MSC2586074A", "Area% MSC2586075A","Area% any unspecified",
         "Area% any unspecified with Cyanon",  "Area% Imp. sum total"  ) %>% 
  filter(temp == "40°C") %>% 
  arrange(Batch, month) %>%
  group_by(Batch) 

lmsr_tab$`Assay as is (Q-NMR)` <- round_half_up(as.numeric(lmsr_tab$`Assay as is (Q-NMR)`), 2)
lmsr_tab$`Purity Chlorcyanon, MSC2487290A` <- round_half_up(as.numeric(lmsr_tab$`Purity Chlorcyanon, MSC2487290A`), 2)
lmsr_tab$`Residue on drying` <- round_half_up(as.numeric(lmsr_tab$`Residue on drying`), 2)
lmsr_tab$`Area% MSC2492085A` <- "< LOQ"
lmsr_tab$`Area% MSC2519932A` <- round_half_up(as.numeric(lmsr_tab$`Area% MSC2519932A`), 3)
lmsr_tab$`Area% MSC1010052A` <- round_half_up(as.numeric(lmsr_tab$`Area% MSC1010052A`), 3)
lmsr_tab$`Area% Cyanon, MSC2007362A` <- round_half_up(as.numeric(lmsr_tab$`Area% Cyanon, MSC2007362A`), 3)
lmsr_tab$`Area% MSC2588530A` <- round_half_up(as.numeric(lmsr_tab$`Area% MSC2588530A`), 3)
lmsr_tab$`Area% MSC2586074A` <- round_half_up(as.numeric(lmsr_tab$`Area% MSC2586074A`), 3)
lmsr_tab$`Area% MSC2586075A` <- round_half_up(as.numeric(lmsr_tab$`Area% MSC2586075A`), 3)
lmsr_tab$`Area% Imp. sum total` <- round_half_up(as.numeric(lmsr_tab$`Area% Imp. sum total`), 3)

lmsr_tab <- as_grouped_data(x = lmsr_tab, groups = c("Batch"))

tab1 <- flextable(lmsr_tab)
tab1 <- set_header_labels(tab1, temp = "Storage Condition", month = "Duration [months]")
tab1 <- theme_vanilla(tab1)
tab1

save_as_docx(tab1, path = "lmsr_tab_40.docx")
