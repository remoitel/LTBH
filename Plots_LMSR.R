
lmsr_long %>% 
  filter(analy_param == "HOMeTHFA" ) %>% 
  # filter(str_detect(Batch,"BMBD000")) %>% 
  filter(!is.na(value)) %>% 
  
  ggplot( aes(month, value, color=temp)) +  
  # geom_line(linewidth = 1.5, alpha=0.4)+
  geom_line( linewidth = 1.5, alpha=0.4)+
  geom_point(size = 2, shape=16, color="black", alpha=0.4) +
  scale_x_continuous(expand=c(0,0), limits=c(-1,25),breaks = c(0,3,6,9,12,18,24)) + 
  geom_hline(yintercept=0.64,linetype= 1, col = "red3", linewidth=0.4)+
  # geom_hline(yintercept=77,linetype= 1, col = "orange", linewidth=0.4)+
  # geom_hline(yintercept=100,linetype= 0)+
  # scale_y_continuous(breaks = seq(95,105, by=1)) + 
  facet_grid(analy_param ~ Batch,  scales="free_y")+
  xlab("Months")+
  ylab("% w/w") +
  theme(strip.text.x = element_text(family="Merck",  size = 8, colour = "#503291"),
        strip.text.y = element_text(family="Merck",  size = 12, colour = "#503291"),
        axis.text.x = element_text(size = 10, angle=0),
        axis.text.y = element_text(size = 12, angle=0),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        legend.position = "bottom", legend.background = element_rect(fill="transparent"), 
        legend.direction = "horizontal", # "bottom"
        legend.text=element_text(size=10),
        legend.title =element_text(size=10),
        strip.background =element_rect(fill="#b4dc96"),
        panel.background  = element_rect(fill = "#f2f2f2"),
        panel.spacing.y =  unit(2, "mm", data = NULL),
        panel.border = element_rect(colour = "gray", fill=NA, size=1))+
  scale_color_manual(values = c("#0f69af", "#149b5f", "#ffc832", "#e41a1c"), name ="Storage conditions")

ggsave("lmsr_HOMe.svg",  last_plot(),units = "cm", width = 20, height = 10)

### ---  DiMeTHFA ---

lmsr_long %>% 
  filter(analy_param == "DiMeTHFA" ) %>% 
  # filter(str_detect(Batch,"BMBD000")) %>% 
  filter(!is.na(value)) |> 
  
  ggplot( aes(month, value, color=temp)) +  
  geom_line(linewidth = 1.5, alpha=0.4)+
  geom_point(size = 2, shape=16, color="black", alpha=0.4) +
  scale_x_continuous(expand=c(0,0), limits=c(-1,25),breaks = c(0,3,6,9,12,18,24)) + 
  geom_hline(yintercept=0.345,linetype= 1, col = "red3", linewidth=0.4)+
  # geom_hline(yintercept=77,linetype= 1, col = "orange", linewidth=0.4)+
  # geom_hline(yintercept=100,linetype= 0)+
  # scale_y_continuous(breaks = seq(95,105, by=1)) + 
  facet_grid(analy_param ~ Batch,  scales="free_y")+
  xlab("Months")+
  ylab("% w/w") +
  theme(strip.text.x = element_text(family="Merck",  size = 8, colour = "#503291"),
        strip.text.y = element_text(family="Merck",  size = 12, colour = "#503291"),
        axis.text.x = element_text(size = 10, angle=0),
        axis.text.y = element_text(size = 12, angle=0),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        legend.position = "bottom", legend.background = element_rect(fill="transparent"), 
        legend.direction = "horizontal", # "bottom"
        legend.text=element_text(size=10),
        legend.title =element_text(size=10),
        strip.background =element_rect(fill="#b4dc96"),
        panel.background  = element_rect(fill = "#f2f2f2"),
        panel.spacing.y =  unit(2, "mm", data = NULL),
        panel.border = element_rect(colour = "gray", fill=NA, size=1))+
  scale_color_manual(values = c("#0f69af", "#149b5f", "#ffc832", "#e41a1c"), name ="Storage conditions")

ggsave("lmsr_DiMeTHFA.svg",  last_plot(),units = "cm", width = 20, height = 10)


### ---  Sum ---

lmsr_long %>% 
  filter(analy_param == "Sum ORC + RC" ) %>% 
  # filter(str_detect(Batch,"BMBD000")) %>% 
  filter(!is.na(value)) |> 
  
  ggplot( aes(month, value, color=temp)) +  
  geom_line(linewidth = 1.5, alpha=0.4)+
  geom_point(size = 2, shape=16, color="black", alpha=0.4) +
  scale_x_continuous(expand=c(0,0), limits=c(-1,25),breaks = c(0,3,6,9,12,18,24)) + 
  geom_hline(yintercept=3.049,linetype= 1, col = "red3", linewidth=0.4)+
  facet_grid(analy_param ~ Batch,  scales="free_y")+
  xlab("Months")+
  ylab("% w/w") +
  theme(strip.text.x = element_text(family="Merck",  size = 8, colour = "#503291"),
        strip.text.y = element_text(family="Merck",  size = 12, colour = "#503291"),
        axis.text.x = element_text(size = 10, angle=0),
        axis.text.y = element_text(size = 12, angle=0),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        legend.position = "bottom", legend.background = element_rect(fill="transparent"), 
        legend.direction = "horizontal", # "bottom"
        legend.text=element_text(size=10),
        legend.title =element_text(size=10),
        strip.background =element_rect(fill="#b4dc96"),
        panel.background  = element_rect(fill = "#f2f2f2"),
        panel.spacing.y =  unit(2, "mm", data = NULL),
        panel.border = element_rect(colour = "gray", fill=NA, size=1))+
  scale_color_manual(values = c("#0f69af", "#149b5f", "#ffc832", "#e41a1c"), name ="Storage conditions")

ggsave("lmsr_sum.svg",  last_plot(),units = "cm", width = 20, height = 10)



### ---  Assay ---
 
lmsr_long %>% 
  filter(analy_param == "5-MeTHFA" ) %>% 
  # filter(str_detect(Batch,"BMBD000")) %>% 
  filter(!is.na(value)) |> 
  
  ggplot( aes(month, value, color=temp)) +  
  geom_line(linewidth = 1.5, alpha=0.4)+
  geom_point(size = 2, shape=16, color="black", alpha=0.4) +
  scale_x_continuous(expand=c(0,0), limits=c(-1,25),breaks = c(0,3,6,9,12,18,24)) + 
  geom_hline(yintercept=84.5,linetype= 1, col = "red3", linewidth=0.4)+
  # geom_hline(yintercept=77,linetype= 1, col = "orange", linewidth=0.4)+
  # geom_hline(yintercept=100,linetype= 0)+
  # scale_y_continuous(breaks = seq(95,105, by=1)) + 
  facet_grid(analy_param ~ Batch,  scales="free_y")+
  xlab("Months")+
  ylab("% w/w") +
  theme(strip.text.x = element_text(family="Merck",  size = 8, colour = "#503291"),
        strip.text.y = element_text(family="Merck",  size = 12, colour = "#503291"),
        axis.text.x = element_text(size = 10, angle=0),
        axis.text.y = element_text(size = 12, angle=0),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        legend.position = "bottom", legend.background = element_rect(fill="transparent"), 
        legend.direction = "horizontal", # "bottom"
        legend.text=element_text(size=10),
        legend.title =element_text(size=10),
        strip.background =element_rect(fill="#b4dc96"),
        panel.background  = element_rect(fill = "#f2f2f2"),
        panel.spacing.y =  unit(2, "mm", data = NULL),
        panel.border = element_rect(colour = "gray", fill=NA, size=1))+
  scale_color_manual(values = c("#0f69af", "#149b5f", "#ffc832", "#e41a1c"), name ="Storage conditions")

ggsave("lmsr_Assay.svg",  last_plot(),units = "cm", width = 20, height = 8) 


lmsr_long %>% 
  filter(analy_param == "5-MeTHFA" ) %>% 
  filter(temp == "25°C") %>% 
  filter(Batch != "LMSR0716ZX") %>% 
  filter(!is.na(value)) |> 
  
  ggplot( aes(month, value, color=temp)) +  
  geom_line(linewidth = 1.5, alpha=0.4)+
  geom_point(size = 2, shape=16, color="black", alpha=0.4) +
  scale_x_continuous(expand=c(0,0), limits=c(-1,37),breaks = c(0,3,6,9,12,18,24,36)) + 
  geom_hline(yintercept=84.5,linetype= 1, col = "red3", linewidth=0.4)+
  # geom_hline(yintercept=77,linetype= 1, col = "orange", linewidth=0.4)+
  # geom_hline(yintercept=100,linetype= 0)+
  # scale_y_continuous(breaks = seq(95,105, by=1)) +
  
  geom_smooth(method=lm , color="skyblue4", fill="skyblue2", se=TRUE, formula = y ~ x, level=0.90, fullrange =T)+
  facet_grid(Batch ~ analy_param)+
  # facet_grid(analy_param ~ Batch)+
  xlab("Months")+
  ylab("% w/w") +
  theme(strip.text.x = element_text(family="Merck",  size = 14, colour = "#503291"),
        strip.text.y = element_text(family="Merck",  size = 8, colour = "#503291"),
        axis.text.x = element_text(size = 10, angle=0),
        axis.text.y = element_text(size = 12, angle=0),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        legend.position = "bottom", legend.background = element_rect(fill="transparent"), 
        legend.direction = "horizontal", # "bottom"
        legend.text=element_text(size=10),
        legend.title =element_text(size=10),
        strip.background =element_rect(fill="#b4dc96"),
        panel.background  = element_rect(fill = "#f2f2f2"),
        panel.spacing.y =  unit(2, "mm", data = NULL),
        panel.border = element_rect(colour = "gray", fill=NA, size=1))+
  scale_color_manual(values = c( "#ffc832"), name ="Storage conditions")

ggsave("lmsr_Assay25C_extrapol.svg",  last_plot(),units = "cm", width = 9, height = 20)
  
lmsr_long %>% 
  filter(analy_param == "5-MeTHFA" ) %>% 
  filter(temp == "-20°C") %>% 
  filter(Batch != "LMSR0716ZX") %>% 
  filter(!is.na(value)) |> 
  
  ggplot( aes(month, value, color=temp)) +  
  geom_line(linewidth = 1.5, alpha=0.4)+
  geom_point(size = 2, shape=16, color="black", alpha=0.4) +
  scale_x_continuous(expand=c(0,0), limits=c(-1,37),breaks = c(0,3,6,9,12,18,24,36)) + 
  geom_hline(yintercept=84.5,linetype= 1, col = "red3", linewidth=0.4)+
  # geom_hline(yintercept=77,linetype= 1, col = "orange", linewidth=0.4)+
  # geom_hline(yintercept=100,linetype= 0)+
  # scale_y_continuous(breaks = seq(95,105, by=1)) +
  
  geom_smooth(method=lm , color="skyblue4", fill="skyblue2", se=TRUE, formula = y ~ x, level=0.90, fullrange =T)+
  facet_grid(Batch ~ analy_param)+
  # facet_grid(analy_param ~ Batch)+
  xlab("Months")+
  ylab("% w/w") +
  theme(strip.text.x = element_text(family="Merck",  size = 14, colour = "#503291"),
        strip.text.y = element_text(family="Merck",  size = 8, colour = "#503291"),
        axis.text.x = element_text(size = 10, angle=0),
        axis.text.y = element_text(size = 12, angle=0),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        legend.position = "bottom", legend.background = element_rect(fill="transparent"), 
        legend.direction = "horizontal", # "bottom"
        legend.text=element_text(size=10),
        legend.title =element_text(size=10),
        strip.background =element_rect(fill="#b4dc96"),
        panel.background  = element_rect(fill = "#f2f2f2"),
        panel.spacing.y =  unit(2, "mm", data = NULL),
        panel.border = element_rect(colour = "gray", fill=NA, size=1))+
  scale_color_manual(values = c( "#0f69af"), name ="Storage conditions")

ggsave("lmsr_Assay-20C_extrapol.svg",  last_plot(),units = "cm", width = 9, height = 20)

### ---- Impurities ------

lmsr_long %>% 
  filter(!analy_param %in% c("5-MeTHFA","DiMeTHFA","HOMeTHFA","sum unknown",  
                             "Sum ORC", "Sum ORC + RC", "(6S)-5-MeTHFA", "(6R)-5-MeTHFA" )) %>% 
  # filter(temp == "-20°C") %>% 
  # filter(Batch != "LMSR0716ZX") %>% 
  filter(!is.na(value)) |> 
  
  ggplot( aes(month, value, color=temp)) +  
  geom_line(linewidth = 1.0, alpha=0.4)+
  # geom_point(size = 2, shape=16, color="black", alpha=0.4) +
  scale_x_continuous(expand=c(0,0), limits=c(-1,25),breaks = c(0,3,6,9,12,18,24)) + 
  geom_hline(yintercept=1.549,linetype= 1, col = "red3", linewidth=0.4)+
  
  # geom_smooth(method=lm , color="skyblue4", fill="skyblue2", se=TRUE, formula = y ~ x, level=0.95, fullrange =T)+
  # facet_grid(Batch ~ analy_param, scales="free_y")+
  facet_grid(analy_param ~ Batch, scales="free_y")+
  xlab("Months")+
  ylab("% w/w") +
  theme(strip.text.x = element_text(family="Merck",  size = 12, colour = "#503291"),
        strip.text.y = element_text(family="Merck",  size = 7, colour = "#503291"),
        axis.text.x = element_text(size = 10, angle=0),
        axis.text.y = element_text(size = 12, angle=0),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12),
        legend.position = "bottom", legend.background = element_rect(fill="transparent"), 
        legend.direction = "horizontal", # "bottom"
        legend.text=element_text(size=10),
        legend.title =element_text(size=10),
        strip.background =element_rect(fill="#b4dc96"),
        panel.background  = element_rect(fill = "#f2f2f2"),
        panel.spacing.y =  unit(2, "mm", data = NULL),
        panel.border = element_rect(colour = "gray", fill=NA, size=1))+
  scale_color_manual(values = c("#0f69af", "#149b5f", "#ffc832", "#e41a1c"), name ="Storage conditions")

ggsave("lmsr_imp.png", dpi = 150, last_plot(),units = "cm", width = 20, height = 25)
