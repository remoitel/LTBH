
ltbh_long %>% 
  filter(analy_param == "Assay THFS" ) %>% 
  # filter(str_detect(Batch,"BMBD000")) %>% 
  filter(!is.na(value)) %>% 
  
  ggplot( aes(month, value, color=temp)) +  
  geom_line(linewidth = 2, alpha=0.7)+
  geom_point(size = 3, shape=16, color="black", alpha=0.2) +
  scale_x_continuous(expand=c(0,0), limits=c(-1,25),breaks = c(0,3,6,9,12,18,24)) + 
  geom_hline(yintercept=66,linetype= 1, col = "red3", linewidth=0.4)+
  geom_hline(yintercept=77,linetype= 1, col = "orange", linewidth=0.4)+
  # geom_hline(yintercept=100,linetype= 0)+
  # scale_y_continuous(breaks = seq(95,105, by=1)) + 
  facet_grid(analy_param ~ Batch,  scales="free_y")+
  xlab("Months")+
  ylab("% w/w") +
  theme(strip.text.x = element_text(family="Merck",  size = 14, colour = "#503291"),
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
  scale_color_manual(values = c("#ffc832","#ff4d00"), name ="Storage conditions")

ggsave("ltbh_assay.svg",  last_plot(),units = "cm", width = 20, height = 10)

#-------- Assay stacked

ltbh_long %>% 
  filter( analy_param == "THFS" | analy_param == "BSA" ) %>% 
  filter(temp == "25°C") %>% 
  filter(!is.na(value)) %>% 
  
  ggplot( aes(month, value, fill=analy_param)) + 
  geom_area()+
  # geom_line(linewidth = 2, alpha=0.7)+
  geom_smooth(data=ltbh_long  %>% filter(analy_param=="Assay THFS+BSA")%>% filter(temp == "25°C"),
              method=lm ,color="transparent", fill="grey40", se=TRUE, formula = y ~ x, level=0.95, fullrange =T) +
  geom_smooth(data=ltbh_long  %>% filter(analy_param=="THFS")%>% filter(temp == "25°C"),
              method=lm ,color="transparent", fill="grey70", se=TRUE, formula = y ~ x, level=0.95, fullrange =T) +
  scale_x_continuous(expand=c(0,0), limits=c(-1,37),breaks = c(0,3,6,9,12,18,24,36)) +
  geom_point(data=ltbh_long  %>% filter(analy_param=="Assay THFS+BSA")%>% filter(temp == "25°C") ,size = 1.5, alpha=0.5) +
  geom_point(data=ltbh_long  %>% filter(analy_param=="THFS") %>% filter(temp == "25°C") ,size = 1.5, alpha=0.5) +
 
  # geom_hline(yintercept=95,linetype= 2, col = "orange3", linewidth=0.8)+
  geom_hline(yintercept=66,linetype= 2, col = "orange3", linewidth=0.8)+
  geom_hline(yintercept=77,linetype= 2, col = "orange3", linewidth=0.8)+
  # geom_hline(yintercept=100,linetype= 0)+
  # scale_y_continuous(breaks = seq(95,105, by=1)) + 
  facet_grid(temp ~ Batch,  scales="free_y")+
  xlab("Months")+
  ylab("% w/w") +
  theme(strip.text.x = element_text(family="Merck",  size = 14, colour = "#503291"),
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
  scale_fill_manual(values = c("#96d7d2","#ffdcb9"),limits=c("BSA", "THFS"), name ="Concentration") #Limits: display only these values

ggsave("ltbh_assay_stack.svg",  last_plot(),units = "cm", width = 20, height = 10)

#--------- Water

ltbh_long %>% 
  filter(analy_param == "Water content" ) %>% 
  filter(!is.na(value)) %>% 
  
  ggplot( aes(month, value, color=temp)) +  
  geom_smooth(data=ltbh_long  %>% filter(analy_param=="Water content")%>% filter(temp == "25°C"),
              method=lm ,color="transparent", fill="grey70", se=TRUE, formula = y ~ x, level=0.95, fullrange =T) +
  geom_line(linewidth = 2, alpha=0.7)+
  geom_point(size = 3, shape=16, color="black", alpha=0.2) +
  scale_x_continuous(expand=c(0,0), limits=c(-1,37),breaks = c(0,3,6,9,12,18,24,36)) + 
  geom_hline(yintercept=5,linetype= 2, col = "orange3", linewidth=0.8)+
  # geom_hline(yintercept=77,linetype= 1, col = "orange", linewidth=0.4)+
  # geom_hline(yintercept=100,linetype= 0)+
  # scale_y_continuous(breaks = seq(95,105, by=1)) + 
  facet_grid(analy_param ~ Batch,  scales="free_y")+
  xlab("Months")+
  ylab("% w/w") +
  theme(strip.text.x = element_text(family="Merck",  size = 14, colour = "#503291"),
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
  scale_color_manual(values = c("#ffc832","#ff4d00"), name ="Storage conditions")

ggsave("ltbh_water.svg",  last_plot(),units = "cm", width = 20, height = 8)


#----------- Impurities

ltbh_long %>% 
  filter(analy_param %in% c("ABGS","DHFS","FS","Individ. unkn. RC", "Sum unkn. RC" ) )%>% 
  filter(!is.na(value)) %>% 
  
  ggplot( aes(month, value, color=temp)) +  
  geom_line(linewidth = 2, alpha=0.7)+
  geom_point(size = 3, shape=16, color="black", alpha=0.2) +
  scale_x_continuous(expand=c(0,0), limits=c(-1,25),breaks = c(0,3,6,9,12,18,24)) + 
  # geom_hline(yintercept=5,linetype= 2, col = "orange3", linewidth=0.8)+)+
  # geom_hline(yintercept=77,linetype= 1, col = "orange", linewidth=0.4)+
  # geom_hline(yintercept=100,linetype= 0)+
  # scale_y_continuous(breaks = seq(0,0.6, by=0.1)) + 
  facet_grid(analy_param ~ Batch, labeller = label_wrap_gen(width = 5,multi_line = TRUE))+
  xlab("Months")+
  ylab("% w/w") +
  ylim(0,0.6)+
  theme(strip.text.x = element_text(family="Merck",  size = 14, colour = "#503291"),
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
  scale_color_manual(values = c("#ffc832","#ff4d00"), name ="Storage conditions")

ggsave("ltbh_imp.svg",  last_plot(),units = "cm", width = 20, height = 20)

#----------- Impurities 2
hline_dat = data.frame(analy_param=rep(c("THPte", "Sum of all RC"),each = 1),
                       new_limit=rep(c(0.8, NA),each=1))

ltbh_long %>% 
  filter(analy_param %in% c("THPte","Sum of all RC" ) )%>% 
  filter(!is.na(value)) %>% 
  
  ggplot( aes(month, value, color=temp)) +  
  geom_line(linewidth = 2, alpha=0.7)+
  geom_point(size = 3, shape=16, color="black", alpha=0.2) +
  scale_x_continuous(expand=c(0,0), limits=c(-1,25),breaks = c(0,3,6,9,12,18,24)) + 
  # geom_hline(yintercept=5,linetype= 2, col = "orange3", linewidth=0.8)+)+
  geom_hline(data=hline_dat, aes(yintercept=new_limit),linetype= 2, col = "orange3", linewidth=0.8)+
  facet_grid(analy_param ~ Batch, scales = "free_y")+
  xlab("Months")+
  ylab("% w/w") +
  theme(strip.text.x = element_text(family="Merck",  size = 14, colour = "#503291"),
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
  scale_color_manual(values = c("#ffc832","#ff4d00"), name ="Storage conditions")

ggsave("ltbh_imp2.svg",  last_plot(),units = "cm", width = 20, height = 12)


#--------- Diaisostereomeres

ltbh_long %>% 
  filter(analy_param == "Area% 6R-CH2-THFS" ) %>% 
  filter(!is.na(value)) %>% 
  
  ggplot( aes(month, value, color=temp)) +  
  geom_smooth(data=ltbh_long  %>% filter(analy_param=="Area% 6R-CH2-THFS")%>% filter(temp == "25°C"),
              method=lm ,color="transparent", fill="grey70", se=TRUE, formula = y ~ x, level=0.95, fullrange =T) +
  geom_line(linewidth = 2, alpha=0.7)+
  geom_point(size = 3, shape=16, color="black", alpha=0.2) +
  scale_x_continuous(expand=c(0,0), limits=c(-1,37),breaks = c(0,3,6,9,12,18,24,36)) + 
  geom_hline(yintercept=95,linetype= 2, col = "orange3", linewidth=0.8)+
  # geom_hline(yintercept=77,linetype= 1, col = "orange", linewidth=0.4)+
  # geom_hline(yintercept=100,linetype= 0)+
  # scale_y_continuous(breaks = seq(95,105, by=1)) + 
  facet_grid(analy_param ~ Batch,  scales="free_y",labeller = label_wrap_gen(width = 8,multi_line = TRUE))+
  ylim(94.8,100)+
  xlab("Months")+
  ylab("% area") +
  theme(strip.text.x = element_text(family="Merck",  size = 14, colour = "#503291"),
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
  scale_color_manual(values = c("#ffc832","#ff4d00"), name ="Storage conditions")

ggsave("ltbh_dia.svg",  last_plot(),units = "cm", width = 20, height = 8)

