library(readxl)
library(tidyverse)


pal_merck_sens <- c("#e1c3cd", "#96d7d2","#b4dc96","#ffdcb9")
names(pal_merck_sens) <- c("Sensitve Pink", "Sensitive Blue","Sensitive Green","Sensitive Yellow")

pal_merck_rich <- c("#503291","#0f69af","#149b5f","#e61e50")
names(pal_merck_rich) <- c("Rich Purple","Rich Blue","Rich Green","Rich Red")

pal_merck_vib <- c("#eb3c96","#2dbecd","#a5cd50","#ffc832")
names(pal_merck_vib) <- c("Vibrant Magenta","Vibrant Cyan","Vibrant Green","Vibrant Yellow")

pal_merck_vib2 <- c("#a5cd50","#2dbecd","#eb3c96","#ffc832","#503291", "#96d7d2") #andere Reihenfolge

pal_merck <- c("#e1c3cd", "#96d7d2","#b4dc96","#ffdcb9","#503291","#0f69af","#149b5f","#e61e50","#eb3c96","#2dbecd","#a5cd50","#ffc832")
names(pal_merck) <- c("Sensitve Pink", "Sensitive Blue","Sensitive Green","Sensitive Yellow","Rich Purple","Rich Blue","Rich Green","Rich Red","Vibrant Magenta","Vibrant Cyan","Vibrant Green","Vibrant Yellow")


library(showtext)
font_add("Merck", regular="C:/Windows/Fonts/Merck-Regular.ttf")
showtext_auto()


#IL <- readClipboard()
