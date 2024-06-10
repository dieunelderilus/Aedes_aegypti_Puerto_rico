# this script was implemeted by Derilus Dieunel
#volcano plot----
library(ggplot2)
library(ggpubr)

# Change  directory
# volcano plot
setwd("//home.biotech.cdc.gov/home/qlk5/Desktop/Dieunel_analysis/Aedes_aegypti/all_bam/new_DEG/volcano/new_volcano_ODP")
#Malathion**********************----
#1-stat1_IMtS_vs_RX.csv----
### with salivary proteins
md<- read.csv("stat1_IMtS_vs_RX.csv")
md$FDR[md$FDR <1e-20] <- 1e-20
# Re-plot but this time color the points with "diffexpressed
imsr<- ggplot(data=md, aes(x=logFC, y=-log10(FDR), col=Target)) + geom_point(stroke =2 , shape = 16) +
  theme(panel.background = element_rect(fill = "white", color = "grey50"))+
  scale_color_manual(breaks = c("COE", "CUT", "CYP", "GST","SGP", "Other"),
                     values=c("red", "green", "blue","black","pink","darkgrey"))+
  #scale_y_continuous(limits = c(0, 120))+
  geom_vline(xintercept=c(-1, 1), col="black", lty=3) + geom_hline(yintercept = -log10(0.01), lty=3, color="black")+
  theme(legend.title=element_blank())+
  #scale_y_continuous(limits = c(0, 90))+
  ggtitle("I-MAL vs Rock")+
  labs(x = expression ("log"[2](FC)), y=expression("-log"[10](FDR)))
imsr

#4-stat1_MMtS_vs_RX.csv----
### with salivary proteins
md<- read.csv("stat1_MMtS_vs_RX.csv")
md$FDR[md$FDR <1e-20] <- 1e-20
# Re-plot but this time color the points with "diffexpressed
mmsr<- ggplot(data=md, aes(x=logFC, y=-log10(FDR), col=Target)) + geom_point(stroke =2 , shape = 16) +
  theme(panel.background = element_rect(fill = "white", color = "grey50"))+
  scale_color_manual(breaks = c("COE", "CUT", "CYP", "GST","SGP", "Other"),
                     values=c("red", "green", "blue","black","pink","darkgrey"))+
  #scale_y_continuous(limits = c(0, 120))+
  geom_vline(xintercept=c(-1, 1), col="black", lty=3) + geom_hline(yintercept = -log10(0.01), lty=3, color="black")+
  theme(legend.title=element_blank())+
  #scale_y_continuous(limits = c(0, 90))+
  ggtitle("M-MAL vs Rock")+
  labs(x = expression ("log"[2](FC)), y=expression("-log"[10](FDR)))
mmsr



#1- stat1_ISA_vs_RX.csv----
md<- read.csv("stat1_ISA_vs_RX.csv")
# Re-plot but this time color the points with "diffexpressed
iasr<- ggplot(data=md, aes(x=logFC, y=-log10(FDR), col=Target)) + geom_point(stroke =2 , shape = 16) +
  theme(panel.background = element_rect(fill = "white", color = "grey50"))+
  scale_color_manual(breaks = c("COE", "CUT", "CYP", "GST","SGP", "Other"),
                     values=c("red", "green", "blue","black","pink","darkgrey"))+
  #scale_y_continuous(limits = c(0, 120))+
  geom_vline(xintercept=c(-1, 1), col="black", lty=3) + geom_hline(yintercept = -log10(0.01), lty=3, color="black")+
  theme(legend.title=element_blank())+
  #scale_y_continuous(limits = c(0, 90))+
  ggtitle("ISA vs RX")+
  labs(x="log2FC")
iasr

# ggarrange 1-----
annotate_figure(m, top=text_grob("Malathion Experiment", face="bold", color="red"))


### lambda I-Sabella**********----
#1- stat1_ILS_vs_RX.csv----
md<- read.csv("stat2_ILS_vs_RX.csv")
# Re-plot but this time color the points with "diffexpressed
ilsr<- ggplot(data=md, aes(x=logFC, y=-log10(FDR), col=Target1)) + geom_point(stroke =2 , shape = 16) +
  theme(panel.background = element_rect(fill = "white", color = "grey50"))+
  scale_color_manual(breaks = c("COE", "CUT", "CYP", "GST","SGP", "OBP", "OR", "Other"),
                     values=c("red", "green", "blue","black","pink","darkviolet","deeppink4", "darkgrey"))+
  #scale_y_continuous(limits = c(0, 120))+
  geom_vline(xintercept=c(-1, 1), col="black", lty=3) + geom_hline(yintercept = -log10(0.01), lty=3, color="black")+
  theme(legend.title=element_blank())+
  #scale_y_continuous(limits = c(0, 90))+
  ggtitle("ILS vs RX")+
  labs(x="log2FC")
ilsr



# ggarrange-----
ggarrange(ilsr,ilsiu,ilkiu,ilsilk,ilkr, common.legend = T)

#lamda manati-----
#1- stat1_MLS_vs_RX.csv----
md<- read.csv("stat1_MLS_vs_RX.csv")
# Re-plot but this time color the points with "diffexpressed
mlsr<- ggplot(data=md, aes(x=logFC, y=-log10(FDR), col=Target)) + geom_point(stroke =2 , shape = 16) +
  theme(panel.background = element_rect(fill = "white", color = "grey50"))+
  scale_color_manual(breaks = c("COE", "CUT", "CYP", "GST","SGP", "Other"),
                     values=c("red", "green", "blue","black","pink","darkgrey"))+
  #scale_y_continuous(limits = c(0, 120))+
  geom_vline(xintercept=c(-1, 1), col="black", lty=3) + geom_hline(yintercept = -log10(0.05), lty=3, color="black")+
  theme(legend.title=element_blank())+
  #scale_y_continuous(limits = c(0, 90))+
  ggtitle("MLS vs RX")+
  labs(x="log2FC")
mlsr


# ggarange----
ggarrange(mlsr,mlsmu,mlsmlk,mlkr,mlkmu, common.legend = T)
l<- ggarrange(mlsr,mlsmu,mlsmlk,mlkr,mlkmu,ilsr,ilsiu,ilkiu,ilsilk,ilkr, common.legend = T)
annotate_figure(l, top=text_grob("Lambdacyhalothrin Experiment", face="bold", color="red"))

# Alfa-cyper---------
#1- stat1_ISA_vs_RX.csv----
md<- read.csv("stat1_ISA_vs_RX.csv")
# Re-plot but this time color the points with "diffexpressed
iasr<- ggplot(data=md, aes(x=logFC, y=-log10(FDR), col=Target)) + geom_point(stroke =2 , shape = 16) +
  theme(panel.background = element_rect(fill = "white", color = "grey50"))+
  scale_color_manual(breaks = c("COE", "CUT", "CYP", "GST","SGP", "Other"),
                     values=c("red", "green", "blue","black","pink","darkgrey"))+
  #scale_y_continuous(limits = c(0, 120))+
  geom_vline(xintercept=c(-1, 1), col="black", lty=3) + geom_hline(yintercept = -log10(0.05), lty=3, color="black")+
  theme(legend.title=element_blank())+
  #scale_y_continuous(limits = c(0, 90))+
  ggtitle("ISA vs RX")+
  labs(x="log2FC")
iasr

#2- stat1_MAS_vs_RX.csv----
md<- read.csv("stat1_MAS_vs_RX.csv")
# Re-plot but this time color the points with "diffexpressed
masr<- ggplot(data=md, aes(x=logFC, y=-log10(FDR), col=Target)) + geom_point(stroke =2 , shape = 16) +
  theme(panel.background = element_rect(fill = "white", color = "grey50"))+
  scale_color_manual(breaks = c("COE", "CUT", "CYP", "GST","SGP", "Other"),
                     values=c("red", "green", "blue","black","pink","darkgrey"))+
  #scale_y_continuous(limits = c(0, 120))+
  geom_vline(xintercept=c(-1, 1), col="black", lty=3) + geom_hline(yintercept = -log10(0.05), lty=3, color="black")+
  theme(legend.title=element_blank())+
  #scale_y_continuous(limits = c(0, 90))+
  ggtitle("MAS vs RX")
  labs(x="log2FC")
masr
ggarrange(imsr,mmsr, iasr, masr, ilsr, mlsr, ncol=2, nrow=3)
a<- ggarrange(iasr,masr, common.legend = T)
annotate_figure(a, top=text_grob("Alphacypermethrin Experiment", face="bold", color="red"))






