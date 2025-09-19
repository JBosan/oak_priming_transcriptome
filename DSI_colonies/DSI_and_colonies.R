#Initial set-up########
library(ggplot2) #for plotting
library(readxl) #to load data from excel
library(olsrr) #for checking anova assumptions
library(car) #for levennes test
library(multcomp) #for anova post hoc testing
library(FSA) #for kruskal wallis post hoc testing (Dunn's test)



#Colonies lab
colonies <- read_excel(path = "data/DSI_colonies_lab.xlsx",
                       sheet = "14dpi_colonies", #load 14dpi lab colonies data
                       na = "NA")

colonies <- na.omit(colonies)

colonies$Treatment <- ordered(colonies$Treatment, #order by treatment
                             levels = c("Water", "BABA", "JA", "SA"))


colonies <- as.data.frame(colonies) #convert from tibble to df so shapiro test works



#DSI lab
DSI_lab <- read_excel(path = "data/DSI_colonies_lab.xlsx", 
                      sheet = "DSI_lab_30dpi", #load DSI 30dpi lab data
                      na = "NA")

DSI_lab <- na.omit(DSI_lab) #remove NAs

DSI_lab$Treatment <- ordered(DSI_lab$Treatment, #order by treatment
                               levels = c("Water", "BABA", "JA", "SA"))


DSI_lab <- as.data.frame(DSI_lab) #convert from tibble to df so shapiro test works

#DSI field
DSI_field <- read.csv(file = "data/DSI_field.csv") #load DSI 30dpi field data

DSI_field$Treatment <- ordered(DSI_field$Treatment, #order by treatment
                               levels = c("Water", "BABA", "JA", "SA"))


#Set up colour/shape coding for graphs
treatment_cols <- c("Water" = "#33bbee",
                    "BABA" = "#44AA99",
                    "JA" = "#882255",
                    "SA" = "#999933")

#Plot colonies lab graph#########
p1 <- ggplot(data = colonies, 
             mapping = aes(x = Treatment, y = Colonies, fill = Treatment))+
  geom_boxplot()+
  scale_fill_manual(values = treatment_cols)+
  geom_jitter(size = 10, shape = 17)+
  ylab("Colonies lab 14dpi")+
  xlab(NULL)+
  scale_y_continuous(limits = c(-2,105), breaks = seq(0,125, 25))+ #start limits at -2 to avoid cutting off points due to size
  guides(fill = "none", pch = "none")+
  theme(panel.grid.major = element_blank(), #remove major grid lines
        panel.grid.minor = element_blank(), #remove minor grid lines
        panel.background = element_rect(fill = "#f5f5f5"), #light grey background to match other graphs
        axis.line = element_line(), #add lines to axis
        axis.text.x = element_text(size = 40), # Change x-axis label size
        axis.text.y = element_text(size = 40)) #change y-axis label size

p1 #view graph

ggsave(filename = "final_graphs/colonies.pdf", p1, device = pdf, width = 33, height = 20) #save graph

#Colonies lab stats######
colonies_model <- lm(Colonies ~ Treatment, data = colonies)

#check quality of model with olsrr package
ols_plot_resid_fit(colonies_model)#residual v fitted graph, looks pretty homogeneous
ols_plot_resid_qq(colonies_model) #qq plot = deviates from normal
ols_plot_resid_hist(colonies_model) #residuals histogram = looks pretty normal

#test normality of each treatment group with shapiro wilk test
shapiro.test(subset(colonies, colonies$Treatment == "Water")[,2]) #PM Water, p-value = 0.003363, NOT normal
shapiro.test(subset(colonies, colonies$Treatment == "SA")[,2]) #PM SA, p-value = 0.0004192, NOT normal
shapiro.test(subset(colonies, colonies$Treatment == "JA")[,2]) #PM JA, p-value = 0.002997, NOT normal
shapiro.test(subset(colonies, colonies$Treatment == "BABA")[,2]) #PM BABA, p-value = 0.0004469, NOT normal

#overall the data doesnt meet assumptions for ANOVA
#think lots of zeros cause this, would also make transforming data difficult
#so will do KW
kruskal.test(Colonies ~ Treatment, data = colonies) #p-value = 0.2784 so not significant

#note ANOVA also not significant (p = 0.05759)
anova(colonies_model)

#Plot DSI lab graph#########
p2 <- ggplot(data = DSI_lab, 
             mapping = aes(x = Treatment, y = DSI, fill = Treatment))+
  geom_boxplot()+
  scale_fill_manual(values = treatment_cols)+
  geom_jitter(size = 10, shape = 17)+
  ylab("DSI lab 30dpi")+
  xlab(NULL)+
  scale_y_continuous(limits = c(-2,105), breaks = seq(0,125, 25))+ #start limits at -2 to avoid cutting off points due to size
  guides(fill = "none", pch = "none")+
  theme(panel.grid.major = element_blank(), #remove major grid lines
        panel.grid.minor = element_blank(), #remove minor grid lines
        panel.background = element_rect(fill = "#f5f5f5"), #light grey background to match other graphs
        axis.line = element_line(), #add lines to axis
        axis.text.x = element_text(size = 40), # Change x-axis label size
        axis.text.y = element_text(size = 40)) #change y-axis label size

p2 #view graph

ggsave(filename = "final_graphs/DSI_lab.pdf", p2, device = pdf, width = 33, height = 20) #save graph

#DSI lab stats#######
DSI_lab_model <- lm(DSI ~ Treatment, data = DSI_lab)

#check quality of model with olsrr package
ols_plot_resid_fit(DSI_lab_model)#residual v fitted graph, looks pretty homogeneous
ols_plot_resid_qq(DSI_lab_model) #qq plot = looks pretty normal
ols_plot_resid_hist(DSI_lab_model) #residuals histogram = looks pretty normal

#test normality of each treatment group with shapiro wilk test
shapiro.test(subset(DSI_lab, DSI_lab$Treatment == "Water")[,2]) #PM Water, p-value = 0.4527, normal
shapiro.test(subset(DSI_lab, DSI_lab$Treatment == "SA")[,2]) #PM SA, p-value = 0.7402, normal
shapiro.test(subset(DSI_lab, DSI_lab$Treatment == "JA")[,2]) #PM JA, p-value = 0.6968, normal
shapiro.test(subset(DSI_lab, DSI_lab$Treatment == "BABA")[,2]) #PM BABA, p-value = 0.001528, NOT normal, probably due to lots of 0s

#test for homogeneous variances with levenes test
car::leveneTest(DSI_lab_model) #p = 0.6448, this is >0.05 so variances are homogeneous

#overall variance homogeneous and all except one sample are normal, probably fine to do ANOVA
anova(DSI_lab_model) #p = 1.498e-07 ***, so ANOVA significant

#post-hoc testing 
#variances are homogeneous so use tukey test
summary(glht(DSI_lab_model, mcp(Treatment = "Tukey")))

#BABA - Water p < 0.001 *** 
#SA - Water p = 0.00169 ** 
#JA - BABA p < 0.001 ***
#SA - BABA p = 0.01703 *  

#double check with kruskal and dunn post hoc
kruskal.test(DSI ~ Treatment, data = DSI_lab) #p-value = 1.974e-05, so KW significant
FSA::dunnTest(DSI_lab$DSI, DSI_lab$Treatment, method = "none")

#BABA - JA p = 2.175365e-03
#BABA - SA p = 4.252270e-02
#BABA - Water p = 1.401218e-06
#SA - Water p = 5.166528e-03 

#overall KW and ANOVA both find significant differences
#post-hoc agrees on which groups are significant for KW and ANOVA

#Plot DSI field graph#######
p3 <- ggplot(data = DSI_field, 
             mapping = aes(x = Treatment, y = DSI, fill = Treatment))+
  geom_boxplot()+
  scale_fill_manual(values = treatment_cols)+
  geom_jitter(size = 10, shape = 17)+
  ylab("DSI field 30dpi")+
  xlab(NULL)+
  scale_y_continuous(limits = c(-2,105), breaks = seq(0,125, 25))+
  guides(fill = "none", pch = "none")+
  theme(panel.grid.major = element_blank(), #remove major grid lines
        panel.grid.minor = element_blank(), #remove minor grid lines
        panel.background = element_rect(fill = "#f5f5f5"), #light grey background to match other graphs
        axis.line = element_line(), #add lines to axis
        axis.text.x = element_text(size = 40), # Change x-axis label size
        axis.text.y = element_text(size = 40)) #change y-axis label size

p3 #view graph

ggsave(filename = "final_graphs/DSI_field.pdf", p3, device = pdf, width = 33, height = 20) #save graph

#DSI field stats#########
DSI_field_model <- lm(DSI ~ Treatment, data = DSI_field)

#check quality of model with olsrr package
ols_plot_resid_fit(DSI_field_model)#residual v fitted graph, looks pretty homogeneous
ols_plot_resid_qq(DSI_field_model) #qq plot = looks pretty normal
ols_plot_resid_hist(DSI_field_model) #residuals histogram = looks pretty normal

#test normality of each treatment group with shapiro wilk test
shapiro.test(subset(DSI_field, DSI_field$Treatment == "Water")[,2]) #PM Water, p-value = 0.1758, normal
shapiro.test(subset(DSI_field, DSI_field$Treatment == "SA")[,2]) #PM SA, p-value = 0.02848, NOT normal
shapiro.test(subset(DSI_field, DSI_field$Treatment == "JA")[,2]) #PM JA, p-value = 0.4055, normal
shapiro.test(subset(DSI_field, DSI_field$Treatment == "BABA")[,2]) #PM BABA, p-value = 0.01744, NOT normal

#test for homogeneous variances with levenes test
car::leveneTest(DSI_field_model) #p = 0.5973, this is >0.05 so variances are homogeneous

#anova probably okay but will do KW as well to check
anova(DSI_field_model) #p = 0.000841 ***, so ANOVA significant

#post-hoc testing 
#variances are homogeneous so use tukey test
summary(glht(DSI_field_model, mcp(Treatment = "Tukey")))

#BABA - Water p = 0.00994 ** 
#JA - BABA p = 0.03940 *  
#SA - BABA p < 0.001 *** 

#double check with kruskal and dunn post hoc
kruskal.test(DSI ~ Treatment, data = DSI_field) #p-value = 0.009005, so KW significant
FSA::dunnTest(DSI_field$DSI, DSI_field$Treatment, method = "none")

#BABA - Water p = 0.0190644489 *
#BABA - SA p = 0.0008562415 ***

