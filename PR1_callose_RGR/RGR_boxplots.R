#Iniital set-up########
library(ggplot2) #for plotting
library(readxl) #to load data from excel
library(olsrr) #for checking anova assumptions
library(car) #for levennes test
library(multcomp) #for anova post hoc testing

RGR <- read_excel("data/RGR_leaf_no_data.xlsx", 
             sheet = "-7_14_RGR_no_neg", na = "NA") #load data for -7 v 14dpi RGR

RGR$Treatment <- ordered(RGR$Treatment, #order by treatment
                         levels = c("Mock Water", "Mock BABA", "Mock JA", "Mock SA"))

RGR <- na.omit(RGR) #remove NAs

RGR <- as.data.frame(RGR) #convert from tibble to df (otherwise shapiro wilk doesnt work)

#Set up treatment colours/shapes
treatment_cols <- c("Mock Water" = "blue", 
                    "Mock BABA" = "#117733", 
                    "Mock JA" = "#AA4499", 
                    "Mock SA" = "#DDCC77")

#Plot graphs#########

#height
p1 <- ggplot(data = RGR, 
             mapping = aes(x = Treatment, y = height_RGR, fill = Treatment))+
  geom_boxplot()+
  scale_fill_manual(values = treatment_cols)+
  geom_jitter(size = 10)+
  ylab("Relative growth rate per day")+
  xlab(NULL)+
  ggtitle("Height")+
  guides(fill = "none", pch = "none")+
  scale_y_continuous(limits = c(-0.01, 0.05))+
  theme(panel.grid.major = element_blank(), #remove major grid lines
        panel.grid.minor = element_blank(), #remove minor grid lines
        panel.background = element_rect(fill = "#f5f5f5"), #light grey background to match other graphs
        axis.line = element_line(), #add lines to axes
        axis.text.x = element_text(size = 40), # Change x-axis label size
        axis.text.y = element_text(size = 40)) #change y-axis label size

p1

#diameter
p2 <- ggplot(data = RGR, 
             mapping = aes(x = Treatment, y = diameter_RGR, fill = Treatment))+
  geom_boxplot()+
  scale_fill_manual(values = treatment_cols)+
  geom_jitter(size = 10)+
  ylab(NULL)+
  xlab(NULL)+
  ggtitle("Diameter")+
  guides(fill = "none", pch = "none")+
  scale_y_continuous(limits = c(-0.01, 0.05))+
  theme(panel.grid.major = element_blank(), #remove major grid lines
        panel.grid.minor = element_blank(), #remove minor grid lines
        panel.background = element_rect(fill = "#f5f5f5"), #light grey background to match other graphs
        axis.line = element_line(), #add lines to axes
        axis.text.x = element_text(size = 40), # Change x-axis label size
        axis.text.y = element_text(size = 40)) #change y-axis label size

p2

#leaf size
p3 <- ggplot(data = RGR, 
             mapping = aes(x = Treatment, y = leaf_length_RGR, fill = Treatment))+
  geom_boxplot()+
  scale_fill_manual(values = treatment_cols)+
  geom_jitter(size = 10)+
  ylab(NULL)+
  xlab(NULL)+
  ggtitle("Leaf Length")+
  guides(fill = "none", pch = "none")+
  scale_y_continuous(limits = c(-0.01, 0.05))+
  theme(panel.grid.major = element_blank(), #remove major grid lines
        panel.grid.minor = element_blank(), #remove minor grid lines
        panel.background = element_rect(fill = "#f5f5f5"), #light grey background to match other graphs
        axis.line = element_line(), #add line to x to axis
        axis.text.x = element_text(size = 40), # Change x-axis label size
        axis.text.y = element_text(size = 40)) #change y-axis label size

p3

ggsave(filename = "final_graphs/height_RGR.pdf", plot = p1, device = pdf, width = 33, height = 20)
ggsave(filename = "final_graphs/diameter_RGR.pdf", plot = p2, device = pdf, width = 33, height = 20)
ggsave(filename = "final_graphs/leaf_length_RGR.pdf", plot = p3, device = pdf, width = 33, height = 20)

#Height stats#######
height_model <- lm(height_RGR ~ Treatment, data = RGR)

#check quality of model with olsrr package
ols_plot_resid_fit(height_model)#residual v fitted graph, looks pretty homogeneous
ols_plot_resid_qq(height_model) #qq plot = looks pretty normal
ols_plot_resid_hist(height_model) #residuals histogram = deviates a bit from normal

#test normality of each treatment group with shapiro wilk test
shapiro.test(subset(RGR, RGR$Treatment == "Mock Water")[,2]) #Mock Water, p-value = 0.1228, normal
shapiro.test(subset(RGR, RGR$Treatment == "Mock SA")[,2]) #Mock SA, p-value = 0.003036, NOT normal
shapiro.test(subset(RGR, RGR$Treatment == "Mock JA")[,2]) #Mock JA, p-value = 0.3075, normal
shapiro.test(subset(RGR, RGR$Treatment == "Mock BABA")[,2]) #Mock BABA, p-value = 0.01744, NOT normal

#two out of four samples are not normal so will do KW
kruskal.test(height_RGR ~ Treatment, data = RGR) #p-value = 0.05201, NOT significant
#note anova
anova(height_model) #p = 0.1292, NOT significant

#Diameter stats#########
diameter_model <- lm(diameter_RGR ~ Treatment, data = RGR)

#check quality of model with olsrr package
ols_plot_resid_fit(diameter_model)#residual v fitted graph, looks pretty homogeneous
ols_plot_resid_qq(diameter_model) #qq plot = looks pretty normal
ols_plot_resid_hist(diameter_model) #residuals histogram = looks pretty normal

#test normality of each treatment group with shapiro wilk test
shapiro.test(subset(RGR, RGR$Treatment == "Mock Water")[,4]) #Mock Water, p-value = 0.09049, normal
shapiro.test(subset(RGR, RGR$Treatment == "Mock SA")[,4]) #Mock SA, p-value = 0.05552, normal
shapiro.test(subset(RGR, RGR$Treatment == "Mock JA")[,4]) #Mock JA, p-value = 0.01933, NOT normal
shapiro.test(subset(RGR, RGR$Treatment == "Mock BABA")[,4]) #Mock BABA, p-value = 0.2898, normal

#one out of 4 samples is NOT normal so could possibly do anova
#BUT will do KW to be consistent with other RGR measures
kruskal.test(diameter_RGR ~ Treatment, data = RGR) #p-value = 0.5525, NOT significant
#note anova
anova(diameter_model) #p = 0.2531, NOT significant

#anova and KW NOT sig

#Leaf_length stats########
leaf_length_model <- lm(leaf_length_RGR ~ Treatment, data = RGR)

#check quality of model with olsrr package
ols_plot_resid_fit(leaf_length_model)#residual v fitted graph, looks pretty homogeneous
ols_plot_resid_qq(leaf_length_model) #qq plot = looks pretty normal
ols_plot_resid_hist(leaf_length_model) #residuals histogram = looks pretty normal

#test normality of each treatment group with shapiro wilk test
shapiro.test(subset(RGR, RGR$Treatment == "Mock Water")[,3]) #Mock Water, p-value = 0.0001778, NOT normal
shapiro.test(subset(RGR, RGR$Treatment == "Mock SA")[,3]) #Mock SA, p-value = 0.002012, NOT normal
shapiro.test(subset(RGR, RGR$Treatment == "Mock JA")[,3]) #Mock JA, p-value = 0.01026, NOT normal
shapiro.test(subset(RGR, RGR$Treatment == "Mock BABA")[,3]) #Mock BABA, p-value = 0.2573, normal

#3 out of 4 samples are NOT normal so will do KW
kruskal.test(leaf_length_RGR ~ Treatment, data = RGR) #p-value =  0.1858, NOT significant
#note anova
anova(leaf_length_model) #p = 0.1272, NOT significant


