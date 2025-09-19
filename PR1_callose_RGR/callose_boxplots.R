#Load packages######
library(ggplot2) #for plotting
library(olsrr) #for checking anova assumptions
library(car) #for levennes test
library(multcomp) #for anova post hoc testing
library(PMCMRplus) #for Dunnett's T3 test (welch anova post-hoc)

#Load and prep data########

#Effective callose
effective_callose <- read.csv(file = "data/effective_callose.csv")

effective_callose$Treatment <- ordered(effective_callose$Treatment, #order by treatment
                         levels = c("Water", "BABA", "JA", "SA"))

#Total callose
total_callose <- read.csv(file = "data/total_callose.csv")

total_callose$Sample <- as.factor(total_callose$Sample) #convert sample to factor for stats

total_callose$average_area <- rowMeans(total_callose[,2:4]) #calculate average callose area

total_callose$Treatment <- ordered(total_callose$Treatment, #order by treatment
                                       levels = c("Water", "BABA", "JA", "SA"))

#Set up treatment colours/shapes
treatment_cols <- c("Water" = "#33bbee", 
                    "BABA" = "#44AA99", 
                    "JA" = "#882255", 
                    "SA" = "#999933")

#set up mock v infected shapes
treatment_pch <- c("Mock" = 19, "Infected" = 17)

#Plot effective callose#####
p1 <- ggplot(data = effective_callose, 
             mapping = aes(x = Treatment, y = percent_callose, fill = Treatment))+
  geom_boxplot()+
  scale_fill_manual(values = treatment_cols)+
  geom_jitter(shape = 17, size = 10)+
  ylab("Percentage effective_callose")+
  xlab(NULL)+
  guides(fill = "none", pch = "none")+
  scale_y_continuous(breaks = seq(0, 60, 10), limits = c(0,60))+
  theme(panel.grid.major = element_blank(), #remove major grid lines
        panel.grid.minor = element_blank(), #remove minor grid lines
        panel.background = element_rect(fill = "#f5f5f5"), #light grey background to match other graphs
        axis.line = element_line(), #add lines to axis
        axis.text.x = element_text(size = 40), # Change x-axis label size
        axis.text.y = element_text(size = 40)) #change y-axis label size

p1

ggsave(filename = "final_graphs/effective_callose.pdf", plot = p1, device = pdf, width = 33, height = 20)

#Stats testing - effective callose#####
effective_callose_model <- lm(percent_callose ~ Treatment, data = effective_callose)

#check quality of model with olsrr package
ols_plot_resid_fit(effective_callose_model)#residual v fitted graph, looks pretty homogeneous
ols_plot_resid_qq(effective_callose_model) #qq plot = looks pretty normal
ols_plot_resid_hist(effective_callose_model) #residuals histogram = looks pretty normal

#test normality of each treatment group with shapiro wilk test
shapiro.test(subset(effective_callose, effective_callose$Treatment == "Water")[,2]) #Water, p-value = 0.4794, normal
shapiro.test(subset(effective_callose, effective_callose$Treatment == "SA")[,2]) #SA, p-value = 0.6303, normal
shapiro.test(subset(effective_callose, effective_callose$Treatment == "JA")[,2]) #JA, p-value = 0.4483, normal
shapiro.test(subset(effective_callose, effective_callose$Treatment == "BABA")[,2]) #BABA, p-value = 0.4807, normal

#test for homogeneous variances with levenes test
car::leveneTest(effective_callose_model) #p = 0.946, this is >0.05 so variances are homogeneous

#overall data meets assumptions for ANOVA
anova(effective_callose_model) #p < 2.2e-16 ***, significant

#post-hoc testing 
#variances are homogeneous so use tukey test
summary(glht(effective_callose_model, mcp(Treatment="Tukey")))

#BABA - Water p <0.001 ***
#SA - Water p <0.001 ***
#JA - BABA p <0.001 ***
#SA - BABA p <0.001 ***
#SA - JA p <0.001 ***
#i.e. all except JA-Water are significant

#Plot total callose#####
p3 <- ggplot(data = total_callose, 
             mapping = aes(x = Treatment, y = average_area, fill = Treatment, pch = Infection))+
  geom_boxplot()+
  scale_fill_manual(values = treatment_cols)+
  scale_shape_manual(values = treatment_pch)+
  geom_jitter(size = 10)+
  ylab("Total callose")+
  facet_grid(~Timepoint)+
  xlab(NULL)+
  guides(fill = "none", pch = "none")+
  scale_y_continuous(breaks = seq(0, 40, 10), limits = c(0,40))+
  theme(panel.grid.major = element_blank(), #remove major grid lines
        panel.grid.minor = element_blank(), #remove minor grid lines
        panel.background = element_rect(fill = "#f5f5f5"), #light grey background to match other graphs
        axis.line = element_line(), #add lines to axis
        axis.text.x = element_text(size = 40), # Change x-axis label size
        axis.text.y = element_text(size = 40)) #change y-axis label size

p3

ggsave(filename = "final_graphs/total_callose.pdf", plot = p3, device = pdf, width = 33, height = 20)

#Stats testing - total callose####
total_callose_model <- lm(average_area ~ Sample, data = total_callose)

#check quality of model with olsrr package
ols_plot_resid_fit(total_callose_model)#residual v fitted graph, looks pretty homogeneous
ols_plot_resid_qq(total_callose_model) #qq plot = looks pretty normal
ols_plot_resid_hist(total_callose_model) #residuals histogram = looks pretty normal

#test normality of each treatment group with shapiro wilk test
shapiro.test(subset(total_callose, total_callose$Sample == "0_PM_Water")[,8]) #Water 0dpi, p-value = 0.4053, normal

shapiro.test(subset(total_callose, total_callose$Sample == "1_PM_Water")[,8]) #Water 1dpi, p-value = 0.591, normal

shapiro.test(subset(total_callose, total_callose$Sample == "0_PM_BABA")[,8]) #BABA 0dpi, p-value = 0.9523, normal

shapiro.test(subset(total_callose, total_callose$Sample == "1_PM_BABA")[,8]) #BABA 1dpi, p-value = 0.8928, normal

shapiro.test(subset(total_callose, total_callose$Sample == "0_PM_JA")[,8]) #JA 0dpi, p-value = 0.5407, normal

shapiro.test(subset(total_callose, total_callose$Sample == "1_PM_JA")[,8]) #JA 1dpi, p-value = 0.6475, normal

shapiro.test(subset(total_callose, total_callose$Sample == "0_PM_SA")[,8]) #SA 0dpi, p-value = 0.00721, NOT normal

shapiro.test(subset(total_callose, total_callose$Sample == "1_PM_SA")[,8]) #SA 1dpi, p-value = 0.9227, normal

#all except SA 0dpi are normal

#test for homogeneous variances with levenes test
car::leveneTest(total_callose_model) #p = 0.03618 *, this is <0.05 so variances are NOT homogeneous

#overall data looks to match normality assumption for anova (only 1 of 8 is not normal)
#since it does not match homogeneous variances assumption will do welch anova
oneway.test(average_area ~ Sample, data = total_callose, var.equal = FALSE) #p-value = 1.93e-08 = significant

#post hoc test
dunnettT3Test(average_area ~ Sample, data = total_callose)

#0B = 1B, 1S
#0J = 1B, 1S
#0S = 1B, 1J, 1S, 1W
#0W = 1B, 1S
#1B = 1J, 1S, 1W
#1J = 1S

