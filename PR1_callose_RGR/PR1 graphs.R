#Initial set-up############
library(ggplot2) #for plotting
library(ggbreak) #for axis breaks
library(olsrr) #for checking anova assumptions
library(car) #for levennes test
library(multcomp) #for anova post hoc testing
library(FSA) #for kruskal wallis post hoc testing (Dunn's test)

#load data
PR1_data <- read.csv("data/PR1_data.csv")

#set treatment as factor to keep boxes in right order (all mocks, then all PMs)
PR1_data$treatment <- 
  factor(PR1_data$treatment, levels = 
           c("Mock Water", "Mock BABA", "Mock JA", "Mock SA", "PM Water", "PM BABA", "PM JA", "PM SA")) 

#Set up colour/shape coding for graphs
treatment_cols <- c("Mock Water" = "blue", 
                    "Mock BABA" = "#117733", 
                    "Mock JA" = "#AA4499", 
                    "Mock SA" = "#DDCC77",
                    "PM Water" = "#33bbee",
                    "PM BABA" = "#44AA99",
                    "PM JA" = "#882255",
                    "PM SA" = "#999933")

treatment_pch <- c("Mock Water" = 19, 
                   "Mock BABA" = 19, 
                   "Mock JA" = 19, 
                   "Mock SA" = 19,
                   "PM Water" = 17,
                   "PM BABA" = 17,
                   "PM JA" = 17,
                   "PM SA" = 17)

#All Graphs########

#no breaks
p1 <- ggplot(data = PR1_data, 
       mapping = aes(x = treatment, y = fold_change, fill = treatment))+
  geom_boxplot()+
  facet_grid(~ infection, scales = "free_x")+ #split mock and PM on x axis
  geom_jitter(size = 10)+ #show data points
  scale_y_continuous(limits = c(0, 3000))+
  scale_fill_manual(values = treatment_cols)+ #colour code
  scale_shape_manual(values = treatment_pch)+ #shape of points
  guides(fill = "none", pch = "none")+ #remove legend
  xlab(NULL)+ #remove x axis label
  ylab(NULL)+ #remove y axis label
  theme(panel.grid.major = element_blank(), #remove major grid lines
        panel.grid.minor = element_blank(), #remove minor grid lines
        axis.line = element_line(), #add lines to axis
        axis.line.y.right = element_blank(),
        axis.text.y.right = element_blank(), #remove right y axis added by scale_y_break
        axis.ticks.y.right = element_blank(),
        axis.text.x = element_text(size = 18), # Change x-axis label size
        axis.text.y = element_text(size = 18), #change y-axis label size
        panel.spacing = unit(1, "lines")) #add more width for x axis split

p1

#1 break
p2 <- ggplot(data = PR1_data, 
       mapping = aes(x = treatment, y = fold_change, fill = treatment))+
  geom_boxplot()+
  facet_grid(~ infection, scales = "free_x")+ #split mock and PM on x axis
  geom_jitter(size = 10)+ #show data points
  scale_y_break(c(1000, 2500), scales = 0.25, space = 0.2, ticklabels = seq(2500, 2700, 100))+ #break for mock BABA
  scale_y_continuous(limits = c(0, 2700))+ #set axis limits
  scale_fill_manual(values = treatment_cols)+ #colour code
  scale_shape_manual(values = treatment_pch)+ #shape of points
  guides(fill = "none", pch = "none")+ #remove legend
  xlab(NULL)+ #remove x axis label
  ylab(NULL)+ #remove y axis label
  theme(panel.grid.major = element_blank(), #remove major grid lines
        panel.grid.minor = element_blank(), #remove minor grid lines
        axis.line = element_line(), #add lines to axis
        axis.line.y.right = element_blank(),
        axis.text.y.right = element_blank(), #remove right y axis added by scale_y_break
        axis.ticks.y.right = element_blank(),
        axis.text.x = element_text(size = 18), # Change x-axis label size
        axis.text.y = element_text(size = 18), #change y-axis label size
        panel.spacing = unit(1, "lines")) #add more width for x axis split

p2

#2 breaks
p3 <- ggplot(data = PR1_data, 
       mapping = aes(x = treatment, y = fold_change, fill = treatment))+
  geom_boxplot()+
  facet_grid(~ infection, scales = "free_x")+ #split mock and PM on x axis
  geom_jitter(size = 10)+ #show data points
  scale_y_break(c(1100, 2500), scales = 0.25, space = 0.5, ticklabels = seq(2500, 2700, 100))+ #break for mock BABA
  scale_y_break(c(160, 300), scales = 0.25, space = 0.5, ticklabels = seq(250, 1250, 500))+ #break for mock JA
  scale_y_continuous(limits = c(0, 2700))+ #set axis limits
  scale_fill_manual(values = treatment_cols)+ #colour code
  scale_shape_manual(values = treatment_pch)+ #shape of points
  guides(fill = "none", pch = "none")+ #remove legend
  xlab(NULL)+ #remove x axis label
  ylab(NULL)+ #remove y axis label
  theme(panel.grid.major = element_blank(), #remove major grid lines
        panel.grid.minor = element_blank(), #remove minor grid lines
        axis.line = element_line(), #add lines to axis
        axis.line.y.right = element_blank(),
        axis.text.y.right = element_blank(), #remove right y axis added by scale_y_break
        axis.ticks.y.right = element_blank(),
        axis.text.x = element_text(size = 18), # Change x-axis label size
        axis.text.y = element_text(size = 40), #change y-axis label size
        panel.spacing = unit(1, "lines")) #add more width for x axis split

p3

#3 breaks
p4 <- ggplot(data = PR1_data, 
       mapping = aes(x = treatment, y = fold_change, fill = treatment, pch = treatment))+
  geom_boxplot()+
  facet_grid(~ infection, scales = "free_x")+ #split mock and PM on x axis
  geom_jitter(size = 10)+ #show data points
  scale_y_break(c(1250, 2500), scales = 0.5, space = 1, ticklabels = seq(2500, 2700, 100))+ #break for mock BABA
  scale_y_break(c(200, 250), scales = 0.5, space = 1, ticklabels = seq(250, 1250, 500))+ #break for mock JA
  scale_y_break(c(30, 40), scales = 0.5, space = 1, ticklabels = seq(50, 200, 75))+ #break for PM BABA and PM JA
  scale_y_continuous(limits = c(0, 2700))+ #set axis limits
  scale_fill_manual(values = treatment_cols)+ #colour code
  scale_shape_manual(values = treatment_pch)+ #shape of points
  guides(fill = "none", pch = "none")+ #remove legend
  xlab(NULL)+ #remove x axis label
  ylab(NULL)+ #remove y axis label
  theme(panel.grid.major = element_blank(), #remove major grid lines
        panel.grid.minor = element_blank(), #remove minor grid lines
        panel.background = element_rect(fill = "#f5f5f5"), #light grey background to make splits more visible
        axis.line = element_line(), #add lines to axis
        axis.line.y.right = element_blank(),
        axis.text.y.right = element_blank(), #remove right y axis added by scale_y_break
        axis.ticks.y.right = element_blank(),
        axis.text.x = element_text(size = 40), # Change x-axis label size
        axis.text.y = element_text(size = 40), #change y-axis label size
        panel.spacing = unit(1, "lines")) #add more width for x axis split
p4

#Save all plots
ggsave(filename = "final_graphs/PR1_no_break.pdf", plot = p1, device = pdf, width = 33, height = 20)
ggsave(filename = "final_graphs/PR1_1_break.pdf", plot = p2, device = pdf, width = 33, height = 20)
ggsave(filename = "final_graphs/PR1_2_breaks.pdf", plot = p3, device = pdf, width = 33, height = 20)
ggsave(filename = "final_graphs/PR1_3_breaks.pdf", plot = p4, device = pdf, width = 33, height = 20)

#3 breaks look best so will use that graph
#for stats will do mock and PM separate to compare elicitor alone v priming effects
#will also do each elicitor alone + water as data very variable so hard to see effects when all together

#Mock stats######

PR1_mock_data <- subset(PR1_data, PR1_data$infection == "Mock") #select only mock data
PR1_mock_model <- lm(fold_change ~ treatment, data = PR1_mock_data) #set up model

#check quality of model with olsrr package
ols_plot_resid_fit(PR1_mock_model)#residual v fitted graph, looks pretty homogeneous
ols_plot_resid_qq(PR1_mock_model) #qq plot = deviates from normal
ols_plot_resid_hist(PR1_mock_model) #residuals histogram = deviates from normal

#test normality of each treatment group with shapiro wilk test
shapiro.test(subset(PR1_mock_data, PR1_mock_data$treatment == "Mock Water")[,2]) #Mock Water, p-value = 0.3576, normal
#shapiro.test(subset(PR1_mock_data, PR1_mock_data$treatment == "Mock SA")[,2]) #Mock SA, too few samples (<3)/NOT normal
shapiro.test(subset(PR1_mock_data, PR1_mock_data$treatment == "Mock JA")[,2]) #Mock JA, p-value = 0.4189, normal
#shapiro.test(subset(PR1_mock_data, PR1_mock_data$treatment == "Mock BABA")[,2]) #Mock BABA, too few samples (<3)/NOT normal

#overall mock SA and mock BABA have too few samples/are NOT normal
#so data is unlikely to fit assumptions of anova so will do KW instead
kruskal.test(fold_change ~ treatment, data = PR1_mock_data) #p-value = 0.03749, so KW significant
FSA::dunnTest(fold_change ~ treatment, data = PR1_mock_data, method = "none") 

# Mock BABA - Mock Water p = 0.006655605
# Mock JA - Mock Water p = 0.043114447 

#PM stats######

PR1_PM_data <- subset(PR1_data, PR1_data$infection == "PM") #select only PM data
PR1_PM_model <- lm(fold_change ~ treatment, data = PR1_PM_data) #set up model

#check quality of model with olsrr package
ols_plot_resid_fit(PR1_PM_model)#residual v fitted graph, looks pretty homogeneous
ols_plot_resid_qq(PR1_PM_model) #qq plot = deviates a bit from normal
ols_plot_resid_hist(PR1_PM_model) #residuals histogram = deviates a bit from normal

#test normality of each treatment group with shapiro wilk test
shapiro.test(subset(PR1_PM_data, PR1_PM_data$treatment == "PM Water")[,2]) #PM Water, p-value = 0.2026, normal
shapiro.test(subset(PR1_PM_data, PR1_PM_data$treatment == "PM SA")[,2]) #PM SA, p-value = 0.3992, normal
shapiro.test(subset(PR1_PM_data, PR1_PM_data$treatment == "PM JA")[,2]) #PM JA, p-value = 0.03385, NOT normal
#shapiro.test(subset(PR1_PM_data, PR1_PM_data$treatment == "PM BABA")[,2]) #PM BABA, too few samples (<3)/NOT normal

#overall two out of four samples are have too few samples/are not normal so do KW
kruskal.test(fold_change ~ treatment, data = PR1_PM_data) #p-value = 0.1368, so KW NOT significant
#note anova
anova(PR1_PM_model) #p = 0.5199, so anova NOT significant

#SA + water stats####

PR1_SA_water_data <- PR1_data[1:11,] #select only SA and water data
PR1_SA_water_model <- lm(fold_change ~ treatment, data = PR1_SA_water_data) #set up model

#check quality of model with olsrr package
ols_plot_resid_fit(PR1_SA_water_model)#residual v fitted graph, looks fairly homogeneous
ols_plot_resid_qq(PR1_SA_water_model) #qq plot = looks fairly normal
ols_plot_resid_hist(PR1_SA_water_model) #residuals histogram = deviates a bit from normal

#test normality of each treatment group with shapiro wilk test
shapiro.test(subset(PR1_SA_water_data, PR1_SA_water_data$treatment == "Mock Water")[,2]) #Mock Water, p-value = 0.3576, normal
#shapiro.test(subset(PR1_SA_water_data, PR1_SA_water_data$treatment == "Mock SA")[,2]) #Mock SA, too few samples (<3)
shapiro.test(subset(PR1_SA_water_data, PR1_SA_water_data$treatment == "PM Water")[,2]) #PM Water, p-value = 0.2026, normal
shapiro.test(subset(PR1_SA_water_data, PR1_SA_water_data$treatment == "PM SA")[,2]) #PM SA, p-value = 0.3992, normal

#test for homogeneous variances with levenes test
car::leveneTest(PR1_SA_water_model) #p = 0.6345, this is >0.05 so variances are homogeneous

#overall variances are homogeneous + only one group has too few samples/is not normal
#so will assume data fits assumptions of anova = perform anova
anova(PR1_SA_water_model) #p = 0.0007942 ***, so anova significant
#double check with kruskal
kruskal.test(fold_change ~ treatment, data = PR1_SA_water_data) #p-value = 0.04478, so KW significant

#post-hoc testing 
#have done ANOVA and variances are homogeneous so use tukey test
summary(glht(PR1_SA_water_model, mcp(treatment="Tukey")))

#PM SA - Mock Water p < 0.001 ***
#PM SA - Mock SA p = 0.02565 *  
#PM SA - PM Water p = 0.00204 ** 

#JA + water stats#######

PR1_JA_water_data <- PR1_data[c(1:3, 6:8, 12:14, 15:17),] #select only JA and water data
PR1_JA_water_model <- lm(fold_change ~ treatment, data = PR1_JA_water_data) #set up model

#check quality of model with olsrr package
ols_plot_resid_fit(PR1_JA_water_model)#residual v fitted graph, looks fairly homogeneous
ols_plot_resid_qq(PR1_JA_water_model) #qq plot = deviates a bit from normal
ols_plot_resid_hist(PR1_JA_water_model) #residuals histogram = deviates a bit from normal

#test normality of each treatment group with shapiro wilk test
shapiro.test(subset(PR1_JA_water_data, PR1_JA_water_data$treatment == "Mock Water")[,2]) #Mock Water, p-value = 0.3576, normal
shapiro.test(subset(PR1_JA_water_data, PR1_JA_water_data$treatment == "Mock JA")[,2]) #Mock JA, p-value = 0.4189, normal
shapiro.test(subset(PR1_JA_water_data, PR1_JA_water_data$treatment == "PM Water")[,2]) #PM Water, p-value = 0.2026, normal
shapiro.test(subset(PR1_JA_water_data, PR1_JA_water_data$treatment == "PM JA")[,2]) #PM JA, p-value = 0.03385, NOT normal

#test for homogeneous variances with levenes test
car::leveneTest(PR1_JA_water_model) #p = 0.2149, this is >0.05 so variances are homogeneous

#overall whilst PM JA isnt normal, rest of data is normal + variances are homogeneous
#so will assume data fits assumptions of anova = perform anova
anova(PR1_JA_water_model) #p = 0.06096, so anova NOT significant

#double check with kruskal
kruskal.test(fold_change ~ treatment, data = PR1_JA_water_data) #p-value = 0.05723, so KW NOT significant

#BABA + Water stats########

PR1_BABA_water_data <- PR1_data[c(1:3, 6:8, 18:21),] #select only BABA and water data
PR1_BABA_water_model <- lm(fold_change ~ treatment, data = PR1_BABA_water_data) #set up model

#check quality of model with olsrr package
ols_plot_resid_fit(PR1_BABA_water_model)#residual v fitted graph, looks fairly homogeneous
ols_plot_resid_qq(PR1_BABA_water_model) #qq plot = deviates a bit from normal
ols_plot_resid_hist(PR1_BABA_water_model) #residuals histogram = fairly normal

#test normality of each treatment group with shapiro wilk test
shapiro.test(subset(PR1_BABA_water_data, PR1_BABA_water_data$treatment == "Mock Water")[,2]) #Mock Water, p-value = 0.3576, normal
#shapiro.test(subset(PR1_BABA_water_data, PR1_BABA_water_data$treatment == "Mock BABA")[,2]) #Mock BABA, too few samples (<3)
shapiro.test(subset(PR1_BABA_water_data, PR1_BABA_water_data$treatment == "PM Water")[,2]) #PM Water, p-value = 0.2026, normal
#shapiro.test(subset(PR1_BABA_water_data, PR1_BABA_water_data$treatment == "PM BABA")[,2]) #PM BABA, too few samples (<3)

#overall two out of four samples are have too few samples/are not normal so do KW
kruskal.test(fold_change ~ treatment, data = PR1_BABA_water_data) #p-value = 0.05922, so KW NOT significant

#SA + Water graph#######
#since stats for SA + Water give significant result will also plot to better visualize
#won't plot JA and BABA data as not significant
p5 <- ggplot(data = PR1_SA_water_data, 
             mapping = aes(x = treatment, y = fold_change, fill = treatment))+
  geom_boxplot()+
  geom_jitter(size = 10)+ #show data points
  scale_y_continuous(limits = c(0, 25))+
  scale_fill_manual(values = treatment_cols)+ #colour code
  scale_shape_manual(values = treatment_pch)+ #shape of points
  guides(fill = "none", pch = "none")+ #remove legend
  xlab(NULL)+ #remove x axis label
  ylab(NULL)+ #remove y axis label
  theme(panel.grid.major = element_blank(), #remove major grid lines
        panel.grid.minor = element_blank(), #remove minor grid lines
        axis.line = element_line(), #add lines to axis
        axis.line.y.right = element_blank(),
        axis.text.y.right = element_blank(), #remove right y axis added by scale_y_break
        axis.ticks.y.right = element_blank(),
        axis.text.x = element_text(size = 18), # Change x-axis label size
        axis.text.y = element_text(size = 18), #change y-axis label size
        panel.spacing = unit(1, "lines")) #add more width for x axis split

p5

ggsave(filename = "final_graphs/SA_water_graph.pdf", plot = p5, device = pdf, width = 33, height = 20)
