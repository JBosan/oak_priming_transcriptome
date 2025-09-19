#Initial set-up####
library(ggplot2) #for plotting
library(readxl) #to load data from excel
library(olsrr) #for checking anova assumptions
library(car) #for levennes test
library(multcomp) #for anova post hoc testing

#load data
leaf_no_14 <- 
  read_excel("data/RGR_leaf_no_data.xlsx", 
             sheet = "14_leaf_no",
             na = "NA") #load 14dpi leaf number data

leaf_no_14 <- na.omit(leaf_no_14) #remove NAs

leaf_no_14 <- as.data.frame(leaf_no_14) #convert from tibble to df (otherwise shapiro wilk doesnt work)

#order by treatment, to match PR1 graph
leaf_no_14$treatment <- ordered(leaf_no_14$treatment,
                                             levels = c("Mock Water",
                                                        "Mock BABA",
                                                        "Mock JA", 
                                                        "Mock SA"))
#Set up colour/shape codes
treatment_cols <- c("Mock Water" = "blue", 
                    "Mock BABA" = "#117733", 
                    "Mock JA" = "#AA4499", 
                    "Mock SA" = "#DDCC77")


#Plot graphs########
p1 <- ggplot(data = leaf_no_14, 
       mapping = aes(x = treatment, y = leaf_number, fill = treatment))+
  geom_boxplot()+
  scale_fill_manual(values = treatment_cols)+ #colour code by treatment
  geom_jitter(size = 10)+ #show data points
  xlab(NULL)+ #remove x axis label
  ylab(NULL)+ #remove y axis label
  guides(fill = "none", pch = "none")+
  scale_y_continuous(limits = c(0, 25))+
  theme(panel.grid.major = element_blank(), #remove major grid lines
        panel.grid.minor = element_blank(), #remove minor grid lines
        panel.background = element_rect(fill = "#f5f5f5"), #light grey background to match other graphs
        axis.line = element_line(), #add lines to axis
        axis.text.x = element_blank(), #remove x axis text, will add later
        axis.text.y = element_text(size = 40)) #change y-axis label size

p1

ggsave(filename = "final_graphs/leaf_number_box_14dpi.pdf", plot = p1, device = pdf, width = 33, height = 20)

#Stats###########
leaf_no_model_14 <- lm(leaf_number ~ treatment, data = leaf_no_14)

#check quality of model with olsrr package
ols_plot_resid_fit(leaf_no_model_14)#residual v fitted graph, looks pretty homogeneous
ols_plot_resid_qq(leaf_no_model_14) #qq plot = looks quite normal
ols_plot_resid_hist(leaf_no_model_14) #residuals histogram = looks quite normal

#test normality of each treatment group with shapiro wilk test
shapiro.test(subset(leaf_no_14, leaf_no_14$treatment == "Mock Water")[,2]) #p-value = 0.2804, normal
shapiro.test(subset(leaf_no_14, leaf_no_14$treatment == "Mock SA")[,2]) #p-value = 0.4046, normal
shapiro.test(subset(leaf_no_14, leaf_no_14$treatment == "Mock JA")[,2]) #p-value = 0.2316, normal
shapiro.test(subset(leaf_no_14, leaf_no_14$treatment == "Mock BABA")[,2]) #p-value = 0.9133, normal

#test for homogeneous variances with levenes test
car::leveneTest(leaf_no_model_14) #p = 0.7698, this is >0.05 so variances are homogeneous

#overall data looks to fit anova assumptions
anova(leaf_no_model_14) #p = 0.7754, so anova NOT significant

#double check with KW
kruskal.test(leaf_number ~ treatment, data = leaf_no_14) #p-value = 0.7277 so KW NOT significant
