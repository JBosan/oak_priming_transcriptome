library(tidyverse) #for data processing
library(ggbreak) #for axis breaks

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
PR1_bar_data <- 
  PR1_data %>%
  group_by(treatment) %>% #group by treatment i.e. mock/PM BABA/JA/SA/Water
  summarise(mean_fold_change = mean(fold_change), #mean fold_change for each treatment
            SEM = sd(fold_change)/sqrt(length(treatment))) %>% #SEM for each treatment
  mutate(infection = c(rep("mock", 4), rep("PM", 4))) #add infection status column

#no breaks
p1 <- 
  ggplot(data = PR1_bar_data, 
             mapping = aes(x = treatment, y = mean_fold_change, fill = treatment))+
  geom_col()+
  geom_errorbar(mapping = aes(ymax = mean_fold_change+SEM, ymin = mean_fold_change-SEM))+
  facet_grid(~ infection, scales = "free_x")+ #split mock and PM on x axis
  scale_y_continuous(limits = c(0, 2700))+ #set axis limits
  scale_fill_manual(values = treatment_cols)+ #colour code
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

#3 breaks
p2 <- 
  ggplot(data = PR1_bar_data, 
         mapping = aes(x = treatment, y = mean_fold_change, fill = treatment))+
  geom_col()+
  geom_errorbar(mapping = aes(ymax = mean_fold_change+SEM, ymin = mean_fold_change-SEM))+
  facet_grid(~ infection, scales = "free_x")+ #split mock and PM on x axis
  scale_y_break(c(1250, 2500), scales = 0.5, space = 0.5, ticklabels = seq(2500, 2700, 100))+ #break for mock BABA
  scale_y_break(c(200, 250), scales = 0.5, space = 0.5, ticklabels = seq(250, 1250, 500))+ #break for mock JA
  scale_y_break(c(30, 40), scales = 0.5, space = 0.5, ticklabels = seq(50, 200, 75))+ #break for PM BABA and PM JA
  scale_y_continuous(limits = c(0, 2700))+ #set axis limits
  scale_fill_manual(values = treatment_cols)+ #colour code
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

ggsave(filename = "final_graphs/PR1_bar_no_break.pdf", plot = p1, device = pdf, width = 33, height = 20)
ggsave(filename = "final_graphs/PR1_bar_3_break.pdf", plot = p2, device = pdf, width = 33, height = 20)
