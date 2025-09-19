library(tidyverse)

degs <- read.csv("data/summary_data_full_fc1.csv")

degs <- degs %>%
  pivot_longer(cols = c(All_significant_DEGs, Upregulated_DEGs, Downregulated_DEGs),
               names_to = "Category", values_to = "Count") #convert to long format for ease of plotting

#set Conditions as factor and set order to appear on graph
degs$Condition <- factor(degs$Condition, levels = c(
  "Mock BABA",
  "Mock JA",
  "Mock SA",
  "PM BABA",
  "PM JA",
  "PM SA"
))

# set all, up, down as factor and set order to appear on graph
degs$Category <- factor(degs$Category, 
                           levels = c("All_significant_DEGs", 
                                      "Upregulated_DEGs", 
                                      "Downregulated_DEGs"))

#0dpi
degs_0dpi <- degs %>%
  filter(Timepoint == "0dpi")

p <- ggplot(degs_0dpi, aes(x = Condition, y = Count, fill = Category)) + 
  geom_bar(stat = "identity", position = position_dodge()) + #bar plot of DEGs split by all, up, down
  geom_text(aes(label = Count), 
            position = position_dodge(width = 0.9), #add numbers as labels to bars
            vjust = -0.3, size = 12) +
  labs(y = "Number of DEGs") +
  scale_fill_manual(values = c(
    "All_significant_DEGs" = "#DBF2D0",
    "Upregulated_DEGs" = "#D3DB97", # set custom colours
    "Downregulated_DEGs" = "#C1E8F5"
  )) + 
  theme(panel.grid.major = element_blank(), #remove major grid lines
        panel.grid.minor = element_blank(), #remove minor grid lines
        panel.background = element_rect(fill = "#f5f5f5"), #light grey background to match other graphs
        axis.line = element_line(), #add lines to axes
        axis.text.x = element_text(size = 60, angle = 45, hjust = 1), # Change x-axis label size and position
        axis.text.y = element_text(size = 60), #change y-axis label size
        legend.position = "none",     # remove legend
        axis.title.x = element_blank(), # remove x-axis title
        axis.title.y = element_text(size = 60)) #change y-axis title size
    
ggsave(filename = "graphs/0dpi_DEGs.pdf", plot = p, device = pdf, width = 33, height = 20)

#1dpi
degs_1dpi <- degs %>%
  filter(Timepoint == "1dpi")

p <- ggplot(degs_1dpi, aes(x = Condition, y = Count, fill = Category)) + 
  geom_bar(stat = "identity", position = position_dodge()) + #bar plot of DEGs split by all, up, down
  geom_text(aes(label = Count), 
            position = position_dodge(width = 0.9), #add numbers as labels to bars
            vjust = -0.3, size = 12) +
  scale_fill_manual(values = c(
    "All_significant_DEGs" = "#DBF2D0",
    "Upregulated_DEGs" = "#D3DB97", # set custom colours
    "Downregulated_DEGs" = "#C1E8F5"
  )) + 
  labs(y = "Number of DEGs") +
  theme(panel.grid.major = element_blank(), #remove major grid lines
        panel.grid.minor = element_blank(), #remove minor grid lines
        panel.background = element_rect(fill = "#f5f5f5"), #light grey background to match other graphs
        axis.line = element_line(), #add lines to axes
        axis.text.x = element_text(size = 60, angle = 45, hjust = 1), # Change x-axis label size and position
        axis.text.y = element_text(size = 60), #change y-axis label size
        legend.position = "none",     # remove legend
        axis.title.x = element_blank(), # remove x-axis title
        axis.title.y = element_text(size = 60)) #change y-axis title size

ggsave(filename = "graphs/1dpi_DEGs.pdf", plot = p, device = pdf, width = 33, height = 20)

#2dpi
degs_2dpi <- degs %>%
  filter(Timepoint == "2dpi")

p <- ggplot(degs_2dpi, aes(x = Condition, y = Count, fill = Category)) + 
  geom_bar(stat = "identity", position = position_dodge()) + #bar plot of DEGs split by all, up, down
  geom_text(aes(label = Count), 
            position = position_dodge(width = 0.9), #add numbers as labels to bars
            vjust = -0.3, size = 12) +
  scale_fill_manual(values = c(
    "All_significant_DEGs" = "#DBF2D0",
    "Upregulated_DEGs" = "#D3DB97", # set custom colours
    "Downregulated_DEGs" = "#C1E8F5"
  )) + 
  labs(y = "Number of DEGs") +
  theme(panel.grid.major = element_blank(), #remove major grid lines
        panel.grid.minor = element_blank(), #remove minor grid lines
        panel.background = element_rect(fill = "#f5f5f5"), #light grey background to match other graphs
        axis.line = element_line(), #add lines to axes
        axis.text.x = element_text(size = 60, angle = 45, hjust = 1), # Change x-axis label size and position
        axis.text.y = element_text(size = 60), #change y-axis label size
        legend.position = "none",     # remove legend
        axis.title.x = element_blank(), # remove x-axis title
        axis.title.y = element_text(size = 60)) #change y-axis title size

ggsave(filename = "graphs/2dpi_DEGs.pdf", plot = p, device = pdf, width = 33, height = 20)
