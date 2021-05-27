###########################################
## A script to make figures showing node ##
## counts vs cvR2 based on l1 penalty    ##
###########################################

#import libraries
library(ggplot2)
library(dplyr)

#create the figures
font_size <- 15
line_size <- .75

#import results


#create figure
node_counts %>%
  ggplot(aes(cpu,cpu_r2)) + 
  geom_line() + 
  labs(title = "CPU", x = "Node Count", y = expression(paste("CV-", R^2))) +
  coord_cartesian(xlim = c(0, 45000), ylim = c(0, .9)) + 
  theme_bw() + 
  theme(plot.title = element_text(size = font_size),
        text = element_text(size = font_size),
        axis.text = element_text(size = font_size)) + 
  geom_hline(yintercept = .84, linetype = "dotted", color = "blue", size = line_size) + 
  geom_hline(yintercept = .58, linetype = "dashed", color = "green", size = line_size) + 
  geom_hline(yintercept = .87, linetype = "solid", color = "red", size = line_size)

#output figure
