###########################################
## A script to make figures showing node ##
## counts vs cvR2 based on l1 penalty    ##
##                                      ##
## REQUIRES: Assignment of df_name      ##
###########################################


'
This script currenlty makes a plot of terminal node count vs cvr2.
If you want a plot of unique terminal node counts vs cvr2, change tn_count
to utn_count in the ggplot command below.

You will need to set the df_name either manually or by running the bash script
l1_fig.sh with a dataset name.

You may need to play around with the cartesian_coordinates(xlim, ylim)
(commented out in the code below), the font size and the figure width and height.
'

#import libraries
library(ggplot2, quietly = TRUE)
library(dplyr, quietly = TRUE)

#get the dataset name from the command line
df_name <- commandArgs(trailingOnly = TRUE)

#create the figures
font_size <- 9

#import results
node_counts <- read.csv(paste0('./', df_name, '_l1_eval.csv'))

#create figure
l1_plot <- node_counts %>%
  ggplot(aes(tn_counts,r2)) + 
  geom_line() + 
  labs(title = df_name, x = "Node Count", y = expression(paste("CV-", R^2))) +
  #coord_cartesian(xlim = c(0, 45000), ylim = c(0, .9)) + 
  theme_bw() + 
  theme(plot.title = element_text(size = font_size),
        text = element_text(size = font_size),
        axis.text = element_text(size = font_size)) 

#save figure
fig_width <- 5
fig_height <- 3
ggsave(filename = paste0("./", df_name, '_l1_performance.png'),
       plot = l1_plot, 
       width = fig_width, height = fig_height, 
       units = "in")
