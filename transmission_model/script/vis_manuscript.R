#create a combined plot of the estimated R0 and results from the transmission model

#load animal level prevalence figure for all regions
load(paste('./transmission_model/RData_files/fig_prev_animals', '.RData', sep=""))

#load farm level prevalence figure for all regions
load(paste('./transmission_model/RData_files/fig_prev_farms', '.RData', sep=""))

#load figure of estimated R0 from farms
load(paste('./transmission_model/RData_files/vis_R0_by_herd', '.RData', sep=""))

#load map of Ethiopia with study sites indicated
#Map with first-level Administrative Divisions obtained from
#https://earthworks.stanford.edu/catalog/stanford-gw295bx8934
load(paste('./transmission_model/RData_files/ethiopia', '.RData', sep=""))

#combine the figure of study sites and R0 by herd
study_farms<-cowplot::plot_grid(
  ethiopia,
  NULL,
  vis_R0_by_herd,
  NULL,
  rel_widths = c(1.1, 0.00, 1.2, 0.0), 
  rel_heights = c(1.1, 0.00, 0.7, 0.0),
  labels = c("A", "", "B", ""),
  label_size=40,
  label_x=-0.006, 
  label_y=1.01, 
  label_fontfamily="sans",
  ncol=4,
  nrow=1)

# extract the legend from one of the prevalence plots to assign single legend
legend_prev <- cowplot::get_legend(
  prev_farms + 
  theme(legend.box.margin = margin(t=15, r=0.5, b=5, l=105)))

#combine prevalence figures
prev<-cowplot::plot_grid(
  prev_animals + 
    theme(legend.position = "none",
          axis.title.x = element_blank(), axis.text.x = element_blank()), 
  prev_farms +
    theme(legend.position = "none"), 
  rel_heights = c(0.86, 1), labels = c("C", ""),
  label_size=40,
  label_x=-0.006, 
  label_y=1.01,
  label_fontfamily="sans",
  nrow=2)
  
# add the legend to the plot
prev2<-cowplot::plot_grid(prev, legend_prev, 
                          rel_widths = c(5, 0.5),
                          rel_heights = c(5, 0.2),
                          nrow=2)

#combine  figures
cowplot::plot_grid(
  study_farms,
  NULL,
  prev2,
  rel_heights = c(0.6, 0.05, 1), 
  label_size=40,
  label_x=-0.006, 
  label_y=1.01, 
  label_fontfamily="sans",
  nrow=3)+
  theme(plot.background = element_rect(fill="white", color = NA))


ggsave(paste("./Manuscript_figures/Fig_3", '.png', sep=""), width=19, height=27)


#supplementary figure
#combine prevalence figures
prev<-cowplot::plot_grid(
  prev_animals + 
    theme(legend.position = "none"), 
  NULL,
  prev_farms +
    theme(legend.position = "none"),
  rel_heights = c(0.86, 1), 
  labels = c("", ""),
  label_size=40,
  label_x=-0.006, 
  label_y=1.01, 
  label_fontfamily="sans",
  nrow=2)

# extract the legend from one of the prevalence plots to assign single legend
legend_prev <- cowplot::get_legend(
  prev_farms + 
    theme(legend.box.margin = margin(t=15, r=0.5, b=5, l=105)) 
)

prev2<-cowplot::plot_grid(prev, legend_prev, 
                          rel_widths = c(5, 1),
                          rel_heights = c(5, 0.2),
                          nrow=2)

ggsave(paste("./Manuscript_figures/Fig_S10", '.png', sep=""), width=18, height=17)

