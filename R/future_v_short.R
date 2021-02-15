# title: "future_v_short.R"
# author: Robert S. C. Cooke (03rcooke@gmail.com)
# date: 15/02/2021

#### Set up ####

# dplyr: data manipulation
# ggplot2: plotting
# cowplot: plotting

if(!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, ggplot2, cowplot)

# set plotting theme
theme_set(theme_cowplot())

# load: yr_traj_c
yr_traj_c <- readRDS("yr_traj_c.rds")

# load: yr_0
yr_0 <- readRDS("yr_0.rds")

traj <- ggplot(yr_traj_c, aes(x = yr, y = mean, colour = diet_5cat)) +
  facet_wrap(vars(diet_5cat), scales = "free") +
  geom_segment(data = yr_0, aes(x = 0, xend = 500, y = med, yend = med, colour = diet_5cat), lwd = 1, lty = 2, alpha = 0.5) +
  geom_path(lwd = 1) +
  geom_ribbon(aes(ymin = low, ymax = upp, fill = diet_5cat), alpha = 0.2, colour = NA) +
  scale_colour_manual(values = c("#359B73", "#2271B2", "#FFAC3B", "#CD022D")) +
  scale_fill_manual(values = c("#359B73", "#2271B2", "#FFAC3B", "#CD022D")) +
  labs(x = "Years in future", y = "Median ln Mass (g)") +
  theme(legend.title = element_blank(),
        legend.position = "none")

cowplot::save_plot("traj.pdf", traj, base_height = 4, base_width = 7, dpi = 300)

