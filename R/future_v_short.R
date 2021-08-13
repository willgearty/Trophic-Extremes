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
yr_traj_c <- readRDS("../data/yr_traj_c.rds")

# load: yr_0
yr_0 <- readRDS("../data/yr_0.rds")

# calculate percent change in median mass per trophic guild
chng <- yr_traj_c %>% 
  dplyr::left_join(dplyr::select(yr_0, diet_5cat, med), by = "diet_5cat") %>% 
  # convert back to raw body mass
  dplyr::mutate_at(vars(mean, low, upp, med), exp) %>% 
  # percent change
  dplyr::mutate(perc_med = ((mean - med)/med) * 100,
                perc_low = ((low - med)/med) * 100,
                perc_upp = ((upp - med)/med) * 100)

# Figure 5A ####
# plot percent change in mass per trophic guild
traj <- ggplot(chng, aes(x = yr, y = perc_med, colour = diet_5cat)) +
  facet_wrap(vars(diet_5cat), nrow = 1) +
  geom_hline(aes(yintercept = 0, colour = diet_5cat), linetype = "dashed", lwd = 1) +
  geom_path(lwd = 1) +
  geom_ribbon(aes(ymin = perc_low, ymax = perc_upp, fill = diet_5cat), alpha = 0.4, colour = NA) +
  scale_colour_manual(values = c("#359B73", "#2271B2", "#FFAC3B", "#CD022D")) +
  scale_fill_manual(values = c("#359B73", "#2271B2", "#FFAC3B", "#CD022D")) +
  labs(x = "Years in future", y = "Percent change in mass") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(limits = c(-21, 21), expand = c(0,0)) +
  theme(legend.title = element_blank(),
        legend.position = "none",
        panel.spacing = unit(1.5, "lines"))

# add small space at right edge to prevent 500 year being cut off
traj_buff <- cowplot::plot_grid(traj, NULL, nrow = 1, rel_widths = c(1, 0.015))

# save plot
cowplot::save_plot("../figures/traj.pdf", traj_buff, base_height = 4, base_width = 10, dpi = 600)

