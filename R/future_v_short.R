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

#### Median ####

# load: yr_traj_c
yr_traj_c <- readRDS("../data/yr_traj_c.rds")

# load: yr_0
yr_0 <- readRDS("../data/yr_0.rds")

# calculate percent change in median mass per trophic guild
chng <- yr_traj_c %>% 
  dplyr::left_join(dplyr::select(yr_0, diet_5cat, med), by = "diet_5cat") %>%
  # convert back to raw body mass
  dplyr::mutate_at(vars(mean, low_ci, upp_ci, low_ci_50, upp_ci_50, med), exp) %>% 
  # percent change
  dplyr::mutate(perc_med = ((mean - med)/med) * 100,
                perc_low = ((low_ci - med)/med) * 100,
                perc_upp = ((upp_ci - med)/med) * 100,
                perc_low_50 = ((low_ci_50 - med)/med) * 100,
                perc_upp_50 = ((upp_ci_50 - med)/med) * 100)

#### 90th quantile ####

# load: yr_traj_c_90
yr_traj_c_90 <- readRDS("..data/yr_traj_c_90.rds")

# load: yr_0_90
yr_0_90 <- readRDS("..data/yr_0_90.rds")

chng_90 <- yr_traj_c_90 %>% 
  dplyr::left_join(dplyr::select(yr_0_90, diet_5cat, quant_90), by = "diet_5cat") %>% 
  # convert back to raw body mass
  dplyr::mutate_at(vars(mean, low_ci, upp_ci, low_ci_50, upp_ci_50, quant_90), exp) %>% 
  # percent change
  dplyr::mutate(perc_med = ((mean - quant_90)/quant_90) * 100,
                perc_low = ((low_ci - quant_90)/quant_90) * 100,
                perc_upp = ((upp_ci - quant_90)/quant_90) * 100,
                perc_low_50 = ((low_ci_50 - quant_90)/quant_90) * 100,
                perc_upp_50 = ((upp_ci_50 - quant_90)/quant_90) * 100)


# plot limit (max change across median and 90 quantile)
max_val <- max(c(abs(min(chng_90$perc_low)), abs(max(chng_90$perc_upp)))) * 1.01

# Figure 5A ####
# plot percent change in mass per trophic guild
traj <- ggplot(chng, aes(x = yr, y = perc_med, colour = diet_5cat)) +
  facet_wrap(vars(diet_5cat), nrow = 1) +
  geom_hline(aes(yintercept = 0, colour = diet_5cat), linetype = "dashed", lwd = 1) +
  geom_path(lwd = 1.5) +
  geom_ribbon(aes(ymin = perc_low, ymax = perc_upp, fill = diet_5cat), alpha = 0.3, colour = NA) +
  geom_ribbon(aes(ymin = perc_low_50, ymax = perc_upp_50, fill = diet_5cat), alpha = 0.6, colour = NA) +
  scale_x_continuous(name = "Years in future", expand = c(0, 0)) +
  scale_y_continuous(name = "Percent change in median mass", limits = c(-max_val, max_val)) +
  scale_colour_manual(values = c("#359B73", "#7d22b2", "#FFAC3B", "#ad0025")) +
  scale_fill_manual(values = c("#359B73", "#7d22b2", "#FFAC3B", "#ad0025")) +
  theme(legend.title = element_blank(),
        legend.position = "none",
        panel.spacing = unit(1.5, "lines"))

# add small space at right edge to prevent 500 year being cut off
traj_buff <- cowplot::plot_grid(traj, NULL, nrow = 1, rel_widths = c(1, 0.015))

# save plot
cowplot::save_plot("../figures/fig_5A.pdf", traj_buff, base_height = 4, base_width = 8.99, dpi = 600)

## Fig 5B

traj_90 <- ggplot(chng_90, aes(x = yr, y = perc_med, colour = diet_5cat)) +
  facet_wrap(vars(diet_5cat), nrow = 1) +
  geom_hline(aes(yintercept = 0, colour = diet_5cat), linetype = "dashed", lwd = 1) +
  geom_path(lwd = 1.5) +
  geom_ribbon(aes(ymin = perc_low, ymax = perc_upp, fill = diet_5cat), alpha = 0.3, colour = NA) +
  geom_ribbon(aes(ymin = perc_low_50, ymax = perc_upp_50, fill = diet_5cat), alpha = 0.6, colour = NA) +
  scale_x_continuous(name = "Years in future", expand = c(0, 0)) +
  scale_y_continuous(name = expression(paste("Percent change in  ", 90^'th', " quantile mass")), limits = c(-max_val, max_val)) +
  scale_colour_manual(values = c("#359B73", "#7d22b2", "#FFAC3B", "#ad0025")) +
  scale_fill_manual(values = c("#359B73", "#7d22b2", "#FFAC3B", "#ad0025")) +
  theme(legend.title = element_blank(),
        legend.position = "none",
        panel.spacing = unit(1.5, "lines"),
        strip.background = element_rect(colour = "white", fill = "white"),
        strip.text = element_text(colour = "white"))

# add small space at right edge to prevent 500 year being cut off
traj_buff_90 <- cowplot::plot_grid(traj_90, NULL, nrow = 1, rel_widths = c(1, 0.015))

# save plot
cowplot::save_plot("../figures/fig_5B.pdf", traj_buff_90, base_height = 4, base_width = 9.1, dpi = 600)


