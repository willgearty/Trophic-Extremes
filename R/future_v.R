# title: "future_v.R"
# author: Robert S. C. Cooke (03rcooke@gmail.com)
# date: 12/06/2020

#### Set up ####

# dplyr: data manipulation
# plyr: data manipulation
# ggplot2: plotting
# cowplot: plotting

if(!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, plyr, ggplot2, cowplot)

# set plotting theme
theme_set(theme_cowplot())

#### Analysis ####

# future V mammals #

Atraits <- readRDS("..data/v_trait.rds")

#[1] PlantSeed: Plant and Seeds, [2] FruiNect: Fruits and Nectar, [3] VertFishScav: Vertebrates and Fish and Carrion, [4] Invertebrate: Invertebrates, [5] Omnivore

# collapse categories to Phylacine before calculating omnivore
mam_pres <- Atraits %>% 
  dplyr::mutate(diet_plant = diet_fruit + diet_nect + diet_seed + diet_planto) %>% 
  dplyr::mutate(diet_vert = diet_vend + diet_vect + diet_vfish + diet_vunk + diet_scav) %>%   
  dplyr::filter(realm == "terrestrial" & class == "Mammalia") %>% 
  dplyr::filter(!is.na(diet_plant)) %>% 
  dplyr::mutate(body_mass_median = log(body_mass_median)) %>% 
  dplyr::select(binomial, body_mass_median, diet_plant, diet_vert, diet_inv)

phylacine <- readRDS("..data/v_phylacine_trait.rds")

phylacine <- phylacine %>%
  dplyr::mutate(body_mass_median = log(mass)) %>% 
  dplyr::select(binomial, body_mass_median, diet_plant, diet_vert, diet_inv)

# combine phylacine mammals and present mammals
# i.e., mammals present at Late Pleistocene
pleis <- dplyr::bind_rows(mam_pres, phylacine)

pleis <- pleis %>%
  # omnivores
  dplyr::mutate(omnivore = apply(dplyr::select(pleis, diet_plant, diet_vert, diet_inv), 1, max)) %>%
  dplyr::mutate(diet_5cat = ifelse(omnivore <=50, 4, max.col(dplyr::select(., diet_plant:diet_inv))))

pleis$diet_5cat = plyr::mapvalues(pleis$diet_5cat, 
                                  from = c("1", "2", "3", "4"), 
                                  to = c("Herbivore", "Carnivore", "Invertivore", "Omnivore"))

pleis <- dplyr::mutate(pleis, diet_5cat = factor(diet_5cat, levels = c("Herbivore", "Omnivore", "Invertivore", "Carnivore")))

# boxplot data
# have to do it manually as future is averaged across 10,000 runs
pleis_df <- pleis %>% 
  dplyr::group_by(diet_5cat) %>% 
  dplyr::summarize(y0 = min(body_mass_median),
                   y25 = quantile(body_mass_median, 0.25),
                   y50 = median(body_mass_median),
                   y75 = quantile(body_mass_median, 0.75),
                   y100 = max(body_mass_median)) %>% 
  dplyr::mutate(time = "Pleistocene")

# present mammals
pres <- pleis %>%
  dplyr::filter(binomial %in% mam_pres$binomial) %>% 
  # additional EW
  dplyr::filter(!binomial %in% c("Elaphurus davidianus", "Oryx dammah"))

# boxplot data
pres_df <- pres %>% 
  dplyr::group_by(diet_5cat) %>% 
  dplyr::summarize(y0 = min(body_mass_median),
                   y25 = quantile(body_mass_median, 0.25),
                   y50 = median(body_mass_median),
                   y75 = quantile(body_mass_median, 0.75),
                   y100 = max(body_mass_median)) %>% 
  dplyr::mutate(time = "Present")

# prep for extinction models

# gl <- read.csv("eco_dist_paper_traits.csv", header = TRUE, stringsAsFactors = FALSE) %>% 
#   dplyr::rename(binomial = ï..binomial) %>% 
#   dplyr::select(-class)
# 
# gl_pres <- dplyr::left_join(mam_pres, gl) %>% 
#   dplyr::mutate_at(vars(contains("GL")), ~10^.)
#
# gl_tobi <- read.delim("mammals_gl.txt", header = FALSE) %>% 
#   dplyr::full_join(dplyr::select(mam_pres, binomial), by = c("V1" = "binomial")) %>% 
#   dplyr::filter(V1 %in% mam_pres$binomial)
# 
# write.table(gl_tobi, file = "mam_pres_gl.txt", sep = "\t", col.names = FALSE, row.names = FALSE)

#### run code in Anaconda ####

#https://github.com/tobiashofmann88/iucn_extinction_simulator

# import output from anaconda
ext_mam <- read.delim("..data/te_all_species.txt", header = FALSE, stringsAsFactors = FALSE) 

# names of extinct mammals per run
ext_names <- lapply(2:ncol(ext_mam), function(x) {
  out1 <- na.omit(cbind(ext_mam[,1], ext_mam[,x]))
  out2 <- out1[,1]
})

ext_sim <- lapply(1:length(ext_names), function(x) {
  # filter out extinct mammals per run
  out <- dplyr::filter(pres, !binomial %in% ext_names[[x]]) %>% 
    dplyr::group_by(diet_5cat) %>% 
    # boxplot data per run
    dplyr::summarize(y0 = min(body_mass_median),
                     y25 = quantile(body_mass_median, 0.25),
                     y50 = median(body_mass_median),
                     y75 = quantile(body_mass_median, 0.75),
                     y100 = max(body_mass_median)) %>% 
    dplyr::mutate(time = paste0("sim_", x))
})

# summarise across runs
ext_sim_df <- dplyr::bind_rows(ext_sim) %>% 
  dplyr::group_by(diet_5cat) %>% 
  dplyr::summarize_at(vars(y0, y25, y50, y75, y100), mean) %>% 
  dplyr::mutate(time = "Future")

# combine datasets
tem_df <- dplyr::bind_rows(pleis_df, pres_df, ext_sim_df) %>% 
  dplyr::mutate(time = factor(time, levels = c("Pleistocene", "Present", "Future")))

#### Plotting ####

fut_v <- ggplot(tem_df, aes(x = diet_5cat, fill = time)) +
  geom_boxplot(aes(ymin = y0, lower = y25, middle = y50, upper = y75, ymax = y100), stat = "identity") +
  scale_fill_manual(values = c("#CC79A7", "#D55E00", "#0072B2")) +
  labs(x = "", y = "ln Mass (g)") +
  theme(legend.title = element_blank())

#cowplot::save_plot("future_v.png", fut_v, base_height = 10, base_width = 8, dpi = 300)