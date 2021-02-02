# title: "v_plots_rob.R"
# author: Rob Cooke (03rcooke@gmail.com)

#### Set up ####

if(!require("pacman")) install.packages("pacman")
pacman::p_load(nlme, dplyr, tidyr, ggplot2, stringr, gtable, cowplot, lemon)
pacman::p_load_gh("willgearty/deeptime")


# Will's colour scheme
colors4 <- setNames(c("#359B73", "#2271B2", "#FFAC3B", "#CD022D"), c("herbivore", "omnivore", "insectivore", "carnivore"))

##### Data ####

# Prep biome and trait data
biome_traits <- 
  readRDS("../data/amanda_biome_traits.rds") %>% 
  # Make sure that biome ids are numeric
  mutate(biome = as.numeric(biome))

# Create lookup for biome ids
biome_key <- tibble(biome = 1:14, biome_name = c("Tropical & Subtropical Moist Broadleaf Forests", "Tropical & Subtropical Dry Broadleaf Forests", "Tropical & Subtropical Coniferous Forests", "Temperate Broadleaf & Mixed Forests", "Temperate Conifer Forests", "Boreal Forests/Taiga", "Tropical & Subtropical Grasslands, Savannas & Shrublands", "Temperate Grasslands, Savannas & Shrublands", "Flooded Grasslands & Savannas", "Montane Grasslands & Shrublands", "Tundra", "Mediterranean Forests, Woodlands & Scrub", "Deserts & Xeric Shrublands", "Mangroves"), 
                    biome_label = c("Tropical & Subtropical\nMoist Broadleaf Forests", "Tropical & Subtropical\nDry Broadleaf Forests", "Tropical & Subtropical\nConiferous Forests", "Temperate Broadleaf & Mixed Forests", "Temperate Conifer Forests", "Boreal Forests/Taiga", "Tropical & Subtropical \nGrasslands, Savannas & Shrublands", "Temperate\nGrasslands, Savannas & Shrublands", "Flooded Grasslands & Savannas", "Montane Grasslands & Shrublands", "Tundra", "Mediterranean\nForests, Woodlands & Scrub", "Deserts & Xeric Shrublands", "Mangroves"))

# Master biome data
biomes <- 
  left_join(biome_traits, biome_key) %>% 
  # We only need the biome information for now
  select(binomial, biome, biome_name, biome_label)

# Prepare diet labels
diet_cat_key <- tibble(diet_5cat = c(1, 2, 3, 4), 
                       diet_name = c('herbivore', 'omnivore', 'insectivore', 'carnivore'), 
                       diet_abbr = c('H', 'O', 'I', 'C'))

v_traits_orig <- 
  readRDS("../data/v_trait.rds") %>% 
  as_tibble()  %>% # easier to manage
  dplyr::mutate(diet_plant = diet_fruit + diet_nect + diet_seed + diet_planto) %>% 
  dplyr::mutate(diet_vert = diet_vend + diet_vect + diet_vfish + diet_vunk + diet_scav) %>% 
  select(binomial, class, realm, diet_plant, diet_inv, diet_vert, body_mass_median) %>%
  # to match Will's plot
  mutate(ln_body_mass_median = log(body_mass_median))

v_traits <- v_traits_orig %>% 
  # omnivores
  dplyr::mutate(omnivore = apply(dplyr::select(v_traits_orig, diet_plant, diet_inv, diet_vert), 1, max)) %>%
  dplyr::mutate(diet_5cat = ifelse(omnivore <=50, 4, max.col(dplyr::select(., diet_plant:diet_vert))))

# Master V data
v_data <- 
  full_join(biomes, v_traits, by = c("binomial")) %>%
  # Relabel diet categories to match diet_cat_key
  mutate(diet_5cat = case_when(diet_5cat == 1 ~ 1, 
                               diet_5cat == 2 ~ 3, 
                               diet_5cat == 3 ~ 4, 
                               diet_5cat == 4 ~ 2)) %>% 
  # Add diet labels
  left_join(., diet_cat_key) %>% 
  mutate(diet_name = factor(diet_name, levels = diet_cat_key$diet_name)) %>% 
  # Remove species missing diet category
  drop_na(diet_name) %>% 
  # Factor biome labels
  mutate(biome_label = factor(biome_label, levels = c("Tundra", "Boreal Forests/Taiga", "Temperate Broadleaf & Mixed Forests", "Temperate Conifer Forests", "Temperate\nGrasslands, Savannas & Shrublands", "Mediterranean\nForests, Woodlands & Scrub", "Deserts & Xeric Shrublands", "Montane Grasslands & Shrublands", "Tropical & Subtropical\nConiferous Forests", "Tropical & Subtropical\nDry Broadleaf Forests", "Tropical & Subtropical \nGrasslands, Savannas & Shrublands", "Tropical & Subtropical\nMoist Broadleaf Forests", "Flooded Grasslands & Savannas", "Mangroves")))

terr_mammals <- filter(v_data, realm == "terrestrial", class == "Mammalia", !is.na(biome_label))
terr_birds   <- filter(v_data, realm == "terrestrial", class == "Aves", !is.na(biome_label))

#### Figures ####

## Mammals across biomes ##
# Figure 3 #

boxplot_theme <- function(base_size = 16) {
  theme_classic(base_size = base_size) + 
    theme(axis.title.y = element_text(angle = 90, vjust = 2),
          axis.title.x = element_text(vjust = -5),
          axis.text.x = element_blank(),
          axis.text = element_text(color = "black"),
          axis.ticks.x =  element_blank(),
          panel.border = element_rect(color = "black", fill = NA),
          axis.line = element_blank(),
          legend.title = element_blank(),
          legend.direction = "vertical",
          legend.text = element_text(size = base_size*1.5),
          strip.background = element_rect(fill = "grey90", colour = "grey90"),
          strip.text = element_text(size = base_size),
          plot.margin = unit(c(10, 5, 10, 5), "mm")
    )
}

shift_legend2 <- function(p) {
  # ...
  # to grob
  gp <- ggplotGrob(p)
  facet.panels <- grep("^panel", gp[["layout"]][["name"]])
  empty.facet.panels <- sapply(facet.panels, function(i) "zeroGrob" %in% class(gp[["grobs"]][[i]]))
  empty.facet.panels <- facet.panels[empty.facet.panels]
  
  # establish name of empty panels
  empty.facet.panels <- gp[["layout"]][empty.facet.panels, ]
  names <- empty.facet.panels$name
  # example of names:
  #[1] "panel-3-2" "panel-3-3"
  
  # now we just need a simple call to reposition the legend
  reposition_legend(p, 'center', panel=names)
}

# Terrestrial mammal body size ~ diet ~ biome plot
tm_sum <- terr_mammals %>% 
  dplyr::group_by(biome, biome_label, diet_name) %>% 
  dplyr::count(name = "tm_n")

tm <- terr_mammals %>% 
  ggplot(., aes(x = diet_name, y = ln_body_mass_median, group = diet_name, fill = diet_name)) +
  geom_boxplot(coef = 10, color = "black") +
  geom_text(data = tm_sum, aes(y = 16.4, label = tm_n), size = 7, fontface = "bold", show.legend = FALSE) +
  facet_wrap(~ biome_label, ncol = 5, labeller = label_wrap_gen(width=25)) +
  boxplot_theme(base_size = 20) +
  scale_fill_manual(values = c("#359B73", "#2271B2", "#FFAC3B", "#CD022D")) +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = "Mass (kg)",
                     breaks = log(c(1, 10, 100, 1000, 10000, 100000, 1000000, 10000000)),
                     labels = c(1, 10, 100, 1000, 10000, 100000, 1000000, 10000000)/1000) +
  coord_cartesian(ylim = c(1, 17))

tm <- shift_legend2(p = tm)

save_plot('../figures/v_plot_terr_mam_colour.pdf', tm, base_width = 18, base_height = 12)

# Terrestrial bird body size ~ diet ~ biome plot
tb_sum <- terr_birds %>% 
  dplyr::group_by(biome, biome_label, diet_name) %>% 
  dplyr::count(name = "tm_n")

tm <- terr_birds %>% 
  ggplot(., aes(x = diet_name, y = ln_body_mass_median, group = diet_name, fill = diet_name)) +
  geom_boxplot(coef = 10, color = "black") +
  geom_text(data = tb_sum, aes(y = 12.6, label = tm_n), size = 7, fontface = "bold", show.legend = FALSE) +
  facet_wrap(~ biome_label, ncol = 5, labeller = label_wrap_gen(width=25)) +
  boxplot_theme(base_size = 20) +
  scale_fill_manual(values = c("#359B73", "#2271B2", "#FFAC3B", "#CD022D")) +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = "Mass (kg)",
                     breaks = log(c(1, 10, 100, 1000, 10000, 100000, 1000000, 10000000)),
                     labels = c(1, 10, 100, 1000, 10000, 100000, 1000000, 10000000)/1000) +
  coord_cartesian(ylim = c(1, 13.2))

tm <- shift_legend2(p = tm)

save_plot('../figures/v_plot_terr_bird_colour.pdf', tm, base_width = 18, base_height = 12)

## Across taxa ## 
# Figure S4 #

boxplot_theme_tax <- function(base_size = 16) {
  theme_classic(base_size = base_size) + 
    theme(axis.title.y = element_text(angle = 90, vjust = 2),
          axis.title.x = element_text(vjust = -5),
          axis.text = element_text(color = "black"),
          axis.ticks.x =  element_blank(),
          panel.border = element_rect(color = "black", fill = NA),
          axis.line = element_blank(),
          legend.title = element_blank(),
          legend.direction = "horizontal",
          legend.text = element_text(size = base_size*1.5),
          strip.background = element_rect(fill = "grey90", colour = "grey90"),
          strip.text = element_text(size = base_size)
    )
}

# birds (non-marine)

birds <- filter(v_data, class == "Aves", realm == "terrestrial") %>% 
  dplyr::distinct(binomial, .keep_all = TRUE) %>% 
  dplyr::mutate(tax = "Birds (non-marine)")

br_sum <- birds %>% 
  dplyr::group_by(tax, diet_name) %>% 
  dplyr::count(name = "br_n")

br <- birds %>% 
  ggplot(., aes(x = diet_name, y = ln_body_mass_median, group = diet_name, fill = diet_name)) +
  geom_boxplot(coef = 10, color = "black") +
  geom_text(data = br_sum, aes(y = 12.7, label = br_n), size = 7, fontface = "bold") +
  scale_fill_manual(values = c("#359B73", "#2271B2", "#FFAC3B", "#CD022D"), drop = FALSE) +
  scale_x_discrete(NULL, drop = FALSE) +
  scale_y_continuous(name = "Mass (kg)",
                     breaks = log(c(1, 10, 100, 1000, 10000, 100000, 1000000, 10000000)),
                     labels = c(1, 10, 100, 1000, 10000, 100000, 1000000, 10000000)/1000) +
  coord_cartesian(ylim = c(0, 13.2)) +
  facet_wrap(~ tax) +
  boxplot_theme_tax(base_size = 20) +
  theme(legend.position = "none")

# Marine birds

mbirds <- filter(v_data, class == "Aves", realm == "seabird") %>% 
  dplyr::distinct(binomial, .keep_all = TRUE) %>% 
  dplyr::mutate(tax = "Birds (marine)")

mbr_sum <- mbirds %>% 
  dplyr::group_by(tax, diet_name) %>% 
  dplyr::count(name = "mbr_n")

mbr <- mbirds %>% 
  ggplot(., aes(x = diet_name, y = ln_body_mass_median, group = diet_name, fill = diet_name)) +
  geom_boxplot(coef = 10) +
  geom_text(data = mbr_sum, aes(y = 11.2, label = mbr_n), size = 7, fontface = "bold") +
  scale_fill_manual(values = c("#359B73", "#2271B2", "#FFAC3B", "#CD022D"), drop = FALSE) +
  scale_x_discrete(NULL, drop = FALSE) +
  scale_y_continuous(name = "Mass (kg)",
                     breaks = log(c(1, 10, 100, 1000, 10000, 100000, 1000000, 10000000)),
                     labels = c(1, 10, 100, 1000, 10000, 100000, 1000000, 10000000)/1000) +
  coord_cartesian(ylim = c(0, 11.6)) +
  facet_wrap(~ tax) +
  boxplot_theme_tax(base_size = 20) +
  theme(legend.position = "none")

#terrestrial mammals
terr_mam <- filter(v_data, class == "Mammalia", realm == "terrestrial") %>% 
  dplyr::distinct(binomial, .keep_all = TRUE) %>% 
  dplyr::mutate(tax = "Mammals (non-marine)")

tm_sum <- terr_mam %>% 
  dplyr::group_by(tax, diet_name) %>% 
  dplyr::count(name = "mr_n")

tm <- terr_mam %>% 
  ggplot(., aes(x = diet_name, y = ln_body_mass_median, group = diet_name, fill = diet_name)) +
  geom_boxplot(coef = 10) +
  geom_text(data = tm_sum, aes(y = 16.4, label = mr_n), size = 7, fontface = "bold") +
  scale_fill_manual(values = c("#359B73", "#2271B2", "#FFAC3B", "#CD022D"), drop = FALSE) +
  scale_x_discrete(NULL, drop = FALSE) +
  scale_y_continuous(name = "Mass (kg)",
                     breaks = log(c(1, 10, 100, 1000, 10000, 100000, 1000000, 10000000)),
                     labels = c(1, 10, 100, 1000, 10000, 100000, 1000000, 10000000)/1000) +
  coord_cartesian(ylim = c(0, 17)) +
  facet_wrap(~ tax) +
  boxplot_theme_tax(base_size = 20) +
  theme(legend.position = "none")


# marine mammals
marine_mam <- filter(v_data, class == "Mammalia", realm == "aquatic") %>% 
  dplyr::distinct(binomial, .keep_all = TRUE) %>% 
  dplyr::mutate(tax = "Mammals (marine)")

mr_sum <- marine_mam %>% 
  dplyr::group_by(tax, diet_name) %>% 
  dplyr::count(name = "mr_n")

mr <- marine_mam %>% 
  ggplot(., aes(x = diet_name, y = ln_body_mass_median, group = diet_name, fill = diet_name)) +
  geom_boxplot(coef = 10) +
  geom_text(data = mr_sum, aes(y = 19.9, label = mr_n), size = 7, fontface = "bold") +
  scale_fill_manual(values = c("#359B73", "#2271B2", "#FFAC3B", "#CD022D"), drop = FALSE) +
  scale_x_discrete(NULL, drop = FALSE) +
  scale_y_continuous(name = "Mass (kg)",
                     breaks = log(c(1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000)),
                     labels = c(1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000)/1000) +
  coord_cartesian(ylim = c(0, 20.6)) +
  facet_wrap(~ tax) +
  boxplot_theme_tax(base_size = 20) +
  theme(legend.position = "none")

# amphibians 

tr_amph_orig <- readr::read_csv("../data/AmphiBIO_v1.csv") %>% 
  dplyr::select(Species, Flowers:Vert, Body_mass_g, Body_size_mm) %>% 
  dplyr::mutate(plant = pmin(Flowers, Seeds, Fruits, na.rm = TRUE)) %>%
  dplyr::filter(!is.na(Body_mass_g))

tr_amph <- tr_amph_orig %>% 
  dplyr::mutate(across(c(plant, Arthro, Vert), ~replace(.x, is.na(.x), 0))) %>% 
  dplyr::mutate(Arthro = ifelse(Species == "Atylodes genei", 1, Arthro)) %>% 
  dplyr::mutate(omnivore = apply(dplyr::select(., plant, Arthro, Vert), 1, sum)) %>% 
  dplyr::filter(omnivore > 0) %>% 
  dplyr::mutate(diet_5cat = ifelse(omnivore > 1, 4, max.col(dplyr::select(., plant, Arthro, Vert)))) %>% 
  dplyr::mutate(diet_5cat = case_when(diet_5cat == 1 ~ 1, 
                               diet_5cat == 2 ~ 3, 
                               diet_5cat == 3 ~ 4, 
                               diet_5cat == 4 ~ 2)) %>% 
  # Add diet labels
  left_join(., diet_cat_key) %>% 
  mutate(diet_name = factor(diet_name, levels = diet_cat_key$diet_name)) %>% 
  dplyr::mutate(tax = "Amphibians") %>% 
  dplyr::mutate(ln_body_mass = log(Body_mass_g))

am_sum <- tr_amph %>% 
  dplyr::group_by(tax, diet_name) %>% 
  dplyr::count(name = "am_n")

am <- tr_amph %>% 
  ggplot(., aes(x = diet_name, y = ln_body_mass, group = diet_name, fill = diet_name)) +
  geom_boxplot(coef = 10) +
  geom_text(data = am_sum, aes(y = 11.8, label = am_n), size = 7, fontface = "bold") +
  scale_fill_manual(values = c("#359B73", "#2271B2", "#FFAC3B", "#CD022D"), drop = FALSE) +
  scale_x_discrete(NULL, drop = FALSE) +
  scale_y_continuous(name = "Mass (kg)",
                     breaks = log(c(1, 10, 100, 1000, 10000, 100000, 1000000, 10000000)),
                     labels = c(1, 10, 100, 1000, 10000, 100000, 1000000, 10000000)/1000) +
  coord_cartesian(ylim = c(0, 12)) +
  facet_wrap(~ tax) +
  boxplot_theme_tax(base_size = 20) +
  theme(legend.position = "none")

# reptiles

tr_rep_orig <- readr::read_csv("../data/Final IUCN data set-published (Amanda).csv") %>% 
  dplyr::filter(Class == "REPTILIA") %>% 
  dplyr::select(Order, Family, Genus, binomial = Sciname, starts_with("Cat"), starts_with("Diet"), starts_with("Body"))

rep_diet_cat_key <- data.frame(diet_5cat = c(1, 2, 3, 4, 5), diet_name = c("herbivore", "planktivore", "omnivore", "benthic carnivore", "higher carnivore"))

tr_rep <- tr_rep_orig %>% 
  dplyr::mutate(plant = pmax(Cat_Plant, Cat_Plant_Other, Cat_Foliage, Cat_Root, Cat_Wood, Cat_Nectar, Cat_Fruit, Cat_Grain, na.rm = TRUE)) %>% 
  dplyr::mutate(inv = pmax(Cat_Invert, Cat_Insect)) %>% 
  dplyr::mutate(vert = pmax(Cat_Scavenger, Cat_Endotherm, Cat_Mammal, Cat_Bird, Cat_Herptile, Cat_Amphibian, Cat_Reptile, Cat_Fish, Cat_VertUnk, Cat_Vertebrate, Cat_Egg)) %>% 
  dplyr::mutate(omnivore = apply(dplyr::select(., plant, inv, vert), 1, sum)) %>% 
  dplyr::mutate(diet_5cat = ifelse(omnivore > 1, 4, max.col(dplyr::select(., plant, inv, vert)))) %>% 
  dplyr::mutate(diet_5cat = case_when(diet_5cat == 1 ~ 1, 
                                      diet_5cat == 2 ~ 3, 
                                      diet_5cat == 3 ~ 4, 
                                      diet_5cat == 4 ~ 2)) %>% 
  # Add diet labels
  left_join(., diet_cat_key) %>% 
  mutate(diet_name = factor(diet_name, levels = diet_cat_key$diet_name)) %>% 
  dplyr::mutate(tax = "Reptiles") %>% 
  dplyr::mutate(ln_body_mass = log(Body_Mass_Value))

rep_sum <- tr_rep %>% 
  dplyr::group_by(tax, diet_name) %>% 
  dplyr::count(name = "rep_n")

rep <- tr_rep %>% 
  ggplot(., aes(x = diet_name, y = ln_body_mass, group = diet_name, fill = diet_name)) +
  geom_boxplot(coef = 10) +
  geom_text(data = rep_sum, aes(y = 14.1, label = rep_n), size = 7, fontface = "bold") +
  scale_fill_manual(values = c("#359B73", "#2271B2", "#FFAC3B", "#CD022D"), drop = FALSE) +
  scale_x_discrete(NULL, drop = FALSE) +
  scale_y_continuous(name = "Mass (kg)",
                     breaks = log(c(1, 10, 100, 1000, 10000, 100000, 1000000, 10000000)),
                     labels = c(1, 10, 100, 1000, 10000, 100000, 1000000, 10000000)/1000) +
  coord_cartesian(ylim = c(0, 14.6)) +
  facet_wrap(~ tax) +
  boxplot_theme_tax(base_size = 20) +
  theme(legend.position = "none")

## fish

tr_fish_orig <- readr::read_csv("../data/Traits V3 (fish).csv")

fish_diet_cat_key <- data.frame(diet_5cat = c(1, 2, 3, 4, 5), diet_name = c("herbivore", "planktivore", "omnivore", "benthic\ncarnivore", "higher\ncarnivore"))

tr_fish <- tr_fish_orig %>% 
  dplyr::distinct(CURRENT_TAXONOMIC_NAME, .keep_all = TRUE) %>% 
  dplyr::mutate(diet_5cat = case_when(TrophicGrp_1 == "Herbivore" ~ 1, 
                                      TrophicGrp_1 == "Planktivore" ~ 2, 
                                      TrophicGrp_1 == "Omnivore" ~ 3,
                                      TrophicGrp_1 == "omnivore" ~ 3,
                                      TrophicGrp_1 == "Benthic carnivore" ~ 4,
                                      TrophicGrp_1 == "Higher carnivore" ~ 5)) %>% 
  # Add diet labels
  left_join(., fish_diet_cat_key) %>% 
  mutate(diet_name = factor(diet_name, levels = fish_diet_cat_key$diet_name)) %>% 
  dplyr::mutate(tax = "Fishes") %>% 
  dplyr::mutate(ln_lmax = log(Lmax))

fish_sum <- tr_fish %>% 
  dplyr::group_by(tax, diet_name) %>% 
  dplyr::count(name = "fish_n")

fish <- tr_fish %>% 
  ggplot(., aes(x = diet_name, y = ln_lmax, group = diet_name, fill = diet_name)) +
  geom_boxplot(coef = 10) +
  geom_text(data = fish_sum, aes(y = 8.3, label = fish_n), size = 7, fontface = "bold") +
  scale_fill_manual(values = c("#359B73", "darkseagreen1", "#2271B2", "orangered", "red"), drop = FALSE) +
  scale_x_discrete(NULL, drop = FALSE) +
  scale_y_continuous(name = "Max Length (cm)",
                     breaks = log(c(1, 10, 100, 1000, 10000, 100000, 1000000, 10000000)),
                     labels = c(1, 10, 100, 1000, 10000, 100000, 1000000, 10000000)) +
  coord_cartesian(ylim = c(0, 8.6)) +
  facet_wrap(~ tax) +
  boxplot_theme_tax(base_size = 20) +
  theme(legend.position = "none")

tax <- deeptime::ggarrange2(tm, mr, br, mbr, rep, fish, ncol = 2, byrow = TRUE,
                            labels = c("A", "B", "C", "D", "E", "F"),
                            label.args = list(gp = grid::gpar(font = 2, cex = 2)))

# Figure 3
save_plot("../figures/v_plots_tax.pdf", tax, base_width = 16, base_height = 12)

# Figure S3
save_plot("../figures/v_plots_tax_supp.pdf", am, base_width = 8, base_height = 4)

