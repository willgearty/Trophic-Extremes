# title: "v_plots_rob.R"
# author: Rob Cooke (03rcooke@gmail.com)

#### Set up ####

if(!require("pacman")) install.packages("pacman")
pacman::p_load(nlme, dplyr, tidyr, ggplot2, stringr, cowplot, gtable, lemon)


# Will's colour scheme
colors4 <- setNames(c("#359B73", "#2271B2", "#FFAC3B", "#CD022D"), c("herbivore", "omnivore", "insectivore", "carnivore"))

##### Data ####

# Prep biome and trait data
biome_traits <- 
  readRDS("amanda_biome_traits.rds") %>% 
  # Make sure that biome ids are numeric
  mutate(biome = as.numeric(biome))

# Create lookup for biome ids
biome_key <- tibble(biome = 1:14, biome_name = c("Tropical & Subtropical Moist Broadleaf Forests", "Tropical & Subtropical Dry Broadleaf Forests", "Tropical & Subtropical Coniferous Forests", "Temperate Broadleaf & Mixed Forests", "Temperate Conifer Forests", "Boreal Forests/Taiga", "Tropical & Subtropical Grasslands, Savannas & Shrublands", "Temperate Grasslands, Savannas & Shrublands", "Flooded Grasslands & Savannas", "Montane Grasslands & Shrublands", "Tundra", "Mediterranean Forests, Woodlands & Scrub", "Deserts & Xeric Shrublands", "Mangroves"), 
                    biome_label = c("Tropical & Subtropical\nMoist Broadleaf Forests", "Tropical & Subtropical\nDry Broadleaf Forests", "Tropical & Subtropical\nConiferous Forests", "Temperate Broadleaf & Mixed Forests", "Temperate Conifer Forests", "Boreal Forests/Taiga", "Tropical & Subtropical \nGrasslands, Savannas & Shrublands", "Temperate\nGrasslands, Savannas & Shrublands", "Flooded Grasslands & Savannas", "Montane Grasslands & Shrublands", "Tundra", "Mediterranean\nForests, Woodlands & Scrub", "Deserts & Xeric Shrublands", "Mangroves"))

# Master biome data
biomes <- 
  left_join(biome_traits, biome_key) %>% 
  # We only need the biome information for now
  select(binomial, class, biome, biome_name, biome_label, body_mass_median) %>% 
  # Clarify that body_mass_median in this dataframe is actually log10 body mass
  mutate(body_mass_median = 10 ^ body_mass_median) %>% 
  # to match Will's plot
  mutate(ln_body_mass_median = log(body_mass_median))

# Prepare diet labels
diet_cat_key <- tibble(diet_5cat = c(1, 2, 3, 4), 
                       diet_name = c('herbivore', 'omnivore', 'insectivore', 'carnivore'), 
                       diet_abbr = c('H', 'O', 'I', 'C'))

v_traits_orig <- 
  readRDS("v_trait.rds") %>% 
  as_tibble()  %>% # easier to manage
  dplyr::mutate(diet_plant = diet_fruit + diet_nect + diet_seed + diet_planto) %>% 
  dplyr::mutate(diet_vert = diet_vend + diet_vect + diet_vfish + diet_vunk + diet_scav) %>% 
  select(binomial, realm, diet_plant, diet_inv, diet_vert, body_mass_median)

v_traits <- v_traits_orig %>% 
  # omnivores
  dplyr::mutate(omnivore = apply(dplyr::select(v_traits_orig, diet_plant, diet_inv, diet_vert), 1, max)) %>%
  dplyr::mutate(diet_5cat = ifelse(omnivore <=50, 4, max.col(dplyr::select(., diet_plant:diet_vert))))

# Master V data
v_data <- 
  left_join(biomes, v_traits, by = "binomial") %>%
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

terr_mammals <- filter(v_data, realm == "terrestrial", class == "Mammalia")
terr_birds   <- filter(v_data, realm == "terrestrial", class == "Aves")

#### Figures ####

## Mammals across biomes ##
# Figure 3 #

boxplot_theme <- function(base_size = 16) {
  theme_minimal(base_size = base_size) + 
    theme(axis.line = element_line(colour = "black", size = 0.5, lineend = "butt"), 
          panel.grid = element_blank(), 
          axis.title.y = element_text(angle = 90, vjust = 2),
          axis.title.x = element_text(vjust = -5),
          axis.text.x = element_blank(),
          legend.title = element_blank(),
          legend.direction = "vertical",
          strip.background = element_rect(fill = "grey90", colour = "white"),
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

tm_sum <- terr_mammals %>% 
  dplyr::group_by(biome, diet_name) %>% 
  dplyr::count(name = "tm_n")

terr_mammals_sum <- terr_mammals %>% 
  dplyr::left_join(tm_sum, by = c("biome", "diet_name"))

# Terrestrial mammal body size ~ diet plot
tm <- terr_mammals_sum %>% 
  ggplot(., aes(x = diet_name, y = ln_body_mass_median, group = diet_name, fill = diet_name)) +
  geom_boxplot(coef = 10) +
  geom_text(aes(y = 16.6, label = tm_n), size = 5) +
  facet_wrap(~ biome_label, ncol = 5) +
  boxplot_theme() +
  scale_fill_manual(values = c("#359B73", "#2271B2", "#FFAC3B", "#CD022D")) +
  labs(x = "", y = "ln Mass (g)")

tm <- shift_legend2(p = tm)

#save_plot('v_plot_terr_mam_colour.pdf', tm, base_width = 18, base_height = 12)

## Across taxa ## 
# Figure S4 #

boxplot_theme_tax <- function(base_size = 16) {
  theme_minimal(base_size = base_size) + 
    theme(axis.line = element_line(colour = "black", size = 0.5, lineend = "butt"), 
          panel.grid = element_blank(), 
          axis.title.y = element_text(angle = 90, vjust = 2),
          axis.title.x = element_text(vjust = -5),
          legend.title = element_blank(),
          legend.direction = "horizontal",
          strip.background = element_rect(fill = "grey90", colour = "white")
    )
}

# birds (non-marine)

birds <- filter(v_data, class == "Aves", realm == "terrestrial") %>% 
  dplyr::distinct(binomial, .keep_all = TRUE) %>% 
  dplyr::mutate(tax = "Birds (non-marine)")

br_sum <- birds %>% 
  dplyr::group_by(diet_name) %>% 
  dplyr::count(name = "br_n")

birds_sum <- birds %>% 
  dplyr::left_join(br_sum, by = "diet_name")

br <- birds_sum %>% 
  ggplot(., aes(x = diet_name, y = ln_body_mass_median, group = diet_name, fill = diet_name)) +
  geom_boxplot(coef = 10) +
  geom_text(aes(y = 13, label = br_n), size = 5) +
  lims(y = c(0, 13.2)) +
  facet_wrap(~ tax) +
  boxplot_theme_tax() +
  scale_fill_manual(values = c("#359B73", "#2271B2", "#FFAC3B", "#CD022D")) +
  labs(x = "", y = "ln Mass (g)") +
  theme(legend.position = "none")

# Marine birds

mbirds <- filter(v_data, class == "Aves", realm == "seabird") %>% 
  dplyr::distinct(binomial, .keep_all = TRUE) %>% 
  dplyr::mutate(tax = "Marine birds")

mbr_sum <- mbirds %>% 
  dplyr::group_by(diet_name) %>% 
  dplyr::count(name = "mbr_n")

mbirds_sum <- mbirds %>% 
  dplyr::left_join(mbr_sum, by = "diet_name")

mbr <- mbirds_sum %>% 
  ggplot(., aes(x = diet_name, y = ln_body_mass_median, group = diet_name, fill = diet_name)) +
  geom_boxplot(coef = 10) +
  geom_text(aes(y = 11.4, label = mbr_n), size = 5) +
  lims(y = c(0, 11.6)) +
  facet_wrap(~ tax) +
  boxplot_theme_tax() +
  scale_x_discrete(drop = FALSE) +
  scale_fill_manual(values = c("#2271B2", "#FFAC3B", "#CD022D")) +
  labs(x = "", y = "ln Mass (g)") +
  theme(legend.position = "none")

# marine mammals

Atraits <- readRDS("v_trait.rds") %>% 
  dplyr::mutate(ln_body_mass_median = log(body_mass_median)) %>% 
  dplyr::mutate(diet_plant = diet_fruit + diet_nect + diet_seed + diet_planto) %>% 
  dplyr::mutate(diet_vert = diet_vend + diet_vect + diet_vfish + diet_vunk + diet_scav) %>% 
  dplyr::mutate(omnivore = apply(dplyr::select(v_traits_orig, diet_plant, diet_inv, diet_vert), 1, max)) %>%
  dplyr::mutate(diet_5cat = ifelse(omnivore <=50, 4, max.col(dplyr::select(., diet_plant, diet_inv, diet_vert)))) %>% 
  mutate(diet_5cat = case_when(diet_5cat == 1 ~ 1, 
                               diet_5cat == 2 ~ 3, 
                               diet_5cat == 3 ~ 4, 
                               diet_5cat == 4 ~ 2)) %>% 
  # Add diet labels
  left_join(., diet_cat_key) %>% 
  mutate(diet_name = factor(diet_name, levels = diet_cat_key$diet_name)) %>% 
  # Remove species missing diet category
  drop_na(diet_name)

marine_mam <- dplyr::filter(Atraits, realm == "aquatic") %>% 
  dplyr::mutate(tax = "Marine mammals")

mr_sum <- marine_mam %>% 
  dplyr::group_by(diet_name) %>% 
  dplyr::count(name = "mr_n")

marine_mam_sum <- marine_mam %>% 
  dplyr::left_join(mr_sum, by = "diet_name")

mr <- marine_mam_sum %>% 
  ggplot(., aes(x = diet_name, y = ln_body_mass_median, group = diet_name, fill = diet_name)) +
  geom_boxplot(coef = 10) +
  geom_text(aes(y = 20.4, label = mr_n), size = 5) +
  lims(y = c(0, 20.6)) +
  facet_wrap(~ tax) +
  boxplot_theme_tax() +
  scale_fill_manual(values = c("#359B73", "#2271B2", "#FFAC3B", "#CD022D")) +
  labs(x = "", y = "ln Mass (g)") +
  theme(legend.position = "none")

# amphibians 

tr_amph_orig <- readr::read_csv("AmphiBIO_v1.csv") %>% 
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
  dplyr::group_by(diet_name) %>% 
  dplyr::count(name = "am_n")

amphibian_sum <- tr_amph %>% 
  dplyr::left_join(am_sum, by = "diet_name")

am <- amphibian_sum %>% 
  ggplot(., aes(x = diet_name, y = ln_body_mass, group = diet_name, fill = diet_name)) +
  geom_boxplot(coef = 10) +
  geom_text(aes(y = 11.8, label = am_n), size = 5) +
  lims(y = c(0, 12)) +
  facet_wrap(~ tax) +
  boxplot_theme_tax() +
  scale_x_discrete(drop = FALSE) +
  scale_fill_manual(values = c("#359B73", "#2271B2", "#FFAC3B", "#CD022D"), drop = FALSE) +
  labs(x = "", y = "ln Mass (g)") +
  theme(legend.position = "none")

# reptiles

tr_rep_orig <- readr::read_csv("Final IUCN data set-published (Amanda).csv") %>% 
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
  dplyr::group_by(diet_name) %>% 
  dplyr::count(name = "rep_n")

reptile_sum <- tr_rep %>% 
  dplyr::left_join(rep_sum, by = "diet_name")

rep <- reptile_sum %>% 
  ggplot(., aes(x = diet_name, y = ln_body_mass, group = diet_name, fill = diet_name)) +
  geom_boxplot(coef = 10) +
  geom_text(aes(y = 14.4, label = rep_n), size = 5) +
  lims(y = c(0, 14.6)) +
  facet_wrap(~ tax) +
  boxplot_theme_tax() +
  scale_x_discrete(drop = FALSE) +
  scale_fill_manual(values = c("#359B73", "#2271B2", "#FFAC3B", "#CD022D"), drop = FALSE) +
  labs(x = "", y = "ln Mass (g)") +
  theme(legend.position = "none")

## fish

tr_fish_orig <- readr::read_csv("Traits V3 (fish).csv")

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
  dplyr::group_by(diet_name) %>% 
  dplyr::count(name = "fish_n")

fishes_sum <- tr_fish %>% 
  dplyr::left_join(fish_sum, by = "diet_name")

fish <- fishes_sum %>% 
  ggplot(., aes(x = diet_name, y = ln_lmax, group = diet_name, fill = diet_name)) +
  geom_boxplot(coef = 10) +
  geom_text(aes(y = 8.4, label = fish_n), size = 5) +
  lims(y = c(0, 8.6)) +
  facet_wrap(~ tax) +
  boxplot_theme_tax() +
  scale_x_discrete(drop = FALSE) +
  scale_fill_manual(values = c("#359B73", "darkseagreen1", "#2271B2", "orangered", "red"), drop = FALSE) +
  labs(x = "", y = "ln Maximum length") +
  theme(legend.position = "none")

tax <- cowplot::plot_grid(br, mbr, rep, am, mr, fish, ncol = 2, nrow = 3)

# Figure S4
#save_plot("v_plots_tax.pdf", tax, base_width = 11, base_height = 8)
