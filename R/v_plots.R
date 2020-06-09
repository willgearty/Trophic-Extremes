library(tidyverse)
library(nlme)

# Prepare V trait data
# ------------------------------------------------------------------------------
# Prep biome and trait data
biome_traits <- 
  readRDS("../data/amanda_biome_traits.rds") %>% 
  # Make sure that biome ids are numeric
  mutate(biome = as.numeric(biome))
# Create lookup for biome ids
biome_key <- tibble(biome = 1:14, biome_name = c("Tropical & Subtropical Moist Broadleaf Forests", "Tropical & Subtropical Dry Broadleaf Forests", "Tropical & Subtropical Coniferous Forests", "Temperate Broadleaf & Mixed Forests", "Temperate Conifer Forests", "Boreal Forests/Taiga", "Tropical & Subtropical Grasslands, Savannas & Shrublands", "Temperate Grasslands, Savannas & Shrublands", "Flooded Grasslands & Savannas", "Montane Grasslands & Shrublands", "Tundra", "Mediterranean Forests, Woodlands & Scrub", "Deserts & Xeric Shrublands", "Mangroves"))

# Master biome data
biomes <- 
  left_join(biome_traits, biome_key) %>% 
  # We only need the biome information for now
  select(binomial, class, biome, biome_name, body_mass_median) %>% 
  # Clarify that body_mass_median in this dataframe is actually log10 body mass
  rename(log10_body_mass_median = body_mass_median)

# Prepare diet labels
diet_cat_key <- tibble(diet_5cat = c(1, 2, 3, 4, 5), 
                       diet_name = c('Herbivores', 'Frugivores', 'Omnivores', 
                                     'Insectivores', 'Carnivores'), 
                       diet_abbr = c('H', 'F', 'O', 'I', 'C'))

# These data supply the realm and diet category
v_traits <- 
  readRDS("../data/v_trait.rds") %>% 
  as_tibble()  %>% # easier to manage
  select(binomial, realm, diet_5cat, body_mass_median)

# Master V data
v_data <- 
  left_join(biomes, v_traits, by = "binomial") %>%
  # Relabel diet categories to match diet_cat_key
  mutate(diet_5cat = case_when(diet_5cat == 1 ~ 1, 
                               diet_5cat == 2 ~ 2, 
                               diet_5cat == 3 ~ 5, 
                               diet_5cat == 4 ~ 3, 
                               diet_5cat == 5 ~ 4)) %>% 
  # Add diet labels
  left_join(., diet_cat_key) %>% 
  mutate(diet_factor = factor(diet_5cat), 
         biome_factor = factor(biome))

terr_mammals <- filter(v_data, realm == "terrestrial", class == "Mammalia")
terr_birds   <- filter(v_data, realm == "terrestrial", class == "Aves")

#-------------------------------------------------------------------------------
# Figures 3 & 4
#-------------------------------------------------------------------------------

# Terrestrial mammal body size ~ diet plot
terr_mammals %>% 
  ggplot(., aes(x = diet_abbr, y = log10_body_mass_median, group = diet_abbr)) +
    geom_boxplot(outlier.shape = NA) +
    facet_wrap( ~ biome_name) +
    theme(axis.text.x = element_text(angle = 50, size = 9, vjust = 0.5))+
    theme(panel.background = element_rect(fill = 'white'))+
    theme(axis.line = element_line(colour = "black", size = 0.5, linetype = 1, lineend = "butt")) +
    xlab("Diet category") +
    ylab(ylab(expression(Log[10]~median~body~mass)))

# Terrestrial bird body size ~ diet plot
terr_birds %>% 
  ggplot(., aes(diet_abbr, log10_body_mass_median, group = diet_abbr))+
    geom_boxplot(outlier.shape = NA) +
    facet_wrap( ~ biome_name) +
    theme(axis.text.x = element_text(angle = 50, size = 9, vjust = 0.5))+
    theme(panel.background = element_rect(fill = 'white'))+
    theme(axis.line = element_line(colour = "black", size = 0.5, linetype = 1, lineend = "butt")) +
    labs(x = "Diet category", y = expression(Log[10]~median~body~mass))


#-------------------------------------------------------------------------------
# Calculated means
#-------------------------------------------------------------------------------

# gls model for the different body mass ~ diet category across space
mammal_gls <- 
  gls(log10_body_mass_median ~ diet_factor - 1, 
      weights = varIdent(form = ~ 1 | diet_factor), 
      data = terr_mammals %>% drop_na(diet_factor, biome_factor))

summary(mammal_gls)

bird_gls <- 
  gls(log10_body_mass_median ~ diet_factor - 1, 
      weights = varIdent(form = ~ 1 | diet_factor), 
      data = terr_birds %>% drop_na(diet_factor, biome_factor))
summary(bird_gls)