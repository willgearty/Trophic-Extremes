# title: "v_plots_rob.R"
# authors: Rob Cooke (03rcooke@gmail.com) and Will Gearty (willgearty@gmail.com)

# Set up ####

if(!require("pacman")) install.packages("pacman")
pacman::p_load(nlme, dplyr, tidyr, ggplot2, tibble, stringr, gtable, cowplot, lemon, gtools, grImport, deeptime, rcompanion, shadowtext)
pacman::p_load_gh("richfitz/vectoR")

# Will's colour scheme
colors4 <- setNames(c("#359B73", "#2271B2", "#FFAC3B", "#CD022D"), c("herbivore", "omnivore", "invertivore", "carnivore"))

## Data ####

# Prep biome and trait data
biome_traits <- 
  readRDS("../data/amanda_biome_traits.rds") %>% 
  # Make sure that biome ids are numeric
  mutate(biome = as.numeric(biome))

# Create lookup for biome ids
biome_key <- tibble(biome = 1:14, biome_name = c("Tropical & Subtropical Moist Broadleaf Forests", "Tropical & Subtropical Dry Broadleaf Forests", "Tropical & Subtropical Coniferous Forests", "Temperate Broadleaf & Mixed Forests", "Temperate Conifer Forests", "Boreal Forests/Taiga", "Tropical & Subtropical Grasslands, Savannas & Shrublands", "Temperate Grasslands, Savannas & Shrublands", "Flooded Grasslands & Savannas", "Montane Grasslands & Shrublands", "Tundra", "Mediterranean Forests, Woodlands & Scrub", "Deserts & Xeric Shrublands", "Mangroves"), 
                    biome_label = c("Tropical & Subtropical\nMoist Broadleaf Forests", "Tropical & Subtropical\nDry Broadleaf Forests", "Tropical & Subtropical\nConiferous Forests", "Temperate Broadleaf & Mixed Forests", "Temperate Conifer Forests", "Boreal Forests/Taiga", "Tropical & Subtropical\nGrasslands, Savannas & Shrublands", "Temperate\nGrasslands, Savannas & Shrublands", "Flooded Grasslands & Savannas", "Montane Grasslands & Shrublands", "Tundra", "Mediterranean\nForests, Woodlands & Scrub", "Deserts & Xeric Shrublands", "Mangroves"))

# Master biome data
biomes <- 
  left_join(biome_traits, biome_key) %>% 
  # We only need the biome information for now
  select(binomial, biome, biome_name, biome_label)

# Prepare diet labels
diet_cat_key <- tibble(diet_5cat = c(1, 2, 3, 4), 
                       diet_name = c('herbivore', 'omnivore', 'invertivore', 'carnivore'), 
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
  mutate(biome_label = factor(biome_label, levels = c("Tundra", "Boreal Forests/Taiga", "Temperate Broadleaf & Mixed Forests", "Temperate Conifer Forests", "Temperate\nGrasslands, Savannas & Shrublands", "Mediterranean\nForests, Woodlands & Scrub", "Deserts & Xeric Shrublands", "Montane Grasslands & Shrublands", "Tropical & Subtropical\nConiferous Forests", "Tropical & Subtropical\nDry Broadleaf Forests", "Tropical & Subtropical\nGrasslands, Savannas & Shrublands", "Tropical & Subtropical\nMoist Broadleaf Forests", "Flooded Grasslands & Savannas", "Mangroves")))

terr_mammals <- filter(v_data, realm == "terrestrial", class == "Mammalia", !is.na(biome_label))
terr_birds   <- filter(v_data, realm == "terrestrial", class == "Aves", !is.na(biome_label))

# Figures ####

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
          legend.text = element_text(size = base_size*1.75),
          legend.key.size = unit(2, "lines"),
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

## Figure 3 ####
## Terrestrial Mammals across biomes ##
tm_sum <- terr_mammals %>% 
  dplyr::group_by(biome, biome_label, diet_name) %>% 
  dplyr::count(name = "tm_n")

tm_p <- with(terr_mammals, pairwise.wilcox.test(ln_body_mass_median, interaction(diet_name, biome_name), p.adjust.method = "none"))
# the indices of the comparisons between diets within biomes
p_idx <- sort(c(seq(1, 55, 4), seq(2, 55, 4), seq(3, 55, 4)))
# adjust the p-values now since we don't care about most of them
tm_stars <- stars.pval(p.adjust(diag(tm_p$p.value)[p_idx]))
tm_star_df <- data.frame(biome_label = factor(rep(levels(terr_mammals$biome_label), each = 3), levels = levels(terr_mammals$biome_label)),
                         x = seq(1.5,3.5), y = 0, star = tm_stars)

tm90 <- do.call(rbind, lapply(levels(terr_mammals$biome_label), function(x) {
  df <- pairwisePercentileTest(ln_body_mass_median ~ diet_name, data = subset(terr_mammals, biome_label == x),
                               test = "percentile", tau = 0.90, r = 5000, digits = 7)[c(1,4,6), 1:2]
  df$biome_label <- x
  df
}))
tm90$p.adjust <- p.adjust(tm90$p.value)
tm90$stars <- stars.pval(tm90$p.adjust)

tm <- terr_mammals %>% 
  ggplot(., aes(x = diet_name, y = ln_body_mass_median, group = diet_name, fill = diet_name)) +
  geom_boxplot(color = "black") +
  geom_text(data = tm_sum, aes(y = 16.4, label = tm_n), size = 7, fontface = "bold", show.legend = FALSE) +
  geom_text(data = tm_star_df, aes(x = x, y = y, label = star), size = 8, inherit.aes = FALSE) +
  geom_shadowtext(data = tm90, aes(label = stars), x = rep(seq(1.5, 3.5), 14), y = 14.4, size = 7.5, colour = "white", inherit.aes = FALSE) +
  facet_wrap(~ biome_label, ncol = 5, labeller = label_wrap_gen(width=25)) +
  boxplot_theme(base_size = 20) +
  scale_fill_manual(values = c("#359B73", "#2271B2", "#FFAC3B", "#CD022D")) +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = "Mass (kg)",
                     breaks = log(c(1, 10, 100, 1000, 10000, 100000, 1000000, 10000000)),
                     labels = c(1, 10, 100, 1000, 10000, 100000, 1000000, 10000000)/1000) +
  coord_cartesian(ylim = c(0, 17))

tm <- shift_legend2(p = tm)

save_plot('../figures/v_plot_terr_mam_colour.pdf', tm, base_width = 18, base_height = 12)

#tests of diet among biomes
tm_p_among <- with(terr_mammals, pairwise.wilcox.test(ln_body_mass_median, interaction(biome_name, diet_name), p.adjust.method = "none"))$p.value
tm_p_among_adjust <- tm_p_among %>%
  as.data.frame() %>%
  mutate(biome1 = rownames(tm_p_among)) %>%
  pivot_longer(cols = !biome1, names_to = "biome2", values_to = "p.value") %>%
  filter(sub("^.*[:.:]", "", biome1) == sub("^.*[:.:]", "", biome2) & !is.na(p.value)) %>%
  mutate(diet = sub("^.*[:.:]", "", biome1), p.adjust = p.adjust(p.value)) %>%
  mutate(biomes = paste(sub("[:.:].*$", "", biome2), sub("[:.:].*$", "", biome1), sep = " - ")) %>%
  select(c("biomes", "diet", "p.adjust")) %>%
  pivot_wider(names_from = diet, values_from = p.adjust) %>%
  arrange(biomes) %>%
  column_to_rownames(var = "biomes")
tm_p_among_adjust <- rbind(tm_p_among_adjust, "# Significant" = colSums(tm_p_among_adjust < .05))
write.csv(tm_p_among_adjust, "../tables/among_biome_mann_whitney.csv")

tm90_among <- do.call(cbind, lapply(levels(terr_mammals$diet_name), function(x) {
  df <- pairwisePercentileTest(ln_body_mass_median ~ biome_name, data = subset(terr_mammals, diet_name == x),
                               test = "percentile", tau = 0.90, r = 5000, digits = 7)
  return(setNames(as.numeric(df$p.value), sub(" = 0","",df$Comparison)))
}))
colnames(tm90_among) <- levels(terr_mammals$diet_name)
tm90_among_adjust <- tm90_among %>%
  as.data.frame() %>%
  rownames_to_column(var = "biomes") %>%
  pivot_longer(!biomes, names_to = "diet", values_to = "p.value") %>%
  mutate(p.adjust = p.adjust(p.value)) %>%
  select(c("biomes", "diet", "p.adjust")) %>%
  pivot_wider(names_from = diet, values_from = p.adjust) %>%
  arrange(biomes) %>%
  column_to_rownames(var = "biomes")
tm90_among_adjust <- rbind(tm90_among_adjust, "# Significant" = colSums(tm90_among_adjust < .05))
write.csv(tm90_among_adjust, "../tables/among_biome_90th.csv")

## Figure S1####
# Terrestrial bird body size ~ diet ~ biome plot
tb_sum <- terr_birds %>% 
  dplyr::group_by(biome, biome_label, diet_name) %>% 
  dplyr::count(name = "tm_n")

tb_p <- with(terr_birds, pairwise.wilcox.test(ln_body_mass_median, interaction(diet_name, biome_label), p.adjust.method = "none"))
# the indices of the comparisons between diets within biomes
p_idx <- sort(c(seq(1, 55, 4), seq(2, 55, 4), seq(3, 55, 4)))
# adjust the p-values now since we don't care about most of them
tb_stars <- stars.pval(p.adjust(diag(tb_p$p.value)[p_idx]))
tb_star_df <- data.frame(biome_label = factor(rep(levels(terr_birds$biome_label), each = 3), levels = levels(terr_birds$biome_label)),
                         x = seq(1.5,3.5), y = 0.5, star = tb_stars)

tb90 <- do.call(rbind, lapply(levels(terr_birds$biome_label), function(x) {
  df <- pairwisePercentileTest(ln_body_mass_median ~ diet_name, data = subset(terr_birds, biome_label == x),
                               test = "percentile", tau = 0.90, r = 5000, digits = 7)[c(1,4,6), 1:2]
  df$biome_label <- x
  df
}))
tb90$p.adjust <- p.adjust(tb90$p.value)
tb90$stars <- stars.pval(tb90$p.adjust)

tb <- terr_birds %>% 
  ggplot(., aes(x = diet_name, y = ln_body_mass_median, group = diet_name, fill = diet_name)) +
  geom_boxplot(color = "black") +
  geom_text(data = tb_sum, aes(y = 12.6, label = tm_n), size = 7, fontface = "bold", show.legend = FALSE) +
  geom_text(data = tb_star_df, aes(x = x, y = y, label = star), size = 8, inherit.aes = FALSE) +
  geom_shadowtext(data = tb90, aes(label = stars), x = rep(seq(1.5, 3.5), 14), y = 10.6, size = 7.5, colour = "white", inherit.aes = FALSE) +
  facet_wrap(~ biome_label, ncol = 5, labeller = label_wrap_gen(width=25)) +
  boxplot_theme(base_size = 20) +
  scale_fill_manual(values = c("#359B73", "#2271B2", "#FFAC3B", "#CD022D")) +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = "Mass (kg)",
                     breaks = log(c(1, 10, 100, 1000, 10000, 100000, 1000000, 10000000)),
                     labels = c(1, 10, 100, 1000, 10000, 100000, 1000000, 10000000)/1000) +
  coord_cartesian(ylim = c(0, 13.2))

tb <- shift_legend2(p = tb)

save_plot('../figures/v_plot_terr_bird_colour.pdf', tb, base_width = 18, base_height = 12)

## Figure S2 ####
# Fish maximum body length ~ diet ~ biome plot
tr_fish_orig <- readr::read_csv("../data/Traits V3 (fish).csv")

fish_diets <- c("herbivore", "planktivore", "omnivore", "benthic carnivore", "higher carnivore")
fish_diet_cat_key_new <- data.frame(diet_5cat = c(1, 2, 3, 4, 5), diet_name = fish_diets)

tr_fish <- tr_fish_orig %>% 
  dplyr::distinct(CURRENT_TAXONOMIC_NAME, .keep_all = TRUE) %>% 
  dplyr::mutate(diet_5cat = case_when(TrophicGrp...6 == "Herbivore" ~ 1, 
                                      TrophicGrp...6 == "Planktivore" ~ 2, 
                                      TrophicGrp...6 == "Omnivore" ~ 3,
                                      TrophicGrp...6 == "omnivore" ~ 3,
                                      TrophicGrp...6 == "Benthic carnivore" ~ 4,
                                      TrophicGrp...6 == "Higher carnivore" ~ 5)) %>% 
  # Add diet labels
  left_join(., fish_diet_cat_key_new) %>% 
  mutate(diet_name = factor(diet_name, levels = fish_diet_cat_key_new$diet_name)) %>% 
  dplyr::mutate(tax = "Fishes") %>% 
  dplyr::mutate(ln_lmax = log(Lmax)) %>% 
  # order by latitudinal position
  dplyr::mutate(Realm = factor(Realm, levels = c("Arctic", "Southern Ocean", "Temperate Australasia", "Temperate Southern Africa", "Temperate South America", "Temperate Northern Atlantic", "Temperate Northern Pacific", "Tropical Atlantic", "Western Indo-Pacific", "Central Indo-Pacific", "Eastern Indo-Pacific", "Tropical Eastern Pacific"))) %>%
  filter(!is.na(ln_lmax))

fsh_sum <- tr_fish %>% 
  dplyr::group_by(Realm, diet_name) %>% 
  dplyr::count(name = "fsh_n")

fsh_p <- with(tr_fish, pairwise.wilcox.test(ln_lmax, interaction(diet_name, Realm), p.adjust.method = "none"))
# get indices of comparisons between diets within realms
# don't forget the last comparison of the last realm
realm_idx <- c(do.call(c, lapply(levels(tr_fish$Realm), function(realm) head(which(sub("^.*\\.","",colnames(fsh_p$p.value)) == realm), -1))), 50)
# adjust the p-values here because we don't need most of the comparisons
fsh_stars <- stars.pval(p.adjust(diag(fsh_p$p.value)[realm_idx]))
# get diet indices with data
diet_idx <- match(sub("\\..*$","",colnames(fsh_p$p.value)), fish_diets)
fsh_star_df <- data.frame(Realm = factor(sub("^.*\\.","",colnames(fsh_p$p.value))[realm_idx], levels = levels(tr_fish$Realm)),
                          x = diet_idx[realm_idx] + .5, y = 1, star = fsh_stars)

fsh90 <- do.call(rbind, lapply(levels(tr_fish$Realm), function(x) {
  n_diets <- length(unique(subset(tr_fish, Realm == x)$diet_name))
  if(n_diets >= 2){
    df <- pairwisePercentileTest(ln_lmax ~ diet_name, data = subset(tr_fish, Realm == x),
                               test = "percentile", tau = 0.90, r = 5000, digits = 7)[, 1:2]
    df$Realm <- x
    if(n_diets == 5){
      return(df[c(1,5,8,10),])
    } else if(n_diets == 3){
      return(df[c(1,3),])
    } else if(n_diets == 2){
      return(df)
    }
  }
}))
fsh90$p.adjust <- p.adjust(fsh90$p.value)
fsh90$stars <- stars.pval(fsh90$p.adjust)
fsh90$x <- match(sub(" -.*$","",fsh90$Comparison), fish_diets) + .5

fsh <- tr_fish %>% 
  ggplot(., aes(x = diet_name, y = ln_lmax, group = diet_name, fill = diet_name)) +
  geom_boxplot(color = "black") +
  geom_text(data = fsh_sum, aes(y = 8.2, label = fsh_n), size = 7, fontface = "bold", show.legend = FALSE) +
  geom_text(data = fsh_star_df, aes(x = x, y = y, label = star), size = 8, inherit.aes = FALSE) +
  geom_shadowtext(data = fsh90, aes(x = x, label = stars), y = 7.2, size = 7.5, colour = "white", inherit.aes = FALSE) +
  facet_wrap(~ Realm, ncol = 5, labeller = label_wrap_gen(width=25)) +
  boxplot_theme(base_size = 20) +
  scale_fill_manual(values = c("#359B73", "darkseagreen1", "#2271B2", "orangered", "red")) +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = "Max Length (cm)",
                     breaks = log(c(1, 10, 100, 1000, 10000, 100000, 1000000, 10000000)),
                     labels = c(1, 10, 100, 1000, 10000, 100000, 1000000, 10000000)) +
  coord_cartesian(ylim = c(1, 8.8))

# fsh <- shift_legend2(p = fsh) # doesn't work due to different panel layout
                               
fsh <- cowplot::ggdraw(fsh + theme(legend.position = "none")) +
  # manually position legend
  cowplot::draw_plot(cowplot::get_legend(fsh), 0.32, -0.09, 0.5, 0.5)

cowplot::save_plot('../figures/v_plot_fish_realm_colour.pdf', fsh, base_width = 18, base_height = 12)

## Figure 2 ####
## Across taxa ##

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

# phylopics
uuids <- c("Mammals (terrestrial)" = "62398ac0-f0c3-48f8-8455-53512a05fbc4", #Loxodonta africana
           "Mammals (marine)" = "41a350a0-a19c-41ed-a85e-47e6b4ba7da4", #Steno bredanensis
           "Birds (terrestrial)" = "416ec8c6-0ed1-4c9f-b4a4-8c1ef6496e84", #Turnix sylvaticus
           "Birds (marine)" = "21c50828-58d8-42df-9219-2c1b0fb57c99", #Aptenodytes patagonicus
           "Reptile" = "83053aee-0f56-4cf3-bbfa-9207e6f13f46", #Ardeosaurus brevipes
           "Fishes" = "7413aa3a-d736-435a-8635-0c316ff73f26", #Salmoninae
           "Amphibian" = "4679516b-405b-444f-974d-9775876716e2" #Hyloidea
)

getPhyloPic <- function(x){
  isexe <- Sys.which("inkscape.exe")
  #gsexe <- Sys.which("gswin64c.exe")
  if(file.exists(paste0(x,".xml"))){
    # read xml using vectoR
    ret_val <- vector_read_xml(paste0(x, ".xml"))
  } else {
    if(!file.exists(paste0(x,".svg"))){
      download.file(paste0("http://phylopic.org/assets/images/submissions/",x,".svg"), paste0(x,".svg"))
    }
    # use inkscape to convert from svg to eps
    cmd <- sprintf("%s %s -o %s", isexe, paste0(x,".svg"), paste0(x, ".eps"))
    system(cmd, ignore.stdout = TRUE)
    # read eps using vectoR
    ret_val <- vector_read_eps(paste0(x, ".eps"))
    file.remove(paste0(x, ".svg"))
    file.remove(paste0(x, ".eps"))
  }
  return(ret_val)
}

phylopics <- lapply(uuids, getPhyloPic)
#make them all face to the right
phylopics[[1]] <- flip(phylopics[[1]], horizontal = TRUE)
phylopics[[3]] <- flip(phylopics[[3]], horizontal = TRUE)
phylopics[[4]] <- flip(phylopics[[4]], horizontal = TRUE)
phylopics[[5]] <- flip(phylopics[[5]], horizontal = TRUE)
phylopics[[6]] <- flip(phylopics[[6]], horizontal = TRUE)
phylopics[[7]] <- flip(phylopics[[7]], horizontal = TRUE)

phylopics <- lapply(phylopics, pictureGrob)

# birds (terrestrial)

birds <- filter(v_data, class == "Aves", realm == "terrestrial") %>% 
  dplyr::distinct(binomial, .keep_all = TRUE) %>% 
  dplyr::mutate(tax = "Birds (terrestrial)")

br_sum <- birds %>% 
  dplyr::group_by(tax, diet_name) %>% 
  dplyr::count(name = "br_n")

br_p <- pairwise.wilcox.test(birds$ln_body_mass_median, birds$diet_name)
br_stars <- stars.pval(diag(br_p$p.value))
br_90 <- pairwisePercentileTest(ln_body_mass_median ~ diet_name, data = birds,
                                test = "percentile", tau = 0.90, r = 5000, digits = 7)[c(1,4,6),]
br_90$stars <- stars.pval(br_90$p.adjust)

br <- birds %>% 
  ggplot(., aes(x = diet_name, y = ln_body_mass_median, group = diet_name, fill = diet_name)) +
  geom_boxplot(coef = 10, color = "black") +
  geom_text(data = br_sum, aes(y = 12.7, label = br_n), size = 7, fontface = "bold") +
  annotate("text", label = br_stars, x = seq(1.5, 3.5), y = 0, size = 8, colour = "black") +
  geom_shadowtext(data = br_90, aes(label = stars), x = seq(1.5, 3.5), y = 11.7, size = 7.5, colour = "white", inherit.aes = FALSE) +
  annotation_custom(phylopics[[3]], 4, 4.6, ymin = -.25, ymax = 2.75) +
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

mbr_p <- pairwise.wilcox.test(mbirds$ln_body_mass_median, mbirds$diet_name)
mbr_stars <- stars.pval(diag(mbr_p$p.value))
mbr_90 <- pairwisePercentileTest(ln_body_mass_median ~ diet_name, data = mbirds,
                                test = "percentile", tau = 0.90, r = 5000, digits = 7)[c(1,3),]
mbr_90$stars <- stars.pval(mbr_90$p.adjust)

mbr <- mbirds %>% 
  ggplot(., aes(x = diet_name, y = ln_body_mass_median, group = diet_name, fill = diet_name)) +
  geom_boxplot(coef = 10) +
  geom_text(data = mbr_sum, aes(y = 11.2, label = mbr_n), size = 7, fontface = "bold") +
  annotate("text", label = mbr_stars, x = seq(2.5, 3.5), y = 0, size = 8, colour = "black") +
  geom_shadowtext(data = mbr_90, aes(label = stars), x = seq(2.5, 3.5), y = 10.2, size = 7.5, colour = "white", inherit.aes = FALSE) +
  annotation_custom(phylopics[[4]], 4, 4.6, ymin = -.25, ymax = 2.25) +
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
  dplyr::mutate(tax = "Mammals (terrestrial)")

tm_sum <- terr_mam %>% 
  dplyr::group_by(tax, diet_name) %>% 
  dplyr::count(name = "mr_n")

tm_p <- pairwise.wilcox.test(terr_mam$ln_body_mass_median, terr_mam$diet_name)
tm_stars <- stars.pval(diag(tm_p$p.value))
tm_90 <- pairwisePercentileTest(ln_body_mass_median ~ diet_name, data = terr_mam,
                                 test = "percentile", tau = 0.90, r = 5000, digits = 7)[c(1,4,6),]
tm_90$stars <- stars.pval(tm_90$p.adjust)

tm <- terr_mam %>% 
  ggplot(., aes(x = diet_name, y = ln_body_mass_median, group = diet_name, fill = diet_name)) +
  geom_boxplot(coef = 10) +
  geom_text(data = tm_sum, aes(y = 16.4, label = mr_n), size = 7, fontface = "bold") +
  annotate("text", label = tm_stars, x = seq(1.5, 3.5), y = 0, size = 8, colour = "black") +
  geom_shadowtext(data = tm_90, aes(label = stars), x = seq(1.5, 3.5), y = 15.4, size = 7.5, colour = "white", inherit.aes = FALSE) +
  annotation_custom(phylopics[[1]], 3.9, 4.7, ymin = -.5, ymax = 2.5) +
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

mr_p <- pairwise.wilcox.test(marine_mam$ln_body_mass_median, marine_mam$diet_name)
mr_stars <- stars.pval(diag(mr_p$p.value))
mr_90 <- pairwisePercentileTest(ln_body_mass_median ~ diet_name, data = marine_mam,
                                test = "percentile", tau = 0.90, r = 5000, digits = 7)[c(1,4,6),]
mr_90$stars <- stars.pval(mr_90$p.adjust)

mr <- marine_mam %>% 
  ggplot(., aes(x = diet_name, y = ln_body_mass_median, group = diet_name, fill = diet_name)) +
  geom_boxplot(coef = 10) +
  geom_text(data = mr_sum, aes(y = 19.9, label = mr_n), size = 7, fontface = "bold") +
  annotate("text", label = mr_stars, x = seq(1.5, 3.5), y = 0, size = 8, colour = "black") +
  geom_shadowtext(data = mr_90, aes(label = stars), x = seq(1.5, 3.5), y = 18.4, size = 7.5, colour = "white", inherit.aes = FALSE) +
  annotation_custom(phylopics[[2]], 3.8, 4.6, ymin = -.5, ymax = 2.5) +
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

am_p <- pairwise.wilcox.test(tr_amph$ln_body_mass, tr_amph$diet_name)
am_stars <- stars.pval(diag(am_p$p.value))
am_90 <- pairwisePercentileTest(ln_body_mass ~ diet_name, data = tr_amph,
                                test = "percentile", tau = 0.90, r = 5000, digits = 7)[1,]
am_90$stars <- stars.pval(am_90$p.adjust)

am <- tr_amph %>% 
  ggplot(., aes(x = diet_name, y = ln_body_mass, group = diet_name, fill = diet_name)) +
  geom_boxplot(coef = 10) +
  geom_text(data = am_sum, aes(y = 11.8, label = am_n), size = 7, fontface = "bold") +
  annotate("text", label = mbr_stars, x = seq(2.5, 2.5), y = 0, size = 8, colour = "black") +
  geom_shadowtext(data = am_90, aes(label = stars), x = seq(2.5, 2.5), y = 10.8, size = 7.5, colour = "white", inherit.aes = FALSE) +
  annotation_custom(phylopics[[7]], 4, 4.6, ymin = -.5, ymax = 2.5) +
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

rep_p <- pairwise.wilcox.test(tr_rep$ln_body_mass, tr_rep$diet_name)
rep_stars <- stars.pval(diag(rep_p$p.value))
rep_90 <- pairwisePercentileTest(ln_body_mass ~ diet_name, data = tr_rep,
                                test = "percentile", tau = 0.90, r = 5000, digits = 7)[c(1,4,6),]
rep_90$stars <- stars.pval(rep_90$p.adjust)

rep <- tr_rep %>% 
  ggplot(., aes(x = diet_name, y = ln_body_mass, group = diet_name, fill = diet_name)) +
  geom_boxplot(coef = 10) +
  geom_text(data = rep_sum, aes(y = 14.1, label = rep_n), size = 7, fontface = "bold") +
  annotate("text", label = rep_stars, x = seq(1.5, 3.5), y = 0, size = 8, colour = "black") +
  geom_shadowtext(data = rep_90, aes(label = stars), x = seq(1.5, 3.5), y = 13.1, size = 7.5, colour = "white", inherit.aes = FALSE) +
  annotation_custom(phylopics[[5]], 4, 4.6, ymin = -.75, ymax = 2.25) +
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
  dplyr::mutate(diet_5cat = case_when(TrophicGrp...6 == "Herbivore" ~ 1, 
                                      TrophicGrp...6 == "Planktivore" ~ 2, 
                                      TrophicGrp...6 == "Omnivore" ~ 3,
                                      TrophicGrp...6 == "omnivore" ~ 3,
                                      TrophicGrp...6 == "Benthic carnivore" ~ 4,
                                      TrophicGrp...6 == "Higher carnivore" ~ 5)) %>% 
  # Add diet labels
  left_join(., fish_diet_cat_key) %>% 
  mutate(diet_name = factor(diet_name, levels = fish_diet_cat_key$diet_name)) %>% 
  dplyr::mutate(tax = "Fishes") %>% 
  dplyr::mutate(ln_lmax = log(Lmax))

fish_sum <- tr_fish %>% 
  dplyr::group_by(tax, diet_name) %>% 
  dplyr::count(name = "fish_n")

fish_p <- pairwise.wilcox.test(tr_fish$ln_lmax, tr_fish$diet_name)
fish_stars <- stars.pval(diag(fish_p$p.value))
fish_90 <- pairwisePercentileTest(ln_lmax ~ diet_name, data = tr_fish,
                                 test = "percentile", tau = 0.90, r = 5000, digits = 7)[c(1,5,8,10),]
fish_90$stars <- stars.pval(fish_90$p.adjust)

fish <- tr_fish %>% 
  ggplot(., aes(x = diet_name, y = ln_lmax, group = diet_name, fill = diet_name)) +
  geom_boxplot(coef = 10) +
  geom_text(data = fish_sum, aes(y = 8.3, label = fish_n), size = 7, fontface = "bold") +
  annotate("text", label = fish_stars, x = seq(1.5, 4.5), y = 0, size = 8, colour = "black") +
  geom_shadowtext(data = fish_90, aes(label = stars), x = seq(1.5, 4.5), y = 7.5, size = 7.5, colour = "white", inherit.aes = FALSE) +
  annotation_custom(phylopics[[6]], 4.8, 5.5, ymin = -1.25, ymax = 2.25) +
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
                               
# Figure 2
save_plot("../figures/v_plots_tax.pdf", tax, base_width = 16, base_height = 12)

# amphibians
# save_plot("../figures/v_plots_tax_supp.pdf", am, base_width = 8, base_height = 4)
                               
## Figure S7 ####
# body mass distribution plot for invertivores and carnivores S8
dens_comb <- ggplot(dplyr::filter(terr_mam, diet_name %in% c("invertivore", "carnivore")), aes(x = ln_body_mass_median)) +
  geom_density(aes(fill = diet_name), alpha = 0.5, colour = NA) +
  scale_fill_manual(values = c("#FFAC3B", "#CD022D")) +
  scale_x_continuous(name = "Mass (kg)",
                     breaks = log(c(1, 10, 100, 1000, 10000, 100000, 1000000, 10000000)),
                     labels = c(1, 10, 100, 1000, 10000, 100000, 1000000, 10000000)/1000,
                     expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 0.42), name = "Density") +
  boxplot_theme_tax(base_size = 20) +
  theme(axis.ticks.x = element_line(),
        axis.title.x = element_text(vjust = 0),
        legend.direction = "vertical")
cowplot::save_plot("../figures/body_dist.pdf", dens_comb, base_width = 10, base_height = 8)


