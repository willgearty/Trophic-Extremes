#load and clean data####
library(plyr)
library(tidyverse)
library(viridis)
library(deeptime)
library(Hmisc)
library(gtools)

#Color palettes
#Black, Orange, Sky Blue, Bluish green, Yellow, Blue, Vermilion, Reddish purple
colors8a <- c(rgb(0,0,0), rgb(230/255, 159/255, 0/255), rgb(86/255, 180/255, 233/255), rgb(0/255, 158/255, 115/255), rgb(240/255,228/255,66/255), rgb(0/255, 114/255, 178/255), rgb(213/255, 94/255, 0/255), rgb(204/255, 121/255, 167/255))
#Black, Honolulu Blue, Summer Sky, Barbie Pink, Ocean Green, Bamboo, Gamboge, Paris Daisy
colors8b <- c(rgb(0,0,0), rgb(34/255, 113/255, 178/255), rgb(61/255, 183/255, 233/255), rgb(247/255, 72/255, 165/255), rgb(53/255, 155/255, 115/255), rgb(213/255, 94/255, 0/255), rgb(230/255, 159/255, 0/255), rgb(240/255, 230/255, 66/255))
colors12 <- c("#9F0162", "#009F81", "#FF5AAF", "#00FCCF", "#8400CD", "#008DF9",
              "#00C2F9", "#FFB2FD", "#A40122", "#E20134", "#FF6E3A", "#FFC33B")

#North American Cenozoic mammals
lyons_diet <- read.csv("../data/Lyons_BS_diet.csv", stringsAsFactors = FALSE)
lyons_diet_clean <- lyons_diet %>%
  filter(!is.na(Recoded_Diet), !is.na(FAD), !is.na(LAD),
         !(PBDB.life.habit %in% c("aquatic"))) %>%
  select(Order, Genus_species, FAD, LAD, Recoded_Diet, lnMass_g) %>%
  mutate(Continent = "NA", Recoded_Diet = ifelse(Recoded_Diet == "insectivore", "invertivore", Recoded_Diet))

#MOM (Global Late Quaternary Mammals)
mom_data <- read.csv("../data/MOM_v10.csv", stringsAsFactors = FALSE, na.strings = "", strip.white = TRUE) %>%
  filter(Mass.Status == "valid", LogMass..g. > 0, is.na(TO.DO), !is.na(trophic),
         !grepl("aquatic|marine|NA", habitat.mode, ignore.case = TRUE)) %>%
  mutate(Genus_species = paste(Genus, Species),
         herb = grepl("browse|frug|graze", trophic, ignore.case = TRUE),
         carn = grepl("carn|piscivore", trophic, ignore.case = TRUE),
         insect = grepl("insect", trophic, ignore.case = TRUE)) %>%
  mutate(omni = rowSums(select(., herb, carn, insect)) > 1)
mom_data$Recoded_Diet <- ifelse(mom_data$omni, "omnivore", NA)
mom_data$Recoded_Diet[!mom_data$omni] <- c("herbivore", "carnivore", "invertivore")[apply(mom_data[!mom_data$omni, c("herb", "carn", "insect")], 1, which.max)]
mom_data$FAD <- .12
mom_data$LAD <- ifelse(mom_data$Status == "extant", 0, as.numeric(mom_data$Last.Occurance..kybp.)/1000)
mom_data$lnMass_g <- log(as.numeric(mom_data$Combined.Mass..g.))

mom_data_clean <- mom_data %>%
  select(Genus_species, Order, FAD, LAD, Recoded_Diet, lnMass_g, Continent)

#Cretaceous mammal data for this study
cretaceous_data <- read.csv("../data/Cretaceous_mammals.csv", stringsAsFactors = FALSE, na.strings = "", strip.white = TRUE) %>%
  filter((!is.na(ln_mass_g) & mass_source != "Body size downgrading of mammals over the late Quaternary") |
           !is.na(ln_mass_g_estimate)) %>%
  mutate(lnMass_g = ifelse(!is.na(ln_mass_g) & mass_source != "Body size downgrading of mammals over the late Quaternary",
                           ln_mass_g, ln_mass_g_estimate)) %>%
  mutate(Genus_species = taxon_name,
         herb = grepl("browse|frug|graze", diet, ignore.case = TRUE),
         carn = grepl("carn|piscivore", diet, ignore.case = TRUE),
         insect = grepl("insect", diet, ignore.case = TRUE)) %>%
  mutate(omni = grepl("omni", diet, ignore.case = TRUE) | rowSums(select(., herb, carn, insect)) > 1,
         Continent = "NA") # probably want to get actual continent data later
cretaceous_data$Recoded_Diet <- ifelse(cretaceous_data$omni, "omnivore", NA)
cretaceous_data$Recoded_Diet[!cretaceous_data$omni] <- c("herbivore", "carnivore", "invertivore")[apply(cretaceous_data[!cretaceous_data$omni, c("herb", "carn", "insect")], 1, which.max)]

cretaceous_data_clean <- cretaceous_data %>%
  select(Genus_species, Order, FAD = firstapp_max_ma, LAD = lastapp_min_ma, Recoded_Diet, lnMass_g, Continent)

#Combine three datasets and clean
mamm_diet <- rbind(cbind(lyons_diet_clean, source = "lyons"),
                   cbind(mom_data_clean, source = "mom"),
                   cbind(cretaceous_data_clean, source = "cretaceous")) %>%
  #Rename out-of-date orders
  mutate(Order = revalue(Order, c("Soricomorpha" = "Eulipotyphla",
                                  "Lipotyphla" = "Eulipotyphla",
                                  "Hicanodonta" = "Cingulata",
                                  "Xenarthra" = "Pilosa"))) %>%
  #Summarise duplicates
  group_by(Genus_species, Continent) %>%
  summarise(Order = paste(na.omit(unique(Order)), collapse = ", "), FAD = max(FAD), LAD = min(LAD), lnMass_g = mean(lnMass_g),
            Recoded_Diet = ifelse("mom" %in% source, Recoded_Diet[source == "mom"],
                                  ifelse("lyons" %in% source, Recoded_Diet[source == "lyons"], Recoded_Diet[source == "cretaceous"])),
            sources = paste(source, collapse = ", "), .groups = "drop") %>%
  #order recoded diet
  mutate(Recoded_Diet = factor(Recoded_Diet, levels = c("herbivore", "omnivore", "invertivore", "carnivore")))

#Categorize orders as archaic or modern
archaic_orders <- mamm_diet %>%
  group_by(Order) %>%
  summarise(archaic = !any(LAD < 5)) %>% filter(archaic)
mamm_diet <- mamm_diet %>%
  mutate(archaic = factor(Order %in% archaic_orders$Order))

#Get relevant subset of time scale
time_scale <- subset(epochs, max_age < 150)
time_scale$name <- factor(time_scale$name, levels = rev(time_scale$name))

#Bluish green, Orange, Reddish purple
colors3 <- setNames(colors8b[c(5, 2, 6)], c("herbivore","omnivore","carnivore"))
colors4 <- setNames(c("#359B73", "#2271B2", "#FFAC3B", "#CD022D"), c("herbivore", "omnivore", "invertivore", "carnivore"))

#phylopics
uuids <- c("Early Cretaceous" = "c579200d-9773-4fe7-a4e1-274fdd5f4507", #Multituberculata
           "Late Cretaceous" = "91324e57-b3f1-42e0-abe3-43e5bc8aa4c6", #Didelphimorphia
           "Paleocene" = "a533b65b-ead2-4360-b613-f9f94294bf93", #Hyaenodon
           "Eocene" = "5fe1c8ef-f7c4-47ff-9cc0-b528107fde98", #Uintatherium
           "Oligocene" = "20521c21-a8c9-4d2b-bcbd-86f63c683fe8", #Hoplophoneus
           "Miocene" = "5d4a8a6f-7564-45b0-ac31-ce2eab6f5b24", #Gomphotherium
           "Pliocene" = "f98f3e0f-abc8-4e8f-a812-67bfe312a276", #Glyptotherium
           "Pleistocene" = "cc04733f-befe-4fbc-948c-9cfd9180c90f", #Smilodon
           "Holocene" = "62398ac0-f0c3-48f8-8455-53512a05fbc4" #Loxodonta
           )

library(grImport)
#devtools::install_github("richfitz/vectoR")
library(vectoR)
getPhyloPic <- function(x){
  isexe <- Sys.which("inkscape.exe")
  gsexe <- Sys.which("gswin64c.exe")
  download.file(paste0("http://phylopic.org/assets/images/submissions/",x,".svg"), paste0(x,".svg"))
  cmd <- sprintf("%s -f %s -E %s", isexe, paste0(x,".svg"), paste0(x, ".eps"))
  ret <- system(cmd, ignore.stderr=TRUE)
  return(vector_read_eps(paste0(x, ".eps")))
}

phylopics <- lapply(uuids, getPhyloPic)
#make them all face to the right
phylopics[[1]] <- flip(phylopics[[1]], horizontal = TRUE)
phylopics[[2]] <- flip(phylopics[[2]], horizontal = TRUE)
phylopics[[4]] <- flip(phylopics[[4]], horizontal = TRUE)
phylopics[[5]] <- flip(phylopics[[5]], horizontal = TRUE)
phylopics[[6]] <- flip(phylopics[[6]], horizontal = TRUE)
phylopics[[8]] <- flip(phylopics[[8]], horizontal = TRUE)
phylopics[[9]] <- flip(phylopics[[9]], horizontal = TRUE)

phylopics <- lapply(phylopics, pictureGrob)

#plots through time####
n_bins <- nrow(time_scale)

mamm_per_bin <- as.data.frame(matrix(NA, nrow = 0, ncol = ncol(mamm_diet) + 2, dimnames = list(c(),c(colnames(mamm_diet), "bin", "bin_mid"))))
for(i in 1:n_bins){
  dat <- subset(mamm_diet, LAD <= time_scale$max_age[i] & FAD >= time_scale$min_age[i])
  if(nrow(dat) > 0) {
    dat$bin <- time_scale$name[i]
    dat$bin_mid <- time_scale$age_mid[i]
    mamm_per_bin <- rbind(mamm_per_bin, dat)
  }
}

#all diets together
ggplot(mamm_per_bin, aes(x = bin, y = lnMass_g)) +
  geom_violin(draw_quantiles = c(.25,.5,.75)) +
  scale_x_discrete(name = "Time (Ma)") +
  scale_y_continuous(name = "ln Mass (g)", lim = c(0,15)) +
  theme_bw()
ggsave("../figures/Mammal Violins.pdf", device = "pdf", width = 10, height = 10)

ggplot(mamm_per_bin, aes(x = bin, y = lnMass_g)) +
  geom_boxplot(position = position_dodge2(preserve = "single")) +
  scale_x_discrete(name = "Time (Ma)") +
  scale_y_continuous(name = "ln Mass (g)", lim = c(0,15)) +
  theme_bw()
ggsave("../figures/Mammal Boxplots.pdf", device = "pdf", width = 10, height = 10)

#all diets together with archaic split out
ggplot(mamm_per_bin, aes(x = bin, y = lnMass_g, fill = archaic)) +
  geom_violin(draw_quantiles = c(.25,.5,.75)) +
  scale_x_discrete(name = "Time (Ma)") +
  scale_y_continuous(name = "ln Mass (g)", lim = c(0,15)) +
  theme_bw()
ggsave("../figures/Mammal Violins Archaic.pdf", device = "pdf", width = 10, height = 10)

ggplot(mamm_per_bin, aes(x = bin, y = lnMass_g, fill = archaic)) +
  geom_boxplot(position = position_dodge2(preserve = "single")) +
  scale_x_discrete(name = "Time (Ma)") +
  scale_y_continuous(name = "ln Mass (g)", lim = c(0,15)) +
  theme_bw()
ggsave("../figures/Mammal Boxplots Archaic.pdf", device = "pdf", width = 10, height = 10)

#diets separate
ggplot(mamm_per_bin, aes(x = bin, y = lnMass_g, fill = Recoded_Diet)) +
  geom_violin(draw_quantiles = c(.25,.5,.75), position = position_dodge(), scale = "width") +
  scale_x_discrete(name = "Time (Ma)") +
  scale_y_continuous(name = "ln Mass (g)", lim = c(0,15)) +
  scale_fill_manual(values = colors4, name = "Diet") +
  theme_bw()
ggsave("../figures/Mammal Diets Violins.pdf", device = "pdf", width = 10, height = 10)

#Figure 3####
mamm_per_bin$bin_num <- as.numeric(mamm_per_bin$bin)
sample_size <- mamm_per_bin %>% group_by(bin, bin_num, Recoded_Diet) %>% filter(n() >= 4, Recoded_Diet %in% c("herbivore", "omnivore", "carnivore")) %>%
  summarise(num = n())

mamm_p <- with(subset(mamm_per_bin, Recoded_Diet != "invertivore"),
               pairwise.wilcox.test(lnMass_g, interaction(Recoded_Diet, bin)))
mamm_stars <- stars.pval(diag(mamm_p$p.value))[sort(c(seq(1, 25, 3), seq(2, 26, 3)))]

gg <- ggplot(mamm_per_bin %>% group_by(bin, Recoded_Diet) %>% filter(n() >= 4, Recoded_Diet %in% c("herbivore", "omnivore", "carnivore")), aes(x = bin_num, y = lnMass_g, fill = Recoded_Diet, group = interaction(bin, Recoded_Diet))) +
  annotate("rect", xmin = seq(0.5, 8.5, 1), xmax = seq(1.5, 9.5, 1), ymin = -Inf, ymax = Inf, fill = rep_len(c("grey90", "white"), length.out = 9)) +
  geom_boxplot(position = position_dodge2(preserve = "single", padding = .15), width = .85) +
  geom_text(data = sample_size, aes(label = num, y = c(0,-.7,.7,0)[as.numeric(Recoded_Diet)], color = Recoded_Diet), position = position_dodge2(preserve = "single", width = .85, padding = .15), size = 6, show.legend = FALSE) +
  annotate("text", label = mamm_stars, x = sort(c(seq(1, 9) - .125, seq(1, 9) + .125)), y = .5, size = 7.5, colour = "black") +
  scale_x_continuous(name = "Time (Ma)", limits = c(0.5, 9.5), labels = rev(c(0, epochs$max_age[1:9])), breaks = seq(0.5, 9.5, 1), expand = c(0,0)) +
  scale_y_continuous(name = "Mass (kg)", breaks = log(c(1, 10, 100, 1000, 10000, 100000, 1000000, 10000000)), labels = c(1, 10, 100, 1000, 10000, 100000, 1000000, 10000000)/1000) +
  coord_cartesian(ylim = c(-2,17)) +
  scale_fill_manual(values = colors4, name = NULL, limits = c("herbivore", "omnivore", "carnivore")) +
  scale_color_manual(values = colors4, name = NULL) +
  theme_classic(base_size = 24) +
  theme(axis.text = element_text(color = "black"), axis.ticks = element_line(color = "black", size = .75),
        panel.border = element_rect(color = "black", fill = NA, size = 1.5), axis.line = element_blank(),
        legend.position = c(.5,.97), legend.direction = "horizontal", legend.background = element_rect(color = NA, fill = NA),
        legend.key.size = unit(2, "lines"), legend.text = element_text(size = 24)) +
  annotation_custom(phylopics[[1]], 1.45, 0.55, ymin = -2.75, ymax = -1.25) +
  annotation_custom(phylopics[[2]], 1.55, 2.45, ymin = -2.75, ymax = -1.25) +
  annotation_custom(phylopics[[3]], 2.55, 3.45, ymin = -2.75, ymax = -1.25) +
  annotation_custom(phylopics[[4]], 3.55, 4.45, ymin = -2.75, ymax = -1.25) +
  annotation_custom(phylopics[[5]], 4.55, 5.45, ymin = -2.75, ymax = -1.25) +
  annotation_custom(phylopics[[6]], 5.55, 6.45, ymin = -2.75, ymax = -1.25) +
  annotation_custom(phylopics[[7]], 6.55, 7.45, ymin = -2.75, ymax = -1.25) +
  annotation_custom(phylopics[[8]], 7.55, 8.45, ymin = -2.75, ymax = -1.25) +
  annotation_custom(phylopics[[9]], 8.55, 9.45, ymin = -2.75, ymax = -1.25)
discrete_periods <- periods
discrete_periods$max_age[1:4] <- c(2, 4, 7, 9)
discrete_periods$min_age[1:4] <- c(0, 2, 4, 7)
discrete_epochs <- epochs
discrete_epochs$max_age[1:9] <- 1:9
discrete_epochs$min_age[1:9] <- 0:8
discrete_epochs$name[8:9] <- c("Late\nCretaceous", "Early\nCretaceous")
(geo_plot <- gggeo_scale(gggeo_scale(ggplotGrob(gg), lims = c(9, 0), dat = discrete_periods, abbrv = FALSE, size = 6, skip = NULL, lwd = .75),
                         dat = discrete_epochs, abbrv = FALSE, skip = NULL, size = 5, lwd = .75, bord = c("left", "right"), height = unit(2.5, "line")))
ggsave("../figures/Mammal Diets Boxplots.pdf", geo_plot, width = 18, height = 12)

#Figure S3####
sample_size <- mamm_per_bin %>% group_by(bin, bin_num, Recoded_Diet) %>% filter(n() >= 4, Recoded_Diet %in% c("herbivore", "omnivore", "carnivore", "invertivore")) %>%
  summarise(num = n())

mamm_p <- with(mamm_per_bin, pairwise.wilcox.test(lnMass_g, interaction(Recoded_Diet, bin)))
mamm_stars <- stars.pval(diag(mamm_p$p.value))[sort(c(seq(1, 33, 4), seq(2, 34, 4), seq(3, 35, 4)))]

gg <- ggplot(mamm_per_bin %>% group_by(bin, Recoded_Diet) %>% filter(n() >= 4, Recoded_Diet %in% c("herbivore", "omnivore", "carnivore", "invertivore")), aes(x = bin_num, y = lnMass_g, fill = Recoded_Diet, group = interaction(bin, Recoded_Diet))) +
  annotate("rect", xmin = seq(0.5, 8.5, 1), xmax = seq(1.5, 9.5, 1), ymin = -Inf, ymax = Inf, fill = rep_len(c("grey90", "white"), length.out = 9)) +
  geom_boxplot(position = position_dodge2(preserve = "single", padding = .15), width = .85) +
  geom_text(data = sample_size, aes(label = num, y = c(.25,-.3,-.9,.25)[as.numeric(Recoded_Diet)], color = Recoded_Diet),
            position = position_dodge2(preserve = "single", width = .85, padding = .15), size = 6, show.legend = FALSE) +
  annotate("text", label = mamm_stars, x = sort(c(seq(1, 9) - .25, seq(1, 9), seq(1, 9) + .25)), y = .75, size = 7.5, colour = "black") +
  scale_x_continuous(name = "Time (Ma)", limits = c(0.5, 9.5), labels = rev(c(0, epochs$max_age[1:9])), breaks = seq(0.5, 9.5, 1), expand = c(0,0)) +
  scale_y_continuous(name = "Mass (kg)", breaks = log(c(1, 10, 100, 1000, 10000, 100000, 1000000, 10000000)), labels = c(1, 10, 100, 1000, 10000, 100000, 1000000, 10000000)/1000) +
  coord_cartesian(ylim = c(-2,17)) +
  scale_fill_manual(values = colors4, name = NULL, limits = c("herbivore", "omnivore", "invertivore", "carnivore")) +
  scale_color_manual(values = colors4, name = NULL) +
  theme_classic(base_size = 24) +
  theme(axis.text = element_text(color = "black"), axis.ticks = element_line(color = "black", size = .75),
        panel.border = element_rect(color = "black", fill = NA, size = 1.5), axis.line = element_blank(),
        legend.position = c(.5,.97), legend.direction = "horizontal", legend.background = element_rect(color = NA, fill = NA),
        legend.key.size = unit(2, "lines"), legend.text = element_text(size = 24)) +
  annotation_custom(phylopics[[1]], 1.45, 0.55, ymin = -2.75, ymax = -1.25) +
  annotation_custom(phylopics[[2]], 1.55, 2.45, ymin = -2.75, ymax = -1.25) +
  annotation_custom(phylopics[[3]], 2.55, 3.45, ymin = -2.75, ymax = -1.25) +
  annotation_custom(phylopics[[4]], 3.55, 4.45, ymin = -2.75, ymax = -1.25) +
  annotation_custom(phylopics[[5]], 4.55, 5.45, ymin = -2.75, ymax = -1.25) +
  annotation_custom(phylopics[[6]], 5.55, 6.45, ymin = -2.75, ymax = -1.25) +
  annotation_custom(phylopics[[7]], 6.55, 7.45, ymin = -2.75, ymax = -1.25) +
  annotation_custom(phylopics[[8]], 7.55, 8.45, ymin = -2.75, ymax = -1.25) +
  annotation_custom(phylopics[[9]], 8.55, 9.45, ymin = -2.75, ymax = -1.25)
discrete_periods <- periods
discrete_periods$max_age[1:4] <- c(2, 4, 7, 9)
discrete_periods$min_age[1:4] <- c(0, 2, 4, 7)
discrete_epochs <- epochs
discrete_epochs$max_age[1:9] <- 1:9
discrete_epochs$min_age[1:9] <- 0:8
discrete_epochs$name[8:9] <- c("Late\nCretaceous", "Early\nCretaceous")
(geo_plot <- gggeo_scale(gggeo_scale(ggplotGrob(gg), lims = c(9, 0), dat = discrete_periods, abbrv = FALSE, size = 6, skip = NULL, lwd = .75),
                         dat = discrete_epochs, abbrv = FALSE, skip = NULL, size = 5, lwd = .75, bord = c("left", "right"), height = unit(2.5, "line")))
ggsave("../figures/Mammal Diets Boxplots with Invertivores.pdf", geo_plot, width = 18, height = 12)

#diets and archaic separate
mamm_per_bin %>% group_by(archaic, bin, Recoded_Diet) %>% filter(n() >= 5) %>%
ggplot(aes(x = bin, y = lnMass_g, fill = archaic, color = Recoded_Diet)) +
  geom_boxplot(position = position_dodge(preserve = "single"), size = 1) +
  scale_x_discrete(name = "Time (Ma)") +
  scale_y_continuous(name = "ln Mass (g)", lim = c(0,15)) +
  scale_color_manual(values = colors4, name = "Diet") +
  scale_fill_manual(values = c("grey80", "grey20")) +
  theme_bw()
ggsave("../figures/Mammal Diets Boxplots Archaic.pdf", device = "pdf", width = 10, height = 10)

#subsampling with increasing sample size####
n_subsets <- 100 #number of replicates for each subset size
subset_sizes <- seq(5,100,5)

basic_subsample <- lapply(subset_sizes, function(subset_size) {
  print(subset_size)
  return(replicate(n_subsets, mamm_per_bin %>%
                     group_by(bin) %>%
                     mutate(species = n()) %>%
                     group_by(bin, species) %>% 
                     sample_n(subset_size, replace = TRUE) %>%
                     summarise(y0 = quantile(lnMass_g, 0), y5 = quantile(lnMass_g, .05),
                               y25 = quantile(lnMass_g, 0.25), y50 = quantile(lnMass_g, 0.5),
                               avg = mean(lnMass_g, na.rm = TRUE), y75 = quantile(lnMass_g, 0.75),
                               y95 = quantile(lnMass_g, 0.95), y100 = quantile(lnMass_g, 1),
                               stddev = sd(lnMass_g, na.rm = TRUE),
                               .groups = "drop") %>%
                     mutate(sample = subset_size),
                   simplify = FALSE) %>%
           data.table::rbindlist() %>%
           as.data.frame())
  }) %>% data.table::rbindlist() %>%
  as.data.frame()

basic_subsample_means <- basic_subsample %>%
  group_by(bin, sample) %>% filter(species >= 5) %>%
  summarise(y0 = mean(y0), y5 = mean(y5, .05),
            y25 = mean(y25, 0.25), y50 = mean(y50, 0.5),
            y75 = mean(y75, 0.75), y95 = mean(y95, 0.95),
            y100 = mean(y100, 1), n_sp = unique(species),
            raw_sd = sd(avg, na.rm = TRUE), raw_mean = mean(avg),
            wtd_mean = wtd.mean(avg, 1/stddev^2), wtd_var = wtd.var(avg, 1/stddev^2))

ggplot(data = basic_subsample_means, aes(x = bin, y = wtd_mean, color = sample, group = sample)) +
  geom_point(position = position_dodge(width = .9)) +
  geom_linerange(aes(ymin = wtd_mean - 1.96 * sqrt(wtd_var), ymax = wtd_mean + 1.96 * sqrt(wtd_var)), position = position_dodge(width = .9)) +
  scale_x_discrete(name = "Time (Ma)", labels = gsub(" ","\n", rev(time_scale$name))) +
  scale_y_continuous(name = "ln Mass (g)", lim = c(0,15)) +
  scale_color_viridis() +
  theme_classic(base_size = 16) +
  theme(axis.text = element_text(color = "black"), axis.text.x = element_text(color = "black", angle = 90, vjust = .5), axis.ticks = element_line(color = "black"))
ggsave("../figures/Mammal Subsample Meta Means.pdf", device = "pdf", width = 10, height = 10)

#subsampling separate diets####
n_subsets <- 100 #number of replicates for each subset size
subset_sizes <- seq(5,100,5)

diet_subsample <- lapply(subset_sizes, function(subset_size) {
  print(subset_size)
  return(replicate(n_subsets, mamm_per_bin %>%
                     group_by(bin, Recoded_Diet) %>% 
                     mutate(species = n()) %>%
                     group_by(bin, Recoded_Diet, species) %>% 
                     sample_n(subset_size, replace = TRUE) %>%
                     summarise(y0 = quantile(lnMass_g, 0), y5 = quantile(lnMass_g, .05),
                               y25 = quantile(lnMass_g, 0.25), y50 = quantile(lnMass_g, 0.5),
                               avg = mean(lnMass_g, na.rm = TRUE), y75 = quantile(lnMass_g, 0.75),
                               y95 = quantile(lnMass_g, 0.95), y100 = quantile(lnMass_g, 1),
                               stddev = sd(lnMass_g, na.rm = TRUE),
                               .groups = "drop") %>%
                     mutate(sample = subset_size),
                   simplify = FALSE) %>%
           data.table::rbindlist() %>%
           as.data.frame())
  }) %>% data.table::rbindlist() %>%
  as.data.frame()

diet_subsample_means <- diet_subsample %>%
  filter(species >= 4) %>%
  group_by(bin, Recoded_Diet, species, sample) %>% 
  summarise(y0 = mean(y0), y5 = mean(y5, .05),
            y25 = mean(y25, 0.25), y50 = mean(y50, 0.5),
            y75 = mean(y75, 0.75), y95 = mean(y95, 0.95),
            y100 = mean(y100, 1),
            raw_sd = sd(avg, na.rm = TRUE), raw_mean = mean(avg),
            wtd_mean = wtd.mean(avg, 1/stddev^2), wtd_var = wtd.var(avg, 1/stddev^2))

#plot distributions of means across all samples
ggplot(data = diet_subsample %>% filter(Recoded_Diet != "insectivore"), aes(x = bin, y = avg, color = Recoded_Diet)) +
  geom_boxplot(position = position_dodge(preserve = "single")) +
  scale_x_discrete(name = "Time (Ma)", labels = gsub(" ","\n", rev(time_scale$name))) +
  scale_y_continuous(name = "ln Mass (g)", lim = c(0,15)) +
  theme_classic(base_size = 24) +
  theme(axis.text = element_text(color = "black"), axis.text.x = element_text(color = "black", angle = 90, vjust = .5), axis.ticks = element_line(color = "black")) +
  scale_color_manual(values = colors4, name = "Diet") +
  facet_wrap(~sample)
ggsave("../figures/Mammal Diets Subsample Mean Boxplots.pdf", device = "pdf", width = 24, height = 20)

diet_subsample_means$bin_num <- as.numeric(diet_subsample_means$bin)
#plot summary distributions
ggplot(data = diet_subsample_means, aes(x = bin_num, fill = Recoded_Diet, group = interaction(bin, Recoded_Diet))) +
  geom_boxplot(aes(ymin = y0, lower = y25, middle = y50, upper = y75, ymax = y100), stat = "identity", width = .8,
               position = position_dodge2(preserve = "single", width = .95, padding = .15), color = "black") +
  scale_x_continuous(name = "Time (Ma)", limits = c(0.5, 9.5), labels = rev(c(0, epochs$max_age[1:9])), breaks = seq(0.5, 9.5, 1), expand = c(0,0)) +
  scale_y_continuous(name = "ln Mass (g)", breaks = seq(1, 17, 2)) +
  coord_cartesian(ylim = c(0, 17)) +
  theme_classic(base_size = 24) +
  theme(axis.text = element_text(color = "black"), axis.text.x = element_text(color = "black", angle = 90, vjust = .5), axis.ticks = element_line(color = "black")) +
  scale_fill_manual(values = colors4, name = "Diet") +
  facet_wrap(~sample)
ggsave("../figures/Mammal Diets Subsample Boxplots.pdf", device = "pdf", width = 24, height = 20)

#pull out sample size=20
diet_subsample$age_bin_num <- as.numeric(diet_subsample$bin)
(gg <- ggplot(data = subset(diet_subsample, sample==20 & species >= 4 & Recoded_Diet != "insectivore"), aes(x = age_bin_num, y = avg, fill = Recoded_Diet, group = interaction(bin, Recoded_Diet))) +
    annotate("rect", xmin = seq(0.5, 8.5, 1), xmax = seq(1.5, 9.5, 1), ymin = 0, ymax = 15, fill = rep_len(c("grey90", "white"), length.out = 9)) +
    geom_boxplot(position = position_dodge2(preserve = "single", width = .95, padding = .15), color = "black") +
    scale_x_continuous(name = "Time (Ma)", limits = c(0.5, 9.5), labels = rev(c(0, epochs$max_age[1:9])), breaks = seq(0.5, 9.5, 1), expand = c(0,0)) +
    scale_y_continuous(name = "ln Mass (g)", breaks = seq(3, 11, 2)) +
    coord_cartesian(ylim = c(2,12)) +
    theme_classic(base_size = 24) +
    theme(axis.text = element_text(color = "black"), axis.ticks = element_line(color = "black", size = .75),
          panel.border = element_rect(color = "black", fill = NA, size = 1.5), axis.line = element_blank(),
          legend.position = c(.5,.97), legend.direction = "horizontal", legend.background = element_rect(color = NA, fill = NA)) +
    scale_fill_manual(name = NULL, values = colors4) +
    annotation_custom(phylopics[[1]], 1.45, 0.55, ymin = 1.5, ymax = 2.5) +
    annotation_custom(phylopics[[2]], 1.55, 2.45, ymin = 1.5, ymax = 2.5) +
    annotation_custom(phylopics[[3]], 2.55, 3.45, ymin = 1.5, ymax = 2.5) +
    annotation_custom(phylopics[[4]], 3.55, 4.45, ymin = 1.5, ymax = 2.5) +
    annotation_custom(phylopics[[5]], 4.55, 5.45, ymin = 1.5, ymax = 2.5) +
    annotation_custom(phylopics[[6]], 5.55, 6.45, ymin = 1.5, ymax = 2.5) +
    annotation_custom(phylopics[[7]], 6.55, 7.45, ymin = 1.5, ymax = 2.5) +
    annotation_custom(phylopics[[8]], 7.55, 8.45, ymin = 1.5, ymax = 2.5) +
    annotation_custom(phylopics[[9]], 8.55, 9.45, ymin = 1.5, ymax = 2.5))
discrete_periods <- periods
discrete_periods$max_age[1:4] <- c(2, 4, 7, 9)
discrete_periods$min_age[1:4] <- c(0, 2, 4, 7)
discrete_epochs <- epochs
discrete_epochs$max_age[1:9] <- 1:9
discrete_epochs$min_age[1:9] <- 0:8
discrete_epochs$name[8:9] <- c("Late\nCretaceous", "Early\nCretaceous")
(geo_plot <- gggeo_scale(gggeo_scale(ggplotGrob(gg), lims = c(9, 0), dat = discrete_periods, abbrv = FALSE, size = 6, skip = NULL, lwd = .75),
                         dat = discrete_epochs, abbrv = FALSE, skip = NULL, size = 5, lwd = .75, bord = c("left", "right"), height = unit(2.5, "line")))
ggsave("../figures/Mammal Diets Subsample Means-20 Sample.pdf", geo_plot, device = "pdf", width = 12, height = 12)

#Means and standard deviations
ggplot(data = subset(diet_subsample_means, Recoded_Diet != "insectivore"), aes(x = bin, y = raw_mean, color = Recoded_Diet, group = sample)) +
  geom_point(position = position_dodge(width = .9)) +
  geom_errorbar(aes(ymin = raw_mean - 1.96*raw_sd, ymax = raw_mean + 1.96*raw_sd), position = position_dodge(width = .9)) +
  scale_x_discrete(name = "Time (Ma)", labels = gsub(" ","\n", rev(time_scale$name))) +
  scale_y_continuous(name = "ln Mass (g)", lim = c(0,15)) +
  theme_classic(base_size = 16) +
  theme(axis.text = element_text(color = "black"), axis.text.x = element_text(color = "black", angle = 90, vjust = .5), axis.ticks = element_line(color = "black")) +
  scale_color_manual(values = colors3, name = "Diet")
ggsave("../figures/Mammal Diets Subsample Means.pdf", device = "pdf", width = 10, height = 10)

#Pull out sample=20
diet_subsample_means$bin_num <- as.numeric(diet_subsample_means$bin)
(gg <- ggplot(data = subset(diet_subsample_means, sample==20 & Recoded_Diet != "insectivore"), aes(x = bin_num, y = raw_mean, color = Recoded_Diet, group = interaction(bin, Recoded_Diet))) +
    annotate("rect", xmin = seq(0.5, 8.5, 1), xmax = seq(1.5, 9.5, 1), ymin = 0, ymax = 15, fill = rep_len(c("grey90", "white"), length.out = 9)) +
    geom_point(position = position_dodge2(preserve = "single", width = .9, padding = .15), size = 3.5) +
    geom_errorbar(aes(ymin = raw_mean - 1.96*raw_sd, ymax = raw_mean + 1.96*raw_sd), position = position_dodge2(preserve = "single", width = .95, padding = .15), size = 1.75) +
    geom_text(aes(label = species, y = raw_mean + 1.96*raw_sd + .2), position = position_dodge2(preserve = "single", width = .9, padding = .15), size = 5, show.legend = FALSE) +
    scale_x_continuous(name = "Time (Ma)", limits = c(0.5, 9.5), labels = rev(c(0, epochs$max_age[1:9])), breaks = seq(0.5, 9.5, 1), expand = c(0,0)) +
    scale_y_continuous(name = "ln Mass (g)", breaks = seq(3, 11, 2)) +
    coord_cartesian(ylim = c(1.5,12)) +
    theme_classic(base_size = 24) +
    theme(axis.text = element_text(color = "black"), axis.ticks = element_line(color = "black", size = .75),
          panel.border = element_rect(color = "black", fill = NA, size = 1.5), axis.line = element_blank(),
          legend.position = c(.5,.97), legend.direction = "horizontal", legend.background = element_rect(color = NA, fill = NA)) +
    scale_color_manual(name = NULL, values = colors4) +
    annotation_custom(phylopics[[1]], 1.45, 0.55, ymin = 1, ymax = 2) +
    annotation_custom(phylopics[[2]], 1.55, 2.45, ymin = 1, ymax = 2) +
    annotation_custom(phylopics[[3]], 2.55, 3.45, ymin = 1, ymax = 2) +
    annotation_custom(phylopics[[4]], 3.55, 4.45, ymin = 1, ymax = 2) +
    annotation_custom(phylopics[[5]], 4.55, 5.45, ymin = 1, ymax = 2) +
    annotation_custom(phylopics[[6]], 5.55, 6.45, ymin = 1, ymax = 2) +
    annotation_custom(phylopics[[7]], 6.55, 7.45, ymin = 1, ymax = 2) +
    annotation_custom(phylopics[[8]], 7.55, 8.45, ymin = 1, ymax = 2) +
    annotation_custom(phylopics[[9]], 8.55, 9.45, ymin = 1, ymax = 2))
discrete_periods <- periods
discrete_periods$max_age[1:4] <- c(2, 4, 7, 9)
discrete_periods$min_age[1:4] <- c(0, 2, 4, 7)
discrete_epochs <- epochs
discrete_epochs$max_age[1:9] <- 1:9
discrete_epochs$min_age[1:9] <- 0:8
discrete_epochs$name[8:9] <- c("Late\nCretaceous", "Early\nCretaceous")
(geo_plot <- gggeo_scale(gggeo_scale(ggplotGrob(gg), lims = c(9, 0), dat = discrete_periods, abbrv = FALSE, size = 6, skip = NULL, lwd = .75),
                         dat = discrete_epochs, abbrv = FALSE, skip = NULL, size = 5, lwd = .75, bord = c("left", "right"), height = unit(2.5, "line")))
ggsave("../figures/Mammal Diets Subsample Means-20 Sample.pdf", geo_plot, device = "pdf", width = 12, height = 12)

#Meta-means and weighted standard deviations
ggplot(data = subset(diet_subsample_means, Recoded_Diet != "insectivore"), aes(x = bin, y = wtd_mean, color = Recoded_Diet, group = sample)) +
  geom_point(position = position_dodge(width = .9)) +
  geom_errorbar(aes(ymin = wtd_mean - 1.96 * sqrt(wtd_var), ymax = wtd_mean + 1.96 * sqrt(wtd_var)), position = position_dodge(width = .9)) +
  scale_x_discrete(name = "Time (Ma)", labels = gsub(" ","\n", rev(time_scale$name))) +
  scale_y_continuous(name = "ln Mass (g)", lim = c(0,15)) +
  theme_classic(base_size = 16) +
  theme(axis.text = element_text(color = "black"), axis.text.x = element_text(color = "black", angle = 90, vjust = .5), axis.ticks = element_line(color = "black")) +
  scale_color_manual(values = colors4, name = "Diet")
ggsave("../figures/Mammal Diets Subsample Meta Means.pdf", device = "pdf", width = 10, height = 10)

#pull out sample size=20
(gg <- ggplot(data = subset(diet_subsample_means, sample==20 & Recoded_Diet != "insectivore"), aes(x = bin_num, y = wtd_mean, color = Recoded_Diet, group = interaction(bin, Recoded_Diet))) +
    annotate("rect", xmin = seq(0.5, 8.5, 1), xmax = seq(1.5, 9.5, 1), ymin = 0, ymax = 15, fill = rep_len(c("grey90", "white"), length.out = 9)) +
    geom_point(position = position_dodge2(preserve = "single", width = .9, padding = .15), size = 3) +
    geom_errorbar(aes(ymin = wtd_mean - 1.96 * sqrt(wtd_var), ymax = wtd_mean + 1.96 * sqrt(wtd_var)), position = position_dodge2(preserve = "single", width = .95, padding = .15), size = 1.75) +
    geom_text(aes(label = species, y = wtd_mean + 1.96 * sqrt(wtd_var) + .2), position = position_dodge2(preserve = "single", width = .9, padding = .15), size = 5, show.legend = FALSE) +
    scale_x_continuous(name = "Time (Ma)", limits = c(0.5, 9.5), labels = rev(c(0, epochs$max_age[1:9])), breaks = seq(0.5, 9.5, 1), expand = c(0,0)) +
    scale_y_continuous(name = "ln Mass (g)", breaks = seq(3, 11, 2)) +
    coord_cartesian(ylim = c(2,12)) +
    theme_classic(base_size = 24) +
    theme(axis.text = element_text(color = "black"), axis.ticks = element_line(color = "black", size = .75),
          panel.border = element_rect(color = "black", fill = NA, size = 1.5), axis.line = element_blank(),
          legend.position = c(.5,.97), legend.direction = "horizontal", legend.background = element_rect(color = NA, fill = NA)) +
    scale_color_manual(name = NULL, values = colors4) +
    annotation_custom(phylopics[[1]], 1.45, 0.55, ymin = 1.5, ymax = 2.5) +
    annotation_custom(phylopics[[2]], 1.55, 2.45, ymin = 1.5, ymax = 2.5) +
    annotation_custom(phylopics[[3]], 2.55, 3.45, ymin = 1.5, ymax = 2.5) +
    annotation_custom(phylopics[[4]], 3.55, 4.45, ymin = 1.5, ymax = 2.5) +
    annotation_custom(phylopics[[5]], 4.55, 5.45, ymin = 1.5, ymax = 2.5) +
    annotation_custom(phylopics[[6]], 5.55, 6.45, ymin = 1.5, ymax = 2.5) +
    annotation_custom(phylopics[[7]], 6.55, 7.45, ymin = 1.5, ymax = 2.5) +
    annotation_custom(phylopics[[8]], 7.55, 8.45, ymin = 1.5, ymax = 2.5) +
    annotation_custom(phylopics[[9]], 8.55, 9.45, ymin = 1.5, ymax = 2.5))
discrete_periods <- periods
discrete_periods$max_age[1:4] <- c(2, 4, 7, 9)
discrete_periods$min_age[1:4] <- c(0, 2, 4, 7)
discrete_epochs <- epochs
discrete_epochs$max_age[1:9] <- 1:9
discrete_epochs$min_age[1:9] <- 0:8
discrete_epochs$name[8:9] <- c("Late\nCretaceous", "Early\nCretaceous")
(geo_plot <- gggeo_scale(gggeo_scale(ggplotGrob(gg), lims = c(9, 0), dat = discrete_periods, abbrv = FALSE, size = 6, skip = NULL, lwd = .75),
                         dat = discrete_epochs, abbrv = FALSE, skip = NULL, size = 5, lwd = .75, bord = c("left", "right"), height = unit(2.5, "line")))
ggsave("../figures/Mammal Diets Subsample Meta Means-20 Sample.pdf", geo_plot, device = "pdf", width = 12, height = 12)

#bootstrap separate diets and calculate quantiles####
diets <- levels(mamm_diet$Recoded_Diet)
n_diets <- length(diets)
n_reps <- 100

diet_bootstrap <- replicate(n = n_reps,
                            mamm_per_bin %>% 
                            group_by(bin, Recoded_Diet) %>%
                            sample_frac(replace = TRUE) %>%
                            summarise(y0 = quantile(lnMass_g, 0), y5 = quantile(lnMass_g, .05),
                                      y25 = quantile(lnMass_g, 0.25), y50 = quantile(lnMass_g, 0.5),
                                      avg = mean(lnMass_g, na.rm = TRUE), y75 = quantile(lnMass_g, 0.75),
                                      y95 = quantile(lnMass_g, 0.95), y100 = quantile(lnMass_g, 1),
                                      stddev = sd(lnMass_g, na.rm = TRUE), species = n(),
                                      .groups = "drop"),
                            simplify = FALSE) %>%
  data.table::rbindlist() %>%
  as.data.frame()


diet_bootstrap_means <- diet_bootstrap %>%
  filter(species >= 5) %>%
  group_by(bin, Recoded_Diet, species) %>% 
  summarise(y0 = mean(y0), y5 = mean(y5, .05),
            y25 = mean(y25, 0.25), y50 = mean(y50, 0.5),
            y75 = mean(y75, 0.75), y95 = mean(y95, 0.95),
            y100 = mean(y100, 1),
            raw_sd = sd(avg, na.rm = TRUE), raw_mean = mean(avg),
            wtd_mean = wtd.mean(avg, 1/stddev^2), wtd_var = wtd.var(avg, 1/stddev^2))

#plot boxplots
diet_bootstrap_means$bin_num <- as.numeric(diet_bootstrap_means$bin)
(gg <- ggplot(data = subset(diet_bootstrap_means, Recoded_Diet != "insectivore"), aes(x = bin_num, fill = Recoded_Diet, group = interaction(bin, Recoded_Diet))) +
    annotate("rect", xmin = seq(0.5, 8.5, 1), xmax = seq(1.5, 9.5, 1), ymin = -Inf, ymax = Inf, fill = rep_len(c("grey90", "white"), length.out = 9)) +
    geom_boxplot(aes(ymin = y0, lower = y25, middle = y50, upper = y75, ymax = y100), stat = "identity", width = .8,
                 position = position_dodge2(preserve = "single", width = .95, padding = .15), color = "black") +
    scale_x_continuous(name = "Time (Ma)", limits = c(0.5, 9.5), labels = rev(c(0, epochs$max_age[1:9])), breaks = seq(0.5, 9.5, 1), expand = c(0,0)) +
    scale_y_continuous(name = "ln Mass (g)", breaks = seq(1, 17, 2)) +
    coord_cartesian(ylim = c(0, 17)) +
    theme_classic(base_size = 24) +
    theme(axis.text = element_text(color = "black"), axis.ticks = element_line(color = "black", size = .75),
          panel.border = element_rect(color = "black", fill = NA, size = 1.5), axis.line = element_blank(),
          legend.position = c(.5,.97), legend.direction = "horizontal", legend.background = element_rect(color = NA, fill = NA)) +
    scale_fill_manual(name = NULL, values = colors4) +
    annotation_custom(phylopics[[1]], 1.45, 0.55, ymin = -.75, ymax = .25) +
    annotation_custom(phylopics[[2]], 1.55, 2.45, ymin = -.75, ymax = .25) +
    annotation_custom(phylopics[[3]], 2.55, 3.45, ymin = -.75, ymax = .25) +
    annotation_custom(phylopics[[4]], 3.55, 4.45, ymin = -.75, ymax = .25) +
    annotation_custom(phylopics[[5]], 4.55, 5.45, ymin = -.75, ymax = .25) +
    annotation_custom(phylopics[[6]], 5.55, 6.45, ymin = -.75, ymax = .25) +
    annotation_custom(phylopics[[7]], 6.55, 7.45, ymin = -.75, ymax = .25) +
    annotation_custom(phylopics[[8]], 7.55, 8.45, ymin = -.75, ymax = .25) +
    annotation_custom(phylopics[[9]], 8.55, 9.45, ymin = -.75, ymax = .25))
discrete_periods <- periods
discrete_periods$max_age[1:4] <- c(2, 4, 7, 9)
discrete_periods$min_age[1:4] <- c(0, 2, 4, 7)
discrete_epochs <- epochs
discrete_epochs$max_age[1:9] <- 1:9
discrete_epochs$min_age[1:9] <- 0:8
discrete_epochs$name[8:9] <- c("Late\nCretaceous", "Early\nCretaceous")
(geo_plot <- gggeo_scale(gggeo_scale(ggplotGrob(gg), lims = c(9, 0), dat = discrete_periods, abbrv = FALSE, size = 6, skip = NULL, lwd = .75),
                         dat = discrete_epochs, abbrv = FALSE, skip = NULL, size = 5, lwd = .75, bord = c("left", "right"), height = unit(2.5, "line")))
ggsave("../figures/Mammal Diets Bootstrap Boxplots.pdf", geo_plot, device = "pdf", width = 12, height = 12)

#plot boxplots of means
diet_bootstrap$bin_num <- as.numeric(diet_bootstrap$bin)
(gg <- ggplot(data = subset(diet_bootstrap, species >= 5 & Recoded_Diet != "insectivore"), aes(x = bin_num, y = avg, fill = Recoded_Diet, group = interaction(bin, Recoded_Diet))) +
    annotate("rect", xmin = seq(0.5, 8.5, 1), xmax = seq(1.5, 9.5, 1), ymin = 0, ymax = 15, fill = rep_len(c("grey90", "white"), length.out = 9)) +
    geom_boxplot(position = position_dodge2(preserve = "single", width = .95, padding = .15), color = "black") +
    scale_x_continuous(name = "Time (Ma)", limits = c(0.5, 9.5), labels = rev(c(0, epochs$max_age[1:9])), breaks = seq(0.5, 9.5, 1), expand = c(0,0)) +
    scale_y_continuous(name = "ln Mass (g)", breaks = seq(2, 10, 2)) +
    coord_cartesian(ylim = c(1.5,11)) +
    theme_classic(base_size = 24) +
    theme(axis.text = element_text(color = "black"), axis.ticks = element_line(color = "black", size = .75),
          panel.border = element_rect(color = "black", fill = NA, size = 1.5), axis.line = element_blank(),
          legend.position = c(.5,.97), legend.direction = "horizontal", legend.background = element_rect(color = NA, fill = NA)) +
    scale_fill_manual(name = NULL, values = colors4) +
    annotation_custom(phylopics[[1]], 1.45, 0.55, ymin = .9, ymax = 1.9) +
    annotation_custom(phylopics[[2]], 1.55, 2.45, ymin = .9, ymax = 1.9) +
    annotation_custom(phylopics[[3]], 2.55, 3.45, ymin = .9, ymax = 1.9) +
    annotation_custom(phylopics[[4]], 3.55, 4.45, ymin = .9, ymax = 1.9) +
    annotation_custom(phylopics[[5]], 4.55, 5.45, ymin = .9, ymax = 1.9) +
    annotation_custom(phylopics[[6]], 5.55, 6.45, ymin = .9, ymax = 1.9) +
    annotation_custom(phylopics[[7]], 6.55, 7.45, ymin = .9, ymax = 1.9) +
    annotation_custom(phylopics[[8]], 7.55, 8.45, ymin = .9, ymax = 1.9) +
    annotation_custom(phylopics[[9]], 8.55, 9.45, ymin = .9, ymax = 1.9))
discrete_periods <- periods
discrete_periods$max_age[1:4] <- c(2, 4, 7, 9)
discrete_periods$min_age[1:4] <- c(0, 2, 4, 7)
discrete_epochs <- epochs
discrete_epochs$max_age[1:9] <- 1:9
discrete_epochs$min_age[1:9] <- 0:8
discrete_epochs$name[8:9] <- c("Late\nCretaceous", "Early\nCretaceous")
(geo_plot <- gggeo_scale(gggeo_scale(ggplotGrob(gg), lims = c(9, 0), dat = discrete_periods, abbrv = FALSE, size = 6, skip = NULL, lwd = .75),
                         dat = discrete_epochs, abbrv = FALSE, skip = NULL, size = 5, lwd = .75, bord = c("left", "right"), height = unit(2.5, "line")))
ggsave("../figures/Mammal Diets Bootstrap Means Boxplots.pdf", geo_plot, device = "pdf", width = 12, height = 12)

#plot weighted means and weighted standard deviations
(gg <- ggplot(data = subset(diet_bootstrap_means, Recoded_Diet != "insectivore"), aes(x = bin_num, y = wtd_mean, color = Recoded_Diet, group = interaction(bin, Recoded_Diet))) +
    annotate("rect", xmin = seq(0.5, 8.5, 1), xmax = seq(1.5, 9.5, 1), ymin = 0, ymax = 15, fill = rep_len(c("grey90", "white"), length.out = 9)) +
    geom_point(position = position_dodge2(preserve = "single", width = .9, padding = .15), size = 3) +
    geom_errorbar(aes(ymin = wtd_mean - 1.96 * sqrt(wtd_var), ymax = wtd_mean + 1.96 * sqrt(wtd_var)), position = position_dodge2(preserve = "single", width = .95, padding = .15), size = 1.75) +
    geom_text(aes(label = species, y = wtd_mean + 1.96 * sqrt(wtd_var) + .2), position = position_dodge2(preserve = "single", width = .9, padding = .15), size = 5, show.legend = FALSE) +
    scale_x_continuous(name = "Time (Ma)", limits = c(0.5, 9.5), labels = rev(c(0, epochs$max_age[1:9])), breaks = seq(0.5, 9.5, 1), expand = c(0,0)) +
    scale_y_continuous(name = "ln Mass (g)", breaks = seq(3, 11, 2)) +
    coord_cartesian(ylim = c(1.5,11)) +
    theme_classic(base_size = 24) +
    theme(axis.text = element_text(color = "black"), axis.ticks = element_line(color = "black", size = .75),
          panel.border = element_rect(color = "black", fill = NA, size = 1.5), axis.line = element_blank(),
          legend.position = c(.5,.97), legend.direction = "horizontal", legend.background = element_rect(color = NA, fill = NA)) +
    scale_color_manual(name = NULL, values = colors4) +
    annotation_custom(phylopics[[1]], 1.45, 0.55, ymin = .9, ymax = 1.9) +
    annotation_custom(phylopics[[2]], 1.55, 2.45, ymin = .9, ymax = 1.9) +
    annotation_custom(phylopics[[3]], 2.55, 3.45, ymin = .9, ymax = 1.9) +
    annotation_custom(phylopics[[4]], 3.55, 4.45, ymin = .9, ymax = 1.9) +
    annotation_custom(phylopics[[5]], 4.55, 5.45, ymin = .9, ymax = 1.9) +
    annotation_custom(phylopics[[6]], 5.55, 6.45, ymin = .9, ymax = 1.9) +
    annotation_custom(phylopics[[7]], 6.55, 7.45, ymin = .9, ymax = 1.9) +
    annotation_custom(phylopics[[8]], 7.55, 8.45, ymin = .9, ymax = 1.9) +
    annotation_custom(phylopics[[9]], 8.55, 9.45, ymin = .9, ymax = 1.9))
discrete_periods <- periods
discrete_periods$max_age[1:4] <- c(2, 4, 7, 9)
discrete_periods$min_age[1:4] <- c(0, 2, 4, 7)
discrete_epochs <- epochs
discrete_epochs$max_age[1:9] <- 1:9
discrete_epochs$min_age[1:9] <- 0:8
discrete_epochs$name[8:9] <- c("Late\nCretaceous", "Early\nCretaceous")
(geo_plot <- gggeo_scale(gggeo_scale(ggplotGrob(gg), lims = c(9, 0), dat = discrete_periods, abbrv = FALSE, size = 6, skip = NULL, lwd = .75),
                         dat = discrete_epochs, abbrv = FALSE, skip = NULL, size = 5, lwd = .75, bord = c("left", "right"), height = unit(2.5, "line")))
ggsave("../figures/Mammal Diets Bootstrap Meta Means.pdf", geo_plot, device = "pdf", width = 12, height = 12)

#data for table/figure####
#using rob's data####
Atraits <- readRDS("../data/v_trait.rds")
mam_pres <- Atraits %>% 
  dplyr::mutate(diet_plant = diet_fruit + diet_nect + diet_seed + diet_planto) %>% 
  dplyr::mutate(diet_vert = diet_vend + diet_vect + diet_vfish + diet_vunk + diet_scav) %>%   
  dplyr::filter(realm == "terrestrial" & class == "Mammalia") %>% 
  dplyr::filter(!is.na(diet_plant)) %>% 
  dplyr::mutate(body_mass_median = log(body_mass_median)) %>% 
  dplyr::select(binomial, body_mass_median, diet_plant, diet_vert, diet_inv)

phylacine <- readRDS("../data/v_phylacine_trait.rds")

phylacine <- phylacine %>%
  dplyr::mutate(body_mass_median = log(mass)) %>% 
  dplyr::select(binomial, body_mass_median, diet_plant, diet_vert, diet_inv)

pleis <- dplyr::bind_rows(mam_pres, phylacine)

pleis <- pleis %>%
  # omnivores
  dplyr::mutate(omnivore = apply(dplyr::select(pleis, diet_plant, diet_vert, diet_inv), 1, max)) %>%
  dplyr::mutate(diet_5cat = ifelse(omnivore <=50, 4, max.col(dplyr::select(., diet_plant:diet_inv))))
pleis$diet_5cat = plyr::mapvalues(pleis$diet_5cat, 
                                  from = c("1", "2", "3", "4"), 
                                  to = c("Herbivore", "Carnivore", "Invertivore", "Omnivore"))

pleis <- dplyr::mutate(pleis, diet_5cat = factor(diet_5cat, levels = c("Herbivore", "Omnivore", "Invertivore", "Carnivore")))

pleis <- pleis %>%
  mutate(time = ifelse(binomial %in% mam_pres$binomial, "Modern", "Pleistocene"))

pleis %>%
  group_by(diet_5cat, time) %>% summarise(min = min(body_mass_median, na.rm = TRUE), which_min = binomial[which.min(body_mass_median)], max = max(body_mass_median, na.rm = TRUE), which_max = binomial[which.max(body_mass_median)])

#using Kate's data####
mom_data_per_bin <- as.data.frame(matrix(NA, nrow = 0, ncol = ncol(mom_data) + 1, dimnames = list(c(),c(colnames(mom_data), "bin"))))
#pleistocene and holocene
for(i in 1:2){
  dat <- subset(mom_data, LAD <= time_scale$max_age[i] & FAD >= time_scale$min_age[i])
  if(nrow(dat) > 0) {
    dat$bin <- time_scale$name[i]
    mom_data_per_bin <- rbind(mom_data_per_bin, dat)
  }
}
#modern
dat <- subset(mom_data, Status == "extant")
dat$bin <- "Modern"
mom_data_per_bin <- rbind(mom_data_per_bin, dat)
#future
dat <- subset(dat, !(Time.of.extinction..LP.125.75..EP.30.50..TP.15.10..Holocene.10.1..historical..1.EW..future...CR.EN.VU..future2...that.plus.NT. %in% c("Future", "Future2")))
dat$bin = "Future"
mom_data_per_bin <- rbind(mom_data_per_bin, dat)

mom_stats <- mom_data_per_bin %>%
  group_by(Recoded_Diet, bin) %>%
  dplyr::summarise(min_size = min(lnMass_g, na.rm = TRUE), which_min = Genus_species[which.min(lnMass_g)],
            max_size = max(lnMass_g, na.rm = TRUE), which_max = Genus_species[which.max(lnMass_g)],
            med_size = median(lnMass_g, na.rm = TRUE), .groups = "drop")
write.csv(mom_stats, "../tables/mom_stats.csv", row.names = FALSE)

mom_stats$diet_num <- as.numeric(factor(mom_stats$Recoded_Diet, levels = c("herbivore", "omnivore", "invertivore", "carnivore")))

box_colors <- setNames(rep("white", length(interaction(mom_stats$Recoded_Diet, mom_stats$bin))),
                       interaction(mom_stats$Recoded_Diet, mom_stats$bin))
box_colors[grepl("Modern", names(box_colors))] <- "grey80"
box_colors[grepl("Future", names(box_colors)) & grepl("carnivore", names(box_colors))] <- colors4["carnivore"]
box_colors[grepl("Future", names(box_colors)) & grepl("invertivore", names(box_colors))] <- colors4["invertivore"]
box_colors[grepl("Future", names(box_colors)) & grepl("omnivore", names(box_colors))] <- colors4["omnivore"]
box_colors[grepl("Future", names(box_colors)) & grepl("herbivore", names(box_colors))] <- colors4["herbivore"]

diet_stats <- mom_stats %>%
  group_by(Recoded_Diet, diet_num) %>%
  summarise(minimum = min(min_size), maximum = max(max_size), avg = (max(max_size) + min(min_size))/2,
            med = max(med_size), extant_med = max(med_size[bin == "Modern"]), future_med = max(med_size[bin == "Future"]),
            extant_max = max(max_size[bin == "Modern"]), future_max = max(max_size[bin == "Future"]), .groups = "drop")
diet_stats$min_mech <- c("Plant Digestive\nPhysiology", "Plant Digestive\nPhysiology",
                         "Metabolic\nPhysiology", "Larger\nThan Prey")[diet_stats$diet_num]
diet_stats$max_mech <- c("Low Quality\nPlant\nAvailability", "High Quality\nPlant\nAvailability",
                         "High Quality\nPlant and Insect\nAvailability", "Hunting\nTradeoffs")[diet_stats$diet_num]

#phylopics
uuids2 <- c("Smallest Carnivore" = "20b6096e-2d6d-43c4-acda-fd74f0f91d48", #Mustela nivalis
           "Largest Carnivore" = "8d4b7834-9dd3-4f5a-84a7-99fa159a112a", #Arctodus simus
           "Smallest Invertivore" = "822c549b-b29b-47eb-9fe3-dc5bbb0abccb", #Soricidae
           "Largest Invertivore" = "cad2eeb5-7827-4b3d-b406-e20864a71637", #Pampatherium
           "Smallest Omnivore" = "81930c02-5f26-43f7-9c19-e9831e780e53", #Sigmodontinae
           "Largest Omnivore" = "0cd82109-bb1c-4e08-ab11-c845d8a82eba", #Ursus arctos
           "Smallest Herbivore" = "92989e35-4e68-4a2d-b3a2-191ba9da671a", #Mus
           "Largest Herbivore" = "43d2a4af-991c-4236-8455-f62271ab73e7" #Mammuthus
)

phylopics2 <- lapply(uuids2, getPhyloPic)

#make them all face to the right
phylopics2[[2]] <- flip(phylopics2[[2]], horizontal = TRUE)
phylopics2[[3]] <- flip(phylopics2[[3]], horizontal = TRUE)
phylopics2[[5]] <- flip(phylopics2[[5]], horizontal = TRUE)
phylopics2[[6]] <- flip(phylopics2[[6]], horizontal = TRUE)
#make the mouse black
phylopics2[[7]]@paths <- lapply(phylopics2[[7]]@paths,
                                function(x) {
                                  x@rgb <- "#000000"
                                  return(x)
                                })
phylopics2[[8]] <- flip(phylopics2[[8]], horizontal = TRUE)

phylopics2 <- lapply(phylopics2, pictureGrob)

ggplot(mom_stats) +
  geom_segment(data = subset(diet_stats, Recoded_Diet != "omnivore"), show.legend = FALSE,
               aes(x = minimum, xend = minimum, y = 0, yend = 4.7, color = Recoded_Diet), size = 2.5, linetype = "11") +
  geom_segment(data = subset(diet_stats, Recoded_Diet != "omnivore"), show.legend = FALSE,
               aes(x = maximum, xend = maximum, y = 0, yend = 4.7, color = Recoded_Diet), size = 2.5, linetype = "11") +
  geom_rect(aes(xmin = min_size, xmax = max_size, ymin = diet_num - .4, ymax = diet_num + .4,
                fill = interaction(Recoded_Diet, bin)), show.legend = FALSE, color = "black", size = 1.25) +
  #geom_segment(aes(x = med_size, xend = med_size, y = diet_num - .4, yend = diet_num + .4,
  #              linetype = bin), show.legend = FALSE, color = "black", size = 1.25) +
  geom_text(data = diet_stats, aes(x = avg, y = diet_num, label = paste0(stringr::str_to_sentence(Recoded_Diet), "s")),
            color = "black", size = 12, angle = 90) +
  geom_text(data = subset(diet_stats, Recoded_Diet != "omnivore"),
            aes(x = (maximum + extant_max) / 2, y = diet_num), label = "extinct", angle = 30, size = 5) +
  geom_text(data = subset(diet_stats, Recoded_Diet %in% c("carnivore", "herbivore")),
            aes(x = (extant_max + future_max) / 2, y = diet_num), label = "threatened", angle = 30, size = 5) +
  geom_text(data = subset(diet_stats, Recoded_Diet != "omnivore"),
            aes(x = minimum, y = 5.4, label = min_mech, color = Recoded_Diet),
            angle = 0, size = 6.5, lineheight = .9, show.legend = FALSE) +
  geom_text(data = subset(diet_stats, Recoded_Diet != "omnivore"),
            aes(x = maximum, y = 5.4, label = max_mech, color = Recoded_Diet),
            angle = 0, size = 6.5, lineheight = .9, show.legend = FALSE) +
  annotate("segment", x = c(11, 5), xend = c(5, 11), y = c(5, 5.5), yend = c(5, 5.5), size = 2.5, linetype = "11") +
  annotate("segment", x = c(5.01, 10.99), xend = c(5, 11), y = c(5, 5.5), yend = c(5, 5.5), size = 2.5,
           arrow = arrow(length = unit(0.02, "npc"), type = "closed")) +
  annotate("text", x = 8, y = c(5.15, 5.75), size = 6.5, lineheight = .9, angle = 90,
           label = c("Lower Extinction Risk", "Higher Feeding Efficiency &\nStarvation Resistance")) +
  scale_x_continuous(name = "Mass (kg)", limits = c(0, 17), breaks = log(c(1, 10, 100, 1000, 10000, 100000, 1000000, 10000000)), labels = c(1, 10, 100, 1000, 10000, 100000, 1000000, 10000000)/1000) +
  scale_y_continuous(name = NULL, expand = c(0,0)) +
  coord_flip(ylim = c(.4, 6.1)) +
  scale_fill_manual(values = box_colors) +
  scale_color_manual(values = colors4) +
  theme_classic(base_size = 24) +
  theme(axis.line.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.text.y = element_text(color = "black")) +
  annotation_custom(phylopics2[[1]], 3.1, 3.7, ymin = 3.8, ymax = 4.2) + #3.85
  annotation_custom(phylopics2[[2]], 13.25, 14.75, ymin = 3.7, ymax = 4.3) + #13.5
  annotation_custom(phylopics2[[3]], -0.19, .41, ymin = 2.8, ymax = 3.2) + #0.560
  annotation_custom(phylopics2[[4]], 11.85, 13.35, ymin = 2.6, ymax = 3.4) + #12.2
  annotation_custom(phylopics2[[5]], .7, 1.4, ymin = 1.8, ymax = 2.2) + #1.55
  annotation_custom(phylopics2[[6]], 12.05, 13.55, ymin = 1.7, ymax = 2.3) + #12.3
  annotation_custom(phylopics2[[7]], 0.62, 1.22, ymin = 0.8, ymax = 1.2) + #1.37
  annotation_custom(phylopics2[[8]], 15.95, 17.45, ymin = 0.6, ymax = 1.4)   #16.2
ggsave("../figures/Mammal Diets Mechanisms.pdf", width = 10, height = 13)

#Rates plot####
#calculate differences between fossil epochs
samp_20 <- diet_subsample %>%
  filter(sample == 20) %>%
  mutate(boot = rep(1:n_subsets, each = n()/n_subsets))

#convert to wide (move all time bins for a diet*boot to single row)???
#maybe select only medians for now
med_wide <- samp_20 %>%
  select(bin, Recoded_Diet, y50, boot) %>%
  pivot_wider(names_from = bin, values_from = y50)

med_long <- med_wide %>% 
  #calculate rates of change
  mutate(eoc_diff = Eocene - Paleocene, olig_diff = Oligocene - Eocene,
         mio_diff = Miocene - Oligocene, plio_diff = Pliocene - Miocene,
         plei_diff = Pleistocene - Pliocene, holo_diff = Holocene - Pleistocene) %>%
  #convert to long
  pivot_longer(cols = eoc_diff:holo_diff, names_to = "time", values_to = "diff") %>%
  select(run = boot, diet = Recoded_Diet, time, diff)

#read in future projections data
fut_diff <- readRDS("../data/fut_diff.rds")

# wide to long
fut_diff2 <- fut_diff %>% 
  # rates
  mutate(mod_diff = pres - pleis, fut_100_diff = fut_100 - pres,
         fut_300_diff = fut_300 - fut_100, fut_500_diff = fut_500 - fut_300)

fut_diff_long <- fut_diff2 %>%
  pivot_longer(cols = mod_diff:fut_500_diff, names_to = "time", values_to = "diff") %>%
  select(run, diet = diet_5cat, time, diff)

bin_lengths <- data.frame(time = c("eoc", "olig", "mio", "plio", "plei", "holo",
                                   "mod", "fut_100", "fut_300", "fut_500"),
                          length = c(16.05000, 16.48500, 14.28350, 10.22100, 2.66065, 1.29400,
                                     .126, .0001, .0002, .0002))

#combine fossil and future rates
all_diffs <- rbind(med_long, fut_diff_long) %>%
  mutate(time = factor(gsub("_diff", "", time),
                       levels = c("eoc", "olig", "mio", "plio", "plei", "holo",
                                  "mod", "fut_100", "fut_300", "fut_500")),
         time_num = as.numeric(time)) %>%
  merge(bin_lengths) %>%
  mutate(rate = diff/length, abs_rate = abs(rate), corr_rate = rate - min(rate),
         log_length = log10(length))

#https://www.umass.edu/landeco/teaching/multivariate/readings/McCune.and.Grace.2002.chapter9.pdf
#log transformation that preserves zeros
c <- trunc(log10(min(all_diffs$corr_rate[all_diffs$corr_rate > 0], na.rm = TRUE)))
all_diffs$log_corr_rate <- log10(all_diffs$corr_rate + 10^c) - c

# non-parametric confidence interval
all_diffs_ci <- all_diffs %>%
  group_by(diet, time, time_num, length) %>%
  summarise(med = median(diff), low = quantile(diff, 0.025), upp = quantile(diff, 0.975))

#plot rates against bin lengths
ggplot(all_diffs, aes(x = length, y = log_corr_rate)) +
  geom_point(aes(color = diet)) +
  geom_quantile(data = subset(all_diffs, !(time %in% c("fut_100", "fut_300", "fut_500"))), quantiles = c(.025,.975)) +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(name = "Adjusted Log Rates") +
  theme_classic(base_size = 24) +
  theme(axis.text = element_text(color = "black"), axis.ticks = element_line(color = "black", size = .75),
        panel.border = element_rect(color = "black", fill = NA, size = 1.5), axis.line = element_blank(),
        legend.position = c(.5,.97), legend.direction = "horizontal", legend.background = element_rect(color = NA, fill = NA)) +
  scale_color_manual(name = NULL, values = colors4)
ggsave("../figures/Mammal Diets Rates.pdf", width = 12, height = 12)

#linear regression
fit <- lm(log_corr_rate ~ log_length, data = all_diffs)
all_diffs$residual_mean <- fit$residuals

#quantile regression
library(quantreg)
fit_2.5 <- rq(log_corr_rate ~ log_length, tau = 0.025,
              data = subset(all_diffs, !(time %in% c("fut_100", "fut_300", "fut_500"))))
fit_97.5 <- rq(log_corr_rate ~ log_length, tau = 0.975,
               data = subset(all_diffs, !(time %in% c("fut_100", "fut_300", "fut_500"))))
pred_2.5 <- cbind.data.frame(log_length = unique(all_diffs$log_length),
                  predict(fit_2.5, data.frame(log_length = unique(all_diffs$log_length)), interval = "confidence"))
pred_97.5 <- cbind.data.frame(log_length = unique(all_diffs$log_length),
                   predict(fit_97.5, data.frame(log_length = unique(all_diffs$log_length)), interval = "confidence"))

all_diffs <- all_diffs %>%
  mutate(residual_2.5 = log_corr_rate - pred_2.5$fit[match(log_length, pred_2.5$log_length)],
         residual_97.5 = log_corr_rate - pred_97.5$fit[match(log_length, pred_97.5$log_length)]) %>%
  mutate(corr_resid = case_when(residual_2.5 < 0 ~ residual_2.5, residual_97.5 > 0 ~ residual_97.5, TRUE ~ 0),
         corr_res_disc = case_when(residual_2.5 < 0 ~ "less", residual_97.5 > 0 ~ "greater", TRUE ~ "expected"))

ggplot(all_diffs, aes(x = corr_resid, y = after_stat(density))) +
  geom_histogram() +
  facet_wrap(~diet + time, nrow = 4)

periods_with_future <- rbind(data.frame(name = c("Future"),
                                        max_age = c(0), min_age = c(0.000005),
                                        abbr = NA, color = c("#0079FA")),
                             periods[1:3,])
periods_with_future$max_age <- c(10.5, 7.5, 5.5, 3.5)
periods_with_future$min_age <- c(7.5, 5.5, 3.5, 0.5)
epochs_with_future <- rbind(data.frame(name = c("500 Years", "300 Years", "100 Years"),
                                       max_age = c(0.000003, 0.000001, 0), min_age = c(0.000005, 0.000003, 0.000001),
                                       abbr = NA, color = c("#00E5F8", "#00C2F9", "#009FFA")),
                            epochs[1:7,])
epochs_with_future$max_age <- seq(10.5, 1.5, -1)
epochs_with_future$min_age <- seq(9.5, 0.5, -1)
gg <- ggplot(all_diffs, aes(x = time_num, fill = corr_res_disc)) +
  geom_bar(position = position_fill(), width = .99, color = "black") +
  scale_x_continuous(name = "Time Period", expand = FALSE, breaks = seq(0.5, 10.5, 1)) +
  scale_y_continuous(name = "Percentage of Replicates") +
  scale_fill_manual(NULL, values = c("grey80", "red", "blue"),
                    labels = c("expected change", "change > 95th percentile", "change < 5th percentile")) +
  coord_geo(pos = list("bottom", "bottom"), dat = list(epochs_with_future, periods_with_future),
            xlim = c(0.5, 10.5), abbrv = FALSE, lwd = .75, size = list(5,6), skip = NULL) +
  facet_wrap(~diet, nrow = 4) +
  theme_classic(base_size = 24) +
  theme(axis.text.y = element_text(color = "black"), axis.ticks.y = element_line(color = "black", size = .75),
        axis.ticks.x = element_blank(), axis.text.x = element_blank(),
        panel.border = element_rect(color = "black", fill = NA, size = 1.5),
        legend.position = "top", legend.direction = "horizontal", legend.background = element_rect(color = NA, fill = NA))
ggsave("../figures/Mammal Diets Rates vs Expected.pdf", gg, width = 12, height = 12)

#calculate percentage of points outside of quantiles for each diet/time
#or calculate distance beyond quantiles?

#plot residuals through time
ggplot(all_diffs, aes(x = time_num, y = residual_2.5, fill = diet, group = interaction(time, diet))) +
  annotate("rect", xmin = seq(0.5, 9.5, 1), xmax = seq(1.5, 10.5, 1), ymin = -Inf, ymax = Inf, fill = rep_len(c("grey90", "white"), length.out = 10)) +
  geom_boxplot(position = position_dodge2(preserve = "single", width = .95, padding = .15)) +
  scale_fill_manual(name = NULL, values = colors4) +
  scale_x_continuous(name = "Time Period", limits = c(0.5, 10.5), labels = NULL, breaks = NULL, expand = c(0,0)) +
  theme_classic(base_size = 24) +
  theme(axis.text = element_text(color = "black"), axis.ticks = element_line(color = "black", size = .75),
        panel.border = element_rect(color = "black", fill = NA, size = 1.5), axis.line = element_blank(),
        legend.position = c(.5,.97), legend.direction = "horizontal", legend.background = element_rect(color = NA, fill = NA))

#plot differences through time
(gg <- ggplot(all_diffs_ci, aes(x = time_num, color = diet, group = interaction(time, diet))) +
  annotate("rect", xmin = seq(0.5, 9.5, 1), xmax = seq(1.5, 10.5, 1), ymin = -Inf, ymax = Inf, fill = rep_len(c("grey90", "white"), length.out = 10)) +
  geom_point(aes(y = med), position = position_dodge2(preserve = "single", width = .9, padding = .15)) +
  geom_errorbar(aes(ymin = low, ymax = upp), position = position_dodge2(preserve = "single", width = .95, padding = .15)) +
  scale_x_continuous(name = "Time Period", limits = c(0.5, 10.5), labels = NULL, breaks = NULL, expand = c(0,0)) +
  scale_y_continuous(name = "Median Mass Change (ln g)") +
  #coord_cartesian(ylim = c(-0.0015, 0.0015)) +
  theme_classic(base_size = 24) +
  theme(axis.text = element_text(color = "black"), axis.ticks = element_line(color = "black", size = .75),
        panel.border = element_rect(color = "black", fill = NA, size = 1.5), axis.line = element_blank(),
        legend.position = c(.5,.97), legend.direction = "horizontal", legend.background = element_rect(color = NA, fill = NA)) +
  scale_color_manual(name = NULL, values = colors4)# +
  # annotation_custom(phylopics[[1]], 1.45, 0.55, ymin = 1.5, ymax = 2.5) +
  # annotation_custom(phylopics[[2]], 1.55, 2.45, ymin = 1.5, ymax = 2.5) +
  # annotation_custom(phylopics[[3]], 2.55, 3.45, ymin = 1.5, ymax = 2.5) +
  # annotation_custom(phylopics[[4]], 3.55, 4.45, ymin = 1.5, ymax = 2.5) +
  # annotation_custom(phylopics[[5]], 4.55, 5.45, ymin = 1.5, ymax = 2.5) +
  # annotation_custom(phylopics[[6]], 5.55, 6.45, ymin = 1.5, ymax = 2.5) +
  # annotation_custom(phylopics[[7]], 6.55, 7.45, ymin = 1.5, ymax = 2.5) +
  # annotation_custom(phylopics[[8]], 7.55, 8.45, ymin = 1.5, ymax = 2.5) +
  # annotation_custom(phylopics[[9]], 8.55, 9.45, ymin = 1.5, ymax = 2.5)
  )
periods_with_future <- rbind(data.frame(name = c("Future"),
                                        max_age = c(0), min_age = c(0.000005),
                                        abbr = NA, color = c("#0079FA")),
                             periods[1:3,])
periods_with_future$max_age <- c(3, 5, 7, 10)
periods_with_future$min_age <- c(0, 3, 5, 7)
epochs_with_future <- rbind(data.frame(name = c("500 Years", "300 Years", "100 Years"),
                                       max_age = c(0.000003, 0.000001, 0), min_age = c(0.000005, 0.000003, 0.000001),
                                       abbr = NA, color = c("#00E5F8", "#00C2F9", "#009FFA")),
                            epochs[1:7,])
epochs_with_future$max_age <- 1:10
epochs_with_future$min_age <- 0:9
(geo_plot <- gggeo_scale(gggeo_scale(ggplotGrob(gg), lims = c(10,0), dat = periods_with_future, abbrv = FALSE, size = 6, skip = NULL, lwd = .75),
                         dat = epochs_with_future, abbrv = FALSE, skip = NULL, size = 5, lwd = .75, bord = c("left", "right"), height = unit(2.5, "line")))
ggsave("../figures/Mammal Diets Differences Through Time.pdf", geo_plot, width = 12, height = 12)

#mom stats by continent####
mom_stats_by_cont <- mom_data_per_bin %>%
  group_by(Recoded_Diet, bin, Continent) %>%
  summarise(min_size = min(lnMass_g, na.rm = TRUE), which_min = Genus_species[which.min(lnMass_g)],
            max_size = max(lnMass_g, na.rm = TRUE), which_max = Genus_species[which.max(lnMass_g)],
            med_size = median(lnMass_g, na.rm = TRUE))

diet_stats_by_cont <- mom_stats_by_cont %>%
  filter(Continent != "Insular") %>%
  group_by(Recoded_Diet, Continent) %>%
  summarise(minimum = min(min_size), maximum = max(max_size), avg = (max(max_size) + min(min_size))/2,
            med = max(med_size), extant_med = max(med_size[bin == "Modern"]), future_med = max(med_size[bin == "Future"]),
            extant_max = max(max_size[bin == "Modern"]), future_max = max(max_size[bin == "Future"]), .groups = "drop") %>%
  mutate(diff_pleis = extant_med - med, diff_future = future_med - extant_med,
         Recoded_Diet = factor(Recoded_Diet, levels = c("carnivore", "invertivore", "omnivore", "herbivore")))

diet_diffs_by_cont <- diet_stats_by_cont %>%
  select(Recoded_Diet, Continent, diff_pleis, diff_future) %>%
  pivot_longer(cols = diff_pleis:diff_future, names_to = "time", values_to = "diff", names_prefix = "diff_") %>%
  mutate(time = factor(time, levels = c("pleis", "future")))

#raw differences in medians
ggplot(diet_diffs_by_cont) +
  geom_point(aes(x = Continent, y = diff, color = time), position = position_dodge(width = .5)) +
  scale_x_discrete() +
  scale_y_continuous() +
  scale_color_discrete(name = "time", labels = c("Modern - Pleistocene", "Future - Modern")) +
  facet_wrap(~Recoded_Diet, ncol = 1)

#lengths of extinctions coarsely based on https://www.annualreviews.org/doi/full/10.1146/annurev.ecolsys.34.011802.132415
#which is based on https://science.sciencemag.org/content/306/5693/70.full
#future projections arbitrarily 1000 years away
diet_diffs_by_cont <- diet_diffs_by_cont %>%
  mutate(length = case_when(time == "future" ~ 500,
                            Continent == "AF" ~ 11500,
                            Continent == "AUS" ~ 40000,
                            Continent == "EA" ~ 38500,
                            Continent == "NA" ~ 2000,
                            Continent == "SA" ~ 6500),
         rate = diff / length, corr_rate = rate - min(rate))

c <- trunc(log10(min(diet_diffs_by_cont$corr_rate[diet_diffs_by_cont$corr_rate > 0], na.rm = TRUE)))
diet_diffs_by_cont$log_corr_rate <- log10(diet_diffs_by_cont$corr_rate + 10^c) - c

#log corrected rates
ggplot(diet_diffs_by_cont) +
  geom_point(aes(x = Continent, y = rate, color = time), position = position_dodge(width = .5)) +
  scale_x_discrete() +
  scale_y_continuous() +
  scale_color_discrete(name = "time", labels = c("Modern - Pleistocene", "Future - Modern")) +
  facet_wrap(~Recoded_Diet, ncol = 1)

#plot rates against length
library(ggrepel)
ggplot(diet_diffs_by_cont, aes(x = length, y = rate)) +
  geom_point(aes(color = time), size = 3) +
  geom_text_repel(aes(label = Continent, color = time), size = 10, show.legend = FALSE) +
  geom_quantile(data = subset(diet_diffs_by_cont, time == "pleis"), quantiles = c(.025,.975), color = "black", linetype = "dashed") +
  scale_x_continuous(name = "Length of Extinction Event (yr)", trans = "log10") +
  scale_y_continuous(name = "Rate of Change of Median Size (ln g/yr)") +
  theme_classic(base_size = 24) +
  theme(axis.text = element_text(color = "black"), axis.ticks = element_line(color = "black", size = .75),
        panel.border = element_rect(color = "black", fill = NA, size = 1.5), axis.line = element_blank(),
        legend.position = "top", legend.direction = "horizontal", legend.background = element_rect(color = NA, fill = NA)) +
  scale_color_viridis(name = NULL, begin = .25, end = .75, discrete = TRUE, labels = c("Pleistocene", "Future")) +
  facet_wrap(~Recoded_Diet)
ggsave("../figures/Mammal Diets Pleistocene vs Future Rates.pdf", width = 12, height = 12)
