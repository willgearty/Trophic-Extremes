# author: Will Gearty (willgearty@gmail.com)
#load and clean data####
library(tidyverse)
library(viridis)
library(deeptime)
library(Hmisc)
library(gtools)
library(rcompanion)
library(shadowtext)

##Data from the PBDB####
pbdb_list <- read.csv("../data/pbdb_mammals.csv", na.strings = c("", "NA"), strip.white = TRUE)

##North American Cenozoic mammals####
# from Smith et al 2018 https://science.sciencemag.org/content/360/6386/310
lyons_diet <- read.csv("../data/Lyons_BS_diet.csv", stringsAsFactors = FALSE)

# Update info based on the PBDB
lyons_diet_merge <- merge(lyons_diet, pbdb_list, by.x = "Genus_species", by.y = "taxon_name", all.x = TRUE) %>%
  mutate(#FAD = ifelse(!is.na(firstapp_max_ma), firstapp_max_ma, FAD),
         #LAD = ifelse(!is.na(lastapp_min_ma), lastapp_min_ma, LAD),
         PBDB.diet = ifelse(!is.na(diet), diet, PBDB.diet),
         PBDB.life.habit = ifelse(!is.na(life_habit), life_habit, PBDB.life.habit),
         Genus_species = ifelse(!is.na(accepted_name), accepted_name, Genus_species),
         Order = ifelse(!is.na(order) & order !="NO_ORDER_SPECIFIED", order, Order),
         Family = ifelse(!is.na(family) & family !="NO_FAMILY_SPECIFIED", family, Family))
lyons_diet_clean <- lyons_diet_merge %>%
  filter(!is.na(PBDB.diet), !is.na(FAD), !is.na(LAD),
         !(PBDB.life.habit %in% c("aquatic")),
         is.na(accepted_rank) | accepted_rank == "species") %>%
  select(Order, Genus_species, FAD, LAD, PBDB.diet, lnMass_g) %>%
  distinct() %>%
  group_by(Order, Genus_species, FAD, LAD, PBDB.diet) %>%
  summarise(lnMass_g = mean(lnMass_g), .groups = "drop") %>%
  mutate(herb = grepl("herb|brows|frug|graz|foliv|gran", PBDB.diet, ignore.case = TRUE),
         carn = grepl("carn|piscivore", PBDB.diet, ignore.case = TRUE),
         insect = grepl("insect", PBDB.diet, ignore.case = TRUE)) %>%
  mutate(omni = grepl("omni", PBDB.diet, ignore.case = TRUE) | rowSums(select(., herb, carn, insect)) > 1,
         Continent = "NA")
lyons_diet_clean$Recoded_Diet <- ifelse(lyons_diet_clean$omni, "omnivore", NA)
lyons_diet_clean$Recoded_Diet[!lyons_diet_clean$omni] <- c("herbivore", "carnivore", "invertivore")[apply(lyons_diet_clean[!lyons_diet_clean$omni, c("herb", "carn", "insect")], 1, which.max)]

lyons_diet_clean <- lyons_diet_clean %>%
  select(Genus_species, Order, FAD, LAD, Recoded_Diet, lnMass_g, Continent)

##MOM (Global Late Quaternary Mammals)####
mom_data <- read.csv("../data/MOM_v10.csv", stringsAsFactors = FALSE, na.strings = "", strip.white = TRUE)
mom_data_clean <- mom_data %>%
  filter(Mass.Status == "valid", LogMass..g. > 0, is.na(TO.DO), !is.na(trophic),
         !grepl("^aquatic$|marine|NA", habitat.mode, ignore.case = TRUE)) %>%
  mutate(Genus_species = paste(Genus, Species),
         herb = grepl("browse|frug|graze", trophic, ignore.case = TRUE),
         carn = grepl("carn|piscivore", trophic, ignore.case = TRUE),
         insect = grepl("insect", trophic, ignore.case = TRUE)) %>%
  mutate(omni = rowSums(select(., herb, carn, insect)) > 1)
mom_data_clean$Recoded_Diet <- ifelse(mom_data_clean$omni, "omnivore", NA)
mom_data_clean$Recoded_Diet[!mom_data_clean$omni] <- c("herbivore", "carnivore", "invertivore")[apply(mom_data_clean[!mom_data_clean$omni, c("herb", "carn", "insect")], 1, which.max)]
#assume all MOM species were around before the late Pleistocene extinction
mom_data_clean$FAD <- 2.58
mom_data_clean$LAD <- ifelse(mom_data_clean$Status %in% c("extant", "historical"), 0, as.numeric(mom_data_clean$Last.Occurance..kybp.)/1000)
mom_data_clean$lnMass_g <- log(as.numeric(mom_data_clean$Combined.Mass..g.))

#save this for later
mom_data_clean_extinction <- mom_data_clean %>%
  select(Genus_species, Order, FAD, LAD, Recoded_Diet, lnMass_g, Continent, Status,
         Time_of_extinction = Time.of.extinction..LP.125.75..EP.30.50..TP.15.10..Holocene.10.1..historical..1.EW..future...CR.EN.VU..future2...that.plus.NT.)

mom_data_clean <- mom_data_clean %>%
  select(Genus_species, Order, FAD, LAD, Recoded_Diet, lnMass_g, Continent)

##Supplementary mammal data collected for this study####
supp_data <- read.csv("../data/supp_mammals.csv", stringsAsFactors = FALSE, na.strings = "", strip.white = TRUE)
supp_data_clean <- supp_data %>%
  filter((!is.na(ln_mass_g) & mass_source != "Smith et al 2018") |
           !is.na(ln_mass_g_estimate)) %>%
  mutate(lnMass_g = ifelse(!is.na(ln_mass_g) & mass_source != "Smith et al 2018",
                           ln_mass_g, ln_mass_g_estimate)) %>%
  mutate(Genus_species = taxon_name,
         herb = grepl("browse|frug|graze", diet, ignore.case = TRUE),
         carn = grepl("carn|piscivore", diet, ignore.case = TRUE),
         insect = grepl("insect", diet, ignore.case = TRUE)) %>%
  mutate(omni = grepl("omni", diet, ignore.case = TRUE) | rowSums(select(., herb, carn, insect)) > 1,
         Continent = "NA") # probably want to get actual continent data later
supp_data_clean$Recoded_Diet <- ifelse(supp_data_clean$omni, "omnivore", NA)
supp_data_clean$Recoded_Diet[!supp_data_clean$omni] <- c("herbivore", "carnivore", "invertivore")[apply(supp_data_clean[!supp_data_clean$omni, c("herb", "carn", "insect")], 1, which.max)]

supp_data_clean <- supp_data_clean %>%
  select(Genus_species, Order, FAD = firstapp_max_ma, LAD = lastapp_min_ma, Recoded_Diet, lnMass_g, Continent)

##Combine three datasets and clean####
mamm_diet <- rbind(cbind(lyons_diet_clean, source = "lyons"),
                   cbind(mom_data_clean, source = "mom"),
                   cbind(supp_data_clean, source = "supp")) %>%
  #Rename out-of-date orders
  mutate(Order = recode(Order, Soricomorpha = "Eulipotyphla", Lipotyphla = "Eulipotyphla",
                        Hicanodonta = "Cingulata", Xenarthra = "Pilosa")) %>%
  #Summarise duplicates
  group_by(Genus_species, Continent) %>%
  summarise(Order = paste(na.omit(unique(Order)), collapse = ", "), FAD = max(FAD), LAD = min(LAD), lnMass_g = mean(lnMass_g),
            Recoded_Diet = ifelse("mom" %in% source, Recoded_Diet[source == "mom"],
                                  ifelse("lyons" %in% source, Recoded_Diet[source == "lyons"], Recoded_Diet[source == "supp"])),
            sources = paste(source, collapse = ", "), .groups = "drop") %>%
  #order recoded diet
  mutate(Recoded_Diet = factor(Recoded_Diet, levels = c("herbivore", "omnivore", "invertivore", "carnivore")))

#Get relevant subset of time scale
time_scale <- subset(epochs, max_age < 150)
time_scale$name <- factor(time_scale$name, levels = rev(time_scale$name))

#Clean up data for saving
mamm_diet_clean <- mamm_diet %>%
  mutate(FAD_interval = cut(FAD, c(time_scale$max_age, 0), labels = time_scale$name),
         LAD_interval = cut(LAD, c(time_scale$min_age, 145), right = FALSE, labels = time_scale$name)) %>%
  select(Order, Genus_species, lnMass_g, Recoded_Diet, FAD, LAD, FAD_interval, LAD_interval, sources) %>%
  filter(!is.na(FAD_interval), !is.na(LAD_interval)) %>%
  arrange(Order, Genus_species)

# uncomment this to write the supplemental file
# write.csv(mamm_diet_clean, "../data/mammal_fossils_compiled.csv", row.names = FALSE)

#plots through time####

colors4 <- setNames(c("#359B73", "#7d22b2", "#FFAC3B", "#ad0025"), c("herbivore", "omnivore", "invertivore", "carnivore"))

#phylopics
uuids <- c("Early Cretaceous" = "b8eda501-71d6-4d26-8c9e-731baedd27b2", #Sinodelphys szalayi
           "Late Cretaceous" = "a6cb4e81-e204-42b4-b9e7-ad2ecd377305", #Adalatherium hui
           "Paleocene" = "05a96450-6166-4dee-9805-9d460c599837", #Phenacodus
           "Eocene" = "5fe1c8ef-f7c4-47ff-9cc0-b528107fde98", #Uintatherium
           "Oligocene" = "20521c21-a8c9-4d2b-bcbd-86f63c683fe8", #Hoplophoneus
           "Miocene" = "5d4a8a6f-7564-45b0-ac31-ce2eab6f5b24", #Gomphotherium
           "Pliocene" = "eba11ec7-30b6-4cda-bc1f-1dcc58d2c228", #Equus simplicidens
           "Pleistocene" = "cc04733f-befe-4fbc-948c-9cfd9180c90f", #Smilodon
           "Holocene" = "62398ac0-f0c3-48f8-8455-53512a05fbc4" #Loxodonta
           )

library(grImport)
#devtools::install_github("richfitz/vectoR")
library(vectoR)
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
phylopics[[2]] <- flip(phylopics[[2]], horizontal = TRUE)
phylopics[[4]] <- flip(phylopics[[4]], horizontal = TRUE)
phylopics[[5]] <- flip(phylopics[[5]], horizontal = TRUE)
phylopics[[6]] <- flip(phylopics[[6]], horizontal = TRUE)
phylopics[[7]] <- flip(phylopics[[7]], horizontal = TRUE)
phylopics[[8]] <- flip(phylopics[[8]], horizontal = TRUE)
phylopics[[9]] <- flip(phylopics[[9]], horizontal = TRUE)

phylopics <- lapply(phylopics, pictureGrob)

#set up data by time interval
n_bins <- nrow(time_scale)

mamm_per_bin <- as.data.frame(matrix(NA, nrow = 0, ncol = ncol(mamm_diet) + 2, dimnames = list(c(),c(colnames(mamm_diet), "bin", "bin_mid"))))
for(i in 1:n_bins){
  dat <- subset(mamm_diet, LAD < time_scale$max_age[i] & FAD > time_scale$min_age[i])
  if(nrow(dat) > 0) {
    dat$bin <- time_scale$name[i]
    dat$bin_mid <- time_scale$age_mid[i]
    mamm_per_bin <- rbind(mamm_per_bin, dat)
  }
}
mamm_per_bin$bin_num <- as.numeric(mamm_per_bin$bin)

##Figure 4####
sample_size <- mamm_per_bin %>% group_by(bin, bin_num, Recoded_Diet) %>% filter(Recoded_Diet %in% c("herbivore", "omnivore", "carnivore", "invertivore")) %>%
  summarise(num = n())

#mann-whitney tests for medians
mamm_p <- with(mamm_per_bin, pairwise.wilcox.test(lnMass_g, interaction(Recoded_Diet, bin), p.adjust.method = "none"))
# adjust the p values here since we don't need most of them
mamm_stars <- stars.pval(p.adjust(diag(mamm_p$p.value)[sort(c(seq(1, 33, 4), seq(2, 34, 4), seq(3, 35, 4)))]))
#remove dots
mamm_stars[mamm_stars == "."] <- " "

#permutation tests for 90th quantiles
set.seed(1234)
pt90 <- do.call(rbind, lapply(1:n_bins, function(x) {
  df <- pairwisePercentileTest(lnMass_g ~ Recoded_Diet, data = subset(mamm_per_bin, bin_num == x),
                               test = "percentile", tau = 0.90, r = 5000, digits = 7)[c(1,4,6), 1:2]
  df$bin <- levels(time_scale$name)[x]
  df
}))
# adjust p-values based on number of comparisons
pt90$p.adjust <- p.adjust(pt90$p.value)
pt90$stars <- stars.pval(pt90$p.adjust)

#plot
gg <- ggplot(mamm_per_bin %>% group_by(bin, Recoded_Diet) %>% filter(Recoded_Diet %in% c("herbivore", "omnivore", "carnivore", "invertivore")), aes(x = bin_num, y = lnMass_g, fill = Recoded_Diet, group = interaction(bin, Recoded_Diet))) +
  annotate("rect", xmin = seq(0.5, 8.5, 1), xmax = seq(1.5, 9.5, 1), ymin = -Inf, ymax = Inf, fill = rep_len(c("grey90", "white"), length.out = 9)) +
  geom_boxplot(position = position_dodge2(preserve = "single", padding = .15), width = .85) +
  geom_text(data = sample_size, aes(label = num, y = c(.25,-.3,-.9,.25)[as.numeric(Recoded_Diet)], color = Recoded_Diet),
            position = position_dodge2(preserve = "single", width = .85, padding = .15), size = 6, show.legend = FALSE) +
  annotate("text", label = mamm_stars, x = sort(c(seq(1, 9) - .25, seq(1, 9), seq(1, 9) + .25)), y = 0.75, size = 7.5, colour = "black") +
  geom_shadowtext(data = pt90, aes(label = stars), x = sort(c(seq(1, 9) - .25, seq(1, 9), seq(1, 9) + .25)), y = 15.5, size = 7.5, colour = "white", inherit.aes = FALSE) +
  #annotate("text", label = pt75$stars, x = sort(c(seq(1, 9) - .25, seq(1, 9), seq(1, 9) + .25)), y = 15.5, size = 7.5, colour = "black") +
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
  annotation_custom(phylopics[[1]], 1.45, 0.55, ymin = -2.65, ymax = -2.15) +
  annotation_custom(phylopics[[2]], 1.55, 2.45, ymin = -2.65, ymax = -1.85) +
  annotation_custom(phylopics[[3]], 2.55, 3.45, ymin = -2.80, ymax = -1.50) +
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

##Figure 4 w/o invertivores####
sample_size <- mamm_per_bin %>% group_by(bin, bin_num, Recoded_Diet) %>% filter(Recoded_Diet %in% c("herbivore", "omnivore", "carnivore")) %>%
  summarise(num = n())

mamm_p <- with(subset(mamm_per_bin, Recoded_Diet != "invertivore"),
               pairwise.wilcox.test(lnMass_g, interaction(Recoded_Diet, bin), p.adjust.method = "none"))
# adjust the p values here since we don't need most of them
mamm_stars <- stars.pval(p.adjust(diag(mamm_p$p.value)[sort(c(seq(1, 25, 3), seq(2, 26, 3)))]))

gg <- ggplot(mamm_per_bin %>% group_by(bin, Recoded_Diet) %>% filter(Recoded_Diet %in% c("herbivore", "omnivore", "carnivore")), aes(x = bin_num, y = lnMass_g, fill = Recoded_Diet, group = interaction(bin, Recoded_Diet))) +
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

#Sensitivity Tests####
##subsampling separate diets####
n_subsets <- 100 #number of replicates for each subset size
subset_sizes <- seq(5,100,5)

set.seed(1234)
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

###Figure S5####
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

#Means and standard deviations
ggplot(data = subset(diet_subsample_means, Recoded_Diet != "insectivore"), aes(x = bin, y = raw_mean, color = Recoded_Diet, group = sample)) +
  geom_point(position = position_dodge(width = .9)) +
  geom_errorbar(aes(ymin = raw_mean - 1.96*raw_sd, ymax = raw_mean + 1.96*raw_sd), position = position_dodge(width = .9)) +
  scale_x_discrete(name = "Time (Ma)", labels = gsub(" ","\n", rev(time_scale$name))) +
  scale_y_continuous(name = "ln Mass (g)", lim = c(0,15)) +
  theme_classic(base_size = 16) +
  theme(axis.text = element_text(color = "black"), axis.text.x = element_text(color = "black", angle = 90, vjust = .5), axis.ticks = element_line(color = "black")) +
  scale_color_manual(values = colors4, name = "Diet")
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

##bootstrap separate diets####
diets <- levels(mamm_diet$Recoded_Diet)
n_diets <- length(diets)
n_reps <- 1000

set.seed(1234)
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

###Figure S3####
#plot boxplots of means
diet_bootstrap$bin_num <- as.numeric(diet_bootstrap$bin)
(gg <- ggplot(data = subset(diet_bootstrap, species >= 5 & Recoded_Diet != "insectivore"), aes(x = bin_num, y = avg, fill = Recoded_Diet, group = interaction(bin, Recoded_Diet))) +
    annotate("rect", xmin = seq(0.5, 8.5, 1), xmax = seq(1.5, 9.5, 1), ymin = 0, ymax = 15, fill = rep_len(c("grey90", "white"), length.out = 9)) +
    geom_boxplot(position = position_dodge2(preserve = "single", width = .95, padding = .15), color = "black") +
    scale_x_continuous(name = "Time (Ma)", limits = c(0.5, 9.5), labels = rev(c(0, epochs$max_age[1:9])), breaks = seq(0.5, 9.5, 1), expand = c(0,0)) +
    scale_y_continuous(name = "ln Mass (g)", breaks = seq(2, 10, 2)) +
    coord_cartesian(ylim = c(1.5,12)) +
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

###Figure S4####
#plot weighted means and weighted standard deviations
(gg <- ggplot(data = subset(diet_bootstrap_means, Recoded_Diet != "insectivore"), aes(x = bin_num, y = wtd_mean, color = Recoded_Diet, group = interaction(bin, Recoded_Diet))) +
    annotate("rect", xmin = seq(0.5, 8.5, 1), xmax = seq(1.5, 9.5, 1), ymin = 0, ymax = 15, fill = rep_len(c("grey90", "white"), length.out = 9)) +
    geom_point(position = position_dodge2(preserve = "single", width = .9, padding = .15), size = 3) +
    geom_errorbar(aes(ymin = wtd_mean - 1.96 * sqrt(wtd_var), ymax = wtd_mean + 1.96 * sqrt(wtd_var)), position = position_dodge2(preserve = "single", width = .95, padding = .15), size = 1.75) +
    geom_text(aes(label = species, y = wtd_mean + 1.96 * sqrt(wtd_var) + .2), position = position_dodge2(preserve = "single", width = .9, padding = .15), size = 5, show.legend = FALSE) +
    scale_x_continuous(name = "Time (Ma)", limits = c(0.5, 9.5), labels = rev(c(0, epochs$max_age[1:9])), breaks = seq(0.5, 9.5, 1), expand = c(0,0)) +
    scale_y_continuous(name = "ln Mass (g)", breaks = seq(3, 11, 2)) +
    coord_cartesian(ylim = c(1.5,12)) +
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

#Limits and concepts figure####
#use MOM data from above
mom_data_per_bin <- as.data.frame(matrix(NA, nrow = 0, ncol = ncol(mom_data_clean) + 1, dimnames = list(c(),c(colnames(mom_data_clean), "bin"))))
#pleistocene and holocene
for(i in 1:2){
  fossil_dat <- subset(mom_data_clean, LAD <= time_scale$max_age[i] & FAD >= time_scale$min_age[i])
  if(nrow(fossil_dat) > 0) {
    fossil_dat$bin <- time_scale$name[i]
    mom_data_per_bin <- rbind(mom_data_per_bin, fossil_dat)
  }
}
#modern
modern_dat <- subset(mom_data_clean_extinction, Status == "extant") %>%
  mutate(bin = "Modern")
#future
future_dat <- subset(modern_dat, !(Time_of_extinction %in% c("Future", "Future2"))) %>%
  mutate(bin = "Future")
mom_data_per_bin <- mom_data_per_bin %>%
  bind_rows(modern_dat, future_dat) %>%
  mutate(bin = factor(bin, levels = c("Pleistocene", "Holocene", "Modern", "Future")))

mom_stats <- mom_data_per_bin %>%
  group_by(Recoded_Diet, bin) %>%
  summarise(min_size = min(lnMass_g, na.rm = TRUE), which_min = Genus_species[which.min(lnMass_g)],
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
            "Largest Invertivore" = "e1c1157a-6d13-49d8-9932-e4794e4fb001", #Holmesina (representing Pampatherium)
            "Smallest Omnivore" = "81930c02-5f26-43f7-9c19-e9831e780e53", #Sigmodontinae
            "Largest Omnivore" = "0cd82109-bb1c-4e08-ab11-c845d8a82eba", #Ursus arctos
            "Smallest Herbivore" = "92989e35-4e68-4a2d-b3a2-191ba9da671a", #Mus
            "Largest Herbivore" = "3866663c-e4ac-426f-b1cc-0f73955d0ed6" #Mammuthus columbi
)

phylopics2 <- lapply(uuids2, getPhyloPic)

#make them all face to the right
phylopics2[[2]] <- flip(phylopics2[[2]], horizontal = TRUE)
phylopics2[[3]] <- flip(phylopics2[[3]], horizontal = TRUE)
phylopics2[[4]] <- flip(phylopics2[[4]], horizontal = TRUE)
phylopics2[[5]] <- flip(phylopics2[[5]], horizontal = TRUE)
phylopics2[[6]] <- flip(phylopics2[[6]], horizontal = TRUE)
#make the mouse black
phylopics2[[7]]@paths <- lapply(phylopics2[[7]]@paths,
                                function(x) {
                                  x@rgb <- "#000000"
                                  return(x)
                                })

phylopics2 <- lapply(phylopics2, pictureGrob)

##Figure 1####
ggplot(mom_stats) +
  geom_segment(data = subset(diet_stats, Recoded_Diet != "omnivore"), show.legend = FALSE,
               aes(x = minimum, xend = minimum, y = 0, yend = 4.7, color = Recoded_Diet), size = 2.5, linetype = "11") +
  geom_segment(data = subset(diet_stats, Recoded_Diet != "omnivore"), show.legend = FALSE,
               aes(x = maximum, xend = maximum, y = 0, yend = 4.7, color = Recoded_Diet), size = 2.5, linetype = "11") +
  geom_rect(aes(xmin = min_size, xmax = max_size, ymin = diet_num - .4, ymax = diet_num + .4,
                fill = interaction(Recoded_Diet, bin)), show.legend = FALSE, color = "black", size = 1.25) +
  #geom_segment(aes(x = med_size, xend = med_size, y = diet_num - .4, yend = diet_num + .4,
  #              linetype = bin), show.legend = FALSE, color = "black", size = 1.25) +
  geom_text(data = diet_stats, aes(x = avg, y = diet_num, label = paste0(Recoded_Diet, "s")),
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
  annotation_custom(phylopics2[[4]], 12.15, 13.2, ymin = 2.6, ymax = 3.4) + #12.2
  annotation_custom(phylopics2[[5]], .7, 1.4, ymin = 1.8, ymax = 2.2) + #1.55
  annotation_custom(phylopics2[[6]], 12.05, 13.55, ymin = 1.7, ymax = 2.3) + #12.3
  annotation_custom(phylopics2[[7]], 0.62, 1.22, ymin = 0.8, ymax = 1.2) + #1.37
  annotation_custom(phylopics2[[8]], 15.95, 17.45, ymin = 0.6, ymax = 1.4)   #16.2
ggsave("../figures/Mammal Diets Mechanisms.pdf", width = 10, height = 13)

#MOM stats by continent####
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

##Figure S6####
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
