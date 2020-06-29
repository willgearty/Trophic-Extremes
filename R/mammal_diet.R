#load and clean data####
library(plyr)
library(dplyr)
library(ggplot2)
library(viridis)
library(deeptime)
library(Hmisc)
library(magrittr)

foss_mamm_diet <- read.csv("../data/Lyons_BS_diet.csv", stringsAsFactors = FALSE)
archaic_orders <- read.csv("../data/ArchaicOrders.csv", stringsAsFactors = FALSE)[["Order"]]
foss_mamm_diet$archaic <- factor(foss_mamm_diet$Order %in% archaic_orders)
time_scale <- subset(epochs, max_age < 150)
time_scale$name <- factor(time_scale$name, levels = rev(time_scale$name))

#remove rows without diet or range data
foss_mamm_diet_clean <- subset(foss_mamm_diet, !is.na(Recoded_Diet) & !is.na(FAD) & !is.na(LAD))

#remove aquatic and amphibious species
foss_mamm_diet_clean <- subset(foss_mamm_diet_clean, !(PBDB.life.habit %in% c("aquatic", "amphibious")))

#order
foss_mamm_diet_clean$Recoded_Diet <- factor(foss_mamm_diet_clean$Recoded_Diet, levels = c("herbivore", "omnivore", "insectivore", "carnivore"))

#settings
n_subsets <- 100 #number of replicates for bootstrap analyses
subset_sizes <- seq(5,100,5) #sample sizes to use for bootstrap analyses

#Black, Orange, Sky Blue, Bluish green, Yellow, Blue, Vermilion, Reddish purple
colors8a <- c(rgb(0,0,0), rgb(230/255, 159/255, 0/255), rgb(86/255, 180/255, 233/255), rgb(0/255, 158/255, 115/255), rgb(240/255,228/255,66/255), rgb(0/255, 114/255, 178/255), rgb(213/255, 94/255, 0/255), rgb(204/255, 121/255, 167/255))
#Black, Honolulu Blue, Summer Sky, Barbie Pink, Ocean Green, Bamboo, Gamboge, Paris Daisy
colors8b <- c(rgb(0,0,0), rgb(34/255, 113/255, 178/255), rgb(61/255, 183/255, 233/255), rgb(247/255, 72/255, 165/255), rgb(53/255, 155/255, 115/255), rgb(213/255, 94/255, 0/255), rgb(230/255, 159/255, 0/255), rgb(240/255, 230/255, 66/255))
#Bluish green, Orange, Reddish purple
colors3 <- setNames(c(rgb(0/255, 158/255, 115/255), rgb(230/255, 159/255, 0/255), rgb(204/255,121/255,167/255)), c("herbivore","omnivore","carnivore"))
colors4 <- setNames(colors8b[c(5, 2, 8, 6)], c("herbivore", "omnivore", "insectivore", "carnivore"))

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

foss_mamm_per_bin <- as.data.frame(matrix(NA, nrow = 0, ncol = ncol(foss_mamm_diet_clean) + 2, dimnames = list(c(),c(colnames(foss_mamm_diet_clean), "bin", "bin_mid"))))
for(i in 1:n_bins){
  dat <- subset(foss_mamm_diet_clean, LAD <= time_scale$max_age[i] & FAD >= time_scale$min_age[i])
  if(nrow(dat) > 0) {
    dat$bin <- time_scale$name[i]
    dat$bin_mid <- time_scale$age_mid[i]
    foss_mamm_per_bin <- rbind(foss_mamm_per_bin, dat)
  }
}

#all diets together
ggplot(foss_mamm_per_bin, aes(x = bin, y = lnMass_g)) +
  geom_violin(draw_quantiles = c(.25,.5,.75)) +
  scale_x_discrete(name = "Time (Ma)") +
  scale_y_continuous(name = "ln Mass (g)", lim = c(0,15)) +
  theme_bw()
ggsave("../figures/Mammal Violins.pdf", device = "pdf", width = 10, height = 10)

ggplot(foss_mamm_per_bin, aes(x = bin, y = lnMass_g)) +
  geom_boxplot(position = position_dodge2(preserve = "single")) +
  scale_x_discrete(name = "Time (Ma)") +
  scale_y_continuous(name = "ln Mass (g)", lim = c(0,15)) +
  theme_bw()
ggsave("../figures/Mammal Boxplots.pdf", device = "pdf", width = 10, height = 10)

#all diets together with archaic split out
ggplot(foss_mamm_per_bin, aes(x = bin, y = lnMass_g, fill = archaic)) +
  geom_violin(draw_quantiles = c(.25,.5,.75)) +
  scale_x_discrete(name = "Time (Ma)") +
  scale_y_continuous(name = "ln Mass (g)", lim = c(0,15)) +
  theme_bw()
ggsave("../figures/Mammal Violins Archaic.pdf", device = "pdf", width = 10, height = 10)

ggplot(foss_mamm_per_bin, aes(x = bin, y = lnMass_g, fill = archaic)) +
  geom_boxplot(position = position_dodge2(preserve = "single")) +
  scale_x_discrete(name = "Time (Ma)") +
  scale_y_continuous(name = "ln Mass (g)", lim = c(0,15)) +
  theme_bw()
ggsave("../figures/Mammal Boxplots Archaic.pdf", device = "pdf", width = 10, height = 10)

#diets separate
ggplot(foss_mamm_per_bin, aes(x = bin, y = lnMass_g, fill = Recoded_Diet)) +
  geom_violin(draw_quantiles = c(.25,.5,.75), position = position_dodge(), scale = "width") +
  scale_x_discrete(name = "Time (Ma)") +
  scale_y_continuous(name = "ln Mass (g)", lim = c(0,15)) +
  scale_fill_manual(values = colors4, name = "Diet") +
  theme_bw()
ggsave("../figures/Mammal Diets Violins.pdf", device = "pdf", width = 10, height = 10)

foss_mamm_per_bin %>% group_by(bin, Recoded_Diet) %>% filter(n() >= 5) %>%
ggplot(aes(x = bin, y = lnMass_g, fill = Recoded_Diet)) +
  geom_boxplot(position = position_dodge(preserve = "single")) +
  scale_x_discrete(name = "Time (Ma)") +
  scale_y_continuous(name = "ln Mass (g)", lim = c(0,15)) +
  scale_fill_manual(values = colors4, name = "Diet") +
  theme_bw()
ggsave("../figures/Mammal Diets Boxplots.pdf", device = "pdf", width = 10, height = 10)

#diets and archaic separate
foss_mamm_per_bin %>% group_by(archaic, bin, Recoded_Diet) %>% filter(n() >= 5) %>%
ggplot(aes(x = bin, y = lnMass_g, fill = archaic, color = Recoded_Diet)) +
  geom_boxplot(position = position_dodge(preserve = "single"), size = 1) +
  scale_x_discrete(name = "Time (Ma)") +
  scale_y_continuous(name = "ln Mass (g)", lim = c(0,15)) +
  scale_color_manual(values = colors4, name = "Diet") +
  scale_fill_manual(values = c("grey80", "grey20")) +
  theme_bw()
ggsave("../figures/Mammal Diets Boxplots Archaic.pdf", device = "pdf", width = 10, height = 10)

#basic bootstrap with increasing sample size####
subset_means <- matrix(NA, n_subsets, n_bins)
colnames(subset_means) <- time_scale$name

basic_bootstrap <- data.frame(age_bin = rep(time_scale$name, length(subset_sizes)), sample = rep(subset_sizes, each = n_bins), mean = NA, std_dev = NA)

for(k in 1:length(subset_sizes)){
  subset_size <- subset_sizes[k]
  subset_means[,] <- NA
  for(j in 1:ncol(subset_means)){
    dat <- subset(foss_mamm_diet_clean, LAD <= time_scale$max_age[j] & FAD >= time_scale$min_age[j])
    for(i in 1:n_subsets){
      subset_means[i,j] <- mean(sample(dat$lnMass_g, subset_size, replace = TRUE))
    }
  }
  basic_bootstrap$mean[((k-1)*n_bins + 1):(k*n_bins)] <- colMeans(subset_means)
  basic_bootstrap$std_dev[((k-1)*n_bins + 1):(k*n_bins)] <- unlist(colwise(sd)(as.data.frame(subset_means)))
}

ggplot(data = basic_bootstrap, aes(x = age_bin, y = mean, color = sample, group = sample)) +
  geom_point(position = position_dodge(width = .9)) +
  geom_linerange(aes(ymin = mean - 1.96*std_dev, ymax = mean + 1.96*std_dev), position = position_dodge(width = .9)) +
  scale_x_discrete(name = "Time (Ma)", labels = gsub(" ","\n", rev(time_scale$name))) +
  scale_y_continuous(name = "ln Mass (g)", lim = c(0,15)) +
  scale_color_viridis() +
  theme_classic(base_size = 16) +
  theme(axis.text = element_text(color = "black"), axis.text.x = element_text(color = "black", angle = 90, vjust = .5), axis.ticks = element_line(color = "black"))
ggsave("../figures/Mammal Bootstrap.pdf", device = "pdf", width = 10, height = 10)

#boostrap separate diets and perform pairwise t-tests####
diets <- levels(foss_mamm_diet_clean$Recoded_Diet)
n_diets <- length(diets)

diet_bootstrap <- data.frame(age_bin = rep(time_scale$name, each = n_subsets * n_diets, times = length(subset_sizes)),
                             diet = rep(diets, times = n_subsets * n_bins * length(subset_sizes)), sample = rep(subset_sizes, each = n_bins * n_diets * n_subsets),
                             mean = NA, sd = NA, n = NA)

diet_differences <- data.frame(age_bin = rep(time_scale$name, each = n_subsets * n_diets, times = length(subset_sizes)),
                                 sample = rep(subset_sizes, each = n_bins * n_diets * n_subsets), diet1 = rep(diets[c(1,1,2)], times = n_subsets * n_diets * length(subset_sizes)), 
                                 diet2 = rep(diets[c(2,3,3)], times = n_subsets * n_diets * length(subset_sizes)), difference = NA, p = NA)
diet_differences$diets <- paste0(diet_differences$diet1, "-", diet_differences$diet2)

idx <- 1
pb <- txtProgressBar(min = 1, max = length(subset_sizes) * n_bins * n_subsets * n_diets, style = 3)
for(k in 1:length(subset_sizes)){
  subset_size <- subset_sizes[k]
  for(j in 1:n_bins){
    dat <- subset(foss_mamm_diet_clean, LAD <= time_scale$max_age[j] & FAD >= time_scale$min_age[j])
    for(i in 1:n_subsets){
      setTxtProgressBar(pb, idx)
      samp <- dat %>% group_by(Recoded_Diet) %>% sample_n(subset_size, replace = TRUE)
      means <- aggregate(. ~ Recoded_Diet, samp[,c("lnMass_g", "Recoded_Diet")], mean)
      sds <- aggregate(. ~ Recoded_Diet, samp[,c("lnMass_g", "Recoded_Diet")], sd)
      ns <- dat %>% group_by(Recoded_Diet) %>% summarise(n = length(Recoded_Diet), .groups = 'drop')
      diet_bootstrap$mean[idx:(idx + n_diets - 1)] <- means$lnMass_g[match(diets, means$Recoded_Diet)]
      diet_bootstrap$sd[idx:(idx + n_diets - 1)] <- sds$lnMass_g[match(diets, sds$Recoded_Diet)]
      diet_bootstrap$n[idx:(idx + n_diets - 1)] <- ns$n[match(diets, sds$Recoded_Diet)]

      diffs <- outer(setNames(means$lnMass_g, means$Recoded_Diet),setNames(means$lnMass_g, means$Recoded_Diet),'-')
      diet_differences$difference[idx:(idx + n_diets - 1)] <- sapply(1:n_diets, function(x) diffs[match(as.character(diet_differences$diet2[x]), rownames(diffs)),match(as.character(diet_differences$diet1[x]), colnames(diffs))])
      pw <- pairwise.wilcox.test(samp$lnMass_g, samp$Recoded_Diet)$p.value
      diet_differences$p[idx:(idx + n_diets - 1)] <- sapply(1:n_diets, function(x) pw[match(as.character(diet_differences$diet2[x]), rownames(pw)),match(as.character(diet_differences$diet1[x]), colnames(pw))])

      idx <- idx + n_diets
    }
  }
}

diet_bootstrap$diet <- factor(diet_bootstrap$diet, levels = c("herbivore", "omnivore", "insectivore", "carnivore"))

ggplot(data = diet_bootstrap, aes(x = age_bin, y = mean, color = diet)) +
  geom_boxplot(position = position_dodge(preserve = "single")) +
  scale_x_discrete(name = "Time (Ma)", labels = gsub(" ","\n", rev(time_scale$name))) +
  scale_y_continuous(name = "ln Mass (g)", lim = c(0,15)) +
  theme_classic(base_size = 16) +
  theme(axis.text = element_text(color = "black"), axis.text.x = element_text(color = "black", angle = 90, vjust = .5), axis.ticks = element_line(color = "black")) +
  scale_color_manual(values = colors4, name = "Diet") +
  facet_wrap(~sample)
ggsave("../figures/Mammal Diets Bootstrap Boxplots.pdf", device = "pdf", width = 20, height = 20)

#pull out sample size=20
diet_bootstrap$age_bin_num <- as.numeric(diet_bootstrap$age_bin)
(gg <- ggplot(data = subset(diet_bootstrap, sample==20 & n >= 5), aes(x = age_bin_num, y = mean, fill = diet, group = interaction(age_bin, diet))) +
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
discrete_epochs$name[8:9] <- c("Early\nCretaceous", "Early\nCretaceous")
(geo_plot <- gggeo_scale(gggeo_scale(ggplotGrob(gg), lims = c(9, 0), dat = discrete_periods, abbrv = FALSE, size = 6, skip = NULL, lwd = .75),
                         dat = discrete_epochs, abbrv = FALSE, skip = NULL, size = 5, lwd = .75, bord = c("left", "right"), height = unit(2.5, "line")))
ggsave("../figures/Mammal Diets Bootstrap Boxplots-20 Sample.pdf", geo_plot, device = "pdf", width = 12, height = 12)

#calculate means, sds, weighted means, and weighted variances for each age bin*diet combination
bootstrap_means <- diet_bootstrap %>%
  group_by(age_bin, diet, sample) %>% filter(n >= 5) %>%
  summarise(stddev = sd(mean, na.rm = TRUE), avg = mean(mean), wtd_mean = wtd.mean(mean, 1/sd^2), wtd_var = wtd.var(mean, 1/sd^2))

#Means and standard deviations
ggplot(data = bootstrap_means, aes(x = age_bin, y = avg, color = diet, group = sample)) +
  geom_point(position = position_dodge(width = .9)) +
  geom_errorbar(aes(ymin = avg - 1.96*stddev, ymax = avg + 1.96*stddev), position = position_dodge(width = .9)) +
  scale_x_discrete(name = "Time (Ma)", labels = gsub(" ","\n", rev(time_scale$name))) +
  scale_y_continuous(name = "ln Mass (g)", lim = c(0,15)) +
  theme_classic(base_size = 16) +
  theme(axis.text = element_text(color = "black"), axis.text.x = element_text(color = "black", angle = 90, vjust = .5), axis.ticks = element_line(color = "black")) +
  scale_color_manual(values = colors4, name = "Diet")
ggsave("../figures/Mammal Diets Bootstrap Means.pdf", device = "pdf", width = 10, height = 10)

#Pull out sample=20
(gg <- ggplot(data = subset(bootstrap_means, sample==20), aes(x = age_bin_num, y = avg, color = diet, group = interaction(age_bin, diet))) +
    annotate("rect", xmin = seq(0.5, 8.5, 1), xmax = seq(1.5, 9.5, 1), ymin = 0, ymax = 15, fill = rep_len(c("grey90", "white"), length.out = 9)) +
    geom_point(position = position_dodge2(preserve = "single", width = .9, padding = .15), size = 3) +
    geom_errorbar(aes(ymin = avg - 1.96*stddev, ymax = avg + 1.96*stddev), position = position_dodge2(preserve = "single", width = .95, padding = .15), size = 1.75) +
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
discrete_epochs$name[8:9] <- c("Early\nCretaceous", "Early\nCretaceous")
(geo_plot <- gggeo_scale(gggeo_scale(ggplotGrob(gg), lims = c(9, 0), dat = discrete_periods, abbrv = FALSE, size = 6, skip = NULL, lwd = .75),
                         dat = discrete_epochs, abbrv = FALSE, skip = NULL, size = 5, lwd = .75, bord = c("left", "right"), height = unit(2.5, "line")))
ggsave("../figures/Mammal Diets Bootstrap Means-20 Sample.pdf", geo_plot, device = "pdf", width = 12, height = 12)

#Meta-means and weighted standard deviations
ggplot(data = bootstrap_means, aes(x = age_bin, y = wtd_mean, color = diet, group = sample)) +
  geom_point(position = position_dodge(width = .9)) +
  geom_errorbar(aes(ymin = wtd_mean - 1.96 * sqrt(wtd_var), ymax = wtd_mean + 1.96 * sqrt(wtd_var)), position = position_dodge(width = .9)) +
  scale_x_discrete(name = "Time (Ma)", labels = gsub(" ","\n", rev(time_scale$name))) +
  scale_y_continuous(name = "ln Mass (g)", lim = c(0,15)) +
  theme_classic(base_size = 16) +
  theme(axis.text = element_text(color = "black"), axis.text.x = element_text(color = "black", angle = 90, vjust = .5), axis.ticks = element_line(color = "black")) +
  scale_color_manual(values = colors4, name = "Diet")
ggsave("../figures/Mammal Diets Bootstrap Meta Means.pdf", device = "pdf", width = 10, height = 10)

#pull out sample size=20
bootstrap_means$age_bin_num <- as.numeric(bootstrap_means$age_bin)
(gg <- ggplot(data = subset(bootstrap_means, sample==20), aes(x = age_bin_num, y = wtd_mean, color = diet, group = interaction(age_bin, diet))) +
    annotate("rect", xmin = seq(0.5, 8.5, 1), xmax = seq(1.5, 9.5, 1), ymin = 0, ymax = 15, fill = rep_len(c("grey90", "white"), length.out = 9)) +
    geom_point(position = position_dodge2(preserve = "single", width = .9, padding = .15), size = 3) +
    geom_errorbar(aes(ymin = wtd_mean - 1.96 * sqrt(wtd_var), ymax = wtd_mean + 1.96 * sqrt(wtd_var)), position = position_dodge2(preserve = "single", width = .95, padding = .15), size = 1.75) +
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
discrete_epochs$name[8:9] <- c("Early\nCretaceous", "Early\nCretaceous")
(geo_plot <- gggeo_scale(gggeo_scale(ggplotGrob(gg), lims = c(9, 0), dat = discrete_periods, abbrv = FALSE, size = 6, skip = NULL, lwd = .75),
                         dat = discrete_epochs, abbrv = FALSE, skip = NULL, size = 5, lwd = .75, bord = c("left", "right"), height = unit(2.5, "line")))
ggsave("../figures/Mammal Diets Bootstrap Meta Means-20 Sample.pdf", geo_plot, device = "pdf", width = 12, height = 12)

ggplot(data = diet_differences, aes(x = age_bin, y = difference, color = diets)) +
  geom_hline(yintercept = 0, color = "grey60") +
  geom_boxplot(position = position_dodge(preserve = "single")) +
  scale_x_discrete(name = "Time (Ma)", labels = gsub(" ","\n", rev(time_scale$name))) +
  scale_y_continuous(name = "Difference in Average Mass (ln g)") +
  theme_classic(base_size = 16) +
  theme(axis.text = element_text(color = "black"), axis.text.x = element_text(color = "black", angle = 90, vjust = .5), axis.ticks = element_line(color = "black")) +
  scale_color_manual(values = unname(colors3)) +
  facet_wrap(~sample)
ggsave("../figures/Mammal Diets Bootstrap Differences.pdf", device = "pdf", width = 20, height = 20)

ggplot(data = diet_differences, aes(x = age_bin, y = p, color = diets)) +
  geom_hline(yintercept = 0.05, color = "grey60") +
  geom_boxplot(position = position_dodge(preserve = "single")) +
  scale_x_discrete(name = "Time (Ma)", labels = gsub(" ","\n", rev(time_scale$name))) +
  scale_y_continuous(name = "Difference in Average Mass (ln g)", lim = c(0,1)) +
  theme_classic(base_size = 16) +
  theme(axis.text = element_text(color = "black"), axis.text.x = element_text(color = "black", angle = 90, vjust = .5), axis.ticks = element_line(color = "black")) +
  scale_color_manual(values = unname(colors3)) +
  facet_wrap(~sample)
ggsave("../figures/Mammal Diets Bootstrap P-values.pdf", device = "pdf", width = 20, height = 20)

#boostrap separate diets/archaic and perform pairwise t-tests####
diets <- levels(foss_mamm_diet_clean$Recoded_Diet)
n_diets <- length(diets)

diet_bootstrap_archaic <- data.frame(age_bin = rep(time_scale$name, each = n_subsets * n_diets * 2, times = length(subset_sizes)),
                             diet = rep(diets, times = 2 * n_subsets * n_diets * length(subset_sizes)),
                             sample = rep(subset_sizes, each = 2 * n_bins * n_diets * n_subsets),
                             archaic = rep(levels(foss_mamm_diet_clean$archaic), each = n_diets, times = n_subsets * n_bins * length(subset_sizes)),
                             mean = NA, sd = NA, n = NA)

#subset_differences <- data.frame(age_bin = rep(time_scale$name, each = n_subsets * n_diets, times = length(subset_sizes)),
#                                 sample = rep(subset_sizes, each = n_bins * n_diets * n_subsets), diet1 = rep(diets[c(1,1,2)], times = n_subsets * n_diets * length(subset_sizes)), 
#                                 diet2 = rep(diets[c(2,3,3)], times = n_subsets * n_diets * length(subset_sizes)), difference = NA, p = NA)
#subset_differences$diets <- paste0(subset_differences$diet1, "-", subset_differences$diet2)

idx <- 1
for(k in 1:length(subset_sizes)){
  subset_size <- subset_sizes[k]
  for(j in 1:n_bins){
    dat <- subset(foss_mamm_diet_clean, LAD <= time_scale$max_age[j] & FAD >= time_scale$min_age[j])
    for(i in 1:n_subsets){
      samp <- dat %>% group_by(Recoded_Diet, archaic) %>% sample_n(subset_size, replace = TRUE)
      means <- aggregate(. ~ Recoded_Diet * archaic, samp[,c("lnMass_g", "Recoded_Diet", "archaic")], mean, drop = FALSE)
      sds <- aggregate(. ~ Recoded_Diet * archaic, samp[,c("lnMass_g", "Recoded_Diet", "archaic")], sd, drop = FALSE)
      ns <- aggregate(. ~ Recoded_Diet * archaic, samp[,c("lnMass_g", "Recoded_Diet", "archaic")], nrow, drop = FALSE)
      diet_bootstrap_archaic$mean[idx:(idx + n_diets * 2 - 1)] <- means$lnMass_g
      diet_bootstrap_archaic$sd[idx:(idx + n_diets * 2 - 1)] <- sds$lnMass_g
      diet_bootstrap_archaic$n[idx:(idx + n_diets * 2 - 1)] <- ns$lnMass_g
      
      #diffs <- outer(setNames(means$lnMass_g, means$Recoded_Diet),setNames(means$lnMass_g, means$Recoded_Diet),'-')
      #subset_differences$difference[idx:(idx + n_diets * 2 - 1)] <- sapply(1:n_diets, function(x) diffs[match(as.character(subset_differences$diet2[x]), rownames(diffs)),match(as.character(subset_differences$diet1[x]), colnames(diffs))])
      #pw <- pairwise.wilcox.test(samp$lnMass_g, samp$Recoded_Diet)$p.value
      #subset_differences$p[idx:(idx + n_diets * 2 - 1)] <- sapply(1:n_diets, function(x) pw[match(as.character(subset_differences$diet2[x]), rownames(pw)),match(as.character(subset_differences$diet1[x]), colnames(pw))])
      
      idx <- idx + n_diets * 2
    }
  }
}

bootstrap_means <- diet_bootstrap_archaic %>%
  group_by(age_bin, diet, sample, archaic) %>%
  summarise(stddev = sd(`mean`, na.rm = TRUE), means = mean(mean, na.rm = TRUE), wtd_mean = wtd.mean(mean, 1/sd^2), wtd_var = wtd.var(mean, 1/sd^2))

ggplot(data = bootstrap_means, aes(x = age_bin, y = means, color = diet, group = sample)) +
  geom_point(position = position_dodge(width = .9)) +
  geom_errorbar(aes(ymin = means - 1.96*stddev, ymax = means + 1.96*stddev), position = position_dodge(width = .9)) +
  scale_x_discrete(name = "Time (Ma)", labels = gsub(" ","\n", rev(time_scale$name))) +
  scale_y_continuous(name = "ln Mass (g)", lim = c(0,15)) +
  theme_classic(base_size = 16) +
  theme(axis.text = element_text(color = "black"), axis.text.x = element_text(color = "black", angle = 90, vjust = .5), axis.ticks = element_line(color = "black")) +
  scale_color_manual(limits = c("herbivore", "omnivore", "carnivore"), values = colors3, name = "Diet") +
  facet_wrap(~archaic, ncol = 1, labeller = as_labeller(c("TRUE" = "Archaic Orders", "FALSE" = "Modern Orders")))
ggsave("../figures/Mammal Diets Bootstrap Means - Archaic.pdf", device = "pdf", width = 10, height = 12)

ggplot(data = bootstrap_means, aes(x = age_bin, y = means, color = diet, shape = archaic)) +
  geom_point(position = position_dodge(width = .9)) +
  geom_errorbar(aes(ymin = means - 1.96*stddev, ymax = means + 1.96*stddev), position = position_dodge(width = .9)) +
  scale_x_discrete(name = "Time (Ma)", labels = gsub(" ","\n", rev(time_scale$name))) +
  scale_y_continuous(name = "ln Mass (g)", lim = c(0,15)) +
  theme_classic(base_size = 16) +
  theme(axis.text = element_text(color = "black"), axis.text.x = element_text(color = "black", angle = 90, vjust = .5), axis.ticks = element_line(color = "black")) +
  scale_color_manual(limits = c("herbivore", "omnivore", "carnivore"), values = colors3, name = "Diet") +
  facet_wrap(~sample)
