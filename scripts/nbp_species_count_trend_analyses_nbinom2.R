################################################################################
### Estimate species count trends from the neighborhood bird project dataset ###
########################### May 2025 ###########################################

# clear environment
rm(list = ls())

# load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,
               readxl,
               glmmTMB,
               DHARMa,
               performance)

source("functions/theme_bcs.R") # custom functions for bcs brand / style


# Neighborhood Bird Project Data
nbp <- read_xlsx("data/nbp_tidy_jan_24.xlsx")  # main dataset
covs <- read.csv("data/circ_no_overlap_covariates.csv")  # table with focal survey stations and a few covariates (covs not used in this analysis)


# prep data
d <- nbp %>%
  # create some new fields to help with data preparation / analysis
  mutate(pls = paste(park, loop, station, sep = "-"),
         dpl = paste(survey_date, park, loop, sep = "-"),
         day = day(survey_date)) %>%
  # subset data to include only non-overlapping count stations
  filter(station.code %in% covs$station.code,
  # filter out spuh records       
         !str_detect(species, pattern = "sp\\."),
  # filter out duplicated mag park survey
         dpl != "2021-09-13-Magnuson Park-Waterfront Loop",
  # filter out years with limited / spotty data collection
         year %in% c(2005:2019, 2022, 2023)) %>%
  group_by(survey_date, pls, bird.code) %>% 
  summarise(nobs = sum(observed), .groups = "drop") %>%
  pivot_wider(names_from = bird.code, values_from = nobs, values_fill = 0) %>%
  pivot_longer(-c(1:2), names_to = "bird.code", values_to = "nobs") %>%
  mutate(year = year(survey_date),
         syear = as.numeric(scale(year)),
         month = as.factor(month(survey_date)),
         day = day(survey_date),
         y_day = as.factor(yday(survey_date)))


## test model on a few species
focal.species <- "ANHU"  ## Anna's Hummingbird

focal.d <- d %>% filter(bird.code == focal.species)

mod <- glmmTMB(nobs ~ syear + month + (1 | pls), data = focal.d, family = "nbinom2")  ## pls = park-loop-station, i.e., site

sum(residuals(mod, type = "pearson")^2) / df.residual(mod)  # Pearson dispersion statistic
summary(mod)

testZeroInflation(mod)


## RESULT: significant positive trend. Dispersion stat looks good. ZI not an issue. 


focal.species <- "AMCR"  # American crow

focal.d <- d %>% filter(bird.code == focal.species)

mod <- glmmTMB(nobs ~ syear + month + (1 | pls), data = focal.d, family = "nbinom2")

sum(residuals(mod, type = "pearson")^2) / df.residual(mod)

testZeroInflation(mod)

summary(mod)

## RESULT:Overdispersed, but not wildly so. ZI not an issue. AMCR scaled year term is positive and significant.


## Analysis objective: Estimate trend for all species with detections in at least 10 years

# Pull codes for species with detections in at least 10 years 
spp <- d %>% 
  group_by(year, bird.code) %>% 
  summarise(dets = sum(nobs > 0), .groups = "drop") %>%
  group_by(bird.code) %>% 
  summarise(years_w_dets = sum(dets > 0), .groups = "drop") %>% 
  filter(years_w_dets >= 10) %>% 
  arrange(bird.code) %>% 
  pull(bird.code)

# Objects to store model coefficients and diagnostic info
r2 <- numeric(length(spp))
disp <- numeric(length(spp))
beta_yr <- numeric(length(spp))
se_yr <- numeric(length(spp))
p_yr <- numeric(length(spp))
species <- character(length(spp))


sd_year <- sd(d$year)  # standard deviation for year for later coefficient adjustment


# loop for modeling observations for each species

for(i in 1:length(spp)) {
  
  mod.dat <- d %>% filter(bird.code == spp[i])
  mod <- glmmTMB(nobs ~ syear + month + (1 | pls), data = mod.dat, family = nbinom2(link = "log"))
    
  # Compute R2
   r2_vals <- r2_nakagawa(mod)
   if (is.list(r2_vals)) {
     r2[i] <- r2_vals$R2_marginal
   } else {
     r2[i] <- NA  # Fallback if the function fails
   }
    
  # Dispersion calculation
   disp[i] <- sum(residuals(mod, type = "pearson")^2) / df.residual(mod)
    
   # Extract model coefficients
   beta_yr[i] <- summary(mod)$coefficients$cond["syear", "Estimate"]
   se_yr[i] <- summary(mod)$coefficients$cond["syear", "Std. Error"]
   p_yr[i] <- summary(mod)$coefficients$cond["syear", "Pr(>|z|)"]
    
  # Store identifiers
  species[i] <- spp[i]
    
  print(paste(spp[i], "complete"))
}
  
res <- data.frame(species, beta_yr, se_yr, p_yr, r2, disp)  ## combine into single dataframe

# create qualitative trend and confidence descriptors
res_status <- res %>%
  mutate(bkt_beta_yr = exp(beta_yr / sd_year),
         trend = factor(case_when(
           bkt_beta_yr >= 1.01018 ~ "Increasing",
           bkt_beta_yr <= 0.9876797 ~ "Decreasing",
           TRUE ~ "Stable"  # catch-all
         ), levels = c("Stable", "Decreasing", "Increasing")),
         confidence = factor(case_when(
           p_yr <= 0.01 & r2 >= 0.3 & disp >= 0.7 & disp <= 1.5 ~ "high confidence",
           p_yr <= 0.1 & r2 >= 0.1 & disp <= 2    ~ "moderate confidence",
           TRUE ~ "low confidence"),  # catch-all
           levels = c("low confidence", "moderate confidence", "high confidence")),
         trend_confidence = paste0(trend, " (", confidence, ")"),
         sht_confidence = ifelse(confidence == "high confidence" | confidence == "moderate confidence", "confident", "not confident"),
         sht_trend_confidence = factor(paste0(trend, " (", sht_confidence, ")"), levels = c("Increasing (confident)", "Increasing (not confident)",
                                                                                            "Stable (confident)", "Stable (not confident)", 
                                                                                            "Decreasing (confident)", "Decreasing (not confident)")))
                                                                                        

    
  
view(res_status)

res_status %>% group_by(sht_trend_confidence) %>% summarise(n = n(), .group = "drop") %>% ggplot() + geom_col(aes(x = "", y = n, fill = sht_trend_confidence)) 

#write.csv(res_status, "results/tables/aggregtaed_results.csv")

res <- read_xlsx("results/tables/aggregated_results.xlsx") %>% select(species:disp)
# visualize results

p.dat <- res_status %>% mutate(
  pspecies = fct_reorder(species, bkt_beta_yr),
  label = ifelse(sht_confidence == "Moderate to high confidence", "*", NA))

p.dat$hjust_star <-  ifelse(p.dat$beta_yr >= 0, 1.3, -0.3)

#png(filename = paste0("results/figures/",parks[i],"_species_trends.png"), width = 4, height = 5, units = "in", res = 300)
print(ggplot(p.dat, aes(x = pspecies, y = (bkt_beta_yr-1), fill = (bkt_beta_yr-1))) +
        geom_col() +
        geom_text(aes(label = label), 
                  vjust = 0.7, hjust = p.dat$hjust_star, size = 10, na.rm = TRUE) +
        coord_flip() +
        scale_fill_gradient2(
          low = "#D73027",      # Red (decreasing)
          mid = "#F5F5F5",       # Neutral (stable)
          high = "#1A9850",     # Green (increasing)
          midpoint = 0,
          name = "Trend") +
        labs(title = "Larger-scale trends", y = "Estimated annual % change", x = "Species code") +
        theme_bcs())
#dev.off()

####### Now lets do park-specific trends

d <- nbp %>%
  # create some new fields to help with data preparation / analysis
  mutate(pls = paste(park, loop, station, sep = "-"),
         dpl = paste(survey_date, park, loop, sep = "-"),
         day = day(survey_date)) %>%
  # subset data to include only non-overlapping count stations
  filter(station.code %in% covs$station.code,
         # filter out spuh records       
         !str_detect(species, pattern = "sp\\."),
         # filter out duplicated mag park survey
         dpl != "2021-09-13-Magnuson Park-Waterfront Loop",
         year %in% c(2005:2019, 2022, 2023)) %>%
  group_by(survey_date, pls, park, bird.code) %>% 
  summarise(nobs = sum(observed), .groups = "drop") %>%
  pivot_wider(names_from = bird.code, values_from = nobs, values_fill = 0) %>%
  pivot_longer(-c(1:3), names_to = "bird.code", values_to = "nobs") %>%
  mutate(year = year(survey_date),
         syear = as.numeric(scale(year)),
         month = as.factor(month(survey_date)),
         day = day(survey_date),
         y_day = as.factor(yday(survey_date)))


# loop for modeling observations for each species at each park
parks <- sort(unique(d$park))

res_list <- list()

for(i in 1:length(parks)) {
  
  park.dat <- d[d$park == parks[i], ]
  
  spp <- park.dat %>% group_by(year, bird.code) %>% 
    summarise(dets = sum(nobs > 0), .groups = "drop") %>%
    group_by(bird.code) %>% 
    summarise(years_w_dets = sum(dets > 0), .groups = "drop") %>% 
    filter(years_w_dets >= 10) %>% 
    arrange(bird.code) %>% 
    pull(bird.code)
  
  print(paste("Assessing trends for", length(spp), "species at", parks[i]))
  
  if (length(spp) == 0) {
    message(parks[i], ": no species with ≥10 years of detections")
    res_list[[parks[i]]] <- data.frame()  #
    next  # skip to next park
  }
  
  r2 <- numeric(length(spp))
  disp <- numeric(length(spp))
  beta_yr <- numeric(length(spp))
  se_yr <- numeric(length(spp))
  p_yr <- numeric(length(spp))
  park <- character(length(spp))
  species <- character(length(spp))
  
  
  for(j in 1:length(spp)){
    
    mod.dat <- park.dat[park.dat$bird.code == spp[j],] 
    mod <- glmmTMB(nobs ~ syear + month + (1 | pls), data = mod.dat, family = nbinom2(link = "log"))
    
    r2_vals <- r2_nakagawa(mod)
    if (is.list(r2_vals)) {
      r2[j] <- r2_vals$R2_marginal
    } else {
      r2[j] <- NA  # Fallback if the function fails
    }
    disp[j] <- sum(residuals(mod, type = "pearson")^2) / df.residual(mod)
    
    beta_yr[j] <- summary(mod)$coefficients$cond["syear", 1]
    se_yr[j] <- summary(mod)$coefficients$cond["syear", 2]
    p_yr[j] <- summary(mod)$coefficients$cond["syear", 4]
    park[j] <- parks[i]
    species[j] <- spp[j]
    print(paste(parks[i], spp[j], "complete"))
    
  }
  
  res_list[[parks[i]]] <- data.frame(park, species, beta_yr, se_yr, p_yr, r2, disp)
  print(paste(parks[i], "complete"))
  
}


count_trends <- bind_rows(res_list)


sd_year <- sd(d$year)


trends_by_park <- count_trends %>%
  mutate(bkt_beta_yr = exp(beta_yr / sd_year),
         trend = factor(case_when(
           bkt_beta_yr >= 1.05 ~ "Increasing",
           bkt_beta_yr <= 0.95 ~ "Decreasing",
           TRUE ~ "Stable"  # catch-all for 0.95–1.05
         ), levels = c("Stable", "Decreasing", "Increasing")),
         confidence = factor(case_when(
           p_yr <= 0.01 & r2 >= 0.3 & disp <= 1.5 ~ "high confidence",
           p_yr <= 0.1 & r2 >= 0.1 & disp <= 2    ~ "moderate confidence",
           TRUE ~ "low confidence"),  # catch-all
           levels = c("low confidence", "moderate confidence", "high confidence")),
         trend_confidence = paste0(trend, " (", confidence, ")"),
         sht_confidence = ifelse(confidence == "high confidence" | confidence == "moderate confidence", "Moderate to high confidence", "Low confidence"))

write.csv(trends_by_park, "results/tables/count_trends_by_species_by_park_v2.csv")

#trends_by_park <- read.csv("results/tables/count_trends_by_species_by_park_v2.csv")


## Visualize species trends for each park
for(i in 1:length(parks)) {
  p.dat <- trends_by_park[trends_by_park$park == parks[i], ] %>% mutate(
    pspecies = fct_reorder(species, bkt_beta_yr),
    label = ifelse(sht_confidence == "Moderate to high confidence", "*", NA))
  
  p.dat$hjust_star <-  ifelse(p.dat$beta_yr >= 0, 1.3, -0.3)
  
 # png(filename = paste0("results/figures/",parks[i],"_species_trends.png"), width = 4, height = 5, units = "in", res = 300)
  print(ggplot(p.dat, aes(x = pspecies, y = (bkt_beta_yr-1), fill = (bkt_beta_yr-1))) +
          geom_col() +
          geom_text(aes(label = label), 
                    vjust = 0.7, hjust = p.dat$hjust_star, size = 10, na.rm = TRUE) +
          coord_flip() +
          scale_fill_gradient2(
            low = "#D73027",      # Red (decreasing)
            mid = "#F5F5F5",       # Neutral (stable)
            high = "#1A9850",     # Green (increasing)
            midpoint = 0,
            name = "Trend") +
          labs(title = parks[i], y = "Estimated annual % change", x = "Species code") +
          theme_bcs())
  #dev.off()
}


##### Try with observation-level random effect. Note that this helped with 
##### dispersion issues; poisson distribution appears suitable with this model

d <- nbp %>%
  # create some new fields to help with data preparation / analysis
  mutate(pls = paste(park, loop, station, sep = "-"),
         dpl = paste(survey_date, park, loop, sep = "-"),
         day = day(survey_date)) %>%
  # subset data to include only non-overlapping count stations
  filter(station.code %in% covs$station.code,
         # filter out spuh records       
         !str_detect(species, pattern = "sp\\."),
         # filter out duplicated mag park survey
         dpl != "2021-09-13-Magnuson Park-Waterfront Loop",
         # filter out years with limited / spotty data collection
         year %in% c(2005:2019, 2022, 2023)) %>%
  group_by(survey_date, park, pls, survey_id, bird.code) %>% 
  summarise(nobs = sum(observed), .groups = "drop") %>%
  pivot_wider(names_from = bird.code, values_from = nobs, values_fill = 0) %>%
  pivot_longer(-c(1:4), names_to = "bird.code", values_to = "nobs") %>%
  mutate(year = year(survey_date),
         syear = as.numeric(scale(year)),
         month = as.factor(month(survey_date)),
         day = day(survey_date),
         y_day = as.factor(yday(survey_date)), 
         obs_id = as.factor(row_number()))

# Pull codes for species with detections in at least 10 years 
spp <- d %>% 
  group_by(year, bird.code) %>% 
  summarise(dets = sum(nobs > 0), .groups = "drop") %>%
  group_by(bird.code) %>% 
  summarise(years_w_dets = sum(dets > 0), .groups = "drop") %>% 
  filter(years_w_dets >= 10) %>% 
  arrange(bird.code) %>% 
  pull(bird.code)
dir.create("results/figures")

# Objects to store model coefficients and diagnostic info
r2 <- numeric(length(spp))
disp <- numeric(length(spp))
beta_yr <- numeric(length(spp))
se_yr <- numeric(length(spp))
p_yr <- numeric(length(spp))
species <- character(length(spp))


sd_year <- sd(d$year)  # standard deviation for year for later coefficient adjustment


# loop for modeling observations for each species

for(i in 1:length(spp)) {
  
  mod.dat <- d %>% filter(bird.code == spp[i])
  mod <- glmmTMB(nobs ~ syear + month + (1 | obs_id) + (1 | pls), data = mod.dat, family = poisson(link = "log"))
  
  # Extract model coefficients
  beta_yr[i] <- summary(mod)$coefficients$cond["syear", "Estimate"]
  se_yr[i] <- summary(mod)$coefficients$cond["syear", "Std. Error"]
  p_yr[i] <- summary(mod)$coefficients$cond["syear", "Pr(>|z|)"]
  
  # Store identifiers
  species[i] <- spp[i]
  
  print(paste(spp[i], "complete"))
}

res <- data.frame(species, beta_yr, se_yr, p_yr)  ## combine into single dataframe
write.csv(res, "poisson_aggregated_results.csv")
view(res)

res$bkt_beta_yr <- exp(res$beta_yr / sd_year)
res$bkt_upper <- exp((res$beta_yr + res$se_yr) / sd_year)
res$bkt_lower <- exp((res$beta_yr - res$se_yr) / sd_year)

p.dat <- res[complete.cases(res), ] %>%
  mutate(pspecies = fct_reorder(species, bkt_beta_yr))

chart <- ggplot(p.dat[p.dat$p_yr < 1, ], aes(y = pspecies, x = (bkt_beta_yr - 1))) +
  geom_vline(xintercept = 0, color = bcs_colors["dark green"], linetype = "dashed") +
  geom_point(size = 2, color = bcs_colors["dark green"]) +  # Trend point estimates
  geom_errorbarh(aes(xmin = (bkt_lower - 1),
                     xmax = (bkt_upper - 1)),
                 height = 0.3, color = bcs_colors["bright green"]) +  # Horizontal error bars
  #geom_text(aes(label = label), 
            #vjust = 0.7, hjust = p.dat$hjust_star, size = 5, na.rm = TRUE) +  # Optional annotations
  #coord_flip() +
  labs(y = "",
       x = "Estimated annual % change") +
  theme_bcs() +
  theme(axis.text = element_text(size = 8),
        axis.title = element_text(size = 12))


png(filename = "all_results_poisson.png", height = 11, width = 5, units = "in", res = 300)
chart
dev.off()

sum(p.dat$bkt_beta_yr < 0.9876797) & p.dat$p_yr < 0.1)
sum(p.dat$bkt_beta_yr >= 0.9876797) & p.dat$p_yr < 0.1)
76/42
72/(72+40)
32/16
32/(16+32)

61/(61+51)

dim(p.dat)
view(p.dat)

focal <- c("CORA", 'PUFI', 'DEJU', 'EUST', 'HOSP', "BARS")
plot <- ggplot(p.dat %>% filter(species %in% focal), aes(x = pspecies, y = ((bkt_beta_yr - 1) * 100))) +
  geom_hline(yintercept = 0, linetype = "dashed", color = bcs_colors["dark green"]) +
  geom_errorbar(aes(ymin = ((bkt_lower - 1) * 100),
                     ymax = ((bkt_upper - 1) * 100)),
                 width = 0.2, color = bcs_colors["bright green"]) +  # Horizontal error bars
  geom_point(size = 2, color = bcs_colors["dark green"]) +  # Trend point estimates
  #geom_text(aes(label = label), 
  #vjust = 0.7, hjust = p.dat$hjust_star, size = 5, na.rm = TRUE) +  # Optional annotations
  #coord_flip() +
  labs(y = "Annual % change", x = "") +
  #annotate("text", y = 0, x = 0.5, label = "no change", hjust = 0, vjust = -0.2, color = bcs_colors["dark green"]) +
  theme_bcs() +
  theme(panel.grid.major.x = element_blank(), 
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12))


png(filename = 'ecnw_graphic.png', width = 5, height = 2, units = 'in', res = 300)
plot
dev.off()
?png
