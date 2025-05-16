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
           bkt_beta_yr >= 1.05 ~ "Increasing",
           bkt_beta_yr <= 0.95 ~ "Decreasing",
           TRUE ~ "Stable"  # catch-all for 0.95–1.05
         ), levels = c("Stable", "Decreasing", "Increasing")),
         confidence = factor(case_when(
           p_yr <= 0.01 & r2 >= 0.3 & disp >= 0.7 & disp <= 1.5 ~ "high confidence",
           p_yr <= 0.1 & r2 >= 0.1 & disp <= 2    ~ "moderate confidence",
           TRUE ~ "low confidence"),  # catch-all
           levels = c("low confidence", "moderate confidence", "high confidence")),
         trend_confidence = paste0(trend, " (", confidence, ")"),
         sht_confidence = ifelse(confidence == "high confidence" | confidence == "moderate confidence", "Moderate to high confidence", "Low confidence"))

    
  
#view(res_status)

#write.csv(res_status, "results/tables/aggregtaed_results.csv")

#res_status <- read_xlsx("results/tables/aggregtaed_results.xlsx") %>% mutate(
#  sht_confidence = ifelse(confidence == "high confidence" | confidence == "moderate confidence", "Moderate to high confidence", "Low confidence"))

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





