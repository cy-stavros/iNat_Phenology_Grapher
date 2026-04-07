# This script will help you grab iNat observations from a region and plot them!
# by Gigi Lowe, Cy Stavros, and Amelia Weeden

# to install and call in packages: 
install.packages("pacman")
pacman::p_load(rinat, ggplot2, dplyr)

##### Getting iNat observations #####

# Phenology Annotations:
# termID for flowers and fruits = 12
# Flowers and Fruits: 13=Flowers, 14=Fruits or Seeds, 15=Flower Buds, 21=No Flowers or Fruits
# termID for leaves:36
# Leaves: 37=Breaking Leaf Buds, 38=Green Leaves, 39=Colored Leaves, 40=No Live Leaves

# this function is for fruits and flowers, but you could easily modify for leaves 
# (or any other phenology annotation, see 
# https://forum.inaturalist.org/t/how-to-use-inaturalists-search-urls-wiki-part-2-of-2/18792)
# by modifying the annotation ID numbers

pheno_grabber <- function(sp, placeID) {

    flowers0 <- get_inat_obs(
    taxon_name = sp,
    place_id = placeID,
    quality = "research", #leaving NULL will include casual
    annotation = c("12","13"),
    maxresults = 1000, #should not be greater than 10,000
    meta = FALSE
  )
  flowers <- flowers0 %>% mutate(pheno = "Flowering")

  fruits0 <- get_inat_obs(
    taxon_name = sp,
    place_id = placeID,
    quality = "research", 
    annotation = c("12","14"),
    maxresults = 1000,
    meta = FALSE
  )
  fruits <- fruits0 %>% mutate(pheno = "Fruits or Seeds")
  
  buds0 <- get_inat_obs(
    taxon_name = sp,
    place_id = placeID,
    quality = "research",
    annotation = c("12","15"),
    maxresults = 1000, 
    meta = FALSE
  )
  buds <- buds0 %>% mutate(pheno = "Flower Buds")
  
  none0 <- get_inat_obs(
    taxon_name = sp,
    place_id = placeID,
    quality = "research", 
    annotation = c("12","21"),
    maxresults = 1000,
    meta = FALSE
  )
  none <- none0 %>% mutate(pheno = "No Fruits or Flowers")
  
  combined <- rbind(flowers, fruits, buds, none)
  
  return(combined)
}


# Implement your function here. Replace your "Diapensia lapponica" with your sp of choice,
# and "64492" with your placeID of choice. 64492 is "Northeastern US" (from PA and NJ north)
# Find other placeIDs by using the iNat explore page and checking URL
#note you need the numerical place ID, sometimes iNat gives you one that is non-numeric

obs <- pheno_grabber("Diapensia lapponica", "64492")


##### Phenology Grapher #####

# This creates a new, filtered and updated dataframe to remove the "No Fruits or Flowers" annotation to clean up the data.
# This also adds two new variables: "month", and a day of year ("doy") which essentially allows us to treat dates as a continuous variable. 
# Feel free to clean up or edit your data as needed (based on your sample size or what annotations you want to use)!

obs_2 <- obs %>%
  filter(pheno %in% c("Flowering", "Fruits or Seeds", "Flower Buds")) %>%
  mutate(
    date  = as.Date(observed_on),
    month = factor(format(date, "%b"), levels = month.abb),
    doy  = as.numeric(format(date, "%j")))

########################
##### density plot #####
########################

# This creates a density plot plotted by the "doy" variable we made and then bins it by month to make it easier to read.
# It shows the relative density of each pheno phase. I like this one because it clearly shows where the observations start and end.

start_month <- 5   # Change these numbers to change your start and end view on the graph!
end_month   <- 10

month_breaks <- as.numeric(format(
  as.Date(paste0("2001-", start_month:end_month, "-15")), "%j"))

month_limits <- range(month_breaks) + c(-15, 15)

pretty_density <- ggplot(obs_2, aes(doy, fill = pheno, group = pheno)) +
  geom_density(alpha = .4, adjust = 1.5, # If you want to change the smoothing, change the "adjust" values. 1.5 is similar to inat!
               from = month_limits[1], to = month_limits[2]) +
  scale_x_continuous(
    limits = month_limits,
    breaks = month_breaks,
    labels = month.abb[start_month:end_month]) +
  theme_minimal() +
  labs(
    x = "Month",
    y = "Relative observation density",
    fill = "Phenophase",
    title = "Seasonal Phenology Timing")

print(pretty_density)
