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

# This creates a new, filtered and updated dataframe to remove "No Fruits or Flowers"
# to clean up the data (low sample size), and adds a new variable of only months 
# to plot the data. Feel free to clean up or edit your data as needed! 
# (e.g. you may want to add No Fruits or Flowers back in or remove others phases based on sample size)
obs_month <- obs %>%
  filter(pheno %in% c("Flowering", "Fruits or Seeds", "Flower Buds")) %>%
  mutate(
    date  = as.Date(observed_on),
    month = factor(format(date, "%b"), levels = month.abb))

# This creates a bar graph that shows the frequency (count) of each phenology stage by month, 
# using the variable we created above.
janky <- ggplot(obs_month, aes(x = month, fill = pheno)) +
  geom_bar(position = "stack", alpha = .9) +
  theme_minimal() +
  labs(y = "Number of observations")

print(janky)

#Once again, this is creating a new, filtered and updated dataframe that removes 
# "No Fruits or Flowers" to clean up the data, and adds a new variable of day of year (doy) 
# which essentially allows us to use date as a continuous variable. 
obs_doy <- obs %>%
  filter(pheno %in% c("Flowering", "Fruits or Seeds", "Flower Buds")) %>%
  mutate(
    date = as.Date(observed_on),
    doy  = as.numeric(format(date, "%j")))

#This determines what day of year corresponds to what month to center the label with the middle of the month.
month_breaks <- c(15,46,74,105,135,166,196,227,258,288,319,349)

# This creates a density plot plotted by day of year (doy) and then smoothed. 
# It shows the relative density of each phenology stage.
pretty_density <- ggplot(obs_doy, aes(x = doy, fill = pheno, group = pheno)) +
  geom_density(alpha = .4, adjust = .5) + # "adjust" here controls the amount of smoothing!
  scale_x_continuous(
    limits = c(121, 305), #this displays April to October, but you can change this 
    breaks = month_breaks,
    labels = month.abb) +
  theme_minimal() +
  labs(
    x = "Month",
    y = "Relative observation density",
    fill = "Phenology stage",
    title = "Seasonal Phenology")

# A note on smoothing: under-smoothed data looks noisy and won't convey overall trends,
# while over-smoothed data erases valuable detail. We recommend starting low (adjust = .5)
# and increasing from there. The amount of smoothing you can get away with will
# also be determined by your sample size for each pheno phase.

print(pretty_density)

