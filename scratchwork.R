library(dplyr)

# Filling in missing site info

all_sites <- read.csv("Data/all_site_info.csv")

habs <- read.csv("Data/hab_advisory_database.csv")

# Important: coordinates for X sites or HAB responses must be added manually to
# the hab_advisory_database. Other sites with accurate SiteIDs can be automatically
# added from all_site_info

habs <- habs %>% 
  left_join(all_sites, by = join_by(SiteID == MonitoringLocationIdentifier))

habs <- habs %>% 
  mutate(Latitude = coalesce(Latitude, LatitudeMeasure),
         Longitude = coalesce(Longitude, LongitudeMeasure)) %>% 
  select(DatePosted:Longitude, AU)
