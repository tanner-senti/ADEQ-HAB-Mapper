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


### Getting the HAB data from the webpage ####
# Install required packages if not already installed
if (!requireNamespace("rvest", quietly = TRUE)) install.packages("rvest")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")

# Load libraries
library(rvest)
library(dplyr)

# URL of the webpage
url <- "https://www.adeq.state.ar.us/water/planning/hab/archive.aspx"

# Read the HTML content
webpage <- read_html(url)

# Extract the table
hab_table <- webpage %>%
  html_nodes("table") %>%
  html_table() %>%
  .[[1]] # Taking the first table on the page

# Clean column names (optional)
names(hab_table) <- gsub(" ", "_", names(hab_table))

# Display the first few rows to verify
head(hab_table)

# Save the data to a CSV file
write.csv(hab_table, "arkansas_hab_data.csv", row.names = FALSE)

cat("Data has been successfully scraped and saved to 'arkansas_hab_data.csv'\n")
