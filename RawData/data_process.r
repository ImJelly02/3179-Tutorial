# Load necessary libraries
library(dplyr)

# Read the datasets
iso_countries <- read.csv("ISO3166_Countries.csv")
population <- read.csv("population_new.csv")
lat_long <- read.csv("world_country_and_usa_states_latitude_and_longitude_values.csv")

# Step 1: Data Cleaning and Wrangling for ISO3166_Countries dataset
iso_cleaned <- iso_countries %>%
  select(alpha.2, alpha.3, region, sub.region) %>%
  rename("country_code" = alpha.2,  # Renaming alpha-2 to country_code
         "Country.of.origin..ISO." = alpha.3,
         "Region" = region,
         "Sub-Region" = sub.region)

# Step 2: Data Cleaning for Population dataset
population_cleaned <- population %>%
  filter(!is.na(Country.of.origin..ISO.) & Country.of.origin..ISO. != "") %>%  # Remove rows with missing/invalid country codes
  filter(!is.na(Year) & Year > 2000) %>%                                       # Filter rows with a valid year (e.g., after 2000)
  mutate(across(starts_with("Refugees"), as.numeric))                   # Ensure numeric fields are numeric

# Step 3: Merging the cleaned population dataset with ISO country dataset
merged_data <- population_cleaned %>%
  left_join(iso_cleaned, by = "Country.of.origin..ISO.")

# Step 4: Selecting relevant columns from latitude and longitude dataset and renaming them
lat_long_cleaned <- lat_long %>%
  select(country_code, 
         latitude_origin = latitude,  # Renaming latitude to latitude_origin
         longitude_origin = longitude)  # Renaming longitude to longitude_origin

# Step 5: Merging with latitude and longitude dataset
final_data <- merged_data %>%
  left_join(lat_long_cleaned, by = "country_code")

# Step 6: Update missing latitude and longitude for South Sudan (SSD)
# Assign values manually for rows with country_code == "SSD"
# Updating the country_code to "SS" for South Sudan
final_data <- final_data %>%
  mutate(
    country_code = ifelse(Country.of.origin..ISO. == "SSD", "SS", country_code),
    latitude_origin = ifelse(Country.of.origin..ISO. == "SSD" & is.na(latitude_origin), 7.8627, latitude_origin),
    longitude_origin = ifelse(Country.of.origin..ISO. == "SSD" & is.na(longitude_origin), 29.6947, longitude_origin)
  )

# Step 7: Replace latitude and longitude with NA for Stateless (XXA) entries
final_data <- final_data %>%
  mutate(
    latitude_origin = ifelse(country_code == "XXA", NA, latitude_origin),
    longitude_origin = ifelse(country_code == "XXA", NA, longitude_origin)
  )

# Step 8: Add latitude_asylum and longitude_asylum to every row
final_data <- final_data %>%
  mutate(
    latitude_asylum = 4.210484,
    longitude_asylum = 101.975766
  )

# Step 9: Renaming all columns to make them more tidy and consistent
final_data <- final_data %>%
  rename(
    year = Year,
    country_of_origin = Country.of.origin,
    country_of_origin_iso = Country.of.origin..ISO.,
    country_of_asylum = Country.of.asylum,
    country_of_asylum_iso = Country.of.asylum..ISO.,
    refugees_under_unhcr_mandate = Refugees.under.UNHCR.s.mandate,
    asylum_seekers = Asylum.seekers,
    country_code = country_code,
    region = Region,
    sub_region = `Sub-Region`,  # Correcting with the right column name
    latitude_origin = latitude_origin,
    longitude_origin = longitude_origin,
    latitude_asylum = latitude_asylum,
    longitude_asylum = longitude_asylum
  )

# Step 10: Inspect the final merged data
head(final_data)

# Step 11: Save the cleaned and merged dataset if needed
write.csv(final_data, "cleaned_population.csv", row.names = FALSE)
