# Load required libraries
library(readr)
library(dplyr)

# Read the CSV file, skipping the first row
trans_health <- read_csv("Survey-Methodology-Group-Project/Trans Health Map 2024 European Union - Sheet1.csv", 
                         skip = 1)

# Rename the first column from "...1" to "Country"
trans_health <- trans_health %>%
  rename(Country = `...1`)

# Now select only the columns you want to keep
trans_health_filtered <- trans_health %>% 
  select(
    "Country",
    "Psychological care",
    "Psychiatric care",
    "Breast augmentation",
    "Electrolysis / Laser Hair Removal",
    "Facial Feminisation Surgery",
    "HRT (Oestrogen)",
    "HRT (Testosterone)",
    "Hysterectomy",
    "Mastectomy",
    "Metoidioplasty",
    "Orchiectomy",
    "Ovariectomy (aka oopherectomy)",
    "Phalloplasty",
    "Tracheal shave",
    "Vaginoplasty",
    "Vocal training"
  )

# Replace responses within the selected columns with numerical values
trans_health_filtered <- trans_health_filtered %>%
  mutate(across(
    -Country,  # Exclude the 'Country' column
    ~case_when(
      grepl("^Available", ., ignore.case = TRUE) ~ "1",  # If response starts with "Available"
      grepl("^Not Available", ., ignore.case = TRUE) ~ "2",  # If response starts with "Not Available"
      grepl("^Not Enough information", ., ignore.case = TRUE) ~ "3",  # If response starts with "Not Enough information"
      TRUE ~ .  # Leave other responses unchanged
    )
  ))

# View the transformed dataset
View(trans_health_filtered)


# Remove the row where "Country" contains "TGEU — Trans Europe and Central Asia·Privacy·Impressu"
trans_health_filtered <- trans_health_filtered %>%
  filter(!grepl("TGEU — Trans Europe and Central Asia·Privacy·Impressu", Country))

# View the cleaned dataset to confirm the row is removed
View(trans_health_filtered )



##SPAIN MODE##

# Define a simple mode function
get_mode <- function(x) {
  ux <- unique(na.omit(x))
  ux[which.max(tabulate(match(x, ux)))]
}

# List the Spain regions
spain_regions <- c("Spain: Andalusia", "Spain: Aragon", "Spain: Balearics", 
                   "Spain: Canary Islands", "Spain: Cantabria", "Spain: Catalonia", 
                   "Spain: Estremadura", "Spain: Galicia")

# Filter and combine in one step
spain_data <- trans_health_filtered %>%
  filter(Country %in% spain_regions)

# Create Spain row with modes
spain_combined <- data.frame(Country = "Spain")
for(col in setdiff(names(spain_data), "Country")) {
  spain_combined[[col]] <- get_mode(spain_data[[col]])
}

# Create final dataset (remove Spain regions, add combined row)
trans_health_final <- bind_rows(
  filter(trans_health_filtered, !(Country %in% spain_regions)),
  spain_combined
)


View(trans_health_final)
