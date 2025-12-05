library(readxl)

# Read the file
df <- read_excel("/kaggle/input/policy-data/JLOP_2025.xlsx")

# Preview
#head(df)

#unique(df$CountryImposing_cleaned)

#Preparing product complexity and economic complexity files

df_prod <- read.csv("/kaggle/input/complexity-files/complexity_pci_a_hs07_hs6.csv")

df_ctry <- read.csv("/kaggle/input/complexity-files/complexity_eci_a_hs07_hs6.csv")


years <- 2010:2022

ctrys <- unique(df_ctry$country_name)
prods <- unique(df_prod$hs2_id)

# create an empty vector to store ECI and PCI means
eci <- numeric(length(ctrys))
pci <- numeric(length(prods))

# loop through countries
for (i in seq_along(ctrys)) {
  country <- ctrys[i]
  
  # filter data for that country and the selected years
  df_filt <- subset(df_ctry, country_name == country & year %in% years)
  
  # calculate mean ECI
  eci[i] <- mean(df_filt$eci, na.rm = TRUE)
}

#loop through products
for (i in seq_along(prods)) {
    product <- prods[i]
    df_filt <- subset(df_prod, hs2_id == product & year %in% years)
    pci[i] <- mean(df_filt$pci, na.rm = TRUE)
}

# combine results into a dataframe
df_eci <- data.frame(country = ctrys, eci = eci)
df_pci <- data.frame(product = prods, pci = pci)

df_pci$product <- sprintf("%02d", df_pci$product)
df_pci$pci <- (df_pci$pci - min(df_pci$pci)) / (max(df_pci$pci) - min(df_pci$pci))
df_eci$eci <- (df_eci$eci - min(df_eci$eci)) / (max(df_eci$eci) - min(df_eci$eci))

#head(df_eci)
#head(df_pci)

library(readxl)

# Read the file (Countries_list created manually by removing the combined countries)
countries_list <- read_excel("/kaggle/input/countries/Countries.xlsx")


#Checking country column for missing countries

# Find countries that are present in df_eci
common_countries <- countries_list$Country[countries_list$Country %in% df_eci$country]

# Find countries that are missing from df_eci
missing_countries <- countries_list$Country[!countries_list$Country %in% df_eci$country]


# Count of common and missing
common_count <- length(common_countries)
missing_count <- length(missing_countries)

# Print results
cat("Common countries:", common_count, "\n")
cat("Missing countries:", missing_count, "\n")
cat("List of missing countries:\n")
print(missing_countries)

#Making necessary changes to match

# Replace country names with those mentioned in the OEC file
#Making changes in the countries_list file
countries_list$Country <- dplyr::recode(countries_list$Country,
  "Taiwan" = "Chinese Taipei",
  "Congo" = "Republic of Congo",
  "Kyrgyztan" = "Kyrgyzstan",
  "Republic of Korea" = "South Korea",
  "Myanmar" = "Burma",
  "United States of America" = "United States",
  "Bosnia & Herzegovina" = "Bosnia and Herzegovina",
  "DR Congo" = "Democratic Republic of the Congo"
)

#Making changes in the main dataframe
df$CountryImposing_cleaned <- dplyr::recode(df$CountryImposing_cleaned,
  "Taiwan" = "Chinese Taipei",
  "Congo" = "Republic of Congo",
  "Kyrgyztan" = "Kyrgyzstan",
  "Republic of Korea" = "South Korea",
  "Myanmar" = "Burma",
  "United States of America" = "United States",
  "Bosnia & Herzegovina" = "Bosnia and Herzegovina",
  "DR Congo" = "Democratic Republic of the Congo"
)

# Remove countries missing from the OEC file
to_remove <- c(
  "Anguilla", "Burundi", "Bahamas", "Belize", "Barbados", "Bhutan",
  "Cape Verde", "Cyprus", "Dominica", "Fiji", "Gambia", "Equatorial Guinea",
  "Guam", "Haiti", "Lesotho", "Latvia", "North Macedonia", "Mauritius", "Nepal",
  "Puerto Rico", "Rwanda", "Sao Tome & Principe", "Swaziland", "Tonga",
  "US Virgin Islands", "Samoa", "Falkland Islands", "Antigua & Barbuda",
  "Bahrain", "Bermuda", "Brunei Darussalam", "Central African Republic",
  "Comoros", "Cayman Islands", "Djibouti", "Estonia", "Faroe Islands",
  "Guinea-Bissau", "Grenada", "Guyana", "Iceland", "Saint Kitts & Nevis",
  "Saint Lucia", "Luxembourg", "Maldives", "Malta", "Montenegro", "New Caledonia",
  "Nauru", "State of Palestine", "Solomon Islands", "Suriname", "Seychelles",
  "Trinidad and Tobago", "Saint Vincent & the Grenadines", "Vanuatu"
)

# Filter out these countries from countries_list
countries_list <- countries_list[!countries_list$Country %in% to_remove, ]
# Filter out these countries from the main dataframe
df <- df[!df$CountryImposing_cleaned %in% to_remove, ]


filtered_df <- df[df$CountryImposing_cleaned %in% countries_list$Country, ]

unique(filtered_df$CountryImposing_cleaned)

library(writexl)


#Cleaning up and organizing the MeasureAffectedProducts column
library(dplyr)
library(tidyr)
library(stringr)

df_clean <- filtered_df %>%
  mutate(MeasureAffectedProducts = str_squish(MeasureAffectedProducts)) %>%
  filter(!is.na(MeasureAffectedProducts) & MeasureAffectedProducts != "NA" & MeasureAffectedProducts != "") %>%          # remove NA rows
  separate_rows(MeasureAffectedProducts, sep = ",")    # split comma-separated values into rows

head(df_clean)
dim(df_clean)


#Making two dataframes - One for industrial policy, and one for non-industrial policy interventions

df_IP = df_clean[df_clean$D_IP_bert_3==1, ]

df_nonIP = df_clean[df_clean$D_OTHER_INTENTION_bert_3==1, ]

dim(df_IP)

dim(df_nonIP)

#Just to remove empty rows which get created after the separate rows operation
df_IP <- df_IP %>%
  filter(MeasureAffectedProducts != "")

df_nonIP <- df_nonIP %>%
  filter(MeasureAffectedProducts != "")

dim(df_IP)

dim(df_nonIP)

write_xlsx(df_IP, "/kaggle/working/Industrial Policy data.xlsx")

write_xlsx(df_nonIP, "/kaggle/working/Non Industrial Policy data.xlsx")


#Making affected products 2 digit
str(df_IP$MeasureAffectedProducts)

#Using this command to extract first two characters in the string
df_IP$prod_code_4 <- substr(df_IP$MeasureAffectedProducts,1,2)
df_nonIP$prod_code_4 <- substr(df_nonIP$MeasureAffectedProducts,1,2)

#head(df_IP)

#Removing duplicate rows considering the year to be irrelevant for the analysis (Based on country-policy-product)

library(dplyr)

df_IP_unique <- df_IP %>%
    distinct(CountryImposing_cleaned, prod_code_4, MeasureType, .keep_all = TRUE)

df_nonIP_unique <- df_nonIP %>%
    distinct(CountryImposing_cleaned, prod_code_4, MeasureType, .keep_all = TRUE)
