# Organizing food_access1

food_access1 <- read.csv("C:/Users/osarasty/OneDrive - Texas Tech University/food data competition/food_access1.csv") 

food_access1 <- food_access1[order(food_access1$fips, food_access1$year), ]

food_access1 <- food_access1[food_access1$fips >= 100, ]

# Identify desired variables
food_access1$z <- 0
food_access1$z[food_access1$variable_name == "food_insecurity_rate"] <- 1
food_access1$z[food_access1$variable_name == "child_food_insecurity_rate"] <- 2
food_access1$z[food_access1$variable_name == "mean_usual_hours"] <- 3

# Keep variables of interest in desired year
food_access1 <- subset(food_access1, z != 0 & year == 2019 & state_name != "Puerto Rico")

# Sort by 'fips' and 'z'
food_access1 <- food_access1[order(food_access1$fips, food_access1$z), ]

# Calculate 'newvar' as the total of 'z' within each 'fips' group
food_access1 <- transform(food_access1, newvar = ave(z, fips, FUN = sum))

# Drop rows where 'newvar' is equal to 3 and the 'topic_area' variable
food_access1 <- subset(food_access1, newvar != 3, select = -c(topic_area))

# Change from long format to wide format
wide_foodaccess1 <- reshape(food_access1, idvar = "fips", timevar = "z", direction = "wide")

# Rename variables of interest
colnames(wide_foodaccess1) <- gsub("value.1", "food_insecurity_rate", colnames(wide_foodaccess1))
colnames(wide_foodaccess1) <- gsub("value.2", "child_food_insecurity_rate", colnames(wide_foodaccess1))
colnames(wide_foodaccess1) <- gsub("value.3", "mean_hours_work", colnames(wide_foodaccess1))

# Keep variables of interest
wide_foodaccess1 <- wide_foodaccess1[, -which(names(wide_foodaccess1) == "category.1")]
colnames(wide_foodaccess1)[colnames(wide_foodaccess1) == "state_name.1"] <- "state_name"
colnames(wide_foodaccess1)[colnames(wide_foodaccess1) == "county_name.1"] <- "county_name"
colnames(wide_foodaccess1)[colnames(wide_foodaccess1) == "year.1"] <- "year"

columns_to_drop <- c("category.1", "category.2", "category.3",
                     "newvar.1", "newvar.2", "newvar.3",
                     "county_name.2", "county_name.3",
                     "state_name.2", "state_name.3",
                     "year.2", "year.3",
                     "variable_name.1", "variable_name.2", "variable_name.3")

# Drop the specified columns
wide_foodaccess1 <- wide_foodaccess1[, -which(names(wide_foodaccess1) %in% columns_to_drop)]


########################################################################################################################
# Organizing food_access2

food_access2 <- read.csv("C:/Users/osarasty/OneDrive - Texas Tech University/food data competition/food_access2.csv") 

# Sort by 'fips' and 'year'
food_access2 <- food_access2[order(food_access2$fips, food_access2$year), ]

# Drop rows with 'fips' less than 100 (for county)
food_access2 <- subset(food_access2, fips >= 100)

# Keep variables of interest in the desired year
food_access2$z <- 0
food_access2$z[food_access2$variable_name == "SNAP_percent_white_alone_not_hispanic"] <- 1
food_access2 <- subset(food_access2, z == 1 & year == 2019)

# Rename variables of interest
colnames(food_access2)[colnames(food_access2) == "value"] <- "SNAP_white_alone_not_hispanic"

# Keep only variables of interest
food_access2 <- food_access2[, c("fips", "SNAP_white_alone_not_hispanic", "year")]
food_access2 <- food_access2[order(food_access2$fips), ]

# Merge dataframes based on 'fips'
merged_foodaccess <- merge(wide_foodaccess1, food_access2, by = "fips", all = TRUE)
colnames(merged_foodaccess)[colnames(merged_foodaccess) == "year.x"] <- "year"
merged_foodaccess<- subset(merged_foodaccess, select = -c(year.y))

############################################################################################################################
# Organizing Grants

grants <- read.csv("C:/Users/osarasty/OneDrive - Texas Tech University/food data competition/grants.csv") 

# Keep only rows where year is 2019 and variable_name is "food_desert_vehicle"
grants <- subset(grants, year == 2019 & variable_name == "food_desert_vehicle")

# Rename 'value' column to 'food_desert_vehicle'
colnames(grants)[colnames(grants) == "value"] <- "food_desert_vehicle"

# Keep only 'fips', 'food_desert_vehicle', and 'year' columns
grants <- grants[, c("fips", "food_desert_vehicle", "year")]

# Sort by 'fips'
grants <- grants[order(grants$fips), ]

# Merge dataframes based on 'fips'
merge_data2 <- merge(merged_foodaccess, grants, by = "fips", all = TRUE)
colnames(merge_data2)[colnames(merge_data2) == "year.x"] <- "year"
merge_data2<- subset(merge_data2, select = -c(year.y))


############################################################################################################################
# Organizing Analitic Data

analytic_data2019 <- read.csv("C:/Users/osarasty/OneDrive - Texas Tech University/food data competition/analytic_data2019.csv")

# Rename 'fipscode' to 'fips'
colnames(analytic_data2019)[colnames(analytic_data2019) == "fipscode"] <- "fips"

# Drop rows with 'fips' less than 100 (for county)
analytic_data2019 <- subset(analytic_data2019, fips >= 100)

# Sort by 'fips'
analytic_data2019 <- analytic_data2019[order(analytic_data2019$fips), ]

columns_to_drop2 <- c("statecode", "countycode", "state", "county", "year", "county_ranked")

# Drop the specified columns
analytic_data2019 <- analytic_data2019[, -which(names(analytic_data2019) %in% columns_to_drop2)]


# Merge dataframes based on 'fips'
data <- merge(merge_data2, analytic_data2019, by = "fips", all = TRUE)
data <- data[!is.na(data$county_name) & data$county_name != "", ]
selected_vars <- c("fips", "state_name", "county_name", "year", "food_insecurity_rate",
                   "child_food_insecurity_rate", "mean_hours_work", "food_desert_vehicle", "SNAP_white_alone_not_hispanic",
                   "v001_rawvalue", "v133_rawvalue", "v137_rawvalue", "v147_rawvalue", "v128_rawvalue",
                   "v129_rawvalue", "v060_rawvalue", "v083_rawvalue", "v003_rawvalue", "v122_rawvalue",
                   "v063_rawvalue", "v065_rawvalue", "v142_rawvalue", "v153_rawvalue", "v051_rawvalue",
                   "v052_rawvalue", "v053_rawvalue", "v054_rawvalue", "v055_rawvalue", "v081_rawvalue",
                   "v080_rawvalue", "v056_rawvalue", "v126_rawvalue", "v059_rawvalue", "v057_rawvalue", "v058_rawvalue")

# Subset the dataframe to selected variables
data <- data[selected_vars]

# Model 1 - Predict missing values for v003_rawvalue
model1 <- glm(v003_rawvalue ~ v137_rawvalue + v153_rawvalue + v051_rawvalue + v052_rawvalue +
                v053_rawvalue + v054_rawvalue + v056_rawvalue + v059_rawvalue, data = data)
pred1 <- predict(model1, type = "response")
data$nv003_rawvalue <- data$v003_rawvalue
data$nv003_rawvalue[is.na(data$v003_rawvalue)] <- pred2[is.na(data$v003_rawvalue)]

# Model 2 - Predict missing values for v083_rawvalue
model2 <- glm(v083_rawvalue ~ v137_rawvalue + v153_rawvalue + v051_rawvalue + v052_rawvalue +
                v053_rawvalue + v054_rawvalue + v056_rawvalue + v059_rawvalue, data = data)
pred2 <- predict(model2, type = "response")
data$nv083_rawvalue <- data$v083_rawvalue
data$nv083_rawvalue[is.na(data$v083_rawvalue)] <- pred2[is.na(data$v083_rawvalue)]

# Model 3 - Predict missing values for v065_rawvalue
model3 <- glm(v065_rawvalue ~ v137_rawvalue + v153_rawvalue + v051_rawvalue + v052_rawvalue +
                v053_rawvalue + v054_rawvalue + v056_rawvalue + v059_rawvalue, data = data)
pred3 <- predict(model3, type = "response")
data$nv065_rawvalue <- data$v065_rawvalue
data$nv065_rawvalue[is.na(data$v065_rawvalue)] <- pred3[is.na(data$v065_rawvalue)]

# Create an empty 'index' column
data$index <- NA

# Perform fractional probit regression
model <- glm(food_insecurity_rate ~ nv083_rawvalue + nv003_rawvalue + nv065_rawvalue + mean_hours_work +
               food_desert_vehicle + SNAP_white_alone_not_hispanic, data = data, family = quasibinomial(link = "probit"))

# Predict the index for non-missing values
data$index[!is.na(data$food_insecurity_rate)] <- predict(model, newdata = data[!is.na(data$food_insecurity_rate), ], type = "response")

# Specify the file path and name for the CSV file
file_path <- "C:/Users/osarasty/OneDrive - Texas Tech University/food data competition/data.csv"

# Export the 'data' dataframe to CSV
write.csv(data, file = file_path, row.names = FALSE)


