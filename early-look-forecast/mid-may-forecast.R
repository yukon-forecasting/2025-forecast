library(dplyr)
library(readr)
library(lubridate)

final_year_with_cpue_data <- 2023

# pull in data
env_file <- read_csv("../data/data/environment/environment.csv")
cpue <- read_csv("../data/data/cpue/cpue.csv")
combined <- env_file |>
  left_join(cpue)

# train full model on mdj
training_set <- subset(combined, year <= final_year_with_cpue_data)
model_fifdj <- lm(fifdj ~ amatc + msstc + pice, training_set)
model_qdj <- lm(qdj ~ amatc + msstc + pice, training_set)
model_mdj <- lm(mdj ~ amatc + msstc + pice, training_set)

# pull in partial values for variables
amatc_latest <- read_csv("early-look-forecast/data_as_of_may_23/nome_air_temp-2025.csv")
msstc_latest <- read_csv("early-look-forecast/data_as_of_may_23/surface_temperature-2025.csv")
pice_latest <- read_csv("early-look-forecast/data_as_of_may_23/sea_ice_concentration-2025.csv")

# calculate mean values to predict against
amatc_2025 <- mean(amatc_latest$air_temperature_c)
msstc_2025 <- mean(msstc_latest$surface_temp_c)
pice_2025 <- mean(pice_latest$mean_sea_ice_percent, na.rm = TRUE) / 100
df_2025 <- data.frame(amatc=amatc_2025, msstc=msstc_2025, pice=pice_2025)

# predict
predict(model_fifdj, df_2025)
# 1
# 14.22248
predict(model_qdj, df_2025)
# 1
# 17.17507
predict(model_mdj, df_2025)
# 1
# 22.95678
