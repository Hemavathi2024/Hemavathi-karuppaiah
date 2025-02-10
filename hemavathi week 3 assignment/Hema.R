# Load required libraries
library(readxl)
library(geosphere)

# Read the Excel file
clinic_data <- read_excel("clinics.xls")

# Convert latitude and longitude to numeric
clinic_data$locLat <- as.numeric(clinic_data$locLat)
clinic_data$locLong <- as.numeric(clinic_data$locLong)

# Function for Haversine distance calculation
haversine_distance <- function(lat1, lon1, lat2, lon2) {
  R <- 3959
  lat1 <- lat1 * pi / 180
  lon1 <- lon1 * pi / 180
  lat2 <- lat2 * pi / 180
  lon2 <- lon2 * pi / 180
  dlat <- lat2 - lat1
  dlon <- lon2 - lon1
  a <- sin(dlat/2)^2 + cos(lat1) * cos(lat2) * sin(dlon/2)^2
  c <- 2 * asin(sqrt(a))
  
  return(R * c)
}

# 1. Basic for loop (least efficient)
system.time({
  distances1 <- numeric(nrow(clinic_data))
  for(i in 1:nrow(clinic_data)) {
    distances1[i] <- haversine_distance(40.671, -73.985, 
                                        clinic_data$locLat[i], 
                                        clinic_data$locLong[i])
  }
})

# 2. Vectorized approach using apply (more efficient)
system.time({
  distances2 <- apply(clinic_data[, c("locLat", "locLong")], 1, 
                      function(x) haversine_distance(40.671, -73.985, x[1], x[2]))
})

# 3. Using the geosphere package
system.time({
  distances3 <- distHaversine(cbind(-73.985, 40.671), 
                              cbind(clinic_data$locLong, clinic_data$locLat))
})