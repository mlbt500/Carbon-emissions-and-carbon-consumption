library(ggplot2)
library(dplyr)
consumption <- read.csv("consumption-co2-emissions.csv")
ncol(consumption)
peers <- countries <- c(
  "Australia", "Switzerland", "Finland", "France", "United Kingdom",
  "Ireland", "Netherlands", "United States", "Spain", "Italy", "Norway", "New Zealand", "Sweden"
)

consumption2 <- consumption[consumption$Entity %in% peers,]
consumption2

# Normalizing the data
normalize_function <- function(df) {
  df$normalized_emission <- ifelse(df$Year == 1990, 1, 
                                   df$Annual.consumption.based.CO..emissions / 
                                     df$Annual.consumption.based.CO..emissions[df$Year == 1990])
  return(df)
}

normalized_data <- split(consumption2, consumption2$Entity)
normalized_data <- lapply(normalized_data, normalize_function)
normalized_data <- do.call(rbind, normalized_data)

# Plotting the data
ylim_values <- range(normalized_data$normalized_emission, na.rm = TRUE)

plot(0, 0, 
     type = "n", 
     xlim = range(normalized_data$Year), 
     ylim = ylim_values, 
     xlab = "Year", 
     ylab = "Normalized CO2 Emissions", 
     main = "Annual Consumption-based CO2 Emissions (Normalized to 1990 values)")

for (country in unique(normalized_data$Entity)) {
  country_data <- normalized_data[normalized_data$Entity == country, ]
  if(country == "United Kingdom") {
    lines(country_data$Year, country_data$normalized_emission, col = "red", lwd = 2)
  } else {
    lines(country_data$Year, country_data$normalized_emission, col = "lightgrey")
  }
}

# Define y-axis breaks and labels
y_breaks <- seq(ylim_values[1], ylim_values[2], by = 0.1)
y_labels <- round(y_breaks, 1)

axis(2, at = y_breaks, labels = y_labels)

# Defining distinct colors for the four countries
colors_for_lowest <- c("black", "darkblue", "darkgrey", "red")

# Associating the countries with their colors
country_colors <- setNames(colors_for_lowest, lowest_countries$Entity)

# Plotting the data as before
plot(0, 0, 
     type = "n", 
     xlim = range(normalized_data$Year), 
     ylim = c(0.5, 2.5), 
     xlab = "Year", 
     ylab = "Normalised CO2 Emissions", 
     main = "Annual Emissions 1990 values")

# Plotting all countries in grey first
for (country in setdiff(unique(normalized_data$Entity), lowest_countries$Entity)) {
  country_data <- normalized_data[normalized_data$Entity == country, ]
  lines(country_data$Year, country_data$normalized_emission, col = "lightgrey")
}


# Plotting the four countries with lowest emissions in their respective colors
for (country in names(country_colors)) {
  country_data <- normalized_data[normalized_data$Entity == country, ]
  lines(country_data$Year, country_data$normalized_emission, col = country_colors[country], lwd = 2)
}



# Adding a legend
legend("topright", 
       legend = c(names(country_colors), "Other countries"), 
       col = c(country_colors, "lightgrey"), 
       lwd = 2, 
       bty = "n", 
       cex = 0.8)