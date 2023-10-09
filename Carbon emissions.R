emissions <- read.csv("co2-emissions-and-gdp.csv")
emissions <- emissions[,c(1:3, 5)]
peers <- countries <- c(
  "Australia", "Switzerland", "Finland", "France", "United Kingdom",
  "Ireland", "Netherlands", "United States", "Spain", "Italy", "Norway", "New Zealand", 
  "Sweden", "Norway", "Austria", "Germany"
)
emissions2 <- emissions[emissions$Entity %in% peers,]

# Normalize the data
normalize_function <- function(df) {
  df$normalized_emission <- ifelse(df$Year == 1990, 1, 
                                   df$Annual.CO..emissions / 
                                     df$Annual.CO..emissions[df$Year == 1990])
  return(df)
}

normalized_data <- split(emissions2, emissions2$Entity)
normalized_data <- lapply(normalized_data, normalize_function)
normalized_data <- do.call(rbind, normalized_data)

# Plotting the data
ylim_values <- range(normalized_data$normalized_emission, na.rm = TRUE)
par(mar=c(6, 4, 2, 2) + 0.1)
plot(0, 0, 
     type = "n", 
     xlim = range(normalized_data$Year), 
     ylim = ylim_values,
     xlab = "",
     ylab = "Normalised CO2 Emissions", 
     main = "Annual CO2 Emissions/1990 values")

for (country in unique(normalized_data$Entity)) {
  country_data <- normalized_data[normalized_data$Entity == country, ]
  if(country == "United Kingdom") {
    lines(country_data$Year, country_data$normalized_emission, col = "red", lwd = 2)
  } else {
    lines(country_data$Year, country_data$normalized_emission, col = "lightgrey")
  }
}

mtext("*Australia, Switzerland, Finland, France, United Kingdom, Ireland, Netherlands, 
United States, Spain, Italy, Norway, New Zealand, Sweden, Norway, Austria, Germany
**Our World in Data",  side = 1, line = 4, cex = 0.7, adj = 0)

legend("topright", 
       legend = c("United Kingdom"), 
       col = c("red"), 
       lwd = 2, 
       bty = "n", 
       cex = 1)