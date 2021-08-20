plot_regions <- list("Colombia", "Argentina", "Uruguay", "Brazil", "Central America and Caribbean", "Mexico", "South America_Northern", "South America_Southern")

total_basin_production_value <- read.csv("total_basin_production_value.csv")
basin_to_country_mapping <- read.csv("basin_to_country_mapping.csv", comment.char="#")

#UA
plot_UA_basin_production_value_2050 <- total_basin_production_value %>%
  left_join(basin_to_country_mapping, by = c("basin" = "GLU_name")) %>%
  filter(region %in% c("Uruguay", "Argentina"), year == 2050) %>%
  mutate(subRegion = paste0(Basin_long_name, "_X_", region)) %>%
  select(scenario, subRegion, value); plot_UA_basin_production_value_2050

plot_map <- rmap::map(plot_UA_basin_production_value_2050,
                      title = paste("Basin agricultural production value in 2050"),
                      fileName = paste("production_", "2050", sep = ""))

#LAC
plot_LAC_basin_production_value_2050 <- total_basin_production_value %>%
  left_join(basin_to_country_mapping, by = c("basin" = "GLU_name")) %>%
  filter(region %in% plot_regions, year == 2050) %>%
  mutate(subRegion = paste0(Basin_long_name, "_X_", region)) %>%
  select(scenario, subRegion, value); plot_LAC_basin_production_value_2050

plot_map <- rmap::map(plot_LAC_basin_production_value_2050,
                      title = paste("Basin agricultural production value in 2050"),
                      fileName = paste("production_", "2050", sep = ""))

plot_UA_basin_production_value_2050$subRegion%>%unique() -> a1; a1
plot_LAC_basin_production_value_2050$subRegion%>%unique() -> a2; a2
a3 <- rmap::mapIntersectGCAMBasin32Reg$subRegion%>%unique(); a3
a4 <- rmap::mapIntersectGCAMBasinCountry$subRegion%>%unique(); a4

a1[grepl("Uruguay",a1)]
a2[grepl("Uruguay",a2)]
a3[grepl("Uruguay",a3)]

# Regions in LAC data not in BasinReg32
a2[!a2 %in% a3] # Reg 32
a2[!a2 %in% a4] # Basins Country

a1[!a1 %in% a3] # Reg 32
a1[!a1 %in% a4] # Basins Country

df <- rmap::mapIntersectGCAMBasinCountrydf %>% filter(subRegion_Country %in% c("Argentina","Uruguay","Brazil")); df
rmap::map(df, labels=T)
df1 <- rmap::mapIntersectGCAMBasin32Regdf %>% filter(subRegion_Country %in% c("Argentina","Uruguay","Brazil")); df1
rmap::map(df1, labels=T)

