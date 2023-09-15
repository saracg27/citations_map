library(here)
library(ggplot2)
library(tidyr)
library(dplyr)
library(purrr)
library(sf)
install.packages("rnaturalearth", dependencies = TRUE)
library(rnaturalearth)
countries <- read.csv(file = here("Citations-by-county-mbt-2018.csv"))
vct <- as.data.frame(countries$Country)
vct_sep <- separate(vct, "countries$Country", into = c("country1", "country2", "country3", "country4", "country5", "country6", "country7", "country8"), sep = ";" )

# there are cells with only one space
vct_sep <- vct_sep |> 
    mutate_all(~ifelse(. == "", NA, .)) 
# everything to a vector
vector_values <- na.omit(as.vector(unlist(vct_sep)))
vector_values[vector_values == "USA"] <- "United States of America"

clean_counts <- as.data.frame(table(vector_values))
#Load world map

world_map <- ne_countries(scale = "medium", returnclass = "sf")
world_map <- merge(world_map, clean_counts, by.x = "name", by.y = "vector_values", all.x = TRUE)
world_map$Freq[is.na(world_map$Freq)] <- 0

p <- ggplot(data = world_map) +
    geom_sf(aes(fill = Freq)) +
    scale_fill_gradient(low = "white", high = "blue", name = "Country Mentions") +
    theme_classic() +
    labs(title = expression(paste("Countries citing Correa-Garcia et al. 2018 ", italic("(Microbial Biotechnology)"))),
         caption = "For a total of 88 citations from 37 countries, as of September 15th 2023")

ggsave(p, file = here("mbt_2018.eps"), width = 9, height = 6, units = "in", dpi = 300, bg = "white")



