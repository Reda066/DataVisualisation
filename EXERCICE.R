install.packages("readr")
library(readr)

install.packages("tibble")
library(tibble)

install.packages("dplyr")
library(dplyr)

install.packages("tidyr")
library(tidyr)

install.packages("stringr")
library(stringr)

install.packages("ggplot2")
library(ggplot2)

install.packages("scales")
library(scales)

round_2 <- read_csv('data/results_pres_elections_dept_2017_round_2.csv')
class(round_2)
round_2$region_name # use the $ symbol to access variables

#We can select variables based on their names
round_2 %>% select(region_name, 'LE PEN', MACRON)

#We can select variables based on their names, positions
round_2 %>% select(1:5)

#We can select variables based on their names, positions, by excluding variables
round_2 %>% select(-c(3:7), -region_code)

#select contain
round_2 %>% select(contains("vote"))

#Filtering by one criterion
round_2 %>% filter(region_name == "Provence-Alpes-Côte d'Azur")

#Filtering by multiple criteria within a single logical expression
round_2 %>% filter(registered_voters > 100000 & present_voters > 100000)

#Tri croissant 
round_2 %>% arrange(registered_voters)

#Creating a new variable that gives the voting rate per department
round_2 %>%
  mutate(voting_rate = present_voters/registered_voters) %>%
  select(c(1:4), voting_rate, everything())

#Creating a new variable that gives the rank of department according to the
#number of votes for Emmanuel Macron
round_2 %>%
  mutate(rank = min_rank(desc(MACRON))) %>%
  select(dept_name, MACRON, rank) %>%
  arrange(rank)

#Summarise 
  #Recovering the total number of votes over the country
  round_2 %>%
    summarise(total_votes = sum(votes_cast))
  
  #Total number of votes per region
  round_2 %>% group_by(region_name) %>%
    summarise(total_votes = sum(votes_cast))
  
##############################################################################

geo_data <- read_csv("data/coordinates_regions_2016.csv")
  
#The left_join() function joins tibbles x and y by returning all rows from x, and all columns from x and y
round_2 %>% left_join(geo_data, by=c("region_code"="insee_reg")) %>%
  select(region_code, region_name, latitude, longitude, everything())

#Using dplyr::bind_rows() function, we combine combine two tibbles to obtain a single tibble
#with results from both rounds of the presidential election.
round_1 <- read_csv('data/results_pres_elections_dept_2017_round_1.csv')
results <- round_1 %>% mutate(round = "Round 1") %>%
  bind_rows(round_2 %>% mutate(round = "Round 2"))


round_2 %>% gather(candidate, votes, c(`LE PEN`, MACRON)) %>%
  arrange(region_name, dept_name) %>%
  select(region_name, candidate, votes, everything())

# Example 1. Calculating the number of votes per candidate and department
  #Using the input data format
  round_2 %>% group_by(region_name) %>%
  summarise(votesLePen = sum(`LE PEN`),
              votesMacron = sum(MACRON),
              .groups='drop')

  #?Using the data format after applying tidyr::gather()

  round_2 %>% group_by(region_name, candidate) %>%
    summarise(votes = sum(votes),
              .groups='drop')

#?Example 2. Identifying the winner candidate per department
  round_2 %>%
    group_by(dept_name) %>%
    mutate(rank = min_rank(desc(votes))) %>%
    arrange(dept_name, rank) %>%
    mutate(winner = if_else(rank == 1, TRUE, FALSE)) %>%
    select(dept_name, candidate, votes, rank, winner)
  
  round_2 %>% spread(candidate, votes) %>%
    select(region_name, `LE PEN`, MACRON, everything())

  
  #############Abstract Data Visualization###############
  
  plot_df <- round_2 %>% group_by(region_code, region_name, candidate) %>%
    summarise(votes = sum(votes))
  
  
  plot <- plot +
    geom_col(aes(x = region_name, y = votes, fill = candidate),
             position = 'dodge')

  plot <- plot + scale_y_continuous(labels = number_format(scale = 1/1000000, suffix = 'M'))
  plot + scale_fill_brewer(palette = 'Set1')
  
  plot <- plot + scale_fill_manual(values = c('#003171', '#ffea00'))
  plot <- plot + theme_bw() + theme(axis.text.x = element_text(angle = 60, hjust = 1))
  
  plot <- plot + labs(title = "Presidential elections of 2017",
                      subtitle = "Votes per region and candidate",
                      caption = "Data source: https://www.data.gouv.fr/en/posts/les-donnees-des-elections/",
                      y = "Number of votes", x = "Region") +
    guides(fill = guide_legend(title = 'Candidate'))
  
  # Summarized chunk code of the bar chart
  ggplot(plot_df) +
    geom_col(aes(x = region_name, y = votes, fill = candidate), # geometric object
             position = 'dodge') +
    scale_y_continuous(labels = number_format(scale = 1/1000000, # y axis format
                                              suffix = 'M')) +
    scale_fill_manual(values = c('#003171', '#ffea00')) + # fill colors
    theme_bw() + # theme
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = 'bottom') +
    labs(title = "Presidential elections of 2017", # title and labels
         subtitle = "Votes per region and candidate",
         caption = "Data source: https://www.data.gouv.fr/
en/posts/les-donnees-des-elections/",
         y = "Number of votes", x = "Region") +
    guides(fill = guide_legend(title = 'Candidate')) # legend
  
  #Combining geometric objects
  missing_votes <- round_2 %>%
    distinct(region_code, dept_code, .keep_all = TRUE) %>% # keep only one observation per department
    group_by(region_code, region_name) %>%
    summarise(blank_ballot = sum(blank_ballot), null_ballot = sum(null_ballot),
              absent_voters = sum(absent_voters)) %>%
    gather(category, value, c(3:5))
  
  
  ggplot(plot_df, aes(x = region_name)) + # common aesthetics
    geom_col(aes(y = votes, fill = candidate), position = 'dodge') +
    # geom_line object for a second variable
    geom_line(data = missing_votes, # new data
              aes(y = value,
                  linetype = category,
                  group = category)) + # aesthetics
    scale_y_continuous(labels = number_format(scale = 1/1000000,
                                              suffix = 'M')) +
    scale_fill_manual(values = c('#003171', '#ffea00')) +
    theme_bw() + theme(axis.text.x = element_text(angle = 60, hjust = 1),
                       legend.position = 'right') +
    labs(title = "Presidential elections of 2017",
         y = "Number of votes", x = "Region") +
    guides(fill = guide_legend(title = 'Candidate'),
           linetype = guide_legend(title = '')) + # title of linetype legend
    scale_linetype_discrete(labels = c("Absent Voters", "Blank Ballot",
                                       "Null Ballot")) # labels for each linetype
  
  
  #Decomposition components: facets
  ggplot(results, aes(x = region_name)) +
    geom_col(aes(y = votes, fill = candidate),
             position = 'fill') + # to generate stacked bars
    scale_y_continuous(labels = percent_format()) + # y axis format as percent
    scale_fill_brewer(palette = 'Paired') +
    theme_bw() + theme(legend.position = 'bottom') +
    labs(title = "Results of presidential elections of 2017",
         y = "Proportion of votes", x = "Region") +
    guides(fill = guide_legend(title = 'Candidate'),
           linetype = guide_legend(title = '')) +
    scale_linetype_discrete(labels = c("Absent Voters", "Blank Ballot", "Null Ballot")) +
    # define cols as the number of different values for the variable "round"
    facet_grid(cols = vars(round)) +
    coord_flip() # flip coordinate system
  
  #############GEOSPATIAL DATA###############
  install.packages('sf')
  library(sf)
  regions_sf <- st_read('data/shapefile/contours-geographiques-des-regions-2019.shp')
  
  #Geospatial data manipulation
  data_sf <- regions_sf %>%
    left_join(plot_df, by = c('insee_reg'='region_code'))
  as_tibble(data_sf) # print sf objects in a nice format
  
  #Static thematic maps with ggplot2
  ggplot(data_sf) +
    geom_sf(aes(fill = votes)) +
    facet_grid(cols = vars(candidate)) +
    scale_fill_viridis_c(name = 'Number of Votes',
                         labels = number_format(scale = 1/1000000, suffix = 'M')) +
    guides(fill = guide_colourbar(title.position = 'top')) +
    theme_minimal() +
    theme(legend.position = "bottom", legend.key.width = unit(2, 'cm'))

  install.packages('leaflet')
  library(leaflet)
  plot_df <- round_2 %>% distinct(region_code, dept_code, .keep_all = TRUE) %>%
    group_by(region_code, region_name) %>%
    summarise(present_voters = sum(present_voters), registered_voters = sum(registered_voters),
              voting_rate = present_voters/registered_voters, .groups = "drop")
  plot_sf <- regions_sf %>% left_join(plot_df, by = c('insee_reg'='region_code'))
  quants <- quantile(plot_sf$voting_rate, probs = seq(from = 0, to = 1, by = 0.2))
  color_scale <- colorBin("YlOrRd", domain = plot_sf$voting_rate, bins = quants)
  map_leaflet <- leaflet(data = plot_sf) %>%
    addProviderTiles(providers$OpenStreetMap) %>%
    addPolygons(fillColor = ~color_scale(voting_rate), fillOpacity = 0.7,
                color = "white", weight = .5, opacity = 1, dashArray = "3") %>%
    addLegend(pal = color_scale, values = ~voting_rate, opacity = 0.7,
              title = "Voting rate", position = "topright")
  
  install.packages('htmlwidgets')
  library(htmlwidgets)
  saveWidget(map_leaflet, "leaflet_map.html")