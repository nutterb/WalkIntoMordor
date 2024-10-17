library(png)
library(dplyr)
library(reshape2)
library(ggplot2)

# Read in Image Data ------------------------------------------------
middle_earth <- "MiddleEarth.png"

image_data <- readPNG(middle_earth)

ImageData <- 
  as.data.frame.table(image_data) %>% 
  mutate(Var1 = rev(as.numeric(Var1)), 
         Var2 = as.numeric(Var2), 
         Var3 = as.numeric(Var3), 
         RGB = factor(Var3, 
                      1:4, 
                      c("R", "G", "B", "T"))) %>% 
  rename(x = Var2, 
         y = Var1, 
         pct = Var3) 

# Plot the Map ------------------------------------------------------

FullPlotData <- 
  dcast(ImageData, 
        x + y ~ RGB, 
        value.var = "Freq") %>% 
  mutate(color_code = rgb(R, G, B)) 

Map <- 
  ggplot(data = FullPlotData, 
         mapping = aes(x = x, 
                       y = y)) + 
  geom_tile(fill = FullPlotData$color_code) + 
  theme_bw() + 
  scale_x_continuous(breaks = seq(0, 1200, by = 100)) + 
  scale_y_continuous(breaks = seq(0, 700, by = 100))

# Distance Function -------------------------------------------------

get_distance <- function(x0, x1, y0, y1){
  sqrt((x0 - x1)^2 + (y0 - y1)^2)
}

# Identify the Track ------------------------------------------------

cobble_path <- function(data, direction = "north"){
  
  if (direction %in% c("north", "south")){
    data <- 
      data %>% 
      group_by(y) %>% 
      summarize(x = mean(x))
    if (direction == "north"){
      data <- arrange(data, y)
    } else {
      data <- arrange(data, desc(y))
    }
  } else if (direction %in% c("east", "west")){
    data <- 
      data %>% 
      group_by(x) %>% 
      summarize(y = mean(y))
    
    if (direction == "east"){
      data <- arrange(data, x)
    } else {
      data <- arrange(data, desc(x))
    }
  }
  data %>% mutate(point_order = seq_len(nrow(.)))
}

Blue <- 
  FullPlotData %>% 
  filter(R < 0.1 & G < 0.1 & B > 0.9) %>% 
  mutate(point_order = NA_real_, 
         row_num = row_number())

Part1 <-
  Blue %>% 
  filter((x > 350 & x <= 700) & 
           (y > 570 & y < 650))

Part2 <-
  Blue %>% 
  filter((x > 700 & x <= 750) & 
           (y > 599 & y < 650))

Part3 <- 
  Blue %>% 
  filter((x > 719 & x <= 750) & 
           (y > 525 & y < 625))

Part4 <- 
  Blue %>% 
  filter((x >= 700 & x <= 720) & 
           (y >= 525 & y <= 553))

Part5 <- 
  Blue %>% 
  filter((x >= 690 & x <= 700) & 
           (y >= 513 & y <= 525))

Part6 <- 
  Blue %>% 
  filter((x >= 697 & x <= 700) & 
           (y >= 512 & y <= 515))

Part7 <- 
  Blue %>% 
  filter((x >= 700 & x <= 720) & 
           (y >= 511 & y <= 520))

Part8 <- 
  Blue %>% 
  filter((x >= 720 & x <= 750) & 
           (y >= 520 & y <= 525))

Part9 <- 
  Blue %>% 
  filter((x >= 687 & x <= 700) & 
           (y >= 490 & y <= 511))

Part10 <- 
  Blue %>% 
  filter((x >= 687 & x <= 690) & 
           (y >= 465 & y <= 490))

Part11 <- 
  Blue %>% 
  filter((x >= 690 & x <= 872) & 
           (y >= 352 & y <= 475))

Part12 <- 
  Blue %>% 
  filter((x >= 860 & x <= 872) & 
           (y >= 330 & y <= 352))

Part13 <- 
  Blue %>% 
  filter((x >= 865 & x <= 900) & 
           (y >= 323 & y <= 332))

Part14 <- 
  Blue %>% 
  filter((x >= 880 & x <= 890) & 
           (y >= 300 & y <= 323))

Part15 <- 
  Blue %>% 
  filter((x >= 880 & x <= 915) & 
           (y >= 280 & y <= 300))

Part16 <- 
  Blue %>% 
  filter((x >= 880 & x <= 915) & 
           (y >= 250 & y <= 280))

Part17 <- 
  Blue %>% 
  filter((x >= 880 & x <= 902) & 
           (y >= 200 & y <= 250))

Part18 <- 
  Blue %>% 
  filter((x >= 902 & x <= 952) & 
           (y >= 200 & y <= 240))

Part19 <- 
  Blue %>% 
  filter((x >= 952 & x <= 1010) & 
           (y >= 200 & y <= 240))

Part20 <- 
  Blue %>% 
  filter((x >= 952 & x <= 1010) & 
           (y >= 90 & y <= 200))

Part21 <- 
  Blue %>% 
  filter((x >= 1010 & x <= 1045) & 
           (y >= 90 & y <= 107))

Part22 <- 
  Blue %>% 
  filter((x >= 1045 & x <= 1051) & 
           (y >= 90 & y <= 98))

Part23 <- 
  Blue %>% 
  filter((x >= 1045 & x <= 1051) & 
           (y >= 90 & y <= 105))

Part24 <- 
  Blue %>% 
  filter((x >= 1030 & x <= 1051) & 
           (y >= 105 & y <= 150))

Part25 <- 
  Blue %>% 
  filter((x >= 1036 & x <= 1083) & 
           (y >= 150 & y <= 170))

Part26 <- 
  Blue %>% 
  filter((x >= 1083 & x <= 1090) & 
           (y >= 125 & y <= 170))

Path <- 
  bind_rows(
    Part1 %>% cobble_path("east"), 
    Part2 %>% cobble_path("east"),
    Part3 %>% cobble_path("south"),
    Part4 %>% cobble_path("south"),
    Part5 %>% cobble_path("south"),
    Part6 %>% cobble_path("east"),
    Part7 %>% cobble_path("east"),
    Part8 %>% cobble_path("east"), 
    Part8 %>% cobble_path("west"), 
    Part7 %>% cobble_path("west"), 
    Part9 %>% cobble_path("west"), 
    Part10 %>% cobble_path("south"), 
    Part11 %>% cobble_path("east"),
    Part12 %>% cobble_path("south"), 
    Part13 %>% cobble_path("east"),
    Part14 %>% cobble_path("south"),
    Part15 %>% cobble_path("south"),
    Part16 %>% cobble_path("south"),
    Part17 %>% cobble_path("south"), 
    Part18 %>% cobble_path("east"), 
    Part19 %>% cobble_path("east"),
    Part20 %>% cobble_path("south"),
    Part21 %>% cobble_path("east"),
    Part22 %>% cobble_path("east"),
    Part23 %>% cobble_path("north"),
    Part24 %>% cobble_path("north"),
    Part25 %>% cobble_path("east"),
    Part26 %>% cobble_path("south")
  ) %>% 
  mutate(path_order = seq_len(nrow(.)))


Milestone <- 
  data.frame(Location = c("Shire", "Bree", "Rivendell", 
                          "Lothlorien", "Rauros Falls", 
                          "Mt. Doom"), 
             x = c(392, 493, 727, 790, 897, 1088), 
             y = c(594, 594, 605, 422, 202, 132), 
             distance_to = c(0, 135, 458, 920, 1309, 1779)) %>% 
  mutate(distance_to_next = c(diff(distance_to), 0))

Path <- 
  Path %>% 
  left_join(Milestone, 
            by = c("x" = "x", 
                   "y" = "y")) %>% 
  mutate(Milestone = zoo::na.locf(Location), 
         distance_to_next = zoo::na.locf(distance_to_next))


DistanceList <- 
  list(Shire = filter(Path, Milestone == "Shire" | Location == "Bree"), 
       Bree = filter(Path, Milestone == "Bree" | Location == "Rivendell"), 
       Rivendell = filter(Path, Milestone == "Rivendell" | Location == "Lothlorien"), 
       Lothlorien = filter(Path, Milestone == "Lothlorien" | Location == "Rauros Falls"), 
       RaurosFalls = filter(Path, Milestone == "Rauros Falls" | Location == "Mt. Doom")) %>% 
  lapply(function(d){
    d %>% 
      mutate(distance_to = distance_to_next[1], 
             distance_between = get_distance(x0 = x, x1 = lag(x),
                                             y0 = y, y1 = lag(y)), 
             distance_between = ifelse(is.na(distance_between), 
                                       0, distance_between), 
             cumulative_distance = cumsum(distance_between), 
             distance = cumulative_distance / sum(distance_between) * distance_to, 
             distance_between = c(0, diff(distance)))
  })

DistanceList[-1] <- 
  lapply(DistanceList[-1], 
         function(d) tail(d, -1))

Path <- 
  bind_rows(DistanceList) %>% 
  mutate(cumulative_distance = cumsum(distance_between), 
         distance = cumulative_distance / sum(distance_between) * distance_to, 
         path_order = seq_len(nrow(.))) %>% 
  select(x, y, 
         path_order, 
         Location, 
         distance_between, 
         distance = cumulative_distance) 


Map + 
  geom_path(data= Path,
            mapping = aes(x = x,
                          y = y),
            color = "red", 
            linewidth = 1) 
#+ 
  # coord_cartesian(xlim = c(750, 850),
  #                 ylim = c(400, 500)) +
  # geom_vline(xintercept = c(790),
  #            color = "green",
  #            linewidth = 1.5) +
  # geom_hline(yintercept = c(422),
  #            color = "green",
  #            linewidth = 1)


write.csv(Path, 
          "PathToMordor.csv", 
          row.names = FALSE)

write.csv(Milestone, 
          "Locations.csv", 
          row.names = FALSE)

write.csv(FullPlotData, 
          "MapOfMiddleEarth.csv", 
          row.names = FALSE)
