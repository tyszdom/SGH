########## ASSUMPTIONS ##########

### PREPARE RAW DATA

path <- "D:/SGH/Studia Magisterskie/2019zima/Prezentacja i wizualizacja danych/Projekt/FIFA19.csv"

library(scales)
library(ggthemes)
library(plotly)
library(ggplot2)
library(stringr)
library(dplyr)
library(gridExtra)
library(maps)

fifa_data <- read.csv2(path, header = TRUE, sep = ",")

fifa_data <- fifa_data[,-c(1)]

options(scipen = 999)

### PREPARE VALUE, WAGES AND AGE

fifa_data <- fifa_data %>%
  mutate(ValueMultiplier = ifelse(str_detect(Value, "K"), 1000, ifelse(str_detect(Value, "M"), 1000000, 1))) %>%
  mutate(ValueNumeric_pounds = as.numeric(str_extract(Value, "[[:digit:]]+\\.*[[:digit:]]*")) * ValueMultiplier)

fifa_data <- fifa_data %>%
  mutate(WageMultiplier = ifelse(str_detect(Wage, "K"), 1000, ifelse(str_detect(Wage, "M"), 1000000, 1))) %>%
  mutate(WageNumeric_pounds = as.numeric(str_extract(Wage, "[[:digit:]]+\\.*[[:digit:]]*")) * WageMultiplier)

fifa_data <- fifa_data %>%
  mutate(AgeGroup = ifelse(Age <= 20, "20 and under", 
                    ifelse(Age > 20 & Age <=25, "21 to 25", 
                    ifelse(Age > 25 & Age <= 30, "26 to 30", 
                    ifelse(Age > 30 & Age <= 35, "31 to 35", "Over 35")))))


### PREPARE POSITIONS

positions <- as.character(unique(fifa_data$Position))
positions <- positions[c(1:27)]
positions <- sort(positions, na.last = TRUE)      
pos <- as.data.frame(positions)

pos$pos <- c("MD", "DF", "MD", "FW", "MD", "GK",
             "MD", "DF", "DF", "MD", "MD", "FW",
             "MD", "FW", "FW", "DF", "MD", "DF",
             "DF", "MD", "MD", "FW", "MD", "FW", "FW", "DF", "FW")

GK <- "GK"
DF <- as.character(pos$positions[which(pos$pos=="DF")])
MD <- as.character(pos$positions[which(pos$pos=="MD")])
FW <- as.character(pos$positions[which(pos$pos=="FW")])

fifa_data <- fifa_data %>% 
  mutate(PositionGroup = ifelse(Position %in% GK, "GK", 
                         ifelse(Position %in% DF, "DF", 
                         ifelse(Position %in% MD, "MD", 
                         ifelse(Position %in% FW, "FW", "Unknown")))))

fifa_data <- fifa_data %>% 
  mutate(PositionNo = ifelse(Position %in% GK, 1,
                      ifelse(Position %in% DF, 2,
                      ifelse(Position %in% MD, 3,
                      ifelse(Position %in% FW, 4, 0)))))

############# USED PLOTS #################

### OVERALL DISTIBUTION 

fifa_data %>%
  ggplot(aes(x= Overall), ylab = "") +
  geom_histogram(color = "white", fill = "black") +
  ggtitle("ROZKLAD UMIEJETNOSCI PILKARZY", subtitle = "Ratingi pilkarzy maja rozklad normalny.") +
  theme_fivethirtyeight() +
  theme(axis.text.y = element_blank())

### AGE VS OVERALL TOTAL

fifa_data %>%
  filter(!PositionGroup %in% c("GK", "Unknown")) %>%
  group_by(Age) %>%
  summarise(Rating = mean(Overall)) %>%
  ggplot(aes(x= Age, y= Rating, group = 1)) +
  geom_line(color = "black", size = 1.2) +
  ggtitle("SREDNIE RATINGI PILKARZY W ZALEZNOSCI OD WIEKU", subtitle = "Pilkarze osiagaja swoj szczyt formy okolo 30. roku zycia.") +
  xlab("Age") +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(), axis.title.y = element_blank(), axis.title.x = element_text(face = "bold"))

### AGE VS OVERALL BY POSITION

fifa_data %>%
  filter(!PositionGroup %in% c("Unknown")) %>%
  group_by(PositionGroup, Age) %>%
  summarise(Rating = mean(Overall)) %>%
  ggplot(aes(x= Age, y= Rating, group = PositionGroup)) +
  geom_line(size = 1, color = "grey50") +
  theme_fivethirtyeight() +
  ggtitle("SREDNIE RATINGI PILKARZY W ZALEZNOSCI OD POZYCJI") +
  facet_wrap(~ PositionGroup, ncol = 1) +
  theme(strip.background = element_rect(fill = "darkgrey"), strip.text = element_text(colour = "white", face = "bold"))

### PLAYER VALUATIONS

p <- fifa_data %>%
  ggplot(aes(x= ValueNumeric_pounds)) +
  geom_histogram(color = "white", fill = "black") +
  scale_x_continuous(labels = dollar_format(prefix = "€")) +
  ggtitle("BADANIE SKOSNOSCI WARTOSCI PILKARZY", subtitle = "Ogon jest prezentowany na bazie pilkarzy FC Barcelona.") +
  theme_fivethirtyeight()

p +
  geom_text(data = subset(fifa_data, Name == "M. ter Stegen"), aes(x= ValueNumeric_pounds, y= 500, label=Name), color = "black") +
  geom_text(data = subset(fifa_data, Name == "L. Messi"), aes(x= ValueNumeric_pounds, y= 1500, label=Name), color = "black") +
  geom_text(data = subset(fifa_data, Name == "Coutinho"), aes(x= ValueNumeric_pounds, y= 2500, label=Name), color = "black") +
  geom_text(data = subset(fifa_data, Name == "S. Umtiti"), aes(x= ValueNumeric_pounds, y= 3500, label=Name), color = "black")

### AGE VS VALUATIONS

fifa_data %>%
  ggplot(aes(x= AgeGroup, y= ValueNumeric_pounds)) +
  geom_boxplot(fill = "darkgrey") +
  scale_y_log10(labels = dollar_format(prefix = "€")) +
  ggtitle("WARTOSC PILKARZY W ZALEZNOSCI OD WIEKU", subtitle = "Zastosowano skale logarytmiczna, co wskazuje na duze roznice miedzy grupami") +
  theme_fivethirtyeight()

### POSITION VS VALUATIONS

a <- fifa_data %>%
  filter(PositionGroup != "Unknown") %>%
  ggplot(aes(x= PositionGroup, y= ValueNumeric_pounds)) +
  geom_boxplot(fill = "darkgrey") +
  scale_y_log10(labels = dollar_format(prefix = "€")) +
  ggtitle("WARTOSC PILKARZY W PODZIALE NA POZYCJE") +
  theme_fivethirtyeight()


b <- fifa_data %>%
  filter(PositionGroup != "Unknown") %>%
  ggplot(aes(x= Position, y= ValueNumeric_pounds)) +
  geom_boxplot(fill = "darkgrey") +
  scale_y_log10(labels = dollar_format(prefix = "€")) +
  coord_flip() +
  theme_fivethirtyeight() +
  facet_wrap(~ PositionGroup, scales = "free") +
  theme(strip.background = element_rect(fill = "darkgrey"), strip.text = element_text(colour = "white", face = "bold"))

grid.arrange(a, b)

### PLAYER RANKING AND VALUATIONS

fifa_data %>%
  filter(PositionGroup != "Unknown") %>%
  ggplot(aes(x= Overall, y= ValueNumeric_pounds)) +
  geom_point(position = "jitter", color = "darkgrey") +
  ggtitle("WARTOSC PILKARZY W ZALEZNOSCI OD RATINGU") +
  scale_y_continuous(labels = dollar_format(prefix = "€")) +
  theme_fivethirtyeight() +
  geom_smooth(method = "lm", color = "black") +
  annotate("text", x= 60, y= 100000000, label = paste0("Wspolczynnik korelacji rang Spearmana = ", round(cor(fifa_data$Overall, fifa_data$ValueNumeric_pounds, method = "spearman"),3)), 
           color = "darkblue", size = 5)

### POTENTIAL AND OVERALL TALENT

fifa_data %>%
  group_by(Age) %>%
  summarise(Potential = mean(Potential),
            Overall = mean(Overall)) %>%
  ggplot(aes(x= Age)) +
  geom_line(aes(y= Potential), color = "blue", size = 1) +
  geom_line(aes(y= Overall), color = "black", size = 1.2) +
  annotate("text", x= 30, y=55, size = 5, label = "Pilkarze sie rozwijaja do 30. roku zycia.", color = "blue") +
  ggtitle("POTENCJALNY I RZECZYWISTY RATING") +
  theme_fivethirtyeight()


################# MAPS #####################

fifa_data$Nationality <- gsub('United States', 'USA', fifa_data$Nationality) 
fifa_data$Nationality <- gsub('China PR', 'China', fifa_data$Nationality)
fifa_data$Nationality <- gsub('DR Congo', 'Democratic Republic of the Congo', fifa_data$Nationality)
fifa_data$Nationality <- gsub('Scotland', 'UK', fifa_data$Nationality)
fifa_data$Nationality <- gsub('England', 'UK', fifa_data$Nationality)
fifa_data$Nationality <- gsub('Wales', 'UK', fifa_data$Nationality)
fifa_data$Nationality <- gsub('Northern Ireland', 'UK', fifa_data$Nationality)

### TOTAL NUMBER OF PLAYERS ON MAP

overall_data <- fifa_data %>% 
  group_by(Nationality) %>% 
  summarise(Count = n(), 
            Avg_Overall = mean(Overall),
            Avg_Potential = mean(Potential),
            Avg_Pot_Diff = mean(Potential-Overall))
worldmap = map_data("world")
merged_data <- merge(x = worldmap, y = overall_data, by.x = "region", by.y = "Nationality", all.x = TRUE) %>% arrange(order)
ggplot(data = merged_data, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = Count)) +
  labs(fill="LICZBA PILKARZY \nW DANYM KRAJU") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())


mean(fifa_data$Age)
median(fifa_data$Age)

mean(fifa_data$Overall)
median(fifa_data$Overall)


fifa_data %>% 
  filter(fifa_data$PositionGroup == "DF") %>% summarise(aaa = mean(fifa_data$Overall, na.rm = TRUE))

### OVERALL AND POTENTIAL RATING

overall_data <- fifa_data %>% 
  group_by(Nationality) %>% 
  summarise(Count = n(), 
            Avg_Overall = mean(Overall),
            Avg_Potential = mean(Potential),
            Avg_Pot_Diff = mean(Potential-Overall)) %>%
  filter(Count > 30)

overall_plot <- ggplot(data = merged_data, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = Avg_Overall)) +
  labs(fill='SREDNI RATING PILKARZY \nW DANYM KRAJU') +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

potential_plot <- ggplot(data = merged_data, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = Avg_Potential)) +
  labs(fill='SREDNI POTENCJAL \nPILKARZY W DANYM KRAJU') +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

grid.arrange(overall_plot, potential_plot, nrow=2)

################# USED BY SHINY #################


### TEAM AGE / YOUNGEST AND OLDEST

age_avg <- mean(fifa_data$Age)
age_sd <- sd(fifa_data$Age)

team_age <- fifa_data %>%
  group_by(Club) %>%
  summarise(AvgAge = mean(Age)) %>%
  mutate(AgeZ_score = (AvgAge - age_avg) / age_sd)

team_age <- team_age %>%
  mutate(AgeType = ifelse(AgeZ_score <0, "Below", "Above"))


team_age <- team_age %>%
  arrange(desc(AgeZ_score)) %>%
  head(10) %>%
  rbind(team_age %>% arrange(desc(AgeZ_score)) %>% tail(10))


team_age %>%
  ggplot(aes(x= reorder(Club,AgeZ_score), y= AgeZ_score)) +
  geom_bar(stat = 'identity', aes(fill = AgeType), colour = "white") +
  geom_text(aes(label = round(AvgAge,1))) +
  scale_fill_manual(values = c("purple", "green")) +
  coord_flip() +
  ggtitle("Nordic Clubs Are Younger Than South American Clubs", subtitle = "Ranking the 10 oldest playing lists vs the 10 youngest playing lists") +
  theme_fivethirtyeight() +
  theme(legend.position = "none", axis.text.x = element_blank())


### TEAM OVERALL TALENT

top_15_overall_clubs <- fifa_data %>%
  group_by(Club) %>%
  summarise(AverageRating = mean(Overall, na.rm = T)) %>%
  arrange(desc(AverageRating)) %>%
  head(n = 15) %>% pull(Club) 
 
fifa_data %>%
  filter(Club %in% top_15_overall_clubs) %>%
  mutate(Top3 = ifelse(Club %in% c("Juventus", "Napoli", "Inter"), "Yes", "No")) %>%
  ggplot(aes(x= reorder(Club,Overall), y= Overall, fill = Top3)) +
  geom_boxplot(color = "black") +
  scale_fill_manual(values = c("lightgrey", "purple")) +
  ggtitle("Italian Teams Have The Highest Overall Ratings", subtitle = "The average overall rating of the 15 highest rated teams in the game, sorted in descending order") +
  coord_flip() +
  theme_fivethirtyeight() +
  theme(legend.position = "none")
