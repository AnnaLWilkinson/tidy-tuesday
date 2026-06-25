
# About this script -------------------------------------------------------

#  Purpose: Tidy Tuesday 
#  Project: Papal Encyclicals: Industrial Revolution vs. AI Revolution
#  Author: Anna Wilkinson; anna.wilkinson@burnet.edu.au
#  Data started: 25 June 2026


# Load libraries ----------------------------------------------------------

library(tidyverse)


# Load data ---------------------------------------------------------------


# Using R
# Option 1: tidytuesdayR R package 
## install.packages("tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load('2026-06-23')
## OR
#tuesdata <- tidytuesdayR::tt_load(2026, week = 25)

encyclicals <- tuesdata$encyclicals
papal_encyclicals <- tuesdata$papal_encyclicals
scripture_references <- tuesdata$scripture_references

# Option 2: Read directly from GitHub

# encyclicals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-06-23/encyclicals.csv')
# papal_encyclicals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-06-23/papal_encyclicals.csv')
# scripture_references <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-06-23/scripture_references.csv')
# 


# Exploratory analysis ----------------------------------------------------

scripture_references %>% 
  ggplot() + 
  geom_bar(aes(x = book),
           stat = "count") + 
  coord_flip()


scripture_references %>% 
  ggplot() + 
  geom_bar(aes(x = book,
               fill = testament),
           stat = "count") + 
  coord_flip() + 
  facet_wrap(~ encyclical)




# Books of the bible ------------------------------------------------------

bible_books_catholic <- c(
  # Pentateuch
  "Genesis", "Exodus", "Leviticus", "Numbers", "Deuteronomy",
  
  # Historical Books
  "Joshua", "Judges", "Ruth",
  "1 Samuel", "2 Samuel",
  "1 Kings", "2 Kings",
  "1 Chronicles", "2 Chronicles",
  "Ezra", "Nehemiah",
  "Tobit", "Judith", "Esther",
  "1 Maccabees", "2 Maccabees",
  
  # Wisdom Literature
  "Job", "Psalms", "Proverbs", "Ecclesiastes",
  "Song of Songs", "Wisdom", "Sirach",
  
  # Major Prophets
  "Isaiah", "Jeremiah", "Lamentations",
  "Baruch", "Ezekiel", "Daniel",
  
  # Minor Prophets
  "Hosea", "Joel", "Amos", "Obadiah", "Jonah",
  "Micah", "Nahum", "Habakkuk", "Zephaniah",
  "Haggai", "Zechariah", "Malachi",
  
  # Gospels
  "Matthew", "Mark", "Luke", "John",
  
  # Acts
  "Acts",
  
  # Pauline Epistles
  "Romans",
  "1 Corinthians", "2 Corinthians",
  "Galatians", "Ephesians", "Philippians",
  "Colossians",
  "1 Thessalonians", "2 Thessalonians",
  "1 Timothy", "2 Timothy",
  "Titus", "Philemon",
  
  # Catholic Epistles
  "Hebrews", "James",
  "1 Peter", "2 Peter",
  "1 John", "2 John", "3 John",
  "Jude",
  
  # Apocalypse
  "Revelation"
)

length(bible_books_catholic)



# Pretty plot -------------------------------------------------------------

# 135 years between papal encyclicals
# Pope Leo XIII's Rerum Novarum (1891), which addressed the Industrial Revolution's 
# impact on workers, and Pope Leo XIV's Magnifica Humanitas (2026), which addresses 
# artificial intelligence's impact on human dignity. Both were signed on May 15 of 
# their respective years.


scripture_references <- scripture_references %>% 
  mutate(book_fct = factor(book, bible_books_catholic))
levels(scripture_references$book_fct)

scripture_references <- scripture_references %>% 
  mutate(encyclical = factor(encyclical, levels = c("Rerum Novarum",
                                                    "Magnifica Humanitas")))
levels(scripture_references$encyclical)


scripture_references %>% 
  ggplot(aes(x = book_fct)) + 
  geom_bar(stat = "count") + 
  geom_text(aes(label = after_stat(count)), 
            stat = "count", 
            hjust = 2,
            colour = "white") + 
  
  scale_y_continuous(limits = c(0, 6), 
                     breaks = seq(0,6,1)) + 
  scale_x_discrete(limits = rev) + 
  
  labs(x = "Standardised English name of the biblical book", 
       y = "Frequency (n)", 
       caption = "papal encyclicals are the most authoritative \nform of papal teaching in 
       the Catholic Church",
       title = "Which books did Pope Leo XIII's Rerum Novarum (1891) cite \nvs Pope Leo XIV's Magnifica Humanitas (2026)") + 
  
  coord_flip() + 
  facet_wrap(~ encyclical) + 
  theme_minimal() + 
  theme(
    
    panel.grid.major.x = element_blank(), 
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(linetype = 2, 
                                      size = 1),
    plot.margin = margin(t=30,r=20,b=20,l=30, unit = "pt"),
    axis.title.y = element_text(vjust = 4)
  )



# Save plot ---------------------------------------------------------------

ggsave(filename = here::here("2026", "2026-06-24", "20260623.png"), 
       plot = last_plot(),
       dpi = 300)


## END



