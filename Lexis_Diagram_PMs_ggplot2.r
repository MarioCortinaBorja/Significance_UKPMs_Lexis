#
# Packages
#
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)
#
# The final date used in the file is
#
final_date <- ymd("2017/12/16")
#
# Read in the data
#
# It's necessary to ensure that the data file Prime_Ministers_data.txt is in the working directory
#
pm <- read_csv("Prime_Ministers_data.txt") 
#
names(pm)
#
######################################################################################
#
# ***** Data manipulation *****
#
# Turn the date columns into dates
# Turn Party into a factor
# Record the PMs' current status (Alive or Dead) as a factor
#
pm_2 <- pm %>% mutate(Date_Birth_2 = ymd(Date_Birth),
                      Date_Start_PM_2 = ymd(Date_Start_PM),
                      Date_End_PM_2 = ymd(Date_End_PM),
                      Date_Death_2 = ymd(Date_Death),
                      Party_f = factor(Party),
                      Status = if_else(Date_Death_2 == final_date, "Alive", "Dead"),
                      Status_f = factor(Status))
#
# Compute the other variables
#
pm_3 <- pm_2 %>% mutate(Age_Start_PM_years = (Date_Start_PM_2 - Date_Birth_2) / 365.2425, 
                          # Units are years, even though days are stated
                        Age_End_PM_years = (Date_End_PM_2 - Date_Birth_2) / 365.2425,
                        PM_Length_years = (Date_End_PM_2 - Date_Start_PM_2) / 365.2425,
                        Age_Death_years = (Date_Death_2 - Date_Birth_2) / 365.2425)

#
# We will also need to work with PMs that are Dead, i.e. exclude living PMs
#
pm_Dead <- pm_3 %>% filter(Status_f == "Dead")
#
######################################################################################
#
# ***** ggplot2 code for the Lexis diagram *****
#
ggplot(pm_3, aes(x = Date_Start_PM_2, 
                 y = Age_Start_PM_years)) +
  #
  # For premiership
  #
  geom_segment(aes(x = Date_Start_PM_2, 
                   y = Age_Start_PM_years, 
                   xend = Date_End_PM_2, 
                   yend = Age_End_PM_years, 
                   colour = Party_f), 
               size = 1.05) +
  #
  # For rest of life
  #     
  geom_segment(aes(x = Date_End_PM_2, 
                   y = Age_End_PM_years, 
                   xend = Date_Death_2, 
                   yend = Age_Death_years, 
                   colour = Party_f), 
               lty = "longdash", 
               size = 1.05) + 
  #
  # Point at end of premiership
  # 
  geom_point(aes(x = Date_End_PM_2, 
                 y = Age_End_PM_years,
                 colour = Party_f),
             pch = 8, 
             size = 3,
             show.legend = FALSE) + 
  #
  # Point at end of life
  #
  geom_point(aes(x = Date_Death_2, 
                 y = Age_Death_years,
                 shape = Status,
                 colour = Party_f),
             size = 3) + 
  #
  # Add name
  #
  geom_text(aes(x = Date_Start_PM_2, 
                y = Age_Start_PM_years, 
                label = Name,
                colour = Party_f), 
            size = 3.5, 
            angle = 90,
            hjust = 1.1,
            vjust = 0.5,
            show.legend = FALSE) + 
  #
  # Smoother for age start of Premiership
  #
  geom_smooth(aes(x = Date_Start_PM_2, 
                  y = Age_Start_PM_years),
              se = FALSE,
              colour = "black", lty = "longdash") +
  #
  # Smoother for age at death
  # We must be careful not to include living PMs
  #
  geom_smooth(aes(x = Date_Death_2, 
                  y = Age_Death_years),
              data = pm_Dead, # Only PMs who have died
              se = FALSE, 
              colour = "black") +
  #
  # Date scale
  #
  scale_x_date(breaks = seq(ymd("1700-01-01"), ymd("2025-01-01"), by = "25 years"), 
               minor_breaks = NULL,
               limits = c(ymd("1700-01-01"), ymd("2025-01-01")), 
               labels = date_format("%Y")) +
  #
  # Age scale
  #
  scale_y_continuous(breaks = c(20, 30, 40, 50, 60, 70, 80, 90, 100), 
                     minor_breaks = c(25, 35, 45, 55, 65, 75, 85, 95),
                     limits = c(12.5, 100)) +
  #
  # Colour scale
  #
  scale_colour_manual(name = "Political Party:", 
                      values = c("CON" = "blue", 
                                 "LAB" = "red", 
                                 "LIB" = "orange", 
                                 "TORY" = "dark green", 
                                 "WHIG" = "purple")) +
  #
  # Shape scale
  #
  scale_shape_manual(name = paste0("Living on ", final_date, "?:"),
                     values = c("Alive" = 16, "Dead" = 17)) +
  #
  # Title and labels
  #
  labs(title = "Lexis Diagram for all British Prime Ministers",
       subtitle = "Periods of premiership shown in an unbroken way",
       caption = "Smoothers through age at start of premiership and age at death",
       x = "Date", 
       y = "Age during Premiership (years)" ) +
  #
  # Change fonts
  #
  theme(plot.title = element_text(size = 18),
        plot.subtitle = element_text(size = 16),
        plot.caption = element_text(size = 14),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        legend.position = 'bottom')

#
# Save the graph
#
height <- 35
width <- 30
#
ggsave("Prime_Ministers.pdf", width = width, height = height, units = "cm")

