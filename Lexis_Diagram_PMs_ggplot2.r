#
# Packages
#
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(stringr)
library(scales)
#
# Read in the data
#
pm<- read_csv('Prime_Ministers_data.txt')
#
# Manipulate the dates
#
# I used if_else from dplyr because otherwise dates were translated into numeric
#
# Some of the dates of death are wrong
#
# Robert Cecil 22/08/1903
# Archibald Primrose 21/05/1929
# Henry Campbell-Bannerman 22/04/1908
# Herbert Asquith 15/02/1928
# Bonar Law 30/10/1923
#
pm_2 <- pm %>% mutate(DoB_2 = if_else(str_detect(DoB, "-"), # DoB etc in two different forms
                                     ymd(DoB), 
                                     dmy(DoB)),
                      DoStart_2 = if_else(str_detect(DoStart, "-"), 
                                      ymd(DoStart), 
                                      dmy(DoStart)),
                      DoEnd_2 = if_else(str_detect(DoEnd, "-"), 
                                          ymd(DoEnd), 
                                          dmy(DoEnd)),
                      DoD_2 = if_else(str_detect(DoD, "-"), 
                                        ymd(DoD), 
                                        dmy(DoD)),
                      DoD_Revised = if_else(name == "Robert Cecil", dmy("22/08/1903"), # Correct wrong dates
                                            if_else(name == "Archibald Primrose", dmy("21/05/1929"),
                                                    if_else(name == "Henry Campbell-Bannerman", dmy("22/04/1908"),
                                                            if_else(name == "Herbert Asquith", dmy("15/02/1928"),
                                                                    if_else(name == "Bonar Law", dmy("30/10/1923"), 
                                                                            if_else(is.na(DoD_2), Sys.Date(),DoD_2))))))) # Replace NAs with today
                    
#
# Compute the other variables
#

pm_3 <- pm_2 %>% mutate(AgeStart_years = (DoStart_2 - DoB_2) / 365.2425, # Units are years, even though days are stated
                        AgeEnd_years = (DoEnd_2 - DoB_2) / 365.2425,
                        PM_Length_years = (DoEnd_2 - DoStart_2) / 365.2425,
                        AgeDeath_years = (DoD_Revised - DoB_2) / 365.2425, 
                        Status= if_else(DoD_Revised == Sys.Date(), "Alive", "Dead"))


#
# ggplot2 Lexis diagram
#
ggplot(pm_3, aes(x = DoStart_2, 
                 y = AgeStart_years)) +
  #
  # For premiership
  #
  geom_segment(aes(x = DoStart_2, 
                   y = AgeStart_years, 
                   xend = DoEnd_2, 
                   yend = AgeEnd_years, 
                   colour = party), size = 1.05) +
  #
  # For rest of life
  #     
  geom_segment(aes(x = DoEnd_2, 
                   y = AgeEnd_years, 
                   xend = DoD_Revised, 
                   yend = AgeDeath_years, 
                   colour = party), 
               lty = "longdash", size = 1.05) + 
  #
  # Point at end of premiership
  # 
  geom_point(aes(x = DoEnd_2, 
                 y = AgeEnd_years,
                 colour = party),
             pch = 8, 
             size = 3,
             show.legend = FALSE) + 
  #
  # Point at end of life
  #
  geom_point(aes(x = DoD_Revised, 
                 y = AgeDeath_years,
                 shape = Status,
                 colour = party),
             size = 3) + 
  #
  # Add name
  #
  geom_text(aes(x = DoStart_2, 
                y = AgeStart_years, 
                label = name,
                colour = party), 
            size = 3.5, 
            angle = 90,
            hjust = 1.1,
            vjust = 0.5,
            show.legend = FALSE) + 
  #
  # Smoother for start
  #
  geom_smooth(aes(x = DoStart_2, 
                  y = AgeStart_years),
              se = FALSE,
              colour = "black", lty = "longdash") +
  #
  # Smoother for death
  # We must be careful not to include living PMs
  #
  geom_smooth(aes(x = DoD_for_Dead, 
                  y = AgeDeath_for_Dead),
              data = with(pm_3, data.frame(DoD_for_Dead = DoD_Revised[Status == "Dead"],
                                AgeDeath_for_Dead = AgeDeath_years[Status == "Dead"])),
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
  scale_shape_manual(name = paste0("Living on ", format(Sys.Date(), "%d/%m/%Y"), "?:"),
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
# Save it as a pdf
#
ggsave("Prime_Ministers.pdf", width = 35, height = 30, units = "cm")

#
# Look at some of the data
#

data_used <- pm_3 %>% 
       select(name, 
              DoB_2,
              DoStart_2, AgeStart_years, 
              DoEnd_2, AgeEnd_years, 
              DoD_Revised, AgeDeath_years, party)

data_used_2 <- data_used %>% dplyr::rename(Name = name,
                                           Date_Birth = DoB_2,
                                           Date_Start_PM = DoStart_2,
                                           Age_Start_PM = AgeStart_years,
                                           Date_End_PM = DoEnd_2,
                                           Age_End_PM = AgeEnd_years,
                                           Date_Death = DoD_Revised,
                                           Age_Death = AgeDeath_years,
                                           Party = party)

#
# Write out
#
write_csv(data_used_2, "Prime_Ministers_data.csv")

write_delim(data_used_2, "Prime_Ministers_data.txt", delim = ",")
