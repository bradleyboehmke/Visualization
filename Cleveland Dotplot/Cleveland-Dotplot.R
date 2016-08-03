library(readxl)
library(dplyr)
library(ggplot2)


supermarket <- read_excel("~/Dropbox/Academia/University of Cincinnati/Intro to R Bootcamp/data/Supermarket Transactions.xlsx", sheet = "Data")


##################
# Bar Chart icon #
##################
# basic bar chart for icon

city_order <- supermarket %>%
        group_by(City) %>%
        summarize(Revenue = sum(Revenue, na.rm = TRUE)) %>%
        arrange(desc(Revenue)) %>%
        top_n(15) %>%
        arrange(Revenue) %>%
        .$City

icon_df <- supermarket %>%
        filter(City %in% city_order) %>%
        group_by(Gender, City) %>%
        summarize(Revenue = sum(Revenue, na.rm = TRUE)) %>%
        mutate(City = factor(City, levels = city_order))

# basic bar chart for explanation
ggplot(icon_df, aes(Revenue, City)) +
        geom_line(aes(group = City), color = "#fdfdfd", size = 1) +
        geom_line(aes(group = City), color = "grey50", size = .2) +
        geom_point(aes(color = Gender)) +
        scale_colour_grey() +
        scale_y_discrete(labels = rev(LETTERS[1:15])) +
        scale_x_continuous(limits = c(2000, 10000), breaks = seq(2000, 10000, 2000), labels = scales::comma) +
        theme_minimal() +
        theme(plot.background = element_rect(fill = "#fdfdfd", color = "#fdfdfd", size = .2),
              axis.ticks = element_blank(),
              axis.title = element_blank(),
              panel.grid.major.y = element_line(color = "grey50", linetype = "dotted"),
              panel.grid.major.x = element_blank(),
              panel.grid.minor = element_blank(),
              legend.position = "none")



