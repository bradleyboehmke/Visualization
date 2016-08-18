library(ggplot2)
library(dplyr)

?midwest

##################
# Bar Chart icon #
##################

icon <- midwest %>%
        filter(state == "OH") %>%
        select(county, percollege) %>%
        arrange(desc(percollege)) %>%
        slice(c(1, 15, 30, 45, 60)) %>%
        arrange(percollege) %>%
        mutate(county = factor(county, levels = .$county),
               percollege = percollege / 100)

ggplot(icon, aes(percollege, county)) +
        geom_segment(aes(x = 0, y = county, xend = percollege, yend = county), color = "grey50") +
        geom_point(size = 3) +
        scale_y_discrete(labels = rev(LETTERS[1:5])) +
        scale_x_continuous(limits = c(0, .35), labels = scales::percent, expand = c(0, 0)) +
        theme_minimal() +
        theme(plot.background = element_rect(fill = "#fdfdfd", color = "#fdfdfd", size = .2),
              axis.ticks = element_blank(),
              axis.title = element_blank(),
              panel.grid.major.x = element_line(color = "grey50", linetype = "dotted"),
              panel.grid.major.y = element_blank(),
              panel.grid.minor = element_blank(),
              legend.position = "none")




# final graphic
ohio <- midwest %>%
        filter(state == "OH") %>%
        select(county, percollege) %>%
        arrange(percollege) %>%
        mutate(Avg = mean(percollege, na.rm = TRUE),
               Above = ifelse(percollege - Avg > 0, TRUE, FALSE),
               county = factor(county, levels = .$county))

ggplot(ohio, aes(percollege/100, county, color = Above)) +
        geom_segment(aes(x = Avg/100, y = county, xend = percollege/100, yend = county), color = "grey50") +
        geom_point() +
        annotate("text", x = .25, y = "ALLEN", label = "Above Average", color = "#00BFC4", size = 3, hjust = -0.1, vjust = .75) +
        annotate("text", x = .25, y = "FULTON", label = "Below Average", color = "#F8766D", size = 3, hjust = -0.1, vjust = -.1) +
        geom_segment(aes(x = .25, xend = .25 , y = "ASHLAND", yend = "DEFIANCE"),
                     arrow = arrow(length = unit(0.2,"cm")), color = "#00BFC4") +
        geom_segment(aes(x = .25, xend = .25 , y = "KNOX", yend = "PUTNAM"),
                     arrow = arrow(length = unit(0.2,"cm")), color = "#F8766D") +
        scale_x_continuous(labels = scales::percent, expand = c(0, 0), limits = c(.07, .33)) +
        labs(title = "Percentage of College Educated Adults in Ohio Counties",
             subtitle = "The average percent of college educated adults in Ohio is 16.89%. Franklin, Greene, Geauga, and \nDelaware counties lead Ohio with over 30% of their adults being college educated while Vinton, \nAdams, Holmes, and Perry trailing with less than 10% of their adults being college educated.",
             caption = "U.S. Census Bureau: 2000 Census") +
        theme_minimal() +
        theme(axis.title = element_blank(),
              panel.grid.minor = element_blank(),
              legend.position = "none",
              text = element_text(family = "Georgia"),
              axis.text.y = element_text(size = 8),
              plot.title = element_text(size = 20, margin = margin(b = 10), hjust = 0),
              plot.subtitle = element_text(size = 12, color = "darkslategrey", margin = margin(b = 25)),
              plot.caption = element_text(size = 8, margin = margin(t = 10), color = "grey70", hjust = 0))
        
        