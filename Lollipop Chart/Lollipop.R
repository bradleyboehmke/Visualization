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
        
        