library(xlsx)
library(dplyr)
library(tidyr)
library(ggplot2)
library(magrittr)
library(gridExtra)


##################
# Bar Chart icon #
##################
# basic bar chart for icon
random <- data.frame(x = 1:5, y = 5:1)
ggplot(random, aes(x, y)) +
        geom_bar(stat = "identity", color = "#EEEEEE") +
        theme_minimal() +
        theme(plot.background = element_rect(fill = "#EEEEEE", color = "#EEEEEE"),
              axis.text = element_blank(),
              axis.ticks = element_blank(),
              axis.title = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())

# basic bar chart for explanation
ggplot(random, aes(x, y)) +
        geom_bar(stat = "identity", color = "#fdfdfd") +
        theme_minimal() +
        theme(plot.background = element_rect(fill = "#fdfdfd", color = "#fdfdfd"),
              axis.text = element_blank(),
              axis.ticks = element_blank(),
              axis.title = element_blank(),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())


# preset background color to my blogs background color
theme_update(plot.background = element_rect(fill = "#fdfdfd"))



###################
# Basic Bar Chart #
###################

# plot the count of categories in a variable
p1 <- ggplot(mtcars, aes(cyl)) +
        geom_bar() +
        ggtitle("x-axis as a continuous variable")

p2 <- ggplot(mtcars, aes(factor(cyl))) +
        geom_bar() +
        ggtitle("x-axis as a categorical (factor) variable")

grid.arrange(p1, p2, ncol = 2)

# can adjust width of bars
p1 <- ggplot(mtcars, aes(factor(cyl))) +
        geom_bar(width = .5) +
        ggtitle("bar width = 0.5")
        

p2 <- ggplot(mtcars, aes(factor(cyl))) +
        geom_bar(width = .75) +
        ggtitle("bar width = 0.75")

p3 <- ggplot(mtcars, aes(factor(cyl))) +
        geom_bar(width = .9) +
        ggtitle("bar width = 0.9")

p4 <- ggplot(mtcars, aes(factor(cyl))) +
        geom_bar(width = .99) +
        ggtitle("bar width = 0.99")

grid.arrange(p1, p2, p3, p4, ncol = 4)

# adjust the fill and outline color
ggplot(mtcars, aes(factor(cyl))) +
        geom_bar(fill = "dodgerblue", color = "grey40", alpha = .5)

# plot the mpg for each car
ggplot(mtcars, aes(row.names(mtcars), mpg)) +
        geom_bar(stat = "identity")

# rotate to make more readable
ggplot(mtcars, aes(row.names(mtcars), mpg)) +
        geom_bar(stat = "identity") +
        coord_flip()

# order bars
ggplot(mtcars, aes(reorder(row.names(mtcars), mpg), mpg)) +
        geom_bar(stat = "identity") +
        coord_flip()


####################
# Comparing Groups #
####################

# use color to compare groups
# compare mpg across all cars and color based on cyl
ggplot(mtcars, aes(x = reorder(row.names(mtcars), mpg), y = mpg, fill = factor(cyl))) +
        scale_fill_manual(values = c("#e5f5e0", "#a1d99b", "#31a354")) +
        geom_bar(stat = "identity") +
        coord_flip() +
        theme_minimal()


# use side-by-side bars to compare groups
# average mpg based on cyl and am
library(dplyr)
avg_mpg <- mtcars %>%
        group_by(cyl, am) %>%
        summarise(mpg = mean(mpg, na.rm = TRUE))

p1 <- ggplot(avg_mpg, aes(factor(cyl), mpg, fill = factor(am))) +
        geom_bar(stat = "identity", position = "dodge") +
        ggtitle("Default color comparison")

# more pleasing colors
p2 <- ggplot(avg_mpg, aes(factor(cyl), mpg, fill = factor(am))) +
        geom_bar(stat = "identity", position = "dodge", color = "grey40") +
        scale_fill_brewer(palette = "Pastel1") +
        ggtitle("Adjusted color comparison")

grid.arrange(p1, p2, ncol = 2)

# adjust the dodge width
p1 <- ggplot(avg_mpg, aes(factor(cyl), mpg, fill = factor(am))) +
        geom_bar(stat = "identity", position = "dodge") +
        ggtitle("Default dodge positioning") +
        theme(legend.position = "none")

p2 <- ggplot(avg_mpg, aes(factor(cyl), mpg, fill = factor(am))) +
        geom_bar(stat = "identity", position = position_dodge(width = .5)) +
        ggtitle("Greater overlap of comparison bars") +
        theme(legend.position = "none")

p3 <- ggplot(avg_mpg, aes(factor(cyl), mpg, fill = factor(am))) +
        geom_bar(stat = "identity", position = position_dodge(width = 1)) +
        ggtitle("Greater spacing between comparison bars") +
        labs(fill = "AM") +
        theme(legend.position = c(1,1), legend.justification = c(1,1),
              legend.background = element_blank())

grid.arrange(p1, p2, p3, ncol = 3)


# use stacked bars to compare groups
# when you fill by a categorical variable the default will create a stacked bar chart
ggplot(avg_mpg, aes(factor(cyl), mpg, fill = factor(am))) +
        geom_bar(stat = "identity")

# unfortunately the legend is opposite of stacked bars
ggplot(avg_mpg, aes(factor(cyl), mpg, fill = factor(am))) +
        geom_bar(stat = "identity") +
        guides(fill = guide_legend(reverse = TRUE))

# or reverse stacking order by changing the factor levels
ggplot(avg_mpg, aes(factor(cyl), mpg, fill = factor(am, levels = c(1, 0)))) +
        geom_bar(stat = "identity") +
        guides(fill = guide_legend(reverse = TRUE))

# as before, we can change fill colors as desired
ggplot(avg_mpg, aes(factor(cyl), mpg, fill = factor(am, levels = c(1, 0)))) +
        geom_bar(stat = "identity", color = "grey40") +
        scale_fill_manual(values = c("#a1d99b", "#31a354")) +
        labs(fill = "AM")

# use proportional stacked bars to compare groups
# proportional distribution of cars by cyl and am
proportion <- mtcars %>%
        group_by(cyl, am) %>%
        tally() %>%
        group_by(cyl) %>%
        mutate(pct = n / sum(n))

ggplot(proportion, aes(factor(cyl), pct, fill = factor(am, levels = c(1, 0)))) +
        geom_bar(stat = "identity", color = "grey40") +
        scale_fill_manual(values = c("#a1d99b", "#31a354")) +
        labs(fill = "AM")


#########################
# Adding Labels/Markers #
#########################
ggplot(mtcars, aes(reorder(row.names(mtcars), mpg), mpg)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        geom_text(aes(label = mpg), nudge_y = 1)

ggplot(mtcars, aes(reorder(row.names(mtcars), mpg), mpg)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        geom_text(aes(label = mpg), nudge_y = -1, color = "white")


# highlight just one car and label
cars <- mtcars %>%
        mutate(Make = row.names(mtcars),
               ID = ifelse(Make == "Fiat X1-9", TRUE, FALSE))

ggplot(cars, aes(reorder(Make, mpg), mpg, fill = ID)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        scale_fill_manual(values = c("grey90", "dodgerblue")) +
        annotate("text", x = "Fiat X1-9", y = 24.5, label = "mpg = 27.3", color = "white") +
        theme_minimal() +
        theme(legend.position = "none")
               
# label grouped bars (vjust = .5 places labels just on top of bar)
ggplot(avg_mpg, aes(factor(cyl), mpg, fill = factor(am))) +
        geom_bar(stat = "identity", position = "dodge") +
        geom_text(aes(label = round(mpg, 1)), position = position_dodge(0.9),
                  vjust = 1.5, color = "white")

# label proportional bars
proportion <- proportion %>%
        group_by(cyl) %>%
        mutate(label_y = cumsum(pct))

ggplot(proportion, aes(factor(cyl), pct, fill = factor(am, levels = c(1, 0)))) +
        geom_bar(stat = "identity", color = "grey40") +
        geom_text(aes(label = round(pct, 2), y = label_y), vjust = 1.5, color = "white") +
        scale_fill_manual(values = c("#a1d99b", "#31a354")) +
        labs(fill = "AM")
               
#####################
# Finishing Touches #
#####################
income <- read.xlsx("Data/PEW Middle Class Data.xlsx",
                    sheetIndex = "1. Distribution, metro",
                    startRow = 10, colIndex = c(1:4, 6:8)) %>%
        set_colnames(c("Metro", "Lower_00", "Middle_00", "Upper_00", "Lower_14",
                       "Middle_14", "Upper_14")) %>%
        filter(Metro != "NA")

head(income)

# compare 2000 to 2014 for Dayton Ohio
dayton <- income %>%
        filter(Metro == "Dayton, OH") %>%
        gather(metric, value, -Metro) %>%
        separate(metric, into = c("class", "year")) %>%
        mutate(year = ifelse(year == "00", 2000, 2014),
               value = value/100,
               y_label = paste0(round(value*100, 1), "%"))

ggplot(dayton, aes(x = class, y = value, fill = factor(year))) +
        geom_bar(stat = "identity", position = "dodge", color = "grey40") +
        geom_text(aes(label = y_label), position = position_dodge(0.9), 
                      vjust = 1.5, color = "white", family = "Georgia") +
        scale_fill_manual(values = c("#a1d99b", "#31a354")) +
        scale_y_continuous(labels = scales::percent) +
        scale_x_discrete(labels = c("Lower" = "Lower Class",
                         "Middle" = "Middle Class", "Upper" = "Upper Class")) +
        labs(title = "Distribution of Adults by Income in Dayton, OH",
             subtitle = "The percentage of adults in the middle class eroded by 5.3% from 2000 to 2014. Although a small \nfraction of these individuals moved into the upper class (+0.5%), the majority of these middle class \nindividuals moved into the lower income class (+4.8%).",
             caption = "Source: Pew Research Center analysis of the \n2000 decennial census and 2014 American \nCommunity Survey (IPUMS)") +
        theme_minimal() +
        theme(axis.title = element_blank(),
              panel.grid.major.x = element_blank(),
              panel.grid.minor = element_blank(),
              legend.position = c(1,1), legend.justification = c(1,1),
              legend.background = element_blank(),
              legend.direction="horizontal",
              legend.title = element_blank(),
              text = element_text(family = "Georgia"),
              plot.title = element_text(size = 20, margin = margin(b = 10)),
              plot.subtitle = element_text(size = 12, color = "darkslategrey", margin = margin(b = 25)),
              plot.caption = element_text(size = 8, margin = margin(t = 10), color = "grey70", hjust = 0))
        
        



               