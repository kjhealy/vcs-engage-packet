## -----------------------------------------------------------------------------
#| label: "08-polishing-and-presenting-plots-1"
#| message: FALSE
library(here)       # manage file paths
library(tidyverse)  # your friend and mine
library(socviz)     # data and some useful functions
library(ggrepel)    # Text and labels
library(colorspace) # luminance-balanced palettes
library(scales)      # scale adjustments and enhancements
library(ggforce)    # useful enhancements to ggplot


## -----------------------------------------------------------------------------
#| label: "08-polishing-and-presenting-plots-2"
asasec <- as_tibble(asasec)
asasec


## -----------------------------------------------------------------------------
#| label: "08-polishing-and-presenting-plots-3"
#| echo: FALSE
#| fig.height: 6
#| fig.width: 10
asasec |>
  filter(Year == 2014) |>
  ggplot(mapping = aes(x = Members,
                       y = Revenues,
                       label = Sname)) +
  geom_smooth() +
  geom_point()



## -----------------------------------------------------------------------------
#| label: "08-polishing-and-presenting-plots-4"
#| echo: FALSE
#| fig.height: 6
#| fig.width: 10
asasec |>
  filter(Year == 2014) |>
  ggplot(mapping = aes(x = Members,
                       y = Revenues,
                       label = Sname)) +
  geom_smooth(method = "lm",
              se = FALSE,
              color = "gray60") +
  geom_point(mapping = aes(color = Journal),
             size = rel(3)) +
  geom_text_repel(data=subset(asasec,
                    Year == 2014 &
                    Revenues > 7000),
                    size = rel(5),
                    mapping =
                    aes(family = "Tenso Slide")) +
  scale_y_continuous(labels =
                       scales::label_dollar()) +
  labs(x="Membership", y="Revenues",
        color = "Section has own Journal",
        title = "ASA Sections",
        subtitle = "2014 Calendar year.",
        caption = "Source: ASA annual report.") +
  theme(legend.position = "bottom")




## -----------------------------------------------------------------------------
#| label: "reveal-asasteps"
#| include: FALSE
#| fig-width: 6
#| fig-height: 10

df_rich <- asasec |> filter(Year == 2014 &
                            Revenues > 7000)

asasec |>
  filter(Year == 2014) |>
  ggplot(mapping = aes(x = Members,
                       y = Revenues,
                       label = Sname)) +
  geom_smooth(method = "lm",
              se = FALSE,
              color = "gray60") +
  geom_point(mapping = aes(color = Journal),
             size = rel(3)) +
  geom_text_repel(data = df_rich,
                    size = rel(5),
                    mapping =
                    aes(family = "Tenso Slide")) +
  scale_y_continuous(labels =
                       scales::label_dollar()) +
  labs(x="Membership", y="Revenues",
        color = "Section has own Journal",
        title = "ASA Sections",
        subtitle = "2014 Calendar year.",
        caption = "Source: ASA annual report.") +
  theme(legend.position = "bottom")



## -----------------------------------------------------------------------------
#| label: "08-polishing-and-presenting-plots-15"
#| echo: FALSE


## -----------------------------------------------------------------------------
county_data |>
  select(name, state, pop, black, partywinner16, flipped) |>
  sample_n(10)


## -----------------------------------------------------------------------------
#| label: "08-polishing-and-presenting-plots-16"
#| echo: FALSE
#| fig.width: 15
#| fig.height: 8.5
# Democratic Blue and Republican Red
party_colors <- c("#2E74C0", "#CB454A")
party_colors <- c("royalblue1", "red2")

ggplot(data = subset(county_data,
                     flipped == "No"),
       mapping = aes(x = pop,
                     y = black/100)) +
  geom_point(alpha = 0.15, color = "gray20",
             size = rel(2)) +
  scale_x_log10(labels = label_comma()) +
  geom_point(data = subset(county_data,
                      flipped == "Yes"),
             mapping = aes(x = pop, y = black/100,
                           color = partywinner16),
             size = rel(2)) +
  geom_text_repel(data = subset(county_data,
                                flipped == "Yes" &
                                  black  > 25),
                  mapping = aes(x = pop,
                                y = black/100,
                                label = state,
                                family = "Tenso Slide",
                                face = "bold"),
                  size = 3.5) +
  scale_color_manual(values = party_colors) +
  scale_y_continuous(labels = label_percent()) +
  labs(color = "County flipped to ... ",
       x = "County Population (log scale)",
       y = "Percent Black Population",
       title = "Flipped counties, 2016",
       caption = "Counties in gray did not flip.")




## -----------------------------------------------------------------------------
#| label: "reveal-fliptrump"
#| include: FALSE

## Brighter Blue and Red
party_colors <- c("royalblue1", "red2")

## Data subsets for our geoms
df_noflip <- county_data |> filter(flipped == "No")
df_flip <- county_data |> filter(flipped == "Yes")
df_text <- county_data |> filter(flipped == "Yes" & black  > 25)

ggplot(data = df_noflip,
       mapping = aes(x = pop,
                     y = black/100)) +
  geom_point(alpha = 0.15,
             color = "gray30", size = rel(2)) +
  scale_x_log10(labels = label_comma()) +
  geom_point(data = df_flip,
             mapping = aes(x = pop, y = black/100,
                           color = partywinner16),
             size = rel(2)) +
  geom_text_repel(data = df_text,
                  mapping = aes(label = state,
                    family = "Tenso Slide"),
                    size = rel(3.5)) +
  scale_color_manual(values = party_colors) +
  scale_y_continuous(labels = label_percent()) +
  labs(color = "County flipped to ... ",
       x = "County Population (log scale)",
       y = "Percent Black Population",
       title = "Flipped counties, 2016",
       caption = "Counties in gray did not flip.")



## -----------------------------------------------------------------------------
#| label: "08-polishing-and-presenting-plots-17"
#| echo: FALSE
county_data <- as_tibble(county_data) |>
  filter(!is.na(name) & name %nin% as.character(c(1:52)))

pop_min <- min(county_data$pop)
pop_max <- max(county_data$pop)

black_min <- min(county_data$black/100)
black_max <- max(county_data$black/100 + 0.047)

x_label <- "County Population (log scale)"
y_label <- "Percent Black Population"

x_breaks <- c(1e3, 1e4, 1e5, 1e6, 1e7)
y_breaks <- seq(from = 0, to = 0.8, by = 0.2)
data_point_size <- rel(3)

p_layer_1 <- ggplot(data = county_data,
                    mapping = aes(x = pop, y = black/100)) +
  geom_point(color = "gray20",
             alpha = 0.25,
             size = data_point_size) +
  scale_x_log10(breaks = x_breaks,
                labels = label_number(scale_cut = cut_short_scale())) +
  scale_y_continuous(breaks = y_breaks,
                     labels = label_percent()) +
  expand_limits(x = pop_max,
                y = black_max) +
  labs(x = x_label,
       y = y_label,
       title = "U.S. Counties by Population and Percent Black",
       caption = "")

p_layer_2 <- ggplot(data = subset(county_data,
                     flipped == "No"),
                    mapping = aes(x = pop, y = black/100)) +
  geom_point(color = "gray20",
             alpha = 0.25,
             size = data_point_size) +
  expand_limits(x = pop_max,
                y = black_max) +
  scale_x_log10(breaks = x_breaks,
                labels = label_number(scale_cut = cut_short_scale())) +
  scale_y_continuous(breaks = y_breaks,
                     labels = label_percent()) +
  labs(x = x_label,
       y = y_label,
       title = "These counties did not flip in 2016",
       caption = "")


p_layer_3 <- ggplot(data = subset(county_data,
                     flipped == "Yes"),
                    mapping = aes(x = pop, y = black/100)) +
  geom_point(color = "gray5", shape = 1, size = data_point_size) +
  scale_x_log10(breaks = x_breaks,
                labels = label_number(scale_cut = cut_short_scale())) +
  scale_y_continuous(breaks = y_breaks,
                     labels = label_percent()) +
  expand_limits(x = pop_max,
                y = black_max) +
  labs(x = x_label,
       y = y_label,
       title = "These counties did",
       caption = "")


p_layer_4 <- ggplot(data = subset(county_data,
                     flipped == "No"),
                    mapping = aes(x = pop, y = black/100)) +
  geom_point(color = "gray30",
             alpha = 0.25,
             size = data_point_size) +
  geom_point(data = subset(county_data,
                      flipped == "Yes"),
             mapping = aes(x = pop, y = black/100,
                           color = partywinner16),
             size = data_point_size) +
  scale_x_log10(breaks = x_breaks,
                labels = label_number(scale_cut = cut_short_scale())) +
  scale_y_continuous(breaks = y_breaks,
                     labels = label_percent()) +
  scale_color_manual(values = party_colors) +
  expand_limits(x = pop_max,
                y = black_max) +
  labs(x = x_label,
       y = y_label,
       title = "Counties that flipped shown by party color",
       color = "Flipped to",
       caption = "Counties in gray did not flip.")

p_layer_5 <- ggplot(data = subset(county_data,
                     flipped == "No"),
                    mapping = aes(x = pop, y = black/100)) +
  geom_point(color = "gray30",
             alpha = 0.25,
             size = data_point_size) +
  geom_point(data = subset(county_data,
                      flipped == "Yes"),
             mapping = aes(x = pop, y = black/100,
                           color = partywinner16),
             size = rel(2)) +
  geom_label_repel(data = subset(county_data,
              flipped == "Yes" & black  > 25),
                  mapping = aes(x = pop,
                    y = black/100, label = state,
                    family = "Tenso Slide"),
                    size = rel(3.5)) +
  scale_x_log10(breaks = x_breaks,
                labels = label_number(scale_cut = cut_short_scale())) +
  scale_y_continuous(breaks = y_breaks,
                     labels = label_percent()) +
  scale_color_manual(values = party_colors) +
  expand_limits(x = pop_max,
                y = black_max) +
  labs(x = x_label,
       y = y_label,
       color = "Flipped to",
       title = "Counties that flipped shown by party color, and labeled by state",
       caption = "Counties in gray did not flip.")


## Zoom in -- replace expand_limits() with coord_cartesian()
## Adjust repel criteria also
p_layer_6 <- ggplot(data = subset(county_data,
                     flipped == "No"),
                    mapping = aes(x = pop, y = black/100)) +
  geom_point(color = "gray30",
             alpha = 0.25,
             size = data_point_size) +
  geom_point(data = subset(county_data,
                      flipped == "Yes"),
             mapping = aes(x = pop, y = black/100,
                           color = partywinner16),
             size = rel(2)) +
  geom_label_repel(data = subset(county_data,
              flipped == "Yes" & black
              > 20 & black < 50),
                  mapping = aes(x = pop,
                    y = black/100, label = state,
                    family = "Tenso Slide"),
                    size = rel(3.5)) +
  scale_x_log10(breaks = x_breaks,
                labels = label_number(scale_cut = cut_short_scale())) +
  scale_y_continuous(breaks = y_breaks,
                     labels = label_percent()) +
  scale_color_manual(values = party_colors) +
  coord_cartesian(xlim = c(0.75e4, 1.5e5),
                  ylim = c(0.2, 0.5)) +
  labs(x = x_label,
       y = y_label,
       color = "Flipped to",
       title = "Counties that flipped shown by party color, and labeled by state; zoomed-in",
       caption = "Counties in gray did not flip.")





## -----------------------------------------------------------------------------
#| label: "08-polishing-and-presenting-plots-18"
#| echo: FALSE
#| fig.width: 15
#| fig.height: 8.5
p_layer_1


## -----------------------------------------------------------------------------
#| label: "08-polishing-and-presenting-plots-19"
#| echo: FALSE
#| fig.width: 15
#| fig.height: 8.5
p_layer_2


## -----------------------------------------------------------------------------
#| label: "08-polishing-and-presenting-plots-20"
#| echo: FALSE
#| fig.width: 15
#| fig.height: 8.5
p_layer_3


## -----------------------------------------------------------------------------
#| label: "08-polishing-and-presenting-plots-21"
#| echo: FALSE
#| fig.width: 15
#| fig.height: 8.5
p_layer_4


## -----------------------------------------------------------------------------
#| label: "08-polishing-and-presenting-plots-22"
#| echo: FALSE
#| fig.width: 15
#| fig.height: 8.5
p_layer_5


## -----------------------------------------------------------------------------
#| label: "08-polishing-and-presenting-plots-23"
#| echo: FALSE
#| fig.width: 15
#| fig.height: 8.5
p_layer_6


## -----------------------------------------------------------------------------
#| label: "08-polishing-and-presenting-plots-24"
#| echo: FALSE
#| fig.width: 15
#| fig.height: 8.5
p_layer_5


## -----------------------------------------------------------------------------
#| label: "08-polishing-and-presenting-plots-25"
#| echo: false


## -----------------------------------------------------------------------------
#| label: "codefig-themes1"
#| message: FALSE
#| fig.width: 4.8
#| fig.height: 4.5
#| output-location: column

p <- organdata |>
  drop_na(world) |>
  ggplot(mapping = aes(x = roads, y = donors,
                          color = world)) +
  geom_point(size = 3) +
  labs(x = "Road Deaths",
       y = "Procurement Rate",
       title = "By Welfare State Regime")

p



## -----------------------------------------------------------------------------
#| label: "codefig-theme2"
#| message: FALSE
#| fig.width: 4.8
#| fig.height: 4.5
#| output-location: column
p + theme_bw()


## -----------------------------------------------------------------------------
#| label: "codefig-theme3"
#| message: FALSE
#| fig.width: 4.8
#| fig.height: 4.5
#| output-location: column
p + theme_minimal()


## -----------------------------------------------------------------------------
#| label: "codefig-themedark"
#| message: FALSE
#| fig.width: 4.8
#| fig.height: 4.5
#| output-location: column
p + theme_dark()


## -----------------------------------------------------------------------------
#| label: "codefig-themeadditive"
#| message: FALSE
#| fig.width: 4.8
#| fig.height: 4.5
#| output-location: column

p + theme_bw() +
  theme(legend.position = "top")



## -----------------------------------------------------------------------------
#| label: "codefig-elementtext"
#| message: FALSE
#| fig.width: 4.8
#| fig.height: 4.5
#| output-location: column

p + theme(plot.title =
            element_text(size = rel(3),
                         face = "bold",
                         color = "orange"))



## -----------------------------------------------------------------------------
#| label: "codefig-elementline"
#| message: FALSE
#| fig.width: 4.8
#| fig.height: 4.5
#| output-location: column

p + theme(panel.grid.major.y =
            element_line(color = "red"),
          panel.grid.minor.y =
            element_line(color = "black",
                         linetype = "dotted"))



## -----------------------------------------------------------------------------
#| label: "08-polishing-and-presenting-plots-33"
#| echo: FALSE
#| fig.width: 12
#| fig.height: 8
p_layer_4


## -----------------------------------------------------------------------------
#| label: "08-polishing-and-presenting-plots-34"
library(ggthemes)
theme_set(theme_fivethirtyeight())



## -----------------------------------------------------------------------------
#| label: "08-polishing-and-presenting-plots-35"
#| echo: FALSE
#| fig.width: 12
#| fig.height: 8
p_layer_4


## -----------------------------------------------------------------------------
#| label: "08-polishing-and-presenting-plots-36"
theme_set(theme_economist())



## -----------------------------------------------------------------------------
#| label: "08-polishing-and-presenting-plots-37"
#| echo: FALSE
#| fig.width: 12
#| fig.height: 8
p_layer_4


## -----------------------------------------------------------------------------
#| label: "08-polishing-and-presenting-plots-38"
theme_set(theme_stata())



## -----------------------------------------------------------------------------
#| label: "08-polishing-and-presenting-plots-39"
#| echo: FALSE
#| fig.width: 12
#| fig.height: 8
p_layer_4


## -----------------------------------------------------------------------------
theme_bw


## -----------------------------------------------------------------------------
theme_rice <- function(base_size = 14,
                          base_family = "",
                          base_line_size = base_size/22,
                          base_rect_size = base_size/22) {

  ## See https://brand.rice.edu/colors/
  riceblue1 <- "#00205B"
  riceblue2 <- "#6D7D9C"
  ricegray1 <- "#686B6C"
  ricegray2 <- "#B0B2B2"
  textcolor <- "#FFFFFF"
  half_line <- base_size / 2

  ggplot2::theme_minimal(base_size = base_size, base_family = base_family,
        base_line_size = base_line_size, base_rect_size = base_rect_size) %+replace%
    theme(
      axis.text = element_text(color = textcolor,
                               face = "bold"),
      axis.ticks = element_line(color = textcolor),
      axis.title = element_text(color = textcolor),
      plot.title = element_text(color = textcolor,
                                face = "bold",
                                size = rel(2),
                                # left-justified text
                                hjust = 0, vjust = 1),
      plot.subtitle = element_text(color = textcolor, size = rel(1.2),
                                   hjust = 0, vjust = 1,
                                   margin = margin(t = half_line, b = half_line)),
      plot.caption = element_text(color = textcolor, hjust = 1), # right-justified
      plot.background = element_rect(fill = riceblue2, color = "white"),
      panel.background = element_rect(fill = "white", color = NA),
      panel.border = element_rect(fill = NA, color = riceblue2),
      panel.grid = element_line(color = riceblue2,
                                linetype = "dotted",
                                linewidth = rel(0.4)),
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "#FCF7E5"),
      legend.text = element_text(color = textcolor),
      legend.title = element_text(color = textcolor),
      legend.position = "top",
      legend.key.size = unit(2,"line"),
      legend.title.position = "top"
    )
}


## -----------------------------------------------------------------------------
p <- ggplot(data = mtcars,
            mapping = aes(x = wt,
                          y = mpg,
                          fill = factor(cyl))) +
  geom_point(size = rel(3),
             shape = 21, color = "white") +
  labs(x = "Weight", y = "MPG", fill = "Cylinders",
       title = "A plot about cars",
       subtitle = "This is the subtitle",
       caption = "And this is the caption")



## -----------------------------------------------------------------------------
#| fig-width: 8
#| fig-height: 6
#| out-width: "100%"

rice_accents <- c("#E9A139", "#C04829", "#359245", "#362E52", "#005B50")

p + scale_fill_manual(values = rice_accents) +
  theme_rice(base_family = "Mallory") # See https://brand.rice.edu





