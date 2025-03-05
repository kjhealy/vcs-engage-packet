## -----------------------------------------------------------------------------
#| label: "05-work-with-dplyr-and-geoms-1"
#| message: TRUE
library(here)      # manage file paths
library(socviz)    # data and some useful functions
library(tidyverse) # your friend and mine
library(gapminder) # some data


## -----------------------------------------------------------------------------
#| label: "02-about-r-66-2"
gapminder


## -----------------------------------------------------------------------------
#| label: "codefig-plot-2"
#| message: FALSE
#| fig.width: 8
#| fig.height: 6
#| output-location: slide

library(tidyverse)
library(gapminder)

p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap,
                          y = lifeExp,
                          color = continent))

p + geom_point() +
   labs(x = "GDP per capita",
        y = "Life Expectancy",
        color = "Continent")


## -----------------------------------------------------------------------------
#| label: "02-about-r-68"
#| message: FALSE
#| eval: FALSE
#| echo: TRUE

#
# library(gapminder)


## -----------------------------------------------------------------------------
#| label: "02-about-r-69"
#| message: FALSE
#| eval: FALSE
#| echo: TRUE

# p <- ggplot(data = gapminder,
#             mapping = aes(x = gdpPercap,
#                           y = lifeExp,
#                           color = continent))


## -----------------------------------------------------------------------------
#| label: "02-about-r-70-2"
#| message: FALSE
#| eval: FALSE
#| echo: TRUE

#
# p + geom_point() +
#    labs(x = "GDP per capita",
#         y = "Life Expectancy",
#         color = "Continent")
#


## -----------------------------------------------------------------------------
#| label: "03-make-a-graph-12"
gapminder


## -----------------------------------------------------------------------------
#| label: "03-make-a-graph-13"
dim(gapminder)


## -----------------------------------------------------------------------------
#| label: "03-make-a-graph-14"
p <- ggplot(data = gapminder)


## -----------------------------------------------------------------------------
#| label: "03-make-a-graph-15"
p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap,
                          y = lifeExp))


## -----------------------------------------------------------------------------
#| label: "03-make-a-graph-16"
#| fig.cap: "This empty plot has no geoms."
#| fig.width: 8
#| fig.height: 5
p


## -----------------------------------------------------------------------------
#| label: "03-make-a-graph-17"
#| fig.cap: "A scatterplot of Life Expectancy vs GDP"
#| fig.width: 8
#| fig.height: 5
p + geom_point()


## -----------------------------------------------------------------------------
#| label: "03-make-a-graph-18"
#| fig.cap: "A scatterplot of Life Expectancy vs GDP"
#| fig.width: 8
#| fig.height: 5
p + geom_smooth()


## -----------------------------------------------------------------------------
#| label: "03-make-a-graph-19"
#| fig.cap: "Life Expectancy vs GDP, using a smoother."
#| fig.width: 8
#| fig.height: 5

p <- ggplot(data = gapminder,
            mapping = aes(x = gdpPercap,
                          y=lifeExp))
p + geom_smooth()



## -----------------------------------------------------------------------------
#| label: "reveal-additive1"
#| include: FALSE

p + geom_smooth() +
    geom_point()


## -----------------------------------------------------------------------------
#| label: "codefig-functionargs"
#| fig.caption: "An ill-advised linear fit"
#| echo: TRUE
#| output-location: column

p + geom_point() +
    geom_smooth(method = "lm") +
    scale_x_log10()


## -----------------------------------------------------------------------------
#| label: "reveal-logtrans2"
#| include: FALSE

p + geom_point() +
    geom_smooth(method = "lm") +
    scale_x_log10(labels = scales::label_dollar())


## -----------------------------------------------------------------------------
#| label: "codefig-logtranslab"
#| message: FALSE
#| fig.width: 5
#| fig.height: 4.5
#| output-location: column

p + geom_point() +
  geom_smooth(method = "lm") +
    scale_x_log10(labels = scales::label_dollar()) +
    labs(x = "GDP Per Capita",
         y = "Life Expectancy in Years",
         title = "Economic Growth and Life Expectancy",
         subtitle = "Data points are country-years",
         caption = "Source: Gapminder.")


## -----------------------------------------------------------------------------
#| label: "codefig-logtranslab-pipe"
#| message: FALSE
#| fig.width: 5
#| fig.height: 4.5
#| output-location: column

gapminder |> #<<
  ggplot(mapping = aes(x = gdpPercap, #<<
                          y = lifeExp)) + #<<
  geom_point() +
  geom_smooth(method = "lm") +
    scale_x_log10(labels = scales::label_dollar()) +
    labs(x = "GDP Per Capita",
         y = "Life Expectancy in Years",
         title = "Economic Growth and Life Expectancy",
         subtitle = "Data points are country-years",
         caption = "Source: Gapminder.")


## -----------------------------------------------------------------------------
#| label: "03-make-a-graph-27"
#| fig.width: 8.5
#| fig.height: 5
#| output-location: slide

gapminder |>
  ggplot(aes(x = gdpPercap,
             y = lifeExp)) +
  geom_point(alpha = 0.3) +
  geom_smooth(color = "orange",
              se = FALSE,
              linewidth = 8,
              method = "lm") +
  scale_x_log10()


## -----------------------------------------------------------------------------
#| label: "codefig-alphapoints"
#| message: FALSE
#| fig.width: 5
#| fig.height: 4.5
#| output-location: column

gapminder |>
  ggplot(aes(x = gdpPercap,
               y = lifeExp)) +
  geom_point(alpha = 0.3) + #<<
  geom_smooth(method = "lm", se = FALSE) +
    scale_x_log10(labels = scales::label_dollar()) +
    labs(x = "GDP Per Capita",
         y = "Life Expectancy in Years",
         title = "Economic Growth and Life Expectancy",
         subtitle = "Data points are country-years",
         caption = "Source: Gapminder.")


## -----------------------------------------------------------------------------
#| label: "reveal-pergeom1"
#| include: FALSE

gapminder |>
  ggplot(aes(x = gdpPercap,
             y = lifeExp,
             color = continent,
             fill = continent)) +
  geom_point() +
  geom_smooth(method = "loess") +
  scale_x_log10(labels = scales::label_dollar())


## -----------------------------------------------------------------------------
#| label: "reveal-pergeom2"
#| include: FALSE

gapminder |>
  ggplot(aes(x = gdpPercap,
             y = lifeExp)) +
  geom_point(mapping = aes(color = continent)) +
  geom_smooth(method = "loess") +
  scale_x_log10(labels = scales::label_dollar())



## -----------------------------------------------------------------------------
#| label: "reveal-facet"
#| include: FALSE
#| output-location: column

gapminder |>
  ggplot(aes(x = year,
             y = gdpPercap)) +
  geom_line(color="gray70",
            aes(group = country)) +
  geom_smooth(linewidth = 1.1,
              method = "loess",
              se = FALSE) +
    scale_y_log10(labels=scales::label_dollar()) +
    facet_wrap(~ continent, nrow = 1) +#<<
    labs(x = "Year",
         y = "log GDP per capita",
         title = "GDP per capita on Five Continents",
         caption = "Data: Gapminder") -> p_out


## -----------------------------------------------------------------------------
#| label: "04-show-the-right-numbers-3"
#| echo: FALSE
#| fig.width: 18
#| fig.height: 5
p_out


## -----------------------------------------------------------------------------
gss_sm |>
   select(year, age, race, sex, bigregion, religion)


## -----------------------------------------------------------------------------
#| label: "reveal-dplyr-and-geoms-24"

rel_by_region <- gss_sm |>
  group_by(bigregion, religion) |>
  tally() |>
  mutate(pct = round((n/sum(n))*100, 1)) |>
  drop_na()


rel_by_region


## -----------------------------------------------------------------------------
#| label: "05-work-with-dplyr-and-geoms-25"
p <- ggplot(data = rel_by_region,
                mapping = aes(x = bigregion,
                              y = pct,
                              fill = religion))

p_out <- p + geom_col(position = "dodge") +
    labs(x = "Region", y = "Percent",
         fill = "Religion")


## -----------------------------------------------------------------------------
#| label: "05-work-with-dplyr-and-geoms-26"
#| echo: FALSE
#| fig.height: 7
#| fig.width: 12
p_out


## -----------------------------------------------------------------------------
#| label: "05-work-with-dplyr-and-geoms-27"
#| echo: FALSE
#| fig.height: 7
#| fig.width: 12
p_out


## -----------------------------------------------------------------------------
#| label: "05-work-with-dplyr-and-geoms-28"
#| fig-width: 12
#| fig-height: 3

p <- ggplot(data = rel_by_region,
                mapping = aes(x = pct, #<<
                              y = reorder(religion, -pct), #<<
                              fill = religion))
p + geom_col() +
  guides(fill = "none") +
  facet_wrap(~ bigregion, nrow = 1) +
  labs(x = "Percent", y = NULL)



## -----------------------------------------------------------------------------
#| label: "05-work-with-dplyr-and-geoms-30"
#| fig.width: 10
#| fig.height: 5
p <-  ggplot(data = gss_sm,
             mapping = aes(x = age, y = childs))

p + geom_point(alpha = 0.2) +
  geom_smooth() +
  facet_wrap(~ race)



## -----------------------------------------------------------------------------
#| label: "05-work-with-dplyr-and-geoms-31"
#| fig.width: 8
#| fig.height: 5.5
p <-  ggplot(data = gss_sm,
             mapping = aes(x = age, y = childs))

p + geom_point(alpha = 0.2) +
  geom_smooth() +
  facet_wrap(~ sex + race) #<<



## -----------------------------------------------------------------------------
#| label: "05-work-with-dplyr-and-geoms-32"
#| fig.width: 15
#| fig.height: 5.5
p <-  ggplot(data = gss_sm,
             mapping = aes(x = age, y = childs))

p + geom_point(alpha = 0.2) +
  geom_smooth() +
  facet_wrap(~ sex + race, nrow = 1) #<<



## -----------------------------------------------------------------------------
#| label: "05-work-with-dplyr-and-geoms-33"
#| fig.width: 11
#| fig.height: 6.5
#| warning: FALSE
p + geom_point(alpha = 0.2) +
  geom_smooth() +
  facet_grid(sex ~ race) #<<



## -----------------------------------------------------------------------------
#| label: "05-work-with-dplyr-and-geoms-34"
#| fig.width: 11
#| fig.height: 6.5
#| warning: FALSE
p_out <- p + geom_point(alpha = 0.2) +
  geom_smooth() +
  facet_grid(bigregion ~ race + sex) #<<



## -----------------------------------------------------------------------------
#| label: "05-work-with-dplyr-and-geoms-35"
#| echo: FALSE
#| warning: FALSE
#| fig.width: 12
#| fig.height: 8
p_out

