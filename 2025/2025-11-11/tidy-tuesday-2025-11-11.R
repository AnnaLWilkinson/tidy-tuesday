
#  About this script ------------------------------------------------------

#  Project: Tidy Tuesday
#  Purpose: WHO TB Burden Data
#  Author: Anna Wilkinson; anna.wilkinson@burnet.edu.au
#  Date started: 11 Nov 2025


# Libraries ---------------------------------------------------------------

pacman::p_load(rio, 
               here, 
               tidyverse,
               janitor,
               lubridate,
               gtsummary,
               patchwork
               )

# Import data -------------------------------------------------------------

# Using R
# Option 1: tidytuesdayR R package 
## install.packages("tidytuesdayR")

tuesdata <- tidytuesdayR::tt_load('2025-11-11')
## OR
#tuesdata <- tidytuesdayR::tt_load(2025, week = 45)

who_tb_data <- tuesdata$who_tb_data

# Option 2: Read directly from GitHub

#who_tb_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-11-11/who_tb_data.csv')


# Summary -----------------------------------------------------------------

summary(who_tb_data)


# Exploratory data anlaysis -----------------------------------------------

tabyl(who_tb_data, country)
tabyl(who_tb_data, g_whoregion)

who_tb_data |> 
  ggplot() + 
  geom_line(aes(x = year, 
                 y = c_cdr,    # case detection rate (Rx coverage %)
                 group = country))

who_tb_data |> 
  ggplot() + 
  geom_line(aes(x = year, 
                y = c_cdr,
                group = country)) + 
  facet_wrap(~ g_whoregion)


who_tb_data |> 
  filter(g_whoregion == "Western Pacific") |> 
  ggplot() + 
  geom_line(aes(x = year, 
                y = c_cdr)) + 
  facet_wrap (~ country)


who_tb_data |> 
  filter(g_whoregion == "Western Pacific") |> 
  ggplot() + 
  geom_line(aes(x = year, 
                y = cfr)) +    # case fatality rate
  facet_wrap (~ country)


who_tb_data |> 
  filter(g_whoregion == "Western Pacific") |> 
  ggplot() + 
  geom_line(aes(x = year, 
                y = e_inc_100k)) +    # case fatality rate
  facet_wrap (~ country)


who_tb_data |> 
  filter(g_whoregion == "Western Pacific") |> 
  ggplot() + 
  geom_line(aes(x = year, 
                y = e_inc_num)) +    # case fatality rate
  facet_wrap (~ country)


who_tb_data |> 
  ggplot() + 
  geom_bar(aes(x = year,
               y = e_mort_100k),
           stat = "identity")


who_tb_data |> 
  ggplot() + 
  geom_bar(aes(x = year,
               y = e_mort_num),
           stat = "identity")

who_tb_data |> 
  ggplot() + 
  geom_bar(aes(x = year, 
           y = e_mort_tbhiv_num),
           stat = "identity" )
    

# PHP eda ---------------------------------------------------------------------


php <- who_tb_data |> 
  filter(country == "Philippines")

php |> 
  ggplot() + 
  geom_bar(aes(x = factor(year), 
               y = c_cdr),
           stat = "identity") + 
  theme(
    
    axis.text.x = element_text(angle = 270, vjust = 0)
  )

# define a function that accepts strings as inputs
plot_for_loop <-  function(df, x_var, y_var) {
  
  df |> 
    ggplot() + 
    geom_bar(aes(x = factor(.data[[x_var]]), 
                 y = .data[[y_var]]),
             stat = "identity") + 
    labs(x = "", 
         y = y_var,
         title = "Philippines") + 
    theme_bw() + 
    theme(
      
      axis.text.x = element_text(angle = 270, vjust = 0.5)
    )
}


plot_list <-  colnames(php)[-c(1:6)] |> 
  purrr::map(~ plot_for_loop(php, colnames(php)[6], .x))

plot_list
cowplot::plot_grid(plotlist = plot_list) + 
  labs( title = 'PHP')


## PHP

## plots with rates

### case notification rate per 100k
plot1 <- php |> 
  ggplot(aes(x = year, 
                y = c_newinc_100k,
             group = 1)) + 
  geom_bar(stat = "identity",
           fill = "grey70",
           colour = "grey80") + 
  #geom_point() + 
  geom_smooth(se = FALSE, 
              method = "gam",
              formula = y ~ s(x, bs = "ps"),
              colour = "grey9") + 
  labs(x = "",
       y = "Rate per 100k") + 
  ggtitle("Notifications") + 
  
  theme_bw() + 
  theme(
    
    axis.text.x = element_text(angle = 270, 
                               vjust =0,
                               colour = "grey80"),
    axis.text.y = element_text(colour = "grey80"),
    axis.title.y = element_text(colour = "grey80"),
    plot.title = element_text(margin = margin(t = 40,
                                              b = -30,
                                              l = 5),
                              size = 11)
  ) 


### incidence per 100k
plot2 <- php |> 
  ggplot(aes(x = year, 
             y = e_inc_100k)) + 
  geom_bar(stat = "identity",
           fill = "grey70",
           colour = "grey80") + 
  #geom_point() + 
  geom_smooth(se = FALSE, 
              method = "gam",
              formula = y ~ s(x, bs = "ps"),
              colour = "grey9") + 
  scale_y_continuous(limits = c(0, 700)) + 
  labs(x = "",
       y = "Rate per 100k") + 
  ggtitle("Incidence") + 
  theme_bw() + 
  theme(
    
    axis.text.x = element_text(angle = 270, 
                               vjust =0,
                               colour = "grey80"),
    axis.text.y = element_text(colour = "grey80"),
    axis.title.y = element_text(colour = "grey80"),
    plot.title = element_text(margin = margin(t = 40,
                                              b = -30,
                                              l = 5),
                              size = 11)
  )


### mortality all forms per 100k
plot3 <- php |> 
  ggplot(aes(x = year, 
             y = e_mort_100k)) + 
  geom_bar(stat = "identity",
           fill = "grey70",
           colour = "grey80") + 
  #geom_point() + 
  geom_smooth(se = FALSE, 
              method = "gam",
              formula = y ~ s(x, bs = "ps"),
              colour = "grey9") +
  scale_y_continuous(limits = c(0, 50)) + 
  labs(x = "",
       y = "Rate per 100k") + 
  ggtitle("Mortality") + 
  theme_bw() + 
  theme(
    
    axis.text.x = element_text(angle = 270, 
                               vjust =0,
                               colour = "grey80"),
    axis.text.y = element_text(colour = "grey80"),
    axis.title.y = element_text(colour = "grey80"),
    plot.title = element_text(margin = margin(t = 40,
                                              b = -30,
                                              l = 5),
                              size = 11)
  )


### mortality all forms  excl HIV per 100k
plot4 <- php |> 
  ggplot(aes(x = year, 
             y = e_mort_exc_tbhiv_100k)) + 
  geom_bar(stat = "identity",
           fill = "grey70",
           colour = "grey80") + 
  #geom_point() + 
  geom_smooth(se = FALSE, 
              method = "gam",
              formula = y ~ s(x, bs = "ps"),
              colour = "grey9") +
  scale_y_continuous(limits = c(0, 50)) + 
  labs(x = "",
       y = "Rate per 100k") + 
  ggtitle("Mortality (excl. HIV)") + 
  theme_bw() + 
  theme(
    
    axis.text.x = element_text(angle = 270, 
                               vjust =0,
                               colour = "grey80"),
    axis.text.y = element_text(colour = "grey80"),
    axis.title.y = element_text(colour = "grey80"),
    plot.title = element_text(margin = margin(t = 40,
                                              b = -30,
                                              l = 5),
                              size = 11)
  )



### mortality among people living with HIV all forms  excl HIV per 100k
plot5 <- php |> 
  ggplot(aes(x = year, 
             y = e_mort_tbhiv_100k)) + 
  geom_bar(stat = "identity",
           fill = "grey70",
           colour = "grey80") + 
  #geom_point() + 
  geom_smooth(se = FALSE, 
              method = "gam",
              formula = y ~ s(x, bs = "ps"),
              colour = "grey9") +
  #scale_y_continuous(limits = c(0, 10)) + 
  labs(x = "",
       y = "Rate per 100k") + 
  ggtitle("Mortality (living with HIV)") + 
  theme_bw() + 
  theme(
    
    axis.text.x = element_text(angle = 270, 
                               vjust =0,
                               colour = "grey80"),
    axis.text.y = element_text(colour = "grey80"),
    axis.title.y = element_text(colour = "grey80"),
    plot.title = element_text(margin = margin(t = 40,
                                              b = -30, 
                                              l = 5),
                              size =11)
  )


#### patchwork

(plot1 + plot2 + plot3 + plot4 + plot_spacer() + plot5)  +
  patchwork::plot_layout(axes = "collect_x") + 
  patchwork::plot_annotation(title = "TB in the Philippines")


ggsave(filename = here::here("figs","who_tb_burden.jpeg"),
       plot = last_plot())



## END






