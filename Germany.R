library(ggplot2)

# Run djhurio's script once to get all EU data & regenerate faceted plots
# install.packages(c("data.table", "ISOcodes", "eurostat"))
source('deaths-by-week.R'); rm(list = ls())

# My goal here: Split mortality data for a single country by pre- & post-CoVID
# TODO: Loop over all other countries & generate plots with English labels.
country <- 'Germany'
covid_start <- 2020
label_pre  <- 'vor SARS-CoV2'
label_post <- 'Pandemiejahre'
weeks <- c(1, 12, 24 , 36, 48)
file_name <- paste0('output/', country, '.jpg')

data <- readr::read_csv('data.csv') |>
  dplyr::filter(cntry == country) |>
  dplyr::transmute(year, # = gsub('W-\\d+', '', time),
                   # country = cntry, # for all countries, instead of filter
                   week, death.rate) |>
  dplyr::mutate(SARS = ifelse(year < covid_start, label_pre, label_post)) #|>
# dplyr::group_by(Woche)
# death_median = median(death.rate),
# death_iqr = IQR(death.rate)

pre <- dplyr::filter(data, SARS == label_pre) |> dplyr::rename(year_pre = year)
post <- dplyr::filter(data, SARS == label_post) |> dplyr::rename(year_post = year)
post_years <- unique(post$year_post)
N_years <- length(post_years)
max_deaths <- ceiling(max(post$death.rate, pre$death.rate))

pre_start <- min(pre$year_pre)
pre_end <- max(pre$year_pre)
# Grippe <- dplyr::filter(pre, Woche > 7 & Woche < 11) %>%
#   dplyr::slice_max(death.rate, prop = 0.05) %>%
#   dplyr::select(Jahr) %>%
#   unique()

(compare_pre <- ggplot(mapping = aes(x = week, y = death.rate)) +
  geom_line(data = pre,
            aes(alpha = year_pre, group = year_pre),
            show.legend = FALSE) +
    geom_line(data = post,
              color = "red",
              linewidth = 1,
              show.legend = FALSE
    ) +
  scale_x_continuous(breaks = weeks, minor_breaks = NULL) +
  labs(
    x = '', y = '') +
  facet_grid(. ~ year_post) +
  theme_minimal())

(compare_post <- ggplot(
    data = post,
    mapping = aes(x = week, y = death.rate, color = factor(year_post))
  ) +
    geom_line(linewidth = 1) +
    scale_color_discrete(name = 'CoVID-Jahr') +
    scale_x_continuous(breaks = weeks, minor_breaks = NULL) +
    # scale_colour_gradient2(low = 'orange', mid = "blue", midpoint = mid_year, high = "red", name = label_post) +
    xlab('') +
    ylab('') +
    # ) +
    guides(color = guide_legend(ncol = N_years, label.position = 'top')) +
    theme_minimal() +
    theme(
      legend.position = 'inside',
      legend.position.inside = c(.5, .8),
      legend.background = element_rect(fill = 'white', linewidth = 0)
    ))

(aggregate_plot <- gridExtra::arrangeGrob(
  compare_pre,
  compare_post,
  nrow = 2,
  # widths = c(2/3, 1/3),
  top = paste0(
    "Deutschland: Sterblichkeit seit SARS-CoV2\n\n",
    'Quelle: eurostat demo_r_mwk_ts & GitHub.com/djhurio/COVID-19',
    '\nFrühere Krisen: Grippe im Spätwinter und Hitze im Sommer',
    '\nAlterungstrend: ',
    pre_start,
    '/hellgrau → ',
    pre_end,
    '/schwarz'
  ),
  left = "Todesfälle pro 1 Mio. Einwohner:inne:n",
  bottom = "Kalenderwochen"

))

ggsave(file_name, aggregate_plot,
       width = 3072,
       height = 2048,
       units = 'px')
