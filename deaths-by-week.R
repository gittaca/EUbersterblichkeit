# Deaths by week

options("max.print" = 1e4)

library(eurostat)
library(data.table)
library(ggplot2)
library(ISOcodes)

# Country names
cntry <- as.data.table(ISO_3166_1)
cntry[, .(geo = Alpha_2, cntry = Name)]


# Population
# https://ec.europa.eu/eurostat/databrowser/view/demo_gind/

dat.pop <- get_eurostat(id = tolower("DEMO_GIND"))
setDT(dat.pop)

dat.pop[, .N, keyby = .(indic_de)]

dat.pop[, year := year(TIME_PERIOD)]

dat.pop[indic_de == "JAN", .(year, geo, pop = values)]


# https://ec.europa.eu/eurostat/web/products-datasets/-/demo_r_mwk_ts
dat <- get_eurostat(id = "demo_r_mwk_ts", time_format = "raw")
setDT(dat)

sapply(dat, class)
dat[, time := TIME_PERIOD]

dat[, .N, keyby = .(sex)]
dat[, .N, keyby = .(unit)]

dat[, .N, keyby = .(geo)]
dat[, .N, keyby = .(time)]

dat[, year := as.integer(substr(time, 1, 4))]
dat[, week := as.integer(substr(time, 7, 8))]

dat[, .N, keyby = .(year)]
dat[, .(n = .N, Y = sum(values)), keyby = .(year)][, P := prop.table(Y)][]

dat[, .N, keyby = .(week)]
dat[, .(n = .N, Y = sum(values)), keyby = .(week)][, P := prop.table(Y)][]

dcast.data.table(data = dat[sex == "T" & week < 99],
                 formula = geo ~ year, fun.aggregate = length)
dat[geo == "DE", .N, keyby = .(week)]

dat <- dat[!grepl("W99", time)]

dat[, y2020 := factor(x = ifelse(year < 2020L, 0, year - 2019L),
                      levels = 0:6,
                      labels = c("Y<2020", "Y=2020", "Y=2021", "Y=2022", "Y=2023", "Y=2024", "Y=2025"))]
dat[, .N, keyby = .(y2020)]


# Merge population total
dat1 <- merge(dat[sex == "T"],
              dat.pop[indic_de == "JAN", .(year, geo, pop = values)],
              by = c("year", "geo"), all.x = T)

# While latest population data isn't released, let's simply assume same as last year
current_year <- format(Sys.time(), "%Y")
last_year <- as.character(as.integer(current_year) - 1)

if (all(is.na(dat1[year == current_year & geo == "DE"]$pop))) {
  dat1[year == current_year & geo == "DE"]$pop <- dat1[year == last_year & geo == "DE"]$pop[1]
}

dat1[, death.rate := values / pop * 1e6]

dat1[geo != "AD"][order(death.rate)]

dat1[geo == "EL", geo := "GR"]
dat1[geo == "UK", geo := "GB"]

dat2 <- merge(dat1, cntry[, .(geo = Alpha_2, cntry = Name)],
              by = "geo", all.x = T)

dat2[is.na(cntry), .N, keyby = .(geo, cntry)]

setorder(dat2, geo, time)

dat2[order(time)]
dat2[geo == "LV"][order(time)]

dat2[time == max(time)]

# Filter total and !AD
dat3 <- dat2[sex == "T" & geo != "AD"]

last_obs <- dat3[, max(time)]
cat(last_obs, "\n")

pl1 <- ggplot(data = dat3,
              mapping = aes(x = week, y = values, group = year,
                            colour = y2020, alpha = year)) +
  geom_line() +
  geom_point(data = dat3[year >= 2020], shape = 20) +
  geom_vline(xintercept = 13 * 1:4, colour = "red", alpha = .2) +
  scale_x_continuous(breaks = 13 * 1:4, minor_breaks = 1:53) +
  # scale_colour_brewer(palette = "Dark2") +
  facet_wrap(~ cntry, scales = "free_y") +
  ggtitle("Deaths by week (total count)",
          paste("Datasource: Eurostat [DEMO_R_MWK_TS],",
                "last observation:", last_obs,
                "date:", Sys.Date())) +
  theme_bw()

pl2 <- ggplot(data = dat3[year <= dat.pop[, max(year)]],
              mapping = aes(x = week, y = death.rate, group = year,
                            colour = y2020, alpha = year)) +
  geom_line() +
  geom_point(data = dat3[year >= 2020 & year <= dat.pop[, max(year)]],
             shape = 20) +
  geom_vline(xintercept = 13 * 1:4, colour = "red", alpha = .2) +
  scale_x_continuous(breaks = 13 * 1:4, minor_breaks = 1:53) +
  # scale_colour_brewer(palette = "Paired") +
  facet_wrap(~ cntry) +
  ggtitle("Death rate by week (per 1,000,000 individuals)",
          paste("Datasource: Eurostat [DEMO_R_MWK_TS], [DEMO_GIND],",
                "last observation:", last_obs,
                "date:", Sys.Date())) +
  theme_bw()

pl3 <- ggplot(data = dat3[year <= dat.pop[, max(year)]],
              mapping = aes(x = week, y = death.rate, group = year,
                            colour = y2020, alpha = year)) +
  geom_line() +
  geom_point(data = dat3[year >= 2020 & year <= dat.pop[, max(year)]],
             shape = 20) +
  geom_vline(xintercept = 13 * 1:4, colour = "red", alpha = .2) +
  scale_x_continuous(breaks = 13 * 1:4, minor_breaks = 1:53) +
  # scale_colour_brewer(palette = "Paired") +
  facet_wrap(~ cntry, scales = "free_y") +
  ggtitle("Death rate by week (per 1,000,000 individuals)",
          paste("Datasource: Eurostat [DEMO_R_MWK_TS], [DEMO_GIND],",
                "last observation:", last_obs,
                "date:", Sys.Date())) +
  theme_bw()

ggsave(filename = "dths-by-wk-total.png",
       plot = pl1, width = 16, height = 9)
ggsave(filename = "dths-by-wk-rate.png",
       plot = pl2, width = 16, height = 9)
ggsave(filename = "dths-by-wk-rate-free-y.png",
       plot = pl3, width = 16, height = 9)

cairo_pdf(filename = "dths-by-wk.pdf", width = 16, height = 9, onefile = TRUE)
print(pl1)
print(pl2)
print(pl3)
dev.off()

fwrite(x = dat3, file = "data.csv")
