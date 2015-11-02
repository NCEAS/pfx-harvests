library(dplyr)
spp <- read.csv("data-raw/SppClassification.csv", stringsAsFactors = FALSE, strip.white = TRUE)
d <- readRDS("data-generated/SelectData.rds")
d <- inner_join(d, spp)
d <- d %>%
  select(year, resource, percap_recalc, commname, quad, habitat.captured,
    commpop, xtotlbs_recalc, subregcd, taxonomic.grp) %>%
  mutate(subregcd = gsub(" ", "", subregcd)) %>%
  filter(!subregcd %in% c("UCW", "GLF")) %>%
  as.tbl()

d <- d %>% group_by(subregcd, taxonomic.grp, year, commname) %>%
  summarise(percap = sum(percap_recalc)) %>%
  as.data.frame() %>% as.tbl()

d <- na.omit(d)
d <- d[!is.infinite(d$percap), ]

library(ggplot2)
ggplot(d, aes(year, percap)) + geom_bar(stat = "identity") + 
  facet_grid(taxonomic.grp~subregcd, scales = "free_y") + theme_bw()