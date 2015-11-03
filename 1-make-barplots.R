library(dplyr)
spp <- read.csv("data-raw/SppClassification.csv", stringsAsFactors = FALSE, strip.white = TRUE)
d <- readRDS("data-generated/SelectData.rds")
d <- inner_join(d, spp)
d <- d %>%
  select(year, resource, percap_recalc, commname, quad,
    commpop, xtotlbs_recalc, subregcd, spp.group) %>%
  mutate(subregcd = gsub(" ", "", subregcd)) %>%
  filter(!subregcd %in% c("UCW", "GLF")) %>%
  as.tbl() %>%
  mutate(tax.group = spp.group)
d$tax.group[grepl("fish", d$spp.group)] <- "fish"

d <- d %>% group_by(subregcd, spp.group, year, commname) %>%
  summarise(percap = sum(percap_recalc, na.rm = TRUE))

# d2 <- d %>% group_by(subregcd, spp.group, year) %>%
#   summarise(percap_mean = mean(log(percap+0.01)), 
#     se = sd(log(percap+0.01)/sqrt(n()))) %>%
#   as.data.frame() %>% as.tbl()

d2 <- d %>% group_by(subregcd, spp.group, year) %>%
  summarise(percap_mean = log(median(percap)), 
    se = sd(percap)/sqrt(n()),
    n = n()) %>%
  as.data.frame() %>% as.tbl()

# From Kristin's code:
d2$oil.period <- NA
d2$oil.period[d2$year<1989] <- "pre-spill"
d2$oil.period[d2$year==1989] <- "spill"
d2$oil.period[d2$year>1989 & d2$year<1994] <- "short-post"
d2$oil.period[d2$year>1993] <- "long-Post"
d2$oil.region <- "affected"
d2$oil.region[d2$subregcd=="GLF"] <- "unaffected"
d2$oil.region[d2$subregcd=="JUN"] <- "unaffected"
d2$oil.region[d2$subregcd=="KET"] <- "unaffected"

library(ggplot2)

d2 <- filter(d2, !spp.group %in% c("other", "other fish", "marine algae"))

hab <- data_frame(
  spp.group = c(
    "marine mammal", 
    "marine invertebrate", 
    "marine fish", 
    "salmon", 
    "freshwater-anadromous fish", 
    "bird",
    "land mammal",  
    "terrestrial plant"),
  hab = c(
    "marine", 
    "marine", 
    "marine", 
    "both", 
    "both", 
    "both",
    "terrestrial",  
    "terrestrial"))

d2 <- inner_join(d2, hab)

d2$spp.group <- factor(d2$spp.group, 
  levels = c(
    "marine mammal", 
    "marine invertebrate", 
    "marine fish", 
    "salmon", 
    "freshwater-anadromous fish", 
    "bird",
    "land mammal",  
    "terrestrial plant"
    ))

d2$subregcd <- factor(d2$subregcd, 
  levels = c(
    "PWS",
    "KEN", 
    "KOD", 
    "AKP", 
    "KET", 
    "JUN" 
    ))

d2 <- group_by(d2, spp.group, subregcd) %>%
  mutate(percap_mean_sc = exp(percap_mean) / exp(max(percap_mean)))

# ggplot(d2, aes(year, percap_mean_sc)) + 
#   geom_bar(stat = "identity") +
#   facet_grid(spp.group~subregcd, scales = "free_y") + theme_bw()

# d2 <- filter(d2, !subregcd %in% "JUN")

.simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2),
    sep = "", collapse = " ")
}
d2$spp_cap <- as.character(sapply(as.character(d2$spp.group), .simpleCap))

make_panel <- function(x) {
  ii <<- ii + 1
  plot(1, 1, type = "n", xlim = c(1984, 2012), ylim = c(0, 1.07), axes = FALSE)
  if (x$hab[1] == "marine") col <- "#67A5D5"
  if (x$hab[1] == "terrestrial") col <- "#7BD575"
  if (x$hab[1] == "both") col <- "grey50"
  if (x$oil.region[1] == "affected") {
    rect(1989-0.3, 0, 1993+0.3, 1.07, border = "grey85", col = "grey85")
    rect(1993+0.3, 0, 2013, 1.07, border = "grey95", col = "grey95")
  }
  rect(x$year-0.3, rep(0, length(x$year)), x$year+0.3, x$percap_mean_sc, 
    border = col, col = col)
  box(col = "grey50", lwd = 0.7)
  symbols(x$year, rep(0.05, length(x$year)), circles = sqrt(x$n/pi)/1.5, 
    inches = FALSE, add = TRUE, fg = "grey20")
  if (ii %in% 43:48) axis(1, col = "grey50", at = c(seq(1980, 2010, 10)))
  if (ii %in% seq(1, 37, 6)) axis(2, las = 1, at = c(0.5, 1), col = "grey50")
  if (ii %in% 43) axis(2, las = 1, at = c(0, 0.5, 1), col = "grey50")
  if (ii %in% 1:6) mtext(x$subregcd[1], col = "grey20", cex = 0.8)
  par(xpd = NA)
  if (ii %in% seq(6, 100, 6)) {
   lab <- gsub(" ", "\n", x$spp_cap[1])
   lab <- gsub("-", "-\n", x$spp_cap[1])
     text(2012.4, 0.5, lab, col = "grey20", cex = 1.1, 
      las = 1, pos = 4)
  }
  par (xpd = FALSE)
}

ii <<- 0

pdf("sub-bars.pdf", width = 8, height = 5)
par(mfrow = c(8, 6))
par(cex = 0.65)
par(mar = c(0, 0, 0, 0))
par(oma = c(3.2, 3.2, 2, 9))
par(mgp = c(1.5, 0.4, 0), tck = -0.05,
  yaxs = "i", col.axis = "grey50", col.lab = "grey50")
plyr::d_ply(d2, c("spp.group", "subregcd"), make_panel)
mtext("Scaled median harvest volume per capita", side = 2, outer = TRUE, 
  line = 2, col = "grey20", cex = 0.8)
mtext("Year", side = 1, outer = TRUE, 
  line = 2, col = "grey20", cex = 0.8)
dev.off()
