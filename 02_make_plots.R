library(ggplot2)

afsc_index <- readRDS("afsc_index.rds")
afsc_index_all <- readRDS("afsc_index_all.rds")
afsc_index$Region = "Single"
afsc_index_all$Region = "All"
afsc <- rbind(afsc_index, afsc_index_all)
afsc$Survey <- "GOA"

pbs_index <- readRDS("pbs_index.rds")
pbs_index_all <- readRDS("pbs_index_all.rds")
pbs_index$Region = "Single"
pbs_index_all$Region = "All"

# joint index off by an offset
#pbs_index_all$log_est <- pbs_index_all$log_est + (pbs_index$log_est[1] - pbs_index_all$log_est[1])
pbs <- rbind(pbs_index, pbs_index_all)
pbs$Survey <- "PBS"

nwfsc_index <- readRDS("nwfsc_index.rds")
nwfsc_index_all <- readRDS("nwfsc_index_all.rds")
nwfsc_index$Region = "Single"
nwfsc_index_all$Region = "All"

# joint index off by an offset
#nwfsc_index_all$log_est <- nwfsc_index_all$log_est + (nwfsc_index$log_est[1] - nwfsc_index_all$log_est[1])
nwfsc <- rbind(nwfsc_index, nwfsc_index_all)
nwfsc$Survey <- "NWFSC"

dat <- rbind(pbs, nwfsc,afsc)

p1 <- dat %>%
  ggplot(aes(year, log_est, fill=Region, col=Region)) +
  geom_ribbon(aes(ymin=log_est - 2*se, ymax=log_est + 2*se), alpha=0.5, col=NA) +
  geom_line() +
  theme_bw() +
  xlab("Year") + ylab("Ln density") +
  facet_wrap(~Survey, scale="free")

p2 <- dat %>%
  ggplot(aes(year, se, fill=Region, col=Region)) +
  geom_line() +
  theme_bw() +
  xlab("Year") + ylab("SE") +
  facet_wrap(~Survey, scale="free")

gridExtra::grid.arrange(p1, p2, nrow=2)
ggsave("model_comparisons.png")
