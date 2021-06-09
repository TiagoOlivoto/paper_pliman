library(pliman)
library(tidyverse)
setwd("E:/Desktop/pliman_supp/data/disease_severity/soybean_rust")

time_100 <-
system.time(
rust_100 <-
  symptomatic_area(img_pattern = "1",
                   resize = 100, # default
                   img_symptoms = "soy_rust",
                   img_healthy = "soy_leaf",
                   img_background = "soy_bg",
                   show_image = FALSE,
                   parallel = TRUE)
)

time_30 <-
system.time(
  rust_30 <-
    symptomatic_area(img_pattern = "1",
                     resize = 30,
                     img_symptoms = "soy_rust",
                     img_healthy = "soy_leaf",
                     img_background = "soy_bg",
                     show_image = T,
                     parallel = TRUE)
)

pliman <-
  rbind(
    rust_100 %>% mutate(resolution = 100, .before = sample),
    rust_30 %>% mutate(resolution = 30, .before = sample)) %>%
  rename(pliman = symptomatic) %>%
  mutate(sample = as.numeric(sample),
         resolution = as.factor(resolution)) %>%
  select(resolution, sample, pliman)

results <- rio::import("data_quant.xlsx") %>%
  metan::as_factor(resolution)

rio::export(bind, "data_quant_pliman.xlsx")
bind <- left_join(results, pliman)
corr_plot(bind, Quant:pliman)

library(epiR)
ccc100 <- epi.ccc(bind %>% filter(resolution == 100) %>% .[["Quant"]],
                  bind %>% filter(resolution == 100) %>% .[["pliman"]])
ccc100$rho.c

ccc30 <- epiR::epi.ccc(bind %>% filter(resolution == 30) %>% .[["Quant"]],
                 bind %>% filter(resolution == 30) %>% .[["pliman"]])
ccc30$rho.c





ggplot(bind, aes(Quant, pliman)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_abline(slope = 1, intercept = 0)+
  facet_wrap(~resolution) +
  # ggrepel::geom_text_repel(aes(label = sample))
  theme_bw()+
  labs(x = "Quant",
       y = "pliman")
