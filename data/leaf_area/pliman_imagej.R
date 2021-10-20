setwd("E:/Desktop/paper_pliman/data/leaf_area")
library(pliman)
library(LeafArea)
library(tidyverse)
library(ggrepel)
library(ggthemes)
library(DescTools)

get_ccc <- function(df, predicted, real){
  if(is.grouped_df(df)){
    df %>% 
      group_modify(~get_ccc(.x, {{predicted}}, {{real}})) %>% 
      ungroup()
  } else{
    predicted <- pull(df, {{predicted}})
    real <- pull(df, {{real}})
    cor <- CCC(real, predicted, na.rm = TRUE)
    data.frame(r = cor(real, predicted, use = "pairwise.complete.obs"),
               pc = cor$rho.c[[1]],
               lwr_ci = cor$rho.c[[2]],
               upr_ci = cor$rho.c[[3]],
               bc = cor$C.b)
  }
}



system.time(
lfa <-
  run.ij(path.imagej = "C:/Program Files/ImageJ",
         set.directory = "E:/Desktop/paper_pliman/data/leaf_area/imagej",
         known.distance = 20.9804,
         save.image = FALSE,
         trim.pixel = 0)
)


system.time(
plm <-
  analyze_objects(img_pattern = "img",
                  index = "G",
                  dir_original = "imagej",
                  object_size = "large",
                  parallel = TRUE,
                  workers = 8,
                  fill_hull = TRUE,
                  # show_original = FALSE,
                  # show_segmentation = FALSE,
                  # col_background = "black",
                  # col_foreground = "green",
                  save_image = F,
                  show_image = F)
)

area <- get_measures(plm, dpi = 100)

# images computed by both LeafArea and pliman
area_lfa <- rename(lfa, leaf_area = total.leaf.area)
area_pliman <- area$summary %>% select(img, area_sum) %>% set_names(c("sample", "pliman"))
df_area <- 
  full_join(area_lfa, area_pliman) %>% 
  mutate(res = pliman - leaf_area,
         label = ifelse(sample %in% c("img90", "img87", "img107"), sample, NA),
         color = ifelse(sample %in% c("img90", "img87", "img107"), "red", "gray"),
         shape = case_when(sample %in% paste0("img", 1:101) ~ 1,
                           sample %in% paste0("img", 102:112) ~ 2,
                           sample %in% paste0("img", 113:140) ~ 3,
                           sample %in% paste0("img", 141:150) ~ 4))

df_cor <- df_area[1:98, c(2, 3)]
ccc <- 
  get_ccc(df_cor, leaf_area, pliman) %>% 
  mutate(rho = paste0("rho[c]:~", round(pc, 3),
                      "[(",round(lwr_ci,3), "-",
                      round(upr_ci,3), ")]" ),
         bc = paste0("C[b]:~", round(bc, 3)),
         r = paste0("r:~~~", round(r, 3)))

ggplot(df_area, aes(leaf_area, pliman)) + 
  geom_abline(intercept = 0, slope = 1,
              linetype = 2) +
  geom_point(aes(fill = color),
             show.legend = FALSE,
             shape = 21,
             size = 3,
             alpha = 0.8,
             color = "black",
             stroke = 0.01) +
  geom_text_repel(aes(label = label)) +
  geom_text(aes(label=rho), 
            x = 10,
            y = 260,
            hjust = 0,
            size = 4,
            data = ccc,
            parse = TRUE) + 
  geom_text(aes(label=bc), 
            x = 10,
            y = 250,
            size = 4,
            hjust = 0,
            data = ccc,
            parse = TRUE) + 
  geom_text(aes(label=r), 
            x = 10,
            y = 240,
            size = 4,
            hjust = 0,
            data = ccc,
            parse = TRUE) +
  scale_x_continuous(limits = c(0, 275), breaks = seq(0, 275, by = 50)) + 
  scale_y_continuous(limits = c(0, 275), breaks = seq(0, 275, by = 50)) + 
  theme_base() +
  labs(x = "LeafArea",
       y = "pliman")

ggsave("E:/Desktop/paper_pliman/figs/pliman_leafarea.pdf")


system.time(
  low_pix <-
    analyze_objects(img_pattern = "img",
                    index = "G",
                    dir_original = "50",
                    object_size = "large",
                    parallel = TRUE,
                    save_image = FALSE,
                    nrows = 100,
                    fill_hull = TRUE)
)


