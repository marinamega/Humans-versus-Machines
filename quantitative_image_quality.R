setwd('C:/Users/marin/Documents/Papers/Marine Biology/Revisões/1')

library(imager)
library(magick)
library(tidyverse)
library(purrr)
library(openxlsx)
library(readxl)

base_dir <- "C:/Users/marin/Documents/Papers/Marine Biology/Revisões/1/Photoquadrats_Humans_vs_Machines"

img_files <- list.files(
  path = base_dir,
  pattern = "\\.(jpg|jpeg|JPG|JPEG)$",
  recursive = TRUE,
  full.names = TRUE
)

image_df <- tibble(
  image_path = img_files,
  image = basename(img_files),
  set = ifelse(str_detect(img_files, "Evaluation Set"),
               "Evaluation",
               "Training")
)

### Shadow ###
## Local shadow: parts of the image that are much darker than the surroundings
local_shadow <- function(img_path,
                         sigma = 120,        # illumination scale (large)
                         tex_sigma = 5,    # texture scale (small)
                         q_ratio = 0.10,     # relatively darkest pixels (lowest 10%)
                         #q_sat   = 0.10,     # least saturated pixels (lowest 10%) [unused]
                         q_tex   = 0.30      # least textured pixels (lowest 30%)
) {
  tryCatch({
    
    img <- imager::load.image(img_path) # load image
    hsv <- imager::RGBtoHSV(img) # convert to HSV
    
    #S <- hsv[,,,2, drop = FALSE]
    V <- hsv[,,,3, drop = FALSE] 
    
    # Local illumination and ratio (relative to neighborhood)
    local_mean <- imager::isoblur(V, sigma = sigma) + 1e-6 #apply blur
    ratio <- V / local_mean # how dark a pixel is relative to its neighborhood
    
    # Local texture (SD) at a small scale
    m1 <- imager::isoblur(V,  sigma = tex_sigma)
    m2 <- imager::isoblur(V^2, sigma = tex_sigma)
    varV <- pmax(m2 - m1^2, 0)
    sdV <- sqrt(varV)
    
    # Relative thresholds (per image)
    thr_ratio <- as.numeric(stats::quantile(as.numeric(ratio), probs = q_ratio, na.rm = TRUE))
    # thr_sat   <- as.numeric(stats::quantile(as.numeric(S),     probs = q_sat,   na.rm = TRUE))
    thr_tex   <- as.numeric(stats::quantile(as.numeric(sdV),   probs = q_tex,   na.rm = TRUE))
    
    # Shadow = relatively dark + low texture (relative)
    shadow_map <- (ratio <= thr_ratio) & (sdV <= thr_tex) #& (S <= thr_sat)
    
    mean(as.numeric(shadow_map)) * 100
    
  }, error = function(e) NA_real_)
}

ls_df <- image_df %>%
  #slice(200:300) %>%
  mutate(
    local_shadow = map_dbl(image_path, local_shadow)
  )

write.xlsx(
  ls_df,
  "C:/Users/marin/Documents/Papers/Marine Biology/Revisões/1/local_shadow.xlsx",
  overwrite = TRUE
)

min_img <- ls_df %>%
  filter(!is.na(local_shadow)) %>%
  slice_min(local_shadow, n = 3)

max_img <- ls_df %>%
  filter(!is.na(local_shadow)) %>%
  slice_max(local_shadow, n = 3)

set.seed(123)
random_imgs <- ls_df %>%
  filter(!is.na(local_shadow)) %>%
  filter(!image_path %in% c(min_img$image_path, max_img$image_path)) %>%
  slice_sample(n = 4)

imgs_to_plot <- bind_rows(
  min_img  %>% mutate(type = "MIN"),
  max_img  %>% mutate(type = "MAX"),
  random_imgs %>% mutate(type = "RANDOM")
)

shadow_diagnostic <- function(img_path,
                              sigma = 120,
                              tex_sigma = 5,
                              q_ratio = 0.10,
                              q_tex   = 0.30) {
  
  img <- load.image(img_path)
  hsv <- RGBtoHSV(img)
  V <- hsv[,,,3, drop = FALSE]
  
  local_mean <- isoblur(V, sigma = sigma) + 1e-6
  ratio <- V / local_mean
  
  m1 <- isoblur(V,  sigma = tex_sigma)
  m2 <- isoblur(V^2, sigma = tex_sigma)
  sdV <- sqrt(pmax(m2 - m1^2, 0))
  
  thr_ratio <- as.numeric(quantile(as.numeric(ratio), probs = q_ratio, na.rm = TRUE))
  thr_tex   <- as.numeric(quantile(as.numeric(sdV),   probs = q_tex,   na.rm = TRUE))
  
  shadow_map <- (ratio <= thr_ratio) & (sdV <= thr_tex)
  shadow_pct <- mean(as.numeric(shadow_map)) * 100
  
  list(img = img, shadow_map = shadow_map, shadow_pct = shadow_pct)
}


plot_shadow_side_by_side <- function(img_path,
                                     sigma = 120,
                                     tex_sigma = 5,
                                     q_ratio = 0.10,
                                     q_tex   = 0.30,
                                     title_prefix = "") {
  
  out <- shadow_diagnostic(img_path,
                           sigma = sigma,
                           tex_sigma = tex_sigma,
                           q_ratio = q_ratio,
                           q_tex = q_tex)
  
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  
  par(mfrow = c(1, 2),
      mar = c(0.8, 0.8, 0.8, 0.8),   
      oma = c(0, 0, 2.0, 0))         
  
  plot(out$img, main = "", axes = FALSE)
  plot(out$shadow_map, main = "", axes = FALSE)
  
  mtext(paste0(title_prefix, "Original | Shadow map (", round(out$shadow_pct, 2), "%)"),
        outer = TRUE, cex = 0.9, line = 0.2)
}


dir.create("shadow_figs", showWarnings = FALSE)

for (i in seq_len(nrow(imgs_to_plot))) {
  
  fname <- tools::file_path_sans_ext(basename(imgs_to_plot$image_path[i]))
  
  png(
    filename = file.path("shadow_figs", paste0(fname, "_shadow.png")),
    width = 2313,   # 771 × 3
    height = 1323,  # 441 × 3
    res = 300,
    pointsize = 10,
    type = "cairo"
  )
  
  plot_shadow_side_by_side(
    imgs_to_plot$image_path[i],
    sigma = 120,
    tex_sigma = 5,
    q_ratio = 0.10,
    q_tex = 0.30,
    title_prefix = paste0(imgs_to_plot$type[i], " | ")
  )
  
  dev.off()
}

## Global shadow: the entire image is too dark. This is not completely wrong,
## but it is not what I wanted—this feels more like local shadow to me.
gs_and_overexp_metrics <- function(img_path, p = 0.97, clip = 0.95) {
  tryCatch({
    img <- imager::load.image(img_path)
    hsv <- imager::RGBtoHSV(img)
    V <- hsv[,,,3, drop = FALSE]
    v <- as.numeric(V)
    
    # % of saturated pixels (brightness >= clip)
    overexp_pct <- mean(v >= clip, na.rm = TRUE) * 100
    
    # Remove saturated pixels
    v2 <- v[v < clip]
    
    # If almost everything is saturated, the metric is unreliable
    if (length(v2) < 1000) {
      return(tibble::tibble(
        global_shadow = NA_real_,
        top_mean = NA_real_,
        overexposure_pct = overexp_pct
      ))
    }
    
    thr <- as.numeric(stats::quantile(v2, probs = p, na.rm = TRUE))
    top_mean <- mean(v2[v2 >= thr], na.rm = TRUE)
    
    tibble::tibble(
      global_shadow = 1 - top_mean,
      top_mean = top_mean,
      overexposure_pct = overexp_pct
    )
  }, error = function(e) tibble::tibble(
    global_shadow = NA_real_,
    top_mean = NA_real_,
    overexposure_pct = NA_real_
  ))
}

gs_and_overexp_df <- image_df %>%
  # slice(200:300) %>%
  mutate(metrics = purrr::map(image_path, ~gs_and_overexp_metrics(.x, p = 0.97, clip = 0.95))) %>%
  tidyr::unnest_wider(metrics)

write.xlsx(gs_and_overexp_df, "gs_and_overexp_df.xlsx")

min_img <- gs_and_overexp_df %>%
  filter(!is.na(global_shadow)) %>%
  slice_min(global_shadow, n = 3)

max_img <- gs_and_overexp_df %>%
  filter(!is.na(global_shadow)) %>%
  slice_max(global_shadow, n = 3)

set.seed(123)
random_imgs <- gs_and_overexp_df %>%
  filter(!is.na(global_shadow)) %>%
  filter(!image_path %in% c(min_img$image_path, max_img$image_path)) %>%
  slice_sample(n = 4)

imgs_to_plot <- bind_rows(
  min_img  %>% mutate(type = "MIN"),
  max_img  %>% mutate(type = "MAX"),
  random_imgs %>% mutate(type = "RANDOM")
)

plot_global_shadow_side_by_side <- function(img_path,
                                            global_shadow_value,
                                            top_mean_value,
                                            p = 0.97,
                                            clip = 0.95,
                                            title_prefix = "") {
  
  img <- imager::load.image(img_path)
  hsv <- imager::RGBtoHSV(img)
  V <- hsv[,,,3, drop = FALSE]
  
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  
  par(
    mfrow = c(1, 2),
    mar  = c(0.8, 0.8, 0.8, 0.8),
    oma  = c(0, 0, 2.4, 0)
  )
  
  plot(img, main = "", axes = FALSE, interpolate = TRUE)
  plot(V,   main = "", axes = FALSE, interpolate = TRUE)
  
  ttl <- paste0(
    title_prefix,
    "Global shadow = ", round(global_shadow_value, 3),
    " | top mean (p=", p, ") = ", round(top_mean_value, 3),
    " | clip<", clip
  )
  
  mtext(ttl, outer = TRUE, cex = 0.9, line = 0.4)
  
  invisible(NULL)
}

dir.create("global_shadow_figs", showWarnings = FALSE)

for (i in seq_len(nrow(imgs_to_plot))) {
  
  fname <- tools::file_path_sans_ext(basename(imgs_to_plot$image_path[i]))
  
  png(
    filename = file.path("global_shadow_figs", paste0(fname, "_globalshadow.png")),
    width = 2313,     # 771 * 3
    height = 1323,    # 441 * 3
    res = 300,
    pointsize = 10,
    type = "cairo"
  )
  
  plot_global_shadow_side_by_side(
    img_path = imgs_to_plot$image_path[i],
    global_shadow_value = imgs_to_plot$global_shadow[i],
    top_mean_value = imgs_to_plot$top_mean[i],
    p = 0.97,
    clip = 0.95,
    title_prefix = paste0(imgs_to_plot$type[i], " | ")
  )
  
  dev.off()
}

### Overexposure ###
min_img <- gs_and_overexp_df %>%
  filter(!is.na(overexposure_pct)) %>%
  slice_min(overexposure_pct, n = 3)

max_img <- gs_and_overexp_df %>%
  filter(!is.na(overexposure_pct)) %>%
  slice_max(overexposure_pct, n = 3)

set.seed(123)
random_imgs <- gs_and_overexp_df %>%
  filter(!is.na(overexposure_pct)) %>%
  filter(!image_path %in% c(min_img$image_path, max_img$image_path)) %>%
  slice_sample(n = 4)

imgs_to_plot <- bind_rows(
  min_img  %>% mutate(type = "MIN"),
  max_img  %>% mutate(type = "MAX"),
  random_imgs %>% mutate(type = "RANDOM")
)

plot_overexposure_side_by_side <- function(img_path,
                                           overexposure_value,
                                           clip = 0.95,
                                           alpha = 0.6,
                                           title_prefix = "") {
  library(imager)
  
  img <- imager::load.image(img_path)
  hsv <- imager::RGBtoHSV(img)
  V <- hsv[,,,3, drop = FALSE]
  
  # Mask of saturated pixels
  over_map <- V >= clip
  
  # Create a red layer
  R <- over_map * 1
  G <- over_map * 0
  B <- over_map * 0
  red_mask <- imager::imappend(list(R, G, B), "c")
  
  # overlay
  overlay <- (1 - alpha) * img + alpha * red_mask
  
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  
  par(
    mfrow = c(1, 2),
    mar  = c(0.8, 0.8, 0.8, 0.8),
    oma  = c(0, 0, 2.4, 0)
  )
  
  plot(img,     main = "", axes = FALSE, interpolate = TRUE)
  plot(overlay, main = "", axes = FALSE, interpolate = TRUE)
  
  ttl <- paste0(
    title_prefix,
    "Overexposure = ", round(overexposure_value, 1), "%",
    " | clip ≥ ", clip
  )
  
  mtext(ttl, outer = TRUE, cex = 0.9, line = 0.4)
  
  invisible(NULL)
}

dir.create("overexposure_figs", showWarnings = FALSE)

for (i in seq_len(nrow(imgs_to_plot))) {
  
  fname <- tools::file_path_sans_ext(basename(imgs_to_plot$image_path[i]))
  
  png(
    filename = file.path("overexposure_figs", paste0(fname, "_overexposure.png")),
    width = 2313,     # 771 * 3
    height = 1323,    # 441 * 3
    res = 300,
    pointsize = 10,
    type = "cairo"
  )
  
  plot_overexposure_side_by_side(
    img_path = imgs_to_plot$image_path[i],
    overexposure_value = imgs_to_plot$overexposure_pct[i],
    clip = 0.95,
    alpha = 0.6,
    title_prefix = paste0(imgs_to_plot$type[i], " | ")
  )
  
  dev.off()
}

# Joining dataframes
quantit_imperfections <- bind_cols(ls_df, 
                               gs_and_overexp_df %>% select(overexposure_pct),
                               gs_and_overexp_df %>% select(global_shadow))

quantit_imperfections <- quantit_imperfections %>%
  mutate(image = tolower(image))

write.xlsx(
  quantit_imperfections,
  "C:/Users/marin/Documents/Papers/Marine Biology/Revisões/1/quantit_imperfections.xlsx",
  overwrite = TRUE
)

pa_images_imperfections <- read_excel("pa_images_imperfections.xlsx")

image_quality <- quantit_imperfections %>%
  left_join(pa_images_imperfections, by = "image") %>% 
  select(-c("locality", "year", "site", "transecto"))

write.xlsx(
  image_quality,
  "C:/Users/marin/Documents/Papers/Marine Biology/Revisões/1/image_quality.xlsx",
  overwrite = TRUE
)

#### Histograms
#Kappa
ggplot(image_model_df, aes(estimate)) +
  geom_histogram(bins = 30, color = "black", fill = "grey70") +
  labs(
    x = "Cohen's Kappa value",
    y = "Number of images"
  ) +
  theme_classic()

#Local shadow
ggplot(ls_df, aes(local_shadow)) +
  geom_histogram(bins = 30, color = "black", fill = "grey70") +
  labs(
    x = "% dark pixels (local shadow)",
    y = "Number of images"
  ) +
  theme_classic()

#Global shadow
ggplot(gs_df, aes(global_shadow)) +
  geom_histogram(bins = 30, color = "black", fill = "grey70") +
  labs(
    x = "Mean grayscale value",
    y = "Number of images"
  ) +
  theme_classic()

#Overexposure
ggplot(gs_df, aes(overexposure_pct)) +
  geom_histogram(bins = 30, color = "black", fill = "grey70") +
  labs(
    x = "% saturated pixels (overexposure)",
    y = "Number of images"
  ) +
  theme_classic()