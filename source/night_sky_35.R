
library(jasmines)
library(here)

seeds <- 511:519

blend <- function(x, y, p = .5) {
  x <- col2rgb(x)
  y <- col2rgb(y)
  z <- round(p*x + (1-p)*y)
  z <- rgb(red = z[1, ]/255, 
           green = z[2, ]/255, 
           blue = z[3, ]/255)
  return(z)
} 

night_sky <- function(seed, dpi = 600, width = 10, height = 10, grain = 5000,
                      iterations = 50, scale = .01) {
  
  version <- 35
  file <- make_filename("night_sky", version, seed, ".png")
  cat("making:", file, "\n")
  
  set.seed(seed)
  pal <- make_palette()
  use_seed(seed) %>% 
  scene_sticks(n = 3, grain = 1000) %>%
  unfold_breeze(20, drift = 0, scale = .0005) %>%
  dplyr::filter(time > 3) %>%
  dplyr::mutate(x = x * 2, y =y * 2) %>%
  dplyr::mutate(ind = 1:dplyr::n()) %>%
  unfold_warp(iterations = iterations, scale = scale) %>% 
  style_ribbon(
    palette = pal,
    colour = "ind", 
    alpha = c(0.2, scale), 
    background = blend(rscico(), "white", .3)
  ) %>% 
  export_image(
    filename = here("image", file), 
    dpi = dpi, 
    width = width, 
    height = height 
  )
  return(invisible(NULL))
}

rscico <- function() {
  pal <- sample(scico::scico_palette_names(), 1)
  sample(scico::scico(n=256, palette=pal), 1)
}

make_palette <- function() {
  base <- sample(colours(distinct=TRUE), 3)
  blends <- purrr::map_chr(base, ~blend(.x, "black", .3))
  colorRampPalette(blends)
}

make_filename <- function(prefix, sys_num, seed, suffix, 
                          sys_digits = 2, seed_digits = 3, sep = "_") {
  seed <- as.character(seed)
  sys_num <- as.character(sys_num)
  while(nchar(seed) < seed_digits) seed <- paste0("0", seed)
  while(nchar(sys_num) < sys_digits) sys_num <- paste0("0", sys_num)
  filename <- paste0(prefix, sep, sys_num, sep, seed, suffix)
  return(filename)
}


for(s in seeds) {
  night_sky(s)
  
  # ---- high resolution ----
  # night_sky(
  #   seed = s, 
  #   width = 100/3, 
  #   height = 100/3, 
  #   dpi = 600, 
  #   grain = 15000, 
  #   iterations = 200,
  #   scale = .005
  # )
}