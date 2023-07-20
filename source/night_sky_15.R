
library(jasmines)
library(here)

seeds <- 332:340


night_sky <- function(seed, dpi = 600, width = 10, height = 10, grain = 5000) {
  
  version <- 15
  file <- make_filename("night_sky", version, seed, ".png")
  cat("making:", file, "\n")
  
  use_seed(seed) %>% 
  scene_bubbles(n = 12, grain = grain) %>%
  unfold_warp(iterations = 100, scale = 0.01) %>% 
  style_ribbon(
    palette = palette_named("grayC"),
    colour = "id", 
    alpha = c(0.2, 0.01), 
    background = "gray10"
  ) %>% 
  export_image(
    filename = here("image", file), 
    dpi = dpi, 
    width = width, 
    height = height
  )
  return(invisible(NULL))
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
  night_sky(seed = s)
}