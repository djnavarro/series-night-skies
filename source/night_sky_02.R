# same as 01 but very high resolution!

library(jasmines)
library(here)

seeds <- 53


night_sky <- function(seed, dpi = 600, width = 10, height = 10, grain = 5000,
                      iterations = 100, scale = .01) {
  
  version <- 2
  file <- make_filename("night_sky", version, seed, ".png")
  cat("making:", file, "\n")
  
  use_seed(seed) %>% 
  scene_bubbles(n = 6, grain = grain) %>%
  unfold_warp(iterations = iterations, scale = scale) %>% 
  style_ribbon(
    palette = palette_named("magma"),
    colour = "id", 
    alpha = c(0.2, scale), 
    background = "wheat"
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
  night_sky(
    seed = s, 
    width = 100/3, 
    height = 100/3, 
    dpi = 600, 
    grain = 15000, 
    iterations = 200,
    scale = .005
  )
}