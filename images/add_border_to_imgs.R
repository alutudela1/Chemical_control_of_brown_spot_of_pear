library(magick)
library(purrr)

# define image names and border size
img_names <- c("abate.jpg", "danjou.jpg")

# read images and add borders using map()
imgs <- map(img_names, 
            ~ image_border(image_read(here::here("images", .x)), "white", "30x30"))
names(imgs) <- c("abate_border", "danjou_border")

# write images to disk using walk()
iwalk(imgs, ~image_write(.x, here::here("images", paste0(.y,'.png'))))
        
