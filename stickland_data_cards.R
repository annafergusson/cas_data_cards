library(tidyverse)
library(magick)
library(glue)

# read in data
cas_data <- read_csv("CAS2015_edited.csv") 

# dimension of each data card in pixels
width <- 460
height <- 630

# read in images to use to make cards
stick <- image_read("stickman.png") %>% image_resize("400x")
fb <- image_read("facebook.png") %>% image_resize("100x")
sc <- image_read("snapchat.png") %>% image_resize("100x")
book <- image_read("book.jpg") %>% image_resize("100x")
tv <- image_read("tv.png") %>% image_resize("140x")
phone <- image_read("phone.png") %>% image_resize("80x")
bag <- image_read("bag.png") %>% image_resize("140x")
cross <- image_read("cross.png") %>% image_resize("200x") %>%
  image_crop(geometry="+30+40")

make_card <- function(row) {
  card <- image_blank(width, height, color = "white") %>%
    image_border("grey50", "1x1")
  
  # add image elements
  card <- card %>%
    image_composite(stick, offset="+30+40") %>%
    image_composite(fb, offset="+20+60") %>%
    image_composite(sc, offset="+340+60") %>%
    image_composite(book, offset="+20+500") %>%
    image_composite(tv, offset="+310+490") %>%
    image_composite(phone, offset="+340+210") %>%
    image_composite(bag, offset="+20+220")
  
  # add crosses if needed
  if (row$techfacebook == "no") {
    card <- card %>%
      image_composite(cross, offset = "+0+50")
  }
  if (row$techsnapchat == "no") {
    card <- card %>%
      image_composite(cross, offset = glue("+320+50"))
  }
  if (row$techphone == "no") {
    card <- card %>%
      image_composite(cross, offset = glue("+295+190"))
  }
  
  # add text elements
  font_size <- 25
  card <- card %>%
    image_annotate(row$name, location="+20+25", size = font_size) %>%
    image_annotate(glue("{ifelse(is.na(row$age),'',row$age)} years"), location="+180+60", size = font_size) %>%
    image_annotate(glue("{ifelse(is.na(row$reading), '', round(row$reading,2))} hr"), location = "+20+600", size = font_size) %>%
    image_annotate(glue("{ifelse(is.na(row$tv_time), '', round(row$tv_time,2))} hr"), location = "+330+560", size = font_size) %>%
    image_annotate(glue("{ifelse(is.na(row$bagweight), '', round(row$bagweight,1))} kg"), location = "+40+280", size = font_size)
  
  return(card)
}

# make one card
cas_data %>% slice(1) %>% make_card()

# make all the cards
cards <- map(1 : nrow(cas_data), function(i){
  cas_data %>% slice(i) %>% make_card()
})

# you could just create individual images for each data card
# and insert them into a word document or similar for printing
# or use them within a digital environment

if (!dir.exists("data_cards")) {
  dir.create("data_cards")
} else {
  unlink("data_cards", recursive = TRUE)
  dir.create("data_cards")
}

for(i in 1:length(cards)){
  image_write(cards[[i]], glue("data_cards/card{i}.png"))
}


# this code assembles the data cards into pages for a PDF
# for printing etc.

# six across, three down
cards_per_page <- 18
pages <- split(cards, 
               ceiling(seq_along(cards)/cards_per_page))

make_page <- function(card_imgs) {
  # using A4 paper size 150dpi :)
  page_width <- 1754
  page_height <- 1240
  margin <- 30
  card_width <- floor((page_width - 2*margin)/6)
  card_height <- ceiling((height * card_width)/width)
  page <- image_blank(page_width, page_height, color = "white") 
  xpos <- rep(seq(margin, by = card_width, length.out = 6), 3)
  ypos <- rep(seq(margin, by = card_height, length.out = 3), each = 6)
  for (i in seq_along(card_imgs)) {
    card_resized <- image_resize(card_imgs[[i]], glue("{card_width}x{card_height}!"))
    page <- image_composite(page, card_resized, offset = glue("+{xpos[i]}+{ypos[i]}"))
  }
  page
}

# testing first page - good idea!
# pages <- list(pages[[1]])
pages_imgs <- map(pages, make_page)
pdf <- image_join(pages_imgs)
image_write(pdf, path = "data_cards.pdf", format = "pdf")
