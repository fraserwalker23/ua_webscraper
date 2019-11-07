library(rvest)
library(dplyr)
library(RSelenium)
library(stringr)

# Shop all Mens web page from UA
ua <- read_html("https://www.underarmour.com/en-us/mens/g/39")

# Pull all headers <h1>
ua %>%
  html_nodes("h1")

# Extract the text from said headers <h1>
ua %>%
  html_nodes("h1") %>%
  html_text()

# List items
# only show a small batch otherwise the console will explode
ua %>%
  html_nodes("li") %>%
  html_text() %>%
  .[c(1:2,4:6)]

# Product Titles
tmp <- ua %>%
  html_nodes("div") %>%
  html_nodes(".title") %>%
  html_text()
tmp[(length(tmp)-10):length(tmp)]

# Notice: this includes product sub-titles as well as product titles.

tmp2 <- ua %>%
  html_nodes("div") %>%
  html_nodes(".title:not(.sub)") %>%
  html_text()
tmp2[(length(tmp2)-10):length(tmp2)]
# Key difference: something like "Men's Shorts" has 2 classes: "title", and "sub"
# NOT one class called "title sub"

# Extract all pertinent information:
# Given an html page, extract all of the following:
# Product ID, Title, Sub-title, Price

# by default UA loads 60 items: need everything to be 60 elements long.

title <- ua %>%
  html_nodes("div") %>%
  html_nodes(".title:not(.sub)") %>%
  html_text()
length(title)

subtitle <- ua %>%
  html_nodes("div") %>%
  html_nodes(".title.sub") %>%
  html_text()
length(subtitle)

px <- ua %>%
  html_nodes("div") %>%
  html_nodes(".price") %>%
  html_text()
length(px) # 118. Almost two prices per product

px <- ua %>%
  html_nodes("div") %>%
  html_nodes(".price-detail") %>%
  html_text()
length(px) 
# Conclusion: "price" is used apx 2x for each product (sale and regular?)
# The reason it's not 120 vs. 118: 2 products are "Sold Out" or "Coming Soon!" 
# No regular price?

pid <- ua %>%
  # This is an xpath selector - probably worth looking into
  xml_find_all("//@data-pid") %>%
  html_text() %>%
  as.integer()

# all components *must* be same length
df <- cbind(pid, title, subtitle, px)  %>%
  as.data.frame()

# The Infinite Scroll ----

# checkForServer() and startserver() both defunct, both replaced by rsDriver()
#?rsDriver
#rD[["server"]]$stop(); rm(rD, remDr) # Close the server and remove the objects
rD <- rsDriver(browser=c("chrome"), chromever = "77.0.3865.40")
remDr <- rD[["client"]]
#remDr$open()
remDr$navigate("http://www.underarmour.com/en-us/mens/g/39/")
#binman::list_versions("chromedriver")

# Close the "How Do You Want to Shop" BUtton
popup <- remDr$findElement(using = 'css selector', 
                           "div.modal-dialog button")
popup$clickElement()
# Wait
Sys.sleep(2)
# Close the newsletter popup
# notice: It's the exact same code!!
popup2 <- remDr$findElement(using = 'css selector', 
                            "div.modal-dialog button")
popup2$clickElement()

# Begin Scrolling
# Logic: Use a while loop that terminates once the scrollHeight doesn't change
# However: in practice this takes a *long* time - can it be done faster by 
# playing around with the Sleep time?
lastHeight <- remDr$executeScript("return document.body.scrollHeight")
for (i in c(1:5)){
  print(lastHeight)
  remDr$executeScript("window.scrollTo(0, document.body.scrollHeight);")
  Sys.sleep(5) # wait 10 seconds
  newHeight <- remDr$executeScript("return document.body.scrollHeight")
  if(newHeight[[1]] == lastHeight[[1]]){
    break
  }
  lastHeight = newHeight
}
ua3 <- remDr$getPageSource()
#save(ua3, file = "underarmour_webscraper/ua3_html.RData")
#load("underarmour_webscraper/ua3_html.RData")

title <- read_html(ua3[[1]]) %>%
  html_nodes("div.grid") %>%
  html_nodes(".title:not(.sub)") %>%
  html_text()
length(title)

# tmp <- read_html(ua3[[1]]) %>%
#   html_nodes("#grid-container") %>%
#   html_nodes("div") %>%
#   html_nodes(".title") %>%
#   html_text()
# length(title)
# tmp_class <- html_attr(tmp, "class")
# which(tmp_class == "title sub")

subtitle <- read_html(ua3[[1]]) %>%
  html_nodes("div") %>%
  html_nodes(".title.sub") %>%
  html_text()
length(subtitle)

px <- read_html(ua3[[1]]) %>%
  html_nodes("div") %>%
  html_nodes(".price-detail") %>%
  html_text()
length(px) 

pid <- read_html(ua3[[1]]) %>%
  # This is an xpath selector - probably worth looking into
  xml_find_all("//@data-pid") %>%
  html_text() %>%
  as.integer()
length(pid)
#which(is.na(pid)) # 2 NA pid's
#title[c(147,209)] # Gift Cards!!!

# What *doesnt* have a sub-title?


# all components *must* be same length
df <- cbind(pid, title, subtitle, px)  %>%
  as.data.frame()


# Alternative: A For Loop.

tmp <- read_html(ua3[[1]]) %>%
  html_nodes("ul.tileset.stack-0") %>%
  html_nodes("li.tile") %>%
  html_attr("class")
data_id <- str_extract_all(tmp, "[0-9]+") %>%
  unlist() %>%
  as.integer()
# data_id
# max(data_id)

read_html(ua3[[1]]) %>%
  html_nodes("ul.tileset.stack-0") %>%
  html_nodes("li.tile") %>%
  html_attr("class") %>%
  str_extract_all("[0-9]+") %>%
  unlist() %>%
  as.integer()

df <- data.frame(matrix(NA, nrow = max(data_id) + 1, ncol = 4)) %>%
  mutate_if(is.logical, as.character)
colnames(df) <- c("product_id", 
                  "product_title", 
                  "product_subtitle", 
                  "product_px")

for (i in c(0:max(data_id))){
  #print(paste0("li.tile-",i))
  print(i)
  tmp <- read_html(ua3[[1]]) %>%
    html_nodes(paste0("li.tile-",i))
  
  tmp_title <- tmp %>%
    html_nodes("div.title:not(.sub)") %>%
    html_text()
  
  tmp_subtitle <- tmp %>%
    html_nodes("div.title.sub") %>%
    html_text()
  
  if(length(tmp_subtitle) == 0){
    tmp_subtitle <- NA_character_
  }
  
  tmp_px <- tmp %>%
    html_nodes("div.price-detail") %>%
    html_text()
  
  tmp_pid <- tmp %>%
    xml_find_all(".//@data-pid") %>%
    html_text()
  
  df$product_id[i+1] <- tmp_pid
  df$product_title[i+1] <- tmp_title
  df$product_subtitle[i+1] <- tmp_subtitle
  df$product_px[i+1] <- tmp_px
}

write.csv(df, "underarmour_webscraper/ua_us_mens.csv", row.names = FALSE)
nrow(unique(df))
# Appendix ----

# The Lego Movie
movie <- read_html("http://www.imdb.com/title/tt1490017/")
# "#titleCast" - the element with id = "titleCast"
# "div.see-more" - <div> elements with class "see-more" attribute
cast <- html_nodes(movie, "#titleCast") %>%
  html_nodes("div.see-more")
html_text(cast)
html_name(cast) # tag names
html_attrs(cast) # attributes
html_attr(cast, "class") # a specific attribute, eg: "class"