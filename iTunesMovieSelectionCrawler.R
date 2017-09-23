library(dplyr)
library(rvest)
library(httr)
library(xml2)
library(data.table)
library(ggplot2)
library(ggthemes)
#library(ggmap)
#library(leaflet)
#library(RColorBrewer)






## FUNCTIONS

fetch_MoviePage_itunes    <- function(url, MovieName){
  tmpURL <- read_html(url)
  actors <- html_nodes(x=tmpURL, css = ".movie-description, .plot-summary span") %>% html_nodes("[itemprop=actor]") %>% html_text()
  story <- html_nodes(x=tmpURL, css = ".movie-description, .plot-summary span") %>% html_nodes("[itemprop=description]") %>% html_text()
  directors <- html_nodes(x=tmpURL, css = ".movie-description, .plot-summary span") %>% html_nodes("[itemprop=director]") %>% html_text()
  rottomPCT <- html_nodes(x=tmpURL, css = ".movie-review .percent") %>%  html_text()
  rottomCNT <- html_nodes(x=tmpURL, css = ".movie-review .total-reviews") %>% html_text()
  rottomRVW <- html_nodes(x=tmpURL, css = ".movie-review .average-reviews") %>% html_text()
  price <- html_nodes(x=tmpURL, css = ".price") %>% html_nodes("[itemprop=price]")%>% html_text()
  genre <- html_nodes(x=tmpURL, css = ".genre") %>% html_nodes("[itemprop=genre]") %>% html_text()
  year <- html_nodes(x=tmpURL, css = ".release-date") %>% html_nodes("[itemprop=dateCreated]") %>% html_text()
  image <- html_nodes(x=tmpURL, css=".artwork") %>% html_nodes("img")%>% html_attr("src-swap-high-dpi")
  image <- image[1]
  if(length(rottomPCT)== 0L){rottomPCT <- rottomCNT <- rottomRVW <- ''}
  out <- data.table(MovieName = MovieName
                    , MovieImageURL = image
                    , MovieCast = paste(actors, collapse = ', ')
                    , MovieDirector = paste(directors, collapse = ', ')
                    , MovieStory = story
                    , Price = price
                    , Genre = paste(genre, collapse = ', ')
                    , Year = year
                    , rottomPCT = rottomPCT
                    , rottomCNT = rottomCNT
                    , rottomRVW = rottomRVW
  )
  return(out)
}






## iTunes Film Layout
## Main page
url <- read_html("https://www.apple.com/itunes/charts/movies/")
selector_name <- ".section-content"

fnames <- html_nodes(x = url, css = selector_name) %>% html_nodes("h3")  %>% html_text()
furls  <- html_nodes(x = url, css = selector_name) %>% html_nodes("h3")  %>% html_nodes("a")  %>% html_attr("href")
#html_nodes(x = url, css = selector_name) %>% html_nodes("strong")  %>% html_text()
#dt.itunesMovies <- data.table(MovieName = fnames
#                              , MovieURL = furls)

for(i in 1:length(fnames)){
  outMovie <- fetch_MoviePage_itunes(furls[i], fnames[i])
  if(i==1){outMovies = outMovie}else{outMovies = rbind(outMovies, outMovie)}
}

View(outMovies)

write.csv(outMovies, file='~/Dropbox/ilab2/Project_Searching/surveyWebsite/iturnsTop100.csv')

outMoviesiTunes   <- data.table(read.csv(file='~/Dropbox/ilab2/Project_Searching/surveyWebsite/iturnsTop100.csv'))

summary(outMovies$Price)
#outMovies[, Price := as.double(substring(Price, 2))]
#ggplot(data=outMovies, aes(x = Price)) + geom_histogram()
#levels(outMovies$Price) <- c("$4.99 ", "$6.99 ","$7.99 ","$9.99 ","$12.99 ","$14.99 ", "$17.99 ", "$19.99 ")
ggplot(data=outMovies, aes(x = Price)) + geom_bar()



