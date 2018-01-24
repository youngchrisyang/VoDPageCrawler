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


## FUNCTION



fetch_MoviePage_amazon    <- function(url, MovieName){
  #tmpURL <- read_html(url)
  #res <- GET(url,query=list(r_date="2017-04-17",meeting_id=18))
  res <- GET(url)
  #content(res, "text", encoding = "ISO-8859-1")
  tmpURL <- read_html(content(res, as="text", encoding = "UTF-8"))
  title  <- tmpURL %>% html_nodes(".title-content-cascade") %>% html_nodes("[id=aiv-content-title]") %>%  html_text()   
  title  <- strsplit(title,split='\n', fixed=TRUE)[[1]][2]
  year   <- tmpURL %>% html_nodes(".title-content-cascade") %>% html_nodes("[id=aiv-content-title]")%>% html_nodes("span") %>%  html_text()   
  #certif <- tmpURL %>% html_nodes(".dv-badge-dark") %>%   html_text()   
  #certif <- certif[1]
  image <- tmpURL %>% html_nodes(".dp-meta-icon-container") %>% html_nodes("img")%>% html_attr("src")
  ratingAmazonStars <- tmpURL %>% html_nodes(".dp-rating-group i span") %>%  html_text()  
  ratingAmazonCNT   <- tmpURL %>% html_nodes(xpath='//*[@id="reviewLink"]/a')  %>%  html_text()   
  imdbRating <- tmpURL %>% html_nodes(".imdb-rating strong") %>%  html_text()   

  

  #result_0 > div > div > div > div.a-fixed-left-grid-col.a-col-right > div:nth-child(2) > div.a-column.a-span5.a-span-last > div:nth-child(4) > div > div.a-text-left.a-fixed-left-grid-col.a-col-right > span
  #result_0 > div > div > div > div.a-fixed-left-grid-col.a-col-right > div:nth-child(2) > div.a-column.a-span5.a-span-last > div.a-row.a-spacing-mini > span > span > a > i.a-icon.a-icon-star.a-star-4 > span
  
  story  <- tmpURL %>% html_nodes(".dv-info .dv-simple-synopsis p") %>%  html_text()   
  meta <- tmpURL %>% html_nodes(xpath='//*[@id="dv-center-features"]/div/div/table')%>% html_table()
  meta <- meta[[1]]
  meta <- t(meta)
  colnames(meta) <- meta[1,]
  meta <- data.table(meta)
  meta <- meta[2,]
  
  runtime <- tmpURL %>% html_nodes(xpath='//*[@id="aiv-main-content"]/div[7]/div/dl/dd[2]')%>% html_text()
  if(length(runtime)>0){
    runtime <- strsplit(runtime, '\n')[[1]][2]
  }
  
  price <- html_nodes(x=tmpURL, xpath='//*[@id="dv-action-box"]/div[1]/form/span/span/div') %>% html_text()
  
  out <- data.table(MovieName = MovieName
                    , MovieImageURL = image
                    , Genre = ifelse('Genres' %in% names(meta), meta[,Genres], NA)
                    , MovieDirector = ifelse('Director' %in% names(meta), meta[,Director], NA)
                    , MovieCast = ifelse('Starring' %in% names(meta), meta[,Starring], NA)
                    , Studio = ifelse('Studio' %in% names(meta), meta[,Studio], NA)
                    , ratingMPAA = ifelse('MPAA rating' %in% names(meta), meta[,`MPAA rating`], NA)
                    , ratingAmazonStars = ifelse(length(ratingAmazonStars)>0, ratingAmazonStars, NA)
                    , ratingAmazonCNT = ifelse(length(ratingAmazonCNT)>0, ratingAmazonCNT, NA)
                    , MovieStory = ifelse(length(story)>0, story, NA)
                    , priceAmazon = ifelse(length(price)>0, price, NA)
                    , ratingIMDB = ifelse(length(imdbRating)>0, imdbRating, NA)
                    , Runtime = ifelse(length(runtime)>0, runtime, NA)
  )
  return(out)
}




## Amazon Film Layout
## Main page
url <- read_html("https://www.amazon.com/s/ref=sr_pg_2?rh=n%3A7589478011%2Cp_n_feature_fourteen_browse-bin%3A2654454011%2Cp_n_ways_to_watch%3A12007867011%2Cp_85%3A0%2Cp_n_entity_type%3A14069184011&page=2&bbn=7589478011&ie=UTF8&qid=1497895393")

# movie names
#fnames <- html_nodes(x = url, css = ".sx-badge-text") %>%  html_text()
fnames <- url %>% html_nodes("ul") %>% html_nodes(".s-access-detail-page") %>%  html_text()

# movie page urls
#furls  <- html_nodes(x = url, css = ".s-access-detail-page") %>% html_attr("href")
furls  <- url %>% html_nodes("ul") %>% html_nodes(".s-access-detail-page") %>% html_attr("href")



for(i in 1:length(fnames)){
  outMovie <- fetch_MoviePage_amazon(furls[i], fnames[i])
  if(i==1){outMovies = outMovie}else{outMovies = rbind(outMovies, outMovie)}
}

View(outMovies)


summary(outMovies$priceAmazon)
#outMovies[, Price := as.double(substring(Price, 2))]
#ggplot(data=outMovies, aes(x = Price)) + geom_histogram()
#levels(outMovies$Price) <- c("$4.99 ", "$6.99 ","$7.99 ","$9.99 ","$12.99 ","$14.99 ", "$17.99 ", "$19.99 ")
ggplot(data=outMovies, aes(x = priceAmazon)) + geom_bar()









## Amazon Film Layout: Multiple Pages
# as of Oct 2017
pages   <- c('https://www.amazon.com/s/ref=sr_pg_1?rh=n%3A7589478011%2Cp_n_feature_fourteen_browse-bin%3A2654454011%2Cp_n_ways_to_watch%3A12007867011%2Cp_85%3A0%2Cp_n_entity_type%3A14069184011&bbn=7589478011&ie=UTF8&qid=1497897570'
             , 'https://www.amazon.com/s/ref=sr_pg_2?rh=n%3A7589478011%2Cp_n_feature_fourteen_browse-bin%3A2654454011%2Cp_n_ways_to_watch%3A12007867011%2Cp_85%3A0%2Cp_n_entity_type%3A14069184011&page=2&bbn=7589478011&ie=UTF8&qid=1497895393'
             ,'https://www.amazon.com/s/ref=sr_pg_3?rh=n%3A7589478011%2Cp_n_feature_fourteen_browse-bin%3A2654454011%2Cp_n_ways_to_watch%3A12007867011%2Cp_85%3A0%2Cp_n_entity_type%3A14069184011&page=3&bbn=7589478011&ie=UTF8&qid=1497895899'
             , 'https://www.amazon.com/s/ref=sr_pg_4?rh=n%3A7589478011%2Cp_n_feature_fourteen_browse-bin%3A2654454011%2Cp_n_ways_to_watch%3A12007867011%2Cp_85%3A0%2Cp_n_entity_type%3A14069184011&page=4&bbn=7589478011&ie=UTF8&qid=1497895915'
             , 'https://www.amazon.com/s/ref=sr_pg_5?rh=n%3A7589478011%2Cp_n_feature_fourteen_browse-bin%3A2654454011%2Cp_n_ways_to_watch%3A12007867011%2Cp_85%3A0%2Cp_n_entity_type%3A14069184011&page=5&bbn=7589478011&ie=UTF8&qid=1497895933'
             , 'https://www.amazon.com/s/ref=sr_pg_6?rh=n%3A7589478011%2Cp_n_feature_fourteen_browse-bin%3A2654454011%2Cp_n_ways_to_watch%3A12007867011%2Cp_85%3A0%2Cp_n_entity_type%3A14069184011&page=6&bbn=7589478011&ie=UTF8&qid=1497895949'
             , 'https://www.amazon.com/s/ref=sr_pg_7?rh=n%3A7589478011%2Cp_n_feature_fourteen_browse-bin%3A2654454011%2Cp_n_ways_to_watch%3A12007867011%2Cp_85%3A0%2Cp_n_entity_type%3A14069184011&page=7&bbn=7589478011&ie=UTF8&qid=1497895963')
#n_pages  <- 50


# as of Jan 2018
pages   <- c('https://www.amazon.com/s/ref=sr_pg_1?rh=n%3A13431540011%2Cp_n_ways_to_watch%3A12007867011%2Cp_85%3A0%2Cp_n_entity_type%3A14069184011&page=1&bbn=13431540011&ie=UTF8&qid=1515549975'
             , 'https://www.amazon.com/s/ref=sr_pg_2?rh=n%3A13431540011%2Cp_n_ways_to_watch%3A12007867011%2Cp_85%3A0%2Cp_n_entity_type%3A14069184011&page=2&bbn=13431540011&ie=UTF8&qid=1515534840'
             , 'https://www.amazon.com/s/ref=sr_pg_3?rh=n%3A13431540011%2Cp_n_ways_to_watch%3A12007867011%2Cp_85%3A0%2Cp_n_entity_type%3A14069184011&page=3&bbn=13431540011&ie=UTF8&qid=1515534986'
             , 'https://www.amazon.com/s/ref=sr_pg_4?rh=n%3A13431540011%2Cp_n_ways_to_watch%3A12007867011%2Cp_85%3A0%2Cp_n_entity_type%3A14069184011&page=4&bbn=13431540011&ie=UTF8&qid=1515535005'
             )





for(i in 1:length(pages)){
  print(i)
  url <- read_html(pages[i])
  # movie names
  #fnames <- html_nodes(x = url, css = ".sx-badge-text") %>%  html_text()
  fnames <- url %>% html_nodes("ul") %>% html_nodes(".s-access-detail-page") %>%  html_text()
  
  # movie page urls
  #furls  <- html_nodes(x = url, css = ".s-access-detail-page") %>% html_attr("href")
  furls  <- url %>% html_nodes("ul") %>% html_nodes(".s-access-detail-page") %>% html_attr("href")
  
  for(j in 1:length(fnames)){
    print(j)
    outMovie <- fetch_MoviePage_amazon(furls[j], fnames[j])
    if(j==1){outMovies <- outMovie}else{outMovies <- rbind(outMovies, outMovie)}
  }
  if(i==1){
    outMoviesPopAmazon <- outMovies
  }else{
    outMoviesPopAmazon <- rbind(outMoviesPopAmazon, outMovies)
  }
}

View(outMoviesPopAmazon)
outMoviesPopAmazon   <- outMoviesPopAmazon[grepl('Rent', priceAmazon),]
outMoviesPopAmazon[, price := substring(priceAmazon, 16)]

write.csv(outMoviesPopAmazon, file='~/Dropbox/ilab2/Project_Searching/surveyWebsite/amazonRentPopSelectionSep2017.csv')
write.csv(outMoviesPopAmazon, file='~/Dropbox/ilab2/Project_Searching/surveyWebsite/amazonRentPopSelectionJan2018.csv')



outMoviesiTunes      <- rbind(outMoviesiTunes1[,list(MovieName,Year)]
                              , outMoviesiTunes2[,list(MovieName,Year)]
                              , outMoviesiTunes3[,list(MovieName,Year)])
outMoviesPopAmazon   <- base::merge(outMoviesPopAmazon
                                    , outMoviesiTunes
                                    , by = 'MovieName'
                                    , all.x = T)

#tmp <- copy(outMoviesPopAmazon)

#write.csv(outMoviesPopAmazon, file='~/Dropbox/ilab2/Project_Searching/surveyWebsite/amazonRentPopSelection.csv')


#outMoviesPopAmazon  <- data.table(read.csv( file='~/Dropbox/ilab2/Project_Searching/surveyWebsite/amazonRentPopSelectionTrailerAdded.csv'))




#outMoviesPopAmazon  <- base::merge(outMoviesPopAmazon, tmp[,list(MovieName,Runtime)]
#                                   , by = 'MovieName', all.x = T)

outMoviesPopAmazon  <- outMoviesPopAmazon[!is.na(Year)]
outMoviesPopAmazon  <- outMoviesPopAmazon[!is.na(ratingIMDB)]
outMoviesPopAmazon  <- outMoviesPopAmazon[!is.na(MovieCast)]
outMoviesPopAmazon  <- outMoviesPopAmazon[!is.na(MovieStory)]
outMoviesPopAmazon  <- outMoviesPopAmazon[!is.na(Runtime)]
setnames(outMoviesPopAmazon, 'ratingIMDB', 'IMDBRating')



write.csv(outMoviesPopAmazon, file='~/Dropbox/ilab2/Project_Searching/surveyWebsite/amazonRentPopSelection.csv')




################################ SANDBOX #######################

## FUNCTIONS


fetch_MoviePage_amazon    <- function(url, MovieName){
  #tmpURL <- read_html(url)
  #res <- GET(url,query=list(r_date="2017-04-17",meeting_id=18))
  res <- GET(url)
  #content(res, "text", encoding = "ISO-8859-1")
  tmpURL <- read_html(content(res, as="text", encoding = "UTF-8"))
  title  <- tmpURL %>% html_nodes(".title-content-cascade") %>% html_nodes("[id=aiv-content-title]") %>%  html_text()   
  title  <- strsplit(title,split='\n', fixed=TRUE)[[1]][2]
  year   <- tmpURL %>% html_nodes(".title-content-cascade") %>% html_nodes("[id=aiv-content-title]")%>% html_nodes("span") %>%  html_text()   
  #certif <- tmpURL %>% html_nodes(".dv-badge-dark") %>%   html_text()   
  #certif <- certif[1]
  image <- tmpURL %>% html_nodes(".dp-meta-icon-container") %>% html_nodes("img")%>% html_attr("src")
  ratingAmazonStars <- tmpURL %>% html_nodes(".dp-rating-group i span") %>%  html_text()  
  ratingAmazonCNT   <- tmpURL %>% html_nodes(xpath='//*[@id="reviewLink"]/a')  %>%  html_text()   
  imdbRating <- tmpURL %>% html_nodes(".imdb-rating strong") %>%  html_text()   
  
  story  <- tmpURL %>% html_nodes(".dv-info .dv-simple-synopsis p") %>%  html_text()   
  meta <- tmpURL %>% html_nodes(xpath='//*[@id="dv-center-features"]/div/div/table')%>% html_table()
  meta <- meta[[1]]
  meta <- t(meta)
  colnames(meta) <- meta[1,]
  meta <- data.table(meta)
  meta <- meta[2,]
  
  #runtime <- tmpURL %>% html_nodes(xpath='//*[@id="aiv-main-content"]/div[7]/div/dl/dd[2]')%>% html_text()
  runtime <- tmpURL  %>% html_nodes("[id=aiv-main-content]")  %>% html_nodes("dl") %>% html_nodes("dd")
  if(length(runtime)>1){
    runtime <- runtime[[2]] %>% html_text()
  }else if(length(runtime)==1){
    runtime <- runtime[[1]] %>% html_text()
  }else{
    runtime <- 'NA'
  }
    
  if(!is.null(runtime)){
    if(length(runtime)>0){
      runtime <- strsplit(runtime, '\n')[[1]][2]
    }else{
      runtime <- 'NA'
    }
    pos = regexpr('[0-9]', runtime)[[1]]
    runtime <- substring(runtime, pos)
  }
  
  
  print(runtime)


  
  price <- html_nodes(x=tmpURL, xpath='//*[@id="dv-action-box"]/div[1]/form/span/span/div') %>% html_text()
  
  out <- data.table(MovieName = MovieName
                    , Year = ifelse(is.null(year), 'NA',year)
                    , MovieImageURL = image
                    , Genre = ifelse('Genres' %in% names(meta), meta[,Genres], NA)
                    , MovieDirector = ifelse('Director' %in% names(meta), meta[,Director], NA)
                    , MovieCast = ifelse('Starring' %in% names(meta), meta[,Starring], NA)
                    , Studio = ifelse('Studio' %in% names(meta), meta[,Studio], NA)
                    , ratingMPAA = ifelse('MPAA rating' %in% names(meta), meta[,`MPAA rating`], NA)
                    , ratingAmazonStars = ifelse(length(ratingAmazonStars)>0, ratingAmazonStars, NA)
                    , ratingAmazonCNT = ifelse(length(ratingAmazonCNT)>0, ratingAmazonCNT, NA)
                    , MovieStory = ifelse(length(story)>0, story, NA)
                    , priceAmazon = ifelse(length(price)>0, price, NA)
                    , ratingIMDB = ifelse(length(imdbRating)>0, imdbRating, NA)
                    , Runtime = ifelse(length(runtime)>0, runtime, NA)
  )
  return(out)
}






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




