library(dplyr)
library(rvest)
library(httr)
library(xml2)
library(data.table)
library(ggplot2)
library(ggthemes)
#library(ggmap)

## AMAZON MOVIES

outMoviesiTunes1   <- data.table(read.csv(file='~/Dropbox/ilab2/Project_Searching/surveyWebsite/iturnsTop100.csv'))
outMoviesiTunes2   <- data.table(read.csv(file='~/Dropbox/ilab2/Project_Searching/surveyWebsite/iturnsTop100Sep2017.csv'))
outMoviesPopAmazon1709 <- data.table(read.csv(file='~/Dropbox/ilab2/Project_Searching/surveyWebsite/amazonRentPopSelectionSep2017.csv'))
outMoviesPopAmazon1801 <- data.table(read.csv(file='~/Dropbox/ilab2/Project_Searching/surveyWebsite/amazonRentPopSelectionJan2018.csv'))
outMoviesAmazonOld <- data.table(read.csv(file='~/Dropbox/ilab2/Project_Searching/surveyWebsite/amazonRentPopSelection.csv'))
outMoviesSurveyOld <- data.table(read.csv( file='~/Dropbox/ilab2/Project_Searching/surveyWebsite/SurveyMovieSelectionPilot.csv'))


nrow(outMoviesPopAmazon)
nrow(outMoviesPopAmazon[MovieName %in% outMoviesiTunes2[, MovieName]])
nrow(outMoviesPopAmazon[MovieName %in% outMoviesAmazonOld[, MovieName]])
nrow(outMoviesPopAmazon1801[MovieName %in% outMoviesPopAmazon1709[, MovieName]])

names   <- intersect(names(outMoviesPopAmazon1709), names(outMoviesPopAmazon1801))

setcolorder(outMoviesPopAmazon1801, names)

outMoviesPopAmazon <- rbind(outMoviesPopAmazon1801, outMoviesPopAmazon1709[, names, with = F])

## filter movies for survey

outMoviesSurvey   <- base::merge(outMoviesPopAmazon
                                    , unique(rbind(outMoviesiTunes1[MovieTrailerURL!='',list(MovieName,MovieTrailerURL)]
                                            , outMoviesAmazonOld[MovieTrailerURL!='',list(MovieName,MovieTrailerURL)]
                                            , outMoviesSurveyOld[MovieTrailerURL!='',list(MovieName,MovieTrailerURL)]
                                    ))
                                    , by = 'MovieName'
                                    , all.x = T)
outMoviesSurvey[, GenreDrama := grepl('Drama', Genre)]
outMoviesSurvey[, GenreComedy := grepl('Comedy', Genre)]
outMoviesSurvey[, GenreAction := grepl('Action', Genre)]
outMoviesSurvey[, GenreFamily := grepl('Family', Genre)]
outMoviesSurvey   <- unique(outMoviesSurvey)
#tmp <- copy(outMoviesPopAmazon)
outMoviesSurvey  <- outMoviesSurvey[GenreAction==T | GenreDrama==T | GenreComedy==T | GenreFamily==T]

#outMoviesPopAmazon  <- base::merge(outMoviesPopAmazon, tmp[,list(MovieName,Runtime)]
#                                   , by = 'MovieName', all.x = T)

outMoviesSurvey  <- outMoviesSurvey[!is.na(Year)]
outMoviesSurvey  <- outMoviesSurvey[!is.na(ratingIMDB)]
outMoviesSurvey  <- outMoviesSurvey[!is.na(MovieCast)]
outMoviesSurvey  <- outMoviesSurvey[!is.na(MovieStory)]
outMoviesSurvey  <- outMoviesSurvey[!is.na(Runtime)]
setnames(outMoviesSurvey, 'ratingIMDB', 'IMDBRating')

write.csv(outMoviesSurvey, file='~/Dropbox/ilab2/Project_Searching/surveyWebsite/SurveyMovieSelectionJan2018.csv')
write.csv(outMoviesSurvey, file='~/Dropbox/ilab2/Project_Searching/surveyWebsite/SurveyMovieSelectionOct2017.csv')
write.csv(outMoviesPopAmazon, file='~/Dropbox/ilab2/Project_Searching/surveyWebsite/amazonRentPopSelection.csv')



## FINALIZE GENRE SELECTION

outMoviesSurvey   <- data.table(read.csv(file='~/Dropbox/ilab2/Project_Searching/surveyWebsite/SurveyMovieSelectionOct2017.csv'))
set.seed(1)
pool.selectedDrama   <- sample(outMoviesSurvey[GenreDrama == T, as.character(MovieName)], 12)
pool.selected        <- c(pool.selectedDrama)
pool.selectedAction  <- sample(setdiff(outMoviesSurvey[GenreAction == T, as.character(MovieName)], pool.selected), 12)
pool.selected        <- c(pool.selectedDrama,pool.selectedAction)
pool.selectedComedy  <- sample(setdiff(outMoviesSurvey[GenreComedy == T & GenreFamily == F, as.character(MovieName)],pool.selected), 12)
pool.selected        <- c(pool.selectedDrama,pool.selectedComedy)
pool.selectedFamily  <- sample(setdiff(outMoviesSurvey[GenreFamily == T, as.character(MovieName)],pool.selected), 12)
pool.selectedDrama
pool.selectedAction
pool.selectedComedy
pool.selectedFamily

outMoviesSurvey[, selectedDrama := ifelse(MovieName %in% pool.selectedDrama, T, F)]
outMoviesSurvey[, selectedAction := ifelse(MovieName %in% pool.selectedAction, T, F)]
outMoviesSurvey[, selectedComedy := ifelse(MovieName %in% pool.selectedComedy, T, F)]
outMoviesSurvey[, selectedFamily := ifelse(MovieName %in% pool.selectedFamily, T, F)]
write.csv(outMoviesSurvey, file='~/Dropbox/ilab2/Project_Searching/surveyWebsite/SurveyMovieSelectionPilot.csv')





