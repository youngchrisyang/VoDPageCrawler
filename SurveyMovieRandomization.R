library(dplyr)
library(rvest)
library(httr)
library(xml2)
library(data.table)
library(ggplot2)
library(ggthemes)
library(FNN)
library(e1071)
library(corrplot)
#library(ggmap)

#outMoviesSurvey  <- data.table(read.csv(file='~/Dropbox/ilab2/Project_Searching/surveyWebsite/SurveyMovieSelectionPilot.csv'))


outMoviesSurvey   <- data.table(read.csv(file='~/Dropbox/ilab2/Project_Searching/surveyWebsite/SurveyMovieSelectionOct2017.csv'))
outMoviesSurvey   <- data.table(read.csv(file='~/Dropbox/ilab2/Project_Searching/surveyWebsite/SurveyMovieSelectionJan2018.csv'))

outMovies.reduced <- outMoviesSurvey[, list(MovieName, price = as.double(price)
                                            , IMDBRating = as.double(IMDBRating), GenreDrama, GenreAction, GenreComedy, GenreFamily)]

nn  <- get.knn(outMovies.reduced[, 2:7, with=F], k=1, algorithm="kd_tree")

outMovies.reduced[, nn_index := nn$nn.index]
outMovies.reduced[, nn_dist := nn$nn.dist]
#outMovies.reduced  <- outMovies.reduced[nn_dist<1]

dist_cutoff        <- 0.7
set.seed(15217)
names.used         <- c()



pool.selectedDrama <- c()
tmp.selected       <- 0
while(tmp.selected < 6){
  tmpname            <- sample(setdiff(outMovies.reduced[GenreDrama==T & GenreFamily==F, as.character(MovieName)], names.used), 1)
  if(outMovies.reduced[MovieName==tmpname, nn_dist] <= dist_cutoff){
    tmppairname        <- outMovies.reduced[outMovies.reduced[MovieName==tmpname, nn_index], as.character(MovieName)]
    if((!tmppairname %in% names.used) & tmppairname!= tmpname){
      pool.selectedDrama <- c(pool.selectedDrama, c(tmpname, tmppairname))
      names.used         <- c(names.used, c(tmpname, tmppairname))
      tmp.selected       <- tmp.selected + 1
    }
  }
} 


pool.selectedAction <- c()
tmp.selected       <- 0
while(tmp.selected < 6){
  tmpname            <- sample(setdiff(outMovies.reduced[GenreAction==T & GenreFamily==F, as.character(MovieName)], names.used), 1)
  tmppairname        <- outMovies.reduced[outMovies.reduced[MovieName==tmpname, nn_index], as.character(MovieName)]
  if((!tmppairname %in% names.used) & tmppairname!= tmpname){
    pool.selectedAction <- c(pool.selectedAction, c(tmpname, tmppairname))
    names.used         <- c(names.used, c(tmpname, tmppairname))
    tmp.selected       <- tmp.selected + 1
  }
} 


pool.selectedComedy <- c()
tmp.selected       <- 0
while(tmp.selected < 6){
  tmpname            <- sample(setdiff(outMovies.reduced[GenreComedy==T & GenreFamily==F, as.character(MovieName)], names.used), 1)
  tmppairname        <- outMovies.reduced[outMovies.reduced[MovieName==tmpname, nn_index], as.character(MovieName)]
  if((!tmppairname %in% names.used) & tmppairname!= tmpname){
    pool.selectedComedy <- c(pool.selectedComedy, c(tmpname, tmppairname))
    names.used         <- c(names.used, c(tmpname, tmppairname))
    tmp.selected       <- tmp.selected + 1
  }
} 



pool.selectedFamily <- c()
tmp.selected       <- 0
while(tmp.selected < 6){
  tmpname            <- sample(setdiff(outMovies.reduced[GenreFamily==T, as.character(MovieName)], names.used), 1)
  tmppairname        <- outMovies.reduced[outMovies.reduced[MovieName==tmpname, nn_index], as.character(MovieName)]
  if((!tmppairname %in% names.used) & tmppairname!= tmpname){
    pool.selectedFamily <- c(pool.selectedFamily, c(tmpname, tmppairname))
    names.used         <- c(names.used, c(tmpname, tmppairname))
    tmp.selected       <- tmp.selected + 1
  }
} 

pool.selectedDrama
pool.selectedAction
pool.selectedComedy
pool.selectedFamily


dt.block            <- data.table(MovieName = c(pool.selectedDrama
                                                ,pool.selectedAction
                                                ,pool.selectedComedy
                                                ,pool.selectedFamily)
                                  , GenreSelected = rep(c('Drama', 'Action', 'Comedy', 'Family')
                                                        , each = 12)
                                  , BlockNum = rep(1:24, each = 2)
                                  )

outMoviesSurvey     <- outMoviesSurvey[MovieName %in% dt.block[, MovieName]]
outMoviesSurvey     <- base::merge(dt.block, outMoviesSurvey, by = 'MovieName', all.x = T)



## generate versions
n_versions         <- 5
outMoviesSurvey    <- outMoviesSurvey[rep(seq(1, nrow(outMoviesSurvey)), n_versions)]
outMoviesSurvey[, VersionNum := rep(1:n_versions, each = nrow(outMoviesSurvey)/5)]


set.seed(111)
outMoviesSurvey[, Likes_Group := sample(c('H', 'L'), 2), by = list(BlockNum, VersionNum)]
outMoviesSurvey[, Frd_Group := sample(c('H', 'L'), 2), by = list(BlockNum, VersionNum)]
outMoviesSurvey[, Pos_Group := sample(c('T', 'B'), 2), by = list(BlockNum, VersionNum)]
outMoviesSurvey[, Price_Group := sample(c('H', 'L'), 2), by = list(BlockNum, VersionNum)]

## randomize likes based on levels
outMoviesSurvey[Likes_Group=='H', n_likes := round(rlnorm(nrow(outMoviesSurvey)/2, 6, 2))]
outMoviesSurvey[Likes_Group=='L', n_likes := round(rlnorm(nrow(outMoviesSurvey)/2, 4, 2))]

highFrdPool         <- c(0,1,2,3)
lowFrdPool          <- c(0,0,0,1)
outMoviesSurvey[Frd_Group=='H', n_frd := sample(highFrdPool, nrow(outMoviesSurvey)/2, replace = T)]
outMoviesSurvey[Frd_Group=='L', n_frd := sample(lowFrdPool, nrow(outMoviesSurvey)/2, replace = T)]


## randomize prices based on levels
highPricesPool      <- 10:14*0.5-0.01
lowPricesPool       <- 5:9*0.5-0.01
outMoviesSurvey[Price_Group=='H', Price := sample(highPricesPool, nrow(outMoviesSurvey)/2, replace = T)]
outMoviesSurvey[Price_Group=='L', Price := sample(lowPricesPool, nrow(outMoviesSurvey)/2, replace = T)]

## order of menus in versions
set.seed(1)
menues               <- c('Drama', 'Action', 'Comedy', 'Family')
for(i in 1:n_versions){
  tmp   <- sample(menues, length(menues), replace = F)
  outMoviesSurvey[VersionNum==i, menu_order := match(GenreSelected, tmp)]
}

## randomize horizontal order
set.seed(1111)
outMoviesSurvey      <- outMoviesSurvey[order(VersionNum, GenreSelected, BlockNum)]
outMoviesSurvey[, hor_order := rep(sample(1:6), each=2), by = list(VersionNum, GenreSelected)]
outMoviesSurvey[, index := (menu_order-1)*12 + as.double(Pos_Group=='B')*6 + hor_order]



summary(lm(data = outMoviesSurvey, log(n_likes+1)~IMDBRating))
summary(lm(data = outMoviesSurvey, log(n_likes+1)~Price))
summary(lm(data = outMoviesSurvey, log(n_likes+1)~hor_order))
summary(lm(data = outMoviesSurvey, log(n_likes+1)~n_frd))
summary(lm(data = outMoviesSurvey, n_frd~IMDBRating))
summary(lm(data = outMoviesSurvey, n_frd~Price))
summary(lm(data = outMoviesSurvey, n_frd~hor_order))

M <-cor(outMoviesSurvey[, list(n_likes, n_frd, hor_order, offered_price = Price, IMDBRating, AmazonPrice = price)])
corrplot(M, method="number")


write.csv(unique(outMoviesSurvey[, list(MovieName, MovieTrailerURL)]), file='~/Dropbox/ilab2/Project_Searching/surveyWebsite/SurveyMovieSelectionRandomizedPilotJan2018_Trailer2Impute.csv')
tmp   <- data.table(read.csv(file='~/Dropbox/ilab2/Project_Searching/surveyWebsite/SurveyMovieSelectionRandomizedPilotJan2018_Trailer2Impute.csv'))
outMoviesSurvey[, MovieTrailerURL := tmp[match(outMoviesSurvey$MovieName, MovieName), MovieTrailerURL]]

write.csv(outMoviesSurvey, file='~/Dropbox/ilab2/Project_Searching/surveyWebsite/SurveyMovieSelectionRandomizedPilotJan2018.csv')

write.csv(outMoviesSurvey, file='~/Dropbox/ilab2/Project_Searching/surveyWebsite/SurveyMovieSelectionRandomizedPilot.csv')

outMoviesSurvey   <- data.table(read.csv(file='~/Dropbox/ilab2/Project_Searching/surveyWebsite/SurveyMovieSelectionRandomizedPilot.csv'))



### INFORMATION RANDOMIZATION

outMoviesSurvey  <- data.table(read.csv(file='~/Dropbox/ilab2/Project_Searching/surveyWebsite/SurveyMovieSelectionPilot.csv'))

n_versions   <- 5
outMoviesSurvey  <- outMoviesSurvey[rep(seq(1, nrow(outMoviesSurvey)), n_versions)]

set.seed(1)
outMoviesSurvey[, n_likes := round(rlnorm(nrow(outMoviesSurvey), 6, 2))]
summary(lm(data= outMoviesSurvey, log(n_likes) ~ as.double(IMDBRating) + as.double(price)))









### SANDBOX ###



# K-Means Cluster Analysis
fit <- kmeans(outMovies.reduced[, 2:7, with=F], 25) # tune the cluster numbers 
# get cluster means 
aggregate(outMovies.reduced[, 2:7, with=F],by=list(fit$cluster),FUN=mean)
# append cluster assignment
outMovies.reduced <- data.table(outMovies.reduced, cl = fit$cluster)
View(outMovies.reduced[order(cl)])
outMovies.reduced[,.N, by = cl][N>1, unique(cl)]

outMovies.reduced <- outMovies.reduced[cl %in% outMovies.reduced[,.N, by = cl][N>1, unique(cl)]]


