##########################################################################################################
### Import the player statistical data from the fitzRoy package

# load the library
library(fitzRoy)
library(plyr)
library(dplyr)

# fetch the player data
s2022 <- fetch_player_stats_fryzigg(season = 2022)
s2023 <- fetch_player_stats_fryzigg(season = 2023)

# join the data sets
df1 <- rbind(s2022,s2023)

##########################################################################################################

##########################################################################################################
### Data Pre-Processing
# explore the data
str(df1)

#Remove columns that aren’t associated with the target “disposals” column and deal with match round values.

#Remove unnecessary columns with respect to ‘disposals’
df1 <- subset(df1, select = -c(player_height_cm, player_weight_kg, subbed, supercoach_score,
                               player_is_retired, date))


#Deal with match round values
# match_round is a character and contains finals as well as numeric rounds.
# we will convert them all to numeric.
df1$match_round <- case_when (
  df1$match_round == 'Finals Week 1' ~ 24,
  df1$match_round == 'Semi Finals' ~ 25,
  df1$match_round == 'Preliminary Finals' ~ 25,
  df1$match_round == 'Grand Final' ~ 26,
  TRUE ~ as.numeric(as.factor(df1$match_round)))

#Add and update other columns
#add a column for the year (season)
df1$season <- as.numeric(format(as.Date(df1$match_date, format="%Y-%m-%d"),"%Y"))

# update venue names for the current season
df1$venue_name[df1$venue_name=="Metricon Stadium"] <- "Heritage Bank Stadium"
df1$venue_name[df1$venue_name=="UNSW Canberra Oval"] <- "Manuka Oval"

#check missing values
sapply(df1, function(x) sum(is.na(x)))

######################################################################################

##########################################################################################################
### explore and visualize the data

# overall disposal distribution

library(ggplot2)

ggplot(data = df1, aes(x = disposals, fill = ..count..)) +
  geom_histogram(bins = 30) +
  scale_x_continuous(name = 'no. disposals', breaks = seq(0, 50, 10), limits=c(0, 50)) +
  scale_y_continuous(name = "Count", limits = c(0,2500), expand = c(0,0)) +
  ggtitle('Overall Distribution of Player Disposals') +
  scale_fill_gradient("Count", low = "lightblue", high = "darkblue") +
  theme(legend.position = 'right',
        panel.background = element_rect(fill = 'lightgray'),
        plot.title = element_text(size = 8, face = "bold", hjust = 0.5),
        legend.title=element_text(size=8), 
        legend.text=element_text(size=8),
        axis.text = element_text(size = 7),
        axis.title = element_text(size = 8))


# create position groups
df1$player_group <- case_when (
  df1$player_position == 'FPR' ~ 'Forward',
  df1$player_position == 'FB' ~ 'Defence',
  df1$player_position == 'RK' ~ 'Centre',
  df1$player_position == 'CHF' ~ 'Forward',
  df1$player_position == 'CHB' ~ 'Defence',
  df1$player_position == 'HFFR' ~ 'Forward',
  df1$player_position == 'WR' ~ 'Centre',
  df1$player_position == 'R' ~ 'Centre',
  df1$player_position == 'RR' ~ 'Centre',
  df1$player_position == 'HBFL' ~ 'Defence',
  df1$player_position == 'FF' ~ 'Forward',
  df1$player_position == 'HBFR' ~ 'Defence',
  df1$player_position == 'WL' ~ 'Centre',
  df1$player_position == 'C' ~ 'Centre',
  df1$player_position == 'BPR' ~ 'Defence',
  df1$player_position == 'BPL' ~ 'Defence',
  df1$player_position == 'FPL' ~ 'Forward',
  df1$player_position == 'HFFL' ~ 'Forward',
  TRUE ~ 'Other')

#create plot by group
df1 <- df1[!(df1$player_group == 'Other'),]

ggplot(df1, aes(x=disposals, fill = player_group)) +
  geom_histogram(position="identity", bins = 20) +
  scale_fill_manual(values = c('darkblue', 'brown', 'darkgreen')) +
  labs(fill = "Player Group")

##########################################################################################################

### perform feature importance with boruta

# remove the match data and focus on player data for feature importance
# we'll keep match_id for use later
df1 <- df1[  -c(1,3:18,20:22)]

library(Boruta)

# for reproducibility
set.seed(111)

# get importance using boruta
boruta.df1_train <- Boruta(disposals~., data = df1, doTrace = 2) # only attribute rejected - opponent

# print the results
print(boruta.df1_train)

#Boruta performed 68 iterations in 1.567528 hours.
#52 attributes confirmed important: afl_fantasy_score, behinds, bounces,
#centre_clearances, clangers and 47 more;
#3 attributes confirmed unimportant: brownlow_votes, match_id, season;

# plot the results
#https://stackoverflow.com/questions/47342553/boruta-box-plots-in-r

# generateCol is needed by plot.Boruta
generateCol<-function(x,colCode,col,numShadow){
  #Checking arguments
  if(is.null(col) & length(colCode)!=4)
    stop('colCode should have 4 elements.');
  #Generating col
  if(is.null(col)){
    rep(colCode[4],length(x$finalDecision)+numShadow)->cc;
    cc[c(x$finalDecision=='Confirmed',rep(FALSE,numShadow))]<-colCode[1];
    cc[c(x$finalDecision=='Tentative',rep(FALSE,numShadow))]<-colCode[2];
    cc[c(x$finalDecision=='Rejected',rep(FALSE,numShadow))]<-colCode[3];
    col=cc;
  }
  return(col);
}

# Modified plot.Boruta
plot.Boruta.sel <- function(
    x,
    pars = NULL,
    colCode = c('green','yellow','red','blue'),
    sort = TRUE,
    whichShadow = c(TRUE, TRUE, TRUE),
    col = NULL, xlab = 'Attributes', ylab = 'Importance', ...) {
  
  #Checking arguments
  if(class(x)!='Boruta')
    stop('This function needs Boruta object as an argument.');
  if(is.null(x$ImpHistory))
    stop('Importance history was not stored during the Boruta run.');
  
  #Removal of -Infs and conversion to a list
  lz <- lapply(1:ncol(x$ImpHistory), function(i)
    x$ImpHistory[is.finite(x$ImpHistory[,i]),i]);
  colnames(x$ImpHistory)->names(lz);
  
  #Selection of shadow meta-attributes
  numShadow <- sum(whichShadow);
  lz <- lz[c(rep(TRUE,length(x$finalDecision)), whichShadow)];
  
  #Generating color vector
  col <- generateCol(x, colCode, col, numShadow);
  
  #Ordering boxes due to attribute median importance
  if (sort) {
    ii <- order(sapply(lz, stats::median));
    lz <- lz[ii];
    col <- col[ii];
  }
  
  # Select parameters of interest
  if (!is.null(pars)) lz <- lz[names(lz) %in% pars];
  
  #Final plotting
  graphics::boxplot(lz, xlab = xlab, ylab = ylab, col = 'green', cex.lab = 0.5, cex.axis=0.5);
  invisible(x);
}

plot.Boruta.sel(boruta.df1_train, pars = c('disposal_efficiency_percentage', 'handballs', 'uncontested_possessions', 
                                           'kicks', 'effective_disposals', 'contested_possessions', 'ground_ball_gets',
                                           'afl_fantasy_score'))


# put results into df
df1_boruta <- attStats(boruta.df1_train)

# we will take those variables with meanImp >= 20
df_final_output <- subset(df1_boruta, df1_boruta$meanImp >= 20)

######################################################################################

df1 <- df1[, c('disposals', 'player_id', 'season', 'match_id', 'disposal_efficiency_percentage', 'handballs', 'uncontested_possessions', 
               'kicks', 'effective_disposals', 'contested_possessions', 'ground_ball_gets', 'afl_fantasy_score')]

# create features

vars <- c('disposal_efficiency_percentage', 'handballs', 'uncontested_possessions', 'kicks', 'effective_disposals',
          'contested_possessions', 'ground_ball_gets', 'afl_fantasy_score')

###Group 1 - Averages of each variable
df1 <- df1 %>%
  arrange(player_id, season, match_id) %>%
  group_by(player_id, season) %>%
  mutate(rec = 1) %>%
  mutate(cum_rec = cumsum(rec), across(all_of(vars), ~cumsum(.x)/cum_rec, .names = 'avg_{.col}')) %>%
  ungroup()


######################################################################################
#### first attempt at training model, using 10,000 trees and 5 cross fold validation

library(gbm)
library(xgboost)

# take only relevant cols
df1 <- df1[, c(1,3,15:22)]

# split train and test
train <- subset(df1, df1$season == '2022')
test <- subset(df1, df1$season == '2023')

# remove season
train <- train[ ,-2]
test <- test[ ,-2]

# for reproducibility
set.seed(123)

# train GBM model
gbm.fit <- gbm(
  formula = disposals ~ .,
  distribution = "gaussian",
  data = train,
  n.trees = 10000,
  interaction.depth = 1,
  shrinkage = 0.001,
  cv.folds = 5,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
)  

# print results
print(gbm.fit)

#gbm(formula = disposals ~ ., distribution = "gaussian", data = train, 
#    n.trees = 10000, interaction.depth = 1, shrinkage = 0.001, 
#    cv.folds = 5, verbose = FALSE, n.cores = NULL)
#A gradient boosted model with gaussian loss function.
#10000 iterations were performed.
#The best cross-validation iteration was 10000.
#There were 8 predictors of which 8 had non-zero influence

# Here, we see that the minimum CV RMSE is 4.41 (this means on average our model is about 4.4 disposals from the actual amount of disposals
# but the plot also illustrates that the CV error is still decreasing at 10,000 trees.

# get MSE and compute RMSE
sqrt(min(gbm.fit$cv.error))
# [1] 4.412487

# plot loss function as a result of n trees added to the ensemble
gbm.perf(gbm.fit, method = "cv")
# [1] 10000


# second attempt at training model, using 5,000 trees and interaction.depth = 3

# for reproducibility
set.seed(123)

# train GBM model
gbm.fit2 <- gbm(
  formula = disposals ~ .,
  distribution = "gaussian",
  data = train,
  n.trees = 5000,
  interaction.depth = 3,
  shrinkage = 0.1,
  cv.folds = 5,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
) 

# print results
print(gbm.fit2)

#gbm(formula = disposals ~ ., distribution = "gaussian", data = train, 
#    n.trees = 5000, interaction.depth = 3, shrinkage = 0.1, cv.folds = 5, 
#    verbose = FALSE, n.cores = NULL)
#A gradient boosted model with gaussian loss function.
#5000 iterations were performed.
#The best cross-validation iteration was 89.
#There were 8 predictors of which 8 had non-zero influence

#This model achieves a slightly improved RMSE than our initial model with only 89 trees.

# find index for n trees with minimum CV error
min_MSE <- which.min(gbm.fit2$cv.error)

# get MSE and compute RMSE
sqrt(gbm.fit2$cv.error[min_MSE])
## [1] 4.397025

# plot loss function as a result of n trees added to the ensemble
gbm.perf(gbm.fit2, method = "cv")
# [1] 89


# third attempt at training model using grid search

# perform grid search to iterate over every combination of hyper parameter values
# which allows us to assess which combination tends to perform well

# create hyperparameter grid
hyper_grid <- expand.grid(
  shrinkage = c(.01, .1, .3),
  interaction.depth = c(1, 3, 5),
  n.minobsinnode = c(5, 10, 15),
  bag.fraction = c(.65, .8, 1), 
  optimal_trees = 0,               # a place to dump results
  min_RMSE = 0                     # a place to dump results
)

# total number of combinations
nrow(hyper_grid)
## [1] 81

########################################################

# randomize data
random_index <- sample(1:nrow(train), nrow(train))
random_data_train <- train[random_index, ]

# grid search 
for(i in 1:nrow(hyper_grid)) {
  
  # reproducibility
  set.seed(123)
  
  # train model - duration xx mins 11.50am
  gbm.tune <- gbm(
    formula = disposals ~ .,
    distribution = "gaussian",
    data = random_data_train,
    n.trees = 5000,
    interaction.depth = hyper_grid$interaction.depth[i],
    shrinkage = hyper_grid$shrinkage[i],
    n.minobsinnode = hyper_grid$n.minobsinnode[i],
    bag.fraction = hyper_grid$bag.fraction[i],
    train.fraction = .75,
    n.cores = NULL, # will use all cores by default
    verbose = FALSE
  )
  
  # add min training error and trees to grid
  hyper_grid$optimal_trees[i] <- which.min(gbm.tune$valid.error)
  hyper_grid$min_RMSE[i] <- sqrt(min(gbm.tune$valid.error))
}

hyper_grid %>% 
  dplyr::arrange(min_RMSE) %>%
  head(10)

#shrinkage interaction.depth n.minobsinnode bag.fraction optimal_trees min_RMSE
#1       0.01                 3             10          0.8          1101 4.341233
#2       0.01                 3             15          1.0          1165 4.341327
#3       0.01                 3              5          1.0          1235 4.341514
#4       0.01                 3             10          1.0          1449 4.341835
#5       0.01                 3              5          0.8          1077 4.341933
#6       0.10                 3             10          1.0           156 4.342466
#7       0.01                 3             15          0.8           874 4.342751
#8       0.10                 3              5          1.0           161 4.342898
#9       0.10                 3              5          0.8           114 4.343888
#10      0.10                 3             10          0.8           106 4.344947

#These results provide a guidance in looking at specific parametera that we can refine to improve our overall RMSE.
#Let’s adjust our grid and and refine it to look at closer parameters that appear to produce the best results in our previous grid search.

# modify hyperparameter grid
hyper_grid <- expand.grid(
  shrinkage = c(.01, .05, .1),
  interaction.depth = 3,
  n.minobsinnode = c(5, 10, 15),
  bag.fraction = c(.8, 1), 
  optimal_trees = 0,               # a place to dump results
  min_RMSE = 0                     # a place to dump results
)

# grid search 
for(i in 1:nrow(hyper_grid)) {
  
  # reproducibility
  set.seed(123)
  
  # train model - duration xx mins 11.50am
  gbm.tune <- gbm(
    formula = disposals ~ .,
    distribution = "gaussian",
    data = random_data_train,
    n.trees = 5000,
    interaction.depth = hyper_grid$interaction.depth[i],
    shrinkage = hyper_grid$shrinkage[i],
    n.minobsinnode = hyper_grid$n.minobsinnode[i],
    bag.fraction = hyper_grid$bag.fraction[i],
    train.fraction = .75,
    n.cores = NULL, # will use all cores by default
    verbose = FALSE
  )
  
  # add min training error and trees to grid
  hyper_grid$optimal_trees[i] <- which.min(gbm.tune$valid.error)
  hyper_grid$min_RMSE[i] <- sqrt(min(gbm.tune$valid.error))
}

hyper_grid %>% 
  dplyr::arrange(min_RMSE) %>%
  head(10)

# the results are similar to before, with the best model producing a slightly better result.

########################################################

# final model with optimal parameters
# Once we have found our top model we train a model with those specific parameters.
# As the model converged at 223 trees I train a cross validated model (to provide a more robust error estimate) with 1000 trees.

# for reproducibility
set.seed(123)

# train GBM model
gbm.fit.final <- gbm(
  formula = disposals ~ .,
  distribution = "gaussian",
  data = train,
  n.trees = 1000,
  interaction.depth = 3,
  shrinkage = 0.05,
  n.minobsinnode = 5,
  bag.fraction = 0.8, 
  train.fraction = 1,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
) 

par(mar=c(5,12,4,1)+.1)
summary(
  gbm.fit.final, 
  cBars = 10,
  method = relative.influence, # also can use permutation.test.gbm
  las = 2,
  cex.lab = 0.7, cex.names = 0.7, cex.axis = 0.7,
)

# reset to default
par(mgp=c(3,1,0))

###############################################################################
#Predicting
#Once we have produced our final model, we use it to predict on new observations.
# To do this, we use the predict function; however, we also need to supply the number of trees to use
#(see ?predict.gbm for details). We see that our RMSE for our test set is very close to the RMSE we obtained on our best gbm model.

# predict values for test data
pred <- predict(gbm.fit.final, n.trees = gbm.fit.final$n.trees, test)

# results
caret::RMSE(pred, test$disposals)
## [1] 4.345416


