
# Predicting Individual Player Disposals in Australian Rules Football (AFL) using Linear Regression Models

This project attempts to predict the number of disposals for each individual player in each game of the AFL using linear regression models based on past data with some analyses. 

## Prerequisites
- [fitzRoy](https://cran.r-project.org/web/packages/fitzRoy/index.html) Package in R  
- [Shiny](https://cran.r-project.org/web/packages/shiny/index.html) Package in R


## Workflow
- Import the player statistical data from the fitzRoy package  
- Pre-process the data by removing columns that aren’t associated with the target “disposals” column  
- Exploring and visualizing the data – distribution of number of disposals and by player position  
- Feature Engineering – extract features based on their importance  
- Build the prediction model – split data to train and test, build model by regression, select mean absolute error (MAE) as error metric

## Import Data
- Import the player statistical data from the fitzRoy package  
- Player stats will be imported season-by-season using the main player_stats fetch function.  
- These stats will then be bound and joined together to form our main dataset.

### Import the player statistical data from the [fitzRoy](https://cran.r-project.org/web/packages/fitzRoy/index.html) package

    # load required libraries
    library(fitzRoy)
    library(plyr)
    library(dplyr)

Fetch the player data:

    s2022 <- fetch_player_stats_fryzigg(season = 2022)
    s2023 <- fetch_player_stats_fryzigg(season = 2023)

Join the data sets:

    df1 <- rbind(s2022,s2023)  
## Data Pre-Processing

#### Explore the data
    str(df1)

    tibble [19,044 × 81] (S3: tbl_df/tbl/data.frame)
    $ venue_name                    : chr [1:19044] "MCG" "MCG" "MCG" "MCG" ...
    $ match_id                      : int [1:19044] 16117 16117 16117 16117 16117 16117 16117 16117 16117 16117 ...
    $ match_home_team               : chr [1:19044] "Melbourne" "Melbourne" "Melbourne" "Melbourne" ...
    $ match_away_team               : chr [1:19044] "Western Bulldogs" "Western Bulldogs" "Western Bulldogs" "Western Bulldogs" ...
    $ match_date                    : chr [1:19044] "2022-03-16" "2022-03-16" "2022-03-16" "2022-03-16" ...
    $ match_local_time              : chr [1:19044] "19:10:00" "19:10:00" "19:10:00" "19:10:00" ...
    $ match_attendance              : int [1:19044] 0 0 0 0 0 0 0 0 0 0 ...
    $ match_round                   : chr [1:19044] "1" "1" "1" "1" ...
    $ match_home_team_goals         : int [1:19044] 14 14 14 14 14 14 14 14 14 14 ...
    $ match_home_team_behinds       : int [1:19044] 13 13 13 13 13 13 13 13 13 13 ...
    $ match_home_team_score         : int [1:19044] 97 97 97 97 97 97 97 97 97 97 ...
    $ match_away_team_goals         : int [1:19044] 11 11 11 11 11 11 11 11 11 11 ...
    $ match_away_team_behinds       : int [1:19044] 5 5 5 5 5 5 5 5 5 5 ...
    $ match_away_team_score         : int [1:19044] 71 71 71 71 71 71 71 71 71 71 ...
    $ match_margin                  : int [1:19044] 26 26 26 26 26 26 26 26 26 26 ...
    $ match_winner                  : chr [1:19044] "Melbourne" "Melbourne" "Melbourne" "Melbourne" ...
    $ match_weather_temp_c          : int [1:19044] 27 27 27 27 27 27 27 27 27 27 ...
    $ match_weather_type            : chr [1:19044] "RAIN" "RAIN" "RAIN" "RAIN" ...
    $ player_id                     : int [1:19044] 11904 11945 11972 12015 12034 12058 12097 12152 12172 12210 ...
    $ player_first_name             : chr [1:19044] "Tom" "Steven" "Max" "Tom" ...
    $ player_last_name              : chr [1:19044] "Liberatore" "May" "Gawn" "McDonald" ...
    $ player_height_cm              : int [1:19044] 184 193 209 195 194 185 187 179 192 183 ...
    $ player_weight_kg              : int [1:19044] 85 101 110 101 95 84 84 93 89 82 ...
    $ player_is_retired             : logi [1:19044] FALSE FALSE FALSE FALSE FALSE FALSE ...
    $ player_team                   : chr [1:19044] "Western Bulldogs" "Melbourne" "Melbourne" "Melbourne" ...
    $ guernsey_number               : int [1:19044] 21 1 11 25 20 1 9 7 11 7 ...
    $ kicks                         : int [1:19044] 8 13 11 10 6 16 14 11 19 7 ...
    $ marks                         : int [1:19044] 4 7 4 5 4 5 12 5 5 4 ...
    $ handballs                     : int [1:19044] 6 1 3 2 2 10 5 12 20 6 ...
    $ disposals                     : int [1:19044] 14 14 14 12 8 26 19 23 39 13 ...
    $ effective_disposals           : int [1:19044] 11 11 8 9 6 18 17 16 31 8 ...
    $ disposal_efficiency_percentage: int [1:19044] 79 79 57 75 75 69 90 70 80 62 ...
    $ goals                         : int [1:19044] 0 0 1 0 0 2 1 1 0 0 ...
    $ behinds                       : int [1:19044] 0 0 0 1 0 0 0 0 0 0 ...
    $ hitouts                       : int [1:19044] 0 0 34 0 0 0 0 0 0 0 ...
    $ tackles                       : int [1:19044] 4 0 1 0 1 0 3 4 3 3 ...
    $ rebounds                      : int [1:19044] 0 5 2 5 1 0 3 2 2 0 ...
    $ inside_fifties                : int [1:19044] 3 1 5 1 0 4 2 3 6 1 ...
    $ clearances                    : int [1:19044] 1 0 4 0 0 3 0 4 11 0 ...
    $ clangers                      : int [1:19044] 2 3 9 2 2 4 3 4 4 0 ...
    $ free_kicks_for                : int [1:19044] 0 0 2 1 0 0 0 2 2 1 ...
    $ free_kicks_against            : int [1:19044] 0 1 6 1 1 0 2 0 2 0 ...
    $ brownlow_votes                : int [1:19044] 0 0 0 0 0 0 0 0 0 0 ...
    $ contested_possessions         : int [1:19044] 3 3 12 3 3 4 3 12 15 3 ...
    $ uncontested_possessions       : int [1:19044] 10 6 3 8 6 22 18 11 24 10 ...
    $ contested_marks               : int [1:19044] 0 2 2 1 0 0 1 0 0 0 ...
    $ marks_inside_fifty            : int [1:19044] 0 0 0 0 0 1 0 1 0 0 ...
    $ one_percenters                : int [1:19044] 0 12 3 4 4 0 2 1 1 2 ...
    $ bounces                       : int [1:19044] 0 3 0 0 0 0 1 0 0 0 ...
    $ goal_assists                  : int [1:19044] 1 0 0 1 0 1 0 0 0 0 ...
    $ time_on_ground_percentage     : int [1:19044] 75 97 97 83 100 81 75 79 84 88 ...
    $ afl_fantasy_score             : int [1:19044] 64 59 79 48 35 95 100 96 120 58 ...
    $ supercoach_score              : int [1:19044] NA NA NA NA NA NA NA NA NA NA ...
    $ centre_clearances             : int [1:19044] 0 0 1 0 0 1 0 1 6 0 ...
    $ stoppage_clearances           : int [1:19044] 1 0 3 0 0 2 0 3 5 0 ...
    $ score_involvements            : int [1:19044] 4 1 8 2 0 6 4 5 4 1 ...
    $ metres_gained                 : int [1:19044] 103 384 326 250 151 456 391 315 613 167 ...
    $ turnovers                     : int [1:19044] 4 3 4 2 3 10 3 4 6 3 ...
    $ intercepts                    : int [1:19044] 1 5 4 3 3 0 6 7 1 0 ...
    $ tackles_inside_fifty          : int [1:19044] 1 0 0 0 0 0 0 0 0 0 ...
    $ contest_def_losses            : int [1:19044] 0 1 0 0 0 0 0 0 0 0 ...
    $ contest_def_one_on_ones       : int [1:19044] 0 6 1 2 0 0 2 0 0 0 ...
    $ contest_off_one_on_ones       : int [1:19044] 0 0 0 0 0 0 0 0 0 2 ...
    $ contest_off_wins              : int [1:19044] 0 0 0 0 0 0 0 0 0 0 ...
    $ def_half_pressure_acts        : int [1:19044] 5 3 9 12 3 1 2 10 6 5 ...
    $ effective_kicks               : int [1:19044] 5 10 5 7 4 9 12 6 14 3 ...
    $ f50_ground_ball_gets          : int [1:19044] 1 0 1 0 0 0 0 0 2 0 ...
    $ ground_ball_gets              : int [1:19044] 3 1 3 1 2 4 1 9 9 2 ...
    $ hitouts_to_advantage          : int [1:19044] 0 0 8 0 0 0 0 0 0 0 ...
    $ hitout_win_percentage         : num [1:19044] 0 0 64.2 0 0 0 0 0 0 0 ...
    $ intercept_marks               : int [1:19044] 0 3 1 1 1 0 3 1 0 0 ...
    $ marks_on_lead                 : int [1:19044] 1 0 0 0 0 0 0 0 0 1 ...
    $ pressure_acts                 : int [1:19044] 23 3 15 16 4 10 6 24 15 8 ...
    $ rating_points                 : num [1:19044] 4.6 9.6 10.4 6.6 2.9 8.9 15.6 11.2 20.6 4.3 ...
    $ ruck_contests                 : int [1:19044] 0 0 53 0 0 0 0 0 0 0 ...
    $ score_launches                : int [1:19044] 0 1 5 1 0 0 1 2 1 0 ...
    $ shots_at_goal                 : int [1:19044] 0 0 2 1 0 3 1 2 0 1 ...
    $ spoils                        : int [1:19044] 0 10 3 4 3 0 1 0 0 1 ...
    $ subbed                        : chr [1:19044] NA NA NA NA ...
    $ player_position               : chr [1:19044] "FPR" "FB" "RK" "CHF" ...
    $ date                          : Date[1:19044], format: "2022-03-16" "2022-03-16" "2022-03-16" ...

#### Remove columns that aren’t associated with the target “disposals” column and deal with match round values.

Remove unnecessary columns with respect to ‘disposals’: 

    df1 <- subset(df1, select = -c(player_height_cm, player_weight_kg, subbed, supercoach_score, player_is_retired, date))

#### Deal with match round values
Match_round is a character and contains finals as well as numeric rounds. We will convert them all to numeric:  

    df1$match_round <- case_when (
        df1$match_round == 'Finals Week 1' ~ 24,
        df1$match_round == 'Semi Finals' ~ 25,
        df1$match_round == 'Preliminary Finals' ~ 26,
        df1$match_round == 'Grand Final' ~ 27,
        TRUE ~ as.numeric(as.factor(df1$match_round)))

#### Add and update other columns
Add a column for the year (season):  

    df1$season <- as.numeric(format(as.Date(df1$match_date, format="%Y-%m-%d"),"%Y"))

Update venue names for the current season:  

    df1$venue_name[df1$venue_name=="Metricon Stadium"] <- "Heritage Bank Stadium"
    df1$venue_name[df1$venue_name=="UNSW Canberra Oval"] <- "Manuka Oval"

Check missing values:  

    sapply(df1, function(x) sum(is.na(x)))

## Explore and Visualize the data

Let's take a look at overall disposal distribution.

    library(ggplot2)

    ggplot(data = df1, aes(x = disposals, fill = ..count..)) +
    geom_histogram(bins = 30) +
    scale_x_continuous(name = 'Disposals', breaks = seq(0, 50, 10), limits=c(0, 50)) +
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

![Disposals_Histogram](https://github.com/mattm14/AFL-Player-Disposal-Prediction/assets/34406190/29792322-e883-4760-8cd5-5cf91a69dd18)

Most players acquire between 10-20 disposals each match. 

Let's take it further and explore by position.

First, we need to create some position categories from the existing player_position levels.

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

    df1 <- df1[!(df1$player_group == 'Other'),]

    ggplot(df1, aes(x=disposals, fill = player_group)) +
      geom_histogram(position="identity", bins = 20) +
      scale_fill_manual(values = c('darkblue', 'brown', 'darkgreen')) +
      labs(fill = "Player Group")
      
![Disposals_Histogram_Group](https://github.com/mattm14/AFL-Player-Disposal-Prediction/assets/34406190/73181930-441e-45c7-bbcc-d06467f414bf)

We can see that positions categorized as "centre" usually end up with the highest number of disposals.

## Perform feature importance with boruta
Remove the match data and focus on player data for feature importance. We'll keep match_id for use later.  

    df1 <- df1[  -c(1,3:18,20:22)]  
     
Load the Boruta library and perform feature importance.

    library(Boruta)

    # for reproducibility
    set.seed(111)

    # get importance using boruta
    boruta.df1_train <- Boruta(disposals~., data = df1, doTrace = 2)
    
Let's take a look at our results:

    print(boruta.df1_train)  
    
    Boruta performed 68 iterations in 1.567528 hours.
    52 attributes confirmed important: afl_fantasy_score, behinds, bounces, centre_clearances, clangers and 47 more;
    3 attributes confirmed unimportant: brownlow_votes, match_id, season;

Let's plot these lot the results showing their relative importance. This can be done using the suggestion [here](https://stackoverflow.com/questions/47342553/boruta-box-plots-in-r).

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

We'll now plot the data using the most important features:  

    plot.Boruta.sel(boruta.df1_train, pars = c('disposal_efficiency_percentage', 'handballs', 'uncontested_possessions', 
                                            'kicks', 'effective_disposals', 'contested_possessions', 'ground_ball_gets',
                                            'afl_fantasy_score'))
                                            
![box](https://github.com/mattm14/AFL-Player-Disposal-Prediction/assets/34406190/f3e11c9b-d6cb-4bb1-a326-9750f5c4837a)

    # put results into df
    df1_boruta <- attStats(boruta.df1_train)

We will take those variables with meanImp >= 20.  

    df_final_output <- subset(df1_boruta, df1_boruta$meanImp >= 20)

## Feature Engineering  
Let's take these features and now create some variables for our model.

    df1 <- df1[, c('disposals', 'player_id', 'season', 'match_id', 'disposal_efficiency_percentage', 'handballs',
                  'uncontested_possessions', 'kicks', 'effective_disposals', 'contested_possessions', 'ground_ball_gets',
                  'afl_fantasy_score')]

    # create a variable group of features
    vars <- c('disposal_efficiency_percentage', 'handballs', 'uncontested_possessions', 'kicks', 'effective_disposals',
            'contested_possessions', 'ground_ball_gets', 'afl_fantasy_score')

Create a group of averages for each variable:

    #Averages of each variable
        df1 <- df1 %>%
        arrange(player_id, season, match_id) %>%
        group_by(player_id, season) %>%
        mutate(rec = 1) %>%
        mutate(cum_rec = cumsum(rec), across(all_of(vars), ~cumsum(.x)/cum_rec, .names = 'avg_{.col}')) %>%
        ungroup()

## Build the prediction model
Here, we make our first attempt at training the model, using 10,000 trees and 5 cross fold validation.

    # load required libraries
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

Let's take a look at the results:

    # print results
    print(gbm.fit)
    
    gbm(formula = disposals ~ ., distribution = "gaussian", data = train, 
        n.trees = 10000, interaction.depth = 1, shrinkage = 0.001, 
        cv.folds = 5, verbose = FALSE, n.cores = NULL)
    A gradient boosted model with gaussian loss function.
    10000 iterations were performed.
    The best cross-validation iteration was 10000.
    There were 8 predictors of which 8 had non-zero influence

Here, we see that the minimum CV RMSE is 4.41 (this means on average our model is about 4.4 disposals from the actual amount of disposals but the plot also illustrates that the CV error is still decreasing at 10,000 trees.)

    # get MSE and compute RMSE
    sqrt(min(gbm.fit$cv.error))
    # [1] 4.412487

    # plot loss function as a result of n trees added to the ensemble
    gbm.perf(gbm.fit, method = "cv")
    # [1] 10000

![plot_loss](https://github.com/mattm14/AFL-Player-Disposal-Prediction/assets/34406190/a1d72689-3057-4150-aa3b-058ee415d489)

We make a second attempt at training the model, this time using 5,000 trees and an interaction depth of 3.

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

Let's see how we performed:

    # print results
    print(gbm.fit2)

    gbm(formula = disposals ~ ., distribution = "gaussian", data = train, 
        n.trees = 5000, interaction.depth = 3, shrinkage = 0.1, cv.folds = 5, 
        verbose = FALSE, n.cores = NULL)
    A gradient boosted model with gaussian loss function.
    5000 iterations were performed.
    The best cross-validation iteration was 89.
    There were 8 predictors of which 8 had non-zero influence

This model achieves a slightly improved RMSE than our initial model with only 89 trees.

    # find index for n trees with minimum CV error
    min_MSE <- which.min(gbm.fit2$cv.error)

    # get MSE and compute RMSE
    sqrt(gbm.fit2$cv.error[min_MSE])
    ## [1] 4.397025

    # plot loss function as a result of n trees added to the ensemble
    gbm.perf(gbm.fit2, method = "cv")
    # [1] 89

We make another attempt at traiing the model by tuning some of our hyperparameters using a grid search. Performing a grid search allows us to iterate over every combination of hyper parameter values. This allows us to assess which combination tends to perform well.

    # create hyperparameter grid
    hyper_grid <- expand.grid(
    shrinkage = c(.01, .1, .3),
    interaction.depth = c(1, 3, 5),
    n.minobsinnode = c(5, 10, 15),
    bag.fraction = c(.65, .8, 1), 
    optimal_trees = 0,               
    min_RMSE = 0                     
    )

    # total number of combinations
    nrow(hyper_grid)
    ## [1] 81

    # randomize data
    random_index <- sample(1:nrow(train), nrow(train))
    random_data_train <- train[random_index, ]   

    # grid search 
    for(i in 1:nrow(hyper_grid)) {
    
    # reproducibility
    set.seed(123)
    
    # train model
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

Let's take a look at our results:

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

These results provide a guidance in looking at specific parameters that we can refine to improve our overall RMSE.

Let’s adjust our grid and and refine it to look at closer parameters that appear to produce the best results in our previous grid search.

    # modify hyperparameter grid
    hyper_grid <- expand.grid(
    shrinkage = c(.01, .05, .1),
    interaction.depth = 3,
    n.minobsinnode = c(5, 10, 15),
    bag.fraction = c(.8, 1), 
    optimal_trees = 0,               
    min_RMSE = 0                     
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

The results are similar to before, with the best model producing a slightly better result.

### Final model with optimal parameters
Once we have found our top model we train a model with those specific parameters.

As the model converged at 223 trees I train a cross validated model (to provide a more robust error estimate) with 1000 trees.

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

Let's chart our results:

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

![rel_influence](https://github.com/mattm14/AFL-Player-Disposal-Prediction/assets/34406190/50f9fe3c-9723-4e9b-a932-bc40c887f1fd)

## Making predictions
Once we have produced our final model, we use it to predict on new observations.

To do this, we use the predict function; however, we also need to supply the number of trees to use (see ?predict.gbm for details). 

We see that our RMSE for our test set is very close to the RMSE we obtained on our best gbm model.

    # predict values for test data
    pred <- predict(gbm.fit.final, n.trees = gbm.fit.final$n.trees, test)

    # results
    caret::RMSE(pred, test$disposals)
    ## [1] 4.345416

    # add predictions back into test data
    test <- cbind(test, pred)

## Set up df for shiny

    # add player/match data back to test df
    test$match_home_team <- s2023$match_home_team[match(test$match_id, s2023$match_id)]
    test$match_away_team <- s2023$match_away_team[match(test$match_id, s2023$match_id)]
    test$venue_name <- s2023$venue_name[match(test$match_id, s2023$match_id)]
    test$match_date <- s2023$match_date[match(test$match_id, s2023$match_id)]
    test$match_round <- s2023$match_round[match(test$match_id, s2023$match_id)]
    
    test$player_first_name <- s2023$player_first_name[match(test$player_id, s2023$player_id)]
    test$player_last_name <- s2023$player_last_name[match(test$player_id, s2023$player_id)]
    test$player_name <- factor(paste(test$player_first_name, test$player_last_name, sep = " ")) # join name
    test$player_team <- s2023$player_team[match(test$player_id, s2023$player_id)]
    
    # subset test data to look at round 1 2023 only
    df1.1 <- subset(test, test$match_round == 1 )
    
    # arrange data
    df1.1 <- df1.1 %>%
      arrange(match_id, match_home_team, match_away_team, player_team)
    
    # round the predicted no. of disposals
    df1.1$pred <- round(df1.1$pred,0)
        
    # select req'd cols
    df1.1 <- df1.1[c('match_home_team', 'match_away_team', 'venue_name', 'match_date', 'match_id', 'match_round',
                   'player_id', 'player_team', 'player_name', 'pred', 'disposals')]


    # add last 10 games' disposals
    s2022 <- s2022 %>% 
      arrange(player_id, match_date) %>%
      group_by(player_id) %>% 
      dplyr::slice(tail(row_number(), 10))
    
    df_last_10 <- setDT(s2022)[, c(paste0(1:10)):=shift(disposals, 0:9), by=player_id][]
    df_last_10 <- df_last_10[,c('player_id', 1,2,3,4,5,6,7,8,9,10)]
    df_last_10 <- na.omit(df_last_10)
    
    # join dfs
    df1.1 <- left_join(df1.1, df_last_10, by = c('player_id'))

## Viewing predictions with [Shiny](https://cran.r-project.org/web/packages/shiny/index.html)
Once we have our predictions, we can use [Shiny](https://cran.r-project.org/web/packages/shiny/index.html) to bring these together for each round, and include some other key stats.  

Create min and max for disposals heat map:

    x_min <- 10
    x_max <- 30
    x <- c(x_min,x_max)
    quantile(x,probs = seq(0, 1, 0.25))

Set breaks and colours for heat map:

    brks <- as.vector(quantile(x, probs = seq(0, 1, 0.25)))
    ramp <- colorRampPalette(c("white", "lightgreen","lightblue","orange"))
    clrs <- ramp(length(brks) + 1)

Define the ui:

    ui <- fluidPage(
    tags$head(
        tags$style(HTML(
        "table {table-layout: fixed;}",
        "td {white-space: nowrap;}",
        "div.dataTables_wrapper div.dataTables_filter input {width: 75%;}",
        '.navbar { background-color: lightgray;}
                .navbar-default .navbar-brand{color: white;}
                .tab-panel{ background-color: lightgray; color: black}
                .navbar-default .navbar-nav > .active > a, 
                .navbar-default .navbar-nav > .active > a:focus, 
                .navbar-default .navbar-nav > .active > a:hover {color: black; background-color: gray;}',
        ))),
 
      titlePanel(div("AFL - Rd 1 2023: Player Disposals", style = "font-size: 70%")),
      navbarPage("",
                 tags$style(HTML('.navbar-nav > li > a, .navbar-brand {
                       padding-top:0px !important; 
                       padding-bottom:6px !important;
                       height: 20px;}
                       .navbar {min-height:20px !important;}')),
                 id = "navbarID",
                 tabPanel("All", ""),
                 tabPanel("Adelaide", ""),
                 tabPanel("Brisbane", ""),
                 tabPanel("Carlton", ""),
                 tabPanel("Collingwood", ""),
                 tabPanel("Essendon", ""),
                 tabPanel("Fremantle", ""),
                 tabPanel("Geelong", ""),
                 tabPanel("Gold Coast", ""),
                 tabPanel("Greater Western Sydney", ""),
                 tabPanel("Hawthorn", ""),
                 tabPanel("Melbourne", ""),
                 tabPanel("North Melbourne", ""),
                 tabPanel("Port Adelaide", ""),
                 tabPanel("Richmond", ""),
                 tabPanel("St Kilda", ""),
                 tabPanel("Sydney", ""),
                 tabPanel("West Coast", ""),
                 tabPanel("Western Bulldogs", ""),
                 
                 tags$style("li a {
                            font-size: 9px;
                            font-weight: bold;}"),
                 mainPanel(div(DT::dataTableOutput("my_table"), style = "font-size: 65%; width: 150%"))
      )
    )

Output to server:

    server <- function(input, output) {
    
    table <- reactive({
        
        if (input$navbarID == 'All') {
        df1.1
        
        } else { 
        df1.1 %>% 
            filter(TEAM == input$navbarID)
        }
        
    })
    
    output$my_table <- DT::renderDataTable({
        DT::datatable(table(),
                    options = list(
                        autoWidth = TRUE,
                        scrollX = FALSE, scrollY = "540px",
                        iDisplayLength = 100, # show default number of entries,
                        initComplete = JS(
                        "function(settings, json) {",
                        "$(this.api().table().header()).css({'background-color': 'lightblue', 'color': 'black'});",
                        "}"),
                        columnDefs = list(                      
                        list(targets = c(0:9), width = "50px"),
                        list(targets = c(10:20), width = "1px"),
                        list(className = 'dt-center', targets = c(0,10:20)),
                        list(className = 'dt-head-center', targets = (10:20))))) %>% 
                    formatStyle(c('1','2','3','4','5','6','7','8','9','10'),
                    backgroundColor = styleInterval(brks, clrs)
        )
    })  
    }

    shinyApp(ui, server)

![shiny](https://github.com/mattm14/AFL-Player-Disposal-Prediction/assets/34406190/46dc0997-1950-4fe3-b686-c670ced254d4)
