---
title: "Tennis Match Simulation"
author: "Isaac Stiepleman"
date: "November 2, 2026"
output:
  pdf_document:
    keep_md: TRUE
fontsize: 11pt
geometry: margin = 1in
---



# Introduction

In the Spring of 2023, I took a class at Columbia called Applied Probability and Stochastic Processes with Dr. Sumit Mukherjee. For our final project, Professor Mukherjee let us choose from a series of articles to summarize and present to the class. I chose one entitled "Probability and Tennis" published in *Data Genetics* which calculated a player's probability of winning a match based on the probability that they would win any individual point (moving forward I will define that probability within the code and commentary as *point_prob*).

As the article notes, "as a match of tennis contains many point battles, and tennis has some quirky score advancing rules; these concentrate the advantage that a more skillful player has to win the match." In my initial project, I was interested to see if I could build a Monte Carlo simulation based on real-life matches that supported the article's conclusions of a concentrated advantage. In the years since that presentation, I have tried to enhance that simulation.


``` r
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(ggplot2)
  library(scales)
})
```

# Original Project Recap

The following comes from my original presentation on April 24, 2023. 

## Compounding Probabilities

I started by copying the article's calculations here and graphing the compounding advantage of a higher `point_prob`.


``` r
#The range of probabilities for winning a single point (p)
point_prob <- seq(0, 1, .001)

#The range of probabilities for winning a game based on p (g)
game_prob <- sapply(point_prob, 
                    function(p) return(p^4 + (4*p^4)*(1-p) + 
                                         (10*p^4)*((1-p)^2) + 
                                         ((20*p^5)*((1-p)^3))/(1-(2*p)*(1-p))))
game_prob <- round(game_prob, 10)

#The range of probabilities for winning a set based on g and p (s)
set_prob <- c()
for (p in point_prob) {
  g <- p^4 + (4*p^4)*(1-p) + (10*p^4)*((1-p)^2) + 
    ((20*p^5)*((1-p)^3))/(1-(2*p)*(1-p))
  
  t <- (p^7)*(1 + 7*(1-p) + 28*((1-p)^2) + 84*((1-p)^3) + 210*((1-p)^4) + 
                462*((1-p)^5)) + 924*(p^6)*((1-p)^6)*((p^2)/(1-(2*p)*(1-p)))
  
  s <- (g^6)*(1 + 6*(1-g) + 21*((1-g)^2) + 56*((1-g)^3) + 
                126*((1-g)^4) + 252*g*((1-g)^5) + 504*((1-g)^6)*t)
  
  set_prob <- c(set_prob, s)
}

set_prob <- round(set_prob, 10)

#The range of probabilities for winning a match based on s (m)
match_prob <- sapply(set_prob, 
                     function(s) return((s^3)*(1 + 3*(1-s) + 6*((1-s)^2))))
match_prob <- round(match_prob, 10)

#Graphing the calculated probabilities against the probability of winning a point
tennis_probs <- data.frame(point_prob, game_prob, set_prob, match_prob)

ggplot(tennis_probs, mapping = aes(x = point_prob)) + 
  geom_line(aes(y = point_prob, color = "Winning a point"), linewidth = 1) +
  geom_line(aes(y = game_prob, color = "Winning a game"), linewidth = 1) + 
  geom_line(aes(y = set_prob, color = "Winning a set"), linewidth = 1) + 
  geom_line(aes(y = match_prob, color = "Winning a match"), linewidth = 1) + 
  scale_color_manual(name = "The Probability of...", 
                      values = c("Winning a point" = "black", 
                                 "Winning a game" = "blue",
                                 "Winning a set" = "red", 
                                 "Winning a match" = "green"),
                      breaks = c("Winning a point", "Winning a game", 
                                 "Winning a set", "Winning a match")) +
  scale_x_continuous(labels = label_percent()) +
  scale_y_continuous(labels = label_percent()) +
  labs(title = "The Compounding Probabilistic Effects of Winning Points", 
       x = "Probability of Winning a Point", 
       y = "Additional Winning Probabilities") + 
  theme_minimal()
```



\begin{center}\includegraphics{Tennis-Simulator_files/figure-latex/Graph of Compounding Probabilities-1} \end{center}

Note how the succeeding lines compress excessively around the midpoint. A player with merely a $60\%$ probability of winning a point is already virtually guaranteed to win the match.

## Original Match Simulation

Next I moved to my own function. Here I've built a Markov model of tennis scoring (point $\rightarrow$ game $\rightarrow$ set $\rightarrow$ match).


``` r
#Define function to simulate a single game
simulate_game <- function(point_prob) {
  #Initialize the game scores for players 1 and 2
  player1_gamescore <- 0
  player2_gamescore <- 0
  
  #Play points until the game is won
  while (TRUE) {
    #Simulate a point using the probability given
    point_winner <- sample(c("player 1", "player 2"), 1, 
                           prob = c(point_prob, 1 - point_prob))
    
    #Update the game score
    if (point_winner == "player 1") {
      player1_gamescore <- player1_gamescore + 1
    } else {
      player2_gamescore <- player2_gamescore + 1
    }
    
    #Check if a player has won
    if (player1_gamescore >= 4 && 
        player1_gamescore - player2_gamescore >= 2) {
      return("player 1")
    } else if (player2_gamescore >= 4 && 
               player2_gamescore - player1_gamescore >= 2) {
      return("player 2")
    }
  }
}

#Define function to simulate a single set
simulate_set <- function(point_prob) {
  #Initialize the set scores for players 1 and 2
  player1_setscore <- 0
  player2_setscore <- 0
  
  #Play games until the set is won
  while (TRUE) {
    #Simulate a game using the prior simulation
    game_winner <- simulate_game(point_prob)
    
    #Update the set score
    if (game_winner == "player 1") {
      player1_setscore <- player1_setscore + 1
    } else {
      player2_setscore <- player2_setscore + 1
    }
    
    #Check if a player has won
    if (player1_setscore >= 6 && 
        player1_setscore - player2_setscore >= 2) {
      return(list(winner = "player 1", `player 1 score` = player1_setscore, 
                  `player 2 score` = player2_setscore))
    } else if (player2_setscore >= 6 && 
               player2_setscore - player1_setscore >= 2) {
      return(list(winner = "player 2", `player 1 score` = player1_setscore, 
                  `player 2 score` = player2_setscore))
    } else if (player1_setscore == 6 && player2_setscore == 6) {
      #Tiebreak: a player must win at least 7 points AND win by 2 to win the set
      
      #Initialize the tiebreak scores for players 1 and 2
      player1_tiebreakscore <- 0
      player2_tiebreakscore <- 0
      
      while (TRUE) {
        #Simulate a point using the probability given
        point_winner <- sample(c("player 1", "player 2"), 1, 
                               prob = c(point_prob, 1 - point_prob))
        
        #Update the tiebreak score
        if (point_winner == "player 1") {
          player1_tiebreakscore <- player1_tiebreakscore + 1
        } else {
          player2_tiebreakscore <- player2_tiebreakscore + 1
        }
        
        #Check if the tiebreak has been won
        if (player1_tiebreakscore >= 7 && 
            player1_tiebreakscore - player2_tiebreakscore >= 2) {
          
          return(list(winner = "player 1", 
                      `player 1 score` = 7, 
                      `player 2 score` = 6, 
                      `player 1 tiebreak score` = player1_tiebreakscore, 
                      `player 2 tiebreak score` = player2_tiebreakscore))
        } else if (player2_tiebreakscore >= 7 && 
                   player2_tiebreakscore - player1_tiebreakscore >= 2) {
          return(list(winner = "player 2", 
                      `player 1 score` = 6, 
                      `player 2 score` = 7, 
                      `player 1 tiebreak score` = player1_tiebreakscore, 
                      `player 2 tiebreak score` = player2_tiebreakscore))
        }
      }
    }
  }
}

#Define function to simulate a best-of-5 contest
simulate_match <- function(point_prob) {
  #Initialize match scores for players 1 and 2
  player1_matchscore <- 0
  player2_matchscore <- 0
  
  #Initialize a list to store the set scores
  scores <- list()
  
  #Play sets until the match is won
  while (TRUE) {
    # Simulate a set using the prior simulation
    set_result <- simulate_set(point_prob)
    set_winner <- set_result$winner

    #Update the match score
    if (set_winner == "player 1") {
      player1_matchscore <- player1_matchscore + 1
    } else {
      player2_matchscore <- player2_matchscore + 1
    }
    
    # Add the set scores to the scores list
    scores[[length(scores) + 1]] <- c(set_result$`player 1 score`, 
                                      set_result$`player 2 score`, 
                                      set_result$`player 1 tiebreak score`, 
                                      set_result$`player 2 tiebreak score`)
    
    # Check if a player has won the match
    if (player1_matchscore >= 3 || player2_matchscore >= 3) {
      break
    }
  }
  
  # Return the winner of the match
  if (player1_matchscore > player2_matchscore) {
    return(list(winner = "player 1", score = set_scores))
  } else {
    return(list(winner = "player 2", score = set_scores))
  }
}
```

It took a while to build a function that was consistent with the scoring rules of tennis but it was gratifying to successfully complete.

## Win Proportions

In my original presentation, I simulated specific and iconic Grand Slam Finals (2012 Australian Open, 2008 and 2019 Wimbledon) as case studies. In recreating this, I've chosen to replace those specific matches with a graph that demonstrates the converging stability of the simulation across 10, 100, and 1000 sims of a select `point_prob` range.


``` r
#Sequence of probabilities for testing, concentrated around breakeven
probs <- seq(.4, .6, .001)

results <- data.frame()

for (i in c(10, 100, 1000)) {#outer loop for each simulation threshold
  win_pct <- c()
  
  for (k in seq_along(probs)) {#middle loop for each probability
    winners <- c()
    
    for (j in 1:i) {#inner loop for i simulations
      winners[j] <- simulate_match(probs[k])$winner #using original simulation
    }
    
    win_pct[k] <- mean(winners == "player 1")
  }
  
  results <- rbind(results, data.frame(probs, win_pct, sims = paste(i, "sims")))
}

results <- rbind(results, data.frame(probs, #add the exact probabilities
                                     win_pct = match_prob[round(probs * 1000) + 1],
                                     sims = "Exact Proportion"))

ggplot(results, aes(x = probs, y = win_pct, color = sims, linetype = sims)) +
  geom_line() +
  scale_color_manual(name = "Simulations per probability",
                     values = c("#D1E8FF", #h/t: Claude for the color recs
                                "#59A5F5", 
                                "#093A7E", 
                                "red")) +
  scale_linetype_manual(name = "Simulations per probability",
                        values = c("solid", 
                                   "solid", 
                                   "solid", 
                                   "dashed")) +
  scale_x_continuous(labels = label_percent()) +
  scale_y_continuous(labels = label_percent()) +
  labs(title = "Win Proportion by Point Probability",
       subtitle = "Simulated curves converge on the exact match win proportion",
       x = "Player point_prob",
       y = "Proportion of Wins") +
  theme_minimal()
```



\begin{center}\includegraphics{Tennis-Simulator_files/figure-latex/Graph of Win Proportions-1} \end{center}

Note the red dashed line which indicates the exact calculated probability from the first graph. Now begins my additions.

# Dataset

I will be using match-by-match data from the official ATP tour (Association of Tennis Professionals). Credit to Jeff Sackmann (official license at the end) for his exhaustive work in gathering the match-by-match data for this project; it's a truly invaluable resource.

## Introduction and Manipulation


``` r
atpmatches <- read_csv("atp 2000_2024.csv") #list of matches from 2000 to 2024
```

I restricted my analysis to the 21st Century so as to minimize errors associated with the massive difference in playstyles between eras.


``` r
names(atpmatches)
```

```
##  [1] "tourney_id"         "tourney_name"       "surface"           
##  [4] "draw_size"          "tourney_level"      "tourney_date"      
##  [7] "match_num"          "winner_id"          "winner_seed"       
## [10] "winner_entry"       "winner_name"        "winner_hand"       
## [13] "winner_ht"          "winner_ioc"         "winner_age"        
## [16] "loser_id"           "loser_seed"         "loser_entry"       
## [19] "loser_name"         "loser_hand"         "loser_ht"          
## [22] "loser_ioc"          "loser_age"          "score"             
## [25] "best_of"            "round"              "minutes"           
## [28] "w_ace"              "w_df"               "w_svpt"            
## [31] "w_1stIn"            "w_1stWon"           "w_2ndWon"          
## [34] "w_SvGms"            "w_bpSaved"          "w_bpFaced"         
## [37] "l_ace"              "l_df"               "l_svpt"            
## [40] "l_1stIn"            "l_1stWon"           "l_2ndWon"          
## [43] "l_SvGms"            "l_bpSaved"          "l_bpFaced"         
## [46] "winner_rank"        "winner_rank_points" "loser_rank"        
## [49] "loser_rank_points"
```

``` r
tail(atpmatches |> 
  group_by(w_ace) |> 
  summarize(Count = n())) #number of matches without point by point data
```

```
## # A tibble: 6 x 2
##   w_ace Count
##   <dbl> <int>
## 1    54     1
## 2    61     1
## 3    64     1
## 4    75     1
## 5   113     1
## 6    NA  6520
```

``` r
atpmatches |> 
  group_by(surface) |> 
  summarize(Count = n()) #distribution of surfaces
```

```
## # A tibble: 5 x 2
##   surface Count
##   <chr>   <int>
## 1 Carpet   2039
## 2 Clay    24244
## 3 Grass    7699
## 4 Hard    40871
## 5 <NA>       53
```

Note the variables tracked, the 6,520 matches without point by point data, and the 2,039 matches played on carpet. The latter two will need to be addressed.


``` r
atpmatches <- atpmatches |> 
  filter(!is.na(w_ace)) #remove matches with no point by point data

atpmatches <- atpmatches |> 
  filter(surface != 'Carpet') #remove matches played on carpet (obsolete surface)
```

## Addressing Outliers

Because this dataset tracks *every* match played on the ATP tour, many players will have played hardly any matches (as you can observe from the chart below) and will have wildly unreliable point-by-point data as a result.


``` r
#Number of matches by player (total and by surface)
player_matches <- bind_rows(atpmatches |> select(names = winner_name, surface),
                            atpmatches |> select(names = loser_name, surface)) |> 
  group_by(names) |> 
  summarize(total = n(), 
            clay = sum(surface == "Clay"),
            hard = sum(surface == "Hard"),
            grass = sum(surface == "Grass"))

#Graph to observe distribution 
ggplot(player_matches, aes(x = total)) +
  geom_density(fill = "steelblue") + 
  geom_vline(xintercept = 100, 
             color = "black", 
             linetype = "dashed") +
  annotate("text", 
           x = 100, 
           y = -.001, 
           hjust = -.1,
           label = "Cutoff = 100") +
  labs(title = "Distribution of Matches Played per Player",
       x = "Number of Matches Played",
       y = "Density") +
  theme_minimal()
```



\begin{center}\includegraphics{Tennis-Simulator_files/figure-latex/Addressing Outliers-1} \end{center}

``` r
#Filter those with at least 100 total matches and 10 clay, hard, & grass matches
qualified_players <- player_matches |> 
  filter(total >= 100, 
         clay >= 10, 
         hard >= 10, 
         grass >= 10) |>
  pull(names)
length(qualified_players)
```

```
## [1] 313
```

In order to remove outliers, I set the required minimum number of matches played at 100 (as well as 10 matches per surface). Our analysis will be based on 313 players and thus will only describe established tour regulars.

Moving now to resimulating the base model with real-life match data.

# Part 1 -- Base Model

## Improving Match Simulation

I redefine the base simulation model with general maintenance and a few substantive changes:

- First, I add a `bestof` argument that defines a set threshold for victory which allows me to simulate best of 3 and best of 5 set matches with the same function.
- Second, I've changed it so that the function takes *both* players `point_probs` instead of just player 1's. Player 1 will win each point with probability $\frac{p_1}{p_1 + p_2}$. 
- Third, I've removed `list()` from the return of each function. I really only care about the winners now, not the set scores.
- Fourth, I swapped `sample()` for `runif()` throughout so that the function runs much faster (which will matter once thousands of matches are simulated at once).


``` r
#Define function to simulate a single game
base_gamefunction <- function(point_prob1, point_prob2) {
  #Initialize the game scores for players 1 and 2
  p1_gamescore <- 0
  p2_gamescore <- 0
  
  #Calculating point_prob without using sample() for efficiency's sake
  p1_pointprob <- point_prob1 / (point_prob1 + point_prob2)
  
  #Play points until the game is won
  while (TRUE) {
    #Simulate a point and update the game score
    if (runif(1) < p1_pointprob) {
      p1_gamescore <- p1_gamescore + 1
    } else {
      p2_gamescore <- p2_gamescore + 1
    }
    
    #Check if a player has won
    if (p1_gamescore >= 4 && p1_gamescore - p2_gamescore >= 2) {
      return("player 1")
    } else if (p2_gamescore >= 4 && p2_gamescore - p1_gamescore >= 2) {
      return("player 2")
    }
  }
}

#Define function to simulate a single set
base_setfunction <- function(point_prob1, point_prob2) {
  #Initialize the set scores for players 1 and 2
  p1_setscore <- 0
  p2_setscore <- 0
  
  #Play games until the set is won
  while (TRUE) {
    #Simulate a game using the prior simulation
    game_winner <- base_gamefunction(point_prob1, point_prob2)
    
    #Update the set score
    if (game_winner == "player 1") {
      p1_setscore <- p1_setscore + 1
    } else {
      p2_setscore <- p2_setscore + 1
    }
    
    #Check if a player has won
    if (p1_setscore >= 6 && p1_setscore - p2_setscore >= 2) {
      return("player 1")
    } else if (p2_setscore >= 6 && p2_setscore - p1_setscore >= 2) {
      return("player 2")
    } else if (p1_setscore == 6 && p2_setscore == 6) {
      #Tiebreak triggered: first player to 7 points (win by 2) wins the set
      
      #Initialize the tiebreak scores for players 1 and 2
      p1_tbscore <- 0
      p2_tbscore <- 0
      
      #Again without sample() for the sake of efficiency
      p1_pointprob <- point_prob1 / (point_prob1 + point_prob2)
      
      while (TRUE) {
        #Simulate a point and update the tiebreak score
        if (runif(1) < p1_pointprob) {
          p1_tbscore <- p1_tbscore + 1
        } else {
          p2_tbscore <- p2_tbscore + 1
        }
        
        #Check if the tiebreak has been won
        if (p1_tbscore >= 7 && p1_tbscore - p2_tbscore >= 2) {
          return("player 1")
        } else if (p2_tbscore >= 7 && p2_tbscore - p1_tbscore >= 2) {
          return("player 2")
        }
      }
    }
  }
}

#Define function to simulate a match
base_matchfunction <- function(point_prob1, point_prob2, bestof) {
  #Define the number of sets needed to win
  setstowin <- ceiling(bestof / 2)
  
  #Initialize match scores for players 1 and 2
  p1_matchscore <- 0
  p2_matchscore <- 0
  
  #Play sets until the match is won
  while (TRUE) {
    #Simulate a set using the prior simulation
    set_winner <- base_setfunction(point_prob1, point_prob2)

    #Update the match score
    if (set_winner == "player 1") {
      p1_matchscore <- p1_matchscore + 1
    } else {
      p2_matchscore <- p2_matchscore + 1
    }
    
    #Check if a player has won the match
    if (p1_matchscore == setstowin || p2_matchscore == setstowin) {
      return(if (p1_matchscore > p2_matchscore) "player 1" else "player 2")
    }
  }
}
```

## Find point_probs

Now that we have data for every player, we can assign each one a `point_prob` and simulate their matches. In order to properly evaluate the performance of the simulation functions, I randomly split all of the valid matches 80/20 into a train and test set. Every probability is calculated from only the train matches and the test matches are set aside for grading.


``` r
set.seed(2026)

#First split the dataset into train/test batches for future analysis
atpmatches <- atpmatches |>
  mutate(match_id = row_number())

train_ids <- atpmatches |> slice_sample(prop = .8) |> pull(match_id)

atpmatches_train <- atpmatches |> filter(match_id %in% train_ids)
atpmatches_test <- atpmatches |> filter(!(match_id %in% train_ids)) |> #*
  filter(winner_name %in% qualified_players, loser_name %in% qualified_players)

point_prob <- c()

for (player in qualified_players) {
  player_matches <- atpmatches_train |>
    filter(winner_name == player | loser_name == player) |> #select their matches
    mutate(pts_won = ifelse(winner_name == player, #points won in each match
                            w_1stWon + w_2ndWon + l_svpt - (l_1stWon + l_2ndWon),
                            l_1stWon + l_2ndWon + w_svpt - (w_1stWon + w_2ndWon)))

  total_pts <- sum(player_matches$w_svpt) + sum(player_matches$l_svpt)
  total_ptswon <- sum(player_matches$pts_won)
  
  point_prob <- c(point_prob, total_ptswon / total_pts) 
}

ggplot(data.frame(point_prob), aes(x = point_prob)) +
  geom_histogram(aes(y = after_stat(density)), 
                 bins = 30, 
                 fill = "steelblue", 
                 color = "white", 
                 alpha = .8) +
  geom_density(color = "darkred", linewidth = 1) +
  geom_vline(xintercept = mean(point_prob), 
             color = "black", 
             linetype = "dashed", 
             linewidth = .7) +
  scale_x_continuous(labels = label_percent()) +
  labs(title = "Distribution of Player Point-Win Probabilities",
       x = "Point-Win Probability", 
       y = "Density") +
  annotate("text", 
           x = mean(point_prob), 
           y = Inf, 
           label = paste0("Mean = ", percent(mean(point_prob), accuracy = .01)),
           vjust = 2, 
           hjust = -.1, 
           size = 3.5) +
  theme_minimal()
```



\begin{center}\includegraphics{Tennis-Simulator_files/figure-latex/Find point_probs-1} \end{center}

``` r
#Dataset to track all XYZ_probs
master_trainset <- data.frame(player = qualified_players, point_prob = point_prob)
head(master_trainset)
```

```
##             player point_prob
## 1 Adrian Mannarino  0.4942384
## 2    Adrian Voinea  0.4855483
## 3  Agustin Calleri  0.5070830
## 4  Albert Montanes  0.4929014
## 5    Albert Portas  0.4864633
## 6     Albert Ramos  0.4957481
```

``` r
#Dataset to track all XYZ_winpcts
master_testset <- bind_rows(atpmatches_test |> 
                              transmute(player = winner_name, won = TRUE),
                            atpmatches_test |> 
                              transmute(player = loser_name, won = FALSE)) |>
  group_by(player) |>
  summarize(test_matches = n(), real_winpct = mean(won))
head(master_testset)
```

```
## # A tibble: 6 x 3
##   player           test_matches real_winpct
##   <chr>                   <int>       <dbl>
## 1 Adrian Mannarino          128       0.422
## 2 Adrian Voinea              16       0.375
## 3 Agustin Calleri            49       0.408
## 4 Albert Montanes            72       0.347
## 5 Albert Portas              26       0.231
## 6 Albert Ramos              103       0.369
```

Most players sit just in the narrow range around 50\% with only an elite few winning more than 52\%.

$*$ I added this filter because both players need a point_prob for the simulation to work so we can only consider matches where both players are qualified.

## Base Simulation


``` r
#Vector holding player name and their point_prob
pp <- setNames(master_trainset$point_prob, master_trainset$player)

#Function to simulate matches with the base model
num_sims <- 50 #this is per match

basesim <- function(player) {
  player_stats <- pp[[player]]
  testset_matches <- atpmatches_test |>
    filter(winner_name == player | loser_name == player) |>
    mutate(opp = ifelse(winner_name == player, loser_name, winner_name))
  
  sim_results <- logical(nrow(testset_matches) * num_sims) #values to be filled
  k <- 0 #for indexing
  
  for (i in 1:nrow(testset_matches)) {
    opp_stats <- pp[[testset_matches$opp[i]]]
    bestof_i <- testset_matches$best_of[i]
    
    for (s in 1:num_sims) {
      match_result <- base_matchfunction(player_stats, opp_stats, bestof_i)
      k <- k + 1 
      sim_results[k] <- match_result == "player 1" #filling values
    }
  }

  mean(sim_results)
}

#Add the win% from simulating matches with the base model
master_testset$basesim_winpct <- unname(sapply(master_testset$player, basesim))
head(master_testset)
```

```
## # A tibble: 6 x 4
##   player           test_matches real_winpct basesim_winpct
##   <chr>                   <int>       <dbl>          <dbl>
## 1 Adrian Mannarino          128       0.422          0.458
## 2 Adrian Voinea              16       0.375          0.38 
## 3 Agustin Calleri            49       0.408          0.525
## 4 Albert Montanes            72       0.347          0.456
## 5 Albert Portas              26       0.231          0.407
## 6 Albert Ramos              103       0.369          0.455
```

In order to grade the model, I simulate each test match 50 times in the same format as the real match and with the same `point_prob` of each player. A player's simulated win\% is the share of those simulations they win.

## Analyze Base Simulation Accuracy


``` r
base_data <- master_testset |>
  select(test_matches, real_winpct, basesim_winpct)

ggplot(base_data, aes(x = basesim_winpct, y = real_winpct)) +
  geom_point(color = "steelblue") +
  geom_smooth(aes(weight = test_matches), #actual predictive power
              method = "lm",
              se = FALSE,
              linetype = "dashed",
              color = "darkred") +
  geom_abline(slope = 1, #ideal predictive power
              intercept = 0, 
              color = "black", 
              linetype = "dashed", 
              linewidth = .8) +
  scale_x_continuous(labels = label_percent()) +
  scale_y_continuous(labels = label_percent()) +
  labs(title = "Base Model: Simulated vs Actual Match Win %",
       x = "Simulated Win % (base model)",
       y = "Actual Win % (held-out matches)") +
  theme_minimal()
```



\begin{center}\includegraphics{Tennis-Simulator_files/figure-latex/Analyze Base Model Accuracy-1} \end{center}

``` r
base_model <- lm(real_winpct ~ basesim_winpct, data = base_data, 
                 weights = test_matches) #test_matches are unequal, weight needed
summary(base_model)
```

```
## 
## Call:
## lm(formula = real_winpct ~ basesim_winpct, data = base_data, 
##     weights = test_matches)
## 
## Weighted Residuals:
##      Min       1Q   Median       3Q      Max 
## -1.55594 -0.44721 -0.02121  0.33478  1.67442 
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)    -0.32430    0.03159  -10.27   <2e-16 ***
## basesim_winpct  1.64693    0.06252   26.34   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.5712 on 311 degrees of freedom
## Multiple R-squared:  0.6905,	Adjusted R-squared:  0.6895 
## F-statistic: 693.9 on 1 and 311 DF,  p-value: < 2.2e-16
```

If the simulator had perfect prediction power, every player would land on the black dashed line where `basesim_winpct` equals `real_winpct`. Instead, the dashed red line of best fit is much steeper than the diagonal which means the simulation is too conservative. It punishes good players and rewards bad ones. We will aim to improve this in the next part. 

# Part 2 -- Serve + Return

Although it's intuitive to say that adding arguments to represent the clear advantage of being "on serve" will make a better model, I still want to run a multiple regression to show that win\% is a function of serving + returning.


``` r
#Create a unified dataset of all matches for players as winners and losers
player_stats <- atpmatches |>
  mutate(player_name = winner_name, 
         result = TRUE,
         svpts_won = w_1stWon + w_2ndWon, 
         svpts = w_svpt,
         rtpts_won = l_svpt - l_1stWon - l_2ndWon, 
         rtpts = l_svpt) |>
  bind_rows(atpmatches |>
              mutate(player_name = loser_name, 
                     result = FALSE,
                     svpts_won = l_1stWon + l_2ndWon, 
                     svpts = l_svpt,
                     rtpts_won = w_svpt - w_1stWon - w_2ndWon, 
                     rtpts = w_svpt)) |> 
  #Now summarize player stats
  group_by(player_name) |>
  summarize(matches_played = n(), 
            matches_won = sum(result),
            totalwon_svpts = sum(svpts_won), 
            total_svpts = sum(svpts),
            totalwon_rtpts = sum(rtpts_won), 
            total_rtpts = sum(rtpts)) |>
  mutate(win_pct = matches_won / matches_played,
         svpts_pct = totalwon_svpts / total_svpts,
         rtpts_pct = totalwon_rtpts / total_rtpts) |>
  filter(player_name %in% qualified_players) #remove outliers

#Showing that both serve% and return% rise with win%
svrt_long <- bind_rows(player_stats |>
                         select(player_name, win_pct, value = svpts_pct) |>
                         mutate(stat = "Serve Win%"),
                       player_stats |>
                         select(player_name, win_pct, value = rtpts_pct) |>
                         mutate(stat = "Return Win%"))

ggplot(svrt_long, aes(x = value, y = win_pct)) +
  geom_point(aes(color = stat)) +
  scale_color_manual(name = "Predictor", 
                     values = c("Return Win%" = "darkorange",
                                "Serve Win%" = "steelblue")) +
  scale_x_continuous(labels = label_percent()) +
  scale_y_continuous(labels = label_percent()) +
  labs(title = "Return/Serve Win% vs Match Win%",
    x = "Return or Serve Win%",
    y = "Match Win%") +
  theme_minimal()
```



\begin{center}\includegraphics{Tennis-Simulator_files/figure-latex/Serve-Return Regression Model-1} \end{center}

``` r
#Model: win% as a function of serve% and return%
model <- lm(win_pct ~ svpts_pct + rtpts_pct, data = player_stats)
summary(model)
```

```
## 
## Call:
## lm(formula = win_pct ~ svpts_pct + rtpts_pct, data = player_stats)
## 
## Residuals:
##       Min        1Q    Median        3Q       Max 
## -0.079572 -0.016705 -0.001075  0.017613  0.094870 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -3.07310    0.05309  -57.88   <2e-16 ***
## svpts_pct    3.65319    0.05997   60.92   <2e-16 ***
## rtpts_pct    3.43729    0.06896   49.85   <2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.02627 on 310 degrees of freedom
## Multiple R-squared:  0.9357,	Adjusted R-squared:  0.9353 
## F-statistic:  2257 on 2 and 310 DF,  p-value: < 2.2e-16
```

Fortunately, $R^2 = 0.9357$ provides a solid sanity check that a player's results depend on both serving and returning. Moving on.

## Find Relevant Probabilities

Each player's `point_prob` has two core components: `serve_prob`, which is the share of their service points that they win, and `return_prob`, which is the share of their opponents' service points that they win. Both are derived from the train set.


``` r
#Find serve_prob for all qualified players
serve_prob <- c()

for (player in qualified_players) {
  player_matches <- atpmatches_train |> #use the same train split from Part 1
    filter(winner_name == player | loser_name == player) |>
    mutate(is_winner = ifelse(winner_name == player, TRUE, FALSE),
           svpt = ifelse(is_winner, w_svpt, l_svpt),
           first_won = ifelse(is_winner, w_1stWon, l_1stWon),
           second_won = ifelse(is_winner, w_2ndWon, l_2ndWon))
  
  total_svpts <- sum(player_matches$svpt)
  svpts_won <- sum(player_matches$first_won + player_matches$second_won)
  svwin_proportion <- svpts_won / total_svpts
  
  serve_prob <- c(serve_prob, svwin_proportion)
}

ggplot(data.frame(serve_prob), aes(x = serve_prob)) +
  geom_histogram(aes(y = after_stat(density)), 
                 bins = 30, 
                 fill = "steelblue", 
                 color = "white") +
  geom_density(color = "darkred", linewidth = 1) +
  geom_vline(xintercept = mean(serve_prob), 
             color = "black", 
             linetype = "dashed", 
             linewidth = .7) +
  scale_x_continuous(labels = label_percent()) +
  labs(title = "Distribution of Player Serve-Win Probabilities",
       x = "Serve-Win Probability", 
       y = "Density") +
  annotate("text", x = mean(serve_prob), y = Inf, 
           label = paste0("Mean = ", percent(mean(serve_prob), accuracy = .01)),
           vjust = 1.25, hjust = 1.05, size = 3.5) +
  theme_minimal()
```



\begin{center}\includegraphics{Tennis-Simulator_files/figure-latex/Find serve_probs-1} \end{center}

``` r
#Continue building train dataset
master_trainset$serve_prob <- serve_prob
head(master_trainset)
```

```
##             player point_prob serve_prob
## 1 Adrian Mannarino  0.4942384  0.6237983
## 2    Adrian Voinea  0.4855483  0.6043956
## 3  Agustin Calleri  0.5070830  0.6339232
## 4  Albert Montanes  0.4929014  0.6141838
## 5    Albert Portas  0.4864633  0.5910249
## 6     Albert Ramos  0.4957481  0.6159823
```

Serving gives each player a massive advantage as everyone wins a majority of their points on serve. The spread is also much wider than it is for `point_prob` highlighting the existence of players who specialize in the skill.


``` r
#Pretty identical to serve_prob but for returning strength
return_prob <- c()

for (player in qualified_players) {
  player_matches <- atpmatches_train |> #use the same train split from Part 1
    filter(winner_name == player | loser_name == player) |>
    mutate(is_winner = ifelse(winner_name == player, TRUE, FALSE),
      rtpt = ifelse(is_winner, l_svpt, w_svpt),
      first_won = ifelse(is_winner, l_1stWon, w_1stWon),
      second_won = ifelse(is_winner, l_2ndWon, w_2ndWon))
  
  total_rtpts <- sum(player_matches$rtpt)
  rtpts_won <- sum(player_matches$rtpt - 
                     player_matches$first_won - 
                     player_matches$second_won) #slightly different formula
  rtwin_proportion <- rtpts_won / total_rtpts
  
  return_prob <- c(return_prob, rtwin_proportion)
}

ggplot(data.frame(return_prob), aes(x = return_prob)) +
  geom_histogram(aes(y = after_stat(density)), 
                 bins = 30, 
                 fill = "steelblue", 
                 color = "white", 
                 alpha = .8) +
  geom_density(color = "darkred", linewidth = 1) +
  geom_vline(xintercept = mean(return_prob), 
             color = "black", 
             linetype = "dashed", 
             linewidth = .7) +
  scale_x_continuous(labels = label_percent()) +
  labs(title = "Distribution of Player Return-Win Probabilities",
       x = "Return-Win Probability", 
       y = "Density") +
  annotate("text", x = mean(return_prob), y = Inf, 
           label = paste0("Mean = ", percent(mean(return_prob), accuracy = .01)),
           vjust = 2, hjust = 1.05, size = 3.5) +
  theme_minimal()
```



\begin{center}\includegraphics{Tennis-Simulator_files/figure-latex/Find return_probs-1} \end{center}

``` r
#Continue building train dataset
master_trainset$return_prob <- return_prob
head(master_trainset)
```

```
##             player point_prob serve_prob return_prob
## 1 Adrian Mannarino  0.4942384  0.6237983   0.3645661
## 2    Adrian Voinea  0.4855483  0.6043956   0.3656259
## 3  Agustin Calleri  0.5070830  0.6339232   0.3838282
## 4  Albert Montanes  0.4929014  0.6141838   0.3710206
## 5    Albert Portas  0.4864633  0.5910249   0.3794466
## 6     Albert Ramos  0.4957481  0.6159823   0.3756151
```

Returning presents the exact opposite distribution with no player winning a majority of their points. Intuitively, that tracks since every point a player wins on serve is a point that their opponent loses on the return.

## Improving Match Simulation (pt. 2)

Looking at the match simulation from Part 1, the obvious problem that emerges is that a player's raw serve\% is earned against an average opponent. Against a specific opponent, it should fall if that opponent returns well, and rise if they return poorly. Barnett and Clarke (2005) handle this with a simple formula.

$$f_{ij} = f_t + (f_i - f_{av}) - (g_j - g_{av})$$
Where $f_{ij}$ is the probability that player $i$ wins a point on serve against player $j$, $f_i$ is player $i$'s serve win\%, $g_j$ is player $j$'s return win\%, $f_{av}$ and $g_{av}$ are the tour averages, and $f_t$ is the baseline serve win\% for the event (for this project, $f_t = f_{av}$).


``` r
tour_sv <- with(atpmatches_train, 
                sum(w_1stWon + w_2ndWon + l_1stWon + l_2ndWon) / 
                  sum(w_svpt + l_svpt))
tour_rt <- 1 - tour_sv

c(tour_serve = tour_sv, tour_return = tour_rt)
```

```
##  tour_serve tour_return 
##    0.630849    0.369151
```

Across the train set, servers win 63\% of points and returners win 37\%. To see these baselines in action, take a hypothetical match between Novak Djokovic and Rafael Nadal.


``` r
dj <- master_trainset[master_trainset$player == "Novak Djokovic", ]
rn <- master_trainset[master_trainset$player == "Rafael Nadal", ]

data.frame(server = c("Djokovic", "Nadal"),
           raw_serve = c(dj$serve_prob, rn$serve_prob),
           vs_other = c(tour_sv + (dj$serve_prob - tour_sv) - 
                          (rn$return_prob - tour_rt), 
                        tour_sv + (rn$serve_prob - tour_sv) - 
                          (dj$return_prob - tour_rt)))
```

```
##     server raw_serve  vs_other
## 1 Djokovic 0.6772424 0.6213181
## 2    Nadal 0.6722723 0.6198869
```

Note how both players serve win\% go down because they're playing a much stronger opponent than the average player.

Now, when we return to our original function, the core updates are 1) that each point now has a server and 2) that the winner of each point will now be decided by Barnett and Clarke's formula rather than compressing two players `point_prob` onto a $\frac{p_1}{p_1 + p_2}$ scale.


``` r
svrt_gamefunction <- function(p_server, game_server) {
  #Initialize the game scores for players 1 and 2
  p1_gamescore <- 0
  p2_gamescore <- 0
  
  #Probability that player 1 wins any given point in this game
  p1_pointprob <- if (game_server == "player 1") p_server else 1 - p_server
  
  #Play points until the game is won
  while (TRUE) {
    #Simulate a point with runif() (faster) and update the game score
    if (runif(1) < p1_pointprob) {
      p1_gamescore <- p1_gamescore + 1
    } else {
      p2_gamescore <- p2_gamescore + 1
    }
    
    #Check if a player has won
    if (p1_gamescore >= 4 && p1_gamescore - p2_gamescore >= 2) {
      return("player 1")
    } else if (p2_gamescore >= 4 && p2_gamescore - p1_gamescore >= 2) {
      return("player 2")
    }
  }
}

#Tiebreaks have their own serving order rules necessitating a separate function
svrt_tbfunction <- function(p1_servepoint, p2_servepoint, tb_server) {
  #Initialize the tiebreak scores
  p1_tbscore <- 0
  p2_tbscore <- 0
  points_served <- 0  #tracks total points served

  while (TRUE) {
    #Probability that player 1 wins this point, given who is serving it
    p_server <- if (tb_server == "player 1") p1_servepoint else p2_servepoint
    p1_pointprob <- if (tb_server == "player 1") p_server else 1 - p_server
    
    #Simulate and update the tiebreak score
    if (runif(1) < p1_pointprob) {
      p1_tbscore <- p1_tbscore + 1
    } else {
      p2_tbscore <- p2_tbscore + 1
    }
    
    #Check if the tiebreak has been won
    if (p1_tbscore >= 7 && p1_tbscore - p2_tbscore >= 2) {
      return("player 1")
    } else if (p2_tbscore >= 7 && p2_tbscore - p1_tbscore >= 2) {
      return("player 2")
    }
    
    #Serve order: First player serves once, then they alternate every two points
    points_served <- points_served + 1
    if (points_served == 1 || (points_served > 1 && points_served %% 2 == 1)) {
      tb_server <- if (tb_server == "player 1") "player 2" else "player 1"
    }
  }
}

svrt_setfunction <- function(p1_servepoint, p2_servepoint) {
  #Initialize the set scores for players 1 and 2
  p1_setscore <- 0
  p2_setscore <- 0
  
  game_server <- if (runif(1) < .5) "player 1" else "player 2"

  #Play games until the set is won
  while (TRUE) {
    p_server <- if (game_server == "player 1") p1_servepoint else p2_servepoint

    #Simulate a game
    game_winner <- svrt_gamefunction(p_server, game_server)

    #Update the set score
    if (game_winner == "player 1") {
      p1_setscore <- p1_setscore + 1
    } else {
      p2_setscore <- p2_setscore + 1
    }
    
    #Check if a player has won
    if (p1_setscore >= 6 && p1_setscore - p2_setscore >= 2) {
      return("player 1")
    } else if (p2_setscore >= 6 && p2_setscore - p1_setscore >= 2) {
      return("player 2")
    } else if (p1_setscore == 6 && p2_setscore == 6) {
      #Tiebreak: Simulate a tiebreak
      tb_server1 <- if (game_server == "player 1") "player 2" else "player 1"
      
      return(svrt_tbfunction(p1_servepoint, p2_servepoint, tb_server1))
    }
    
    #Alternate the server
    game_server <- if (game_server == "player 1") "player 2" else "player 1"
  }
}

svrt_matchfunction <- function(p1_stats, p2_stats, bestof, tour_sv, tour_rt) {
  #Set player data
  p1_servepoint <- tour_sv + (p1_stats$serve - tour_sv) - 
    (p2_stats$return - tour_rt)
  p2_servepoint <- tour_sv + (p2_stats$serve - tour_sv) - 
    (p1_stats$return - tour_rt)
  
  #Determine the number of sets needed to win
  setstowin <- ceiling(bestof / 2)
  
  #Initialize match scores for players 1 and 2
  p1_matchscore <- 0
  p2_matchscore <- 0

  while (TRUE) {
    #Simulate a set
    set_winner <- svrt_setfunction(p1_servepoint, p2_servepoint)
    
    #Update the match score
    if (set_winner == "player 1") {
      p1_matchscore <- p1_matchscore + 1
    } else {
      p2_matchscore <- p2_matchscore + 1
    }
    
    #Check if a player has won the match
    if (p1_matchscore == setstowin || p2_matchscore == setstowin) {
      return(if (p1_matchscore > p2_matchscore) "player 1" else "player 2")
    }
  }
}
```

## Serve/Return Simulation

This section works exactly like Part 1 as it uses the same test matches (50 simulations each) in the same format. However, this time it uses both players' serve and return probabilities rather than their overarching `point_prob`.


``` r
#Vector for serve/return probabilities
sv <- setNames(master_trainset$serve_prob, master_trainset$player)
rt <- setNames(master_trainset$return_prob, master_trainset$player)

#Define a similar function as the base model but using serve/return
num_sims <- 50 #same as base model chunk

svrtsim <- function(player) {
  player_stats <- list(name = player, serve = sv[[player]], return = rt[[player]])
  testset_matches <- atpmatches_test |>
    filter(winner_name == player | loser_name == player) |>
    mutate(opp = ifelse(winner_name == player, loser_name, winner_name))
  
  sim_results <- logical(nrow(testset_matches) * num_sims) 
  k <- 0 #preallocate and establish index
  
  for (i in 1:nrow(testset_matches)) {
    opp <- testset_matches$opp[i]
    opp_stats <- list(name = opp, serve = sv[[opp]], return = rt[[opp]])
    
    bestof_i <- testset_matches$best_of[i]
    
    for (s in 1:num_sims) {
      match_result <- svrt_matchfunction(player_stats, opp_stats, bestof_i, 
                                         tour_sv, tour_rt)
      k <- k + 1
      sim_results[k] <- match_result == "player 1"
    }
  }
  
  mean(sim_results)
}

#Add the win% from simulating matches with the serve/return model
master_testset$svrtsim_winpct <- unname(sapply(master_testset$player, svrtsim))
head(master_testset)
```

```
## # A tibble: 6 x 5
##   player           test_matches real_winpct basesim_winpct svrtsim_winpct
##   <chr>                   <int>       <dbl>          <dbl>          <dbl>
## 1 Adrian Mannarino          128       0.422          0.458          0.431
## 2 Adrian Voinea              16       0.375          0.38           0.352
## 3 Agustin Calleri            49       0.408          0.525          0.553
## 4 Albert Montanes            72       0.347          0.456          0.411
## 5 Albert Portas              26       0.231          0.407          0.293
## 6 Albert Ramos              103       0.369          0.455          0.415
```

## Analyze Serve/Return Simulation Accuracy


``` r
svrt_data <- master_testset |>
  select(test_matches, real_winpct, svrtsim_winpct)

ggplot(svrt_data, aes(x = svrtsim_winpct, y = real_winpct)) +
  geom_point(color = "darkorange") +
  geom_smooth(aes(weight = test_matches),
              method = "lm",
              se = FALSE,
              linetype = "dashed",
              color = "darkred") +
  geom_abline(slope = 1, 
              intercept = 0,
              color = "black", 
              linetype = "dashed",
              linewidth = .8) +
  scale_x_continuous(labels = label_percent()) +
  scale_y_continuous(labels = label_percent()) +
  labs(title = "Serve/Return Model: Simulated vs Actual Match Win %",
       x = "Simulated Win % (serve/return model)",
       y = "Actual Win % (held-out matches)") +
  theme_minimal()
```



\begin{center}\includegraphics{Tennis-Simulator_files/figure-latex/Analyze SVRT Model Accuracy-1} \end{center}

``` r
svrt_model <- lm(real_winpct ~ svrtsim_winpct, data = svrt_data, 
                 weights = test_matches) #similar regression as base model
summary(svrt_model)
```

```
## 
## Call:
## lm(formula = real_winpct ~ svrtsim_winpct, data = svrt_data, 
##     weights = test_matches)
## 
## Weighted Residuals:
##      Min       1Q   Median       3Q      Max 
## -1.57806 -0.37522 -0.02072  0.32434  1.80568 
## 
## Coefficients:
##                Estimate Std. Error t value Pr(>|t|)    
## (Intercept)     0.05671    0.01703   3.331  0.00097 ***
## svrtsim_winpct  0.88750    0.03302  26.881  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.5632 on 311 degrees of freedom
## Multiple R-squared:  0.6991,	Adjusted R-squared:  0.6981 
## F-statistic: 722.6 on 1 and 311 DF,  p-value: < 2.2e-16
```

Compared to the base model, the serve/return model has definitely improved to the point of over-correcting and being a touch too generous. The dashed red line of best fit now sits close to the diagonal and the simulated win\% now covers roughly the same range as the real results. 

Accounting for serve and return is the more realistic way to model a match, but it doesn't fix everything. That points to Part 3: a players serve and return aren't the same on clay as on grass.

# Part 3 -- Surface (*still in progress*)

# Part 4 -- Future Work

If I end up returning to this project (or if someone else wants to pick up where I left off), I've identified three areas for possible future work.

## Age


``` r
#Limiting analysis to the 6 men to win grand slams in the 2010s
players <- c('Novak Djokovic', 'Rafael Nadal', 'Roger Federer',
             'Andy Murray', 'Stan Wawrinka', 'Marin Cilic')

#Point_prob per match
age_matches <- bind_rows(
  atpmatches |> transmute(player = winner_name, age = winner_age,
                          pts_won = w_1stWon + w_2ndWon + 
                            l_svpt - l_1stWon - l_2ndWon,
                          pts = w_svpt + l_svpt),
  atpmatches |> transmute(player = loser_name, age = loser_age,
                          pts_won = l_1stWon + l_2ndWon + 
                            w_svpt - w_1stWon - w_2ndWon,
                          pts = w_svpt + l_svpt)) |>
  filter(player %in% players) |>
  mutate(point_prob = pts_won / pts)

#Pooled point_prob per year of age
age_curve <- age_matches |>
  group_by(player, age = floor(age)) |>
  summarize(point_prob = sum(pts_won) / sum(pts))

#Plot both
ggplot(age_matches, aes(x = age, y = point_prob)) +
  geom_point(alpha = .5, 
             color = "grey") +
  geom_line(data = age_curve, 
            color = "red") +
  facet_wrap(~ player) +
  scale_y_continuous(labels = label_percent()) +
  labs(title = "Point-Win % by Age",
       subtitle = "Grey: Individual Matches; Red: Pooled Avg% per Year",
       x = "Age", 
       y = "Point-Win %") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = .5))
```



\begin{center}\includegraphics{Tennis-Simulator_files/figure-latex/Age-1} \end{center}

Note the curve for every player. Every player's performance takes a notable dip towards the end of their careers (except Djokovic, who's still playing at an elite level). A more accurate simulation wouldn't assume that a player's `point_prob` stays consistent throughout their career.

## Clutch Points 


``` r
#Stats on break points
bp_stats <- bind_rows(atpmatches |> transmute(player = winner_name, 
                                              bp_faced = w_bpFaced, 
                                              bp_saved = w_bpSaved,
                                              svpt = w_svpt, 
                                              sv_won = w_1stWon + w_2ndWon),
                      atpmatches |> transmute(player = loser_name, 
                                              bp_faced = l_bpFaced, 
                                              bp_saved = l_bpSaved,
                                              svpt = l_svpt, 
                                              sv_won = l_1stWon + l_2ndWon)) |>
  filter(player %in% qualified_players) |>
  group_by(player) |>
  summarize(bpsaved_pct = sum(bp_saved) / sum(bp_faced),
            serve_pct = sum(sv_won) / sum(svpt))

#Plot
ggplot(bp_stats, aes(x = serve_pct, y = bpsaved_pct)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", 
              se = FALSE, 
              color = "black", 
              linetype = "dashed") +
  scale_x_continuous(labels = label_percent()) +
  scale_y_continuous(labels = label_percent()) +
  labs(title = "Serve Win% vs Break Point Save%",
       x = "Overall Serve Win%", 
       y = "Break Point Save%") +
  theme_minimal()
```



\begin{center}\includegraphics{Tennis-Simulator_files/figure-latex/Clutch Points-1} \end{center}

Although break point save\% is heavily correlated with overall serve win\%, there are a few players who rise clearly above the trend line. A future analysis would likely examine how much of that is skill versus noise and what effect that would have on their win\%.

## Specific Opponents


``` r
#Match level point_probs
matches <- bind_rows(
  atpmatches |> transmute(player = winner_name, opponent = loser_name,
                          point_prob = (w_1stWon + w_2ndWon + l_svpt - l_1stWon 
                                        - l_2ndWon) / (w_svpt + l_svpt)),
  atpmatches |> transmute(player = loser_name, opponent = winner_name,
                          point_prob = (l_1stWon + l_2ndWon + w_svpt - w_1stWon 
                                        - w_2ndWon) / (w_svpt + l_svpt)))

overall <- matches |> 
  filter(player %in% players) |>
  group_by(player) |> 
  summarize(overall_pp = mean(point_prob)) #each player's avg% overall

h2h <- matches |> 
  filter(player %in% players, opponent %in% players) |> #same players as Age
  mutate(opponent = sub(".* ", "", opponent)) #last names only so x labels fit

h2h_means <- h2h |> 
  group_by(player, opponent) |>
  summarize(mean_pp = mean(point_prob)) #each player's avg% vs specific opp

#Combine into one plot
ggplot(h2h, aes(x = opponent, y = point_prob)) +
  geom_jitter(width = .1, 
              alpha = .4) +
  geom_point(data = h2h_means, 
             aes(y = mean_pp), 
             color = "red", 
             size = 2) +
  geom_hline(data = overall, 
             aes(yintercept = overall_pp), 
             linetype = "dashed") +
  facet_wrap(~ player, 
             ncol = 2, 
             scales = "free_x") +
  scale_y_continuous(limits = c(.275, .725),
                     breaks = seq(.3, .7, .1),
                     labels = label_percent()) +
  labs(title = "point_prob vs the Champions of the 2010s",
       subtitle = "Red: Avg% vs Opponent; Dashed: Avg% Overall",
       x = "Opponent", 
       y = "point_prob") +
theme_minimal() +
theme(plot.title = element_text(hjust = .5),
      strip.text = element_text(face = "bold", size = 11),
      panel.spacing = unit(1.2, "lines"))
```



\begin{center}\includegraphics{Tennis-Simulator_files/figure-latex/Specific Opponents-1} \end{center}

Many players perform measurably different against certain opponents than they do the general field. It would be interesting to explore just how much better and just how many opponents.

# Attributions

This analysis uses data by Jeff Sackmann/Tennis Abstract:

- **ATP match results (2000–2024):** [tennis_atp](https://github.com/JeffSackmann/tennis_atp)
- This dataset is licensed under a
[Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-nc-sa/4.0/).

Barnett, T and Clarke, S (2005). Combining player statistics to predict outcomes of tennis matches. *IMA Journal of Management Mathematics*, 16(2), 113–120.
