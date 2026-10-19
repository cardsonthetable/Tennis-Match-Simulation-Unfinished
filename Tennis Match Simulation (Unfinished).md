---
title: "Tennis Match Simulation (Unfinished)"
author: "Isaac Stiepleman"
date: "October 14, 2026"
output:
  pdf_document:
    keep_md: TRUE
fontsize: 11pt
geometry: margin = 1in
---



# Introduction

In the Spring of 2023, I took a class at Columbia called Applied Probability and Stochastic Processes with Dr. Sumit Mukherjee. For our final project, Professor Mukherjee let us choose from a series of articles to summarize and present to the class. I chose one titled "Probability and Tennis" published in *Data Genetics* which calculated a player's probability of winning a match based on the probability that they would win any individual point (moving forward I will define that probability within the code and the commentary as *point_prob*).

As the paper notes, "as a match of tennis contains many point battles, and tennis has some quirky score advancing rules; these concentrate the advantage that a more skillful player has to win the match." In my initial project, I was interested to see if I could build a Monte Carlo simulation based on real-life matches that supported the article's conclusions of a concentrated advantage. In the years since that presentation, I have tried to enhance that simulation.


``` r
suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(ggplot2)
})
```

# Original Project Recap

The following is what I presented on April 24, 2023. 

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
  labs(title = "The Compounding Probablistic Effects of Winning Individual Points", 
       x = "Probability of Winning a Point", 
       y = "Additional Winning Probabilities") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = .25))
```



See `Graph of Compounding Probabilities`

Note how the succeeding lines compress excessively around the midpoint. A player with merely a $60\%$ probability of winning a point is already virtually guaranteed to win the match.

Next I moved to my own function.


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
    if (player1_gamescore >= 4 && player1_gamescore - player2_gamescore >= 2) {
      return("player 1")
    } else if (player2_gamescore >= 4 && player2_gamescore - player1_gamescore >= 2) {
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
    if (player1_setscore >= 6 && player1_setscore - player2_setscore >= 2) {
      return(list(winner = "player 1", `player 1 score` = player1_setscore, 
                  `player 2 score` = player2_setscore))
    } else if (player2_setscore >= 6 && player2_setscore - player1_setscore >= 2) {
      return(list(winner = "player 2", `player 1 score` = player1_setscore, 
                  `player 2 score` = player2_setscore))
    } else if (player1_setscore == 6 && player2_setscore == 6) {
      #Tiebreak: a player needs to win at least 7 points AND win by 2 to win the set
      
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
          
          return(list(winner = "player 1", `player 1 score` = 7, `player 2 score` = 6, 
                      `player 1 tiebreak score` = player1_tiebreakscore, 
                      `player 2 tiebreak score` = player2_tiebreakscore))
        } else if (player2_tiebreakscore >= 7 && 
                   player2_tiebreakscore - player1_tiebreakscore >= 2) {
          return(list(winner = "player 2", `player 1 score` = 6, `player 2 score` = 7, 
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
  set_scores <- list()
  
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
    
    # Add the set scores to the set_scores list
    set_scores[[length(set_scores) + 1]] <- c(set_result$`player 1 score`, 
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

It took awhile to build a function that was consistent with the scoring rules of tennis but it was gratifying to successfully complete.

In my original presentation, I simulated specific and iconic Grand Slam Finals (2012 Australian Open, 2008 and 2019 Wimbledon) as case studies. In recreating this, I've chosen to replace those specific matches with a graph that demonstrats the converging stability of the simulation across 10, 100, and 1000 sims of each point_prob.


``` r
#Sequence of probabilities for testing, concentrated around breakeven
probs <- seq(.4, .6, .001)

results <- data.frame()

for (i in c(10, 100, 1000)) {#outer loop for each simulation threshold
  win_pct <- c()
  
  for (k in seq_along(probs)) {#middle loop for each probability
    winners <- c()
    
    for (j in 1:i) {#inner loop for i simulations
      winners[j] <- simulate_match(probs[k])$winner #using my MC simulation
    }
    
    win_pct[k] <- mean(winners == "player 1")
  }
  
  results <- rbind(results, data.frame(probs, win_pct, sims = paste(i, "sims")))
}

results <- rbind(results, data.frame(probs, #add the exact paper calculations
                                     win_pct = match_prob[probs * 1000 + 1],
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
  labs(title = "Win Proportion by Point Probability",
       subtitle = "Simulated curves converge on the exact match win proportion",
       x = "Player point_prob",
       y = "Proportion of Wins") +
  theme_minimal()
```



See `Graph of Win Proportions`

Note the red dashed line which indicates the exact calculated probability from the first graph.
