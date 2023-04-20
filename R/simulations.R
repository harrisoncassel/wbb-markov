### Markov Chain simulation functions

simulate_simple_game <- function(trans.mat, n.steps, score=TRUE) {
  ### Tuned to 20-state game as it worked on 4/10/2023 at 11:20pm
  # score := TRUE to return data.frame of simulated score; FALSE to
  #           return boolean indicating if the home team won
  
  # Initialize variables
  home.score <- 0
  away.score <- 0
  previous.state <- sample(c(6,16), 1) # initial state
  # ^ format_events_and_teams() counts a won jumpball as a team inbound, so
  # This selects one random team to "win" the beginning jumpball and have the
  # first posession
  
  # Simulate game
  for (i in 1:n.steps) {
    next.state <- sample(1:20, 1, prob=trans.mat[previous.state, ])
    
    if (next.state %in% c(1,2,3)) {
      home.score <- home.score + next.state # since state # = added score
    } else if (next.state %in% c(11,12,13)) {
      away.score <- away.score + (next.state - 10)
    }
    
    previous.state <- next.state
  }
  
  # Return result
  if (score == TRUE) {
    result <- data.frame(home=home.score, away=away.score)
    return(result)
  } else {
    result <- TRUE
    if (home.score < away.score) {result <- FALSE}
    return(result)
  }
}

wl_multi_simple_sim <- function(trans.mat, n.sims=1, n.games=1000, n.steps=300, score=FALSE) {
  ### Returns list of simulated W/L outcomes for the home team (Team A)
  # n.steps = 135 is definitely an underestimate (average is soemthing like
  #           318 transitions per game), but this gets predicted scores to be
  #           more reasonable (66-63 avg scores, compared to 68-60 2023 avg)
  #           with only a slight decrease in accuracy (under-estimates home
  #           advantage by just under 2 % points in the all-2023 test case).
  results <- c()
  for (j in 1:n.sims) {
    total <- 0
    
    if (!score) {
      for (i in 1:n.games) {
        game.outcome <- simulate_simple_game(trans.mat, n.steps, score=FALSE)
        total <- total + game.outcome
      }
      current <- total / n.games
      results <- c(results, current)
    } else {
      results <- data.frame()
      for (i in 1:n.games) {
        new.result <- simulate_simple_game(trans.mat, n.steps, score=TRUE)
        results <- rbind(results, new.result)
      }
    }
    print(j)
  }
  return(results)
}

multi_sim <- function(trans.mat, n.sims=1, n.games=1000, n.steps=300) {
  ## Returns a list of dataframes. Each dataframe is a set of outcomes
  # From a different "set" of simulations. Each row is the "stat line"
  # (count of state occurrences) for each simulated game.
  
  previous.state <- sample(c(6,16), 1) # initial state
  # ^ format_events_and_teams() counts a won jumpball as a team inbound, so
  # This selects one random team to "win" the beginning jumpball and have the
  # first posession
  
  df.list <- list()
  
  for (i in 1:n.sims) {
    state.counts <- data.frame(matrix(ncol=21)) # 21 will be the W/L bool variable
    for (j in 1:n.games) {
      game.state.counts <- rep(0, 21) # 21 will be the W/L bool variable
      game.state.counts[previous.state] <- 1 # +1 for initial state
      a.score <- 0
      b.score <- 0
      for (k in 1:n.steps) {
        next.state <- sample(1:nrow(trans.mat), 1, prob=trans.mat[previous.state,])
        game.state.counts[next.state] <- game.state.counts[next.state] + 1
        previous.state <- next.state
        
        if (next.state %in% c(1,2,3)) {
          a.score <- a.score + next.state
        } else if (next.state %in% c(11,12,13)) {
          b.score <- b.score + (next.state - 10)
        }
      }
      a.win <- TRUE
      if (b.score > a.score) {a.win = FALSE}
      game.state.counts[21] <- a.win
      state.counts <- rbind(state.counts, game.state.counts)
    }
    df.list[[i]] <- state.counts
  }
  return(df.list)
}

sim_season <- function(team.a.id, year, n.steps.per.game=310, n.iter=1000, regular.season.only=TRUE) {
  team.a.id <- as.integer(team.a.id)
  year <- as.integer(year)
  results.df <- data.frame()
  
  # Load the PBP data
  pbp <- load_wbb_pbp(year)
  if (regular.season.only) {
    sel.reg.season <- which(pbp$season_type==2)
    pbp <- pbp[sel.reg.season, ]
  }
  sel <- which(pbp$home_team_id == team.a.id | pbp$away_team_id == team.a.id)
  pbp.team <- pbp[sel, ]

  # Make Team A's transition matrix
  games <- team_games(team.a.id, pbp=pbp.team)
  team.matrix <- matrix(data=0, nrow=20, ncol=20)
  team.matrix <- update_transition_matrix(team.matrix, games)
  team.matrix <- make_transition_probs(team.matrix)
  
  # Select indexes for each game (Team A)
  opp.ids <- c()
  comp.mats <- list()
  game.ids <- unique(pbp.team$game_id)
  print('teams')
  for (game in 1:length(game.ids)) {
    # Select game of interest, then non-team-A opponent
    game.id <- game.ids[game]
    matchup <- pbp[pbp$game_id==game.id, ]
    matchup <- unique(matchup$team_id)
    matchup <- matchup[!is.na(matchup)]
    opp.id <- matchup[matchup!=team.a.id]
    opp.ids <- c(opp.ids, opp.id) # Add to running list of opponent ids
    
    # Get opponent's PBP data
    sel <- which(pbp$home_team_id == opp.id | pbp$away_team_id == opp.id)
    opp.pbp <- pbp[sel, ]
    opp.pbp <- team_games(opp.id, pbp=opp.pbp)

    # Create opponent transition matrices
    opp.mat <- matrix(data=0, nrow=20, ncol=20)
    opp.mat <- update_transition_matrix(opp.mat, opp.pbp)
    opp.mat <- make_transition_probs(opp.mat)
    
    # Create comparison matrices
    comp.mat <- create_comparison_matrix(team.matrix, opp.mat)
    
    # Add to list
    comp.mats[[game]] <- comp.mat
    print(game)
  }
  print('sims')
  for (sim in 1:n.iter) {
    # Run simulation for each season
    stat.line <- matrix(data=0, nrow=1, ncol=21)
    for (comp.mat in comp.mats) {
      # Simulate game
      #result <- simulate_simple_game(comp.mat, n.steps=310, score=FALSE)
      stat.result <- multi_sim(comp.mat, n.sims=1, n.games=1, n.steps=n.steps.per.game)[[1]][2,] # stat line
      stat.line <- stat.line + stat.result
    }
    results.df <- rbind(results.df, stat.line)
    
    # Status update
    msg <- paste('Completed simulation: ', sim, '/', n.iter, sep='')
    print(msg)
  }

  names(results.df) <- c('A-FT', 'A-2pt', 'A-3pt', 'A-MissShot', 'A-OREB',
                         'A-TO', 'A-Inbound', 'A-Foul', 'A-DREB', 'A-Block',
                         'B-FT', 'B-2pt', 'B-3pt', 'B-MissShot', 'B-OREB',
                         'B-TO', 'B-Inbound', 'B-Foul', 'B-DREB', 'B-Block',
                         'Wins')
  return(results.df)
}

sim_season_diagnostic <- function(true.season.box, team.a.id, year, n.steps.per.game=310, n.iter=1000, regular.season.only=TRUE) {
  ### Return df of each var for each run's SQUARE DISTANCE from the true value
  # true.season.box := retrieved using diagnostics::get_true_team_box()
  # team.a.id := integer value ESPN ID for team of interest
  # year := year of interest (needs to be same year as that of true.season.box, obviously)
  # n.steps.per.game := the number of state transitions per simulation game
  # n.iter := how many times to simulate the season
  # regular.season.only := TRUE means chains are only trained on reg season data and only reg season games are simulated
  
  team.a.id <- as.integer(team.a.id)
  year <- as.integer(year)
  results.df <- data.frame()
  
  # Load the PBP data
  pbp <- load_wbb_pbp(year)
  if (regular.season.only) {
    sel.reg.season <- which(pbp$season_type==2)
    pbp <- pbp[sel.reg.season, ]
  }
  sel <- which(pbp$home_team_id == team.a.id | pbp$away_team_id == team.a.id)
  pbp.team <- pbp[sel, ]
  
  # Make Team A's transition matrix
  games <- team_games(team.a.id, pbp=pbp.team)
  team.matrix <- matrix(data=0, nrow=20, ncol=20)
  team.matrix <- update_transition_matrix(team.matrix, games)
  team.matrix <- make_transition_probs(team.matrix)
  
  # Select indexes for each game (Team A)
  opp.ids <- c()
  comp.mats <- list()
  game.ids <- unique(pbp.team$game_id)
  for (game in 1:length(game.ids)) {
    # Select game of interest, then non-team A opponent
    game.id <- game.ids[game]
    matchup <- pbp[pbp$game_id==game.id, ]
    matchup <- unique(matchup$team_id)
    matchup <- matchup[!is.na(matchup)]
    opp.id <- matchup[matchup!=team.a.id]
    opp.ids <- c(opp.ids, opp.id) # Add to running list of opponent ids
    
    # Get opponent's PBP data
    sel <- which(pbp$home_team_id == opp.id | pbp$away_team_id == opp.id)
    opp.pbp <- pbp[sel, ]
    opp.pbp <- team_games(opp.id, pbp=opp.pbp)
    
    # Create opponent transition matrices
    opp.mat <- matrix(data=0, nrow=20, ncol=20)
    opp.mat <- update_transition_matrix(opp.mat, opp.pbp)
    opp.mat <- make_transition_probs(opp.mat)
    
    # Create comparison matrices
    comp.mat <- create_comparison_matrix(team.matrix, opp.mat)
    
    # Add to list
    comp.mats[[game]] <- comp.mat
  }
  
  for (sim in 1:n.iter) {
    # Run simulation for each season
    stat.line <- matrix(data=0, nrow=1, ncol=21)
    for (comp.mat in comp.mats) {
      # Simulate game
      #result <- simulate_simple_game(comp.mat, n.steps=310, score=FALSE)
      stat.result <- multi_sim(comp.mat, n.sims=1, n.games=1, n.steps=n.steps.per.game)[[1]][2,] # stat line
      stat.line <- stat.line + stat.result
    }
    stat.difference <- (stat.line - true.season.box)**2 # (from argument) -- find squared difference in predicted vs. actual
    results.df <- rbind(results.df, stat.difference)
    
    # Status update
    msg <- paste('Completed simulation: ', sim, '/', n.iter, sep='')
    print(msg)
  }
  results.df[,'Season'] = year
  results.df[,'Team'] = team.a.id
  return(results.df)
}



