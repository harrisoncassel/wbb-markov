library('wehoop')

# Try to load other source file; give warning if unable
load.result <- tryCatch({
    source('scripts/data_management.R')
  }, warning = function(w) {
    return(w)
  }, error = function(e) {
    load2.result <- tryCatch({
      source('https://raw.githubusercontent.com/harrisoncassel/wbb-markov/main/R/data_management.R')
    }, warning = function(w) {
      return(w)
    }, error = function(e) {
      msg <- paste('WARNING: data_management.R NOT LOADED! MOST FUNCTIONS WILL NOT OPERATE PROPERLY!', 'SPECIFIC ERROR:', e, sep='\n')
      return(msg)
    })
  })

get_true_team_box <- function(team.id, pbp=NULL, seasons=NULL, game.id=NULL, game.season=NULL) {
  ### Returns table. Gets the actual state count (as table) for a given team (set to team a)
  ### for a set of seasons (or season) or a specific game.
    
    ## Only confident this works when using the seasons argument at the moment (at least when using this with sim_season_diagnostic)
    
  # team.id := EITHER:
    # A) the ESPN team id for Team A (team of interest) -- IF the pbp argument is an un-cleaned pbp dataset OR using one of the other options
    # B) for a cleaned pbp dataset
  # pbp := if passed, ignores all following arguments. A pbp to get stats from
  # seasons := an array of seasons (YYYY format, >= 2004) to get PBP data from
  # game.id := a specific game_id to get data for. This argument will be
  #            ignored if something is passed to seasons. Requires game.season
  # game.season := season in qhich game.id took place
  
  if (!is.null(pbp)) { # Use the given pbp data -- affects team_games()
    pbp <- team_games(team.id, pbp=pbp) # from data_management.R
    tab.order <- c(6, 10, 9, 7, 8, 5, 4, 3, 2, 1)
    
    team <- table(pbp[pbp$team_id=='A', ])[tab.order]
    opp <- table(pbp[pbp$team_id=='B', ])[tab.order]
    tab <- c(team, opp)
    
  } else if (!is.null(seasons)) {
    for (season in seasons) {
      pbp <- team_games(team.id, seasons=season)
      
      tab.order <- c(6, 10, 9, 7, 8, 5, 4, 3, 2, 1)
      unsorted.tab <- table(pbp$team_id, pbp$type_text)
      tab <- unsorted.tab[, tab.order]
    }
  } else { # this is the specific-game case. Load new pbp, then filter by game_id
    pbp <- as.data.frame(load_wbb_pbp(game.season))
    game <- pbp[pbp$game_id == game.id, ]
    pbp <- team_games(team.id, pbp=game)
    tab.order <- c(6, 10, 9, 7, 8, 5, 4, 3, 2, 1)
    
    team <- table(pbp[pbp$team_id=='A', ])[tab.order]
    opp <- table(pbp[pbp$team_id=='B', ])[tab.order]
    tab <- c(team, opp)
  }
  
  names(tab) <- c('A-FT', 'A-2pt', 'A-3pt', 'A-MissShot', 'A-OREB',
                         'A-TO', 'A-Inbound', 'A-Foul', 'A-DREB', 'A-Block',
                         'B-FT', 'B-2pt', 'B-3pt', 'B-MissShot', 'B-OREB',
                         'B-TO', 'B-Inbound', 'B-Foul', 'B-DREB', 'B-Block')
  return(tab)
}

test_chain_season_accuracy <- function(team.ids, seasons, n.steps.per.game=310, n.iter=1000, regular.season.only=TRUE) {
  ### Returns data.frame of squared error of each simulated season vs. reality
  ### Uses sim_season_diagnostic() from diagnostics.R
  # team.ids := vector (or single int) of ESPN team ids for of teams of interest
  # seasons := vector (or single int) of seasons (YYYY format) to simulate/test
  # n.steps.per.game := number of state transitions per simulated game
  # n.iter := number of sims for each season for each team
  # regular.season.only := if TRUE, train and test *only* on regular season games
  result <- data.frame()
  
  for (season in seasons) { # each desired season
    season.pbp <- load_wbb_pbp(season)
    for (team in team.ids) { # each "team-season"
      true.box <- get_true_team_box(team, pbp=season.pbp)
      pbp <- team_games(team, pbp=season.pbp)
      season.results <- sim_season_diagnostic(true.box, team, season, n.steps.per.game, n.iter, regular.season.only)
      # ^ from diagnostics.R
      result <- rbind(result, season.results)
      
      # Print status message
      msg <- paste('SEASON COMPLETE:', season, '-', team)
      print(msg)
    }
  }
  # Setup result data.frame -- can't 100% remember what the "???" col is
  cols <- c('FT', '2pt', '3pt', 'miss', 'OREB', 'TO', 'Inbound', 'Foul', 'DREB', 'Blk')
  col.names <- c( paste('A-',cols,sep=''), paste('B-',cols,sep='')  )
  col.names <- c('???', 'Season', 'Team')
  return(result)
}

min.max.scale <- function(season.test.results) {
  ### Returnes data.frame where every column is max-min scaled to be on [0,1]
  # df := result from test_chain_season_accuracy()...or any all-numerical data.frame, I think
  df <- sqrt(season.test.results)
  
  maxs <- sapply(df, max)
  mins <- sapply(df, min)
  
  for (i in 1:length(maxs)) {
    df[,i] <- df[,i] / (  maxs[i] - mins[i] )
  }
  return(df)
}








