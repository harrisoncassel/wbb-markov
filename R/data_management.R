library('wehoop')

format_events_and_teams <- function(pbp, team.a.id) {
  ### Returns a data.frame formatted for analysis.
  # pbp := data downloaded from wehoop::load_wbb_pbp()
  # team.a.id := team_id for Team A (home team or team of interest)
  # Since cleanings reduces PBP data to two teams, Team B is inferred as !A
  
  df <- as.data.frame(pbp)
  
  # Make inbound state: jumpball, timeouts
  sel.inbound <- c('Jumpball', 'RegularTimeOut', 'ShortTimeOut')
  df$type_text[df$type_text %in% sel.inbound] <- 'Inbound'
  
  # Combine fouls
  sel.fouls <- c('PersonalFoul', 'Technical Foul')
  df$type_text[df$type_text %in% sel.fouls] <- 'Foul'
  
  # Assign point-scoring events based on the score_value column
  sel.ft <- which(df$score_value == 1 & df$scoring_play == TRUE) 
  df$type_text[sel.ft] <- 'MadeFreeThrow'
  sel.missft <- which(df$score_value == 1 & df$scoring_play == FALSE)
  df <- df[-sel.missft, ] # Missed free throws not in state space
  sel.2pt <- which(df$score_value == 2 & df$scoring_play == TRUE)
  df$type_text[sel.2pt] <- 'TwoPoint'
  sel.3pt <- which(df$score_value == 3 & df$scoring_play == TRUE)
  df$type_text[sel.3pt] <- 'ThreePoint'
  
  # Missed shots
  sel.missedshot <- which(df$score_value > 1 & df$scoring_play == FALSE)
  df$type_text[sel.missedshot] <- 'Missed Shot'
  
  # Remove unneeded events
  sel.remove <- c('JumpShot', 'End Game', 'End Period', 'Not Available', 'OfficialTVTimeOut', 'Dead Ball Rebound', 'Steal')
  df <- df[!(df$type_text %in% sel.remove),]
  
  # Remove null events
  sel <- which(is.null(df$type_text) | is.na(df$type_text) | is.null(df$team_id) | is.na(df$team_id))
  if (length(sel) > 0) {df <- df[-sel, ]}
  
  # Remove double-counted missed/blocked shots -- these are recorded together. Block shot is more interesting.
  sel <- c()
  for (i in 2:nrow(df)) {
    if (df$type_text[i] == 'Block Shot') {
      if(df$type_text[i-1] == 'Missed Shot') {
        sel <- c(sel, (i-1))
      }
    }
  }
  df <- df[-sel, ]
  
  
  # Format teams into A/B (easier to generalize) -- A is team of interest
  df$team_id <- as.integer(df$team_id)
  team.a.actions <- which(df$team_id == as.integer(team.a.id))
  df[team.a.actions, ]$team_id <- rep('A', length(df[team.a.actions, ]$team_id))
  df[-team.a.actions, ]$team_id <- rep('B', length(df[-team.a.actions, ]$team_id))
  
  return(df)
}

select_cols <- function(df) {
  ### Returns a data.frame with only the type_text and team columns
  # df := data.frame returned by format_events_and_teams()
  cols <- c('team_id','type_text')
  df <- df[,cols]
  return(df)
}

all_cleaning <- function(df, a.team.id) {
  ## Returns a data.frame cleaned by all cleaning functions -- same args as above
  df <- format_events_and_teams(df, a.team.id)
  df <- select_cols(df)
  return(df)
}

team_games <- function(team.id, pbp=NULL, seasons=NULL) {
  ### Returns a data.frame...really just a glorified all_cleaning
  ### Collects and cleans all PBP data for a given season for a given team.
  # pbp := data.frame downloaded from wehoop::load_wbb_pbp()
  # team.id := ESPN team_id code
  # seasons := array of seasons (YYYY format, >= 2004) to collect PBP data for.
  #             inputting seasons will ignore the pbp argument and will use
  #             wehoop::load_wbb_pbp() to get data for each desired season.
  
  # If seasons are given, use wehoop::load_wbb_pbp to get season-by-season PBP data
  if (!is.null(seasons)) {
    df <- data.frame() # final results data.frame
    for (season in seasons) { # Have to cycle through seasons since data becomes huge
      # Load season data
      pbp <- load_wbb_pbp(as.integer(season)) # wehoop
      new.data <- data.frame(pbp)
      
      # Get data of interest & add to final data.frame
      sel <- which(new.data$home_team_id == team.id | new.data$away_team_id == team.id)
      new.data <- new.data[sel, ]
      new.data <- all_cleaning(new.data, team.id)
      df <- rbind(df, new.data)
      
      # Print status update
      msg <- paste('Loaded and cleaned season ', season, '/', seasons, sep='')
      print(msg)
    }
  } else { # this assumes a pbp data.frame was given
    df <- as.data.frame(pbp)
    sel <- which(df$home_team_id == team.id | df$away_team_id == team.id) # data of interest
    df <- df[sel, ]
    df <- all_cleaning(df, team.id)
  }
  return(df)
}
