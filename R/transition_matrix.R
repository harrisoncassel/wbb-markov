# Try to load other source file; give warning if unable
load.result <- tryCatch({
    source('scripts/data_management.R')
  }, warning = function(w) {
    print(w)
  }, error = function(e) {
    load2.result <- tryCatch({
      source('https://raw.githubusercontent.com/harrisoncassel/wbb-markov/main/R/data_management.R')
    }, warning = function(w) {
      print(w)
    }, error = function(e) {
      print('WARNING: data_management.R NOT LOADED! MOST FUNCTIONS WILL NOT OPERATE PROPERLY!')
      print('SPECIFIC ERROR:')
      print(e)
    })
  })

states_dataframe <- function() {
  ### Returns a data.frame with the states defined for this research
  a.states <- seq(1,10)
  b.states <- seq(11,20)
  events <- c('MadeFreeThrow','TwoPoint','ThreePoint','Missed Shot','Offensive Rebound','Lost Ball Turnover','Inbound','Foul','Defensive Rebound','Block Shot')
  states <- data.frame(A=a.states,B=b.states, row.names=events)
  return(states)
}

make_transition_probs <- function(trans.mat) {
  ### Returns matrix with frequency counts converted to Markov Train prob. matrix
  ### transition probabilities.
  # trans.mat := a transition matrix of frequency counts
  for (i in 1:nrow(trans.mat)) {
    current <- trans.mat[i,]
    current <- current / sum(current)
    trans.mat[i,] <- current
  }
  return(trans.mat)
}

update_transition_matrix <- function(trans.mat, formatted.pbp) {
  ### Returns a (frequency/count) transition matrix with updated transition counts
  ### from a formatted play-by-play data set.
  # trans.mat := *Frequency (count)* transition matrix to be updated
  # formatted.pbp := data.frame cleaned/processed by data_management.R to be
  #                   ready (team.ids 'A' and 'B', cleaned/renamed event types, etc.)
  pbp <- formatted.pbp
  states <- states_dataframe()
  games <- unique(pbp$game_id)
  
  for (game in games) {
    # Get game-specific pbp data
    game.pbp <- pbp[pbp$game_id == game, ]
    
    # Cycle through the plays in the game, counting the transitions & updating the transition matrix
    initial.state <- pbp[1,] # initial state -- used to setup the previous.state variable below
    previous.state <- states[initial.state$type_text, initial.state$team_id] # beginning previous state = initial state
    for (i in 2:length(formatted.pbp$type_text)) { # Cycle through each play in order
      current <- pbp[i,]
      current.state <- states[current$type_text, current$team_id]
      trans.mat[previous.state, current.state] <- 1 + trans.mat[previous.state, current.state]
      previous.state <- current.state
    }
  }
  return(trans.mat)
}

create_comparison_matrix <- function(team.a.trans.mat, team.b.trans.mat) {
  ### Creates a head-to-head comparison transition matrix by replacing the
  ### "Team A" rows with the first half of the rows from the team.a argument,
  ### and the second half with B.
  # team.a.trans.mat := transition matrix (PROBABILITIES) of team of interest
  # team.b.trans.mat := transition matrix (PROBABILITIES) of other team
  # !!!!!: Both transition matrices should be square, even-dimensioned, and
  #         of the same dimension.
  if (nrow(team.a.trans.mat) != nrow(team.b.trans.mat)) {
    stop('TEAMS A AND B HAVE DIFFERENT NUMBERS OF ROWS IN THEIR TRANSITION MATRICES')
  }
  
  rows <- nrow(team.a.trans.mat) / 2
  top.half <- team.a.trans.mat[1:rows, ]
  bottom.half <- team.b.trans.mat[(rows+1):(rows*2), ]
  
  result <- rbind(top.half, bottom.half)
  return(result)
}
