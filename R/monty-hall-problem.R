#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies.
#'
#' @param ... no arguments are used by the function.
#'
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
}



#' @title
#'   Contestant selects a door.
#'
#' @description
#'   `select_door()` allows contestant to choose one  door out of the three available doors at random.
#'
#' @details
#'   Contestant is asked to pick a door out of the three available doors.
#'   Two of the doors are goat doors and only one of the doors is a car door.
#'
#' @param ... no arguments are used by the function.
#'
#' @return  The function returns a numeric vector of length one
#'   indicating the chosen door by contestant.
#'
#' @examples
#'  select_door()
#'
#' @export
select_door <- function( )
{
  doors <- c(1,2,3)
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#' Host opens goat door
#'
#' @description
#'    `open_goat_door()` This function will open one goat door only
#'and it should be the door that is goat door and the door that the contestant did not pick.
#'
#' @details
#' The host will always open a door with a goat behind it.
#' But it can't be a door the contestant has already selected.
#'  It must be a door that is not a car and not a current contestant selection.
#'  If the contestant selects the car on the first guess
#'  the host can open either door, but if the contestant
#'  selects a goat the host only has one option.
#'
#' @param ... arguments used are ( game, a.pick )
#'
#' @return The function returns a  numeric vector of length one
#'   indicating the opened door by host.
#'
#' @examples
#' open_goat_door(game, a.pick)
#'
#'
#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats
   if( game[ a.pick ] == "car" )
   {
     goat.doors <- doors[ game != "car" ]
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   {
     opened.door <- doors[ game != "car" & doors != a.pick ]
   }
   return( opened.door ) # number between 1 and 3
}



#' @title
#' Change doors
#'
#' @description
#'   Contestant has the choice to switch or stay with original choice
#'  of the door.
#'
#' @details
#'  The contestant is given the option to change from their initial selection
#'  to the other door that is still closed.
#'  The function will represent the game-playing strategy
#'  as the argument stay=TRUE or stay=FALSE.
#'  After host opens one of the doors that is not what the contestant picked nor the car door
#'  the contestant has the choice of switching to the other door.
#'
#' @param ... arguments are ( stay=T, opened.door, a.pick )
#'
#' @return The function returns a length 1 numeric vector.
#'
#' @examples
#' change_door( stay=T, opened.door, a.pick )
#'
#'
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3)

   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ]
   }

   return( final.pick )  # number between 1 and 3
}



#' @title
#' Determine Winner
#'
#' @description
#'
#'  The function determines if contestant has won or lost depending on whether the contestant
#'   changed his pick or stayed with original pick.
#'
#' @details
#' The if function is used to further demonstrate that if contestant's final pick
#' whether he changes his pick or not is car , he will win and
#' if contestant's final pick was 'goat' he will lose.
#'
#' @param ... arguments used are (final.pick,game)
#'
#' @return The function returns a WIN or LOSE
#'
#' @examples
#' determine_winner(final.pick, game )
#'
#'
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}





#' @title
#' Play game
#'
#' @description
#' The play_game() executes each step
#' of a single game in order and see which strategy wins/loses.
#'
#' @details
#'  The objects assigned to each of the
#' functions to  help determine the final game results if contestant switches his pick or
#' stays with his same pick.All the functions
#' are packaged into this single play_game () function which executes
#' each step of a single game in order.
#'
#' @param ... no arguments are used by the function.
#'
#' @return
#' The play game function returns game results in both scenarios (stay & switch)
#'
#' @examples
#' play_game()
#'
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )

  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title
#' Monty Hall simulation
#'
#' @description
#' Allows user to run the simulation for n times to know the WIN/LOSE probability.
#'
#' @details
#' The function helps us understand game outcomes if contestant stays or switches
#' and it takes into account the number of times the contestant plays.Results are stored in
#' a data frame and returns probabilities of the two game strategies that are stay and switch.
#'
#' @param  ... arguments used are number of simulations user specifies
#'
#' @return
#'  number of times where stay and switch strategies show WIN and LOSE outcomes.
#'
#' @examples
#' play_n_games()
#'
#'
#' @export
play_n_games <- function( n=100 )
{

  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome
    loop.count <- loop.count + 1
  }

  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>%
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>%
  print()

  return( results.df )

}
