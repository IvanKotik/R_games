
memory <- function(n_col, n_row, n_players){  # input of parameters
  if ((n_col * n_row) %% 2 != 0) {  # stop if a field of pair cannot be created
    stop(" n_row * n_col must be an even number.")
  }
  if (n_col * n_row > 208) {  # stop if the dimentions are too big for the amount of icons/colors combinations
    stop("n_row * n_col must not exceed 208.")
  }
  if ((n_col <= 0) | (n_row <= 0) | (n_players <= 0) | (n_col %% 1 != 0) | (n_row %% 1 != 0) | (n_players %% 1 != 0)) {  # stop if the input is not a natural number
    stop("values must be natural numbers.")
  }
  n_col <<- n_col  # set the parameters globally to be used further
  n_row <<- n_row
  n_players <<- n_players

set_field <- function() {
  ### the function to distribute the cards and shuffle them
  playing_field <<- data.frame(id = 1:(n_row * n_col),  # set a column of ids for easy access
                               x_axis = rep(1:n_col, each = n_row),  # set the x-axis values
                               y_axis = rep(1:n_row, times = n_col))  # set the y-axis values

  # create a table of all combinations to be picked from
  all_combinations <<- data.frame(index = 1:(13*8),  # set a column of ids for easy access
                                  shape = rep(rep(1:13), times = 8),  # set the possible shapes
                                  color = rep((1:8), each = 13))  # set the possible colours

  # picking random variants of pairs from the combinations set according to the size of the playing field
  playing_field$index <<- sample(rep(sample(all_combinations$index, nrow(playing_field)/2), times = 2))  # get random id's
  playing_field <<- merge(playing_field, all_combinations, by = "index")  # merge by id's
  playing_field$win <<- ""  # create the column for the won cells and the win condition
  playing_field[, 7] <<- as.integer(playing_field[, 7])

  # set an empty dataframe for further use (see first_choice + second_choice)
  choises <<- data.frame(index = 0, id = 0, x_axis = 0, y_axis = 0,
                         shape = 0, color = 0, win = 0)

  # creating the live table that will be used for ploting and in-cycle use
  playing_field_live <<- playing_field
  playing_field_live[, 5] <<- 0  # setting shapes and colours to 0 so that they are not plotted
  playing_field_live[, 6] <<- 0

}

create_ladderboard <- function () {
  ### creating the ladderboard
  player_count <- 1:n_players  # create the vector of players
  first_player <- sample(player_count, 1)  # pick the randon first player
  player_order <<- data.frame(name = sub("^", "Player ", c(first_player:n_players, 1:(first_player-1))),  # reodering the playing order for the game
                              numb = c(first_player:n_players, 1:(first_player-1)))
  player_count <- sub("^", "Player ",  as.character(player_count))  # start creating the ladderboard
  player_count <- as.data.frame(player_count)  # this will be the playername column
  score <- as.data.frame(rep(0, times = n_players))  # this will be the score column
  ladderboard <<- cbind(player_count, score)  # create the ladderboard
  colnames(ladderboard) <<- c("Player", "Score")  # set new names for the columns
  print(paste(player_order[1,1], "goes first!"))  # tell which player goes first
}

plot_grid <- function(choise_vector){
  ### plot the gameboard
  par(xaxs = "i", yaxs = "i", new = TRUE)
  plot.new()
  plot.window(xlim = c(0.5, n_col+0.5), ylim = c(0.5, n_row+0.5))
  grid(nx = n_col, ny = n_row, col = "gray")
  box(lwd = 1)
  axis(2, at = 1:n_row, las = 1)
  axis(1, at = 1:n_col)
  title(main = "Memory")
  points(x = choises$x_axis,
         y = choises$y_axis,
         pch = 15,
         col = 0,
         cex = 7)
  text(y_axis~x_axis, data = playing_field_live, labels = win, cex = 2)
}

first_choice <- function() {
  ### taking in the first choice and checking for violations
  i <- 1  # for the first cycle
  j <- 1  # for the second cycle
  while (i == 1) {
    x1 <<- as.numeric(readline("First choice: Enter the X coordinate."))  # read the first x-axis input
      if (is.na(x1) == FALSE) {  # checking that the input is a number, not a letter or symbol
        if ((1 <= x1) & (x1 <= n_col)) {  # check that x1 fits the column number
          if (x1 %% 1 == 0) {  #  check that x1 is a natural number
            if ((any(is.na(playing_field_live[playing_field_live[, 3] == x1, 7])))) {  # check if there is enough free space in that column
              while (j == 1) {
                y1 <<- as.numeric(readline("First choice: Enter the Y coordinate."))  # read the first y-axis input
                  if (is.na(y1) == FALSE) {  # checking that input is a number, not a letter or symbol
                    if ((1 <= y1) & (y1 <= n_row)) {  # check that y1 fits the span
                      if (y1 %% 1 == 0) {  #  check that y1 is a natural number
                        if (is.na(playing_field_live[playing_field_live[, 4] == y1 & playing_field_live[, 3] == x1, 7])) {  # check if the space is empty
                          choice1 <<- playing_field[playing_field[, 3] == x1 & playing_field[, 4] == y1, ]  # save the choice
                          j <- j+1  # exit the cycle
                          i <- i+1  # exit the cycle
                        } else {print("Sorry, but this cell is already won, please choose another one.")}
                      } else {print("Sorry, but this is not a natural number, please choose another number.")}
                    } else {print("Sorry, but this position is outside the playing field, try again.")}
                  } else {print("Sorry but the entry is not a number, please try again.")}
              }
            } else {print("Sorry, but there is no free spaces left in this column, choose another one.")}
          } else {print("Sorry, but this is not a natural number, please choose another number.")}
        } else {print("Sorry, but this position is outside the playing field, try again.")}
      } else {print("Sorry but the entry is not a number, please try again.")}
  }
}

second_choise <- function() {
  ### taking in the first choice and checking for violations
  i <- 1  # for the first cycle
  j <- 1  # for the second cycle
  while (i == 1) {
    x2 <<- as.numeric(readline("Second choice: Enter the X coordinate."))  # read the second x-axis input
      if (is.na(x2) == FALSE) {  # check that the input is a number and not something else
        if ((1 <= x2) & (x2 <= n_col)) {  # check that x2 fits the column numbers
          if (x2 %% 1 == 0) {  #  check that x2 is a natural number
            if ((any(is.na(playing_field_live[playing_field_live[, 3] == x2, 7])))) {  # check if there is enough free space in that column
              if (!(((sum(is.na(playing_field_live[playing_field_live[, 3] == x2, 7])) == 1) & (x1 == x2)))) {  # fail if there is only one space left in the column and it was taken in the first pick
                while (j == 1) {  # read y input
                  y2 <<- as.numeric(readline("Second choice: Enter the Y coordinate."))  # read the second y-axis input
                    if (is.na(y2) == FALSE) {  # check that the input is a number and not something else
                      if ((1 <= y2) & (y2 <= n_row)) {  # check that y2 fits the row numbers
                        if (y2 %% 1 == 0) {  #  check that y2 is a natural number
                          if (is.na(playing_field_live[playing_field_live[, 4] == y2 & playing_field_live[, 3] == x2, 7])) {  # check if the chosen space is empty
                            if (!(x1 == x2 & y1 == y2)) {  # fail if there is an overlap and the choices are the same
                              choice2 <<- playing_field[playing_field[, 3] == x2 & playing_field[, 4] == y2, ]  # save the choice
                              j <- j+1  # exit the cycle
                              i <- i+1  # exit the cycle
                            } else {print("Sorry but you have chosen the same cell again, please alternate your choise.")}
                          } else {print("Sorry, but this cell is already won, please choose another one.")}
                        } else {print("Sorry, but this is not a natural number, please choose another number.")}
                      } else {print("Sorry, but this position is outside the playing field, try again.")}
                    } else {"Sorry but the entry is not a number, please try again."}
                }
              } else {print("Sorry, but you do not have a move left for this column, please choose another column.")}
            } else {print("Sorry, but there is no free spaces left in this column, choose another one.")}
          } else {print("Sorry, but this is not a natural number, please choose another number.")}
        } else {print("Sorry, but this position is outside the playing field, try again.")}
      } else {print("Sorry but the entry is not a number, please try again.")}
  }
}

plot_choise <- function(choise){
  ### plot the choises (two cards)
  points(x = choise$x_axis,
         y = choise$y_axis,
         pch = choise$shape,
         col = choise$color,
         cex = 4)
}

gameplay <- function() {
  ### main game cycle
  game_status <- 1
  while (game_status == 1){
    for (player in 1:n_players){  # start with first player and continue
      current_player <<- player  # save the player number globally
      k <- 1
      while (k == 1){
        print(paste(player_order[current_player, 1], ", choose your first card (1:column, 2:row). Press [y] when you are ready or [q] if you want to quit the game."))
        initiate_round <- readline()  # take an input to check if the player is ready to start
        if (initiate_round == "y") {  # check if the player is ready to start
          plot_grid()  # plot the "fresh" field
          first_choice()  # take the first choice
          plot_choise(choice1)  # plot the first choice
          second_choise()  # take the second choice
          plot_choise(choice2)  # plot the second choice
          choises <<- rbind(choice1, choice2)  # create a data.frame from the two choices

          if (choice1[1] == choice2[1]) {  # if the index of the choice is the same that means that this is a pair and the player has won this round
            playing_field_live[which(playing_field_live[, 2] == as.integer(choice1[2])), 7] <<- player_order[current_player, 2]  # fixate that the player won that cell
            playing_field_live[which(playing_field_live[, 2] == as.integer(choice2[2])), 7] <<- player_order[current_player, 2]  # fixate that the player won that cell
            ladderboard[player_order[current_player, 2], 2] <<-  sum(playing_field_live[, 7] == player_order[current_player, 2], na.rm = TRUE)/2  # recalculate and update the ladderboard
            playing_field_live <<- playing_field_live  # save the changes to the live playing field globally for further use
            print(paste("Correct,", player_order[current_player, 1], "plays again!"))
            print("Current ladderboard:")
            print(ladderboard)
          } else {print("Wrong, next player.")  # if the player did not win
            playing_field_live <<- playing_field_live  # revert back to previous board iteration
            break}  # exit and go to the next player
          if (sum(is.na(playing_field_live[, 7])) == 0) {break}  # check if the game is won, if so -- exit
        }
        if (initiate_round == "q") {print("executing exit...")  # if the player wants to quit by inputting q -- quit
          break}
      }
      if (initiate_round == "q") {break}  # if the player wants to quit by inputting q -- quit
      if ((sum(is.na(playing_field_live[, 7])) == 0) == TRUE) {  # check if the game is won, if so -- exit and show who won
        game_status <- game_status + 1
        print(paste("Game won by", ladderboard[ladderboard[, 2] == max(ladderboard[, 2]), 1]))
        break
      }
    if (initiate_round == "q") {break}  # exit if the player wants to quit
    if ((sum(is.na(playing_field_live[, 7])) == 0) == TRUE) {break}
    }
  if (initiate_round == "q") {break}  # exit if the game is won
  if ((sum(is.na(playing_field_live[, 7])) == 0) == TRUE) {break}
  }
}

### start the game:
  set_field()  # set an initial field
  create_ladderboard()  # create the ladderboard
  gameplay()  # game sequence
}

memory(3, 4, 3)
