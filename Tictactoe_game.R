library(tidyverse)
library(data.table)

tictactoe <- function (){
  terminate <- 0
  startgame <- readline(prompt="Start game, player X? y/n")
  if (startgame == "y"){

    # Setting up the board
    colnum <- readline(prompt="Enter number of columns: ")
    colnum <- as.integer(colnum)
    colseq <- c(1:colnum)

    rownum <- readline(prompt="Enter number of rows: ")
    rownum <- as.integer(rownum)
    rowseq <- c(1:rownum)

    wincond <- readline(prompt="Enter win condition: ")
    wincond <- as.integer(wincond)

    board <- tidyr::crossing(colseq, rowseq)
    board$value <- rep("  ", colnum*rownum)

    # Starting the game
    ready <- readline(prompt="Ready? y/n")
    if (ready == "y") {

      while (terminate == 0){

        # Receive input from X and register it
        occupied <- 0
        while (occupied == 0) {
          x <- as.integer(readline(prompt="Enter column, player X: "))
          y <- as.integer(readline(prompt="Enter row, player X: "))
          block_a <- 0
          block_b <- 0
          block_c <- 0

          if ((0 < x & x <= colnum)&(0 < y & y <= rownum)) {block_a <- 1

          if ((data.table(board))[colseq == x, ][rowseq == y, ][, value == "X"] == TRUE) {print("This cell is already claimed by player X")}
          else {block_b <- 1}

          if ((data.table(board))[colseq == x, ][rowseq == y, ][, value == "Q"] == TRUE) {print("This cell is already claimed by player Q")}
          else {block_c <- 1}

          }
          else {print("You cannot play outside the field")}


          if (block_a == 1 & block_b == 1 & block_c == 1) {break}
        }

        board$value[which(board$colseq == x & board$rowseq == y)] <- "X"

        # Prepare data.table for test
        test <- as.data.table(board)
        test <- dcast(test, rowseq~colseq)
        test[, 1:=NULL]

        #Column win checker
        answer_c <- "No column win condition met"
        for (c in 1:colnum) {                 #fix column
          for (r in 1:(rownum-wincond+1)) { #give a vertical interval as given by win condition
            if (all(test[(r:(r+wincond-1)),][,..c] == rep("X", wincond)) == TRUE) {
                answer_c <- "Player X wins by column"
                terminate <- 1
            }
            if (all(test[(r:(r+wincond-1)),][,..c] == rep("Q", wincond)) == TRUE) {
                answer_c <- "Player Q wins by column"
                terminate <- 1
            }
          }
        }
        print(answer_c)

        #Row win checker
        answer_r <- "No row win condition met"
        for (r in 1:rownum) {                   #fix row
          for (c in 1:(colnum-wincond+1)) {   #give a horizontal interval as given by win condition
            if (all(test[r,][,c:(c+wincond-1)] == rep("X", wincond)) == TRUE) {
                answer_r <- "Player X wins by rows"
                terminate <- 1
            }
            if (all(test[r,][,c:(c+wincond-1)] == rep("Q", wincond)) == TRUE) {
                answer_r <- "Player Q wins by rows"
                terminate <- 1
            }
          }
        }
        print(answer_r)

        # Update the board
        board <- data.table(colseq = rep(seq_len(colnum), each = rownum),
                            rowseq = rep(seq_len(rownum), colnum),
                            value = c(as.matrix(test)))

        # Plotting the board
        for (u in 1:1){
          print(ggplot(board, aes(x = colseq, y = rowseq))+
                  geom_label(aes(label = value), size = 15)+
                  scale_x_continuous(limits = c(0.5, colnum+0.5), breaks = seq(0.5, colnum+0.5, 1), minor_breaks = NULL)+
                  scale_y_continuous(limits = c(0.5, rownum+0.5), breaks = seq(0.5, rownum+0.5, 1), minor_breaks = NULL)
          )
          break}

        if (terminate == 1) {break}

        # Receive input from O and register it
        occupied <- 0
        while (occupied == 0) {
          x <- as.integer(readline(prompt="Enter column, player Q: "))
          y <- as.integer(readline(prompt="Enter row, player Q: "))
          block_a <- 0
          block_b <- 0
          block_c <- 0

          if ((0 < x & x <= colnum)&(0 < y & y <= rownum)) {block_a <- 1

          if ((data.table(board))[colseq == x, ][rowseq == y, ][, value == "X"] == TRUE) {print("This cell is already claimed by player X")}
          else {block_b <- 1}

          if ((data.table(board))[colseq == x, ][rowseq == y, ][, value == "Q"] == TRUE) {print("This cell is already claimed by player Q")}
          else {block_c <- 1}

          }
          else {print("You cannot play outside the field")}

          if (block_a == 1 & block_b == 1 & block_c == 1) {break}
        }

        board$value[which(board$colseq == x & board$rowseq == y)] <- "Q"

        # Prepare data.table for test
        test <- as.data.table(board)
        test <- dcast(test, rowseq~colseq)
        test[, 1:=NULL]

        #Column win checker
        answer_c <- "No column win condition met"
        for (c in 1:colnum) {                 #fix column
          for (r_a in 1:(rownum-wincond+1)) { #give a vertical interval as given by win condition
            r_b <- r_a + 1
            r_c <- r_a + 2
            if (test[r_a, ..c] == "Q" & test[r_b, ..c] == "Q" & test[r_c, ..c] == "Q") { #test win (vertical)
              answer_c <- "Player Q wins by column"
              terminate <- 1
            }
            if (test[r_a, ..c] == "X" & test[r_b, ..c] == "X" & test[r_c, ..c] == "X") { #test win (vertical)
              answer_c <- "Player X wins by column"
              terminate <- 1
            }
          }
        }
        print(answer_c)

        answer_r <- "No row win condition met"
        for (r in 1:rownum) {                   #fix row
          for (c_a in 1:(colnum-wincond+1)) {   #give a horizontal interval as given by win condition
            c_b <- c_a + 1
            c_c <- c_a + 2
            if (test[r, ..c_a] == "Q" & test[r, ..c_b] == "Q" & test[r, ..c_c] == "Q") { #check vertical
              answer_r <- "Player Q wins by row"
              terminate <- 1
            }
            if ((test[r, ..c_a] == "X" & test[r, ..c_b] == "X" & test[r, ..c_c] == "X")) {
              answer_r <- "Player X wins by row"
              terminate <- 1
            }
          }
        }
        print(answer_r)

        # Update the board
        board <- data.table(colseq = rep(seq_len(colnum), each = rownum),
                            rowseq = rep(seq_len(rownum), colnum),
                            value = c(as.matrix(test)))

        # Plotting the board
        for (u in 1:1){
          print(ggplot(board, aes(x = colseq, y = rowseq))+
                  geom_label(aes(label = value), size = 15)+
                  scale_x_continuous(limits = c(0.5, colnum+0.5), breaks = seq(0.5, colnum+0.5, 1), minor_breaks = NULL)+
                  scale_y_continuous(limits = c(0.5, rownum+0.5), breaks = seq(0.5, rownum+0.5, 1), minor_breaks = NULL)
          )
          break}

        if (terminate == 1) {break}

      }
    } else {print("Sorry to hear that.")}
  } else {print("Sorry to hear that.")}
}

tictactoe()