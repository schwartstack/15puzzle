#' Play the 15 Puzzle Game
#'
#' Starts an interactive console version of the classic 15 puzzle.
#' The puzzle consists of fifteen numbered tiles arranged in a 4 × 4
#' grid with one empty space. The puzzle is scrambled before play begins,
#' and the objective is to return the tiles to their original order.
#'
#' During play, enter:
#' \itemize{
#'   \item \code{w} to move the empty space up
#'   \item \code{a} to move the empty space left
#'   \item \code{s} to move the empty space down
#'   \item \code{d} to move the empty space right
#'   \item \code{q} to quit the game
#' }
#'
#' The game ends when the puzzle is solved or the player quits.
#'
#' @param difficulty Integer giving the number of random moves used to
#'   scramble the puzzle before play begins. Larger values generally
#'   produce more difficult starting positions. Defaults to `50`.
#'
#' @return No return value. This function is called for its side effects,
#'   including printing the puzzle state, accepting user input, and
#'   displaying win messages.
#'
#' @examples
#' \dontrun{
#' play_15_puzzle()
#' play_15_puzzle(difficulty = 100)
#' }
#'
#' @export
play_15_puzzle <- function(difficulty = 50) {
    puzzle_matrix = matrix(c(1:15, 0), byrow = T, ncol = 4)
    scramble = sample(c("w", "a", "s", "d"), difficulty, replace = T)
    for (i in 1:difficulty) {
        puzzle_matrix = .move(puzzle_matrix, scramble[i])
    }
    win = FALSE
    turn = 0
    while (!win) {
        .print_puzzle(puzzle_matrix)
        newmove = .get_move()
        if (newmove == "q") {
            break
        } else if (
            newmove == "w" |
                newmove == "a" |
                newmove == "s" |
                newmove == "d"
        ) {
            puzzle_matrix = .move(puzzle_matrix, newmove)
            turn = turn + 1
        } else {
            cat("Input not recognized", "\n")
        }
        if (
            !(FALSE %in%
                as.vector(
                    puzzle_matrix == matrix(c(1:15, 0), byrow = T, ncol = 4)
                ))
        ) {
            win = TRUE
        }
    }
    if (win) {
        .print_puzzle(puzzle_matrix)
        cat("\n")
        print("You win!")
        cat("\n", "It took you", turn, "moves.", "\n")
    }
}

.get_move <- function() {
    direction <- readline(prompt = "Move:")
    return(direction)
}

.move <- function(puzzle_matrix, direction) {
    if (direction == "w") {
        puzzle_matrix = .move_up(puzzle_matrix)
    } else if (direction == "s") {
        puzzle_matrix = .move_down(puzzle_matrix)
    } else if (direction == "a") {
        puzzle_matrix = .move_left(puzzle_matrix)
    } else if (direction == "d") {
        puzzle_matrix = .move_right(puzzle_matrix)
    }
    return(puzzle_matrix)
}

.move_up <- function(puzzle_matrix) {
    if (0 %in% puzzle_matrix[4, ]) {} else {
        pos = which(puzzle_matrix == 0)
        puzzle_matrix[pos] = puzzle_matrix[pos + 1]
        puzzle_matrix[pos + 1] = 0
    }
    return(puzzle_matrix)
}

.move_down <- function(puzzle_matrix) {
    if (0 %in% puzzle_matrix[1, ]) {} else {
        pos = which(puzzle_matrix == 0)
        puzzle_matrix[pos] = puzzle_matrix[pos - 1]
        puzzle_matrix[pos - 1] = 0
    }
    return(puzzle_matrix)
}

.move_left <- function(puzzle_matrix) {
    if (0 %in% puzzle_matrix[, 4]) {
        return(puzzle_matrix)
    } else {
        return(t(.move_up(t(puzzle_matrix))))
    }
}

.move_right <- function(puzzle_matrix) {
    if (0 %in% puzzle_matrix[, 1]) {
        return(puzzle_matrix)
    } else {
        return(t(.move_down(t(puzzle_matrix))))
    }
}

.print_puzzle <- function(puzzle_matrix) {
    cat("+----+----+----+----+", "\n")
    for (r in 1:4) {
        string = "|"
        for (c in 1:4) {
            if (puzzle_matrix[r, c] == 0) {
                string = paste(string, "    |", sep = "")
            } else if (puzzle_matrix[r, c] < 10) {
                string = paste(
                    string,
                    "  ",
                    puzzle_matrix[r, c],
                    " |",
                    sep = ""
                )
            } else {
                string = paste(string, " ", puzzle_matrix[r, c], " |", sep = "")
            }
        }
        cat(string, "\n", "+----+----+----+----+", "\n", sep = "")
    }
}

play_15_puzzle()
