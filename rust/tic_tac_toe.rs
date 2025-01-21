
const DIM: usize = 3;

struct TicTacToe {
    board: Vec<Vec<char>>
}

impl TicTacToe {
    fn new() -> TicTacToe {
        TicTacToe{board: vec![vec!['E'; DIM]; DIM]} // New empty board

    }

    fn is_valid(x: usize, y:usize) -> bool {    // Check if the cell is valid
        x < DIM && y < DIM
    }

    fn player_x_move(&mut self, x: usize, y:usize) -> bool {    
        if self.board[x][y] == 'E' && TicTacToe::is_valid(x,y) {
            self.board[x][y] = 'X';
            return true
        } else {
            return false
        }
    }

    fn player_o_move(&mut self, x: usize, y:usize) -> bool {
        if self.board[x][y] == 'E' && TicTacToe::is_valid(x,y) {
            self.board[x][y] = 'O';
            return true
        } else {
            return false
        }
    }

    fn print_board(&self) {
        println!(" ");
        for row in &self.board {
            println!("{}", 
                row.iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<String>>()
                    .join(" ")
            );
        }
    }

    fn check_line(line: &[char]) -> Option<char> {
        if line.iter().all(|&c| c == 'X') { // All elements are X
            Some('X')
        } else if line.iter().all(|&c| c == 'O') {  // All elements are O
            Some('O')
        } else {
            None
        }
    }

    fn check_winner(&self) -> Option<char> {
        // Check rows
        for row in &self.board {
            if let Some(winner) = TicTacToe::check_line(row) {
                return Some(winner)
            }
        }

        // Check columns
        for col in 0..DIM {
            let column: Vec<char> = (0..DIM).map(|row| self.board[row][col]).collect(); // Get the column
            if let Some(winner) = TicTacToe::check_line(&column) {
                return Some(winner)
            }
        }

        // Check main diagonal
        let main_diag: Vec<char> = (0..DIM).map(|i| self.board[i][i]).collect();
        if let Some(winner) = TicTacToe::check_line(&main_diag) {
            return Some(winner)
        }

        // Check anti diagonal
        let anti_diag: Vec<char> = (0..DIM).map(|i| self.board[i][DIM-i-1]).collect();
        if let Some(winner) = TicTacToe::check_line(&anti_diag) {
            return Some(winner)
        }

        None
    }

    fn is_full(&self) -> bool { // Check if there's no winner
        self.board.iter().all(|row| row.iter().all(|&cell| cell != 'E'))
    }
}

fn main() {
    let mut game = TicTacToe::new();
    let mut game_over = false;
    let mut current_player = 'X';

    while !game_over {
        game.print_board();
        println!("Player {}, enter your move (x y):", current_player);

        let mut input = String::new();
        std::io::stdin().read_line(&mut input).expect("Failed to read input");  // Get input string
        let mut parts = input.trim().split_whitespace();    // Removes withespaces and splits the input

        if let (Some(row_str), Some(col_str)) = (parts.next(),parts.next()) {   // Extracts first two elements
            if let (Ok(row), Ok(col)) = (row_str.parse::<usize>(), col_str.parse::<usize>()) {   // Parse them as usize
                let valid_move = if current_player == 'X'{  // Plays the correct move
                    game.player_x_move(row, col)
                } else {
                    game.player_o_move(row, col)
                };

                if valid_move {
                    if let Some(winner) = game.check_winner() { // Check for winner
                        game.print_board();
                        println!("Game over! Player {} won", winner);
                        game_over = true;
                    } else if game.is_full() {  // Check for draw
                        game.print_board();
                        println!("Game over! It's a draw");
                        game_over = true;
                    } else {
                        current_player = if current_player == 'X' {'O'} else {'X'}; // Switch the player
                    }
                } else {
                    println!("Invalid move, try again");
                }
            } else {
                println!("Invalid input, try x y");
            }
        }
    }
}