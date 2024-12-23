use std::collections::HashSet;
use std::fs;

const BOARD_H: usize = 8;
const BOARD_W: usize = 8;

type Board = Vec<Vec<i32>>;

// Read the board from .txt file
fn read_board(filename: &str) -> Board {
    let content = fs::read_to_string(filename).expect("Error reading the file");
    content
        .lines()
        .map(|line| {
            line.split_whitespace()
                .map(|x| x.parse::<i32>().expect("Error parsing cell content"))
                .collect()
        })
        .collect()
}

// Print the board
fn print_board(board: &Board) {
    for row in board {
        println!("{}",
            row.iter()
                .map(|x| x.to_string())
                .collect::<Vec<String>>()
                .join(" ")
        );
    }
}

// 1 Rule: no adjacent shadowed cells 
fn check_no_adj_shadowed(board: &Board) -> bool {
    for i in 0..BOARD_H {
        for j in 0..BOARD_W {
            if board[i][j] == 0 {
                // Check adj cells
                if (i > 0 && board[i-1][j] == 0)    // Up
                    || (i < BOARD_H-1 && board[i+1][j] == 0)    // Down
                    || (j > 0 && board[i][j-1] == 0)    // Left
                    || (j < BOARD_W-1 && board[i][j+1] == 0)    // Right
                {
                    return false;
                }
            }
        }
    }
    true
}

// 2 Rule: no duplicated elements in each row / cell
fn check_no_duplicates_in_vector(vec: &[i32]) -> bool {
    let mut seen = HashSet::new();
    for &val in vec {
        if val != 0 && !seen.insert(val) {  // insert(x) returns true if x is inserted, false if it's already present in HashSet
            return false;   // Duplicate value found
        }
    }
    true
}

fn check_no_duplicates(board: &Board) -> bool {
    // Check rows
    for r in board {
        if !check_no_duplicates_in_vector(r) {
            return false;
        }
    }

    // Check columns
    for c in 0..BOARD_W {
        let mut col = vec![];
        for r in 0..BOARD_H {
            col.push(board[r][c]);
        }
        if !check_no_duplicates_in_vector(&col) {
            return false;
        }
    }
    true
}

// 3 Rule: non-shadowed cells must be connected
fn check_connected(board: &Board) -> bool {

    // Find first un-shadowed cell
    let mut start = None;
    for (i, row) in board.iter().enumerate() {
        for (j, &val) in row.iter().enumerate() {
            if val != 0 {
                start = Some((i,j));  // Value found
                break;  // Stop inner cycle
            }
        }
        if start.is_some() {    // If a value has been found
            break;  // Stop outer cycle
        }
    }

    // If a valid value has been found (Some(x)) apply floodfill from it
    if let Some(start) = start {
        let mut visited = vec![vec![false; BOARD_W]; BOARD_H];  // Initialize to false an ancilla matrix
        flood_fill(board, start.0, start.1, &mut visited);

        // Check all un-shadowed cells have been visited
        for i in 0..BOARD_H {
            for j in 0..BOARD_W {
                if board[i][j] != 0 && ! visited[i][j] {
                    return false;
                }
            }
        }
        true
    } else {
        false   // There's no un-shadowed cells
    }
}

fn flood_fill(board: &Board, x: usize, y: usize, visited: &mut Vec<Vec<bool>>) {
    if x >= BOARD_H || y >= BOARD_W || visited[x][y] || board[x][y] == 0 {
        return;
    }
    visited[x][y] = true;

    // Check adj cells
    if x > 0 {
        flood_fill(board, x-1, y, visited);
    }
    if x < BOARD_H-1 {
        flood_fill(board, x+1, y, visited);
    }
    if y > 0 {
        flood_fill(board, x, y-1, visited);
    }
    if y < BOARD_W-1 {
        flood_fill(board, x, y+1, visited);
    }
}

// Check all rules
fn check_rules(board: &Board) -> bool {
    check_no_adj_shadowed(board) && check_no_duplicates(board) && check_connected(board)
}

// Main function
fn main() {
    let board = read_board("hitori.txt");
    println!("Board: ");
    print_board(&board);

    if check_rules(&board) {
        println!("Well done!");
    } else {
        println!("The board is not valid");
    }
}