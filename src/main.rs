mod ai;
mod board;
mod hash;
mod perft;
mod piece_values;

use std::{collections::HashSet, io::Stdin};

use ai::*;
use board::Color::*;
use board::*;
use hash::hash_board;
use hash::BoardHash;
use UCICommand::*;
use crate::piece_values::PIECE_VALUES;

pub const CONSIDERABLE_THRESHOLD: i16 = 100;

fn piece_values(file_path: String) {
    let mut rdr;
    let rdr_result = csv::ReaderBuilder::new()
        .has_headers(false)
        .from_path(file_path);
    match rdr_result {
        Ok(t) => rdr = t,
        Err(e) => {
            println!("ERROR: {}", e);
            return;
        }
    }
    for (piece, record_result) in rdr.records().enumerate() {
        let record;
        match record_result {
            Ok(t) => record = t,
            Err(e) => {
                println!("ERROR: {}", e);
                return;
            }
        }
        for (square, value) in record.into_iter().enumerate() {
            unsafe {
                //println!("piece: {}, square: {}, value: {}", piece, square, value);
                PIECE_VALUES[0][piece][square] = value.parse::<i16>().unwrap();
                PIECE_VALUES[1][piece][56 + 2 * (square % 8) - square] =
                    value.parse::<i16>().unwrap();
            }
        }
    }
}

enum UCICommand {
    UCI,
    IsReady,
    UCINewGame,
    Position {
        fen: String,
        move_list: Vec<String>,
    },
    Go {
        wtime: Option<i32>,
        btime: Option<i32>,
        winc: Option<i32>,
        binc: Option<i32>,
        depth: Option<i32>,
    },
    PieceEvals {
        filename: String,
    },
    Quit,
}

fn next_input(stdin: &Stdin) -> UCICommand {
    let mut input = String::new();
    let _ = stdin.read_line(&mut input);
    let vec: Vec<&str> = input.split_whitespace().collect();
    match vec.get(0) {
        Some(&"uci") => UCI,
        Some(&"isready") => IsReady,
        Some(&"ucinewgame") => UCINewGame,
        Some(&"quit") => Quit,
        Some(&"position") => parse_position(&vec),
        Some(&"go") => parse_go(&vec),
        Some(&"setoption") => parse_piece_values(&vec),
        _ => next_input(stdin),
    }
}

fn parse_position(vec: &Vec<&str>) -> UCICommand {
    let mut seen_moves = false;
    let mut fen = "".to_string();
    let mut move_list: Vec<String> = Vec::new();
    for s in vec {
        match s {
            &"position" | &"fen" => {}
            &"startpos" => fen = STARTPOS.to_string(),
            &"moves" => seen_moves = true,
            _ => {
                if seen_moves {
                    move_list.push(s.to_string())
                } else {
                    fen.push_str(" ");
                    fen.push_str(s)
                }
            }
        }
    }
    Position {
        fen: fen,
        move_list: move_list,
    }
}

fn parse_go(vec: &Vec<&str>) -> UCICommand {
    let mut prev = "";
    let mut wtime = None;
    let mut btime = None;
    let mut winc = None;
    let mut binc = None;
    let mut depth = None;

    for s in vec {
        match prev {
            "wtime" => wtime = Some(s.parse::<i32>().unwrap()),
            "btime" => btime = Some(s.parse::<i32>().unwrap()),
            "winc" => winc = Some(s.parse::<i32>().unwrap()),
            "binc" => binc = Some(s.parse::<i32>().unwrap()),
            "depth" => depth = Some(s.parse::<i32>().unwrap()),
            _ => {}
        }

        prev = s;
    }
    Go {
        wtime: wtime,
        btime: btime,
        winc: winc,
        binc: binc,
        depth: depth,
    }
}

fn parse_piece_values(vec: &Vec<&str>) -> UCICommand {
    let mut prev = "";
    let mut filename = "".to_string();
    let mut seen_evalfile = false;
    for s in vec {
        match prev {
            "EvalFile" => seen_evalfile = true,
            "value" => {
                if seen_evalfile {
                    filename = s.to_string();
                }
            }
            _ => {}
        }

        prev = s;
    }
    PieceEvals { filename: filename }
}

fn main() {
    hash::init_hashes();
    let stdin = std::io::stdin();
    let mut board = Board::from_fen(STARTPOS.to_string());
    let mut hash_set = HashSet::new();
    loop {
        match next_input(&stdin) {
            UCI => uci(),
            IsReady => isready(),
            UCINewGame => board = Board::from_fen(STARTPOS.to_string()),
            Position { fen, move_list } => (board, hash_set) = position(fen, move_list),
            Go {
                wtime,
                btime,
                winc: _,
                binc: _,
                depth,
            } => {
                let time_left = match board.turn {
                    WHITE => wtime,
                    BLACK => btime,
                };
                let result = bestmove(&mut board, &mut hash_set, time_left, depth);
                let sorted_moves = result.sorted_moves.unwrap();
                let best_move = sorted_moves.get(0).unwrap();
                println!("info score {}", result.eval);
                println!("bestmove {}", best_move);
            }
            PieceEvals { filename } => piece_values(filename),
            Quit => break,
        }
    }
}

fn uci() {
    println!("id name melsh_bot");
    println!("id author melsharawy");
    println!("option name EvalFile type string default <empty>");
    println!("uciok");
}

fn isready() {
    println!("readyok")
}

fn position(fenstring: String, movestring_list: Vec<String>) -> (Board, HashSet<BoardHash>) {
    let mut b = Board::from_fen(fenstring);
    let mut hash_set = HashSet::new();
    for s in movestring_list {
        hash_set.insert(hash_board(&b));
        take_move(s.to_string(), &mut b);
    }

    (b, hash_set)
}

fn take_move(s: String, b: &mut Board) {
    let move_option = b.get_moves().into_iter().find(|x| x.to_string() == s);
    match move_option {
        None => panic!("No such move: {s}"),
        Some(m) => b.take_move(&m),
    }
}
