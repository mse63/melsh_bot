mod ai;
mod board;
mod eval;
mod hash;
mod perft;
mod piece_values;
use crate::eval::Evaluator;
use ai::*;
use board::Color::*;
use board::*;
use hash::hash_board;
use hash::BoardHash;
use std::{collections::HashSet, io::Stdin};
use UCICommand::*;

fn piece_values(file_path: &String) -> Result<[[[i32; 64]; 7]; 2], csv::Error> {
    let mut piece_values: [[[i32; 64]; 7]; 2] = [[[0; 64]; 7]; 2];
    let mut rdr = csv::ReaderBuilder::new()
        .has_headers(false)
        .from_path(file_path)?;
    let mut num_rows = 0;
    for (piece, record_result) in rdr.records().enumerate() {
        num_rows += 1;
        let record = record_result?;
        for (square, value) in record.into_iter().enumerate() {
            //println!("piece: {}, square: {}, value: {}", piece, square, value);
            piece_values[0][piece][square] = value.parse::<i32>().unwrap();
            piece_values[1][piece][56 + 2 * (square % 8) - square] = piece_values[0][piece][square];
        }
    }
    assert!(num_rows == 7);
    Ok(piece_values)
}

enum UCIOption {
    EvalFile,
    ConsiderableThreshold,
    MaxDepth,
}

enum UCICommand {
    UciInit,
    IsReady,
    UciNewGame,
    Position {
        fen: String,
        move_list: Vec<String>,
    },
    Go {
        wtime: Option<i32>,
        btime: Option<i32>,
        _winc: Option<i32>,
        _binc: Option<i32>,
        depth: Option<i32>,
    },
    Perft {
        depth: u8,
    },
    SetPieceEvals {
        filename: String,
    },
    SetConsiderableThreshold {
        considerable_threshold: i32,
    },
    SetMaxDepth {
        max_depth: u8,
    },

    Debug,
    Quit,
}

fn next_input(stdin: &Stdin) -> UCICommand {
    let mut input = String::new();
    let _ = stdin.read_line(&mut input);
    let vec: Vec<&str> = input.split_whitespace().collect();
    match vec.first() {
        Some(&"uci") => UciInit,
        Some(&"isready") => IsReady,
        Some(&"ucinewgame") => UciNewGame,
        Some(&"quit") => Quit,
        Some(&"position") => parse_position(&vec),
        Some(&"go") => parse_go(&vec),
        Some(&"setoption") => parse_uci_option(&vec),
        Some(&"d") => Debug,
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
                    fen.push(' ');
                    fen.push_str(s)
                }
            }
        }
    }
    Position { fen, move_list }
}

fn parse_go(vec: &Vec<&str>) -> UCICommand {
    let mut prev = "";
    let mut wtime = None;
    let mut btime = None;
    let mut _winc = None;
    let mut _binc = None;
    let mut depth = None;

    for s in vec {
        match prev {
            "wtime" => wtime = s.parse::<i32>().ok(),
            "btime" => btime = s.parse::<i32>().ok(),
            "winc" => _winc = s.parse::<i32>().ok(),
            "binc" => _binc = s.parse::<i32>().ok(),
            "depth" => depth = s.parse::<i32>().ok(),
            "perft" => {
                return Perft {
                    depth: s.parse::<u8>().ok().unwrap_or(1),
                }
            }
            _ => {}
        }

        prev = s;
    }
    Go {
        wtime,
        btime,
        _winc,
        _binc,
        depth,
    }
}

fn parse_uci_option(vec: &Vec<&str>) -> UCICommand {
    let mut prev = "";
    let mut seen_option = None;
    for s in vec {
        match prev {
            "EvalFile" => seen_option = Some(UCIOption::EvalFile),
            "ConsiderableThreshold" => seen_option = Some(UCIOption::ConsiderableThreshold),
            "MaxDepth" => seen_option = Some(UCIOption::MaxDepth),
            "value" => match seen_option {
                Some(UCIOption::EvalFile) => {
                    return SetPieceEvals {
                        filename: s.to_string(),
                    }
                }
                Some(UCIOption::ConsiderableThreshold) => {
                    return SetConsiderableThreshold {
                        considerable_threshold: s.parse::<i32>().unwrap_or(1000),
                    }
                }
                Some(UCIOption::MaxDepth) => {
                    return SetMaxDepth {
                        max_depth: s.parse::<u8>().unwrap_or(255),
                    }
                }
                None => {}
            },
            _ => {}
        }

        prev = s;
    }
    UciInit
}

fn main() {
    hash::init_hashes();
    let stdin = std::io::stdin();
    let mut board = Board::from_fen(STARTPOS.to_string());
    let mut hash_set = HashSet::new();
    let mut evaluator = Evaluator::new();
    let mut ai = AIPlayer::new(evaluator);

    loop {
        match next_input(&stdin) {
            UciInit => uci(),
            IsReady => isready(),
            UciNewGame => {
                board = Board::from_fen(STARTPOS.to_string());
            }
            Position { fen, move_list } => (board, hash_set) = position(fen, move_list),
            Go {
                wtime,
                btime,
                _winc: _,
                _binc: _,
                depth,
            } => {
                let time_left = match board.turn {
                    White => wtime,
                    Black => btime,
                };
                let eval_result = ai.bestmove(&mut board, &mut hash_set, time_left, depth);
                println!("info score {}", eval_result.eval);
                println!(
                    "bestmove {}",
                    eval_result.sorted_moves.unwrap().first().unwrap()
                );
            }

            SetPieceEvals { filename } => match piece_values(&filename) {
                Err(e) => {
                    println! {"Error Parsing PieceEvals: {}", e}
                    break;
                }
                Ok(piece_values) => {
                    evaluator = Evaluator::from_piece_values(piece_values);
                    ai.evaluator = evaluator;
                }
            },
            SetConsiderableThreshold {
                considerable_threshold,
            } => {
                ai.considerable_threshold = considerable_threshold;
            }
            SetMaxDepth { max_depth } => {
                ai.max_depth = max_depth;
            }
            Perft { depth } => perft::perft(&mut board, depth),
            Debug => board.print_board(),
            Quit => {
                break;
            }
        }
    }
}

fn uci() {
    println!("id name melsh_bot");
    println!("id author melsharawy");
    println!("option name EvalFile type string default <empty>");
    println!("option name ConsiderableThreshold type spin default 1000 min 0 max 2147483647");
    println!("option name MaxDepth type spin default 255 min 1 max 255");
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
