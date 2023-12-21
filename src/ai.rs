use crate::board::*;
use crate::hash::*;
use crate::Color::*;
use crate::Move::*;
use crate::PieceType::*;
use crate::CONSIDERABLE_THRESHOLD;
use crate::piece_values::PIECE_VALUES;
use std::collections::HashMap;
use std::collections::HashSet;
use std::time::*;
use Depth::*;
use Eval::*;

#[derive(PartialEq, Eq, Clone, Copy)]
pub enum Eval {
    MATE(i8),
    CP(i16),
}

impl Eval {
    pub fn one_higher(&self) -> Eval {
        match self {
            MATE(x) => {
                if *x <= 0 {
                    MATE(-x + 1)
                } else {
                    MATE(-x)
                }
            }
            CP(x) => CP(-x),
        }
    }
}

impl std::fmt::Display for Eval {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MATE(i) => {
                write!(f, "mate {}", i)
            }
            CP(i) => {
                write!(f, "cp {}", i)
            }
        }
    }
}

impl std::cmp::PartialOrd for Eval {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl std::cmp::Ord for Eval {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match (*self, *other) {
            (CP(s), CP(o)) => s.cmp(&o),
            (MATE(s), CP(_)) => {
                if s == 0 {
                    std::cmp::Ordering::Less
                } else {
                    s.cmp(&0)
                }
            }
            (CP(_), MATE(o)) => {
                if o == 0 {
                    std::cmp::Ordering::Greater
                } else {
                    0.cmp(&o)
                }
            }
            (MATE(s), MATE(o)) => {
                if s.signum() == o.signum() {
                    o.cmp(&s)
                } else if s == 0 {
                    std::cmp::Ordering::Less
                } else if o == 0 {
                    std::cmp::Ordering::Greater
                } else {
                    s.cmp(&o)
                }
            }
        }
    }
}
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Depth {
    RAW,
    CONSIDERABLES,
    POSDEPTH(u8),
}

impl Depth {
    pub fn one_lower(&self) -> Depth {
        match self {
            RAW => RAW,
            POSDEPTH(1) | CONSIDERABLES => CONSIDERABLES,
            POSDEPTH(d) => POSDEPTH(d - 1),
        }
    }
}

#[derive(Clone)]
pub struct EvalResult {
    pub sorted_moves: Option<Vec<Move>>,
    pub depth: Depth,
    pub eval: Eval,
    pub pruned: bool,
}

pub fn eval_piece(piece: &Piece, index: usize) -> i16 {
    unsafe { PIECE_VALUES[piece.color as usize][piece.piece_type as usize][index] }
}

pub fn eval_move(m: &Move) -> i16 {
    match m {
        SLIDE(from_index, to_index, from_piece, to_piece_option, _) => {
            eval_piece(from_piece, *to_index as usize)
                - eval_piece(from_piece, *from_index as usize)
                + match to_piece_option {
                    None => 0,
                    Some(to_piece) => eval_piece(to_piece, *to_index as usize),
                }
        }
        PAWNPUSH(from_index, _) => {
            if *from_index < 32 {
                let pawn = Piece::new(WHITE, PAWN);
                eval_piece(&pawn, (from_index + 16) as usize)
                    - eval_piece(&pawn, *from_index as usize)
            } else {
                let pawn = Piece::new(BLACK, PAWN);
                eval_piece(&pawn, (from_index - 16) as usize)
                    - eval_piece(&pawn, *from_index as usize)
            }
        }
        PROMOTION(from_index, to_index, from_piece, to_piece_option, promo, _) => {
            eval_piece(promo, *to_index as usize) - eval_piece(from_piece, *from_index as usize)
                + match to_piece_option {
                    None => 0,
                    Some(to_piece) => eval_piece(to_piece, *to_index as usize),
                }
        }
        PASSANT(from_index, to_index) => {
            if *from_index < 32 {
                eval_piece(&Piece::new(WHITE, PAWN), (to_index + 8) as usize)
                    + eval_piece(&Piece::new(BLACK, PAWN), (*to_index) as usize)
                    - eval_piece(&Piece::new(BLACK, PAWN), (*from_index) as usize)
            } else {
                eval_piece(&Piece::new(BLACK, PAWN), (to_index - 8) as usize)
                    + eval_piece(&Piece::new(WHITE, PAWN), (*to_index) as usize)
                    - eval_piece(&Piece::new(WHITE, PAWN), (*from_index) as usize)
            }
        }

        CASTLE(sook_index, _passant_before) => {
            let color = if *sook_index < 32 { WHITE } else { BLACK };
            if (sook_index & 0b111) == 0 {
                //castle left
                -eval_piece(&Piece::new(color, SOOK), (*sook_index) as usize)
                    + eval_piece(&Piece::new(color, ROOK), (*sook_index + 3) as usize)
                    - eval_piece(&Piece::new(color, KING), (*sook_index + 4) as usize)
                    + eval_piece(&Piece::new(color, KING), (*sook_index + 2) as usize)
            } else {
                //castle right
                -eval_piece(&Piece::new(color, SOOK), (*sook_index) as usize)
                    + eval_piece(&Piece::new(color, ROOK), (*sook_index - 2) as usize)
                    - eval_piece(&Piece::new(color, KING), (*sook_index - 3) as usize)
                    + eval_piece(&Piece::new(color, KING), (*sook_index - 1) as usize)
            }
        }
        KINGMOVE(
            from_index,
            to_index,
            from_piece,
            to_piece_option,
            castle_left,
            castle_right,
            _,
        ) => {
            let mut ans = eval_piece(from_piece, *to_index as usize)
                - eval_piece(from_piece, *from_index as usize)
                + match to_piece_option {
                    None => 0,
                    Some(to_piece) => eval_piece(to_piece, *to_index as usize),
                };
            if *castle_left {
                ans += eval_piece(
                    &Piece::new(from_piece.color, ROOK),
                    ((from_index >> 3) << 3) as usize,
                ) - eval_piece(
                    &Piece::new(from_piece.color, SOOK),
                    ((from_index >> 3) << 3) as usize,
                );
            }
            if *castle_right {
                ans += eval_piece(
                    &Piece::new(from_piece.color, ROOK),
                    (((from_index >> 3) << 3) + 7) as usize,
                ) - eval_piece(
                    &Piece::new(from_piece.color, SOOK),
                    (((from_index >> 3) << 3) + 7) as usize,
                );
            }
            ans
        }
    }
}
pub fn eval_board(board: &Board) -> Eval {
    let mut sum = 0;
    for i in 0..64 {
        let piece_option = board.pieces[i];
        match piece_option {
            None => {}
            Some(piece) => {
                if piece.color == board.turn {
                    sum += eval_piece(&piece, i)
                } else {
                    sum -= eval_piece(&piece, i)
                }
            }
        }
    }
    CP(sum)
}

pub fn bestmove(
    b: &mut Board,
    hash_set: &mut HashSet<BoardHash>,
    timeleft: Option<i32>,
    max_depth: Option<i32>,
) -> EvalResult {
    let target_time = Duration::from_millis(match timeleft {
        None => 5000,
        Some(time) => std::cmp::min((if time < 0 { 0 } else { time as u64 }) / 40, 5000),
    });
    let target_depth = match max_depth {
        None => 100,
        Some(depth) => i32::min(i32::max(depth, 0), u8::MAX as i32) as u8,
    };

    let end_time = Instant::now() + target_time;
    let mut hash_table = HashMap::new();
    hash_table.insert(
        0,
        EvalResult {
            sorted_moves: None,
            depth: POSDEPTH(u8::max_value()),
            eval: CP(0),
            pruned: false,
        },
    );
    let mut child_hash = 0;
    let board_hash = hash_board(b);

    for depth in 2..=target_depth {
        if depth != 2 && Instant::now() > end_time {
            break;
        }
        child_hash = minimax(
            b,
            POSDEPTH(depth),
            MATE(0),
            &mut hash_table,
            hash_set,
            true,
            &end_time,
            board_hash,
        );
        let score = hash_table.get(&child_hash).unwrap().eval;
        let mut pv = Vec::new();
        principal_variation(&mut pv, b, child_hash, &hash_table);

        print!("info depth {depth} score {score} pv");
        for m in pv{
            print!(" {m}");
        }
        println!();
    }
    hash_table.get(&child_hash).unwrap().clone()
}

fn principal_variation(moves: &mut Vec<Move>, b: &mut Board, root_hash: BoardHash, hash_table: &HashMap<BoardHash, EvalResult>) {
    if let Some(eval_result) = hash_table.get(&root_hash) {
        if let Some(sorted_moves) = &eval_result.sorted_moves {
            if let Some(m) = sorted_moves.get(0){
                let child_board_hash = update_hash(b, root_hash, m);
                b.take_move(&m);
                let eval_here = eval_result.eval;
                if let Some(result_from_child) = hash_table.get(&child_board_hash){
                    if result_from_child.eval.one_higher() == eval_here {
                        moves.push(m.clone());
                        principal_variation(moves, b, child_board_hash, hash_table);
                    }
                }
                b.take_move_back(&m)
            }
        }
    }
}


fn minimax(
    b: &mut Board,
    d: Depth,
    prune_eval: Eval,
    hash_table: &mut HashMap<BoardHash, EvalResult>,
    hash_set: &mut HashSet<BoardHash>,
    is_top_level: bool,
    end_time: &Instant,
    hash: BoardHash,
) -> BoardHash {

    // Uncomment to check hashes
    // if hash != hash_board(b) {
    //     print!("acthash: {:#066b}", hash_board(b));
    //     b.print_board();
    //     panic!("HASHES NOT EQUAL!!!");
    // }

    //check for a repeat
    if !is_top_level && hash_set.contains(&hash) {
        return 0;
    }
    //check for a table hit
    let mut hit_option: Option<EvalResult> = None;
    match hash_table.get(&hash) {
        None => {}
        Some(eval_result) => {
            if !eval_result.pruned || eval_result.eval.one_higher() <= prune_eval {
                //if this eval wasn't pruned, or if the eval is good enough to not be
                if eval_result.depth >= d {
                    return hash;
                } else if let MATE(_) = eval_result.eval {
                    return hash;
                }
            }
            hit_option = Some((*eval_result).clone());
        }
    }
    //handle the base case
    if d == RAW {
        let ans = EvalResult {
            sorted_moves: None,
            depth: d,
            eval: eval_board(&b),
            pruned: false,
        };
        hash_table.insert(hash, ans);
        return hash;
    }

    let mut sorted_moves = match hit_option {
        None => {
            let mut ans = b.get_moves();
            ans.sort_unstable_by_key(|m| -eval_move(m));
            ans
        }
        Some(hit) => {
            if hit.sorted_moves.is_none() {
                let mut ans = b.get_moves();
                ans.sort_unstable_by_key(|m| -eval_move(m));
                ans
            } else {
                hit.sorted_moves.unwrap()
            }
        }
    };

    //handle end of game
    if sorted_moves.len() == 0 {
        let ans = if b.is_check() {
            //a checkmate
            EvalResult {
                sorted_moves: Some(sorted_moves),
                depth: d,
                eval: MATE(0),
                pruned: false,
            }
        } else {
            //a draw
            EvalResult {
                sorted_moves: Some(sorted_moves),
                depth: d,
                eval: CP(0),
                pruned: false,
            }
        };

        hash_table.insert(hash, ans);
        return hash;
    }

    //You're not forced to take a considerables move'
    let mut best_eval = if d == CONSIDERABLES {
        eval_board(b)
    } else {
        MATE(0)
    };
    let mut eval_map = HashMap::new();

    if d == CONSIDERABLES && eval_move(&(sorted_moves.get(0).unwrap())) < CONSIDERABLE_THRESHOLD {
        let ans = EvalResult {
            sorted_moves: Some(sorted_moves),
            depth: d,
            eval: eval_board(&b),
            pruned: false,
        };
        hash_table.insert(hash, ans);
        return hash;
    }

    let mut pruned = false;

    //UNCOMMENT TO HANDLE REPEATS DURING SEARCH
    //hash_set.insert(hash);

    for m in &sorted_moves {
        if d == CONSIDERABLES && eval_move(&m) < CONSIDERABLE_THRESHOLD {
            break;
        }
        let child_board_hash = update_hash(b, hash, m);
        b.take_move(&m);
        let hash_of_child = minimax(
            b,
            d.one_lower(),
            best_eval,
            hash_table,
            hash_set,
            false,
            end_time,
            child_board_hash,
        );
        let eval_from_child = hash_table.get(&hash_of_child).unwrap().eval.one_higher();
        eval_map.insert(m.clone(), eval_from_child);

        if eval_from_child > best_eval {
            best_eval = eval_from_child;
            if eval_from_child.one_higher() <= prune_eval {
                b.take_move_back(&m);
                pruned = true;
                break;
            }
        }
        b.take_move_back(&m);

        if is_top_level && Instant::now() > *end_time {
            break;
        }
    }
    //UNCOMMENT TO HANDLE REPEATS DURING SEARCH
    //hash_set.remove(&hash);

    sorted_moves.sort_by_key(|m| (*eval_map.get(m).unwrap_or(&MATE(0))).one_higher());

    let ans = EvalResult {
        sorted_moves: Some(sorted_moves),
        depth: d,
        eval: best_eval,
        pruned: pruned,
    };

    hash_table.insert(hash, ans);
    return hash;
}
