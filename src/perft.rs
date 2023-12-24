use crate::board::*;
use crate::hash::BoardHash;
use std::collections::HashSet;

pub fn perft(b: &mut Board, depth: u8) {
    let moves = b.get_moves();
    let mut ans = 0;
    for m in moves {
        b.take_move(&m);
        let n = count_pos(b, depth - 1);
        b.take_move_back(&m);
        println!("{}: {}", m, n);
        ans += n;
    }
    println!("Total: {}", ans);
}

pub fn _speed_test() {
    let b = &mut Board::from_fen(
        "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1".to_string(),
    );
    loop {
        for depth in 1..6 {
            let now = std::time::Instant::now();
            let n = _count_pos_unoptimized(b, depth);
            let elapsed_time = now.elapsed();
            println!("{}: {:?}", n, elapsed_time)
        }
    }
}

pub fn count_pos(b: &mut Board, depth: u8) -> usize {
    if depth == 0 {
        return 1;
    }
    if depth == 1 {
        return b.get_moves().len();
    }
    let mut ans = 0;
    for m in b.get_moves() {
        b.take_move(&m);
        //println!("took: {}", m);
        //b.print_board();
        ans += count_pos(b, depth - 1);
        b.take_move_back(&m);
        //println!("took back: {}", m);
        //b.print_board();
    }
    return ans;
}
pub fn _count_pos_unoptimized(b: &mut Board, depth: u8) -> usize {
    if depth == 0 {
        return 1;
    }
    let mut ans = 0;
    for m in b.get_moves() {
        b.take_move(&m);
        ans += _count_pos_unoptimized(b, depth - 1);
        b.take_move_back(&m);
    }
    return ans;
}
pub fn _count_pos_unique(b: &mut Board, depth: u8, set: &mut HashSet<BoardHash>) -> usize {
    if depth == 0 {
        let hash = crate::hash::hash_board(b);
        if set.contains(&hash) {
            return 0;
        } else {
            set.insert(hash);
            return 1;
        }
    }
    let mut ans = 0;
    for m in b.get_moves() {
        b.take_move(&m);
        ans += _count_pos_unique(b, depth - 1, set);
        b.take_move_back(&m);
    }
    return ans;
}
