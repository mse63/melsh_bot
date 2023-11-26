use crate::board::*;
use crate::hash::BoardHash;
use std::collections::HashSet;

pub fn perft() {
    let fenstring = "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10";
    let mut b = Board::from_fen(fenstring.to_string());
    b.print_board();
    let args: Vec<String> = std::env::args().collect();
    let depth: i8 = args[1].parse().unwrap();
    let moves = b.get_moves();
    let mut ans = 0;
    for m in moves {
        b.take_move(&m);
        //println!("took: {}", m);
        //b.print_board();
        let n = count_pos(&mut b, depth - 1);
        b.take_move_back(&m);
        //println!("took back: {}", m);
        //b.print_board();
        println!("{}: {}", m, n);
        ans += n;
    }
    println!("Total: {}", ans);
}

pub fn speed_test() {
    let b = &mut Board::from_fen(
        "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1".to_string(),
    );
    loop {
        for depth in 1..6 {
            let now = std::time::Instant::now();
            let n = count_pos_unoptimized(b, depth);
            let elapsed_time = now.elapsed();
            println!("{}: {:?}", n, elapsed_time)
        }
    }
}

pub fn count_pos(b: &mut Board, depth: i8) -> usize {
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
pub fn count_pos_unoptimized(b: &mut Board, depth: i8) -> usize {
    if depth == 0 {
        return 1;
    }
    let mut ans = 0;
    for m in b.get_moves() {
        b.take_move(&m);
        ans += count_pos_unoptimized(b, depth - 1);
        b.take_move_back(&m);
    }
    return ans;
}
pub fn count_pos_unique(b: &mut Board, depth: i8, set: &mut HashSet<BoardHash>) -> usize {
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
        ans += count_pos_unique(b, depth - 1, set);
        b.take_move_back(&m);
    }
    return ans;
}
