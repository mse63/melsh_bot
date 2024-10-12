use crate::board::*;
use crate::eval::Eval::*;
use crate::eval::*;
use crate::hash::*;

use crate::CONSIDERABLE_THRESHOLD;
use std::collections::HashMap;
use std::collections::HashSet;
use std::time::*;
use Depth::*;

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Depth {
    Raw,
    Considerables,
    Pos(u8),
}

impl Depth {
    pub fn one_lower(&self) -> Depth {
        match self {
            Raw => Raw,
            Pos(1) | Considerables => Considerables,
            Pos(d) => Pos(d - 1),
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

pub struct AIPlayer {
    evaluator: Evaluator,
    hash_table: HashMap<BoardHash, EvalResult>,
}

impl AIPlayer {
    pub fn new(evaluator: Evaluator) -> Self {
        Self {
            evaluator,
            hash_table: HashMap::new(),
        }
    }
    pub fn bestmove(
        &mut self,
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
            None => u8::MAX,
            Some(depth) => i32::min(i32::max(depth, 0), u8::MAX as i32) as u8,
        };

        let end_time = Instant::now() + target_time;
        self.hash_table.insert(
            0,
            EvalResult {
                sorted_moves: None,
                depth: Pos(u8::MAX),
                eval: CentiPawns(0),
                pruned: false,
            },
        );
        let mut child_hash = 0;
        let board_hash = hash_board(b);

        for depth in 2..=target_depth {
            if depth != 2 && Instant::now() > end_time {
                break;
            }
            child_hash = self.minimax(
                b,
                Pos(depth),
                Mate(0),
                hash_set,
                true,
                &end_time,
                board_hash,
            );
            let score = self.hash_table.get(&child_hash).unwrap().eval;
            let mut pv = Vec::new();
            self.principal_variation(&mut pv, b, child_hash, &mut HashMap::new());

            print!("info depth {depth} score {score} pv");
            for m in pv {
                print!(" {m}");
            }
            println!();
            if let Mate(_) = score {
                break;
            }
        }
        //TODO: Make this smart and only delete table entries for positions outside of the set you've returned.
        let ans = self.hash_table.get(&child_hash).unwrap().clone();
        self.hash_table = HashMap::new();
        ans
    }

    fn principal_variation(
        &self,
        moves: &mut Vec<Move>,
        b: &mut Board,
        root_hash: BoardHash,
        position_count: &mut HashMap<BoardHash, u8>,
    ) {
        if let Some(eval_result) = self.hash_table.get(&root_hash) {
            if let Some(sorted_moves) = &eval_result.sorted_moves {
                if let Some(m) = sorted_moves.first() {
                    let child_board_hash = update_hash(b, root_hash, m);
                    b.take_move(m);
                    let eval_here = eval_result.eval;
                    if let Some(result_from_child) = self.hash_table.get(&child_board_hash) {
                        if result_from_child.eval.one_higher() == eval_here {
                            moves.push(m.clone());
                            *position_count.entry(child_board_hash).or_insert(0) += 1;
                            if let Some(3) = position_count.get(&child_board_hash) {
                            } else {
                                self.principal_variation(
                                    moves,
                                    b,
                                    child_board_hash,
                                    position_count,
                                );
                            }
                        }
                    }
                    b.take_move_back(m)
                }
            }
        }
    }

    fn minimax(
        &mut self,
        b: &mut Board,
        d: Depth,
        prune_eval: Eval,
        hash_set: &mut HashSet<BoardHash>,
        is_top_level: bool,
        end_time: &Instant,
        hash: BoardHash,
    ) -> BoardHash {
        //check for a repeat
        if !is_top_level && hash_set.contains(&hash) {
            return 0;
        }

        //check for a table hit
        let mut hit_option: Option<EvalResult> = None;
        match self.hash_table.get(&hash) {
            None => {}
            Some(eval_result) => {
                if !eval_result.pruned || eval_result.eval.one_higher() <= prune_eval {
                    //if this eval wasn't pruned, or if the eval is good enough to not be
                    if eval_result.depth >= d {
                        return hash;
                    } else if let Mate(_) = eval_result.eval {
                        return hash;
                    }
                }
                hit_option = Some((*eval_result).clone());
            }
        }

        //handle the base case
        if d == Raw {
            let ans = EvalResult {
                sorted_moves: None,
                depth: d,
                eval: self.evaluator.eval_board(b),
                pruned: false,
            };
            self.hash_table.insert(hash, ans);
            return hash;
        }

        let mut sorted_moves = match hit_option {
            None => {
                let mut ans = b.get_moves();
                ans.sort_unstable_by_key(|m| -self.evaluator.eval_move(m));
                ans
            }
            Some(hit) => {
                if hit.sorted_moves.is_none() {
                    let mut ans = b.get_moves();
                    ans.sort_unstable_by_key(|m| -self.evaluator.eval_move(m));
                    ans
                } else {
                    hit.sorted_moves.unwrap()
                }
            }
        };

        //handle end of game
        if sorted_moves.is_empty() {
            let ans = if b.is_check() {
                //a checkmate
                EvalResult {
                    sorted_moves: Some(sorted_moves),
                    depth: Pos(u8::MAX),
                    eval: Mate(0),
                    pruned: false,
                }
            } else {
                //a draw
                EvalResult {
                    sorted_moves: Some(sorted_moves),
                    depth: Pos(u8::MAX),
                    eval: CentiPawns(0),
                    pruned: false,
                }
            };

            self.hash_table.insert(hash, ans);
            return hash;
        }

        //You're not forced to take a considerables move
        let mut best_eval = if d == Considerables {
            self.evaluator.eval_board(b)
        } else {
            Mate(0)
        };
        let mut eval_map = HashMap::new();

        if d == Considerables
            && self.evaluator.eval_move(sorted_moves.first().unwrap()) < CONSIDERABLE_THRESHOLD
        {
            let ans = EvalResult {
                sorted_moves: Some(sorted_moves),
                depth: d,
                eval: self.evaluator.eval_board(b),
                pruned: false,
            };
            self.hash_table.insert(hash, ans);
            return hash;
        }

        let mut pruned = false;

        for m in &sorted_moves {
            if d == Considerables && self.evaluator.eval_move(m) < CONSIDERABLE_THRESHOLD {
                break;
            }
            let child_board_hash = update_hash(b, hash, m);
            b.take_move(m);
            let hash_of_child = self.minimax(
                b,
                d.one_lower(),
                best_eval,
                hash_set,
                false,
                end_time,
                child_board_hash,
            );
            let eval_from_child = self
                .hash_table
                .get(&hash_of_child)
                .unwrap()
                .eval
                .one_higher();
            eval_map.insert(m.clone(), eval_from_child);

            if eval_from_child > best_eval {
                best_eval = eval_from_child;
                if eval_from_child.one_higher() <= prune_eval {
                    b.take_move_back(m);
                    pruned = true;
                    break;
                }
            }
            b.take_move_back(m);

            if is_top_level && Instant::now() > *end_time {
                break;
            }
        }

        sorted_moves.sort_by_key(|m| (*eval_map.get(m).unwrap_or(&Mate(0))).one_higher());

        let ans = EvalResult {
            sorted_moves: Some(sorted_moves),
            depth: d,
            eval: best_eval,
            pruned,
        };

        self.hash_table.insert(hash, ans);
        hash
    }
}
