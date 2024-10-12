use crate::piece_values::PIECE_VALUES;
use crate::Board;
use crate::Color::*;
use crate::Move;
use crate::Move::*;
use crate::Piece;
use crate::PieceType::*;

#[derive(PartialEq, Eq, Clone, Copy)]
pub enum Eval {
    Mate(i8),
    CentiPawns(i16),
}

use Eval::*;

impl Eval {
    pub fn one_higher(&self) -> Eval {
        match self {
            Mate(x) => {
                if *x <= 0 {
                    Mate(-x + 1)
                } else {
                    Mate(-x)
                }
            }
            CentiPawns(x) => CentiPawns(-x),
        }
    }
}

impl std::fmt::Display for Eval {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Mate(i) => {
                write!(f, "mate {}", i)
            }
            CentiPawns(i) => {
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
            (CentiPawns(s), CentiPawns(o)) => s.cmp(&o),
            (Mate(s), CentiPawns(_)) => {
                if s == 0 {
                    std::cmp::Ordering::Less
                } else {
                    s.cmp(&0)
                }
            }
            (CentiPawns(_), Mate(o)) => {
                if o == 0 {
                    std::cmp::Ordering::Greater
                } else {
                    0.cmp(&o)
                }
            }
            (Mate(s), Mate(o)) => {
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

pub struct Evaluator {
    pub piece_values: [[[i16; 64]; 7]; 2],
}

impl Evaluator {
    pub fn new() -> Self {
        unsafe {
            Self {
                piece_values: PIECE_VALUES,
            }
        }
    }

    pub fn from_piece_values(piece_values: [[[i16; 64]; 7]; 2]) -> Self {
        Self { piece_values }
    }

    pub fn eval_piece(&self, piece: &Piece, index: usize) -> i16 {
        self.piece_values[piece.color as usize][piece.piece_type as usize][index]
    }

    pub fn eval_move(&self, m: &Move) -> i16 {
        match m {
            Slide {
                from_index,
                to_index,
                piece_moved,
                piece_there,
                passant_before: _,
            } => {
                self.eval_piece(piece_moved, *to_index as usize)
                    - self.eval_piece(piece_moved, *from_index as usize)
                    + match piece_there {
                        Some(to_piece) => self.eval_piece(to_piece, *to_index as usize),
                        None => 0,
                    }
            }
            PawnPush {
                from_index,
                passant_before: _,
            } => {
                if *from_index < 32 {
                    let pawn = Piece::new(White, Pawn);
                    self.eval_piece(&pawn, (from_index + 16) as usize)
                        - self.eval_piece(&pawn, *from_index as usize)
                } else {
                    let pawn = Piece::new(Black, Pawn);
                    self.eval_piece(&pawn, (from_index - 16) as usize)
                        - self.eval_piece(&pawn, *from_index as usize)
                }
            }
            Promotion {
                from_index,
                to_index,
                piece_moved,
                piece_there,
                promotion,
                passant_before: _,
            } => {
                self.eval_piece(promotion, *to_index as usize)
                    - self.eval_piece(piece_moved, *from_index as usize)
                    + match piece_there {
                        None => 0,
                        Some(to_piece) => self.eval_piece(to_piece, *to_index as usize),
                    }
            }
            Passant {
                from_index,
                to_index,
            } => {
                if *from_index < 32 {
                    self.eval_piece(&Piece::new(White, Pawn), (to_index + 8) as usize)
                        + self.eval_piece(&Piece::new(Black, Pawn), (*to_index) as usize)
                        - self.eval_piece(&Piece::new(Black, Pawn), (*from_index) as usize)
                } else {
                    self.eval_piece(&Piece::new(Black, Pawn), (to_index - 8) as usize)
                        + self.eval_piece(&Piece::new(White, Pawn), (*to_index) as usize)
                        - self.eval_piece(&Piece::new(White, Pawn), (*from_index) as usize)
                }
            }

            Castle {
                sook_index,
                passant_before: _,
            } => {
                let color = if *sook_index < 32 { White } else { Black };
                if (sook_index & 0b111) == 0 {
                    //castle left
                    -self.eval_piece(&Piece::new(color, Sook), (*sook_index) as usize)
                        + self.eval_piece(&Piece::new(color, Rook), (*sook_index + 3) as usize)
                        - self.eval_piece(&Piece::new(color, King), (*sook_index + 4) as usize)
                        + self.eval_piece(&Piece::new(color, King), (*sook_index + 2) as usize)
                } else {
                    //castle right
                    -self.eval_piece(&Piece::new(color, Sook), (*sook_index) as usize)
                        + self.eval_piece(&Piece::new(color, Rook), (*sook_index - 2) as usize)
                        - self.eval_piece(&Piece::new(color, King), (*sook_index - 3) as usize)
                        + self.eval_piece(&Piece::new(color, King), (*sook_index - 1) as usize)
                }
            }
            KingMOVE {
                from_index,
                to_index,
                piece_moved,
                piece_there,
                castle_left,
                castle_right,
                passant_before: _,
            } => {
                let mut ans = self.eval_piece(piece_moved, *to_index as usize)
                    - self.eval_piece(piece_moved, *from_index as usize)
                    + match piece_there {
                        None => 0,
                        Some(to_piece) => self.eval_piece(to_piece, *to_index as usize),
                    };
                if *castle_left {
                    ans += self.eval_piece(
                        &Piece::new(piece_moved.color, Rook),
                        ((from_index >> 3) << 3) as usize,
                    ) - self.eval_piece(
                        &Piece::new(piece_moved.color, Sook),
                        ((from_index >> 3) << 3) as usize,
                    );
                }
                if *castle_right {
                    ans += self.eval_piece(
                        &Piece::new(piece_moved.color, Rook),
                        (((from_index >> 3) << 3) + 7) as usize,
                    ) - self.eval_piece(
                        &Piece::new(piece_moved.color, Sook),
                        (((from_index >> 3) << 3) + 7) as usize,
                    );
                }
                ans
            }
        }
    }

    pub fn eval_board(&self, board: &Board) -> Eval {
        let mut sum = 0;
        for i in 0..64 {
            let piece_option = board.pieces[i];
            match piece_option {
                None => {}
                Some(piece) => {
                    if piece.color == board.turn {
                        sum += self.eval_piece(&piece, i)
                    } else {
                        sum -= self.eval_piece(&piece, i)
                    }
                }
            }
        }
        CentiPawns(sum)
    }
}
