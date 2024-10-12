use crate::Board;
use crate::Color::*;
use crate::Move;
use crate::Move::*;
use crate::Piece;
use crate::PieceType::*;
use rand::random;
pub type BoardHash = u64;
static mut PIECE_HASHES: [[[BoardHash; 64]; 7]; 2] = [[[0; 64]; 7]; 2];
static mut PASSANT_HASHES: [BoardHash; 64] = [0; 64];

pub fn init_hashes() {
    unsafe {
        PIECE_HASHES
            .iter_mut()
            .flatten()
            .flatten()
            .for_each(|x| *x = random());
        PASSANT_HASHES.iter_mut().for_each(|x| *x = random());
    }
}

fn hash_piece(piece: Piece, index: usize) -> BoardHash {
    unsafe { PIECE_HASHES[piece.color as usize][piece.piece_type as usize][index] }
}

pub fn hash_board(board: &Board) -> BoardHash {
    let mut hash = 0;
    for i in 0..64 {
        let piece_option = board.pieces[i];
        match piece_option {
            None => {}
            Some(piece) => hash ^= hash_piece(piece, i),
        }
    }
    if board.turn == Black {
        hash ^= BoardHash::MAX;
    }
    match board.passant_square {
        None => hash,
        Some(passant) => unsafe { hash ^ PASSANT_HASHES[passant as usize] },
    }
}

pub fn update_hash(b: &Board, hash: BoardHash, m: &Move) -> BoardHash {
    let move_hash = match m {
        Slide {
            from_index,
            to_index,
            piece_moved,
            piece_there,
            passant_before: _,
        } => {
            hash_piece(*piece_moved, *to_index as usize)
                ^ hash_piece(*piece_moved, *from_index as usize)
                ^ match piece_there {
                    None => 0,
                    Some(to_piece) => hash_piece(*to_piece, *to_index as usize),
                }
        }
        PawnPush {
            from_index,
            passant_before: _,
        } => {
            if *from_index < 32 {
                let pawn = Piece::new(White, Pawn);
                hash_piece(pawn, (from_index + 16) as usize)
                    ^ hash_piece(pawn, *from_index as usize)
                    ^ unsafe { PASSANT_HASHES[*from_index as usize + 8] }
            } else {
                let pawn = Piece::new(Black, Pawn);
                hash_piece(pawn, (from_index - 16) as usize)
                    ^ hash_piece(pawn, *from_index as usize)
                    ^ unsafe { PASSANT_HASHES[*from_index as usize - 8] }
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
            hash_piece(*promotion, *to_index as usize)
                ^ hash_piece(*piece_moved, *from_index as usize)
                ^ match piece_there {
                    None => 0,
                    Some(to_piece) => hash_piece(*to_piece, *to_index as usize),
                }
        }
        Passant {
            from_index,
            to_index,
        } => {
            if *from_index < 32 {
                hash_piece(Piece::new(White, Pawn), (to_index + 8) as usize)
                    ^ hash_piece(Piece::new(Black, Pawn), (*to_index) as usize)
                    ^ hash_piece(Piece::new(Black, Pawn), (*from_index) as usize)
            } else {
                hash_piece(Piece::new(Black, Pawn), (to_index - 8) as usize)
                    ^ hash_piece(Piece::new(White, Pawn), (*to_index) as usize)
                    ^ hash_piece(Piece::new(White, Pawn), (*from_index) as usize)
            }
        }

        Castle {
            sook_index,
            passant_before: _,
        } => {
            let color = if *sook_index < 32 { White } else { Black };
            if (sook_index & 0b111) == 0 {
                //castle left
                hash_piece(Piece::new(color, Sook), (*sook_index) as usize)
                    ^ hash_piece(Piece::new(color, Rook), (*sook_index + 3) as usize)
                    ^ hash_piece(Piece::new(color, King), (*sook_index + 4) as usize)
                    ^ hash_piece(Piece::new(color, King), (*sook_index + 2) as usize)
            } else {
                //castle right
                hash_piece(Piece::new(color, Sook), (*sook_index) as usize)
                    ^ hash_piece(Piece::new(color, Rook), (*sook_index - 2) as usize)
                    ^ hash_piece(Piece::new(color, King), (*sook_index - 3) as usize)
                    ^ hash_piece(Piece::new(color, King), (*sook_index - 1) as usize)
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
            let mut ans = hash_piece(*piece_moved, *to_index as usize)
                ^ hash_piece(*piece_moved, *from_index as usize)
                ^ match piece_there {
                    None => 0,
                    Some(to_piece) => hash_piece(*to_piece, *to_index as usize),
                };
            if *castle_left {
                ans ^= hash_piece(
                    Piece::new(piece_moved.color, Rook),
                    ((from_index >> 3) << 3) as usize,
                ) ^ hash_piece(
                    Piece::new(piece_moved.color, Sook),
                    ((from_index >> 3) << 3) as usize,
                );
            }
            if *castle_right {
                ans ^= hash_piece(
                    Piece::new(piece_moved.color, Rook),
                    (((from_index >> 3) << 3) + 7) as usize,
                ) ^ hash_piece(
                    Piece::new(piece_moved.color, Sook),
                    (((from_index >> 3) << 3) + 7) as usize,
                );
            }
            ans
        }
    };
    let passant_hash = match b.passant_square {
        None => 0,
        Some(passant) => unsafe { PASSANT_HASHES[passant as usize] },
    };
    hash ^ move_hash ^ passant_hash ^ BoardHash::MAX
}
