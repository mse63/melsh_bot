use std::str::from_utf8_unchecked_mut;

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
    for c in 0..2 {
        for p in 0..7 {
            for i in 0..64 {
                unsafe {
                    PIECE_HASHES[c][p][i] = random();
                    PASSANT_HASHES[i] = random();
                }
            }
        }
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
    if board.turn == BLACK {
        hash ^= BoardHash::MAX;
    }
    match board.passant_square {
        None => hash,
        Some(passant) => unsafe { hash ^ PASSANT_HASHES[passant as usize] },
    }
}

pub fn update_hash(b: &Board, hash: BoardHash, m: &Move) -> BoardHash {
    let move_hash = match m {
        SLIDE(from_index, to_index, from_piece, to_piece_option, _) => {
            hash_piece(*from_piece, *to_index as usize)
                ^ hash_piece(*from_piece, *from_index as usize)
                ^ match to_piece_option {
                    None => 0,
                    Some(to_piece) => hash_piece(*to_piece, *to_index as usize),
                }
        }
        PAWNPUSH(from_index, _) => {
            if *from_index < 32 {
                let pawn = Piece::new(WHITE, PAWN);
                hash_piece(pawn, (from_index + 16) as usize)
                    ^ hash_piece(pawn, *from_index as usize)
                    ^ unsafe { PASSANT_HASHES[*from_index as usize + 8] }
            } else {
                let pawn = Piece::new(BLACK, PAWN);
                hash_piece(pawn, (from_index - 16) as usize)
                    ^ hash_piece(pawn, *from_index as usize)
                    ^ unsafe { PASSANT_HASHES[*from_index as usize - 8] }
            }
        }
        PROMOTION(from_index, to_index, from_piece, to_piece_option, promo, _) => {
            hash_piece(*promo, *to_index as usize)
                ^ hash_piece(*from_piece, *from_index as usize)
                ^ match to_piece_option {
                    None => 0,
                    Some(to_piece) => hash_piece(*to_piece, *to_index as usize),
                }
        }
        PASSANT(from_index, to_index) => {
            if *from_index < 32 {
                hash_piece(Piece::new(WHITE, PAWN), (to_index + 8) as usize)
                    ^ hash_piece(Piece::new(BLACK, PAWN), (*to_index) as usize)
                    ^ hash_piece(Piece::new(BLACK, PAWN), (*from_index) as usize)
            } else {
                hash_piece(Piece::new(BLACK, PAWN), (to_index - 8) as usize)
                    ^ hash_piece(Piece::new(WHITE, PAWN), (*to_index) as usize)
                    ^ hash_piece(Piece::new(WHITE, PAWN), (*from_index) as usize)
            }
        }

        CASTLE(sook_index, _passant_before) => {
            let color = if *sook_index < 32 { WHITE } else { BLACK };
            if (sook_index & 0b111) == 0 {
                //castle left
                hash_piece(Piece::new(color, SOOK), (*sook_index) as usize)
                    ^ hash_piece(Piece::new(color, ROOK), (*sook_index + 3) as usize)
                    ^ hash_piece(Piece::new(color, KING), (*sook_index + 4) as usize)
                    ^ hash_piece(Piece::new(color, KING), (*sook_index + 2) as usize)
            } else {
                //castle right
                hash_piece(Piece::new(color, SOOK), (*sook_index) as usize)
                    ^ hash_piece(Piece::new(color, ROOK), (*sook_index - 2) as usize)
                    ^ hash_piece(Piece::new(color, KING), (*sook_index - 3) as usize)
                    ^ hash_piece(Piece::new(color, KING), (*sook_index - 1) as usize)
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
            let mut ans = hash_piece(*from_piece, *to_index as usize)
                ^ hash_piece(*from_piece, *from_index as usize)
                ^ match to_piece_option {
                    None => 0,
                    Some(to_piece) => hash_piece(*to_piece, *to_index as usize),
                };
            if *castle_left {
                ans ^= hash_piece(
                    Piece::new(from_piece.color, ROOK),
                    ((from_index >> 3) << 3) as usize,
                ) ^ hash_piece(
                    Piece::new(from_piece.color, SOOK),
                    ((from_index >> 3) << 3) as usize,
                );
            }
            if *castle_right {
                ans ^= hash_piece(
                    Piece::new(from_piece.color, ROOK),
                    (((from_index >> 3) << 3) + 7) as usize,
                ) ^ hash_piece(
                    Piece::new(from_piece.color, SOOK),
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
    let new_hash = hash ^ move_hash ^ passant_hash ^ BoardHash::MAX;
    return new_hash;
}
