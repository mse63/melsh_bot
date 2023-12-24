use Color::*;
use Move::*;
use PieceType::*;

pub const STARTPOS: &str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";

#[derive(PartialEq, Debug, Eq, Hash, Clone, Copy)]
pub enum Color {
    WHITE,
    BLACK,
}

impl std::ops::Not for Color {
    type Output = Self;
    fn not(self) -> Self::Output {
        match self {
            WHITE => BLACK,
            BLACK => WHITE,
        }
    }
}

#[derive(PartialEq, Debug, Eq, Hash, Clone, Copy)]
pub enum PieceType {
    PAWN,
    KNIGHT,
    BISHOP,
    ROOK,
    SOOK,
    QUEEN,
    KING,
}

#[derive(PartialEq, Debug, Eq, Hash, Clone)]
pub enum Move {
    SLIDE(i8, i8, Piece, Option<Piece>, Option<i8>), //to, from, piece_moved, piece_there, passant_before
    PROMOTION(i8, i8, Piece, Option<Piece>, Piece, Option<i8>), //to, from, piece_moved, piece_there, promotion, passant_before
    CASTLE(i8, Option<i8>),                                     //rook_index, passant_before
    KINGMOVE(i8, i8, Piece, Option<Piece>, bool, bool, Option<i8>), //to, from, piece_moved, piece_there, passant_before
    PAWNPUSH(i8, Option<i8>),                                       //to, passant_before
    PASSANT(i8, i8),                                                //from, to
}

impl std::fmt::Display for Move {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PAWNPUSH(from, _) => {
                let to = if *from < 32 { *from + 16 } else { *from - 16 };
                write!(f, "{}{}", square_name(*from), square_name(to))
            }
            PASSANT(from, to) | KINGMOVE(from, to, _, _, _, _, _) | SLIDE(from, to, _, _, _) => {
                write!(f, "{}{}", square_name(*from), square_name(*to))
            }

            PROMOTION(from, to, piece, _, promo, _) => {
                write!(
                    f,
                    "{}{}{}",
                    square_name(*from),
                    square_name(*to),
                    if piece.piece_type == PAWN {
                        match promo.piece_type {
                            KNIGHT => "n",
                            BISHOP => "b",
                            ROOK => "r",
                            QUEEN => "q",
                            _ => "illegal_promo",
                        }
                    } else {
                        ""
                    }
                )
            }
            CASTLE(rook_index, _) => write!(
                f,
                "{}",
                match rook_index {
                    0 => "e1c1",
                    7 => "e1g1",
                    56 => "e8c8",
                    63 => "e8g8",
                    _ => "IllegalCastle",
                }
            ),
        }
    }
}

#[derive(PartialEq, Debug, Eq, Hash, Clone, Copy)]
pub struct Piece {
    pub color: Color,
    pub piece_type: PieceType,
}

impl Piece {
    pub fn new(color: Color, piece_type: PieceType) -> Self {
        Self { color, piece_type }
    }
}

pub fn piece_name(piece: &Piece) -> String {
    String::from(match piece.color {
        WHITE => "W",
        BLACK => "B",
    }) + match piece.piece_type {
        PAWN => "P",
        KNIGHT => "N",
        BISHOP => "B",
        ROOK => "R",
        SOOK => "S",
        QUEEN => "Q",
        KING => "K",
    }
}

pub fn square_name(s: i8) -> String {
    let byte_vec = vec![(97 + (s & 0b111)) as u8, (49 + (s >> 3)) as u8];
    return String::from_utf8(byte_vec).unwrap();
}

#[derive(Copy, Clone, Ord, PartialOrd, PartialEq, Eq)]
struct Pin {
    index: i8,
    ray: i8,
}

pub struct Board {
    pub pieces: [Option<Piece>; 64],
    pub passant_square: Option<i8>,
    pub white_king_square: i8,
    pub black_king_square: i8,
    pub turn: Color,
}

impl Board {
    pub fn _blank() -> Self {
        const INIT: Option<Piece> = None;
        Self {
            pieces: [INIT; 64],
            passant_square: None,
            white_king_square: 4i8,
            black_king_square: 60i8,
            turn: WHITE,
        }
    }
    pub fn from_fen(fen: String) -> Self {
        const INIT: Option<Piece> = None;
        let mut pieces = [INIT; 64];
        let mut passant_square = None;
        let mut turn = WHITE;
        let mut white_king_square = -1;
        let mut black_king_square = -1;
        let mut i = 0;
        let mut charindex = 0;

        while i < 64 {
            let c: char = fen.chars().nth(charindex).unwrap();
            if char::is_digit(c, 10) {
                i += c.to_digit(10).unwrap() as usize;
                charindex += 1;
                continue;
            }
            let piece_option = if c.is_alphabetic() {
                Some(Piece::new(
                    if c.is_uppercase() { WHITE } else { BLACK },
                    match c.to_ascii_uppercase() {
                        'P' => PAWN,
                        'N' => KNIGHT,
                        'B' => BISHOP,
                        'R' => ROOK,
                        'Q' => QUEEN,
                        'K' => KING,
                        _ => panic!("Unrecognized Character In FEN: {}", c),
                    },
                ))
            } else {
                None
            };

            match piece_option {
                None => {}
                Some(piece) => {
                    let squareindex = 56 + 2 * (i & (0b111)) - i;
                    if piece.color == WHITE && piece.piece_type == KING {
                        white_king_square = squareindex as i8;
                    }
                    if piece.color == BLACK && piece.piece_type == KING {
                        black_king_square = squareindex as i8;
                    }
                    pieces[squareindex] = Some(piece);
                    i += 1;
                }
            }
            charindex += 1;
        }
        while charindex < fen.len() {
            let c: Option<char> = fen.chars().nth(charindex);
            match c {
                Some('K') => pieces[7] = Some(Piece::new(WHITE, SOOK)),
                Some('Q') => pieces[0] = Some(Piece::new(WHITE, SOOK)),
                Some('k') => pieces[63] = Some(Piece::new(BLACK, SOOK)),
                Some('q') => pieces[56] = Some(Piece::new(BLACK, SOOK)),
                None => (),
                _ => (),
            }
            let diff: i8 = (c.unwrap() as i8) - ('a' as i8);
            if diff > 0 && diff < 8 {
                let next_char: Option<char> = fen.chars().nth(charindex + 1);
                if c == Some('b') && next_char == Some(' ') {
                    turn = BLACK;
                } else {
                    passant_square =
                        Some(diff + 8 * (-1 + next_char.unwrap().to_digit(10).unwrap() as i8));
                }
            }
            charindex += 1;
        }

        Self {
            pieces,
            passant_square,
            turn,
            white_king_square,
            black_king_square,
        }
    }

    pub fn print_board(&self) {
        for i in 0..64 {
            if self.passant_square == Some(56 - i + 2 * (i & 0b111)) {
                print!("--");
            } else {
                let piece_option = self.pieces[(56 - i + 2 * (i & 0b111)) as usize].as_ref();
                print!(
                    "{}",
                    match piece_option {
                        None => {
                            "  ".to_string()
                        }
                        Some(piece) => {
                            piece_name(piece)
                        }
                    }
                );
            }
            if (i % 8) == 7 {
                println!();
            }
        }
        println!(
            "passant_square: {}",
            match self.passant_square {
                None => {
                    "None".to_string()
                }
                Some(x) => {
                    square_name(x)
                }
            }
        );
        println!("White King: {}", square_name(self.white_king_square));
        println!("Black King: {}", square_name(self.black_king_square));
        println!(
            "Turn: {}",
            match self.turn {
                WHITE => {
                    "WHITE"
                }
                BLACK => {
                    "BLACK"
                }
            }
        );
    }

    pub fn is_check(&self) -> bool {
        !self.is_safe(
            self.turn,
            match self.turn {
                WHITE => self.white_king_square,
                BLACK => self.black_king_square,
            },
        )
    }

    fn ydiff_of_ray(ray: i8) -> i8 {
        ((18 + ray) >> 3) - 2
    }

    fn blocker(&self, index: i8, ray: i8) -> Option<i8> {
        let mut prevtier = index;
        let mut frontier = index + ray;
        let ydiff = Self::ydiff_of_ray(ray);
        while frontier >= 0 && frontier < 64 && (frontier >> 3) - (prevtier >> 3) == ydiff {
            match self.pieces[frontier as usize] {
                None => {}
                _ => return Some(frontier),
            }
            prevtier = frontier;
            frontier += ray;
        }
        return None;
    }

    fn safe_from_type(
        &self,
        color: Color,
        rays: impl Iterator<Item = i8>,
        index: i8,
        piece_type: PieceType,
    ) -> bool {
        let threats = rays
            .filter(|ray| {
                let square = index + ray;
                (square >> 3) - (index >> 3) == Self::ydiff_of_ray(*ray)
                    && square >= 0
                    && square < 64
            })
            .filter_map(|ray| (self.pieces[(index + ray) as usize]).as_ref());
        for threat in threats {
            if (*threat).color != color && (*threat).piece_type == piece_type {
                return false;
            }
        }

        true
    }

    fn is_safe(&self, color: Color, index: i8) -> bool {
        match color {
            WHITE => {
                if !self.safe_from_type(WHITE, [7, 9].into_iter(), index, PAWN) {
                    return false;
                }
            }
            BLACK => {
                if !self.safe_from_type(BLACK, [-7, -9].into_iter(), index, PAWN) {
                    return false;
                }
            }
        }
        if !self.safe_from_type(color, [-9, -8, -7, -1, 1, 7, 8, 9].into_iter(), index, KING) {
            return false;
        }
        //println!("{} passed the safety check", index);
        if !self.safe_from_type(
            color,
            [-17, -15, -10, -6, 6, 10, 15, 17].into_iter(),
            index,
            KNIGHT,
        ) {
            return false;
        }
        for ray in [-8, -1, 1, 8] {
            match self.blocker(index, ray) {
                None => {}
                Some(threatsquare) => {
                    let attacker = (self.pieces[threatsquare as usize]).as_ref().unwrap();
                    if attacker.color != color
                        && (attacker.piece_type == ROOK
                            || attacker.piece_type == SOOK
                            || attacker.piece_type == QUEEN)
                    {
                        return false;
                    }
                }
            }
        }
        for ray in [-9, -7, 7, 9] {
            match self.blocker(index, ray) {
                None => {}
                Some(threatsquare) => {
                    let attacker = (self.pieces[threatsquare as usize]).as_ref().unwrap();
                    if attacker.color != color
                        && (attacker.piece_type == BISHOP || attacker.piece_type == QUEEN)
                    {
                        return false;
                    }
                }
            }
        }
        return true;
    }

    pub fn take_move(&mut self, m: &Move) {
        self.turn = !self.turn;
        match m {
            SLIDE(from_index, to_index, from_piece, _to_piece, _passant_before) => {
                self.pieces[*from_index as usize] = None;
                self.pieces[*to_index as usize] = Some(*from_piece);
                self.passant_square = None;
            }
            PROMOTION(from_index, to_index, _from_piece, _to_piece, promotion, _passant_before) => {
                self.pieces[*from_index as usize] = None;
                self.pieces[*to_index as usize] = Some(*promotion);
                self.passant_square = None
            }
            CASTLE(sook_index, _passant_before) => {
                self.pieces[*sook_index as usize] = None;
                let color = if *sook_index < 32 { WHITE } else { BLACK };
                if (sook_index & 0b111) == 0 {
                    //castle left
                    self.pieces[(*sook_index) as usize] = None;
                    self.pieces[(*sook_index + 2) as usize] = Some(Piece::new(color, KING));
                    self.pieces[(*sook_index + 3) as usize] = Some(Piece::new(color, ROOK));
                    self.pieces[(*sook_index + 4) as usize] = None;
                    match color {
                        WHITE => self.white_king_square = sook_index + 2,
                        BLACK => self.black_king_square = sook_index + 2,
                    }
                } else {
                    //castle right
                    self.pieces[(*sook_index) as usize] = None;
                    self.pieces[(*sook_index - 1) as usize] = Some(Piece::new(color, KING));
                    self.pieces[(*sook_index - 2) as usize] = Some(Piece::new(color, ROOK));
                    self.pieces[(*sook_index - 3) as usize] = None;
                    match color {
                        WHITE => self.white_king_square = sook_index - 1,
                        BLACK => self.black_king_square = sook_index - 1,
                    }
                }
                self.passant_square = None;
            }
            KINGMOVE(
                from_index,
                to_index,
                from_piece,
                _to_piece,
                castle_left,
                castle_right,
                _passant_before,
            ) => {
                self.pieces[*from_index as usize] = None;
                self.pieces[*to_index as usize] = Some(*from_piece);
                if *castle_left {
                    self.pieces[((*from_index >> 3) << 3) as usize] =
                        Some(Piece::new(from_piece.color, ROOK));
                }
                if *castle_right {
                    self.pieces[(((*from_index >> 3) << 3) + 7) as usize] =
                        Some(Piece::new(from_piece.color, ROOK));
                }

                match from_piece.color {
                    WHITE => self.white_king_square = *to_index,
                    BLACK => self.black_king_square = *to_index,
                }

                self.passant_square = None;
            }
            PAWNPUSH(from_index, _passant_before) => {
                self.pieces[*from_index as usize] = None;
                if *from_index < 32 {
                    //White pawn being pushed
                    self.pieces[(from_index + 16) as usize] = Some(Piece::new(WHITE, PAWN));
                    self.passant_square = Some(from_index + 8)
                } else {
                    //Black pawn being pushed
                    self.pieces[(from_index - 16) as usize] = Some(Piece::new(BLACK, PAWN));
                    self.passant_square = Some(from_index - 8)
                }
            }
            PASSANT(from_index, _passant_square) => {
                self.pieces[*from_index as usize] = None;
                if *from_index < 32 {
                    //White is being captured
                    self.pieces[self.passant_square.unwrap() as usize] =
                        Some(Piece::new(BLACK, PAWN));
                    self.pieces[(self.passant_square.unwrap() + 8) as usize] = None;
                } else {
                    //Black is being captured
                    self.pieces[self.passant_square.unwrap() as usize] =
                        Some(Piece::new(WHITE, PAWN));
                    self.pieces[(self.passant_square.unwrap() - 8) as usize] = None;
                }
                self.passant_square = None
            }
        }
    }

    pub fn take_move_back(&mut self, m: &Move) {
        self.turn = !self.turn;
        match m {
            SLIDE(from_index, to_index, from_piece, to_piece, passant_before) => {
                self.pieces[*from_index as usize] = Some(*from_piece);
                self.pieces[*to_index as usize] = *to_piece;
                self.passant_square = *passant_before;
            }
            PROMOTION(from_index, to_index, from_piece, to_piece, _promotion, passant_before) => {
                self.pieces[*from_index as usize] = Some(*from_piece);
                self.pieces[*to_index as usize] = *to_piece;
                self.passant_square = *passant_before;
            }
            CASTLE(sook_index, passant_before) => {
                let color = if *sook_index < 32 {
                    self.white_king_square = ((sook_index >> 3) << 3) + 4;
                    WHITE
                } else {
                    self.black_king_square = ((sook_index >> 3) << 3) + 4;
                    BLACK
                };
                if sook_index & 0b111 == 0 {
                    self.pieces[*sook_index as usize] = Some(Piece::new(color, SOOK));
                    self.pieces[(*sook_index + 2) as usize] = None;
                    self.pieces[(*sook_index + 3) as usize] = None;
                    self.pieces[(*sook_index + 4) as usize] = Some(Piece::new(color, KING))
                } else {
                    self.pieces[*sook_index as usize] = Some(Piece::new(color, SOOK));
                    self.pieces[(*sook_index - 1) as usize] = None;
                    self.pieces[(*sook_index - 2) as usize] = None;
                    self.pieces[(*sook_index - 3) as usize] = Some(Piece::new(color, KING))
                }
                self.passant_square = *passant_before;
            } //rook_index, passant_before
            KINGMOVE(
                from_index,
                to_index,
                from_piece,
                to_piece,
                castle_left,
                castle_right,
                passant_before,
            ) => {
                match from_piece.color {
                    WHITE => self.white_king_square = *from_index,
                    BLACK => self.black_king_square = *from_index,
                }
                self.pieces[*from_index as usize] = Some(*from_piece);
                self.pieces[*to_index as usize] = *to_piece;
                if *castle_left {
                    self.pieces[((*from_index >> 3) << 3) as usize] =
                        Some(Piece::new(from_piece.color, SOOK))
                }
                if *castle_right {
                    self.pieces[(((*from_index >> 3) << 3) + 7) as usize] =
                        Some(Piece::new(from_piece.color, SOOK))
                }

                self.passant_square = *passant_before;
            } //to, from, piece_moved, piece_there, passant_before
            PAWNPUSH(from_index, passant_before) => {
                if *from_index < 32 {
                    self.pieces[(from_index + 16) as usize] = None;
                    self.pieces[(*from_index) as usize] = Some(Piece::new(WHITE, PAWN))
                } else {
                    self.pieces[(from_index - 16) as usize] = None;
                    self.pieces[(*from_index) as usize] = Some(Piece::new(BLACK, PAWN))
                }
                self.passant_square = *passant_before;
            }
            PASSANT(from_index, to_index) => {
                self.passant_square = Some(*to_index);
                self.pieces[*to_index as usize] = None;
                if *to_index < 32 {
                    //white captured
                    self.pieces[*from_index as usize] = Some(Piece::new(BLACK, PAWN));
                    self.pieces[(*to_index + 8) as usize] = Some(Piece::new(WHITE, PAWN));
                } else {
                    //black captured
                    self.pieces[*from_index as usize] = Some(Piece::new(WHITE, PAWN));
                    self.pieces[(*to_index - 8) as usize] = Some(Piece::new(BLACK, PAWN));
                };
            }
        }
    }

    fn is_legal(&mut self, m: &Move) -> bool {
        self.take_move(m);
        let ans = match self.turn {
            WHITE => self.is_safe(BLACK, self.black_king_square),
            BLACK => self.is_safe(WHITE, self.white_king_square),
        };
        self.take_move_back(m);
        ans
    }
    fn get_pins(&self) -> [Pin; 8] {
        let square = match self.turn {
            WHITE => self.white_king_square,
            BLACK => self.black_king_square,
        };
        let rays: [i8; 8] = [-9, -7, 7, 9, -8, -1, 1, 8];
        let mut pins = [Pin { ray: -1, index: 64 }; 8];

        for i in 0..4 {
            let ray = rays[i];
            let potential_pin_option = self.blocker(square, ray);

            match potential_pin_option {
                None => {}
                Some(potential_pin) => {
                    if self.pieces[potential_pin as usize].unwrap().color == self.turn {
                        let threater_option = self.blocker(potential_pin, ray);
                        match threater_option {
                            None => {}
                            Some(threater) => {
                                let threaterpiece = self.pieces[threater as usize].unwrap();
                                if threaterpiece.color != self.turn
                                    && (threaterpiece.piece_type == QUEEN
                                        || threaterpiece.piece_type == BISHOP)
                                {
                                    pins[i] = Pin {
                                        ray,
                                        index: potential_pin,
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        for i in 4..8 {
            let ray = rays[i];
            let potential_pin_option = self.blocker(square, ray);

            match potential_pin_option {
                None => {}
                Some(potential_pin) => {
                    if self.pieces[potential_pin as usize].unwrap().color == self.turn {
                        let threater_option = self.blocker(potential_pin, ray);
                        match threater_option {
                            None => {}
                            Some(threater) => {
                                let threaterpiece = self.pieces[threater as usize].unwrap();
                                if threaterpiece.color != self.turn
                                    && (threaterpiece.piece_type == QUEEN
                                        || threaterpiece.piece_type == SOOK
                                        || threaterpiece.piece_type == ROOK)
                                {
                                    pins[i] = Pin {
                                        ray,
                                        index: potential_pin,
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        pins.sort_unstable();
        pins
    }
    fn add_slides(&self, moves: &mut Vec<Move>, ray: i8, i: i8, piece: Piece) {
        let mut prevtier = i;
        let mut frontier = i + ray;
        let ydiff = Self::ydiff_of_ray(ray);
        while frontier >= 0
            && frontier < 64
            && (frontier >> 3) - (prevtier >> 3) == ydiff
            && (match self.pieces[frontier as usize] {
                None => true,
                Some(p) => p.color != piece.color,
            })
        {
            let to_piece = self.pieces[frontier as usize];
            moves.push(SLIDE(i, frontier, piece, to_piece, self.passant_square));

            match to_piece {
                None => {}
                Some(_) => break,
            }
            prevtier = frontier;
            frontier += ray;
        }
    }

    fn add_sooks(&self, moves: &mut Vec<Move>, ray: i8, i: i8, piece: Piece) {
        let mut prevtier = i;
        let mut frontier = i + ray;
        let ydiff = Self::ydiff_of_ray(ray);
        while frontier >= 0
            && frontier < 64
            && (frontier >> 3) - (prevtier >> 3) == ydiff
            && (match self.pieces[frontier as usize] {
                None => true,
                Some(p) => p.color != piece.color,
            })
        {
            let to_piece = self.pieces[frontier as usize];
            moves.push(PROMOTION(
                i,
                frontier,
                piece,
                to_piece,
                Piece::new(piece.color, ROOK),
                self.passant_square,
            ));

            match to_piece {
                None => {}
                Some(_) => break,
            }
            prevtier = frontier;
            frontier += ray;
        }
    }

    fn consider_passant(
        &mut self,
        moves: &mut Vec<Move>,
        ray: i8,
        color: Color,
        passant_square: i8,
    ) {
        let attacker = self.pieces[(passant_square + ray) as usize];
        match attacker {
            None => {}
            Some(p) => {
                if p.color == color && p.piece_type == PAWN {
                    let m = PASSANT(passant_square + ray, passant_square);
                    if self.is_legal(&m) {
                        moves.push(m);
                    }
                }
            }
        }
    }

    fn add_pawn_vertical_moves(&self, moves: &mut Vec<Move>, index: i8, piece: Piece) {
        match piece.color {
            WHITE => match self.pieces[(index + 8) as usize] {
                None => {
                    if index >> 3 == 6 {
                        for promo in [QUEEN, ROOK, BISHOP, KNIGHT] {
                            moves.push(PROMOTION(
                                index,
                                index + 8,
                                piece,
                                None,
                                Piece::new(piece.color, promo),
                                self.passant_square,
                            ));
                        }
                    } else {
                        if index >> 3 == 1 {
                            match self.pieces[(index + 16) as usize] {
                                None => moves.push(PAWNPUSH(index, self.passant_square)),
                                Some(_) => {}
                            }
                        }
                        moves.push(SLIDE(index, index + 8, piece, None, self.passant_square));
                    }
                }
                Some(_) => {}
            },
            BLACK => match self.pieces[(index - 8) as usize] {
                None => {
                    if index >> 3 == 1 {
                        for promo in [QUEEN, ROOK, BISHOP, KNIGHT] {
                            moves.push(PROMOTION(
                                index,
                                index - 8,
                                piece,
                                None,
                                Piece::new(piece.color, promo),
                                self.passant_square,
                            ));
                        }
                    } else {
                        if index >> 3 == 6 {
                            match self.pieces[(index - 16) as usize] {
                                None => {
                                    moves.push(PAWNPUSH(index, self.passant_square));
                                }
                                Some(_) => {}
                            }
                        }
                        moves.push(SLIDE(index, index - 8, piece, None, self.passant_square));
                    }
                }
                Some(_) => {}
            },
        }
    }

    fn consider_pawn_capture(&self, moves: &mut Vec<Move>, index: i8, piece: Piece, ray: i8) {
        let ray = match piece.color {
            WHITE => ray,
            BLACK => -1 * ray,
        };
        //println!("index: {}, ray: {}", index, ray);
        if index + ray < 0
            || index + ray > 63
            || ((index + ray) >> 3) - (index >> 3) != Self::ydiff_of_ray(ray)
        {
            return;
        }
        let potential = self.pieces[(index + ray) as usize];
        match potential {
            None => {}
            Some(p) => {
                if p.color != piece.color {
                    if piece.color == WHITE && index >> 3 == 6
                        || piece.color == BLACK && index >> 3 == 1
                    {
                        let promos = [QUEEN, ROOK, BISHOP, KNIGHT];
                        for promo in promos {
                            moves.push(PROMOTION(
                                index,
                                index + ray,
                                piece,
                                potential,
                                Piece::new(piece.color, promo),
                                self.passant_square,
                            ))
                        }
                    } else {
                        moves.push(SLIDE(
                            index,
                            index + ray,
                            piece,
                            potential,
                            self.passant_square,
                        ))
                    }
                }
            }
        }
    }

    pub fn get_moves_unchecked(&mut self) -> Vec<Move> {
        let mut moves: Vec<Move> = Vec::new();
        let blockers = self.get_pins();
        // for pin in blockers {
        //     println!("index: {} and ray: {}", pin.index, pin.ray);
        // }
        let mut bi = 0;
        //deal with Passant
        match self.passant_square {
            None => {}
            Some(passant_square) => match self.turn {
                WHITE => {
                    if passant_square > 40 {
                        self.consider_passant(&mut moves, -9, WHITE, passant_square)
                    }
                    if passant_square < 47 {
                        self.consider_passant(&mut moves, -7, WHITE, passant_square)
                    }
                }
                BLACK => {
                    if passant_square > 16 {
                        self.consider_passant(&mut moves, 7, BLACK, passant_square)
                    }
                    if passant_square < 23 {
                        self.consider_passant(&mut moves, 9, BLACK, passant_square)
                    }
                }
            },
        }

        for i in 0i8..64i8 {
            let piece_option = self.pieces[i as usize];
            match piece_option {
                None => {}
                Some(piece) => {
                    if piece.color == self.turn {
                        let is_pinned = blockers[bi].index == i;
                        match piece.piece_type {
                            PAWN => {
                                if !is_pinned || blockers[bi].ray == -8 || blockers[bi].ray == 8 {
                                    self.add_pawn_vertical_moves(&mut moves, i, piece);
                                }
                                if !is_pinned || blockers[bi].ray == -7 || blockers[bi].ray == 7 {
                                    self.consider_pawn_capture(&mut moves, i, piece, 7);
                                }
                                if !is_pinned || blockers[bi].ray == -9 || blockers[bi].ray == 9 {
                                    self.consider_pawn_capture(&mut moves, i, piece, 9);
                                }
                            }

                            KNIGHT => {
                                if !is_pinned {
                                    //a pinned knight can never move
                                    let rays = [-17, -15, -10, -6, 6, 10, 15, 17];
                                    for ray in rays {
                                        let knightsmove = i + ray;
                                        if knightsmove >= 0
                                            && knightsmove < 64
                                            && (knightsmove >> 3) - (i >> 3)
                                                == Self::ydiff_of_ray(ray)
                                            && (match self.pieces[knightsmove as usize] {
                                                None => true,
                                                Some(p) => p.color != piece.color,
                                            })
                                        {
                                            moves.push(SLIDE(
                                                i,
                                                knightsmove,
                                                piece,
                                                self.pieces[knightsmove as usize],
                                                self.passant_square,
                                            ));
                                        }
                                    }
                                }
                            }

                            BISHOP => {
                                if is_pinned {
                                    let ray = blockers[bi].ray;
                                    match ray {
                                        -9 | -7 | 7 | 9 => {
                                            self.add_slides(&mut moves, -1 * ray, i, piece);
                                            self.add_slides(&mut moves, ray, i, piece);
                                        }
                                        _ => {}
                                    }
                                } else {
                                    for ray in [-9, -7, 7, 9] {
                                        self.add_slides(&mut moves, ray, i, piece);
                                    }
                                }
                            }

                            ROOK => {
                                if is_pinned {
                                    let ray = blockers[bi].ray;
                                    match ray {
                                        -8 | -1 | 1 | 8 => {
                                            self.add_slides(&mut moves, -1 * ray, i, piece);
                                            self.add_slides(&mut moves, ray, i, piece);
                                        }
                                        _ => {}
                                    }
                                } else {
                                    for ray in [-8, -1, 1, 8] {
                                        self.add_slides(&mut moves, ray, i, piece);
                                    }
                                }
                            }

                            SOOK => {
                                if (i & (0b111)) == 0 {
                                    if match self.pieces[(i + 4) as usize] {
                                        None => false,
                                        Some(p) => p.color == piece.color && p.piece_type == KING,
                                    } && self.pieces[(i + 1) as usize].is_none()
                                        && self.pieces[(i + 2) as usize].is_none()
                                        && self.pieces[(i + 3) as usize].is_none()
                                        && self.is_safe(piece.color, i + 2)
                                        && self.is_safe(piece.color, i + 3)
                                        && self.is_safe(piece.color, i + 4)
                                    {
                                        moves.push(CASTLE(i, self.passant_square))
                                    }
                                } else {
                                    if match self.pieces[(i - 3) as usize] {
                                        None => false,
                                        Some(p) => p.color == piece.color && p.piece_type == KING,
                                    } && self.pieces[(i - 1) as usize].is_none()
                                        && self.pieces[(i - 2) as usize].is_none()
                                        && self.is_safe(piece.color, i - 1)
                                        && self.is_safe(piece.color, i - 2)
                                        && self.is_safe(piece.color, i - 3)
                                    {
                                        moves.push(CASTLE(i, self.passant_square))
                                    }
                                }
                                for ray in [-8, -1, 1, 8] {
                                    self.add_sooks(&mut moves, ray, i, piece);
                                }
                            }

                            QUEEN => {
                                if is_pinned {
                                    //a pinned queen is always able to move along the ray
                                    let ray = blockers[bi].ray;
                                    self.add_slides(&mut moves, -1 * ray, i, piece);
                                    self.add_slides(&mut moves, ray, i, piece);
                                } else {
                                    for ray in [-9, -7, 7, 9, -8, -1, 1, 8] {
                                        self.add_slides(&mut moves, ray, i, piece);
                                    }
                                }
                            }

                            KING => {
                                let rays = [-9, -8, -7, -1, 1, 7, 8, 9];
                                for ray in rays {
                                    let kingsmove = i + ray;
                                    if kingsmove >= 0
                                        && kingsmove < 64
                                        && (kingsmove >> 3) - (i >> 3) == Self::ydiff_of_ray(ray)
                                        && (match self.pieces[kingsmove as usize] {
                                            None => true,
                                            Some(p) => p.color != piece.color,
                                        } && self.is_safe(piece.color, kingsmove))
                                    {
                                        let can_castle_left =
                                            match self.pieces[((i >> 3) << 3) as usize] {
                                                None => false,
                                                Some(p) => {
                                                    p.piece_type == SOOK && p.color == piece.color
                                                }
                                            };
                                        let can_castle_right =
                                            match self.pieces[(((i >> 3) << 3) + 7) as usize] {
                                                None => false,
                                                Some(p) => {
                                                    p.piece_type == SOOK && p.color == piece.color
                                                }
                                            };
                                        moves.push(KINGMOVE(
                                            i,
                                            kingsmove,
                                            piece,
                                            self.pieces[kingsmove as usize],
                                            can_castle_left,
                                            can_castle_right,
                                            self.passant_square,
                                        ));
                                    }
                                }
                            }
                        }
                        if is_pinned {
                            bi += 1;
                        }
                    }
                }
            }
        }
        moves
    }

    pub fn get_moves(&mut self) -> Vec<Move> {
        let unchecked_moves = self.get_moves_unchecked();
        if !self.is_check() {
            unchecked_moves
        } else {
            unchecked_moves
                .into_iter()
                .filter(|m| self.is_legal(m))
                .collect()
        }
    }
}
