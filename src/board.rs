use Color::*;
use Move::*;
use PieceType::*;

pub const STARTPOS: &str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";

#[derive(PartialEq, Debug, Eq, Hash, Clone, Copy)]
pub enum Color {
    White,
    Black,
}

impl std::ops::Not for Color {
    type Output = Self;
    fn not(self) -> Self::Output {
        match self {
            White => Black,
            Black => White,
        }
    }
}

#[derive(PartialEq, Debug, Eq, Hash, Clone, Copy)]
pub enum PieceType {
    Pawn,
    Knight,
    Bishop,
    Rook,
    Sook,
    Queen,
    King,
}

#[derive(PartialEq, Debug, Eq, Hash, Clone)]
pub enum Move {
    Slide {
        from_index: i8,
        to_index: i8,
        piece_moved: Piece,
        piece_there: Option<Piece>,
        passant_before: Option<i8>,
    },
    Promotion {
        from_index: i8,
        to_index: i8,
        piece_moved: Piece,
        piece_there: Option<Piece>,
        promotion: Piece,
        passant_before: Option<i8>,
    },
    Castle {
        sook_index: i8,
        passant_before: Option<i8>,
    },
    KingMOVE {
        from_index: i8,
        to_index: i8,
        piece_moved: Piece,
        piece_there: Option<Piece>,
        castle_left: bool,
        castle_right: bool,
        passant_before: Option<i8>,
    },
    PawnPush {
        from_index: i8,
        passant_before: Option<i8>,
    },
    Passant {
        from_index: i8,
        to_index: i8,
    },
}

impl std::fmt::Display for Move {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PawnPush {
                from_index,
                passant_before: _,
            } => {
                let to_index = if *from_index < 32 {
                    *from_index + 16
                } else {
                    *from_index - 16
                };
                write!(f, "{}{}", square_name(*from_index), square_name(to_index))
            }
            Passant {
                from_index,
                to_index,
            }
            | KingMOVE {
                from_index,
                to_index,
                piece_moved: _,
                piece_there: _,
                castle_left: _,
                castle_right: _,
                passant_before: _,
            }
            | Slide {
                from_index,
                to_index,
                piece_moved: _,
                piece_there: _,
                passant_before: _,
            } => {
                write!(f, "{}{}", square_name(*from_index), square_name(*to_index))
            }

            Promotion {
                from_index,
                to_index,
                piece_moved,
                piece_there: _,
                promotion,
                passant_before: _,
            } => {
                write!(
                    f,
                    "{}{}{}",
                    square_name(*from_index),
                    square_name(*to_index),
                    if piece_moved.piece_type == Pawn {
                        match promotion.piece_type {
                            Knight => "n",
                            Bishop => "b",
                            Rook => "r",
                            Queen => "q",
                            _ => "illegal_promo",
                        }
                    } else {
                        ""
                    }
                )
            }
            Castle {
                sook_index,
                passant_before: _,
            } => write!(
                f,
                "{}",
                match sook_index {
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
        White => "W",
        Black => "B",
    }) + match piece.piece_type {
        Pawn => "P",
        Knight => "N",
        Bishop => "B",
        Rook => "R",
        Sook => "S",
        Queen => "Q",
        King => "K",
    }
}

pub fn square_name(s: i8) -> String {
    let byte_vec = vec![(97 + (s & 0b111)) as u8, (49 + (s >> 3)) as u8];
    String::from_utf8(byte_vec).unwrap()
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
            turn: White,
        }
    }
    pub fn from_fen(fen: String) -> Self {
        const INIT: Option<Piece> = None;
        let mut pieces = [INIT; 64];
        let mut passant_square = None;
        let mut turn = White;
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
                    if c.is_uppercase() { White } else { Black },
                    match c.to_ascii_uppercase() {
                        'P' => Pawn,
                        'N' => Knight,
                        'B' => Bishop,
                        'R' => Rook,
                        'Q' => Queen,
                        'K' => King,
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
                    if piece.color == White && piece.piece_type == King {
                        white_king_square = squareindex as i8;
                    }
                    if piece.color == Black && piece.piece_type == King {
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
                Some('K') => pieces[7] = Some(Piece::new(White, Sook)),
                Some('Q') => pieces[0] = Some(Piece::new(White, Sook)),
                Some('k') => pieces[63] = Some(Piece::new(Black, Sook)),
                Some('q') => pieces[56] = Some(Piece::new(Black, Sook)),
                None => (),
                _ => (),
            }
            let diff: i8 = (c.unwrap() as i8) - ('a' as i8);
            if diff > 0 && diff < 8 {
                let next_char: Option<char> = fen.chars().nth(charindex + 1);
                if c == Some('b') && next_char == Some(' ') {
                    turn = Black;
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
                White => {
                    "White"
                }
                Black => {
                    "Black"
                }
            }
        );
    }

    pub fn is_check(&self) -> bool {
        !self.is_safe(
            self.turn,
            match self.turn {
                White => self.white_king_square,
                Black => self.black_king_square,
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
        while (0..64).contains(&frontier) && (frontier >> 3) - (prevtier >> 3) == ydiff {
            match self.pieces[frontier as usize] {
                None => {}
                _ => return Some(frontier),
            }
            prevtier = frontier;
            frontier += ray;
        }
        None
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
                    && (0..64).contains(&square)
            })
            .filter_map(|ray| (self.pieces[(index + ray) as usize]).as_ref());
        for threat in threats {
            if threat.color != color && threat.piece_type == piece_type {
                return false;
            }
        }

        true
    }

    fn is_safe(&self, color: Color, index: i8) -> bool {
        match color {
            White => {
                if !self.safe_from_type(White, [7, 9].into_iter(), index, Pawn) {
                    return false;
                }
            }
            Black => {
                if !self.safe_from_type(Black, [-7, -9].into_iter(), index, Pawn) {
                    return false;
                }
            }
        }
        if !self.safe_from_type(color, [-9, -8, -7, -1, 1, 7, 8, 9].into_iter(), index, King) {
            return false;
        }
        //println!("{} passed the safety check", index);
        if !self.safe_from_type(
            color,
            [-17, -15, -10, -6, 6, 10, 15, 17].into_iter(),
            index,
            Knight,
        ) {
            return false;
        }
        for ray in [-8, -1, 1, 8] {
            match self.blocker(index, ray) {
                None => {}
                Some(threatsquare) => {
                    let attacker = (self.pieces[threatsquare as usize]).as_ref().unwrap();
                    if attacker.color != color
                        && (attacker.piece_type == Rook
                            || attacker.piece_type == Sook
                            || attacker.piece_type == Queen)
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
                        && (attacker.piece_type == Bishop || attacker.piece_type == Queen)
                    {
                        return false;
                    }
                }
            }
        }
        true
    }

    pub fn take_move(&mut self, m: &Move) {
        self.turn = !self.turn;
        match m {
            Slide {
                from_index,
                to_index,
                piece_moved,
                piece_there: _,
                passant_before: _,
            } => {
                self.pieces[*from_index as usize] = None;
                self.pieces[*to_index as usize] = Some(*piece_moved);
                self.passant_square = None;
            }
            Promotion {
                from_index,
                to_index,
                piece_moved: _,
                piece_there: _,
                promotion,
                passant_before: _,
            } => {
                self.pieces[*from_index as usize] = None;
                self.pieces[*to_index as usize] = Some(*promotion);
                self.passant_square = None
            }
            Castle {
                sook_index,
                passant_before: _,
            } => {
                self.pieces[*sook_index as usize] = None;
                let color = if *sook_index < 32 { White } else { Black };
                if (sook_index & 0b111) == 0 {
                    //castle left
                    self.pieces[(*sook_index) as usize] = None;
                    self.pieces[(*sook_index + 2) as usize] = Some(Piece::new(color, King));
                    self.pieces[(*sook_index + 3) as usize] = Some(Piece::new(color, Rook));
                    self.pieces[(*sook_index + 4) as usize] = None;
                    match color {
                        White => self.white_king_square = sook_index + 2,
                        Black => self.black_king_square = sook_index + 2,
                    }
                } else {
                    //castle right
                    self.pieces[(*sook_index) as usize] = None;
                    self.pieces[(*sook_index - 1) as usize] = Some(Piece::new(color, King));
                    self.pieces[(*sook_index - 2) as usize] = Some(Piece::new(color, Rook));
                    self.pieces[(*sook_index - 3) as usize] = None;
                    match color {
                        White => self.white_king_square = sook_index - 1,
                        Black => self.black_king_square = sook_index - 1,
                    }
                }
                self.passant_square = None;
            }
            KingMOVE {
                from_index,
                to_index,
                piece_moved,
                piece_there: _,
                castle_left,
                castle_right,
                passant_before: _,
            } => {
                self.pieces[*from_index as usize] = None;
                self.pieces[*to_index as usize] = Some(*piece_moved);
                if *castle_left {
                    self.pieces[((*from_index >> 3) << 3) as usize] =
                        Some(Piece::new(piece_moved.color, Rook));
                }
                if *castle_right {
                    self.pieces[(((*from_index >> 3) << 3) + 7) as usize] =
                        Some(Piece::new(piece_moved.color, Rook));
                }

                match piece_moved.color {
                    White => self.white_king_square = *to_index,
                    Black => self.black_king_square = *to_index,
                }

                self.passant_square = None;
            }
            PawnPush {
                from_index,
                passant_before: _,
            } => {
                self.pieces[*from_index as usize] = None;
                if *from_index < 32 {
                    //White pawn being pushed
                    self.pieces[(from_index + 16) as usize] = Some(Piece::new(White, Pawn));
                    self.passant_square = Some(from_index + 8)
                } else {
                    //Black pawn being pushed
                    self.pieces[(from_index - 16) as usize] = Some(Piece::new(Black, Pawn));
                    self.passant_square = Some(from_index - 8)
                }
            }
            Passant {
                from_index,
                to_index: _,
            } => {
                self.pieces[*from_index as usize] = None;
                if *from_index < 32 {
                    //White is being captured
                    self.pieces[self.passant_square.unwrap() as usize] =
                        Some(Piece::new(Black, Pawn));
                    self.pieces[(self.passant_square.unwrap() + 8) as usize] = None;
                } else {
                    //Black is being captured
                    self.pieces[self.passant_square.unwrap() as usize] =
                        Some(Piece::new(White, Pawn));
                    self.pieces[(self.passant_square.unwrap() - 8) as usize] = None;
                }
                self.passant_square = None
            }
        }
    }

    pub fn take_move_back(&mut self, m: &Move) {
        self.turn = !self.turn;
        match m {
            Slide {
                from_index,
                to_index,
                piece_moved,
                piece_there,
                passant_before,
            } => {
                self.pieces[*from_index as usize] = Some(*piece_moved);
                self.pieces[*to_index as usize] = *piece_there;
                self.passant_square = *passant_before;
            }
            Promotion {
                from_index,
                to_index,
                piece_moved,
                piece_there,
                promotion: _,
                passant_before,
            } => {
                self.pieces[*from_index as usize] = Some(*piece_moved);
                self.pieces[*to_index as usize] = *piece_there;
                self.passant_square = *passant_before;
            }
            Castle {
                sook_index,
                passant_before,
            } => {
                let color = if *sook_index < 32 {
                    self.white_king_square = ((sook_index >> 3) << 3) + 4;
                    White
                } else {
                    self.black_king_square = ((sook_index >> 3) << 3) + 4;
                    Black
                };
                if sook_index & 0b111 == 0 {
                    self.pieces[*sook_index as usize] = Some(Piece::new(color, Sook));
                    self.pieces[(*sook_index + 2) as usize] = None;
                    self.pieces[(*sook_index + 3) as usize] = None;
                    self.pieces[(*sook_index + 4) as usize] = Some(Piece::new(color, King))
                } else {
                    self.pieces[*sook_index as usize] = Some(Piece::new(color, Sook));
                    self.pieces[(*sook_index - 1) as usize] = None;
                    self.pieces[(*sook_index - 2) as usize] = None;
                    self.pieces[(*sook_index - 3) as usize] = Some(Piece::new(color, King))
                }
                self.passant_square = *passant_before;
            } //rook_index, passant_before
            KingMOVE {
                from_index,
                to_index,
                piece_moved,
                piece_there,
                castle_left,
                castle_right,
                passant_before,
            } => {
                match piece_moved.color {
                    White => self.white_king_square = *from_index,
                    Black => self.black_king_square = *from_index,
                }
                self.pieces[*from_index as usize] = Some(*piece_moved);
                self.pieces[*to_index as usize] = *piece_there;
                if *castle_left {
                    self.pieces[((*from_index >> 3) << 3) as usize] =
                        Some(Piece::new(piece_moved.color, Sook))
                }
                if *castle_right {
                    self.pieces[(((*from_index >> 3) << 3) + 7) as usize] =
                        Some(Piece::new(piece_moved.color, Sook))
                }

                self.passant_square = *passant_before;
            } //to, from, piece_moved, piece_there, passant_before
            PawnPush {
                from_index,
                passant_before,
            } => {
                if *from_index < 32 {
                    self.pieces[(from_index + 16) as usize] = None;
                    self.pieces[(*from_index) as usize] = Some(Piece::new(White, Pawn))
                } else {
                    self.pieces[(from_index - 16) as usize] = None;
                    self.pieces[(*from_index) as usize] = Some(Piece::new(Black, Pawn))
                }
                self.passant_square = *passant_before;
            }
            Passant {
                from_index,
                to_index,
            } => {
                self.passant_square = Some(*to_index);
                self.pieces[*to_index as usize] = None;
                if *to_index < 32 {
                    //white captured
                    self.pieces[*from_index as usize] = Some(Piece::new(Black, Pawn));
                    self.pieces[(*to_index + 8) as usize] = Some(Piece::new(White, Pawn));
                } else {
                    //black captured
                    self.pieces[*from_index as usize] = Some(Piece::new(White, Pawn));
                    self.pieces[(*to_index - 8) as usize] = Some(Piece::new(Black, Pawn));
                };
            }
        }
    }

    fn is_legal(&mut self, m: &Move) -> bool {
        self.take_move(m);
        let ans = match self.turn {
            White => self.is_safe(Black, self.black_king_square),
            Black => self.is_safe(White, self.white_king_square),
        };
        self.take_move_back(m);
        ans
    }
    fn get_pins(&self) -> [Pin; 8] {
        let square = match self.turn {
            White => self.white_king_square,
            Black => self.black_king_square,
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
                                    && (threaterpiece.piece_type == Queen
                                        || threaterpiece.piece_type == Bishop)
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
                                    && (threaterpiece.piece_type == Queen
                                        || threaterpiece.piece_type == Sook
                                        || threaterpiece.piece_type == Rook)
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
        while (0..64).contains(&frontier)
            && (frontier >> 3) - (prevtier >> 3) == ydiff
            && (match self.pieces[frontier as usize] {
                None => true,
                Some(p) => p.color != piece.color,
            })
        {
            let to_piece = self.pieces[frontier as usize];
            moves.push(Slide {
                from_index: i,
                to_index: frontier,
                piece_moved: piece,
                piece_there: to_piece,
                passant_before: self.passant_square,
            });

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
        while (0..64).contains(&frontier)
            && (frontier >> 3) - (prevtier >> 3) == ydiff
            && (match self.pieces[frontier as usize] {
                None => true,
                Some(p) => p.color != piece.color,
            })
        {
            let to_piece = self.pieces[frontier as usize];
            moves.push(Promotion {
                from_index: i,
                to_index: frontier,
                piece_moved: piece,
                piece_there: to_piece,
                promotion: Piece::new(piece.color, Rook),
                passant_before: self.passant_square,
            });

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
                if p.color == color && p.piece_type == Pawn {
                    let m = Passant {
                        from_index: passant_square + ray,
                        to_index: passant_square,
                    };
                    if self.is_legal(&m) {
                        moves.push(m);
                    }
                }
            }
        }
    }

    fn add_pawn_vertical_moves(&self, moves: &mut Vec<Move>, index: i8, piece: Piece) {
        match piece.color {
            White => match self.pieces[(index + 8) as usize] {
                None => {
                    if index >> 3 == 6 {
                        for promo in [Queen, Rook, Bishop, Knight] {
                            moves.push(Promotion {
                                from_index: index,
                                to_index: index + 8,
                                piece_moved: piece,
                                piece_there: None,
                                promotion: Piece::new(piece.color, promo),
                                passant_before: self.passant_square,
                            });
                        }
                    } else {
                        if index >> 3 == 1 {
                            match self.pieces[(index + 16) as usize] {
                                None => moves.push(PawnPush {
                                    from_index: index,
                                    passant_before: self.passant_square,
                                }),
                                Some(_) => {}
                            }
                        }
                        moves.push(Slide {
                            from_index: index,
                            to_index: index + 8,
                            piece_moved: piece,
                            piece_there: None,
                            passant_before: self.passant_square,
                        });
                    }
                }
                Some(_) => {}
            },
            Black => match self.pieces[(index - 8) as usize] {
                None => {
                    if index >> 3 == 1 {
                        for promo in [Queen, Rook, Bishop, Knight] {
                            moves.push(Promotion {
                                from_index: index,
                                to_index: index - 8,
                                piece_moved: piece,
                                piece_there: None,
                                promotion: Piece::new(piece.color, promo),
                                passant_before: self.passant_square,
                            });
                        }
                    } else {
                        if index >> 3 == 6 {
                            match self.pieces[(index - 16) as usize] {
                                None => {
                                    moves.push(PawnPush {
                                        from_index: index,
                                        passant_before: self.passant_square,
                                    });
                                }
                                Some(_) => {}
                            }
                        }
                        moves.push(Slide {
                            from_index: index,
                            to_index: index - 8,
                            piece_moved: piece,
                            piece_there: None,
                            passant_before: self.passant_square,
                        });
                    }
                }
                Some(_) => {}
            },
        }
    }

    fn consider_pawn_capture(&self, moves: &mut Vec<Move>, index: i8, piece: Piece, ray: i8) {
        let ray = match piece.color {
            White => ray,
            Black => -ray,
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
                    if piece.color == White && index >> 3 == 6
                        || piece.color == Black && index >> 3 == 1
                    {
                        let promos = [Queen, Rook, Bishop, Knight];
                        for promo in promos {
                            moves.push(Promotion {
                                from_index: index,
                                to_index: index + ray,
                                piece_moved: piece,
                                piece_there: potential,
                                promotion: Piece::new(piece.color, promo),
                                passant_before: self.passant_square,
                            })
                        }
                    } else {
                        moves.push(Slide {
                            from_index: index,
                            to_index: index + ray,
                            piece_moved: piece,
                            piece_there: potential,
                            passant_before: self.passant_square,
                        })
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
                White => {
                    if passant_square > 40 {
                        self.consider_passant(&mut moves, -9, White, passant_square)
                    }
                    if passant_square < 47 {
                        self.consider_passant(&mut moves, -7, White, passant_square)
                    }
                }
                Black => {
                    if passant_square > 16 {
                        self.consider_passant(&mut moves, 7, Black, passant_square)
                    }
                    if passant_square < 23 {
                        self.consider_passant(&mut moves, 9, Black, passant_square)
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
                            Pawn => {
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

                            Knight => {
                                if !is_pinned {
                                    //a pinned knight can never move
                                    let rays = [-17, -15, -10, -6, 6, 10, 15, 17];
                                    for ray in rays {
                                        let knightsmove = i + ray;
                                        if (0..64).contains(&knightsmove)
                                            && (knightsmove >> 3) - (i >> 3)
                                                == Self::ydiff_of_ray(ray)
                                            && (match self.pieces[knightsmove as usize] {
                                                None => true,
                                                Some(p) => p.color != piece.color,
                                            })
                                        {
                                            moves.push(Slide {
                                                from_index: i,
                                                to_index: knightsmove,
                                                piece_moved: piece,
                                                piece_there: self.pieces[knightsmove as usize],
                                                passant_before: self.passant_square,
                                            });
                                        }
                                    }
                                }
                            }

                            Bishop => {
                                if is_pinned {
                                    let ray = blockers[bi].ray;
                                    match ray {
                                        -9 | -7 | 7 | 9 => {
                                            self.add_slides(&mut moves, -ray, i, piece);
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

                            Rook => {
                                if is_pinned {
                                    let ray = blockers[bi].ray;
                                    match ray {
                                        -8 | -1 | 1 | 8 => {
                                            self.add_slides(&mut moves, -ray, i, piece);
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

                            Sook => {
                                if (i & (0b111)) == 0 {
                                    if match self.pieces[(i + 4) as usize] {
                                        None => false,
                                        Some(p) => p.color == piece.color && p.piece_type == King,
                                    } && self.pieces[(i + 1) as usize].is_none()
                                        && self.pieces[(i + 2) as usize].is_none()
                                        && self.pieces[(i + 3) as usize].is_none()
                                        && self.is_safe(piece.color, i + 2)
                                        && self.is_safe(piece.color, i + 3)
                                        && self.is_safe(piece.color, i + 4)
                                    {
                                        moves.push(Castle {
                                            sook_index: i,
                                            passant_before: self.passant_square,
                                        })
                                    }
                                } else if match self.pieces[(i - 3) as usize] {
                                    None => false,
                                    Some(p) => p.color == piece.color && p.piece_type == King,
                                } && self.pieces[(i - 1) as usize].is_none()
                                    && self.pieces[(i - 2) as usize].is_none()
                                    && self.is_safe(piece.color, i - 1)
                                    && self.is_safe(piece.color, i - 2)
                                    && self.is_safe(piece.color, i - 3)
                                {
                                    moves.push(Castle {
                                        sook_index: i,
                                        passant_before: self.passant_square,
                                    })
                                }

                                for ray in [-8, -1, 1, 8] {
                                    self.add_sooks(&mut moves, ray, i, piece);
                                }
                            }

                            Queen => {
                                if is_pinned {
                                    //a pinned queen is always able to move along the ray
                                    let ray = blockers[bi].ray;
                                    self.add_slides(&mut moves, -ray, i, piece);
                                    self.add_slides(&mut moves, ray, i, piece);
                                } else {
                                    for ray in [-9, -7, 7, 9, -8, -1, 1, 8] {
                                        self.add_slides(&mut moves, ray, i, piece);
                                    }
                                }
                            }

                            King => {
                                let rays = [-9, -8, -7, -1, 1, 7, 8, 9];
                                for ray in rays {
                                    let kingsmove = i + ray;
                                    if (0..64).contains(&kingsmove)
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
                                                    p.piece_type == Sook && p.color == piece.color
                                                }
                                            };
                                        let can_castle_right =
                                            match self.pieces[(((i >> 3) << 3) + 7) as usize] {
                                                None => false,
                                                Some(p) => {
                                                    p.piece_type == Sook && p.color == piece.color
                                                }
                                            };
                                        moves.push(KingMOVE {
                                            from_index: i,
                                            to_index: kingsmove,
                                            piece_moved: piece,
                                            piece_there: self.pieces[kingsmove as usize],
                                            castle_left: can_castle_left,
                                            castle_right: can_castle_right,
                                            passant_before: self.passant_square,
                                        });
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
