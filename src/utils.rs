use crate::grammar::Token;

use rand::distributions::{Distribution, Standard};
use rand::Rng;

impl Distribution<Token> for Standard {
    fn sample<R: Rng + ?Sized>(&self, rng: &mut R) -> Token {
        let num = rng.gen_range(0..2500);
        fn gen_verb(num: u32) -> Token {
            match num {
                0..200 => Token::Is,
                200..260 => Token::Read,
                260..320 => Token::Write,
                320..400 => Token::Become,
                _ => unreachable!(),
            }
        }
        fn gen_reg(num: u32) -> Token {
            match num {
                0..100 => Token::Register(0),
                100..200 => Token::Register(1),
                200..300 => Token::Register(2),
                300..400 => Token::Register(3),
                400..500 => Token::Register(4),
                500..600 => Token::Register(5),
                _ => unreachable!(),
            }
        }
        fn gen_const(num: u32) -> Token {
            match num {
                0..256 => Token::Const(num as u8),
                256..266 => Token::Const(255),
                266..276 => Token::Const(1),
                276..300 => Token::Const(0),
                _ => unreachable!(),
            }
        }
        fn gen_adjective(num: u32) -> Token {
            match num {
                0..50 => Token::Move,
                50..125 => Token::Back,
                125..150 => Token::Push,
                150..175 => Token::Pull,
                175..190 => Token::Swap,
                190..230 => Token::Add,
                230..250 => Token::Sub,
                250..260 => Token::Or,
                260..280 => Token::Xor,
                280..290 => Token::Shr,
                290..300 => Token::Shl,
                // OOPS
                // 300..400 => Token::Win,
                300..400 => Token::Defeat,
                _ => unreachable!(),
            }
        }
        fn gen_other(num: u32) -> Token {
            match num {
                0..300 => Token::Not,
                300..600 => Token::And,
                600..700 => Token::Below,
                700..800 => Token::Above,
                _ => unreachable!(),
            }
        }
        match num {
            0..400 => gen_verb(num),
            400..1000 => gen_reg(num - 400),
            1000..1300 => gen_const(num - 1000),
            1300..1700 => gen_adjective(num - 1300),
            1700..2500 => gen_other(num - 1700),
            _ => unreachable!(),
        }
    }
}

pub fn generate_2x2_block(rng: &mut impl Rng) -> [[Option<Token>; 2]; 2] {
    // 4  4  1  1
    // ox oo oo xo
    // xo xx ox oo
    // 4  4  1  1
    // xo ox oo ox
    // ox ox xo oo
    match rng.gen_range(0..20) {
        0..4 => [[Some(rng.gen()), None], [None, Some(rng.gen())]],
        4..8 => [[None, Some(rng.gen())], [Some(rng.gen()), None]],
        8..12 => [[Some(rng.gen()), Some(rng.gen())], [None, None]],
        12..16 => [[Some(rng.gen()), None], [Some(rng.gen()), None]],
        16 => [[Some(rng.gen()), Some(rng.gen())], [Some(rng.gen()), None]],
        17 => [[Some(rng.gen()), Some(rng.gen())], [None, Some(rng.gen())]],
        18 => [[None, Some(rng.gen())], [Some(rng.gen()), Some(rng.gen())]],
        19 => [[Some(rng.gen()), None], [Some(rng.gen()), Some(rng.gen())]],
        _ => unreachable!(),
    }
}
