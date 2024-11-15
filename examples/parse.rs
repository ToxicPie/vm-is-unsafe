extern crate vm_is_unsafe;

use vm_is_unsafe::prelude::*;

fn parse_and_print(tokens: &[Token]) {
    match Sentence::try_from(tokens) {
        Ok(sentence) => println!("{:#?}", sentence),
        Err(error) => println!("error: {}", error),
    }
}

#[rustfmt::skip]
fn main() {
    use Token::*;
    parse_and_print(&[
        Register(0), Above, Const(0x1), Is, Not, Push,
    ]);
    parse_and_print(&[
        Register(0), Above, Const(0x1), Is, Not,
    ]);
    parse_and_print(&[
        Register(0), Not, Above, Const(0x1), And, Not, Register(1), Above, Const(0x2), Is, Add, And, Not, Sub,
    ]);
    parse_and_print(&[
        Register(0), And, Const(0x1), Read, Const(0x2), And, Register(1),
    ]);
}
