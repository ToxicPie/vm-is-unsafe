#![feature(iter_intersperse)]

#[macro_use]
mod compiler;
mod grammar;
#[cfg(test)]
mod test_pwn;
mod vm;

use compiler::{compile, Instruction, Opcode, Operand};
use grammar::{Sentence, Token};
use vm::Emulator;

fn print_sentence(tokens: &[Token]) {
    println!(
        "{}",
        tokens
            .iter()
            .map(|x| x.to_string())
            .intersperse(" ".to_string())
            .collect::<String>()
    )
}

fn parse_and_print(tokens: &[Token]) {
    print_sentence(tokens);
    match Sentence::try_from(tokens) {
        Ok(sentence) => println!("{:#?}", sentence),
        Err(err) => println!("{}", err),
    }
}

fn parse_and_compile_and_print(tokens: &[Token]) {
    print_sentence(tokens);
    match Sentence::try_from(tokens) {
        Ok(sentence) => {
            let instrs = compiler::compile(&sentence);
            println!("{:#?}", instrs);
        }
        Err(err) => println!("{}", err),
    }
}

#[rustfmt::skip]
fn main() {
    // test parser and compiler from tokens
    use Token::*;
    parse_and_print(&[
        Not, Not, Const(0x69), Above, Register(1), And, Not, Not, Not, Const(0x87), Is, Push, And, Not
    ]);
    parse_and_print(&[
        Not, Not, Const(0x69), Above, Register(1), And, Not, Not, Not, Const(0x87), Is, Push, And, Not, Move,
    ]);
    parse_and_compile_and_print(&[
        Not, Not, Const(0x69), Above, Register(1), And, Not, Not, Not, Const(0x87), Is, Push, And, Not, Move,
    ]);

    let mut emulator = Emulator::from_seed(1069);
    // add compiled sentences
    emulator.insert_instructions(0, compile(&Sentence::try_from(&[
        Register(1), Become, Const(1),
    ][..]).unwrap()));
    emulator.insert_instructions(1, compile(&Sentence::try_from(&[
        Register(1), Is, Not, Add,
    ][..]).unwrap()));
    emulator.insert_instructions(2, compile(&Sentence::try_from(&[
        Const(42), Is, Win,
    ][..]).unwrap()));
    // directly add assembly
    emulator.insert_instructions(3, instrs! {
        Move [R 2], (2)
    });
    emulator.insert_instructions(4, instrs! {
        Add [R 0], [R 1]
    });
    emulator.insert_instructions(5, instrs! {
        Push [R 1]
    });

    // execute
    println!("score gained: {}", emulator.execute_all());

    println!("registers: {:#?}", emulator.memory.registers);
    println!("stack: {:?}", emulator.memory.stack);
}
