#[macro_use]
mod compiler;
mod grammar;
#[cfg(test)]
mod test_pwn;
mod vm;
mod utils;

use compiler::{compile, Instruction, Opcode, Operand};
use grammar::{Sentence, Token};
use rand::SeedableRng;
use vm::Emulator;
use rand_chacha::ChaCha12Rng;

fn parse_and_print(tokens: &[Token]) {
    match Sentence::try_from(tokens) {
        Ok(sentence) => println!("{:#?}", sentence),
        Err(err) => println!("{}", err),
    }
}

fn parse_and_compile_and_print(tokens: &[Token]) {
    match Sentence::try_from(tokens) {
        Ok(sentence) => {
            let instrs = compiler::compile(&sentence);
            println!("{:#?}", instrs);
        }
        Err(err) => println!("{}", err),
    }
}

#[rustfmt::skip]
fn vm_demo() {
    use Token::*;
    // parse and compile from tokens
    parse_and_print(&[
        Not, Not, Const(0x69), Above, Register(1), And, Not, Not, Not, Const(0x87), Is, Push, And, Not,
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
        Const(0), Become, Const(1),
    ][..]).unwrap()));
    emulator.insert_instructions(1, compile(&Sentence::try_from(&[
        Register(1), Is, Add, And, Back,
    ][..]).unwrap()));
    // directly add assembly
    emulator.insert_instructions(2, instrs! {
        Move [R 2], (2);
        Add [R 0], [R 1];
        Push [R 1];
    });

    // execute
    println!("score gained: {}", emulator.execute_all());

    // debug
    println!("registers: {:#?}", emulator.memory.registers);
    println!("stack: {:?}", emulator.memory.stack);
}

fn random_demo() {
    let mut rng = ChaCha12Rng::seed_from_u64(420);
    for _ in 0..3 {
        println!("randomly generated 2x2 block: {:?}", utils::generate_2x2_block(&mut rng));
    }
}

fn main() {
    vm_demo();
    random_demo();
}
