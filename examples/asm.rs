extern crate vm_is_unsafe;

use vm_is_unsafe::prelude::*;

fn main() {
    let mut emulator = Emulator::from_seed(123);

    emulator.insert_instructions(
        0,
        instrs! {
            Move [R 0], (0x1);
            Move [R 1], (0x2);
            Add [R 1], [R 0];
            Push [R 1];
        },
    );

    emulator.execute_all();
    println!("{:#?}", emulator);
}
