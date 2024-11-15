pub mod compiler;
pub mod grammar;
pub mod vm;

pub mod prelude {
    pub use crate::compiler::{compile, Instruction};
    pub use crate::grammar::{Sentence, Token};
    pub use crate::vm::Emulator;
    pub use crate::{instr, instrs};
}
