use crate::grammar::*;
use crate::vm::config;

#[derive(Clone, Copy, Debug)]
pub enum Operand {
    Register(u8),
    Const(u8),
}

#[derive(Clone, Copy, Debug)]
#[repr(u8)]
pub enum Opcode {
    Add,
    Sub,
    Or,
    Xor,
    Shr,
    Shl,
    Move,
    Swap,
    Load,
    Store,
    Push,
    Pull,
    Cmpa,
    Cmpb,
    Cmpe,
    Cmpna,
    Cmpnb,
    Cmpne,
    JumpUnless,
    BanIf,
    BanIfExcept,
    Win,
}

#[derive(Clone, Copy, Debug)]
pub struct Instruction {
    pub opcode: Opcode,
    pub dst: Operand,
    pub src: Operand,
}

macro_rules! operand {
    ([tmp]) => {
        TMP_REG
    };
    ([win]) => {
        WIN_REG
    };
    ([R $reg:expr]) => {
        Operand::Register($reg)
    };
    (($imm:expr)) => {
        Operand::Const($imm)
    };
    ({$operand:expr}) => {
        $operand
    };
}

#[macro_export]
macro_rules! instr {
    ($op:ident $x1:tt, $x2:tt) => {
        Instruction {
            opcode: Opcode::$op,
            dst: operand!($x1),
            src: operand!($x2),
        }
    };
    ($op:ident $x1:tt) => {
        Instruction {
            opcode: Opcode::$op,
            dst: operand!((0)),
            src: operand!($x1),
        }
    };
}

#[macro_export]
macro_rules! instrs {
    ($($op:ident $x1:tt $(, $x2:tt)?);* $(;)?) => {
        vec![
            $(instr!($op $x1 $(, $x2)?),)*
        ]
    };
}

impl From<&Noun> for Operand {
    fn from(value: &Noun) -> Self {
        match value {
            Noun::Register(reg) => Operand::Register(*reg),
            Noun::Const(num) => Operand::Const(*num),
        }
    }
}

const TMP_REG: Operand = Operand::Register(config::GENERAL_REGISTER_COUNT as u8);
const ALWAYS_CMP: Instruction = instr! { Cmpe (0x0), (0x0) };

fn make_conditional(subject: &Noun, modifier: &PrepositionalPhrase) -> Instruction {
    let PrepositionalPhrase {
        negation,
        preposition,
        object,
    } = modifier;
    let dst = subject.into();
    let src = object.into();
    match (negation, preposition) {
        (None, Preposition::Above) => instr! { Cmpa {dst}, {src} },
        (None, Preposition::Below) => instr! { Cmpb {dst}, {src} },
        (None, Preposition::Equal) => instr! { Cmpe {dst}, {src} },
        (Some(_), Preposition::Above) => instr! { Cmpna {dst}, {src} },
        (Some(_), Preposition::Below) => instr! { Cmpnb {dst}, {src} },
        (Some(_), Preposition::Equal) => instr! { Cmpne {dst}, {src} },
    }
}

fn add_condition(
    mut instructions: Vec<Instruction>,
    subject: &Noun,
    modifier: &Option<PrepositionalPhrase>,
) -> Vec<Instruction> {
    let skip_len = instructions.len() as u8;
    if let Some(modifier) = modifier {
        instructions.insert(0, instr! { JumpUnless (skip_len) });
        instructions.insert(1, make_conditional(subject, modifier));
    }
    instructions
}

fn add_bounds_check(mut instructions: Vec<Instruction>, addr: Operand) -> Vec<Instruction> {
    let skip_len = instructions.len() as u8;
    let bound = config::STACK_SIZE as u8;
    instructions.insert(0, instr! { JumpUnless (skip_len) });
    instructions.insert(1, instr! { Cmpb {addr}, (bound) });
    instructions
}

fn compile_is_yes_yes(
    subject: &Noun,
    modifier: &Option<PrepositionalPhrase>,
    adjective: &Adjective,
) -> Vec<Instruction> {
    let src = subject.into();
    let instructions = match adjective {
        Adjective::Move => instrs! { Move [tmp], {src} },
        Adjective::Back => instrs! { Move {src}, [tmp] },
        Adjective::Push => instrs! { Push {src} },
        Adjective::Pull => instrs! { Pull {src} },
        Adjective::Swap => instrs! { Swap [tmp], {src} },
        Adjective::Add => instrs! { Add [tmp], {src} },
        Adjective::Sub => instrs! { Sub [tmp], {src} },
        Adjective::Or => instrs! { Or [tmp], {src} },
        Adjective::Xor => instrs! { Xor [tmp], {src} },
        Adjective::Shr => instrs! { Shr [tmp], {src} },
        Adjective::Shl => instrs! { Shl [tmp], {src} },
        Adjective::Win => instrs! { Win {src} },
        Adjective::Defeat => instrs! { JumpUnless (0x1); Cmpne (0x0), (0x0); Win {src} },
    };
    add_condition(instructions, subject, modifier)
}

fn compile_is_yes_no(
    subject: &Noun,
    modifier: &Option<PrepositionalPhrase>,
    adjective: &Adjective,
) -> Vec<Instruction> {
    let operand = subject.into();
    let opcode = match adjective {
        Adjective::Move => Opcode::Move,
        Adjective::Push => Opcode::Push,
        Adjective::Pull => Opcode::Pull,
        Adjective::Swap => Opcode::Swap,
        Adjective::Add => Opcode::Add,
        Adjective::Sub => Opcode::Sub,
        Adjective::Or => Opcode::Or,
        Adjective::Xor => Opcode::Xor,
        Adjective::Shr => Opcode::Shr,
        Adjective::Shl => Opcode::Shl,
        Adjective::Back | Adjective::Win | Adjective::Defeat => return vec![],
    } as u8;
    let conditional = match modifier {
        Some(modifier) => make_conditional(subject, modifier),
        None => ALWAYS_CMP,
    };
    vec![instr! { BanIf {operand}, (opcode) }, conditional]
}

fn compile_is_no_yes(
    subject: &Noun,
    modifier: &Option<PrepositionalPhrase>,
    adjective: &Adjective,
) -> Vec<Instruction> {
    let operand = subject.into();
    let opcode = match adjective {
        Adjective::Move => Opcode::Move,
        Adjective::Push => Opcode::Push,
        Adjective::Pull => Opcode::Pull,
        Adjective::Swap => Opcode::Swap,
        Adjective::Add => Opcode::Add,
        Adjective::Sub => Opcode::Sub,
        Adjective::Or => Opcode::Or,
        Adjective::Xor => Opcode::Xor,
        Adjective::Shr => Opcode::Shr,
        Adjective::Shl => Opcode::Shl,
        Adjective::Back | Adjective::Win | Adjective::Defeat => return vec![],
    } as u8;
    let opcode = !opcode;
    let conditional = match modifier {
        Some(modifier) => make_conditional(subject, modifier),
        None => ALWAYS_CMP,
    };
    vec![instr! { BanIfExcept {operand}, (opcode) }, conditional]
}

fn compile_is_no_no(
    subject: &Noun,
    modifier: &Option<PrepositionalPhrase>,
    adjective: &Adjective,
) -> Vec<Instruction> {
    let operand = subject.into();
    let opcode = match adjective {
        Adjective::Move => Opcode::Move,
        Adjective::Push => Opcode::Push,
        Adjective::Pull => Opcode::Pull,
        Adjective::Swap => Opcode::Swap,
        Adjective::Add => Opcode::Add,
        Adjective::Sub => Opcode::Sub,
        Adjective::Or => Opcode::Or,
        Adjective::Xor => Opcode::Xor,
        Adjective::Shr => Opcode::Shr,
        Adjective::Shl => Opcode::Shl,
        Adjective::Back | Adjective::Win | Adjective::Defeat => return vec![],
    } as u8;
    let conditional = match modifier {
        Some(modifier) => make_conditional(subject, modifier),
        None => ALWAYS_CMP,
    };
    vec![instr! { BanIfExcept {operand}, (opcode) }, conditional]
}

fn compile_is(subject: &NounPhrase, complement: &AdjectivePhrase) -> Vec<Instruction> {
    match subject {
        NounPhrase {
            negation: None,
            subject,
            modifier,
        } => match complement {
            AdjectivePhrase {
                negation: None,
                adjective,
            } => compile_is_yes_yes(subject, modifier, adjective),
            AdjectivePhrase {
                negation: Some(_),
                adjective,
            } => compile_is_yes_no(subject, modifier, adjective),
        },
        NounPhrase {
            negation: Some(_),
            subject,
            modifier,
        } => match complement {
            AdjectivePhrase {
                negation: None,
                adjective,
            } => compile_is_no_yes(subject, modifier, adjective),
            AdjectivePhrase {
                negation: Some(_),
                adjective,
            } => compile_is_no_no(subject, modifier, adjective),
        },
    }
}

fn compile_become(subject: &NounPhrase, object: &Noun) -> Vec<Instruction> {
    let NounPhrase {
        negation: _,
        subject,
        modifier,
    } = subject;
    let dst = subject.into();
    let src = object.into();
    let instructions = instrs! { Move {dst}, {src} };
    add_condition(instructions, subject, modifier)
}

fn compile_read(subject: &NounPhrase, object: &Noun) -> Vec<Instruction> {
    let NounPhrase {
        negation: _,
        subject,
        modifier,
    } = subject;
    let dst = subject.into();
    let src = object.into();
    let instructions = instrs! { Load {dst}, {src} };
    add_condition(instructions, subject, modifier)
}

fn compile_write(subject: &NounPhrase, object: &Noun) -> Vec<Instruction> {
    let NounPhrase {
        negation: _,
        subject,
        modifier,
    } = subject;
    let src = subject.into();
    let dst = object.into();
    let instructions = instrs! { Store {dst}, {src} };
    add_condition(instructions, subject, modifier)
}

pub fn compile(sentence: &Sentence) -> Vec<Instruction> {
    let mut instructions = vec![];
    match sentence {
        Sentence::Copulative(CopulativeSentence {
            subjects,
            complements,
        }) => {
            for complement in complements.iter() {
                for subject in subjects.iter() {
                    instructions.extend(compile_is(subject, complement));
                }
            }
        }
        Sentence::Transitive(TransitiveSentence {
            subjects,
            verb,
            objects,
        }) => {
            let compile_fn = match verb {
                Verb::Become => compile_become,
                Verb::Read => compile_read,
                Verb::Write => compile_write,
            };
            instructions = subjects
                .iter()
                .zip(objects)
                .flat_map(|(subject, object)| compile_fn(subject, object))
                .collect();
            if matches!(verb, Verb::Read | Verb::Write) {
                for addr in objects {
                    instructions = add_bounds_check(instructions, addr.into());
                }
            }
        }
    }
    instructions
}
