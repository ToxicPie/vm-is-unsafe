use crate::compiler::{Instruction, Opcode, Operand};

use rand::{Rng, SeedableRng};
use rand_chacha::ChaCha12Rng;

pub mod config {
    pub const GENERAL_REGISTER_COUNT: usize = 6;
    pub const TOTAL_REGISTER_COUNT: usize = 8;
    pub const STACK_SIZE: usize = 32;
    pub const INSTRUCTION_CACHE_SIZE: usize = 8;
    pub const OPCODE_COUNT: usize = 22;
}

#[repr(C)]
#[derive(Default, Debug)]
pub struct Registers {
    pub reg_r0: u8,
    pub reg_r1: u8,
    pub reg_r2: u8,
    pub reg_r3: u8,
    pub reg_r4: u8,
    pub reg_r5: u8,
    pub reg_tmp: u8,
    pub reg_win: u8,
}

#[repr(C)]
#[derive(Debug)]
pub struct Memory {
    pub stack: [u8; config::STACK_SIZE],
    pub registers: Registers,
    // NO BREAKING FOURTH WALL
    _padding: [u8; 256],
}

impl Default for Memory {
    fn default() -> Self {
        Self {
            stack: [0; 32],
            registers: Default::default(),
            _padding: [0; 256],
        }
    }
}

#[derive(Debug)]
pub struct Emulator {
    pub memory: Memory,
    instruction_cache: [Vec<Instruction>; config::INSTRUCTION_CACHE_SIZE],
    rng: ChaCha12Rng,
}

impl Emulator {
    fn read_stack(&self, index: u8) -> u8 {
        unsafe { *(&self.memory.stack as *const _ as *const u8).add(index as usize) }
    }
    fn write_stack(&mut self, index: u8, data: u8) {
        unsafe { *(&mut self.memory.stack as *mut _ as *mut u8).add(index as usize) = data }
    }
    fn read_register(&self, index: u8) -> u8 {
        unsafe { *(&self.memory.registers as *const _ as *const u8).add(index as usize) }
    }
    fn write_register(&mut self, index: u8, data: u8) {
        unsafe { *(&mut self.memory.registers as *mut _ as *mut u8).add(index as usize) = data }
    }
    fn refresh_win_value(&mut self) {
        self.memory.registers.reg_win = self.rng.gen();
    }
    fn eval_operand(&self, operand: Operand) -> u8 {
        match operand {
            Operand::Register(reg) => self.read_register(reg),
            Operand::Const(num) => num,
        }
    }
    fn eval_condition(&self, condition: Instruction) -> bool {
        let Instruction {
            opcode,
            dst,
            src,
        } = condition;
        let val1 = self.eval_operand(dst);
        let val2 = self.eval_operand(src);
        match opcode {
            Opcode::Cmpa => val1 > val2,
            Opcode::Cmpb => val1 < val2,
            Opcode::Cmpe => val1 == val2,
            Opcode::Cmpna => val1 <= val2,
            Opcode::Cmpnb => val1 >= val2,
            Opcode::Cmpne => val1 != val2,
            _ => unreachable!(),
        }
    }
    fn execute_const_or_reg<F>(&mut self, dst: Operand, f: F)
    where
        F: Fn(u8) -> u8,
    {
        let dst_val = self.eval_operand(dst);
        match dst {
            Operand::Register(reg) => self.write_register(reg, f(dst_val)),
            Operand::Const(num) => {
                for reg in 0..config::GENERAL_REGISTER_COUNT {
                    let reg = reg as u8;
                    if self.read_register(reg) == num {
                        self.write_register(reg, f(dst_val));
                    }
                }
            }
        }
    }
    pub fn execute_all(&mut self) -> i64 {
        let mut ban_conditions_nums = vec![vec![vec![]; config::OPCODE_COUNT]; 256];
        let mut ban_conditions_regs =
            vec![vec![vec![]; config::OPCODE_COUNT]; config::TOTAL_REGISTER_COUNT];
        let mut score = 0i64;

        for instructions in self.instruction_cache.clone() {
            let mut instruction_stream = instructions.into_iter();
            while let Some(Instruction { opcode, dst, src }) = instruction_stream.next() {
                let ban_conditions = match src {
                    Operand::Register(reg) => {
                        &ban_conditions_regs[reg as usize][opcode as usize]
                    }
                    Operand::Const(num) => {
                        &ban_conditions_nums[num as usize][opcode as usize]
                    }
                };
                let banned = ban_conditions
                    .iter()
                    .any(|condition| self.eval_condition(*condition));

                match opcode {
                    Opcode::Add if !banned => {
                        let src = self.eval_operand(src);
                        self.execute_const_or_reg(dst, |dst| dst.wrapping_add(src));
                    }
                    Opcode::Sub if !banned => {
                        let src = self.eval_operand(src);
                        self.execute_const_or_reg(dst, |dst| dst.wrapping_add(src));
                    }
                    Opcode::Or if !banned => {
                        let src = self.eval_operand(src);
                        self.execute_const_or_reg(dst, |dst| dst | src);
                    }
                    Opcode::Xor if !banned => {
                        let src = self.eval_operand(src);
                        self.execute_const_or_reg(dst, |dst| dst ^ src);
                    }
                    Opcode::Shr if !banned => {
                        let src = self.eval_operand(src);
                        self.execute_const_or_reg(dst, |dst| dst.wrapping_shr(src as u32));
                    }
                    Opcode::Shl if !banned => {
                        let src = self.eval_operand(src);
                        self.execute_const_or_reg(dst, |dst| dst.wrapping_shl(src as u32));
                    }
                    Opcode::Move if !banned => {
                        let src = self.eval_operand(src);
                        self.execute_const_or_reg(dst, |_dst| src);
                    }
                    Opcode::Push if !banned  => {
                        for idx in (1..config::STACK_SIZE as u8).rev() {
                            self.write_stack(idx, self.read_stack(idx - 1));
                        }
                        self.write_stack(0, self.eval_operand(src));
                    }
                    Opcode::Pull if !banned  => {
                        let dst = self.read_stack(0);
                        self.execute_const_or_reg(src, |_src| dst);
                        for idx in 1..config::STACK_SIZE as u8 {
                            self.write_stack(idx - 1, self.read_stack(idx));
                        }
                        self.write_stack(config::STACK_SIZE as u8 - 1, 0);
                    }
                    Opcode::Swap if !banned  => {
                        let val1 = self.eval_operand(dst);
                        let val2 = self.eval_operand(src);
                        self.execute_const_or_reg(dst, |_dst| val2);
                        self.execute_const_or_reg(src, |_dst| val1);
                    }
                    Opcode::Load => {
                        let src = self.read_stack(self.eval_operand(src));
                        self.execute_const_or_reg(dst, |_dst| src);
                    }
                    Opcode::Store => {
                        let dst = self.eval_operand(dst);
                        let src = self.eval_operand(src);
                        self.write_stack(dst, src);
                    }
                    Opcode::JumpUnless if !banned => {
                        let condition = instruction_stream.next().unwrap();
                        if !self.eval_condition(condition) {
                            for _ in 0..self.eval_operand(src) {
                                instruction_stream.next();
                            }
                        }
                    }
                    Opcode::BanIf if !banned => {
                        let condition = instruction_stream.next().unwrap();
                        let ban_vec = match dst {
                            Operand::Register(reg) => &mut ban_conditions_regs[reg as usize],
                            Operand::Const(num) => &mut ban_conditions_nums[num as usize],
                        };
                        let opcode = self.eval_operand(src);
                        if opcode < (config::OPCODE_COUNT as u8) {
                            ban_vec[opcode as usize].push(condition);
                        } else {
                            for ban_opcode in 0..config::OPCODE_COUNT as u8 {
                                if !ban_opcode != opcode {
                                    ban_vec[ban_opcode as usize].push(condition);
                                }
                            }
                        }
                    }
                    Opcode::BanIfExcept if !banned => {
                        let condition = instruction_stream.next().unwrap();
                        let ban = |ban_vec: &mut Vec<Vec<_>>| {
                            let opcode = self.eval_operand(src);
                            if opcode < (config::OPCODE_COUNT as u8) {
                                ban_vec[opcode as usize].push(condition);
                            } else {
                                for ban_opcode in 0..config::OPCODE_COUNT as u8 {
                                    if !ban_opcode != opcode {
                                        ban_vec[ban_opcode as usize].push(condition);
                                    }
                                }
                            }
                        };
                        match dst {
                            Operand::Register(reg) => {
                                for ban_reg in 0..config::GENERAL_REGISTER_COUNT as u8 {
                                    if ban_reg != reg {
                                        ban(&mut ban_conditions_regs[ban_reg as usize])
                                    }
                                }
                            }
                            Operand::Const(num) => {
                                for ban_num in 0..=u8::MAX {
                                    if ban_num != num {
                                        ban(&mut ban_conditions_nums[ban_num as usize])
                                    }
                                }
                            }
                        };
                    }
                    Opcode::Win if !banned => {
                        if self.eval_operand(src) == self.memory.registers.reg_win {
                            score = score.saturating_add(1);
                            self.refresh_win_value();
                        }
                    }
                    _ => {}
                }
            }
        }
        score
    }
    pub fn insert_instructions(&mut self, index: usize, instructions: Vec<Instruction>) {
        if let Some(cache) = self.instruction_cache.get_mut(index) {
            *cache = instructions;
        }
    }
    pub fn from_seed(seed: u64) -> Emulator {
        let mut emulator = Emulator {
            memory: Default::default(),
            instruction_cache: Default::default(),
            rng: ChaCha12Rng::seed_from_u64(seed),
        };
        emulator.refresh_win_value();
        emulator
    }
}
