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
            operand1,
            operand2,
        } = condition;
        let val1 = self.eval_operand(operand1);
        let val2 = self.eval_operand(operand2);
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
    fn execute_const_or_reg<F>(&mut self, operand1: Operand, f: F)
    where
        F: Fn(u8) -> u8,
    {
        let dst = self.eval_operand(operand1);
        match operand1 {
            Operand::Register(reg) => self.write_register(reg, f(dst)),
            Operand::Const(num) => {
                for reg in 0..config::GENERAL_REGISTER_COUNT {
                    let reg = reg as u8;
                    if self.read_register(reg) == num {
                        self.write_register(reg, f(dst));
                    }
                }
            }
        }
    }
    fn execute_arithmetic(
        &mut self,
        opcode: Opcode,
        operand1: Operand,
        operand2: Operand,
        ban_conditions: &[Instruction],
    ) {
        for condition in ban_conditions {
            if self.eval_condition(*condition) {
                return;
            }
        }
        let src = self.eval_operand(operand2);
        match opcode {
            Opcode::Add => self.execute_const_or_reg(operand1, |dst| dst.wrapping_add(src)),
            Opcode::Sub => self.execute_const_or_reg(operand1, |dst| dst.wrapping_sub(src)),
            Opcode::Or => self.execute_const_or_reg(operand1, |dst| dst | src),
            Opcode::Xor => self.execute_const_or_reg(operand1, |dst| dst ^ src),
            Opcode::Shr => self.execute_const_or_reg(operand1, |dst| dst.wrapping_shr(src as u32)),
            Opcode::Shl => self.execute_const_or_reg(operand1, |dst| dst.wrapping_shl(src as u32)),
            _ => unreachable!(),
        };
    }
    pub fn execute_all(&mut self) -> i64 {
        let mut ban_conditions_nums = vec![vec![vec![]; config::OPCODE_COUNT]; 256];
        let mut ban_conditions_regs =
            vec![vec![vec![]; config::OPCODE_COUNT]; config::TOTAL_REGISTER_COUNT];
        let mut score = 0i64;
        for instructions in self.instruction_cache.clone() {
            let mut instruction_stream = instructions.into_iter();
            while let Some(Instruction {
                opcode,
                operand1,
                operand2,
            }) = instruction_stream.next()
            {
                match opcode {
                    Opcode::Add
                    | Opcode::Sub
                    | Opcode::Or
                    | Opcode::Xor
                    | Opcode::Shr
                    | Opcode::Shl => {
                        let ban_conditions = match operand2 {
                            Operand::Register(reg) => {
                                &ban_conditions_regs[reg as usize][opcode as u8 as usize]
                            }
                            Operand::Const(num) => {
                                &ban_conditions_nums[num as usize][opcode as u8 as usize]
                            }
                        };
                        self.execute_arithmetic(opcode, operand1, operand2, ban_conditions);
                    }
                    Opcode::Move => {
                        let src = self.eval_operand(operand2);
                        self.execute_const_or_reg(operand1, |_dst| src);
                    }
                    Opcode::Push => {
                        for idx in (1..config::STACK_SIZE as u8).rev() {
                            self.write_stack(idx, self.read_stack(idx - 1));
                        }
                        self.write_stack(0, self.eval_operand(operand1));
                    }
                    Opcode::Pull => {
                        let src = self.read_stack(0);
                        self.execute_const_or_reg(operand1, |_dst| src);
                        for idx in 1..config::STACK_SIZE as u8 {
                            self.write_stack(idx - 1, self.read_stack(idx));
                        }
                        self.write_stack(config::STACK_SIZE as u8 - 1, 0);
                    }
                    Opcode::Swap => {
                        let val1 = self.eval_operand(operand1);
                        let val2 = self.eval_operand(operand2);
                        self.execute_const_or_reg(operand1, |_dst| val2);
                        self.execute_const_or_reg(operand2, |_dst| val1);
                    }
                    Opcode::Load => {
                        let src = self.read_stack(self.eval_operand(operand2));
                        self.execute_const_or_reg(operand1, |_dst| src);
                    }
                    Opcode::Store => {
                        let dst = self.eval_operand(operand1);
                        let src = self.eval_operand(operand2);
                        self.write_stack(dst, src);
                    }
                    Opcode::JumpUnless => {
                        let condition = instruction_stream.next().unwrap();
                        if !self.eval_condition(condition) {
                            for _ in 0..self.eval_operand(operand1) {
                                instruction_stream.next();
                            }
                        }
                    }
                    Opcode::BanIf => {
                        let condition = instruction_stream.next().unwrap();
                        let ban_vec = match operand1 {
                            Operand::Register(reg) => &mut ban_conditions_regs[reg as usize],
                            Operand::Const(num) => &mut ban_conditions_nums[num as usize],
                        };
                        let opcode = self.eval_operand(operand2);
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
                    Opcode::BanIfExcept => {
                        let condition = instruction_stream.next().unwrap();
                        let ban = |ban_vec: &mut Vec<Vec<_>>| {
                            let opcode = self.eval_operand(operand2);
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
                        match operand1 {
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
                                        ban(&mut ban_conditions_nums[num as usize])
                                    }
                                }
                            }
                        };
                    }
                    Opcode::Syscall => match operand1 {
                        Operand::Const(0x01) => {
                            score = score.saturating_add(1);
                            self.refresh_win_value();
                        }
                        Operand::Const(0xff) => score = score.saturating_sub(1),
                        _ => {}
                    },
                    _ => unreachable!(),
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
