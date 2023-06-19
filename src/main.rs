use std::env;

use num_enum::{IntoPrimitive, TryFromPrimitive};

const STACK_SIZE: usize = 256;

type Value = f64;

#[macro_export]
macro_rules! binary_op {
    ($self: expr, $op: tt) => {{
        let left = $self.pop_stack();
        let right = $self.pop_stack();
        $self.push_stack(left $op right);
    }};
}

#[derive(IntoPrimitive, TryFromPrimitive)]
#[repr(u8)]
enum OpCode {
    OpConstant,
    OpAdd,
    OpSubtract,
    OpMultiply,
    OpDivide,
    OpNegate,
    OpReturn,
}

#[derive(Debug)]
struct Chunk {
    code: Vec<u8>,
    constants: Vec<Value>,
    lines: Vec<usize>,
}

impl Chunk {
    fn new() -> Self {
        Self {
            code: Vec::new(),
            constants: Vec::new(),
            lines: Vec::new(),
        }
    }

    fn write(&mut self, byte: u8, line: usize) {
        self.code.push(byte);
        self.lines.push(line);
    }

    fn add_constant(&mut self, constant: Value) -> usize {
        self.constants.push(constant);
        return self.constants.len() - 1;
    }
}

#[derive(IntoPrimitive, TryFromPrimitive)]
#[repr(u8)]
enum InterpretResult {
    InterpretOk,
    InterpretCompilerError,
    InterpretRuntimeError,
}

struct VM<'a> {
    chunk: &'a Chunk,
    ip: usize,
    stack: Vec<Value>,
    stack_top: usize,
}

impl<'a> VM<'a> {
    fn new(chunk: &'a Chunk) -> Self {
        Self {
            chunk,
            ip: 0,
            stack: Vec::with_capacity(STACK_SIZE),
            stack_top: 0,
        }
    }

    fn read_byte(&mut self) -> u8 {
        let instruction = *self.chunk.code.get(self.ip).unwrap();
        self.ip += 1;
        return instruction;
    }

    fn read_constant(&mut self) -> Value {
        let constant_index = self.read_byte() as usize;
        return *self.chunk.constants.get(constant_index).unwrap();
    }

    fn reset_stack(&mut self) {
        self.stack_top = 0;
    }

    fn push_stack(&mut self, value: Value) {
        self.stack.push(value);
        self.stack_top += 1;
    }

    fn pop_stack(&mut self) -> Value {
        self.stack_top -= 1;
        return self.stack.pop().unwrap();
    }

    fn interpret(&mut self) -> InterpretResult {
        return self.run();
    }

    fn run(&mut self) -> InterpretResult {
        loop {
            if env::var("DEBUG").is_ok() {
                print!("          ");
                for i in 0..self.stack_top {
                    print!("[{}]", *self.stack.get(i).unwrap())
                }
                println!();

                disassemble_instruction(&self.chunk, self.ip);
            }

            let instruction = self.read_byte();
            match OpCode::try_from(instruction).unwrap() {
                OpCode::OpReturn => {
                    let value = self.pop_stack();
                    println!("{value}");
                    return InterpretResult::InterpretOk;
                }
                OpCode::OpAdd => {
                    binary_op!(self, +)
                }
                OpCode::OpSubtract => {
                    binary_op!(self, -)
                }
                OpCode::OpMultiply => {
                    binary_op!(self, *)
                }
                OpCode::OpDivide => {
                    binary_op!(self, /)
                }
                OpCode::OpNegate => {
                    let value = self.pop_stack();
                    self.push_stack(-value);
                }
                OpCode::OpConstant => {
                    let constant = self.read_constant();
                    self.push_stack(constant);
                }
            };
        }
    }
}

fn disassemble_chunk(chunk: &Chunk, name: &str) {
    println!("== {} ==", name);

    let mut offset = 0;
    while offset < chunk.code.len() {
        offset = disassemble_instruction(chunk, offset);
    }
}

fn disassemble_instruction(chunk: &Chunk, offset: usize) -> usize {
    fn simple_instruction(name: &str, offset: usize) -> usize {
        println!("{name}");
        return offset + 1;
    }

    fn constant_instruction(name: &str, chunk: &Chunk, offset: usize) -> usize {
        let constant_offset = *chunk.code.get(offset + 1).unwrap() as usize;
        println!(
            "{:<16} {:>4} '{}'",
            name,
            constant_offset,
            chunk.constants.get(constant_offset).unwrap()
        );
        return offset + 2;
    }

    print!("{:0>4} ", offset);
    if offset > 0 && chunk.lines.get(offset).unwrap() == chunk.lines.get(offset - 1).unwrap() {
        print!("   | ");
    } else {
        print!("{:>4} ", chunk.lines.get(offset).unwrap());
    }

    let instruction = *chunk.code.get(offset).unwrap();
    return match OpCode::try_from(instruction).unwrap() {
        OpCode::OpReturn => simple_instruction("OP_RETURN", offset),
        OpCode::OpAdd => simple_instruction("OP_ADD", offset),
        OpCode::OpSubtract => simple_instruction("OP_SUBTRACT", offset),
        OpCode::OpMultiply => simple_instruction("OP_MULTIPLY", offset),
        OpCode::OpDivide => simple_instruction("OP_DIVIDE", offset),
        OpCode::OpNegate => simple_instruction("OP_NEGATE", offset),
        OpCode::OpConstant => constant_instruction("OP_CONSTANT", chunk, offset),
    };
}

fn main() {
    let mut chunk = Chunk::new();

    chunk.write(OpCode::OpConstant.into(), 123);
    let constant_index = chunk.add_constant(1.2);
    chunk.write(constant_index as u8, 123);

    chunk.write(OpCode::OpConstant.into(), 123);
    let constant_index = chunk.add_constant(3.4);
    chunk.write(constant_index as u8, 123);

    chunk.write(OpCode::OpAdd.into(), 123);

    chunk.write(OpCode::OpConstant.into(), 123);
    let constant_index = chunk.add_constant(5.6);
    chunk.write(constant_index as u8, 123);

    chunk.write(OpCode::OpDivide.into(), 123);

    chunk.write(OpCode::OpNegate.into(), 123);

    chunk.write(OpCode::OpReturn.into(), 123);

    let mut vm = VM::new(&chunk);
    vm.interpret();
}
