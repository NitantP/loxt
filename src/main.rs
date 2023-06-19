use num_enum::{IntoPrimitive, TryFromPrimitive};

type Value = f64;

#[derive(IntoPrimitive, TryFromPrimitive)]
#[repr(u8)]
enum OpCode {
    OpConstant,
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
    match OpCode::try_from(instruction).unwrap() {
        OpCode::OpReturn => simple_instruction("OP_RETURN", offset),
        OpCode::OpConstant => constant_instruction("OP_CONSTANT", chunk, offset),
    }
}

fn main() {
    let mut chunk = Chunk::new();
    chunk.write(OpCode::OpConstant.into(), 123);
    let constant_index = chunk.add_constant(1.2);
    chunk.write(constant_index as u8, 123);
    chunk.write(OpCode::OpReturn.into(), 123);
    disassemble_chunk(&chunk, "TEST");
}
