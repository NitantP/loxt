use std::{env, fs, io};

use num_enum::{IntoPrimitive, TryFromPrimitive};

const STACK_SIZE: usize = 256;

type Value = f64;

fn is_digit(c: char) -> bool {
    return c >= '0' && c <= '9';
}

fn is_alpha(c: char) -> bool {
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
}

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

    fn interpret(&mut self, source: &str) -> InterpretResult {
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

#[derive(Debug, PartialEq)]
enum TokenType {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals.
    Identifier,
    String,
    Number,

    // Keywords.
    And,
    Class,
    Else,
    False,
    For,
    Fun,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    Error,
    EOF,
}

struct Token<'token> {
    r#type: TokenType,
    start: usize,
    length: usize,
    line: usize,
    lexeme: &'token str,
}

impl<'token> Token<'token> {
    fn new(
        r#type: TokenType,
        start: usize,
        length: usize,
        line: usize,
        lexeme: &'token str,
    ) -> Self {
        Self {
            r#type,
            start,
            length,
            line,
            lexeme,
        }
    }

    fn new_error(message: &'token str, line: usize) -> Self {
        Self {
            r#type: TokenType::Error,
            start: 0,
            length: 0,
            line,
            lexeme: message,
        }
    }
}

struct Scanner<'scanner> {
    start: usize,
    current: usize,
    source: &'scanner str,
    line: usize,
}

impl<'scanner> Scanner<'scanner> {
    fn new(source: &'scanner str) -> Self {
        Self {
            start: 0,
            current: 0,
            source,
            line: 1,
        }
    }

    fn make_token(&self, r#type: TokenType) -> Token {
        return Token::new(
            r#type,
            self.start,
            self.current - self.start,
            self.line,
            &self.source[self.start..(self.current + 1)],
        );
    }

    fn make_error_token(&self, message: &'static str, line: usize) -> Token {
        return Token::new_error(message, line);
    }

    fn is_eof(&self) -> bool {
        return self.current >= self.source.len();
    }

    fn advance(&mut self) -> char {
        self.current += 1;
        return self.source.chars().nth(self.current - 1).unwrap();
    }

    fn skip(&mut self) {
        loop {
            match self.peek() {
                ' ' | '\r' | '\t' => {
                    self.advance();
                }
                '\n' => {
                    self.line += 1;
                    self.advance();
                    break;
                }
                '/' => {
                    if self.peek_next() == '/' {
                        while self.peek() != '\n' && !self.is_eof() {
                            self.advance();
                        }
                    } else {
                        return;
                    }
                    break;
                }
                _ => break,
            }
        }
    }

    fn peek(&mut self) -> char {
        return self.source.chars().nth(self.current).unwrap();
    }

    fn peek_next(&mut self) -> char {
        if self.is_eof() {
            return '\0';
        }

        return self.source.chars().nth(self.current + 1).unwrap();
    }

    fn r#match(&mut self, expected: char) -> bool {
        if self.is_eof() {
            return false;
        }
        if self.peek() != expected {
            return false;
        }

        self.current += 1;
        return true;
    }

    fn scan_string(&mut self) -> Token {
        while self.peek() != '"' && !self.is_eof() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance();
        }

        if self.is_eof() {
            return self.make_error_token("Unterminated string.", self.line);
        }

        self.advance();
        return self.make_token(TokenType::String);
    }

    fn scan_number(&mut self) -> Token {
        while is_digit(self.peek()) {
            self.advance();
        }

        if self.peek() == '.' && is_digit(self.peek_next()) {
            loop {
                self.advance();
                if is_digit(self.peek()) {
                    break;
                }
            }
        }

        return self.make_token(TokenType::Number);
    }

    fn identify_identifier(&mut self) -> TokenType {
        fn check_keyword(
            scanner: &Scanner,
            start: usize,
            length: usize,
            rest: &str,
            token_type: TokenType,
        ) -> TokenType {
            if scanner.current - scanner.start == start + length
                && &scanner.source[scanner.start + start..scanner.start + length] == rest
            {
                return token_type;
            }

            return TokenType::Identifier;
        }

        return match self.source.chars().nth(self.start).unwrap() {
            'a' => check_keyword(&self, 1, 2, "nd", TokenType::And),
            'c' => check_keyword(&self, 1, 4, "lass", TokenType::Class),
            'e' => check_keyword(&self, 1, 3, "lse", TokenType::Else),
            'i' => check_keyword(&self, 1, 1, "f", TokenType::If),
            'n' => check_keyword(&self, 1, 2, "il", TokenType::Nil),
            'o' => check_keyword(&self, 1, 1, "r", TokenType::Or),
            'p' => check_keyword(&self, 1, 4, "rint", TokenType::Print),
            'r' => check_keyword(&self, 1, 5, "eturn", TokenType::Return),
            's' => check_keyword(&self, 1, 4, "uper", TokenType::Super),
            'v' => check_keyword(&self, 1, 2, "ar", TokenType::Var),
            'w' => check_keyword(&self, 1, 4, "hile", TokenType::While),
            'f' => match self.source.chars().nth(self.start + 1).unwrap() {
                'a' => check_keyword(&self, 2, 3, "lse", TokenType::False),
                'o' => check_keyword(&self, 2, 1, "r", TokenType::For),
                'u' => check_keyword(&self, 2, 1, "n", TokenType::Fun),
                _ => TokenType::Identifier,
            },
            't' => match self.source.chars().nth(self.start + 1).unwrap() {
                'h' => check_keyword(&self, 2, 2, "is", TokenType::This),
                'r' => check_keyword(&self, 2, 2, "ue", TokenType::True),
                _ => TokenType::Identifier,
            },
            _ => TokenType::Identifier,
        };
    }

    fn scan_identifier(&mut self) -> Token {
        while is_alpha(self.peek()) || is_digit(self.peek()) {
            self.advance();
        }
        let token_type = self.identify_identifier();
        return self.make_token(token_type);
    }

    fn scan_token(&mut self) -> Token {
        self.skip();
        self.start = self.current;

        if self.is_eof() {
            return self.make_token(TokenType::EOF);
        }

        return match self.advance() {
            '(' => self.make_token(TokenType::LeftParen),
            ')' => self.make_token(TokenType::RightParen),
            '{' => self.make_token(TokenType::LeftBrace),
            '}' => self.make_token(TokenType::RightBrace),
            ';' => self.make_token(TokenType::Semicolon),
            ',' => self.make_token(TokenType::Comma),
            '.' => self.make_token(TokenType::Dot),
            '-' => self.make_token(TokenType::Minus),
            '+' => self.make_token(TokenType::Plus),
            '/' => self.make_token(TokenType::Slash),
            '*' => self.make_token(TokenType::Star),
            '!' => {
                if self.r#match('=') {
                    self.make_token(TokenType::BangEqual)
                } else {
                    self.make_token(TokenType::Bang)
                }
            }
            '-' => {
                if self.r#match('=') {
                    self.make_token(TokenType::EqualEqual)
                } else {
                    self.make_token(TokenType::Equal)
                }
            }
            '<' => {
                if self.r#match('=') {
                    self.make_token(TokenType::LessEqual)
                } else {
                    self.make_token(TokenType::Less)
                }
            }
            '>' => {
                if self.r#match('=') {
                    self.make_token(TokenType::GreaterEqual)
                } else {
                    self.make_token(TokenType::Greater)
                }
            }
            '"' => self.scan_string(),
            c => {
                if is_digit(c) {
                    self.scan_number()
                } else if is_alpha(c) {
                    self.scan_identifier()
                } else {
                    self.make_error_token("Unexpected character.", self.line)
                }
            }
        };
    }
}

struct Compiler {}

impl Compiler {
    fn new() -> Self {
        Self {}
    }

    fn compile(source: &str) {
        let mut scanner = Scanner::new(source);

        let mut current_line = 0;
        loop {
            let token = scanner.scan_token();
            if token.line != current_line {
                print!("{:>4} ", token.line);
                current_line = token.line;
            } else {
                print!("   | ");
            }
            println!("{:#?} '{}'", token.r#type, token.lexeme);

            if token.r#type == TokenType::EOF {
                break;
            }
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

fn repl(vm: &mut VM) {
    let mut buffer = String::new();
    loop {
        print!("> ");
        io::stdin().read_line(&mut buffer).unwrap();
        if buffer.len() == 0 {
            break;
        } else {
            vm.interpret(&buffer);
        }
    }
}

fn read_file(path: &str) -> String {
    return fs::read_to_string(path).unwrap();
}

fn run_file(vm: &mut VM, path: &str) {
    let source = read_file(path);
}

fn main() {
    let mut vm = VM::new(&Chunk::new());
}
