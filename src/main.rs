use std::{
    env, fs,
    io::{self, Write},
};

use num_enum::{FromPrimitive, IntoPrimitive, TryFromPrimitive};

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
        let right = $self.pop_stack();
        let left = $self.pop_stack();
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

    fn add_constant(&mut self, constant: Value) -> Result<usize, String> {
        self.constants.push(constant);
        return Ok(self.constants.len() - 1);
    }
}

#[derive(IntoPrimitive, TryFromPrimitive)]
#[repr(u8)]
enum InterpretResult {
    InterpretOk,
    InterpretCompilerError,
    InterpretRuntimeError,
}

struct VM {
    chunk: Chunk,
    ip: usize,
    stack: Vec<Value>,
    stack_top: usize,
}

impl VM {
    fn new(chunk: Chunk) -> Self {
        Self {
            chunk,
            ip: 0,
            stack: Vec::with_capacity(STACK_SIZE),
            stack_top: 0,
        }
    }

    fn interpret(source: &str) -> InterpretResult {
        let mut chunk: Chunk = Chunk::new();

        if !Compiler::compile(source, &mut chunk) {
            return InterpretResult::InterpretCompilerError;
        }

        let mut vm: VM = VM::new(chunk);

        return vm.run();
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

#[derive(Copy, Clone, Debug, IntoPrimitive, PartialEq)]
#[repr(usize)]
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

    fn make_token(&self, r#type: TokenType) -> Token<'scanner> {
        return Token::new(
            r#type,
            self.start,
            self.current - self.start,
            self.line,
            &self.source[self.start..self.current],
        );
    }

    fn make_error_token(&self, message: &'static str, line: usize) -> Token<'scanner> {
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
        if self.is_eof() {
            return '\0';
        }

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

    fn scan_string(&mut self) -> Token<'scanner> {
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

    fn scan_number(&mut self) -> Token<'scanner> {
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

    fn scan_identifier(&mut self) -> Token<'scanner> {
        while is_alpha(self.peek()) || is_digit(self.peek()) {
            self.advance();
        }
        let token_type = self.identify_identifier();
        return self.make_token(token_type);
    }

    fn scan_token(&mut self) -> Token<'scanner> {
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
            '=' => {
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

enum ParserToken {
    Current,
    Previous,
}

struct Parser<'parser> {
    current: Option<Token<'parser>>,
    previous: Option<Token<'parser>>,
    had_error: bool,
    is_panic_mode: bool,
}

impl<'parser> Parser<'parser> {
    fn new() -> Self {
        Self {
            current: None,
            previous: None,
            had_error: false,
            is_panic_mode: false,
        }
    }

    fn error(&mut self, which_token: ParserToken, message: &str) {
        if self.is_panic_mode {
            return;
        }
        self.is_panic_mode = true;

        let token: &Token = match which_token {
            ParserToken::Current => self.current.as_ref().unwrap(),
            ParserToken::Previous => self.previous.as_ref().unwrap(),
        };

        eprint!("[line {}] Error", token.line);

        if token.r#type == TokenType::EOF {
            eprint!(" at end");
        } else if token.r#type == TokenType::Error {
            todo!();
        } else {
            eprint!(" at {}", token.lexeme);
        }

        eprintln!(": {}", message);

        self.had_error = true;
    }

    fn error_at_current(&mut self, message: &str) {
        self.error(ParserToken::Current, message);
    }

    fn error_at_previous(&mut self, message: &str) {
        self.error(ParserToken::Previous, message);
    }
}

#[derive(Clone, Copy, FromPrimitive, PartialEq, PartialOrd)]
#[repr(u8)]
enum Precedence {
    #[num_enum(default)]
    None,
    Assignment,
    Or,
    And,
    Equality,
    Comparison,
    Term,
    Factor,
    Unary,
    Call,
    Primary,
}

type ParseFn<'chunk, 'compiler> = fn(&mut Compiler<'chunk, 'compiler>);
struct ParseRule<'chunk, 'compiler> {
    prefix: Option<ParseFn<'chunk, 'compiler>>,
    infix: Option<ParseFn<'chunk, 'compiler>>,
    precedence: Precedence,
}

impl<'chunk, 'compiler> ParseRule<'chunk, 'compiler> {
    fn new(
        prefix: Option<ParseFn<'chunk, 'compiler>>,
        infix: Option<ParseFn<'chunk, 'compiler>>,
        precedence: Precedence,
    ) -> Self {
        Self {
            prefix,
            infix,
            precedence,
        }
    }
}

struct Compiler<'chunk, 'compiler> {
    chunk: &'chunk mut Chunk,
    scanner: Scanner<'compiler>,
    parser: Parser<'compiler>,
    rules: [ParseRule<'chunk, 'compiler>; 40],
}

impl<'chunk, 'compiler> Compiler<'chunk, 'compiler> {
    fn new(source: &'compiler str, chunk: &'chunk mut Chunk) -> Self {
        Self {
            chunk,
            scanner: Scanner::new(source),
            parser: Parser::new(),
            rules: [
                ParseRule::new(Some(Compiler::grouping), None, Precedence::None),
                ParseRule::new(None, None, Precedence::None), // TokenType::LeftParen
                ParseRule::new(None, None, Precedence::None), // TokenType::RightParen
                ParseRule::new(None, None, Precedence::None), // TokenType::RightBrace
                ParseRule::new(None, None, Precedence::None), // TokenType::Comma
                ParseRule::new(None, None, Precedence::None), // TokenType::Dot
                ParseRule::new(
                    Some(Compiler::unary),
                    Some(Compiler::binary),
                    Precedence::Term,
                ), // TokenType::Minus
                ParseRule::new(None, Some(Compiler::binary), Precedence::Term), // TokenType::Plus
                ParseRule::new(None, None, Precedence::None), // TokenType::Semicolon
                ParseRule::new(None, Some(Compiler::binary), Precedence::Factor), // TokenType::Slash
                ParseRule::new(None, Some(Compiler::binary), Precedence::Factor), // TokenType::Star
                ParseRule::new(None, None, Precedence::None),                     // TokenType::Bang
                ParseRule::new(None, None, Precedence::None), // TokenType::BangEqual
                ParseRule::new(None, None, Precedence::None), // TokenType::Equal
                ParseRule::new(None, None, Precedence::None), // TokenType::EqualEqual
                ParseRule::new(None, None, Precedence::None), // TokenType::Greater
                ParseRule::new(None, None, Precedence::None), // TokenType::GreaterEqual
                ParseRule::new(None, None, Precedence::None), // TokenType::Less
                ParseRule::new(None, None, Precedence::None), // TokenType::LessEqual
                ParseRule::new(None, None, Precedence::None), // TokenType::Identifier
                ParseRule::new(None, None, Precedence::None), // TokenType::String
                ParseRule::new(Some(Compiler::number), None, Precedence::None), // TokenType::Number
                ParseRule::new(None, None, Precedence::None), // TokenType::And
                ParseRule::new(None, None, Precedence::None), // TokenType::Class
                ParseRule::new(None, None, Precedence::None), // TokenType::Else
                ParseRule::new(None, None, Precedence::None), // TokenType::False
                ParseRule::new(None, None, Precedence::None), // TokenType::For
                ParseRule::new(None, None, Precedence::None), // TokenType::Fun
                ParseRule::new(None, None, Precedence::None), // TokenType::If
                ParseRule::new(None, None, Precedence::None), // TokenType::Nil
                ParseRule::new(None, None, Precedence::None), // TokenType::Or
                ParseRule::new(None, None, Precedence::None), // TokenType::Print
                ParseRule::new(None, None, Precedence::None), // TokenType::Return
                ParseRule::new(None, None, Precedence::None), // TokenType::Super
                ParseRule::new(None, None, Precedence::None), // TokenType::This
                ParseRule::new(None, None, Precedence::None), // TokenType::True
                ParseRule::new(None, None, Precedence::None), // TokenType::Var
                ParseRule::new(None, None, Precedence::None), // TokenType::While
                ParseRule::new(None, None, Precedence::None), // TokenType::Error
                ParseRule::new(None, None, Precedence::None), // TokenType::EOF
            ],
        }
    }

    fn compile(source: &str, chunk: &mut Chunk) -> bool {
        let mut compiler: Compiler = Compiler::new(source, chunk);

        compiler.advance();
        compiler.expression();
        compiler.consume(TokenType::EOF, "Expect end of expression.");

        compiler.end_compilation();

        return !compiler.parser.had_error;
    }

    fn emit_byte(&mut self, byte: u8) {
        self.chunk
            .write(byte, self.parser.previous.as_ref().unwrap().line);
    }

    fn emit_bytes(&mut self, byte1: u8, byte2: u8) {
        self.emit_byte(byte1);
        self.emit_byte(byte2);
    }

    fn emit_return(&mut self) {
        self.emit_byte(OpCode::OpReturn.into());
    }

    fn emit_constant(&mut self, value: Value) {
        let index: u8 = match self.chunk.add_constant(value) {
            Ok(x) => x as u8,
            Err(_) => {
                self.parser
                    .error_at_previous("Too many constants in one chunk.");
                0
            }
        };

        self.emit_bytes(OpCode::OpConstant.into(), index);
    }

    fn end_compilation(&mut self) {
        self.emit_return();

        if env::var("DEBUG").is_ok() && !self.parser.had_error {
            disassemble_chunk(self.chunk, "code");
        }
    }

    fn advance(&mut self) {
        self.parser.previous = Option::take(&mut self.parser.current);

        loop {
            self.parser.current = Some(self.scanner.scan_token());
            if self.parser.current.as_ref().unwrap().r#type != TokenType::Error {
                break;
            }

            self.parser
                .error_at_current(self.parser.current.as_ref().unwrap().lexeme);
        }
    }

    fn consume(&mut self, r#type: TokenType, message: &str) {
        if self.parser.current.as_ref().unwrap().r#type == r#type {
            self.advance();
            return;
        }

        self.parser.error_at_current(message);
    }

    fn get_rule(&self, operator_type: TokenType) -> &ParseRule<'chunk, 'compiler> {
        return self.rules.get(operator_type as usize).unwrap();
    }

    fn parse_precedence(&mut self, precedence: Precedence) {
        self.advance();

        match self
            .get_rule(self.parser.previous.as_ref().unwrap().r#type)
            .prefix
        {
            Some(prefix_rule) => prefix_rule(self),
            None => self.parser.error_at_previous("Expect expression."),
        }

        while precedence
            <= self
                .get_rule(self.parser.current.as_ref().unwrap().r#type)
                .precedence
        {
            self.advance();
            let previous_token = self.parser.previous.as_ref().unwrap();
            let previous_token_rules: &ParseRule = self.get_rule(previous_token.r#type);

            match previous_token_rules.infix {
                Some(infix_rule) => infix_rule(self),
                None => self.parser.error_at_previous("Expect infix expression."),
            }
        }
    }

    fn expression(&mut self) {
        self.parse_precedence(Precedence::Assignment);
    }

    fn number(&mut self) {
        let value: Value = self
            .parser
            .previous
            .as_ref()
            .unwrap()
            .lexeme
            .parse::<Value>()
            .unwrap();

        self.emit_constant(value);
    }

    fn grouping(&mut self) {
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after expression.");
    }

    fn unary(&mut self) {
        let operator_type: TokenType = self.parser.previous.as_ref().unwrap().r#type;

        self.parse_precedence(Precedence::Unary);

        match operator_type {
            TokenType::Minus => self.emit_byte(OpCode::OpNegate.into()),
            _ => {}
        }
    }

    fn binary(&mut self) {
        let operator_type: TokenType = self.parser.previous.as_ref().unwrap().r#type;
        let rule: &ParseRule = self.get_rule(operator_type);

        let higher_precedence: Precedence = match Precedence::try_from(rule.precedence as u8 + 1) {
            Ok(result) => result,
            Err(_) => Precedence::from(0),
        };

        self.parse_precedence(higher_precedence);

        match operator_type {
            TokenType::Plus => self.emit_byte(OpCode::OpAdd.into()),
            TokenType::Minus => self.emit_byte(OpCode::OpSubtract.into()),
            TokenType::Star => self.emit_byte(OpCode::OpMultiply.into()),
            TokenType::Slash => self.emit_byte(OpCode::OpDivide.into()),
            _ => {}
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

fn repl() {
    let mut buffer = String::new();
    loop {
        print!("> ");
        io::stdout().flush().unwrap();

        io::stdin().read_line(&mut buffer).unwrap();
        if buffer.len() == 0 {
            break;
        } else {
            VM::interpret(&buffer);
            buffer.clear();
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
    repl();
}
