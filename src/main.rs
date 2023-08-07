use core::fmt;
use std::{
    collections::{HashMap},
    env, fs,
    io::{self, Write}
};

use num_enum::{FromPrimitive, IntoPrimitive, TryFromPrimitive};

const STACK_SIZE: usize = 256;

type Number = f64;

struct Interner {
    map: HashMap<String, u32>,
    vec: Vec<String>,
}

impl Interner {
    fn new() -> Self {
        Self {
            map: HashMap::new(),
            vec: Vec::new(),
        }
    }

    fn intern(&mut self, key: &str) -> u32 {
        if let Some(&idx) = self.map.get(key) {
            return idx;
        }

        let idx = self.vec.len() as u32;
        self.map.insert(key.to_owned(), idx);
        self.vec.push(key.to_owned());

        debug_assert!(self.lookup(idx) == key);
        debug_assert!(self.intern(key) == idx);

        return idx;
    }

    fn lookup(&self, idx: u32) -> &str {
        return self.vec[idx as usize].as_str();
    }
}

#[derive(Clone, Copy, Debug)]
enum Value {
    Bool(bool),
    Nil,
    Number(Number),
    String(u32),
}

fn is_digit(c: char) -> bool {
    return c >= '0' && c <= '9';
}

fn is_alpha(c: char) -> bool {
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
}

fn make_number(n: Number) -> Value {
    return Value::Number(n);
}

fn make_bool(b: bool) -> Value {
    return Value::Bool(b);
}

fn make_nil() -> Value {
    return Value::Nil;
}

fn as_number(value: &Value) -> Number {
    match value {
        Value::Number(n) => *n,
        _ => Number::MAX,
    }
}

fn as_bool(value: &Value) -> bool {
    match value {
        Value::Bool(b) => *b,
        _ => false,
    }
}

fn as_string_idx(value: Value) -> u32 {
    match value {
        Value::String(idx) => idx,
        _ => u32::MAX,
    }
}

fn is_number(value: &Value) -> bool {
    match value {
        Value::Number(_) => true,
        _ => false,
    }
}

fn is_bool(value: &Value) -> bool {
    match value {
        Value::Bool(_) => true,
        _ => false,
    }
}

fn is_nil(value: &Value) -> bool {
    match value {
        Value::Nil => true,
        _ => false,
    }
}

fn is_string(value: &Value) -> bool {
    match value {
        Value::String(_) => true,
        _ => false,
    }
}

fn is_falsey(value: &Value) -> bool {
    return is_nil(value) || (is_bool(value) && !as_bool(value))
}

fn are_equal(v1: &Value, v2: &Value) -> bool {
    match (v1, v2) {
        (Value::Bool(b1), Value::Bool(b2)) => b1 == b2,
        (Value::Number(n1), Value::Number(n2)) => n1 == n2,
        (Value::String(i1), Value::String(i2)) => i1 == i2,
        (Value::Nil, Value::Nil) => true,
        _ => false,
    }
}

#[macro_export]
macro_rules! binary_op {
    ($self: expr, $op: tt, $transform: tt) => {{
        if !is_number($self.peek(0)) || !is_number($self.peek(1)) {
            $self.runtime_error("Expected numeric operands for binary operator.");
            return InterpretResult::InterpretRuntimeError;
        }

        let right = as_number(&$self.pop_stack());
        let left = as_number(&$self.pop_stack());

        $self.push_stack($transform(left $op right));
    }};
}

#[derive(IntoPrimitive, TryFromPrimitive)]
#[repr(u8)]
enum OpCode {
    OpConstant,
    OpNil,
    OpTrue,
    OpFalse,
    OpEqual,
    OpGreater,
    OpLess,
    OpAdd,
    OpSubtract,
    OpMultiply,
    OpDivide,
    OpNot,
    OpNegate,
    OpReturn,
    OpPrint,
    OpPop,
    OpDefineGlobal,
    OpGetGlobal,
    OpSetGlobal,
    OpGetLocal,
    OpSetLocal,
    OpJump,
    OpJumpIfFalse,
    OpLoop,
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
    strings: Interner,
    globals: HashMap<u32, Value>,
}

impl VM {
    fn new(chunk: Chunk) -> Self {
        Self {
            chunk,
            ip: 0,
            stack: Vec::with_capacity(STACK_SIZE),
            stack_top: 0,
            strings: Interner::new(),
            globals: HashMap::new(),
        }
    }

    fn interpret(&mut self, source: &str) -> InterpretResult {
        let mut chunk: Chunk = Chunk::new();

        if !Compiler::compile(source, &mut chunk, &mut self.strings) {
            return InterpretResult::InterpretCompilerError;
        }

        self.chunk = chunk;
        self.ip = 0;
        return self.run();
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

    fn read_short(&mut self) -> usize {
        self.ip += 2;
        return ((self.chunk.code[self.ip - 2] as usize) << 8) | self.chunk.code[self.ip - 1] as usize;
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

    fn peek(&self, offset: usize) -> &Value {
        let index: usize = self.stack_top - 1 - offset;
        return &self.stack[index];
    }

    fn consume_number(&mut self) -> Option<Value> {
        match self.peek(0) {
            Value::Number(_) => {
                return Some(self.pop_stack());
            }
            _ => {
                self.runtime_error("Operand must be a number.");
                return None;
            }
        }
    }

    fn runtime_error(&mut self, message: &str) {
        eprintln!("{message}");

        let index: usize = self.ip - self.chunk.code.len() - 1;
        let line: &usize = self.chunk.lines.get(index).unwrap();
        eprintln!("[line {line}] in script");

        self.reset_stack();
    }

    fn run(&mut self) -> InterpretResult {
        loop {
            if env::var("DEBUG").is_ok() {
                print!("          ");
                for i in 0..self.stack_top {
                    print!("[{:?}]", *self.stack.get(i).unwrap())
                }
                println!();

                disassemble_instruction(&self.chunk, self.ip);
            }

            let instruction = self.read_byte();
            match OpCode::try_from(instruction).unwrap() {
                OpCode::OpReturn => {
                    return InterpretResult::InterpretOk;
                }
                OpCode::OpNegate => {
                    match self.consume_number() {
                        Some(Value::Number(n)) => self.push_stack(Value::Number(-n)),
                        _ => {
                            return InterpretResult::InterpretRuntimeError;
                        }
                    }
                }
                OpCode::OpConstant => {
                    let constant = self.read_constant();
                    self.push_stack(constant);
                }
                OpCode::OpNot => {
                    let result: Value = make_bool(is_falsey(&self.pop_stack()));
                    self.push_stack(result);
                }
                OpCode::OpEqual => {
                    let b = self.pop_stack();
                    let a = self.pop_stack();
                    
                    self.push_stack(make_bool(are_equal(&a, &b)));
                }
                OpCode::OpGreater =>  binary_op!(self, >, make_bool),
                OpCode::OpLess =>  binary_op!(self, <, make_bool),
                OpCode::OpAdd => {
                    if is_string(self.peek(0)) && is_string(self.peek(1)) {
                        let b = as_string_idx(self.pop_stack());
                        let a = as_string_idx(self.pop_stack());

                        let b = self.strings.lookup(b);
                        let a = self.strings.lookup(a);

                        let concatenated = format!("{}{}", a, b);
                        let idx = self.strings.intern(&concatenated);
                        self.push_stack(Value::String(idx));
                    } else{
                        binary_op!(self, +, make_number);
                    }
                },
                OpCode::OpPrint => {
                    match self.pop_stack() {
                        Value::String(idx) => println!("{}", self.strings.lookup(idx)),
                        Value::Number(n) => println!("{}", n),
                        Value::Bool(b) => println!("{}", b),
                        Value::Nil => println!("nil"),
                    }
                }
                OpCode::OpDefineGlobal => {
                    let string_idx = as_string_idx(self.read_constant());
                    self.globals.insert(string_idx, self.peek(0).to_owned());
                    self.pop_stack();
                }
                OpCode::OpGetGlobal => {
                    let string_idx = as_string_idx(self.read_constant());
                    match self.globals.get(&string_idx) {
                        Some(&value) => self.push_stack(value),
                        None => {
                            self.runtime_error(&format!("Undefined variable '{}'.", self.strings.lookup(string_idx)));
                            return InterpretResult::InterpretRuntimeError;
                        }
                    }
                }
                OpCode::OpSetGlobal => {
                    let string_idx = as_string_idx(self.read_constant());
                    if self.globals.contains_key(&string_idx) {
                        self.globals.insert(string_idx, self.peek(0).to_owned());
                    } else {
                        self.runtime_error(&format!("Undefined variable '{}'.", self.strings.lookup(string_idx)));
                        return InterpretResult::InterpretRuntimeError;
                    }
                }
                OpCode::OpGetLocal => {
                    let idx = self.read_byte() as usize;
                    self.push_stack(self.stack[idx]);
                }
                OpCode::OpSetLocal => {
                    let idx = self.read_byte() as usize;
                    self.stack[idx] = *self.peek(0);
                }
                OpCode::OpJump => {
                    let offset = self.read_short();
                    self.ip += offset;
                }
                OpCode::OpJumpIfFalse => {
                    let offset = self.read_short();
                    if is_falsey(self.peek(0)) {
                        self.ip += offset;
                    }
                }
                OpCode::OpLoop => {
                    let offset = self.read_short();
                    self.ip -= offset;
                }
                OpCode::OpSubtract => binary_op!(self, -, make_number),
                OpCode::OpMultiply => binary_op!(self, *, make_number),
                OpCode::OpDivide => binary_op!(self, /, make_number),
                OpCode::OpNil => self.push_stack(make_nil()),
                OpCode::OpTrue => self.push_stack(make_bool(true)),
                OpCode::OpFalse => self.push_stack(make_bool(false)),
                OpCode::OpPop => { self.pop_stack(); }
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
                && &scanner.source[scanner.start + start..scanner.start + start + length] == rest
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

type ParseFn<'chunk, 'compiler> = fn(&mut Compiler<'chunk, 'compiler>, bool);
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

struct Local<'compiler> {
    name: &'compiler str,
    depth: i8,
}

impl<'compiler> Local<'compiler> {
    fn new(name: &'compiler str, depth: i8) -> Self {
        Self {
            name,
            depth,
        }
    }
}

struct Compiler<'chunk, 'compiler> {
    chunk: &'chunk mut Chunk,
    scanner: Scanner<'compiler>,
    parser: Parser<'compiler>,
    rules: [ParseRule<'chunk, 'compiler>; 40],
    strings: &'compiler mut Interner,
    locals: Vec<Local<'compiler>>,
    depth: i8,
}

impl<'chunk, 'compiler> Compiler<'chunk, 'compiler> {
    fn new(source: &'compiler str, chunk: &'chunk mut Chunk, strings: &'compiler mut Interner) -> Self {
        Self {
            chunk,
            scanner: Scanner::new(source),
            parser: Parser::new(),
            strings,
            locals: Vec::new(),
            depth: 0,
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
                ParseRule::new(Some(Compiler::unary), None, Precedence::None), // TokenType::Bang
                ParseRule::new(None, Some(Compiler::binary), Precedence::Equality), // TokenType::BangEqual
                ParseRule::new(None, None, Precedence::None), // TokenType::Equal
                ParseRule::new(None, Some(Compiler::binary), Precedence::Equality), // TokenType::EqualEqual
                ParseRule::new(None, Some(Compiler::binary), Precedence::Comparison), // TokenType::Greater
                ParseRule::new(None, Some(Compiler::binary), Precedence::Comparison), // TokenType::GreaterEqual
                ParseRule::new(None, Some(Compiler::binary), Precedence::Comparison), // TokenType::Less
                ParseRule::new(None, Some(Compiler::binary), Precedence::Comparison), // TokenType::LessEqual
                ParseRule::new(Some(Compiler::variable), None, Precedence::None), // TokenType::Identifier
                ParseRule::new(Some(Compiler::string), None, Precedence::None), // TokenType::String
                ParseRule::new(Some(Compiler::number), None, Precedence::None), // TokenType::Number
                ParseRule::new(None, Some(Compiler::and), Precedence::And), // TokenType::And
                ParseRule::new(None, None, Precedence::None), // TokenType::Class
                ParseRule::new(None, None, Precedence::None), // TokenType::Else
                ParseRule::new(Some(Compiler::literal), None, Precedence::None), // TokenType::False
                ParseRule::new(None, None, Precedence::None), // TokenType::For
                ParseRule::new(None, None, Precedence::None), // TokenType::Fun
                ParseRule::new(None, None, Precedence::None), // TokenType::If
                ParseRule::new(Some(Compiler::literal), None, Precedence::None), // TokenType::Nil
                ParseRule::new(None, Some(Compiler::or), Precedence::Or), // TokenType::Or
                ParseRule::new(None, None, Precedence::None), // TokenType::Print
                ParseRule::new(None, None, Precedence::None), // TokenType::Return
                ParseRule::new(None, None, Precedence::None), // TokenType::Super
                ParseRule::new(None, None, Precedence::None), // TokenType::This
                ParseRule::new(Some(Compiler::literal), None, Precedence::None), // TokenType::True
                ParseRule::new(None, None, Precedence::None), // TokenType::Var
                ParseRule::new(None, None, Precedence::None), // TokenType::While
                ParseRule::new(None, None, Precedence::None), // TokenType::Error
                ParseRule::new(None, None, Precedence::None), // TokenType::EOF
            ],
        }
    }

    fn compile(source: &str, chunk: &mut Chunk, strings: &mut Interner) -> bool {
        let mut compiler: Compiler = Compiler::new(source, chunk, strings);

        compiler.advance();
        
        while !compiler.matches(TokenType::EOF) {
            compiler.declaration();
            compiler.end_compilation();
        }

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

    fn emit_jump(&mut self, instr: OpCode) -> usize {
        self.emit_byte(instr.into());
        self.emit_byte(0xff);
        self.emit_byte(0xff);

        return self.chunk.code.len() - 2;
    }

    fn patch_jump(&mut self, backpatch_idx: usize) {
        let target = self.chunk.code.len() - backpatch_idx - 2;

        if target as u16 > u16::MAX {
            self.parser.error_at_previous("Too much code to jump over.");
        }

        self.chunk.code[backpatch_idx] = (target >> 8) as u8 & 0xff;
        self.chunk.code[backpatch_idx + 1] = target as u8 & 0xff;
    }

    fn emit_loop(&mut self, start_idx: usize) {
        self.emit_byte(OpCode::OpLoop.into());

        let offset: u16 = (self.chunk.code.len() + 2) as u16 - start_idx as u16;
        if offset > u16::MAX {
            self.parser.error_at_previous("Loop body too large.");
        }

        self.emit_byte((offset >> 8) as u8 & 0xff);
        self.emit_byte(offset as u8 & 0xff);
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

    fn check(&mut self, r#type: TokenType) -> bool {
        return self.parser.current.as_ref().unwrap().r#type == r#type;
    }

    fn matches(&mut self, r#type: TokenType) -> bool {
        if !self.check(r#type) {
            return false;
        }

        self.advance();
        return true;
    }

    fn get_rule(&self, operator_type: TokenType) -> &ParseRule<'chunk, 'compiler> {
        return self.rules.get(operator_type as usize).unwrap();
    }

    fn parse_precedence(&mut self, precedence: Precedence) {
        self.advance();

        let can_assign: bool = precedence <= Precedence::Assignment;
        match self
            .get_rule(self.parser.previous.as_ref().unwrap().r#type)
            .prefix
        {
            Some(prefix_rule) => prefix_rule(self, can_assign),
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
                Some(infix_rule) => infix_rule(self, can_assign),
                None => self.parser.error_at_previous("Expect infix expression."),
            }
        }

        if can_assign && self.matches(TokenType::Equal) {
            self.parser.error_at_current("Invalid assignment target.");
        }
    }

    fn parse_variable(&mut self, error_message: &str) -> u8 {
        self.consume(TokenType::Identifier, error_message);

        self.declare_variable();
        if self.depth > 0 {
            return 0;
        }

        return self.identifier_constant();
    }

    fn declare_variable(&mut self) {
        if self.depth == 0 {
            return;
        }

        let name = self.parser.previous.as_ref().unwrap().lexeme;

        for local in self.locals.iter().rev() {
            if local.depth != -1 && local.depth < self.depth {
                break;
            }

            if name == local.name {
                self.parser.error_at_previous("Already a variable with this name in this scope.");
            }
        }

        self.add_local(name);
    }

    fn define_variable(&mut self, global: u8) {
        if self.depth > 0 {
            self.mark_initialized();
            return;
        }

        self.emit_bytes(OpCode::OpDefineGlobal.into(), global);
    }

    fn mark_initialized(&mut self) {
        self.locals.last_mut().unwrap().depth = self.depth;
    }

    fn identifier_constant(&mut self) -> u8 {
        let lexeme = self.parser.previous.as_ref().unwrap().lexeme;
        let idx = self.strings.intern(lexeme);
        return self.chunk.add_constant(Value::String(idx)).unwrap() as u8;
    }

    fn add_local(&mut self, name: &'compiler str) {
        if self.locals.len() == 256 {
            self.parser.error_at_previous("Too many localy variables in function.");
            return;
        }

        self.locals.push(Local::new(name, -1));
    }

    fn synchronize(&mut self) {
        self.parser.is_panic_mode = false;

        let mut current = self.parser.current.as_ref().unwrap().r#type;
        while current != TokenType::EOF {
            if self.parser.previous.as_ref().unwrap().r#type == TokenType::Semicolon {
                return;
            }

            match current {
                TokenType::Class | 
                TokenType::Fun | 
                TokenType::Var | 
                TokenType::For | 
                TokenType::If | 
                TokenType::While | 
                TokenType::Print | 
                TokenType::Return => return,
                _ => {}, 
            }

            self.advance();
            current = self.parser.current.as_ref().unwrap().r#type;
        }
    }

    fn begin_scope(&mut self) {
        self.depth += 1;
    }

    fn end_scope(&mut self) {
        self.depth -= 1;

        loop {
            match self.locals.last() {
                Some(x) if x.depth > self.depth => {
                    self.locals.pop();
                    self.emit_byte(OpCode::OpPop.into())
                }
                _ => break,
            }
        }
    }

    fn declaration(&mut self) {
        if self.matches(TokenType::Var) {
            self.var_declaration();
        } else {
            self.statement();
        }

        if self.parser.is_panic_mode {
            self.synchronize();
        }
    }

    fn var_declaration(&mut self) {
        let global = self.parse_variable("Expect variable name.");

        if self.matches(TokenType::Equal) {
            self.expression();
        } else {
            self.emit_byte(OpCode::OpNil.into());
        }

        self.consume(TokenType::Semicolon, "Expect ';' after variable declaration.");
        self.define_variable(global);
    }

    fn statement(&mut self) {
        if self.matches(TokenType::Print) {
            self.print_statement();
        } else if self.matches(TokenType::While) {
            self.while_statement();
        } else if self.matches(TokenType::For) {
            self.for_statement();
        } else if self.matches(TokenType::LeftBrace) {
            self.begin_scope();
            self.block();
            self.end_scope();
        } else if self.matches(TokenType::If) {
            self.if_statement();
        } else {
            self.expression_statement();
        }
    }

    fn print_statement(&mut self) {
        self.expression();
        self.consume(TokenType::Semicolon, "Expect ';' after value.");
        self.emit_byte(OpCode::OpPrint.into());
    }

    fn if_statement(&mut self) {
        self.consume(TokenType::LeftParen, "Expect '(' after 'if'.");
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after condition.");

        let skip_then_backpatch_idx = self.emit_jump(OpCode::OpJumpIfFalse);
        self.emit_byte(OpCode::OpPop.into());
        self.statement();
        let skip_else_backpatch_idx = self.emit_jump(OpCode::OpJump);

        self.patch_jump(skip_then_backpatch_idx);
        self.emit_byte(OpCode::OpPop.into());
        if self.matches(TokenType::Else) {
            self.statement();
        }
        self.patch_jump(skip_else_backpatch_idx);
    }

    fn while_statement(&mut self) {
        let start_idx = self.chunk.code.len();

        self.consume(TokenType::LeftParen, "Expect '(' after 'while'.");
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after condition.");

        let exit_backpatch_idx = self.emit_jump(OpCode::OpJumpIfFalse);
        self.emit_byte(OpCode::OpPop.into());
        self.statement();
        self.emit_loop(start_idx);

        self.patch_jump(exit_backpatch_idx);
        self.emit_byte(OpCode::OpPop.into());
    }

    fn for_statement(&mut self) {
        self.begin_scope();
        
        self.consume(TokenType::LeftParen, "Expect '(' after 'for'.");
        if self.matches(TokenType::Semicolon) {
        } else if self.matches(TokenType::Var) {
            self.var_declaration();
        } else {
            self.expression_statement();
        }

        let mut loop_start_idx = self.chunk.code.len();
        let mut exit_backpatch_idx: Option<usize> = None;
        if !self.matches(TokenType::Semicolon) {
            self.expression();
            self.consume(TokenType::Semicolon, "Expect ';' after loop condition.");

            exit_backpatch_idx = Some(self.emit_jump(OpCode::OpJumpIfFalse));
            self.emit_byte(OpCode::OpPop.into());
        }
        
        if !self.matches(TokenType::RightParen) {
            let increment_backpatch_idx = self.emit_jump(OpCode::OpJump);
            let increment_start_idx = self.chunk.code.len();

            self.expression();
            self.emit_byte(OpCode::OpPop.into());
            self.consume(TokenType::RightParen, "Expect ')' after for clauses.");

            self.emit_loop(loop_start_idx);
            loop_start_idx = increment_start_idx;
            self.patch_jump(increment_backpatch_idx);
        }

        self.statement();

        self.emit_loop(loop_start_idx);
        match exit_backpatch_idx {
            Some(idx) => {
                self.patch_jump(idx);
                self.emit_byte(OpCode::OpPop.into());
            }
            None => {}
        }

        self.end_scope();
    }

    fn expression_statement(&mut self) {
        self.expression();
        self.consume(TokenType::Semicolon, "Expect ';' after expression.");
        self.emit_byte(OpCode::OpPop.into());
    }

    fn block(&mut self) {
        while !self.check(TokenType::RightBrace) && !self.check(TokenType::EOF) {
            self.declaration();
        }

        self.consume(TokenType::RightBrace, "Expect '}' after block.");
    }

    fn expression(&mut self) {
        self.parse_precedence(Precedence::Assignment);
    }

    fn number(&mut self, can_assign: bool) {
        let number: Number = self
            .parser
            .previous
            .as_ref()
            .unwrap()
            .lexeme
            .parse::<Number>()
            .unwrap();

        self.emit_constant(Value::Number(number));
    }

    fn grouping(&mut self, can_assign: bool) {
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after expression.");
    }

    fn unary(&mut self, can_assign: bool) {
        let operator_type: TokenType = self.parser.previous.as_ref().unwrap().r#type;

        self.parse_precedence(Precedence::Unary);

        match operator_type {
            TokenType::Bang => self.emit_byte(OpCode::OpNot.into()),
            TokenType::Minus => self.emit_byte(OpCode::OpNegate.into()),
            _ => {}
        }
    }

    fn binary(&mut self, can_assign: bool) {
        let operator_type: TokenType = self.parser.previous.as_ref().unwrap().r#type;
        let rule: &ParseRule = self.get_rule(operator_type);

        let higher_precedence: Precedence = match Precedence::try_from(rule.precedence as u8 + 1) {
            Ok(result) => result,
            Err(_) => Precedence::from(0),
        };

        self.parse_precedence(higher_precedence);

        match operator_type {
            TokenType::BangEqual => self.emit_bytes(OpCode::OpEqual.into(), OpCode::OpNot.into()),
            TokenType::EqualEqual => self.emit_byte(OpCode::OpEqual.into()),
            TokenType::Greater => self.emit_byte(OpCode::OpGreater.into()),
            TokenType::GreaterEqual => self.emit_bytes(OpCode::OpLess.into(), OpCode::OpNot.into()),
            TokenType::Less => self.emit_byte(OpCode::OpLess.into()),
            TokenType::LessEqual => self.emit_bytes(OpCode::OpGreater.into(), OpCode::OpNot.into()),
            TokenType::Plus => self.emit_byte(OpCode::OpAdd.into()),
            TokenType::Minus => self.emit_byte(OpCode::OpSubtract.into()),
            TokenType::Star => self.emit_byte(OpCode::OpMultiply.into()),
            TokenType::Slash => self.emit_byte(OpCode::OpDivide.into()),
            _ => {}
        }
    }

    fn literal(&mut self, can_assign: bool) {
        match self.parser.previous.as_ref().unwrap().r#type {
            TokenType::False => self.emit_byte(OpCode::OpFalse.into()),
            TokenType::Nil => self.emit_byte(OpCode::OpNil.into()),
            TokenType::True => self.emit_byte(OpCode::OpTrue.into()),
            _ => {
                self.parser.error_at_previous("Could not identify token type of literal.");
            }
        }
    }

    fn string(&mut self, can_assign: bool) {
        let lexeme = self.parser.previous.as_ref().unwrap().lexeme;
        let idx: u32 = self.strings.intern(&lexeme[1..lexeme.len() - 1]);
        self.emit_constant(Value::String(idx));
    }

    fn variable(&mut self, can_assign: bool) {
        self.named_variable(can_assign);
    }

    fn named_variable(&mut self, can_assign: bool) {
        let (arg, get_op, set_op) = match self.resolve_local() {
            -1 => (self.identifier_constant(), OpCode::OpGetGlobal, OpCode::OpSetGlobal),
            idx => (idx as u8, OpCode::OpGetLocal, OpCode::OpSetLocal),
        };

        if can_assign && self.matches(TokenType::Equal) {
            self.expression();
            self.emit_bytes(set_op.into(), arg);
        } else {
            self.emit_bytes(get_op.into(), arg);
        }
    }

    fn resolve_local(&mut self) -> i8 {
        let name = self.parser.previous.as_ref().unwrap().lexeme;

        for (idx, local) in self.locals.iter().rev().enumerate() {
            if name == local.name {
                if local.depth == -1 {
                    self.parser.error_at_previous("Can't read local variable in its own initializer.");
                }
                return idx as i8;
            }
        }

        return -1;
    }

    fn and(&mut self, can_assign: bool) {
        let short_circuit_backpatch_idx = self.emit_jump(OpCode::OpJumpIfFalse);

        self.emit_byte(OpCode::OpPop.into());
        self.parse_precedence(Precedence::And);

        self.patch_jump(short_circuit_backpatch_idx);
    }

    fn or(&mut self, can_assign: bool) {
        let continue_backpatch_idx = self.emit_jump(OpCode::OpJumpIfFalse);
        let short_circuit_backpatch_idx = self.emit_jump(OpCode::OpJump);

        self.patch_jump(continue_backpatch_idx);
        self.emit_byte(OpCode::OpPop.into());
        
        self.parse_precedence(Precedence::Or);
        self.patch_jump(short_circuit_backpatch_idx);
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
            "{:<16} {:>4} '{:?}'",
            name,
            constant_offset,
            chunk.constants.get(constant_offset).unwrap()
        );
        return offset + 2;
    }

    fn byte_instruction(name: &str, chunk: &Chunk, offset: usize) -> usize {
        let idx = chunk.code[offset + 1];
        println!("{:<16} {:>4}", name, idx);
        return offset + 2;
    }

    fn jump_instruction(name: &str, sign: i16, chunk: &Chunk, offset: usize) -> usize {
        let jump: i16 = (chunk.code[offset + 1] as i16) << 8 | chunk.code[offset + 2] as i16;
        println!("{:<16} {:>4} -> {}", name, offset, offset as i16 + 3 + (sign * jump));
        return offset + 3;
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
        OpCode::OpNil => simple_instruction("OP_NIL", offset),
        OpCode::OpTrue => simple_instruction("OP_TRUE", offset),
        OpCode::OpFalse => simple_instruction("OP_FALSE", offset),
        OpCode::OpNot => simple_instruction("OP_NOT", offset),
        OpCode::OpEqual => simple_instruction("OP_EQUAL", offset),
        OpCode::OpGreater => simple_instruction("OP_GREATER", offset),
        OpCode::OpLess => simple_instruction("OP_LESS", offset),
        OpCode::OpPrint => simple_instruction("OP_PRINT", offset),
        OpCode::OpPop => simple_instruction("OP_POP", offset),
        OpCode::OpDefineGlobal => constant_instruction("OP_DEFINE_GLOBAL", chunk, offset),
        OpCode::OpGetGlobal => constant_instruction("OP_GET_GLOBAL", chunk, offset),
        OpCode::OpSetGlobal => constant_instruction("OP_SET_GLOBAL", chunk, offset),
        OpCode::OpGetLocal => byte_instruction("OP_GET_LOCAL", chunk, offset),
        OpCode::OpSetLocal => byte_instruction("OP_SET_LOCAL", chunk, offset),
        OpCode::OpJump => jump_instruction("OP_JUMP", 1, chunk, offset),
        OpCode::OpJumpIfFalse => jump_instruction("OP_JUMP_IF_FALSE", 1, chunk, offset),
        OpCode::OpLoop => jump_instruction("OP_LOOP", -1, chunk, offset),
    };
}

fn repl() {
    let mut buffer = String::new();
    let mut vm: VM = VM::new(Chunk::new());
    loop {
        print!("> ");
        io::stdout().flush().unwrap();

        io::stdin().read_line(&mut buffer).unwrap();
        if buffer.len() == 0 {
            break;
        } else {
            vm.interpret(&buffer);
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
