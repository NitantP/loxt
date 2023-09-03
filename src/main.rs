use std::{
    collections::HashMap,
    env::{self, args}, fs,
    io::{self, Write}, mem, time::{UNIX_EPOCH, SystemTime}
};

use num_enum::{FromPrimitive, IntoPrimitive, TryFromPrimitive};

const FRAMES_MAX: usize = 64;
const STACK_MAX: usize = FRAMES_MAX * (u8::MAX as usize);

type Number = f64;

struct Interner {
    map: HashMap<String, usize>,
    vec: Vec<String>,
}

impl Interner {
    fn new() -> Self {
        Self {
            map: HashMap::new(),
            vec: Vec::new(),
        }
    }

    fn intern(&mut self, key: &str) -> usize{
        if let Some(&idx) = self.map.get(key) {
            return idx;
        }

        let idx = self.vec.len();
        self.map.insert(key.to_owned(), idx);
        self.vec.push(key.to_owned());

        debug_assert!(self.lookup(idx) == key);
        debug_assert!(self.intern(key) == idx);

        return idx;
    }

    fn lookup(&self, idx: usize) -> &str {
        return self.vec[idx].as_str();
    }
}

#[derive(Clone, Copy, Debug)]
enum Value {
    Bool(bool),
    Nil,
    Number(Number),
    String(usize),
    Function(usize),
    NativeFunction(usize),
    Closure(usize),
    Upvalue(usize),
    Class(usize),
    Instance(usize),
    BoundMethod(usize),
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

fn as_string_idx(value: Value) -> usize {
    match value {
        Value::String(idx) => idx,
        _ => usize::MAX,
    }
}

fn as_function_idx(value: Value) -> usize {
    match value {
        Value::Function(idx) | Value::Closure(idx) => idx,
        _ => usize::MAX,
    }
}

fn as_class_idx(value: Value) -> usize {
    match value {
        Value::Class(idx) => idx,
        _ => usize::MAX,
    }
}

fn as_instance_idx(value: Value) -> usize {
    match value {
        Value::Instance(idx) => idx,
        _ => usize::MAX,
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

fn is_closure(value: &Value) -> bool {
    match value {
        Value::Closure(_) => true,
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

#[derive(Debug)]
struct Upvalue {
    idx: u8,
    is_local: bool,
    value: Value,
    is_closed: bool,
}

impl Upvalue {
    fn new(idx: u8, is_local: bool) -> Self {
        Self {
            idx,
            is_local,
            value: Value::Nil,
            is_closed: false,
        }
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
    OpGetUpvalue,
    OpSetUpvalue,
    OpJump,
    OpJumpIfFalse,
    OpLoop,
    OpCall,
    OpClosure,
    OpCloseUpvalue,
    OpClass,
    OpGetProperty,
    OpSetProperty,
    OpMethod,
    OpInvoke,
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

#[derive(PartialEq)]
enum FunctionType {
    Function,
    Script,
    Method,
    Initializer,
}

struct Function {
    name: Option<Value>,
    arity: usize,
    chunk: Chunk,
    upvalues: Vec<Upvalue>,
}

impl Function {
    fn new() -> Self {
        Self {
            arity: 0,
            name: None,
            chunk: Chunk::new(),
            upvalues: Vec::new(),
        }
    }
}

type NativeFunction = fn(usize) -> Value;

struct Closure {
    function_idx: usize,
    upvalues: Vec<Value>,
    num_upvalues: usize,
}

impl Closure {
    fn new(function_idx: usize, num_upvalues: usize) -> Self {
        Self {
            function_idx,
            upvalues: Vec::with_capacity(num_upvalues),
            num_upvalues,
        }
    }
}

struct Class {
    name: Value,
    methods: HashMap<usize, Value>
}

impl Class {
    fn new(name: Value) -> Self {
        Self {
            name,
            methods: HashMap::new(),
        }
    }
}

struct Instance {
    class: usize,
    fields: HashMap<usize, Value>,
}

impl Instance {
    fn new(class: usize) -> Self {
        Self {
            class,
            fields: HashMap::new(),
        }
    }
}

struct BoundMethod {
    receiver: Value,
    method: Value,
}

impl BoundMethod {
    fn new(receiver: Value, method: Value) -> Self {
        Self {
            receiver,
            method
        }
    }
}

#[derive(IntoPrimitive, TryFromPrimitive)]
#[repr(u8)]
enum InterpretResult {
    InterpretOk,
    InterpretCompilerError,
    InterpretRuntimeError,
}

struct CallFrame {
    function: usize,
    ip: usize,
    stack_start: usize,
    closure: usize,
}

impl CallFrame {
    fn new(function: usize, closure: usize, stack_start: usize) -> Self {
        Self {
            function,
            ip: 0,
            stack_start,
            closure,
        }
    }
}

struct VM {
    frames: Vec<CallFrame>,
    stack: Vec<Value>,
    stack_top: usize,
    strings: Interner,
    globals: HashMap<usize, Value>,
    functions: Vec<Function>,
    closures: Vec<Closure>,
    classes: Vec<Class>,
    instances: Vec<Instance>,
    init_string: usize,
    bound_methods: Vec<BoundMethod>,
    native_functions: Vec<NativeFunction>,
    open_upvalues: Vec<Upvalue>,
}

impl VM {
    fn new() -> Self {
        let mut strings = Interner::new();
        let init_string = strings.intern("init");

        Self {
            frames: Vec::with_capacity(FRAMES_MAX),
            stack: Vec::with_capacity(STACK_MAX),
            stack_top: 0,
            strings,
            globals: HashMap::new(),
            functions: Vec::new(),
            closures: Vec::new(),
            classes: Vec::new(),
            instances: Vec::new(),
            init_string,
            bound_methods: Vec::new(),
            native_functions: Vec::new(),
            open_upvalues: Vec::new(),
        }
    }

    fn interpret(&mut self, source: &str) -> InterpretResult {
        match Compiler::compile(source, &mut self.strings, &mut self.functions) {
            Some(function) => {
                self.functions.push(function);
                self.frames.push(CallFrame::new(self.functions.len() - 1, 0, 0));
            }
            None => return InterpretResult::InterpretCompilerError,
        }

        if env::var("DEBUG").is_ok() {
            for function in self.functions.iter() {
                let function_name: String = match function.name {
                    Some(Value::String(idx)) => self.strings.lookup(idx).to_owned(),
                    _ => String::from("<script>"),
                };
                let chunk: &Chunk = &function.chunk;

                disassemble_chunk(&self.strings, &self.functions, chunk, &function_name);
            }
        }

        return self.run();
    }

    fn get_frame(&self) -> &CallFrame {
        return self.frames.last().unwrap();
    }

    fn get_frame_mut(&mut self) -> &mut CallFrame {
        return self.frames.last_mut().unwrap();
    }

    fn get_chunk(&self) -> &Chunk {
        return &self.functions[self.get_frame().function].chunk;
    }

    fn get_chunk_mut(&mut self) -> &Chunk {
        let frame = self.frames.last_mut().unwrap();
        return &mut self.functions[frame.function].chunk;
    }

    fn read_byte(&mut self) -> u8 {
        let frame = self.get_frame();
        let chunk = self.get_chunk();

        let byte = chunk.code[frame.ip];
        self.get_frame_mut().ip += 1;

        return byte;
    }

    fn read_constant(&mut self) -> Value {
        let constant_index = self.read_byte() as usize;
        let chunk = self.get_chunk();

        return chunk.constants[constant_index];
    }

    fn read_short(&mut self) -> usize {
        let frame = self.get_frame();
        let chunk = self.get_chunk();

        let short = ((chunk.code[frame.ip ] as usize) << 8) | chunk.code[frame.ip + 1] as usize;
        self.get_frame_mut().ip += 2;

        return short;
    }

    fn reset_stack(&mut self) {
        self.stack_top = 0;
        self.frames.clear();
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

    fn call_value(&mut self, callee: Value, arg_count: usize) -> bool {
        return match callee {
            Value::Closure(idx) => self.call(idx, arg_count),
            Value::BoundMethod(idx) => {
                let bound_method = &self.bound_methods[idx];
                self.stack[self.stack_top - 1 - arg_count] = bound_method.receiver;
                self.call(as_function_idx(bound_method.method), arg_count)
            }
            Value::NativeFunction(idx) => {
                let native_function = self.native_functions[idx];
                let result = native_function(arg_count);
                self.stack_top -= arg_count + 1;
                self.stack.truncate(self.stack_top);
                self.push_stack(result);
                true
            }
            Value::Class(idx) => {
                let instance = Instance::new(idx);
                self.instances.push(instance);
                self.stack[self.stack_top - 1 - arg_count] = Value::Instance(self.instances.len() - 1);

                let class = &self.classes[idx];
                match class.methods.get(&self.init_string) {
                    Some(&value) => return self.call(as_function_idx(value), arg_count),
                    _ => {}
                }

                if arg_count != 0 {
                    self.runtime_error(&format!("Expected 0 arguments but got {arg_count}."));
                    return false;
                }

                true
            }
            _ => {
                self.runtime_error("Can only call functions and classes.");
                false
            }
        }
    }

    fn invoke_from_class(&mut self, class: usize, name: usize, arg_count: usize) -> bool {
        let class = &self.classes[class];
        match class.methods.get(&name) {
            Some(value) => self.call(as_function_idx(*value), arg_count),
            _ => {
                let name = self.strings.lookup(name);
                self.runtime_error(&format!("Undefined property '{name}'"));
                return false;
            }
        }
    }

    fn invoke(&mut self, name: usize, arg_count: usize) -> bool {
        match *self.peek(arg_count) {
            Value::Instance(idx) => {
                let instance = &self.instances[idx];
                
                if let Some(&value) = instance.fields.get(&name) {
                    self.stack[self.stack_top - 1 - arg_count] = value;
                    return self.call_value(value, arg_count);
                }

                return self.invoke_from_class(instance.class, name, arg_count);
            }
            _ => {
                self.runtime_error("Only instances have methods.");
                return false;
            }
        }
        
    }

    fn bind_method(&mut self, class: usize, name: usize) -> bool {
        let class = &self.classes[class];
        return match class.methods.get(&name) {
            Some(method) => {
                let bound_method = BoundMethod::new(*self.peek(0), *method);
                self.pop_stack();

                self.bound_methods.push(bound_method);
                self.push_stack(Value::BoundMethod(self.bound_methods.len() - 1));
                true
            }
            None => {
                let name = self.strings.lookup(name);
                self.runtime_error(&format!("Undefined property '{name}'"));
                false
            }
        }
    }

    fn capture_upvalue(&mut self, stack_idx: usize) -> Value {
        let mut insert_idx = 0;
        for x in &self.open_upvalues {
            let upvalue_idx = x.idx as usize;

            // if upvalue_idx < stack_idx {
            //     break;
            // }

            if !x.is_closed && upvalue_idx == stack_idx {
                return Value::Upvalue(insert_idx);
            }

            insert_idx += 1;
        }

        let upvalue = Upvalue::new(stack_idx as u8, true);
        // self.open_upvalues.insert(insert_idx , upvalue);
        self.open_upvalues.push(upvalue);

        insert_idx = self.open_upvalues.len() - 1;

        return Value::Upvalue(insert_idx);
    }

    fn close_upvalues(&mut self, last: usize) {
        if self.frames.len() == 0 {
            return;
        }

        let stack_start = self.get_frame().stack_start;

        for x in &mut self.open_upvalues {
            let upvalue_idx = x.idx as usize;

            if upvalue_idx < last {
                break;
            }

            if !x.is_closed {
                x.value = self.stack[stack_start + upvalue_idx].clone();
                x.is_closed = true;
            }
        }
    }

    fn define_method(&mut self, name: usize) {
        let method = *self.peek(0);
        let class_idx = as_class_idx(*self.peek(1));

        self.classes[class_idx].methods.insert(name, method);
        self.pop_stack();
    }

    fn call(&mut self, idx: usize, arg_count: usize) -> bool {
        let function_idx = self.closures[idx].function_idx;
        let function_arity = self.functions[function_idx].arity;
        if arg_count != function_arity {
            self.runtime_error(&format!("Expected {} arguments, but got {}.", function_arity, arg_count));
            return false;
        }

        if self.frames.len() == FRAMES_MAX {
            self.runtime_error("Stack overflow.");
            return false;
        }

        let frame = CallFrame::new(function_idx, idx, self.stack_top - arg_count - 1);
        self.frames.push(frame);

        return true;
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

    fn define_native_function(&mut self, name: &str, function: NativeFunction) {
        self.native_functions.push(function);
        let function_idx = self.native_functions.len() - 1;
        let name_idx = self.strings.intern(name);

        self.push_stack(Value::String(name_idx));
        self.push_stack(Value::NativeFunction(function_idx));
        self.globals.insert(match self.stack[0] {
            Value::String(idx) => idx,
            _ => 0
        }, self.stack[1]);

        self.pop_stack();
        self.pop_stack();
    }

    fn runtime_error(&mut self, message: &str) {
        eprintln!("{message}");

        let frame = self.get_frame();
        let chunk = self.get_chunk();

        let index = frame.ip - chunk.code.len() - 1;
        let line = chunk.lines[index];

        eprintln!("[line {line}] in script");

        for i in self.frames.len() - 1..=0 {
            let frame = &self.frames[i];
            let function = &self.functions[frame.function];
            let instruction = frame.ip - 1;

            eprint!("[line {} in ]", function.chunk.lines[instruction]);
            match function.name {
                Some(Value::String(idx)) => eprintln!("{}()", self.strings.lookup(idx)),
                _ => eprintln!("script"),
            }
        }
        
        self.reset_stack();
    }

    fn run(&mut self) -> InterpretResult {
        loop {
            if env::var("DEBUG").is_ok() {
                let frame = self.get_frame();
                let chunk = self.get_chunk();

                print!("STACK: [");
                for i in &self.stack {
                    print!("{i:?}, ");
                }
                println!("]");

                print!("GLOBALS: [");
                for i in &self.globals {
                    print!("{i:?}, ");
                }
                println!("]");

                print!("STRINGS: [");
                for i in &self.strings.vec {
                    print!("{i:?}, ");
                }
                println!("]");

                print!("FUNCTIONS: [");
                for i in &self.functions {
                    print!("{}: {:?}, ", match i.name {
                        Some(Value::String(idx)) => self.strings.lookup(idx),
                        _ => "<script>",
                    }, i.chunk.code);
                }
                println!("]");

                println!();

                disassemble_instruction(&self.strings, &self.functions, chunk, frame.ip);
            }

            let instruction = self.read_byte();
            match OpCode::try_from(instruction).unwrap() {
                OpCode::OpReturn => {
                    let result = self.pop_stack();

                    let stack_start = self.frames.pop().unwrap().stack_start;
                    self.close_upvalues(stack_start);
                    self.stack_top = stack_start;
                    self.stack.truncate(self.stack_top);

                    if self.frames.is_empty() {
                        return InterpretResult::InterpretOk;
                    }

                    self.push_stack(result);
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
                        Value::Function(idx) | Value::Closure(idx) => {
                            match self.functions.get(idx).unwrap().name {
                                Some(Value::String(name)) => println!("<fn {}>", self.strings.lookup(name)), 
                                _ => println!("<script>"),
                            }
                        }
                        Value::BoundMethod(idx) => {
                            let bound_method = &self.bound_methods[idx];
                            let method = &self.closures[as_function_idx(bound_method.method)];
                            match self.functions[method.function_idx].name {
                                Some(Value::String(name)) => println!("<fn {}>", self.strings.lookup(name)), 
                                _ => println!("<script>"),
                            }
                        }
                        Value::Class(idx) => {
                            match self.classes.get(idx).unwrap().name {
                                Value::String(idx) => println!("<class {}>", self.strings.lookup(idx)),
                                _ => println!("<class>"),
                            }
                        }
                        Value::Instance(idx) => {
                            let class_idx = self.instances[idx].class;
                            match self.classes.get(class_idx).unwrap().name {
                                Value::String(idx) => println!("<{} instance>", self.strings.lookup(idx)),
                                _ => println!("<instance>"),
                            }
                        }
                        Value::Upvalue(idx) => println!("<upvalue {idx}>"),
                        Value::NativeFunction(_) => println!("<native fn>"),
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
                    let stack_start = self.get_frame().stack_start;
                    self.push_stack(self.stack[stack_start + idx]);
                }
                OpCode::OpSetLocal => {
                    let idx = self.read_byte() as usize;
                    let stack_start = self.get_frame().stack_start;
                    self.stack[stack_start + idx] = *self.peek(0);
                }
                OpCode::OpGetUpvalue => {
                    let slot = self.read_byte() as usize;
                    let upvalue_idx = match self.closures[self.get_frame().closure].upvalues[slot] {
                        Value::Upvalue(idx) => idx,
                        _ => panic!("brah idk idx"),
                    };
                    let upvalue = &self.open_upvalues[upvalue_idx];

                    if upvalue.is_closed {
                        self.push_stack(upvalue.value);
                    } else {
                        self.push_stack(self.stack[upvalue.idx as usize].clone())
                    }
                },
                OpCode::OpSetUpvalue => {
                    let slot = self.read_byte() as usize;
                    let upvalue_idx = match self.closures[self.get_frame().closure].upvalues[slot] {
                        Value::Upvalue(idx) => idx,
                        _ => panic!("brah idk idx"),
                    };
                    
                    if  self.open_upvalues[upvalue_idx].is_closed {
                        self.open_upvalues[upvalue_idx].value = self.peek(0).clone();
                    } else {
                        self.open_upvalues[upvalue_idx].idx = self.stack_top as u8 - 1;
                    }
                },
                OpCode::OpJump => {
                    let offset = self.read_short();
                    self.get_frame_mut().ip += offset;
                }
                OpCode::OpJumpIfFalse => {
                    let offset = self.read_short();
                    if is_falsey(self.peek(0)) {
                        self.get_frame_mut().ip += offset;
                    }
                }
                OpCode::OpLoop => {
                    let offset = self.read_short();
                    self.get_frame_mut().ip -= offset;
                }
                OpCode::OpCall => {
                    let arg_count = self.read_byte() as usize;
                    if !self.call_value(*self.peek(arg_count), arg_count) {
                        return InterpretResult::InterpretRuntimeError;
                    }
                }
                OpCode::OpClosure => {
                    let function_idx = as_function_idx(self.read_constant());
                    let num_upvalues = self.functions[function_idx].upvalues.len();
                    let mut closure = Closure::new(function_idx, num_upvalues);

                    for _ in 0..num_upvalues {
                        let is_local = self.read_byte();
                        let idx = self.read_byte() as usize;

                        closure.upvalues.push(
                            if is_local != 0 {
                                let stack_idx = self.get_frame().stack_start + idx;
                               self.capture_upvalue(stack_idx)
                            } else {
                                let closure_idx = self.get_frame().closure;
                                self.closures[closure_idx].upvalues[idx]
                            }
                        )
                    }

                    self.closures.push(closure);
                    self.push_stack(Value::Closure(self.closures.len() - 1));
                }
                OpCode::OpCloseUpvalue => {
                    self.close_upvalues(self.stack_top - 1);
                    self.pop_stack();
                }
                OpCode::OpClass => {
                    let class = Class::new(self.read_constant());
                    self.classes.push(class);
                    self.push_stack(Value::Class(self.classes.len() - 1));
                }
                OpCode::OpGetProperty => {
                    let instance = match *self.peek(0) {
                        Value::Instance(idx) => idx,
                        _ => {
                            self.runtime_error("Only instances have properties.");
                            return InterpretResult::InterpretRuntimeError;
                        }
                    };
                    let name = match self.read_constant() {
                        Value::String(idx) => idx,
                        v => panic!("Expected property name string on stack, but received {:?}", v),
                    };

                    let instance = &self.instances[instance];
                    match instance.fields.get(&name) {
                        Some(&value) => {
                            self.pop_stack();
                            self.push_stack(value);
                        }
                        _ => {
                            if !self.bind_method(instance.class, name) {
                                return InterpretResult::InterpretRuntimeError;
                            }
                        }
                    }
                },
                OpCode::OpSetProperty => {
                    let instance = match *self.peek(1) {
                        Value::Instance(idx) => idx,
                        _ => {
                            self.runtime_error("Only instances have properties.");
                            return InterpretResult::InterpretRuntimeError;
                        }
                    };
                    let name = match self.read_constant() {
                        Value::String(idx) => idx,
                        v => panic!("Expected property name string on stack, but received {:?}", v),
                    };

                    let value = self.pop_stack();
                    self.instances[instance].fields.insert(name, value);

                    self.pop_stack();
                    self.push_stack(value);
                },
                OpCode::OpMethod => {
                    let name = match self.read_constant() {
                        Value::String(idx) => idx,
                        v => panic!("Expected method name string on stack, but received {:?}", v),
                    };
                    self.define_method(name);
                }
                OpCode::OpInvoke => {
                    let method_name = match self.read_constant() {
                        Value::String(idx) => idx,
                        v => panic!("Expected method name string on stack, but received {:?}", v),
                    };
                    let arg_count = self.read_byte();
                    
                    if !self.invoke(method_name, arg_count.into()) {
                        return InterpretResult::InterpretRuntimeError;
                    }
                },
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
                }
                '/' => {
                    if self.peek_next() == '/' {
                        while self.peek() != '\n' && !self.is_eof() {
                            self.advance();
                        }
                    } else {
                        return;
                    }
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

type ParseFn<'parser> = fn(&mut Parser<'parser>, bool);
struct ParseRule<'parser> {
    prefix: Option<ParseFn<'parser>>,
    infix: Option<ParseFn<'parser>>,
    precedence: Precedence,
}

impl<'parser> ParseRule<'parser> {
    fn new(
        prefix: Option<ParseFn<'parser>>,
        infix: Option<ParseFn<'parser>>,
        precedence: Precedence,
    ) -> Self {
        Self {
            prefix,
            infix,
            precedence,
        }
    }
}

enum ParserToken {
    Current,
    Previous,
}

struct Parser<'parser> {
    scanner: Scanner<'parser>,
    compiler: Compiler<'parser, 'parser>,
    current: Option<Token<'parser>>,
    previous: Option<Token<'parser>>,
    had_error: bool,
    is_panic_mode: bool,
    rules: [ParseRule<'parser>; 40],
}

impl<'parser> Parser<'parser> {
    fn new(source: &'parser str, strings: &'parser mut Interner, functions: &'parser mut Vec<Function>) -> Self {
        Self {
            scanner: Scanner::new(source),
            compiler: Compiler::new(strings, functions),
            current: None,
            previous: None,
            had_error: false,
            is_panic_mode: false,
            rules: [
                ParseRule::new(Some(Parser::grouping), Some(Parser::call), Precedence::Call), // TokenType::LeftParen
                ParseRule::new(None, None, Precedence::None), // TokenType::RightParen
                ParseRule::new(None, None, Precedence::None), // TokenType::LeftBrace
                ParseRule::new(None, None, Precedence::None), // TokenType::RightBrace
                ParseRule::new(None, None, Precedence::None), // TokenType::Comma
                ParseRule::new(None, Some(Parser::dot), Precedence::Call), // TokenType::Dot
                ParseRule::new(
                    Some(Parser::unary),
                    Some(Parser::binary),
                    Precedence::Term,
                ), // TokenType::Minus
                ParseRule::new(None, Some(Parser::binary), Precedence::Term), // TokenType::Plus
                ParseRule::new(None, None, Precedence::None), // TokenType::Semicolon
                ParseRule::new(None, Some(Parser::binary), Precedence::Factor), // TokenType::Slash
                ParseRule::new(None, Some(Parser::binary), Precedence::Factor), // TokenType::Star
                ParseRule::new(Some(Parser::unary), None, Precedence::None), // TokenType::Bang
                ParseRule::new(None, Some(Parser::binary), Precedence::Equality), // TokenType::BangEqual
                ParseRule::new(None, None, Precedence::None), // TokenType::Equal
                ParseRule::new(None, Some(Parser::binary), Precedence::Equality), // TokenType::EqualEqual
                ParseRule::new(None, Some(Parser::binary), Precedence::Comparison), // TokenType::Greater
                ParseRule::new(None, Some(Parser::binary), Precedence::Comparison), // TokenType::GreaterEqual
                ParseRule::new(None, Some(Parser::binary), Precedence::Comparison), // TokenType::Less
                ParseRule::new(None, Some(Parser::binary), Precedence::Comparison), // TokenType::LessEqual
                ParseRule::new(Some(Parser::variable), None, Precedence::None), // TokenType::Identifier
                ParseRule::new(Some(Parser::string), None, Precedence::None), // TokenType::String
                ParseRule::new(Some(Parser::number), None, Precedence::None), // TokenType::Number
                ParseRule::new(None, Some(Parser::and), Precedence::And), // TokenType::And
                ParseRule::new(None, None, Precedence::None), // TokenType::Class
                ParseRule::new(None, None, Precedence::None), // TokenType::Else
                ParseRule::new(Some(Parser::literal), None, Precedence::None), // TokenType::False
                ParseRule::new(None, None, Precedence::None), // TokenType::For
                ParseRule::new(None, None, Precedence::None), // TokenType::Fun
                ParseRule::new(None, None, Precedence::None), // TokenType::If
                ParseRule::new(Some(Parser::literal), None, Precedence::None), // TokenType::Nil
                ParseRule::new(None, Some(Parser::or), Precedence::Or), // TokenType::Or
                ParseRule::new(None, None, Precedence::None), // TokenType::Print
                ParseRule::new(None, None, Precedence::None), // TokenType::Return
                ParseRule::new(None, None, Precedence::None), // TokenType::Super
                ParseRule::new(Some(Parser::this), None, Precedence::None), // TokenType::This
                ParseRule::new(Some(Parser::literal), None, Precedence::None), // TokenType::True
                ParseRule::new(None, None, Precedence::None), // TokenType::Var
                ParseRule::new(None, None, Precedence::None), // TokenType::While
                ParseRule::new(None, None, Precedence::None), // TokenType::Error
                ParseRule::new(None, None, Precedence::None), // TokenType::EOF
            ],
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

    fn advance(&mut self) {
        self.previous = Option::take(&mut self.current);

        loop {
            self.current = Some(self.scanner.scan_token());
            if self.current.as_ref().unwrap().r#type != TokenType::Error {
                break;
            }

            self.error_at_current(self.current.as_ref().unwrap().lexeme);
        }
    }

    fn consume(&mut self, r#type: TokenType, message: &str) {
        if self.current.as_ref().unwrap().r#type == r#type {
            self.advance();
            return;
        }

        self.error_at_current(message);
    }

    fn check(&mut self, r#type: TokenType) -> bool {
        return self.current.as_ref().unwrap().r#type == r#type;
    }

    fn matches(&mut self, r#type: TokenType) -> bool {
        if !self.check(r#type) {
            return false;
        }

        self.advance();
        return true;
    }

    fn synchronize(&mut self) {
        self.is_panic_mode = false;

        let mut current = self.current.as_ref().unwrap().r#type;
        while current != TokenType::EOF {
            if self.previous.as_ref().unwrap().r#type == TokenType::Semicolon {
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
            current = self.current.as_ref().unwrap().r#type;
        }
    }

    fn get_rule(&self, operator_type: TokenType) -> &ParseRule<'parser> {
        return self.rules.get(operator_type as usize).unwrap();
    }

    fn parse_precedence(&mut self, precedence: Precedence) {
        self.advance();

        let can_assign: bool = precedence <= Precedence::Assignment;
        match self
            .get_rule(self.previous.as_ref().unwrap().r#type)
            .prefix
        {
            Some(prefix_rule) => prefix_rule(self, can_assign),
            None => self.error_at_previous("Expect expression."),
        }


        while precedence
            <= self
                .get_rule(self.current.as_ref().unwrap().r#type)
                .precedence
        {
            self.advance();
            let previous_token = self.previous.as_ref().unwrap();
            let previous_token_rules: &ParseRule = self.get_rule(previous_token.r#type);

            match previous_token_rules.infix {
                Some(infix_rule) => infix_rule(self, can_assign),
                None => self.error_at_previous("Expect infix expression."),
            }
        }

        if can_assign && self.matches(TokenType::Equal) {
            self.error_at_current("Invalid assignment target.");
        }
    }

    fn parse_variable(&mut self, error_message: &str) -> u8 {
        self.consume(TokenType::Identifier, error_message);
        let name = self.previous.as_ref().unwrap().lexeme;

        match self.compiler.declare_variable(name) {
            Ok(depth) if depth > 0 => return 0,
            Ok(_) => {},
            Err(message) => self.error_at_previous(&message),
        }

        return self.identifier_constant(name);
    }

    fn declaration(&mut self) {
        if self.matches(TokenType::Class) {
            self.class_declaration();
        } else if self.matches(TokenType::Fun) {
            self.fun_declaration();
        } else if self.matches(TokenType::Var) {
            self.var_declaration();
        } else {
            self.statement();
        }

        if self.is_panic_mode {
            self.synchronize();
        }
    }

    fn class_declaration(&mut self) {
        self.consume(TokenType::Identifier, "Expect class name.");
        let class_name = self.previous.as_ref().unwrap().lexeme;
        let constant_idx = self.identifier_constant(class_name);
        match self.compiler.declare_variable(class_name) {
            Ok(_) => {},
            Err(message) => self.error_at_previous(&message),
        }

        self.compiler.emit_bytes(OpCode::OpClass.into(), constant_idx);
        self.compiler.define_variable(constant_idx);

        let mut compilation_class = Some(Box::new(CompilationClass::new(None)));
        mem::swap(&mut self.compiler.class, &mut compilation_class);

        self.named_variable(false);

        self.consume(TokenType::LeftBrace, "Expect '{' before class body.");

        while !self.check(TokenType::RightBrace) && !self.check(TokenType::EOF) {
            self.method();
        }

        self.consume(TokenType::RightBrace, "Expect '}' after class body.");
        self.compiler.emit_byte(OpCode::OpPop.into());

        mem::swap(&mut self.compiler.class, &mut compilation_class);
    }

    fn fun_declaration(&mut self) {
        let global = self.parse_variable("Expect function name.");
        self.compiler.mark_initialized();
        self.function(FunctionType::Function);
        self.compiler.define_variable(global);
    }

    fn function(&mut self, function_type: FunctionType) {
        let mut swap_target = Box::new(CompilationUnit::new(None, function_type));
        mem::swap(&mut self.compiler.target, &mut swap_target);

        let name_idx = self.compiler.strings.intern(self.previous.as_ref().unwrap().lexeme);
        self.compiler.target.function.name = Some(Value::String(name_idx));
        self.compiler.target.enclosing = Some(swap_target);


        self.compiler.begin_scope();
        self.consume(TokenType::LeftParen, "Expect '(' after function name.");
        if !self.check(TokenType::RightParen) {
            loop {
                let function = &mut self.compiler.target.function;
                function.arity += 1;
                if function.arity > 255 {
                    self.error_at_current("Can't have more than 255 parameters.");
                }

                let constant = self.parse_variable("Expect parameter name.");
                self.compiler.define_variable(constant);

                if !self.matches(TokenType::Comma) {
                    break;
                }
            }
        }

        self.consume(TokenType::RightParen, "Expect ')' after parameters.");
        self.consume(TokenType::LeftBrace, "Expect '{' before function body.");
        self.block();

        let function = self.compiler.end_compilation();

        let idx = Value::Closure(self.compiler.functions.len());
        let constant_idx = self.compiler.get_chunk_mut().add_constant(idx).unwrap() as u8;
        self.compiler.emit_bytes(OpCode::OpClosure.into(), constant_idx);

        for x in &function.upvalues {
            self.compiler.emit_byte(x.is_local as u8);
            self.compiler.emit_byte(x.idx);
        }

        self.compiler.functions.push(function);
    }

    fn method(&mut self) {
        self.consume(TokenType::Identifier, "Expect method name.");
        let name = self.previous.as_ref().unwrap().lexeme;
        let constant_idx = self.identifier_constant(name);

        self.function(
            if name == "init" {
                FunctionType::Initializer
            } else {
                FunctionType::Method
            }
        );

        self.compiler.emit_bytes(OpCode::OpMethod.into(), constant_idx);
    }

    fn var_declaration(&mut self) {
        let global = self.parse_variable("Expect variable name.");

        if self.matches(TokenType::Equal) {
            self.expression();
        } else {
            self.compiler.emit_byte(OpCode::OpNil.into());
        }

        self.consume(TokenType::Semicolon, "Expect ';' after variable declaration.");
        self.compiler.define_variable(global);
    }

    fn statement(&mut self) {
        if self.matches(TokenType::Print) {
            self.print_statement();
        } else if self.matches(TokenType::Return) {
            self.return_statement();
        } else if self.matches(TokenType::While) {
            self.while_statement();
        } else if self.matches(TokenType::For) {
            self.for_statement();
        } else if self.matches(TokenType::LeftBrace) {
            self.compiler.begin_scope();
            self.block();
            self.compiler.end_scope();
        } else if self.matches(TokenType::If) {
            self.if_statement();
        } else {
            self.expression_statement();
        }
    }

    fn print_statement(&mut self) {
        self.expression();
        self.consume(TokenType::Semicolon, "Expect ';' after value.");
        self.compiler.emit_byte(OpCode::OpPrint.into());
    }

    fn return_statement(&mut self) {
        if self.compiler.target.function_type == FunctionType::Script {
            self.error_at_previous("Can't return from top-level code.");
        }

        if self.matches(TokenType::Semicolon) {
            self.compiler.emit_return();
        } else {
            if self.compiler.target.function_type == FunctionType::Initializer {
                self.error_at_previous("Can't return a value from an initializer.");
            }

            self.expression();
            self.consume(TokenType::Semicolon, "Expect ';' after return value.");
            self.compiler.emit_byte(OpCode::OpReturn.into());
        }
    }

    fn while_statement(&mut self) {
        let start_idx = self.compiler.get_chunk().code.len();

        self.consume(TokenType::LeftParen, "Expect '(' after 'while'.");
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after condition.");

        let exit_backpatch_idx = self.compiler.emit_jump(OpCode::OpJumpIfFalse);
        self.compiler.emit_byte(OpCode::OpPop.into());
        
        self.statement();

        match self.compiler.emit_loop(start_idx) {
            Err(message) => self.error_at_previous(&message),
            _ => {},
        }
        match self.compiler.patch_jump(exit_backpatch_idx) {
            Err(message) => self.error_at_previous(&message),
            _ => {},
        }
        self.compiler.emit_byte(OpCode::OpPop.into());
    }

    fn for_statement(&mut self) {
        self.compiler.begin_scope();
        
        self.consume(TokenType::LeftParen, "Expect '(' after 'for'.");
        if self.matches(TokenType::Semicolon) {
        } else if self.matches(TokenType::Var) {
            self.var_declaration();
        } else {
            self.expression_statement();
        }

        let mut loop_start_idx = self.compiler.get_chunk().code.len();
        let mut exit_backpatch_idx: Option<usize> = None;
        if !self.matches(TokenType::Semicolon) {
            self.expression();
            self.consume(TokenType::Semicolon, "Expect ';' after loop condition.");

            exit_backpatch_idx = Some(self.compiler.emit_jump(OpCode::OpJumpIfFalse));
            self.compiler.emit_byte(OpCode::OpPop.into());
        }
        
        if !self.matches(TokenType::RightParen) {
            let increment_backpatch_idx = self.compiler.emit_jump(OpCode::OpJump);
            let increment_start_idx = self.compiler.get_chunk().code.len();

            self.expression();
            self.compiler.emit_byte(OpCode::OpPop.into());
            self.consume(TokenType::RightParen, "Expect ')' after for clauses.");

            self.compiler.emit_loop(loop_start_idx).map_err(|msg| self.error_at_previous(&msg)).ok();
            loop_start_idx = increment_start_idx;
            self.compiler.patch_jump(increment_backpatch_idx).map_err(|msg| self.error_at_previous(&msg)).ok();
        }

        self.statement();

        self.compiler.emit_loop(loop_start_idx).map_err(|msg| self.error_at_previous(&msg)).ok();
        match exit_backpatch_idx {
            Some(idx) => {
                self.compiler.patch_jump(idx).map_err(|msg| self.error_at_previous(&msg)).ok();
                self.compiler.emit_byte(OpCode::OpPop.into());
            }
            None => {}
        }

        self.compiler.end_scope();
    }

    fn block(&mut self) {
        while !self.check(TokenType::RightBrace) && !self.check(TokenType::EOF) {
            self.declaration();
        }

        self.consume(TokenType::RightBrace, "Expect '}' after block.");
    }

    fn if_statement(&mut self) {
        self.consume(TokenType::LeftParen, "Expect '(' after 'if'.");
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after condition.");

        let skip_then_backpatch_idx = self.compiler.emit_jump(OpCode::OpJumpIfFalse);
        self.compiler.emit_byte(OpCode::OpPop.into());
        self.statement();
        let skip_else_backpatch_idx = self.compiler.emit_jump(OpCode::OpJump);

        self.compiler.patch_jump(skip_then_backpatch_idx).map_err(|msg| self.error_at_previous(&msg)).ok();
        self.compiler.emit_byte(OpCode::OpPop.into());
        if self.matches(TokenType::Else) {
            self.statement();
        }
        self.compiler.patch_jump(skip_else_backpatch_idx).map_err(|msg| self.error_at_previous(&msg)).ok();
    }

    fn expression_statement(&mut self) {
        self.expression();
        self.consume(TokenType::Semicolon, "Expect ';' after expression.");
        self.compiler.emit_byte(OpCode::OpPop.into());
    }

    fn expression(&mut self) {
        self.parse_precedence(Precedence::Assignment);
    }

    fn number(&mut self, can_assign: bool) {
        let number: Number = self
            .previous
            .as_ref()
            .unwrap()
            .lexeme
            .parse::<Number>()
            .unwrap();

        match self.compiler.emit_constant(Value::Number(number)) {
            Err(message) => self.error_at_previous(&message),
            _ => {},
        }
    }

    fn grouping(&mut self, can_assign: bool) {
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after expression.");
    }

    fn unary(&mut self, can_assign: bool) {
        let operator_type: TokenType = self.previous.as_ref().unwrap().r#type;

        self.parse_precedence(Precedence::Unary);

        match operator_type {
            TokenType::Bang => self.compiler.emit_byte(OpCode::OpNot.into()),
            TokenType::Minus => self.compiler.emit_byte(OpCode::OpNegate.into()),
            _ => {}
        }
    }

    fn binary(&mut self, can_assign: bool) {
        let operator_type: TokenType = self.previous.as_ref().unwrap().r#type;
        let rule: &ParseRule = self.get_rule(operator_type);

        let higher_precedence: Precedence = match Precedence::try_from(rule.precedence as u8 + 1) {
            Ok(result) => result,
            Err(_) => Precedence::from(0),
        };

        self.parse_precedence(higher_precedence);

        match operator_type {
            TokenType::BangEqual => self.compiler.emit_bytes(OpCode::OpEqual.into(), OpCode::OpNot.into()),
            TokenType::EqualEqual => self.compiler.emit_byte(OpCode::OpEqual.into()),
            TokenType::Greater => self.compiler.emit_byte(OpCode::OpGreater.into()),
            TokenType::GreaterEqual => self.compiler.emit_bytes(OpCode::OpLess.into(), OpCode::OpNot.into()),
            TokenType::Less => self.compiler.emit_byte(OpCode::OpLess.into()),
            TokenType::LessEqual => self.compiler.emit_bytes(OpCode::OpGreater.into(), OpCode::OpNot.into()),
            TokenType::Plus => self.compiler.emit_byte(OpCode::OpAdd.into()),
            TokenType::Minus => self.compiler.emit_byte(OpCode::OpSubtract.into()),
            TokenType::Star => self.compiler.emit_byte(OpCode::OpMultiply.into()),
            TokenType::Slash => self.compiler.emit_byte(OpCode::OpDivide.into()),
            _ => {}
        }
    }

    fn call(&mut self, can_assign: bool) {
        let arg_count = self.arguments();
        self.compiler.emit_bytes(OpCode::OpCall.into(), arg_count);
    }

    fn dot(&mut self, can_assign: bool) {
        self.consume(TokenType::Identifier, "Expect property name after '.'.");
        let name = self.previous.as_ref().unwrap().lexeme;
        let constant_idx = self.identifier_constant(name);

        if can_assign && self.matches(TokenType::Equal) {
            self.expression();
            self.compiler.emit_bytes(OpCode::OpSetProperty.into(), constant_idx);
        } else if self.matches(TokenType::LeftParen) {
            let arg_count = self.arguments();
            self.compiler.emit_bytes(OpCode::OpInvoke.into(), constant_idx);
            self.compiler.emit_byte(arg_count);
        } else {
            self.compiler.emit_bytes(OpCode::OpGetProperty.into(), constant_idx);
        }
    }

    fn literal(&mut self, can_assign: bool) {
        match self.previous.as_ref().unwrap().r#type {
            TokenType::False => self.compiler.emit_byte(OpCode::OpFalse.into()),
            TokenType::Nil => self.compiler.emit_byte(OpCode::OpNil.into()),
            TokenType::True => self.compiler.emit_byte(OpCode::OpTrue.into()),
            _ => {
                self.error_at_previous("Could not identify token type of literal.");
            }
        }
    }

    fn string(&mut self, can_assign: bool) {
        let lexeme = self.previous.as_ref().unwrap().lexeme;
        let idx = self.compiler.strings.intern(&lexeme[1..lexeme.len() - 1]);
        self.compiler.emit_constant(Value::String(idx)).map_err(|msg| self.error_at_previous(&msg)).ok();
    }

    fn variable(&mut self, can_assign: bool) {
        self.named_variable(can_assign);
    }

    fn this(&mut self, can_assign: bool) {
        if self.compiler.class.is_none() {
            self.error_at_previous("Can't use 'this' outside of a class.");
            return;
        }

        self.variable(false);
    }

    fn identifier_constant(&mut self, name: &str) -> u8 {
        let idx = self.compiler.strings.intern(name);
        return self.compiler.get_chunk_mut().add_constant(Value::String(idx)).unwrap() as u8;
    }

    fn arguments(&mut self) -> u8 {
        let mut arg_count = 0;

        if !self.check(TokenType::RightParen) {
            loop {
                self.expression();
                arg_count += 1;

                if arg_count > 255 {
                    self.error_at_previous("Can't have more than 255 arguments.");
                }

                if !self.matches(TokenType::Comma) {
                    break;
                }
            }
        }
        self.consume(TokenType::RightParen, "Expect ')' after arguments.");
        
        return arg_count;
    }

    fn named_variable(&mut self, can_assign: bool) {
        let name = self.previous.as_ref().unwrap().lexeme;

        let (arg, get_op, set_op) = {
            match self.compiler.resolve_local(name) {
                -1 => {
                    match self.compiler.resolve_upvalue(name) {
                        -1 => (self.identifier_constant(name), OpCode::OpGetGlobal, OpCode::OpSetGlobal),
                        idx => (idx as u8, OpCode::OpGetUpvalue, OpCode::OpSetUpvalue),
                        
                    }
                }
                idx => (idx as u8, OpCode::OpGetLocal, OpCode::OpSetLocal),
            }
        };

        if can_assign && self.matches(TokenType::Equal) {
            self.expression();
            self.compiler.emit_bytes(set_op.into(), arg);
        } else {
            self.compiler.emit_bytes(get_op.into(), arg);
        }
    }

    fn and(&mut self, can_assign: bool) {
        let short_circuit_backpatch_idx = self.compiler.emit_jump(OpCode::OpJumpIfFalse);

        self.compiler.emit_byte(OpCode::OpPop.into());
        self.parse_precedence(Precedence::And);

        self.compiler.patch_jump(short_circuit_backpatch_idx).map_err(|msg| self.error_at_previous(&msg)).ok();
    }

    fn or(&mut self, can_assign: bool) {
        let continue_backpatch_idx = self.compiler.emit_jump(OpCode::OpJumpIfFalse);
        let short_circuit_backpatch_idx = self.compiler.emit_jump(OpCode::OpJump);

        self.compiler.patch_jump(continue_backpatch_idx).map_err(|msg| self.error_at_previous(&msg)).ok();
        self.compiler.emit_byte(OpCode::OpPop.into());
        
        self.parse_precedence(Precedence::Or);
        self.compiler.patch_jump(short_circuit_backpatch_idx).map_err(|msg| self.error_at_previous(&msg)).ok();
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

#[derive(Debug)]
struct Local<'compiler> {
    name: &'compiler str,
    depth: i8,
    is_captured: bool
}

impl<'compiler> Local<'compiler> {
    fn new(name: &'compiler str, depth: i8, is_captured: bool) -> Self {
        Self {
            name,
            depth,
            is_captured,
        }
    }
}

struct CompilationClass {
    enclosing: Option<Box<CompilationClass>>,
}

impl CompilationClass {
    fn new(enclosing: Option<Box<CompilationClass>>) -> Self {
        Self { enclosing }
    }
}

struct CompilationUnit<'cu> {
    enclosing: Option<Box<CompilationUnit<'cu>>>,
    function: Function,
    function_type: FunctionType,
    locals: Vec<Local<'cu>>,
    depth: i8,
}

impl<'cu> CompilationUnit<'cu> {
    fn new(enclosing: Option<Box<CompilationUnit<'cu>>>, function_type: FunctionType) -> Self {
        Self {
            enclosing,
            function: Function::new(),
            locals: vec![
                if function_type != FunctionType::Function {
                    Local::new("this", 0, false)
                } else {
                    Local::new("", 0, false)
                }
            ],
            function_type,
            depth: 0,
        }
    }

    fn resolve_local(scope: &CompilationUnit, name: &str) -> i8 {
        for (idx, local) in scope.locals.iter().enumerate().rev() {
            if name == local.name {
                if local.depth == -1 {
                    println!("Can't read local variable in its own initializer.");
                }
                return idx as i8;
            }
        }

        return -1;
    }

    fn resolve_upvalue(scope: &mut CompilationUnit, name: &str) -> i8 {
        return match scope.enclosing.as_mut() {
            None => -1,
            Some(enclosing) => {
                match CompilationUnit::resolve_local(enclosing, name) {
                    -1 => {
                        // @note upvalue to resolve is not on the stack - it's a global?
                        match CompilationUnit::resolve_upvalue(enclosing, name) {
                            -1 => -1,
                            idx => scope.add_upvalue(idx as u8, false) as i8,
                        }
                    }
                    idx => {
                        enclosing.locals[idx as usize].is_captured = true;
                        scope.add_upvalue(idx as u8, true) as i8
                    }
                }
            }
        }
    }

    fn add_upvalue(&mut self, idx: u8, is_local: bool) -> usize {
        for (i, x) in self.function.upvalues.iter().enumerate() {
            if x.idx == idx && x.is_local == is_local {
                return i;
            }
        }

        if self.function.upvalues.len() as u8 == u8::MAX {
            // error: "Too many closure variables in function."
            return 0;
        }

        let upvalue = Upvalue::new(idx, is_local);
        self.function.upvalues.push(upvalue);

        return self.function.upvalues.len() - 1;
    }

}

struct Compiler<'compiler, 'cu> {
    strings: &'compiler mut Interner,
    functions: &'compiler mut Vec<Function>,
    target: Box<CompilationUnit<'cu>>,
    class: Option<Box<CompilationClass>>,
}

impl<'compiler, 'cu> Compiler<'compiler, 'cu> {
    fn new(strings: &'compiler mut Interner, functions: &'compiler mut Vec<Function>) -> Self {
        Self {
            strings,
            functions,
            target: Box::new(CompilationUnit::new(None, FunctionType::Script)),
            class: None,
        }
    }

    fn get_chunk(&self) -> &Chunk {
        return &self.target.function.chunk;
    }

    fn get_chunk_mut(&mut self) -> &mut Chunk {
        return &mut self.target.function.chunk;
    }

    fn compile(source: &str, strings: &mut Interner, functions: &mut Vec<Function>) -> Option<Function> {
        let mut parser: Parser = Parser::new(source, strings, functions);

        parser.advance();
        while !parser.matches(TokenType::EOF) {
            parser.declaration();
        }
        parser.compiler.emit_return();
        let had_error = parser.had_error;

        let function = parser.compiler.target.function;
        return if had_error { None } else { Some(function) };
    }

    fn emit_byte(&mut self, byte: u8) {
        self.get_chunk_mut().write(byte, 0);
    }

    fn emit_bytes(&mut self, byte1: u8, byte2: u8) {
        self.emit_byte(byte1);
        self.emit_byte(byte2);
    }

    fn emit_return(&mut self) {
        if self.target.function_type == FunctionType::Initializer {
            self.emit_bytes(OpCode::OpGetLocal.into(), 0)
        } else {
            self.emit_byte(OpCode::OpNil.into());
        }
        self.emit_byte(OpCode::OpReturn.into());
    }

    fn emit_constant(&mut self, value: Value) -> Result<(), String> {
        let index: u8 = match self.get_chunk_mut().add_constant(value) {
            Ok(x) => x as u8,
            Err(_) => {
                return Err(String::from("Too many constants in one chunk."));
            }
        };

        self.emit_bytes(OpCode::OpConstant.into(), index);

        return Ok(());
    }

    fn emit_jump(&mut self, instr: OpCode) -> usize {
        self.emit_byte(instr.into());
        self.emit_byte(0xff);
        self.emit_byte(0xff);

        return self.get_chunk().code.len() - 2;
    }

    fn patch_jump(&mut self, backpatch_idx: usize) -> Result<(), String> {
        let target = self.get_chunk().code.len() - backpatch_idx - 2;

        if target as u16 > u16::MAX {
            return Err(String::from("Too much code to jump over."));
        }

        let chunk: &mut Chunk = self.get_chunk_mut();
        chunk.code[backpatch_idx] = (target >> 8) as u8 & 0xff;
        chunk.code[backpatch_idx + 1] = target as u8 & 0xff;

        return Ok(());
    }

    fn emit_loop(&mut self, start_idx: usize) -> Result<(), String> {
        self.emit_byte(OpCode::OpLoop.into());

        let offset: u16 = (self.get_chunk().code.len() + 2) as u16 - start_idx as u16;
        if offset > u16::MAX {
            return Err(String::from("Loop body too large."));
        }

        self.emit_byte((offset >> 8) as u8 & 0xff);
        self.emit_byte(offset as u8 & 0xff);

        return Ok(());
    }

    fn end_compilation(&mut self) -> Function {
        self.emit_return();

        if let Some(enclosing) = self.target.enclosing.take() {
            let compiled_target = mem::replace::<CompilationUnit>(&mut self.target, *enclosing);
            return compiled_target.function;
        }

        panic!("No enclosing compilation scope!");
    }

    fn declare_variable(&mut self, name: &'cu str) -> Result<i8, String> {
        // @note: globals don't get pushed to the locals stack, so they can't be captured by
        // upvalues?
        if self.target.depth != 0 {
            for local in self.target.locals.iter().rev() {
                if local.depth != -1 && local.depth < self.target.depth {
                    break;
                }

                if name == local.name {
                    return Err(String::from("Already a variable with this name in this scope."));
                }
            }

            self.add_local(name)?;
        }

        return Ok(self.target.depth);
    }

    fn define_variable(&mut self, global: u8) {
        if self.target.depth > 0 {
            self.mark_initialized();
            return;
        }

        self.emit_bytes(OpCode::OpDefineGlobal.into(), global);
    }

    fn mark_initialized(&mut self) {
        if self.target.depth == 0 {
            return;
        }

        self.target.locals.last_mut().unwrap().depth = self.target.depth;
    }

    fn resolve_local(&self, name: &str) -> i8 {
        return CompilationUnit::resolve_local(&self.target, name);
    }

    fn add_local(&mut self, name: &'cu str) -> Result<usize, &str> {
        if self.target.locals.len() == 256 {
            return Err("Too many local variables in function.");
        }

        self.target.locals.push(Local::new(name, -1, false));
        return Ok(self.target.locals.len() - 1);
    }

    fn resolve_upvalue(&mut self, name: &str) -> i8 {
        return CompilationUnit::resolve_upvalue(&mut self.target, name);
    }

    fn begin_scope(&mut self) {
        self.target.depth += 1;
    }

    fn end_scope(&mut self) {
        self.target.depth -= 1;

        loop {
            match self.target.locals.last() {
                Some(x) if x.depth > self.target.depth => {
                    if x.is_captured {
                        self.emit_byte(OpCode::OpCloseUpvalue.into());
                    } else {
                        self.emit_byte(OpCode::OpPop.into());
                    }
                    self.target.locals.pop();
                }
                _ => break,
            }
        }
    } 

}

fn disassemble_chunk(strings: &Interner, functions: &Vec<Function>, chunk: &Chunk, name: &str) {
    println!("== {} ==", name);

    let mut offset = 0;
    while offset < chunk.code.len() {
        offset = disassemble_instruction(strings, functions, chunk, offset);
    }
}

fn disassemble_instruction(strings: &Interner, functions: &Vec<Function>, chunk: &Chunk, offset: usize) -> usize {
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

    fn invoke_instruction(name: &str, chunk: &Chunk, offset: usize) -> usize {
        let constant = chunk.code[offset + 1];
        let arg_count = chunk.code[offset + 2];
        println!("{name:<16} ({arg_count} args) {constant:>4}");
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
        OpCode::OpCall => byte_instruction("OP_CALL", chunk, offset),
        OpCode::OpClosure => {
            let mut offset = offset + 1;

            let constant_idx = chunk.code[offset];
            print!("{:<16} {:>4} ", "OP_CLOSURE", constant_idx);
            let function_idx = as_function_idx(chunk.constants[constant_idx as usize]);
            let function = &functions[function_idx];

            match function.name {
                Some(Value::String(name)) => println!("<fn {}>", strings.lookup(name)), 
                _ => println!("<script>"),
            }

            offset += 1;

            for _ in 0..function.upvalues.len() {
                let is_local = chunk.code[offset];
                let idx = chunk.code[offset + 1];
                println!("{:0>4}    |                     {} {}", offset, if is_local == 0 { "local" } else { "upvalue" }, idx);

                offset += 2;
            }

            return offset;
        }
        OpCode::OpGetUpvalue => byte_instruction("OP_GET_VALUE", chunk, offset),
        OpCode::OpSetUpvalue => byte_instruction("OP_SET_VALUE", chunk, offset),
        OpCode::OpCloseUpvalue => simple_instruction("OP_CLOSE_UPVALUE", offset),
        OpCode::OpClass => constant_instruction("OP_CLASS", chunk, offset),
        OpCode::OpGetProperty => constant_instruction("OP_GET_PROPERTY", chunk, offset),
        OpCode::OpSetProperty => constant_instruction("OP_SET_PROPERTY", chunk, offset),
        OpCode::OpMethod => constant_instruction("OP_METHOD", chunk, offset),
        OpCode::OpInvoke => invoke_instruction("OP_INVOKE", chunk, offset),
    };
}

fn clock(arg_count: usize) -> Value {
    return Value::Number(SystemTime::now().duration_since(UNIX_EPOCH).unwrap().as_secs_f64());
}

fn repl(vm: &mut VM) {
    let mut buffer = String::new();
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
    vm.interpret(&source);
}

fn main() {
    let mut vm: VM = VM::new();

    vm.define_native_function("clock", clock);

    let args: Vec<_> = env::args().collect();
    match args.len() {
        1 => repl(&mut vm),
        2 => run_file(&mut vm, &args[1]),
        _ => panic!("Usage: cargo run [-- <path>]"),
    }
}
