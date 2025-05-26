use std::collections::HashMap;
use serde::{Serialize, Deserialize};
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum IROperation {
    Add,          // +
    Subtract,     // -
    Multiply,     // *
    Divide,       // /
    Modulus,      // %
    Power,        // ^
    And,   // and
    Or,    // or
    Xor,   // xor
    ShiftLeft,    // <<
    ShiftRight,   // >>
    Equal,        // ==
    NotEqual,     // !=
    Greater,      // >
    Less,         // <
    GreaterEqual, // >=
    LessEqual,    // <=
    Not,   // not
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DebugInfo{
    pub code_position: usize,
}


#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum IR{
    LoadNull, // load null to stack 
    LoadInt(i64), // load integer to stack
    LoadFloat(f64), // load float to stack
    LoadString(String), // load string to stack
    LoadBytes(Vec<u8>), // load bytes to stack
    LoadBool(bool), // load bool to stack
    LoadLambda(String, usize, bool, bool), // signature, code position, should capture
    ForkInstruction, // "fork" instruction and push the forked instruction gcref to stack
    BuildTuple(usize), // number of elements
    BuildKeyValue, // pop key and value from stack and build key value pair
    BuildNamed, // pop key and value from stack and build named argument
    BuildRange, // pop start and end from stack and build range
    BuildSet,
    BindSelf, // bind lambda's self to tuple
    BinaryOp(IROperation), // pop two values from stack and perform binary operation
    UnaryOp(IROperation), // pop one value from stack and perform unary operation
    Let(String), // pop value from stack and store it in variable
    Get(String), // get value from context and push the reference to stack
    Set, // pop value and reference from stack and set value
    Wrap, // wrap value to object
    GetAttr, // pop object and attribute from stack and push the reference to attribute to stack
    IndexOf, // pop object and index from stack and push the reference to index to stack
    KeyOf, // pop object and get the key of the object
    ValueOf, // pop object and get the value of the object
    SelfOf, // pop object and get the self of the object
    TypeOf, // pop object and get the type of the object
    CallLambda, // pop lambda and arguments from stack and call lambda
    Return, // pop value from stack and return it
    Raise, // raise value
    NewFrame, // create new frame
    NewBoundaryFrame(isize), // create new boundary frame
    PopFrame, // pop frame
    PopBoundaryFrame, // pop boundary frame
    Pop, // pop value from stack and discard it
    JumpOffset(isize), // jump to offset
    JumpIfFalseOffset(isize), // jump to offset if false
    ResetStack, // reset stack
    DeepCopyValue, // copy value
    CopyValue, // copy value
    RefValue, // get reference value
    DerefValue, // get dereference value
    Assert, // assert value
    Import, // import module from file
    RedirectJump(String), // redirect ir, not for vm just for ir generation
    RedirectJumpIfFalse(String), 
    RedirectLabel(String),
    RedirectNewBoundaryFrame(String),
    RedirectNextOrJump(String),
    Alias(String),
    WipeAlias,
    AliasOf,
    In,
    Emit,
    AsyncCallLambda,
    IsFinished,
    NextOrJump(isize),
    ResetIter,
    Swap(usize, usize), // swap two values in stack
    ForkStackObjectRef(usize), // fork stack object
    PushValueIntoTuple(usize), // push value into tuple on stack
    CaptureOf, // capture value
    LengthOf, // get length of object
    IsSameObject, // check if two objects are the same
}

#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct IRPackage {
    pub instructions: Vec<(DebugInfo, IR)>,
    pub function_ips: HashMap<String, usize>,
    pub source: Option<String>,
}
impl IRPackage {
    pub fn read_from_file(file_path: &str) -> Result<IRPackage, String> {
        use std::fs::File;
        use std::io::Read;
        use std::path::Path;
        
        // Check if file exists
        if !Path::new(file_path).exists() {
            return Err(format!("File not found: {}", file_path));
        }
        
        // Open and read file
        let mut file = match File::open(file_path) {
            Ok(file) => file,
            Err(e) => return Err(format!("Failed to open file: {}", e)),
        };
        
        let mut buffer = Vec::new();
        if let Err(e) = file.read_to_end(&mut buffer) {
            return Err(format!("Failed to read file content: {}", e));
        }
        
        // Deserialize data
        match bincode::deserialize::<IRPackage>(&buffer) {
            Ok(package) => Ok(package),
            Err(e) => Err(format!("Failed to deserialize IR package: {}", e)),
        }
    }

    pub fn write_to_file(&self, file_path: &str) -> Result<(), String> {
        use std::fs::File;
        use std::io::Write;
        use std::path::Path;
        
        // Ensure directory exists
        if let Some(parent) = Path::new(file_path).parent() {
            if !parent.exists() {
                if let Err(e) = std::fs::create_dir_all(parent) {
                    return Err(format!("Failed to create directory: {}", e));
                }
            }
        }
        
        // Serialize data
        let serialized = match bincode::serialize(self) {
            Ok(data) => data,
            Err(e) => return Err(format!("Failed to serialize IR package: {}", e)),
        };
        
        // Write to file
        let mut file = match File::create(file_path) {
            Ok(file) => file,
            Err(e) => return Err(format!("Failed to create file: {}", e)),
        };
        
        match file.write_all(&serialized) {
            Ok(_) => Ok(()),
            Err(e) => Err(format!("Failed to write to file: {}", e)),
        }
    }
}

#[derive(Debug)]
pub struct Functions{
    function_instructions: HashMap<String, Vec<(DebugInfo, IR)>>, // function name and instructions
}

impl Default for Functions {
    fn default() -> Self {
        Self::new()
    }
}

impl Functions {
    pub fn new() -> Functions {
        Functions {
            function_instructions: HashMap::new(),
        }
    }
    pub fn append(&mut self, function_name: String, instructions: Vec<(DebugInfo, IR)>) {
        self.function_instructions.insert(function_name, instructions);
    }

    pub fn build_instructions(&mut self, source: Option<String>) -> IRPackage {
        let mut func_ips = HashMap::new();
        let mut instructions = Vec::new();
        for (func_name, func_instructions) in self.function_instructions.iter() {
            func_ips.insert(func_name.clone(), instructions.len());
            instructions.extend(func_instructions.clone());
        }
        IRPackage{instructions, function_ips:func_ips, source}
    }
}