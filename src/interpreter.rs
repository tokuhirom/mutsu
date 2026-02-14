#![allow(clippy::result_large_err)]
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::env;
use std::fs;
use std::io::{Read, Seek, SeekFrom, Write};
use std::net::ToSocketAddrs;
#[cfg(unix)]
use std::os::unix::fs::{self as unix_fs, PermissionsExt};
#[cfg(windows)]
use std::os::windows::fs as windows_fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::thread;
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};

thread_local! {
    static UNICODE_PROP_CACHE: RefCell<HashMap<String, Option<regex::Regex>>> =
        RefCell::new(HashMap::new());
}

use crate::ast::{
    AssignOp, CallArg, ExpectedMatcher, Expr, FunctionDef, ParamDef, PhaserKind, Stmt,
};
use crate::lexer::{Lexer, TokenKind};
use crate::opcode::{CompiledCode, OpCode};
use crate::parser::Parser;
use crate::value::{JunctionKind, LazyList, RuntimeError, Value, make_rat, next_instance_id};
use num_traits::{Signed, Zero};

/// Check if a character matches a Unicode property by name.
/// Uses the `regex` crate's Unicode support with a thread-local cache.
fn check_unicode_property(name: &str, c: char) -> bool {
    // Handle Unicode block properties (InXxx) separately since the regex crate
    // doesn't support the unicode-block feature
    if let Some(block_name) = name.strip_prefix("In")
        && let Some((start, end)) = unicode_block_range(block_name)
    {
        let cp = c as u32;
        return cp >= start && cp <= end;
    }
    // Handle parameterized properties: bc<L> → Bidi_Class=L, sc<Latin> → Script=Latin
    if let Some(open) = name.find('<')
        && let Some(close) = name.find('>')
    {
        let prop = &name[..open];
        let value = &name[open + 1..close];
        // BiDi class is not supported by the regex crate; handle manually
        if prop == "bc" {
            return check_bidi_class(value, c);
        }
        let full_prop = match prop {
            "sc" => format!("Script={}", value),
            "gc" => format!("General_Category={}", value),
            _ => format!("{}={}", prop, value),
        };
        return UNICODE_PROP_CACHE.with(|cache| {
            let mut cache = cache.borrow_mut();
            let entry = cache.entry(full_prop.clone()).or_insert_with(|| {
                let pattern = format!(r"^\p{{{}}}", full_prop);
                regex::Regex::new(&pattern).ok()
            });
            match entry {
                Some(re) => re.is_match(&c.to_string()),
                None => false,
            }
        });
    }
    UNICODE_PROP_CACHE.with(|cache| {
        let mut cache = cache.borrow_mut();
        let entry = cache.entry(name.to_string()).or_insert_with(|| {
            let pattern = format!(r"^\p{{{}}}", name);
            regex::Regex::new(&pattern).ok()
        });
        match entry {
            Some(re) => re.is_match(&c.to_string()),
            None => false,
        }
    })
}

/// Check BiDi class of a character (regex crate doesn't support Bidi_Class).
fn check_bidi_class(class: &str, c: char) -> bool {
    match class {
        "L" => {
            // Left-to-Right: most alphabetic, syllabic, Han ideographic, non-currency symbols
            c.is_alphabetic()
                && !matches!(
                    c as u32,
                    0x0590..=0x05FF
                        | 0x07C0..=0x089F
                        | 0xFB1D..=0xFDFF
                        | 0xFE70..=0xFEFF
                        | 0x10800..=0x10FFF
                        | 0x1E800..=0x1EFFF
                        | 0x0600..=0x07BF
                        | 0x08A0..=0x08FF
                )
        }
        "R" => {
            // Right-to-Left: Hebrew, NKo, etc.
            matches!(
                c as u32,
                0x0590..=0x05FF | 0x07C0..=0x089F | 0xFB1D..=0xFB4F | 0x10800..=0x10FFF | 0x1E800..=0x1EFFF
            )
        }
        "EN" => {
            // European Number
            c.is_ascii_digit()
                || matches!(
                    c as u32,
                    0x06F0..=0x06F9 | 0x2070..=0x2079 | 0x2080..=0x2089
                )
        }
        "ES" => {
            // European Number Separator
            matches!(c, '+' | '-')
        }
        "ET" => {
            // European Number Terminator: currency symbols, #, %, etc.
            matches!(
                c as u32,
                0x0023..=0x0025
                    | 0x00A2..=0x00A5
                    | 0x00B0..=0x00B1
                    | 0x0609..=0x060A
                    | 0x066A
                    | 0x09F2..=0x09F3
                    | 0x20A0..=0x20CF
            ) || c == '#'
                || c == '%'
                || c == '\u{B0}'
                || c == '\u{B1}'
        }
        "WS" => {
            // Whitespace
            matches!(
                c as u32,
                0x000C | 0x0020 | 0x1680 | 0x2000..=0x200A | 0x2028 | 0x205F | 0x3000
            )
        }
        "AN" => {
            // Arabic Number
            matches!(c as u32, 0x0600..=0x0605 | 0x0660..=0x0669 | 0x066B..=0x066C)
        }
        "CS" => {
            // Common Number Separator
            matches!(
                c,
                ',' | '.'
                    | ':'
                    | '\u{00A0}'
                    | '\u{060C}'
                    | '\u{202F}'
                    | '\u{2044}'
                    | '\u{FE50}'
                    | '\u{FE52}'
                    | '\u{FF0C}'
                    | '\u{FF0E}'
            )
        }
        "ON" => {
            // Other Neutral: not easily categorizable, approximate
            !c.is_alphanumeric() && !c.is_whitespace() && !c.is_control()
        }
        _ => false,
    }
}

/// Look up a Unicode block range by name. Returns (start, end) inclusive.
/// Names are matched case-insensitively with non-alphanumeric chars stripped.
fn unicode_block_range(name: &str) -> Option<(u32, u32)> {
    let normalized: String = name
        .chars()
        .filter(|c| c.is_alphanumeric())
        .map(|c| c.to_ascii_lowercase())
        .collect();
    // Unicode blocks table (Unicode 15.0)
    match normalized.as_str() {
        "basiclatin" => Some((0x0000, 0x007F)),
        "latin1supplement" | "latin1" => Some((0x0080, 0x00FF)),
        "latinextendeda" => Some((0x0100, 0x017F)),
        "latinextendedb" => Some((0x0180, 0x024F)),
        "ipaextensions" => Some((0x0250, 0x02AF)),
        "spacingmodifierletters" => Some((0x02B0, 0x02FF)),
        "combiningdiacriticalmarks" => Some((0x0300, 0x036F)),
        "greekandcoptic" | "greek" => Some((0x0370, 0x03FF)),
        "cyrillic" => Some((0x0400, 0x04FF)),
        "cyrillicsupplement" | "cyrillicsupplementary" => Some((0x0500, 0x052F)),
        "armenian" => Some((0x0530, 0x058F)),
        "hebrew" => Some((0x0590, 0x05FF)),
        "arabic" => Some((0x0600, 0x06FF)),
        "syriac" => Some((0x0700, 0x074F)),
        "arabicsupplement" => Some((0x0750, 0x077F)),
        "thaana" => Some((0x0780, 0x07BF)),
        "nko" => Some((0x07C0, 0x07FF)),
        "samaritan" => Some((0x0800, 0x083F)),
        "mandaic" => Some((0x0840, 0x085F)),
        "devanagari" => Some((0x0900, 0x097F)),
        "bengali" => Some((0x0980, 0x09FF)),
        "gurmukhi" => Some((0x0A00, 0x0A7F)),
        "gujarati" => Some((0x0A80, 0x0AFF)),
        "oriya" => Some((0x0B00, 0x0B7F)),
        "tamil" => Some((0x0B80, 0x0BFF)),
        "telugu" => Some((0x0C00, 0x0C7F)),
        "kannada" => Some((0x0C80, 0x0CFF)),
        "malayalam" => Some((0x0D00, 0x0D7F)),
        "sinhala" => Some((0x0D80, 0x0DFF)),
        "thai" => Some((0x0E00, 0x0E7F)),
        "lao" => Some((0x0E80, 0x0EFF)),
        "tibetan" => Some((0x0F00, 0x0FFF)),
        "myanmar" => Some((0x1000, 0x109F)),
        "georgian" => Some((0x10A0, 0x10FF)),
        "hanguljamo" => Some((0x1100, 0x11FF)),
        "ethiopic" => Some((0x1200, 0x137F)),
        "ethiopicsupplement" => Some((0x1380, 0x139F)),
        "cherokee" => Some((0x13A0, 0x13FF)),
        "unifiedcanadianaboriginalsyllabics" => Some((0x1400, 0x167F)),
        "ogham" => Some((0x1680, 0x169F)),
        "runic" => Some((0x16A0, 0x16FF)),
        "tagalog" => Some((0x1700, 0x171F)),
        "hanunoo" => Some((0x1720, 0x173F)),
        "buhid" => Some((0x1740, 0x175F)),
        "tagbanwa" => Some((0x1760, 0x177F)),
        "khmer" => Some((0x1780, 0x17FF)),
        "mongolian" => Some((0x1800, 0x18AF)),
        "limbu" => Some((0x1900, 0x194F)),
        "taile" => Some((0x1950, 0x197F)),
        "newtailue" => Some((0x1980, 0x19DF)),
        "khmersymbols" => Some((0x19E0, 0x19FF)),
        "buginese" => Some((0x1A00, 0x1A1F)),
        "balinese" => Some((0x1B00, 0x1B7F)),
        "sundanese" => Some((0x1B80, 0x1BBF)),
        "lepcha" => Some((0x1C00, 0x1C4F)),
        "olchiki" => Some((0x1C50, 0x1C7F)),
        "phoneticextensions" => Some((0x1D00, 0x1D7F)),
        "phoneticextensionssupplement" => Some((0x1D80, 0x1DBF)),
        "combiningdiacriticalmarkssupplement" => Some((0x1DC0, 0x1DFF)),
        "latinextendedadditional" => Some((0x1E00, 0x1EFF)),
        "greekextended" => Some((0x1F00, 0x1FFF)),
        "generalpunctuation" => Some((0x2000, 0x206F)),
        "superscriptsandsubscripts" => Some((0x2070, 0x209F)),
        "currencysymbols" => Some((0x20A0, 0x20CF)),
        "combiningdiacriticalmarksforsymbols" | "combiningmarksforsymbols" => {
            Some((0x20D0, 0x20FF))
        }
        "letterlikesymbols" => Some((0x2100, 0x214F)),
        "numberforms" => Some((0x2150, 0x218F)),
        "arrows" => Some((0x2190, 0x21FF)),
        "mathematicaloperators" => Some((0x2200, 0x22FF)),
        "miscellaneoustechnical" => Some((0x2300, 0x23FF)),
        "controlpictures" => Some((0x2400, 0x243F)),
        "opticalcharacterrecognition" => Some((0x2440, 0x245F)),
        "enclosedalphanumerics" => Some((0x2460, 0x24FF)),
        "boxdrawing" => Some((0x2500, 0x257F)),
        "blockelements" => Some((0x2580, 0x259F)),
        "geometricshapes" => Some((0x25A0, 0x25FF)),
        "miscellaneoussymbols" => Some((0x2600, 0x26FF)),
        "dingbats" => Some((0x2700, 0x27BF)),
        "miscellaneousmathematicalsymbolsa" => Some((0x27C0, 0x27EF)),
        "supplementalarrowsa" => Some((0x27F0, 0x27FF)),
        "braillepatterns" => Some((0x2800, 0x28FF)),
        "supplementalarrowsb" => Some((0x2900, 0x297F)),
        "miscellaneousmathematicalsymbolsb" => Some((0x2980, 0x29FF)),
        "supplementalmathematicaloperators" => Some((0x2A00, 0x2AFF)),
        "miscellaneoussymbolsandarrows" => Some((0x2B00, 0x2BFF)),
        "cjkradicalssupplement" => Some((0x2E80, 0x2EFF)),
        "kangxiradicals" => Some((0x2F00, 0x2FDF)),
        "ideographicdescriptioncharacters" => Some((0x2FF0, 0x2FFF)),
        "cjksymbolsandpunctuation" => Some((0x3000, 0x303F)),
        "hiragana" => Some((0x3040, 0x309F)),
        "katakana" => Some((0x30A0, 0x30FF)),
        "bopomofo" => Some((0x3100, 0x312F)),
        "hangulcompatibilityjamo" => Some((0x3130, 0x318F)),
        "kanbun" => Some((0x3190, 0x319F)),
        "bopomofoextended" => Some((0x31A0, 0x31BF)),
        "katakanaphoneticextensions" => Some((0x31F0, 0x31FF)),
        "enclosedcjklettersandmonths" => Some((0x3200, 0x32FF)),
        "cjkcompatibility" => Some((0x3300, 0x33FF)),
        "cjkunifiedideographsextensiona" => Some((0x3400, 0x4DBF)),
        "yijinghexagramsymbols" => Some((0x4DC0, 0x4DFF)),
        "cjkunifiedideographs" => Some((0x4E00, 0x9FFF)),
        "yisyllables" => Some((0xA000, 0xA48F)),
        "yiradicals" => Some((0xA490, 0xA4CF)),
        "hangulsyllables" => Some((0xAC00, 0xD7AF)),
        "highsurrogates" => Some((0xD800, 0xDB7F)),
        "highprivateusearea"
        | "highprivateuseareausesurrogates"
        | "highprivateuserogatessurrogates"
        | "highprivateuseareauses"
        | "highprivateuseareausesurrogate"
        | "highprivateuseareause" => Some((0xDB80, 0xDBFF)),
        "lowsurrogates" => Some((0xDC00, 0xDFFF)),
        "privateusearea" => Some((0xE000, 0xF8FF)),
        "cjkcompatibilityideographs" => Some((0xF900, 0xFAFF)),
        "alphabeticpresentationforms" => Some((0xFB00, 0xFB4F)),
        "arabicpresentationformsa" => Some((0xFB50, 0xFDFF)),
        "variationselectors" => Some((0xFE00, 0xFE0F)),
        "combininghalfmarks" => Some((0xFE20, 0xFE2F)),
        "cjkcompatibilityforms" => Some((0xFE30, 0xFE4F)),
        "smallformvariants" => Some((0xFE50, 0xFE6F)),
        "arabicpresentationformsb" => Some((0xFE70, 0xFEFF)),
        "halfwidthandfullwidthforms" => Some((0xFF00, 0xFFEF)),
        "specials" => Some((0xFFF0, 0xFFFF)),
        "linearbsyllabary" => Some((0x10000, 0x1007F)),
        "linearbideograms" => Some((0x10080, 0x100FF)),
        "aegeannumbers" => Some((0x10100, 0x1013F)),
        "olditalic" => Some((0x10300, 0x1032F)),
        "gothic" => Some((0x10330, 0x1034F)),
        "ugaritic" => Some((0x10380, 0x1039F)),
        "deseret" => Some((0x10400, 0x1044F)),
        "shavian" => Some((0x10450, 0x1047F)),
        "osmanya" => Some((0x10480, 0x104AF)),
        "cypriotsyllabary" => Some((0x10800, 0x1083F)),
        "byzantinemusicalsymbols" => Some((0x1D000, 0x1D0FF)),
        "musicalsymbols" => Some((0x1D100, 0x1D1FF)),
        "taixuanjingsymbols" => Some((0x1D300, 0x1D35F)),
        "mathematicalalphanumericsymbols" => Some((0x1D400, 0x1D7FF)),
        "cjkunifiedideographsextensionb" => Some((0x20000, 0x2A6DF)),
        "cjkcompatibilityideographssupplement" => Some((0x2F800, 0x2FA1F)),
        "tags" => Some((0xE0000, 0xE007F)),
        "variationselectorssupplement" => Some((0xE0100, 0xE01EF)),
        "supplementaryprivateuseareaa" => Some((0xF0000, 0xFFFFF)),
        "supplementaryprivateuseareab" => Some((0x100000, 0x10FFFF)),
        _ => None,
    }
}

#[derive(Debug, Clone)]
struct ClassDef {
    parents: Vec<String>,
    attributes: Vec<(String, bool, Option<Expr>)>, // (name, is_public, default)
    methods: HashMap<String, Vec<MethodDef>>,      // name -> overloads
    mro: Vec<String>,
}

#[derive(Debug, Clone)]
struct RoleDef {
    attributes: Vec<(String, bool, Option<Expr>)>,
    methods: HashMap<String, Vec<MethodDef>>,
}

#[derive(Debug, Clone)]
struct SubsetDef {
    base: String,
    predicate: Expr,
}

#[derive(Debug, Clone)]
struct MethodDef {
    params: Vec<String>,
    param_defs: Vec<ParamDef>,
    body: Vec<Stmt>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum IoHandleTarget {
    Stdout,
    Stderr,
    Stdin,
    ArgFiles,
    File,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum IoHandleMode {
    Read,
    Write,
    Append,
    ReadWrite,
}

#[derive(Debug)]
struct IoHandleState {
    target: IoHandleTarget,
    mode: IoHandleMode,
    path: Option<String>,
    encoding: String,
    file: Option<fs::File>,
    closed: bool,
}

#[derive(Clone)]
struct RegexPattern {
    tokens: Vec<RegexToken>,
    anchor_start: bool,
    anchor_end: bool,
}

#[derive(Clone)]
struct RegexToken {
    atom: RegexAtom,
    quant: RegexQuant,
}

#[derive(Clone)]
enum RegexAtom {
    Literal(char),
    Named(String),
    Any,
    CharClass(CharClass),
    Digit,
    Word,
    Space,
    Newline,
    NotNewline,
    Group(RegexPattern),
    Alternation(Vec<RegexPattern>),
    ZeroWidth,
    UnicodeProp { name: String, negated: bool },
    UnicodePropAssert { name: String, negated: bool }, // zero-width assertion
}

#[derive(Clone)]
enum RegexQuant {
    One,
    ZeroOrMore,
    OneOrMore,
    ZeroOrOne,
}

#[derive(Clone)]
struct CharClass {
    negated: bool,
    items: Vec<ClassItem>,
}

#[derive(Clone)]
enum ClassItem {
    Range(char, char),
    Char(char),
    Digit,
    Word,
    Space,
}

pub struct Interpreter {
    env: HashMap<String, Value>,
    output: String,
    stderr_output: String,
    test_state: Option<TestState>,
    halted: bool,
    bailed_out: bool,
    forbid_skip_all: bool,
    loose_ok: bool,
    functions: HashMap<String, FunctionDef>,
    token_defs: HashMap<String, Vec<FunctionDef>>,
    lib_paths: Vec<String>,
    handles: HashMap<usize, IoHandleState>,
    next_handle_id: usize,
    program_path: Option<String>,
    current_package: String,
    routine_stack: Vec<(String, String)>,
    block_stack: Vec<Value>,
    doc_comments: HashMap<String, String>,
    when_matched: bool,
    gather_items: Vec<Vec<Value>>,
    enum_types: HashMap<String, Vec<(String, i64)>>,
    classes: HashMap<String, ClassDef>,
    roles: HashMap<String, RoleDef>,
    subsets: HashMap<String, SubsetDef>,
    proto_subs: HashSet<String>,
    proto_tokens: HashSet<String>,
    end_phasers: Vec<(Vec<Stmt>, HashMap<String, Value>)>,
    chroot_root: Option<PathBuf>,
}

pub(crate) struct SubtestContext {
    parent_test_state: Option<TestState>,
    parent_output: String,
    parent_halted: bool,
}

impl Default for Interpreter {
    fn default() -> Self {
        Self::new()
    }
}

impl Interpreter {
    pub fn new() -> Self {
        let mut env = HashMap::new();
        env.insert("*PID".to_string(), Value::Int(std::process::id() as i64));
        env.insert("@*ARGS".to_string(), Value::Array(Vec::new()));
        let mut classes = HashMap::new();
        classes.insert(
            "Promise".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                mro: vec!["Promise".to_string()],
            },
        );
        classes.insert(
            "Channel".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                mro: vec!["Channel".to_string()],
            },
        );
        classes.insert(
            "Supply".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                mro: vec!["Supply".to_string()],
            },
        );
        classes.insert(
            "Proc::Async".to_string(),
            ClassDef {
                parents: Vec::new(),
                attributes: Vec::new(),
                methods: HashMap::new(),
                mro: vec!["Proc::Async".to_string()],
            },
        );
        let mut interpreter = Self {
            env,
            output: String::new(),
            stderr_output: String::new(),
            test_state: None,
            halted: false,
            bailed_out: false,
            forbid_skip_all: false,
            loose_ok: false,
            functions: HashMap::new(),
            token_defs: HashMap::new(),
            lib_paths: Vec::new(),
            handles: HashMap::new(),
            next_handle_id: 1,
            program_path: None,
            current_package: "GLOBAL".to_string(),
            routine_stack: Vec::new(),
            block_stack: Vec::new(),
            doc_comments: HashMap::new(),
            when_matched: false,
            gather_items: Vec::new(),
            enum_types: HashMap::new(),
            classes,
            roles: HashMap::new(),
            subsets: HashMap::new(),
            proto_subs: HashSet::new(),
            proto_tokens: HashSet::new(),
            end_phasers: Vec::new(),
            chroot_root: None,
        };
        interpreter.init_io_environment();
        interpreter.init_order_enum();
        interpreter.env.insert("Any".to_string(), Value::Nil);
        interpreter
    }

    pub fn set_pid(&mut self, pid: i64) {
        self.env.insert("*PID".to_string(), Value::Int(pid));
    }

    pub fn set_program_path(&mut self, path: &str) {
        self.program_path = Some(path.to_string());
        self.env
            .insert("*PROGRAM".to_string(), self.make_io_path_instance(path));
        self.env
            .insert("*PROGRAM-NAME".to_string(), Value::Str(path.to_string()));
    }

    pub fn set_args(&mut self, args: Vec<Value>) {
        self.env.insert("@*ARGS".to_string(), Value::Array(args));
    }

    pub(crate) fn add_lib_path(&mut self, path: String) {
        if !path.is_empty() {
            self.lib_paths.push(path);
        }
    }

    pub(crate) fn use_module(&mut self, module: &str) -> Result<(), RuntimeError> {
        self.load_module(module)
    }

    pub fn output(&self) -> &str {
        &self.output
    }

    pub(crate) fn is_halted(&self) -> bool {
        self.halted
    }

    pub(crate) fn env(&self) -> &HashMap<String, Value> {
        &self.env
    }

    pub(crate) fn has_class(&self, name: &str) -> bool {
        self.classes.contains_key(name)
    }

    pub(crate) fn has_function(&self, name: &str) -> bool {
        let fq = format!("{}::{}", self.current_package, name);
        self.functions.contains_key(&fq) || self.functions.contains_key(name)
    }

    pub(crate) fn register_sub_decl(
        &mut self,
        name: &str,
        params: &[String],
        param_defs: &[ParamDef],
        body: &[Stmt],
        multi: bool,
    ) {
        let def = FunctionDef {
            package: self.current_package.clone(),
            name: name.to_string(),
            params: params.to_vec(),
            param_defs: param_defs.to_vec(),
            body: body.to_vec(),
        };
        if multi {
            let arity = param_defs.iter().filter(|p| !p.slurpy && !p.named).count();
            let type_sig: Vec<&str> = param_defs
                .iter()
                .filter(|p| !p.slurpy && !p.named)
                .map(|p| p.type_constraint.as_deref().unwrap_or("Any"))
                .collect();
            let has_types = type_sig.iter().any(|t| *t != "Any");
            if has_types {
                let typed_fq = format!(
                    "{}::{}/{}:{}",
                    self.current_package,
                    name,
                    arity,
                    type_sig.join(",")
                );
                self.functions.insert(typed_fq, def.clone());
            }
            let fq = format!("{}::{}/{}", self.current_package, name, arity);
            if !has_types {
                self.functions.insert(fq, def);
            } else {
                self.functions.entry(fq).or_insert(def);
            }
        } else {
            let fq = format!("{}::{}", self.current_package, name);
            self.functions.insert(fq, def);
        }
    }

    pub(crate) fn register_token_decl(
        &mut self,
        name: &str,
        params: &[String],
        param_defs: &[ParamDef],
        body: &[Stmt],
        multi: bool,
    ) {
        let def = FunctionDef {
            package: self.current_package.clone(),
            name: name.to_string(),
            params: params.to_vec(),
            param_defs: param_defs.to_vec(),
            body: body.to_vec(),
        };
        self.insert_token_def(name, def, multi);
    }

    pub(crate) fn register_proto_decl(&mut self, name: &str) {
        let key = format!("{}::{}", self.current_package, name);
        self.proto_subs.insert(key);
    }

    pub(crate) fn register_proto_token_decl(&mut self, name: &str) {
        let key = format!("{}::{}", self.current_package, name);
        self.proto_tokens.insert(key);
    }

    pub(crate) fn register_enum_decl(
        &mut self,
        name: &str,
        variants: &[(String, Option<Expr>)],
    ) -> Result<(), RuntimeError> {
        let mut enum_variants = Vec::new();
        let mut next_value: i64 = 0;
        for (key, value_expr) in variants {
            let val = if let Some(expr) = value_expr {
                let v = self.eval_expr(expr)?;
                match v {
                    Value::Int(i) => i,
                    _ => next_value,
                }
            } else {
                next_value
            };
            enum_variants.push((key.clone(), val));
            next_value = val + 1;
        }
        self.enum_types
            .insert(name.to_string(), enum_variants.clone());
        self.env
            .insert(name.to_string(), Value::Str(name.to_string()));
        for (index, (key, val)) in enum_variants.iter().enumerate() {
            let enum_val = Value::Enum {
                enum_type: name.to_string(),
                key: key.clone(),
                value: *val,
                index,
            };
            self.env
                .insert(format!("{}::{}", name, key), enum_val.clone());
            self.env.insert(key.clone(), enum_val);
        }
        Ok(())
    }

    pub(crate) fn register_class_decl(
        &mut self,
        name: &str,
        parents: &[String],
        body: &[Stmt],
    ) -> Result<(), RuntimeError> {
        let mut class_def = ClassDef {
            parents: parents.to_vec(),
            attributes: Vec::new(),
            methods: HashMap::new(),
            mro: Vec::new(),
        };
        for stmt in body {
            match stmt {
                Stmt::HasDecl {
                    name: attr_name,
                    is_public,
                    default,
                } => {
                    class_def
                        .attributes
                        .push((attr_name.clone(), *is_public, default.clone()));
                }
                Stmt::MethodDecl {
                    name: method_name,
                    params,
                    param_defs,
                    body: method_body,
                    multi,
                } => {
                    let def = MethodDef {
                        params: params.clone(),
                        param_defs: param_defs.clone(),
                        body: method_body.clone(),
                    };
                    if *multi {
                        class_def
                            .methods
                            .entry(method_name.clone())
                            .or_default()
                            .push(def);
                    } else {
                        class_def.methods.insert(method_name.clone(), vec![def]);
                    }
                }
                Stmt::DoesDecl { name: role_name } => {
                    let role =
                        self.roles.get(role_name).cloned().ok_or_else(|| {
                            RuntimeError::new(format!("Unknown role: {}", role_name))
                        })?;
                    for attr in &role.attributes {
                        if !class_def.attributes.iter().any(|(n, _, _)| n == &attr.0) {
                            class_def.attributes.push(attr.clone());
                        }
                    }
                    for (mname, overloads) in role.methods {
                        class_def
                            .methods
                            .entry(mname)
                            .or_default()
                            .extend(overloads);
                    }
                }
                _ => {
                    self.exec_stmt(stmt)?;
                }
            }
        }
        self.classes.insert(name.to_string(), class_def);
        let mut stack = Vec::new();
        let _ = self.compute_class_mro(name, &mut stack)?;
        Ok(())
    }

    pub(crate) fn register_role_decl(
        &mut self,
        name: &str,
        body: &[Stmt],
    ) -> Result<(), RuntimeError> {
        let mut role_def = RoleDef {
            attributes: Vec::new(),
            methods: HashMap::new(),
        };
        for stmt in body {
            match stmt {
                Stmt::HasDecl {
                    name: attr_name,
                    is_public,
                    default,
                } => {
                    role_def
                        .attributes
                        .push((attr_name.clone(), *is_public, default.clone()));
                }
                Stmt::MethodDecl {
                    name: method_name,
                    params,
                    param_defs,
                    body: method_body,
                    multi,
                } => {
                    let def = MethodDef {
                        params: params.clone(),
                        param_defs: param_defs.clone(),
                        body: method_body.clone(),
                    };
                    if *multi {
                        role_def
                            .methods
                            .entry(method_name.clone())
                            .or_default()
                            .push(def);
                    } else {
                        role_def.methods.insert(method_name.clone(), vec![def]);
                    }
                }
                _ => {
                    self.exec_stmt(stmt)?;
                }
            }
        }
        self.roles.insert(name.to_string(), role_def);
        Ok(())
    }

    pub(crate) fn register_subset_decl(&mut self, name: &str, base: &str, predicate: &Expr) {
        self.subsets.insert(
            name.to_string(),
            SubsetDef {
                base: base.to_string(),
                predicate: predicate.clone(),
            },
        );
    }

    pub(crate) fn run_subtest_stmt(
        &mut self,
        name: &Expr,
        body: &[Stmt],
    ) -> Result<(), RuntimeError> {
        let name_value = self.eval_expr(name)?;
        let label = name_value.to_string_value();
        let ctx = self.begin_subtest();
        let run_result = self.run_block(body);
        self.finish_subtest(ctx, &label, run_result)
    }

    pub(crate) fn begin_subtest(&mut self) -> SubtestContext {
        let parent_test_state = self.test_state.take();
        let parent_output = std::mem::take(&mut self.output);
        let parent_halted = self.halted;
        self.test_state = Some(TestState::new());
        self.halted = false;
        SubtestContext {
            parent_test_state,
            parent_output,
            parent_halted,
        }
    }

    pub(crate) fn finish_subtest(
        &mut self,
        ctx: SubtestContext,
        label: &str,
        run_result: Result<(), RuntimeError>,
    ) -> Result<(), RuntimeError> {
        let subtest_output = std::mem::take(&mut self.output);
        let subtest_state = self.test_state.take();
        let subtest_failed = subtest_state.as_ref().map(|s| s.failed).unwrap_or(0);

        self.test_state = ctx.parent_test_state;
        self.output = ctx.parent_output;
        self.halted = ctx.parent_halted;

        for line in subtest_output.lines() {
            self.output.push_str("    ");
            self.output.push_str(line);
            self.output.push('\n');
        }

        let ok = run_result.is_ok() && subtest_failed == 0;
        self.test_ok(ok, label, false)?;
        Ok(())
    }

    pub(crate) fn run_whenever_stmt(
        &mut self,
        supply: &Expr,
        param: &Option<String>,
        body: &[Stmt],
    ) -> Result<(), RuntimeError> {
        let supply_val = self.eval_expr(supply)?;
        let target_var = if let Expr::Var(name) = supply {
            Some(name.as_str())
        } else {
            None
        };
        self.run_whenever_with_value(supply_val, target_var, param, body)
    }

    pub(crate) fn run_whenever_with_value(
        &mut self,
        supply_val: Value,
        target_var: Option<&str>,
        param: &Option<String>,
        body: &[Stmt],
    ) -> Result<(), RuntimeError> {
        if let Value::Instance {
            class_name,
            attributes,
            ..
        } = supply_val
            && class_name == "Supply"
        {
            let tap_sub = Value::Sub {
                package: self.current_package.clone(),
                name: String::new(),
                params: param.iter().cloned().collect(),
                body: body.to_vec(),
                env: self.env.clone(),
                id: next_instance_id(),
            };
            let mut attrs = attributes.clone();
            if let Some(Value::Array(items)) = attrs.get_mut("taps") {
                items.push(tap_sub.clone());
            } else {
                attrs.insert("taps".to_string(), Value::Array(vec![tap_sub.clone()]));
            }
            if let Some(Value::Array(values)) = attrs.get("values") {
                for v in values {
                    let _ = self.call_sub_value(tap_sub.clone(), vec![v.clone()], true);
                }
            }
            let updated = Value::make_instance(class_name, attrs);
            if let Some(name) = target_var {
                self.env.insert(name.to_string(), updated);
            }
        }
        Ok(())
    }

    pub(crate) fn run_package_stmt(
        &mut self,
        name: &str,
        body: &[Stmt],
    ) -> Result<(), RuntimeError> {
        let saved = self.current_package.clone();
        self.current_package = name.to_string();
        self.run_block(body)?;
        self.current_package = saved;
        Ok(())
    }

    pub(crate) fn run_react_stmt(&mut self, body: &[Stmt]) -> Result<(), RuntimeError> {
        self.run_block(body)
    }

    pub(crate) fn eval_do_block_expr(
        &mut self,
        body: &[Stmt],
        label: &Option<String>,
    ) -> Result<Value, RuntimeError> {
        loop {
            match self.eval_block_value(body) {
                Ok(v) => return Ok(v),
                Err(e)
                    if e.is_redo
                        && (e.label.is_none() || e.label.as_deref() == label.as_deref()) =>
                {
                    continue;
                }
                Err(e)
                    if e.is_next
                        && (e.label.is_none() || e.label.as_deref() == label.as_deref()) =>
                {
                    return Ok(Value::Array(vec![]));
                }
                Err(e)
                    if e.is_last
                        && (e.label.is_none() || e.label.as_deref() == label.as_deref()) =>
                {
                    return Ok(e.return_value.unwrap_or(Value::Array(vec![])));
                }
                Err(e) => return Err(e),
            }
        }
    }

    pub(crate) fn eval_do_stmt_expr(&mut self, stmt: &Stmt) -> Result<Value, RuntimeError> {
        match stmt {
            Stmt::If {
                cond,
                then_branch,
                else_branch,
            } => self.eval_do_if(cond, then_branch, else_branch),
            Stmt::Given { topic, body } => self.eval_given_value(topic, body),
            _ => {
                self.exec_stmt(stmt)?;
                Ok(Value::Nil)
            }
        }
    }

    pub(crate) fn eval_gather_expr(&self, body: &[Stmt]) -> Value {
        let list = LazyList {
            body: body.to_vec(),
            env: self.env.clone(),
            cache: std::cell::RefCell::new(None),
        };
        Value::LazyList(std::rc::Rc::new(list))
    }

    pub(crate) fn eval_call_on_expr(
        &mut self,
        target: &Expr,
        args: &[Expr],
    ) -> Result<Value, RuntimeError> {
        // For CodeVar targets, check if it's a variable first, otherwise
        // use proper multi-dispatch via call_function
        if let Expr::CodeVar(fname) = target {
            let var_key = format!("&{}", fname);
            if !self.env.contains_key(&var_key) {
                // Not a variable - call by name with multi-dispatch
                let mut eval_args = Vec::new();
                for arg in args {
                    eval_args.push(self.eval_expr(arg)?);
                }
                return self.call_function(fname, eval_args);
            }
        }
        let target_val = self.eval_expr(target)?;
        let mut eval_args = Vec::with_capacity(args.len());
        for arg in args {
            eval_args.push(self.eval_expr(arg)?);
        }
        self.eval_call_on_value(target_val, eval_args)
    }

    pub(crate) fn eval_call_on_value(
        &mut self,
        target_val: Value,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        if let Value::Sub {
            package,
            name,
            params,
            body,
            env,
            ..
        } = target_val
        {
            let saved_env = self.env.clone();
            let mut new_env = saved_env.clone();
            for (k, v) in env {
                if matches!(new_env.get(&k), Some(Value::Array(_))) && matches!(v, Value::Array(_))
                {
                    continue;
                }
                new_env.insert(k, v);
            }
            // Bind named params
            for (i, pname) in params.iter().enumerate() {
                if let Some(value) = args.get(i) {
                    new_env.insert(pname.clone(), value.clone());
                }
            }
            // Bind placeholder variables ($^a, $^b, ...)
            let placeholders = collect_placeholders(&body);
            if !placeholders.is_empty() {
                for (i, ph) in placeholders.iter().enumerate() {
                    if let Some(val) = args.get(i) {
                        new_env.insert(ph.clone(), val.clone());
                    }
                }
            }
            let block_sub = Value::Sub {
                package: package.clone(),
                name: name.clone(),
                params: params.clone(),
                body: body.clone(),
                env: new_env.clone(),
                id: next_instance_id(),
            };
            self.env = new_env;
            self.routine_stack.push((package.clone(), name.clone()));
            self.block_stack.push(block_sub);
            let result = self.eval_block_value(&body);
            self.block_stack.pop();
            self.routine_stack.pop();
            let mut merged = saved_env;
            for (k, v) in self.env.iter() {
                if matches!(v, Value::Array(_)) {
                    merged.insert(k.clone(), v.clone());
                }
            }
            self.env = merged;
            return match result {
                Err(e) if e.return_value.is_some() => Ok(e.return_value.unwrap()),
                other => other,
            };
        }
        if matches!(target_val, Value::Routine { .. }) {
            return self.call_sub_value(target_val, args, false);
        }
        Ok(Value::Nil)
    }

    pub(crate) fn eval_anon_sub_expr(&self, body: &[Stmt]) -> Value {
        Value::Sub {
            package: self.current_package.clone(),
            name: String::new(),
            params: vec![],
            body: body.to_vec(),
            env: self.env.clone(),
            id: next_instance_id(),
        }
    }

    pub(crate) fn eval_anon_sub_params_expr(&self, params: &[String], body: &[Stmt]) -> Value {
        Value::Sub {
            package: self.current_package.clone(),
            name: String::new(),
            params: params.to_vec(),
            body: body.to_vec(),
            env: self.env.clone(),
            id: next_instance_id(),
        }
    }

    pub(crate) fn eval_lambda_expr(&self, param: &str, body: &[Stmt]) -> Value {
        Value::Sub {
            package: self.current_package.clone(),
            name: String::new(),
            params: if param.is_empty() {
                vec![]
            } else {
                vec![param.to_string()]
            },
            body: body.to_vec(),
            env: self.env.clone(),
            id: next_instance_id(),
        }
    }

    pub(crate) fn eval_index_assign_expr(
        &mut self,
        target: &Expr,
        index: &Expr,
        value: &Expr,
    ) -> Result<Value, RuntimeError> {
        let val = self.eval_expr(value)?;
        let idx = self.eval_expr(index)?;

        // Handle chained index assignment: %h<foo><bar> = "baz"
        if let Expr::Index {
            target: inner_target,
            index: inner_index,
        } = target
        {
            let inner_var = match inner_target.as_ref() {
                Expr::HashVar(name) => format!("%{}", name),
                Expr::ArrayVar(name) => format!("@{}", name),
                Expr::Var(name) => name.clone(),
                _ => return Err(RuntimeError::new("Invalid assignment target")),
            };
            let inner_key = self.eval_expr(inner_index)?.to_string_value();
            let outer_key = idx.to_string_value();
            // Ensure the outer hash exists
            if !self.env.contains_key(&inner_var) {
                self.env.insert(
                    inner_var.clone(),
                    Value::Hash(std::collections::HashMap::new()),
                );
            }
            if let Some(Value::Hash(outer_hash)) = self.env.get_mut(&inner_var) {
                // Get or create inner hash
                let inner_hash = outer_hash
                    .entry(inner_key)
                    .or_insert_with(|| Value::Hash(std::collections::HashMap::new()));
                if let Value::Hash(h) = inner_hash {
                    h.insert(outer_key, val.clone());
                }
            }
            return Ok(val);
        }

        // Get the variable name from the target expression
        let var_name = match target {
            Expr::HashVar(name) => format!("%{}", name),
            Expr::ArrayVar(name) => format!("@{}", name),
            Expr::Var(name) => name.clone(),
            _ => {
                return Err(RuntimeError::new("Invalid assignment target"));
            }
        };
        match &idx {
            Value::Array(keys) => {
                // Slice assignment: %hash{"one","three"} = (5, 10)
                let vals = match &val {
                    Value::Array(v) => v.clone(),
                    _ => vec![val.clone()],
                };
                if let Some(Value::Hash(hash)) = self.env.get_mut(&var_name) {
                    for (i, key) in keys.iter().enumerate() {
                        let k = key.to_string_value();
                        let v = vals.get(i).cloned().unwrap_or(Value::Nil);
                        hash.insert(k, v);
                    }
                }
                Ok(val)
            }
            _ => {
                let key = idx.to_string_value();
                if let Some(Value::Hash(hash)) = self.env.get_mut(&var_name) {
                    hash.insert(key, val.clone());
                } else {
                    // Auto-vivify: create a new hash
                    let mut hash = std::collections::HashMap::new();
                    hash.insert(key, val.clone());
                    self.env.insert(var_name, Value::Hash(hash));
                }
                Ok(val)
            }
        }
    }

    pub(crate) fn eval_unary_expr(
        &mut self,
        op: &TokenKind,
        expr: &Expr,
    ) -> Result<Value, RuntimeError> {
        match op {
            TokenKind::PlusPlus => {
                if let Expr::Var(name) = expr {
                    let val = self.env.get(name).cloned().unwrap_or(Value::Int(0));
                    let new_val = match val {
                        Value::Int(i) => Value::Int(i + 1),
                        Value::Rat(n, d) => make_rat(n + d, d),
                        _ => Value::Int(1),
                    };
                    self.env.insert(name.clone(), new_val.clone());
                    return Ok(new_val);
                }
                Ok(Value::Nil)
            }
            TokenKind::MinusMinus => {
                if let Expr::Var(name) = expr {
                    let val = self.env.get(name).cloned().unwrap_or(Value::Int(0));
                    let new_val = match val {
                        Value::Int(i) => Value::Int(i - 1),
                        Value::Rat(n, d) => make_rat(n - d, d),
                        _ => Value::Int(-1),
                    };
                    self.env.insert(name.clone(), new_val.clone());
                    return Ok(new_val);
                }
                Ok(Value::Nil)
            }
            _ => {
                let value = self.eval_expr(expr)?;
                self.eval_unary_value(op, value)
            }
        }
    }

    pub(crate) fn eval_unary_value(
        &mut self,
        op: &TokenKind,
        value: Value,
    ) -> Result<Value, RuntimeError> {
        match op {
            TokenKind::Plus => match value {
                Value::Int(i) => Ok(Value::Int(i)),
                Value::Bool(b) => Ok(Value::Int(if b { 1 } else { 0 })),
                Value::Array(items) => Ok(Value::Int(items.len() as i64)),
                Value::Str(s) => Ok(Value::Int(s.parse::<i64>().unwrap_or(0))),
                Value::Enum { value, .. } => Ok(Value::Int(value)),
                _ => Ok(Value::Int(0)),
            },
            TokenKind::Minus => match value {
                Value::Int(i) => Ok(Value::Int(-i)),
                Value::Num(f) => Ok(Value::Num(-f)),
                Value::Rat(n, d) => Ok(Value::Rat(-n, d)),
                Value::Complex(r, i) => Ok(Value::Complex(-r, -i)),
                Value::Str(ref s) => {
                    if let Ok(i) = s.trim().parse::<i64>() {
                        Ok(Value::Int(-i))
                    } else if let Ok(f) = s.trim().parse::<f64>() {
                        Ok(Value::Num(-f))
                    } else {
                        Err(RuntimeError::new("Unary - expects numeric"))
                    }
                }
                _ => Err(RuntimeError::new("Unary - expects numeric")),
            },
            TokenKind::Tilde => Ok(Value::Str(value.to_string_value())),
            TokenKind::Bang => Ok(Value::Bool(!value.truthy())),
            TokenKind::Question => Ok(Value::Bool(value.truthy())),
            TokenKind::Caret => {
                let n = match value {
                    Value::Int(i) => i,
                    _ => 0,
                };
                Ok(Value::RangeExcl(0, n))
            }
            TokenKind::Ident(name) if name == "so" => Ok(Value::Bool(value.truthy())),
            _ => Err(RuntimeError::new("Unknown unary operator")),
        }
    }

    pub(crate) fn eval_binary_expr(
        &mut self,
        left: &Expr,
        op: &TokenKind,
        right: &Expr,
    ) -> Result<Value, RuntimeError> {
        // Short-circuit operators
        match op {
            TokenKind::AndAnd => {
                let l = self.eval_expr(left)?;
                if !l.truthy() {
                    return Ok(l);
                }
                return self.eval_expr(right);
            }
            TokenKind::OrOr => {
                let l = self.eval_expr(left)?;
                if l.truthy() {
                    return Ok(l);
                }
                return self.eval_expr(right);
            }
            TokenKind::OrWord => {
                let l = self.eval_expr(left)?;
                if l.truthy() {
                    return Ok(l);
                }
                return self.eval_expr(right);
            }
            TokenKind::SlashSlash | TokenKind::OrElse => {
                let l = self.eval_expr(left)?;
                if !matches!(l, Value::Nil) {
                    return Ok(l);
                }
                return self.eval_expr(right);
            }
            TokenKind::AndThen => {
                let l = self.eval_expr(left)?;
                if matches!(l, Value::Nil) {
                    return Ok(Value::Nil);
                }
                return self.eval_expr(right);
            }
            TokenKind::NotAndThen => {
                let l = self.eval_expr(left)?;
                if matches!(l, Value::Nil) {
                    return self.eval_expr(right);
                }
                return Ok(Value::Nil);
            }
            // Smartmatch: set $_ to LHS before evaluating RHS
            TokenKind::SmartMatch | TokenKind::BangTilde => {
                let l = self.eval_expr(left)?;
                let saved_topic = self.env.get("_").cloned();
                self.env.insert("_".to_string(), l.clone());
                let r = self.eval_expr(right)?;
                if let Some(v) = saved_topic {
                    self.env.insert("_".to_string(), v);
                } else {
                    self.env.remove("_");
                }
                return self.eval_binary(l, op, r);
            }
            _ => {}
        }
        let l = self.eval_expr(left)?;
        let r = self.eval_expr(right)?;
        self.eval_binary(l, op, r)
    }

    pub(crate) fn eval_postfix_expr(
        &mut self,
        op: &TokenKind,
        expr: &Expr,
    ) -> Result<Value, RuntimeError> {
        // Helper closure to compute increment/decrement
        let compute_new = |op: &TokenKind, effective_val: &Value| -> Value {
            match (op, effective_val) {
                (TokenKind::PlusPlus, Value::Int(i)) => Value::Int(i + 1),
                (TokenKind::MinusMinus, Value::Int(i)) => Value::Int(i - 1),
                (TokenKind::PlusPlus, Value::Rat(n, d)) => make_rat(n + d, *d),
                (TokenKind::MinusMinus, Value::Rat(n, d)) => make_rat(n - d, *d),
                (TokenKind::PlusPlus, _) => Value::Int(1),
                (TokenKind::MinusMinus, _) => Value::Int(-1),
                _ => effective_val.clone(),
            }
        };

        if let Expr::Var(name) = expr {
            let val = self.env.get(name).cloned().unwrap_or(Value::Int(0));
            let effective_val = match &val {
                Value::Nil => Value::Int(0),
                other => other.clone(),
            };
            let new_val = compute_new(op, &effective_val);
            self.env.insert(name.clone(), new_val);
            Ok(effective_val)
        } else if let Expr::Index { target, index } = expr {
            // Handle %hash{key}++ and @array[idx]++
            let var_name = match target.as_ref() {
                Expr::HashVar(name) => format!("%{}", name),
                Expr::ArrayVar(name) => format!("@{}", name),
                Expr::Var(name) => name.clone(),
                _ => return Ok(Value::Nil),
            };
            let idx = self.eval_expr(index)?;
            let key = idx.to_string_value();

            // Get current value
            let current = if let Some(container) = self.env.get(&var_name) {
                match container {
                    Value::Hash(h) => h.get(&key).cloned().unwrap_or(Value::Nil),
                    Value::Array(arr) => {
                        if let Ok(i) = key.parse::<usize>() {
                            arr.get(i).cloned().unwrap_or(Value::Nil)
                        } else {
                            Value::Nil
                        }
                    }
                    _ => Value::Nil,
                }
            } else {
                Value::Nil
            };

            let effective_val = match &current {
                Value::Nil => Value::Int(0),
                other => other.clone(),
            };
            let new_val = compute_new(op, &effective_val);

            // Auto-vivify container if needed
            if !self.env.contains_key(&var_name) {
                if var_name.starts_with('%') {
                    self.env.insert(
                        var_name.clone(),
                        Value::Hash(std::collections::HashMap::new()),
                    );
                } else {
                    self.env.insert(var_name.clone(), Value::Array(Vec::new()));
                }
            }

            // Store new value back
            if let Some(container) = self.env.get_mut(&var_name) {
                match container {
                    Value::Hash(h) => {
                        h.insert(key, new_val);
                    }
                    Value::Array(arr) => {
                        if let Ok(i) = idx.to_string_value().parse::<usize>() {
                            while arr.len() <= i {
                                arr.push(Value::Nil);
                            }
                            arr[i] = new_val;
                        }
                    }
                    _ => {}
                }
            }
            Ok(effective_val)
        } else {
            Ok(Value::Nil)
        }
    }

    pub(crate) fn eval_try_expr(
        &mut self,
        body: &[Stmt],
        catch: &Option<Vec<Stmt>>,
    ) -> Result<Value, RuntimeError> {
        // Extract CATCH/CONTROL blocks from body
        let mut main_stmts = Vec::new();
        let mut catch_stmts = catch.clone();
        let mut control_stmts: Option<Vec<Stmt>> = None;
        for stmt in body {
            if let Stmt::Catch(catch_body) = stmt {
                catch_stmts = Some(catch_body.clone());
            } else if let Stmt::Control(control_body) = stmt {
                control_stmts = Some(control_body.clone());
            } else {
                main_stmts.push(stmt.clone());
            }
        }
        match self.eval_block_value(&main_stmts) {
            Ok(v) => Ok(v),
            Err(e) => {
                if (e.is_last || e.is_next || e.is_redo || e.is_proceed || e.is_succeed)
                    && let Some(control_body) = control_stmts
                {
                    let saved_when = self.when_matched;
                    self.when_matched = false;
                    for stmt in &control_body {
                        self.exec_stmt(stmt)?;
                        if self.when_matched || self.halted {
                            break;
                        }
                    }
                    self.when_matched = saved_when;
                    return Ok(Value::Nil);
                }
                if let Some(catch_body) = catch_stmts {
                    // Set $! to an Exception instance
                    let mut exc_attrs = HashMap::new();
                    exc_attrs.insert("message".to_string(), Value::Str(e.message));
                    let err_val = Value::make_instance("Exception".to_string(), exc_attrs);
                    let saved_topic = self.env.get("_").cloned();
                    self.env.insert("!".to_string(), err_val.clone());
                    self.env.insert("_".to_string(), err_val);
                    let saved_when = self.when_matched;
                    self.when_matched = false;
                    for stmt in &catch_body {
                        self.exec_stmt(stmt)?;
                        if self.when_matched || self.halted {
                            break;
                        }
                    }
                    self.when_matched = saved_when;
                    // Restore $_ but leave $! set (try semantics: $! persists after try)
                    if let Some(v) = saved_topic {
                        self.env.insert("_".to_string(), v);
                    } else {
                        self.env.remove("_");
                    }
                    Ok(Value::Nil)
                } else {
                    // try without CATCH: set $! to an Exception instance
                    let mut exc_attrs = HashMap::new();
                    exc_attrs.insert("message".to_string(), Value::Str(e.message));
                    let err_val = Value::make_instance("Exception".to_string(), exc_attrs);
                    self.env.insert("!".to_string(), err_val);
                    Ok(Value::Nil)
                }
            }
        }
    }

    pub(crate) fn eval_block_expr(&mut self, body: &[Stmt]) -> Result<Value, RuntimeError> {
        let placeholders = collect_placeholders(body);
        if !placeholders.is_empty() {
            Ok(Value::Sub {
                package: self.current_package.clone(),
                name: String::new(),
                params: vec![],
                body: body.to_vec(),
                env: self.env.clone(),
                id: next_instance_id(),
            })
        } else {
            self.eval_block_value(body)
        }
    }

    pub(crate) fn run_given_stmt(
        &mut self,
        topic: &Expr,
        body: &[Stmt],
    ) -> Result<(), RuntimeError> {
        let topic_val = self.eval_expr(topic)?;
        let saved_topic = self.env.get("_").cloned();
        self.env.insert("_".to_string(), topic_val);
        let saved_when = self.when_matched;
        self.when_matched = false;
        for stmt in body {
            match self.exec_stmt(stmt) {
                Ok(()) => {}
                Err(e) if e.is_succeed => {
                    self.when_matched = true;
                    break;
                }
                Err(e) => return Err(e),
            }
            if self.when_matched || self.halted {
                break;
            }
        }
        self.when_matched = saved_when;
        if let Some(v) = saved_topic {
            self.env.insert("_".to_string(), v);
        } else {
            self.env.remove("_");
        }
        Ok(())
    }

    pub(crate) fn run_when_stmt(&mut self, cond: &Expr, body: &[Stmt]) -> Result<(), RuntimeError> {
        let topic = self.env.get("_").cloned().unwrap_or(Value::Nil);
        let cond_val = self.eval_expr(cond)?;
        if self.smart_match(&topic, &cond_val) {
            let mut did_proceed = false;
            let mut last_val = Value::Nil;
            for stmt in body {
                match stmt {
                    Stmt::Expr(expr) => match self.eval_expr(expr) {
                        Ok(v) => last_val = v,
                        Err(e) if e.is_proceed => {
                            did_proceed = true;
                            break;
                        }
                        Err(e) if e.is_succeed => {
                            if let Some(v) = e.return_value {
                                last_val = v;
                            }
                            break;
                        }
                        Err(e) => return Err(e),
                    },
                    _ => match self.exec_stmt(stmt) {
                        Err(e) if e.is_proceed => {
                            did_proceed = true;
                            break;
                        }
                        Err(e) if e.is_succeed => {
                            if let Some(v) = e.return_value {
                                last_val = v;
                            }
                            break;
                        }
                        other => {
                            other?;
                        }
                    },
                }
                if self.halted {
                    break;
                }
            }
            if !did_proceed {
                self.when_matched = true;
                let mut sig = RuntimeError::succeed_signal();
                sig.return_value = Some(last_val);
                return Err(sig);
            }
        }
        Ok(())
    }

    pub(crate) fn run_default_stmt(&mut self, body: &[Stmt]) -> Result<(), RuntimeError> {
        for stmt in body {
            self.exec_stmt(stmt)?;
            if self.halted {
                break;
            }
        }
        self.when_matched = true;
        Ok(())
    }

    pub(crate) fn run_while_stmt(
        &mut self,
        cond: &Expr,
        body: &[Stmt],
        label: &Option<String>,
    ) -> Result<(), RuntimeError> {
        let mut iter_idx = 0usize;
        let (enter_ph, leave_ph, first_ph, next_ph, last_ph, body_main) =
            self.split_loop_phasers(body);
        'while_loop: while self.eval_expr(cond)?.truthy() {
            loop {
                let mut should_redo = false;
                let mut control: Option<RuntimeError> = None;
                self.run_block(&enter_ph)?;
                if iter_idx == 0 {
                    self.run_block(&first_ph)?;
                } else {
                    self.run_block(&next_ph)?;
                }
                for stmt in &body_main {
                    match self.exec_stmt(stmt) {
                        Err(e)
                            if e.is_redo
                                && (e.label.is_none()
                                    || e.label.as_deref() == label.as_deref()) =>
                        {
                            should_redo = true;
                            break;
                        }
                        Err(e) => {
                            control = Some(e);
                            break;
                        }
                        Ok(()) => {}
                    }
                }
                if should_redo {
                    continue;
                }
                let leave_res = self.run_block(&leave_ph);
                if let Err(e) = leave_res
                    && control.is_none()
                {
                    return Err(e);
                }
                if let Some(e) = control {
                    if e.is_last {
                        if e.label.is_none() || e.label.as_deref() == label.as_deref() {
                            break 'while_loop;
                        }
                        return Err(e);
                    }
                    if e.is_next {
                        if e.label.is_none() || e.label.as_deref() == label.as_deref() {
                            break;
                        }
                        return Err(e);
                    }
                    if e.is_redo {
                        if e.label.is_none() || e.label.as_deref() == label.as_deref() {
                            continue;
                        }
                        return Err(e);
                    }
                    return Err(e);
                }
                break;
            }
            iter_idx += 1;
        }
        if iter_idx > 0 {
            self.run_block(&last_ph)?;
        }
        Ok(())
    }

    pub(crate) fn run_loop_stmt(
        &mut self,
        init: Option<&Stmt>,
        cond: Option<&Expr>,
        step: Option<&Expr>,
        body: &[Stmt],
        repeat: bool,
        label: &Option<String>,
    ) -> Result<(), RuntimeError> {
        if let Some(init_stmt) = init {
            self.exec_stmt(init_stmt)?;
        }
        let mut first = true;
        let mut iter_idx = 0usize;
        let (enter_ph, leave_ph, first_ph, next_ph, last_ph, body_main) =
            self.split_loop_phasers(body);
        'c_loop: loop {
            if let Some(cond_expr) = cond {
                if repeat && first {
                    first = false;
                } else if !self.eval_expr(cond_expr)?.truthy() {
                    break;
                }
            }
            loop {
                let mut should_redo = false;
                let mut control: Option<RuntimeError> = None;
                self.run_block(&enter_ph)?;
                if iter_idx == 0 {
                    self.run_block(&first_ph)?;
                } else {
                    self.run_block(&next_ph)?;
                }
                for stmt in &body_main {
                    match self.exec_stmt(stmt) {
                        Err(e)
                            if e.is_redo
                                && (e.label.is_none()
                                    || e.label.as_deref() == label.as_deref()) =>
                        {
                            should_redo = true;
                            break;
                        }
                        Err(e) => {
                            control = Some(e);
                            break;
                        }
                        Ok(()) => {}
                    }
                }
                if should_redo {
                    continue;
                }
                let leave_res = self.run_block(&leave_ph);
                if let Err(e) = leave_res
                    && control.is_none()
                {
                    return Err(e);
                }
                if let Some(e) = control {
                    if e.is_last {
                        if e.label.is_none() || e.label.as_deref() == label.as_deref() {
                            break 'c_loop;
                        }
                        return Err(e);
                    }
                    if e.is_next {
                        if e.label.is_none() || e.label.as_deref() == label.as_deref() {
                            break;
                        }
                        return Err(e);
                    }
                    if e.is_redo {
                        if e.label.is_none() || e.label.as_deref() == label.as_deref() {
                            continue;
                        }
                        return Err(e);
                    }
                    return Err(e);
                }
                break;
            }
            if let Some(step_expr) = step {
                self.eval_expr(step_expr)?;
            }
            iter_idx += 1;
        }
        if iter_idx > 0 {
            self.run_block(&last_ph)?;
        }
        Ok(())
    }

    pub(crate) fn run_for_stmt(
        &mut self,
        iterable: &Expr,
        param: &Option<String>,
        params: &[String],
        body: &[Stmt],
        label: &Option<String>,
    ) -> Result<(), RuntimeError> {
        let iterable_val = self.eval_expr(iterable)?;
        let values = self.list_from_value(iterable_val)?;
        let (enter_ph, leave_ph, first_ph, next_ph, last_ph, body_main) =
            self.split_loop_phasers(body);
        let mut iter_idx = 0usize;
        if !params.is_empty() {
            let chunk_size = params.len();
            let mut i = 0;
            'for_loop_multi: while i + chunk_size <= values.len() {
                for (pi, p) in params.iter().enumerate() {
                    self.env.insert(p.clone(), values[i + pi].clone());
                }
                self.env.insert("_".to_string(), values[i].clone());
                loop {
                    let mut should_redo = false;
                    let mut control: Option<RuntimeError> = None;
                    self.run_block(&enter_ph)?;
                    if iter_idx == 0 {
                        self.run_block(&first_ph)?;
                    } else {
                        self.run_block(&next_ph)?;
                    }
                    for stmt in &body_main {
                        match self.exec_stmt(stmt) {
                            Err(e)
                                if e.is_redo
                                    && (e.label.is_none()
                                        || e.label.as_deref() == label.as_deref()) =>
                            {
                                should_redo = true;
                                break;
                            }
                            Err(e) => {
                                control = Some(e);
                                break;
                            }
                            Ok(()) => {}
                        }
                    }
                    if should_redo {
                        continue;
                    }
                    let leave_res = self.run_block(&leave_ph);
                    if let Err(e) = leave_res
                        && control.is_none()
                    {
                        return Err(e);
                    }
                    if let Some(e) = control {
                        if e.is_last {
                            if e.label.is_none() || e.label.as_deref() == label.as_deref() {
                                break 'for_loop_multi;
                            }
                            return Err(e);
                        }
                        if e.is_next {
                            if e.label.is_none() || e.label.as_deref() == label.as_deref() {
                                break;
                            }
                            return Err(e);
                        }
                        if e.is_redo {
                            if e.label.is_none() || e.label.as_deref() == label.as_deref() {
                                continue;
                            }
                            return Err(e);
                        }
                        return Err(e);
                    }
                    break;
                }
                i += chunk_size;
                iter_idx += 1;
            }
            if iter_idx > 0 {
                self.run_block(&last_ph)?;
            }
        } else {
            'for_loop: for value in values {
                self.env.insert("_".to_string(), value.clone());
                if let Some(p) = param {
                    self.env.insert(p.clone(), value);
                }
                loop {
                    let mut should_redo = false;
                    let mut control: Option<RuntimeError> = None;
                    self.run_block(&enter_ph)?;
                    if iter_idx == 0 {
                        self.run_block(&first_ph)?;
                    } else {
                        self.run_block(&next_ph)?;
                    }
                    for stmt in &body_main {
                        match self.exec_stmt(stmt) {
                            Err(e)
                                if e.is_redo
                                    && (e.label.is_none()
                                        || e.label.as_deref() == label.as_deref()) =>
                            {
                                should_redo = true;
                                break;
                            }
                            Err(e) => {
                                control = Some(e);
                                break;
                            }
                            Ok(()) => {}
                        }
                    }
                    if should_redo {
                        continue;
                    }
                    let leave_res = self.run_block(&leave_ph);
                    if let Err(e) = leave_res
                        && control.is_none()
                    {
                        return Err(e);
                    }
                    if let Some(e) = control {
                        if e.is_last {
                            if e.label.is_none() || e.label.as_deref() == label.as_deref() {
                                break 'for_loop;
                            }
                            return Err(e);
                        }
                        if e.is_next {
                            if e.label.is_none() || e.label.as_deref() == label.as_deref() {
                                break;
                            }
                            return Err(e);
                        }
                        if e.is_redo {
                            if e.label.is_none() || e.label.as_deref() == label.as_deref() {
                                continue;
                            }
                            return Err(e);
                        }
                        return Err(e);
                    }
                    break;
                }
                iter_idx += 1;
            }
            if iter_idx > 0 {
                self.run_block(&last_ph)?;
            }
        }
        Ok(())
    }

    pub(crate) fn call_function(
        &mut self,
        name: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        crate::trace::trace_log!("call", "call_function: {} ({} args)", name, args.len());
        if let Some(def) = self.resolve_function_with_types(name, &args) {
            let saved_env = self.env.clone();
            self.bind_function_args_values(&def.param_defs, &def.params, &args)?;
            self.routine_stack
                .push((def.package.clone(), def.name.clone()));
            let result = self.eval_block_value(&def.body);
            self.routine_stack.pop();
            self.env = saved_env;
            return match result {
                Err(e) if e.return_value.is_some() => Ok(e.return_value.unwrap()),
                other => other,
            };
        }
        if self.has_proto(name) {
            return Err(RuntimeError::new(format!(
                "No matching candidates for proto sub: {}",
                name
            )));
        }

        let arg_exprs: Vec<Expr> = args.into_iter().map(Expr::Literal).collect();
        self.eval_expr(&Expr::Call {
            name: name.to_string(),
            args: arg_exprs,
        })
    }

    pub(crate) fn env_mut(&mut self) -> &mut HashMap<String, Value> {
        &mut self.env
    }

    pub(crate) fn when_matched(&self) -> bool {
        self.when_matched
    }

    pub(crate) fn set_when_matched(&mut self, v: bool) {
        self.when_matched = v;
    }

    pub(crate) fn smart_match_values(&mut self, left: &Value, right: &Value) -> bool {
        self.smart_match(left, right)
    }

    pub(crate) fn resolve_code_var(&self, name: &str) -> Value {
        // Check if stored as a variable first (my &f = ...)
        let var_key = format!("&{}", name);
        if let Some(val) = self.env.get(&var_key) {
            return val.clone();
        }
        // Look up as a function reference (including multi subs)
        let def = self.resolve_function(name).or_else(|| {
            let prefix_local = format!("{}::{}/", self.current_package, name);
            let prefix_global = format!("GLOBAL::{}/", name);
            self.functions
                .iter()
                .find(|(k, _)| k.starts_with(&prefix_local) || k.starts_with(&prefix_global))
                .map(|(_, v)| v.clone())
        });
        if let Some(def) = def {
            Value::Sub {
                package: def.package,
                name: def.name,
                params: def.params,
                body: def.body,
                env: self.env.clone(),
                id: next_instance_id(),
            }
        } else if Self::is_builtin_function(name) {
            Value::Routine {
                package: "GLOBAL".to_string(),
                name: name.to_string(),
            }
        } else {
            Value::Nil
        }
    }

    /// Check if a name refers to a built-in function
    fn is_builtin_function(name: &str) -> bool {
        matches!(
            name,
            "defined"
                | "undefine"
                | "say"
                | "print"
                | "put"
                | "note"
                | "die"
                | "warn"
                | "exit"
                | "abs"
                | "sqrt"
                | "floor"
                | "ceiling"
                | "ceil"
                | "round"
                | "exp"
                | "log"
                | "sin"
                | "cos"
                | "tan"
                | "asin"
                | "acos"
                | "atan"
                | "chr"
                | "ord"
                | "chars"
                | "chomp"
                | "chop"
                | "flip"
                | "lc"
                | "uc"
                | "tc"
                | "trim"
                | "elems"
                | "keys"
                | "values"
                | "pairs"
                | "sort"
                | "reverse"
                | "join"
                | "map"
                | "grep"
                | "push"
                | "pop"
                | "shift"
                | "unshift"
                | "splice"
                | "flat"
                | "unique"
                | "squish"
                | "min"
                | "max"
                | "sum"
                | "any"
                | "all"
                | "none"
                | "one"
                | "so"
                | "not"
                | "truncate"
        )
    }

    pub(crate) fn routine_stack_top(&self) -> Option<&(String, String)> {
        self.routine_stack.last()
    }

    pub(crate) fn push_routine(&mut self, package: String, name: String) {
        self.routine_stack.push((package, name));
    }

    pub(crate) fn pop_routine(&mut self) {
        self.routine_stack.pop();
    }

    pub(crate) fn block_stack_top(&self) -> Option<&Value> {
        self.block_stack.last()
    }

    pub(crate) fn regex_find_first_bridge(
        &self,
        pattern: &str,
        text: &str,
    ) -> Option<(usize, usize)> {
        self.regex_find_first(pattern, text)
    }

    pub(crate) fn take_value(&mut self, val: Value) {
        if let Some(items) = self.gather_items.last_mut() {
            items.push(val);
        }
    }

    pub(crate) fn current_package(&self) -> &str {
        &self.current_package
    }

    pub(crate) fn set_current_package(&mut self, pkg: String) {
        self.current_package = pkg;
    }

    pub(crate) fn push_end_phaser(&mut self, body: Vec<Stmt>) {
        let captured_env = self.env.clone();
        self.end_phasers.push((body, captured_env));
    }

    fn init_order_enum(&mut self) {
        let variants = vec![
            ("Less".to_string(), -1i64),
            ("Same".to_string(), 0i64),
            ("More".to_string(), 1i64),
        ];
        self.enum_types
            .insert("Order".to_string(), variants.clone());
        self.env
            .insert("Order".to_string(), Value::Str("Order".to_string()));
        for (index, (key, val)) in variants.iter().enumerate() {
            let enum_val = Value::Enum {
                enum_type: "Order".to_string(),
                key: key.clone(),
                value: *val,
                index,
            };
            self.env.insert(format!("Order::{}", key), enum_val.clone());
        }
    }

    pub(crate) fn make_order(ord: std::cmp::Ordering) -> Value {
        match ord {
            std::cmp::Ordering::Less => Value::Enum {
                enum_type: "Order".to_string(),
                key: "Less".to_string(),
                value: -1,
                index: 0,
            },
            std::cmp::Ordering::Equal => Value::Enum {
                enum_type: "Order".to_string(),
                key: "Same".to_string(),
                value: 0,
                index: 1,
            },
            std::cmp::Ordering::Greater => Value::Enum {
                enum_type: "Order".to_string(),
                key: "More".to_string(),
                value: 1,
                index: 2,
            },
        }
    }

    fn version_from_value(arg: Value) -> Value {
        use crate::value::VersionPart;
        match arg {
            Value::Str(s) => {
                if s.is_empty() {
                    return Value::Version {
                        parts: Vec::new(),
                        plus: false,
                        minus: false,
                    };
                }
                let mut plus = false;
                let mut minus = false;
                let mut raw = s.as_str();
                if let Some(stripped) = raw.strip_suffix('+') {
                    plus = true;
                    raw = stripped;
                } else if let Some(stripped) = raw.strip_suffix('-') {
                    minus = true;
                    raw = stripped;
                }
                let parts: Vec<VersionPart> = raw
                    .split('.')
                    .map(|p| {
                        if p == "*" {
                            VersionPart::Whatever
                        } else {
                            VersionPart::Num(p.parse::<i64>().unwrap_or(0))
                        }
                    })
                    .collect();
                Value::Version { parts, plus, minus }
            }
            // Version.new(*) - Whatever argument (Inf is how * is represented)
            Value::Num(f) if f.is_infinite() && f.is_sign_positive() => Value::Version {
                parts: vec![VersionPart::Whatever],
                plus: false,
                minus: false,
            },
            _ => {
                let s = arg.to_string_value();
                Self::version_from_value(Value::Str(s))
            }
        }
    }

    pub(crate) fn version_cmp_parts(
        a_parts: &[crate::value::VersionPart],
        b_parts: &[crate::value::VersionPart],
    ) -> std::cmp::Ordering {
        use crate::value::VersionPart;
        let max_len = a_parts.len().max(b_parts.len());
        for i in 0..max_len {
            let a = a_parts.get(i).unwrap_or(&VersionPart::Num(0));
            let b = b_parts.get(i).unwrap_or(&VersionPart::Num(0));
            match (a, b) {
                (VersionPart::Num(an), VersionPart::Num(bn)) => match an.cmp(bn) {
                    std::cmp::Ordering::Equal => continue,
                    other => return other,
                },
                _ => continue, // Whatever matches anything
            }
        }
        std::cmp::Ordering::Equal
    }

    fn version_smart_match(
        left: &Value,
        right_parts: &[crate::value::VersionPart],
        right_plus: bool,
        right_minus: bool,
    ) -> bool {
        use crate::value::VersionPart;
        if let Value::Version {
            parts: left_parts, ..
        } = left
        {
            if right_plus {
                // LHS >= RHS (base version without +)
                Self::version_cmp_parts(left_parts, right_parts) != std::cmp::Ordering::Less
            } else if right_minus {
                // LHS <= RHS (base version without -)
                Self::version_cmp_parts(left_parts, right_parts) != std::cmp::Ordering::Greater
            } else {
                // Compare up to the length of the RHS; extra LHS parts are ignored
                let rhs_len = right_parts.len();
                for i in 0..rhs_len {
                    let l = left_parts.get(i).unwrap_or(&VersionPart::Num(0));
                    let r = right_parts.get(i).unwrap_or(&VersionPart::Num(0));
                    match (l, r) {
                        (VersionPart::Whatever, _) | (_, VersionPart::Whatever) => continue,
                        (VersionPart::Num(a), VersionPart::Num(b)) => {
                            if a != b {
                                return false;
                            }
                        }
                    }
                }
                // If RHS is longer than LHS, extra RHS parts must be zero
                if rhs_len > left_parts.len() {
                    for p in &right_parts[left_parts.len()..] {
                        match p {
                            VersionPart::Num(n) if *n != 0 => return false,
                            _ => {}
                        }
                    }
                }
                true
            }
        } else {
            false
        }
    }

    /// Convert a value to Value::Hash for hash variable assignment.
    /// Handles arrays of Pairs, flat key-value lists, and existing hashes.
    pub(crate) fn coerce_to_hash(value: Value) -> Value {
        match value {
            Value::Hash(_) => value,
            Value::Array(items) => {
                let mut map = std::collections::HashMap::new();
                let mut i = 0;
                while i < items.len() {
                    if let Value::Pair(k, v) = &items[i] {
                        map.insert(k.clone(), *v.clone());
                        i += 1;
                    } else {
                        // Flat list: key, value, key, value, ...
                        let key = items[i].to_string_value();
                        let val = if i + 1 < items.len() {
                            items[i + 1].clone()
                        } else {
                            Value::Nil
                        };
                        map.insert(key, val);
                        i += 2;
                    }
                }
                Value::Hash(map)
            }
            Value::Pair(k, v) => {
                let mut map = std::collections::HashMap::new();
                map.insert(k, *v);
                Value::Hash(map)
            }
            Value::Nil => Value::Hash(std::collections::HashMap::new()),
            _ => {
                let mut map = std::collections::HashMap::new();
                map.insert(value.to_string_value(), Value::Nil);
                Value::Hash(map)
            }
        }
    }

    /// Convert a value to Value::Array for array variable assignment.
    pub(crate) fn coerce_to_array(value: Value) -> Value {
        match value {
            Value::Array(_) => value,
            Value::Nil => Value::Array(Vec::new()),
            Value::Range(a, b) => Value::Array((a..=b).map(Value::Int).collect()),
            Value::RangeExcl(a, b) => Value::Array((a..b).map(Value::Int).collect()),
            Value::RangeExclStart(a, b) => Value::Array((a + 1..=b).map(Value::Int).collect()),
            Value::RangeExclBoth(a, b) => Value::Array((a + 1..b).map(Value::Int).collect()),
            Value::Slip(items) => Value::Array(items),
            other => Value::Array(vec![other]),
        }
    }

    fn init_io_environment(&mut self) {
        let stdout = self.create_handle(
            IoHandleTarget::Stdout,
            IoHandleMode::Write,
            Some("STDOUT".to_string()),
        );
        self.env.insert("$*OUT".to_string(), stdout.clone());
        let stderr = self.create_handle(
            IoHandleTarget::Stderr,
            IoHandleMode::Write,
            Some("STDERR".to_string()),
        );
        self.env.insert("$*ERR".to_string(), stderr.clone());
        let stdin = self.create_handle(
            IoHandleTarget::Stdin,
            IoHandleMode::Read,
            Some("STDIN".to_string()),
        );
        self.env.insert("$*IN".to_string(), stdin.clone());
        let argfiles = self.create_handle(
            IoHandleTarget::ArgFiles,
            IoHandleMode::Read,
            Some("$*ARGFILES".to_string()),
        );
        self.env.insert("$*ARGFILES".to_string(), argfiles.clone());
        self.env
            .insert("$*SPEC".to_string(), self.make_io_spec_instance());
        let cwd = env::current_dir().unwrap_or_else(|_| PathBuf::from("."));
        self.env.insert(
            "$*CWD".to_string(),
            Value::Str(cwd.to_string_lossy().to_string()),
        );
        let tmpdir = env::temp_dir();
        self.env.insert(
            "$*TMPDIR".to_string(),
            Value::Str(tmpdir.to_string_lossy().to_string()),
        );
        if let Ok(home) = env::var("HOME") {
            self.env.insert("$*HOME".to_string(), Value::Str(home));
        } else {
            self.env.insert(
                "$*HOME".to_string(),
                Value::Str(cwd.to_string_lossy().to_string()),
            );
        }
        let distro = Self::make_distro_instance();
        self.env.insert("*DISTRO".to_string(), distro.clone());
        self.env.insert("?DISTRO".to_string(), distro);
        let perl = Self::make_perl_instance();
        self.env.insert("*PERL".to_string(), perl.clone());
        self.env.insert("?PERL".to_string(), perl);
    }

    fn create_handle(
        &mut self,
        target: IoHandleTarget,
        mode: IoHandleMode,
        path: Option<String>,
    ) -> Value {
        let id = self.next_handle_id;
        self.next_handle_id += 1;
        let state = IoHandleState {
            target,
            mode,
            path: path.clone(),
            encoding: "utf-8".to_string(),
            file: None,
            closed: false,
        };
        self.handles.insert(id, state);
        self.make_handle_instance(id)
    }

    fn make_handle_instance(&self, handle_id: usize) -> Value {
        let mut attrs = HashMap::new();
        attrs.insert("handle".to_string(), Value::Int(handle_id as i64));
        if let Some(state) = self.handles.get(&handle_id) {
            if let Some(path) = &state.path {
                attrs.insert("path".to_string(), Value::Str(path.clone()));
            }
            attrs.insert(
                "mode".to_string(),
                Value::Str(Self::mode_name(state.mode).to_string()),
            );
        }
        Value::make_instance("IO::Handle".to_string(), attrs)
    }

    fn mode_name(mode: IoHandleMode) -> &'static str {
        match mode {
            IoHandleMode::Read => "r",
            IoHandleMode::Write => "w",
            IoHandleMode::Append => "a",
            IoHandleMode::ReadWrite => "rw",
        }
    }

    fn make_io_spec_instance(&self) -> Value {
        let attrs = HashMap::new();
        Value::make_instance("IO::Spec".to_string(), attrs)
    }

    fn make_distro_instance() -> Value {
        let os = std::env::consts::OS;
        let (name, auth, version_str, desc, release) = match os {
            "macos" => {
                let product_version = Command::new("sw_vers")
                    .arg("-productVersion")
                    .output()
                    .ok()
                    .map(|o| String::from_utf8_lossy(&o.stdout).trim().to_string())
                    .unwrap_or_default();
                let build_version = Command::new("sw_vers")
                    .arg("-buildVersion")
                    .output()
                    .ok()
                    .map(|o| String::from_utf8_lossy(&o.stdout).trim().to_string())
                    .unwrap_or_default();
                let product_name = Command::new("sw_vers")
                    .arg("-productName")
                    .output()
                    .ok()
                    .map(|o| String::from_utf8_lossy(&o.stdout).trim().to_string())
                    .unwrap_or_default();
                let major = product_version.split('.').next().unwrap_or("").to_string();
                let desc_str = if product_name.is_empty() {
                    format!("macOS {}", major)
                } else {
                    // sw_vers returns "macOS" as the product name
                    format!("{} {}", product_name, major)
                };
                (
                    "macos".to_string(),
                    "Apple Inc.".to_string(),
                    product_version,
                    desc_str,
                    build_version,
                )
            }
            "linux" => {
                let kernel_release = Command::new("uname")
                    .arg("-r")
                    .output()
                    .ok()
                    .map(|o| String::from_utf8_lossy(&o.stdout).trim().to_string())
                    .unwrap_or_default();
                let mut distro_desc = String::new();
                if let Ok(content) = std::fs::read_to_string("/etc/os-release") {
                    for line in content.lines() {
                        if let Some(val) = line.strip_prefix("PRETTY_NAME=") {
                            distro_desc = val.trim_matches('"').to_string();
                            break;
                        }
                    }
                }
                if distro_desc.is_empty() {
                    distro_desc = "Linux".to_string();
                }
                (
                    "linux".to_string(),
                    "unknown".to_string(),
                    kernel_release.clone(),
                    distro_desc,
                    kernel_release,
                )
            }
            "windows" => {
                let ver = Command::new("cmd")
                    .args(["/C", "ver"])
                    .output()
                    .ok()
                    .map(|o| String::from_utf8_lossy(&o.stdout).trim().to_string())
                    .unwrap_or_default();
                (
                    "mswin32".to_string(),
                    "Microsoft".to_string(),
                    String::new(),
                    ver.clone(),
                    ver,
                )
            }
            _ => {
                let kernel_release = Command::new("uname")
                    .arg("-r")
                    .output()
                    .ok()
                    .map(|o| String::from_utf8_lossy(&o.stdout).trim().to_string())
                    .unwrap_or_default();
                (
                    os.to_string(),
                    "unknown".to_string(),
                    kernel_release.clone(),
                    os.to_string(),
                    kernel_release,
                )
            }
        };

        // Parse version string into Value::Version
        let version = Self::parse_version_string(&version_str);

        let path_sep = if cfg!(windows) {
            ";".to_string()
        } else {
            ":".to_string()
        };

        let is_win = cfg!(windows);

        let mut attrs = HashMap::new();
        attrs.insert("name".to_string(), Value::Str(name));
        attrs.insert("auth".to_string(), Value::Str(auth));
        attrs.insert("version".to_string(), version);
        attrs.insert(
            "signature".to_string(),
            Value::make_instance("Blob".to_string(), HashMap::new()),
        );
        attrs.insert("desc".to_string(), Value::Str(desc));
        attrs.insert("release".to_string(), Value::Str(release));
        attrs.insert("path-sep".to_string(), Value::Str(path_sep));
        attrs.insert("is-win".to_string(), Value::Bool(is_win));

        Value::make_instance("Distro".to_string(), attrs)
    }

    fn parse_version_string(s: &str) -> Value {
        use crate::value::VersionPart;
        let parts: Vec<VersionPart> = s
            .split('.')
            .filter_map(|p| p.parse::<i64>().ok().map(VersionPart::Num))
            .collect();
        if parts.is_empty() {
            Value::Version {
                parts: vec![VersionPart::Num(0)],
                plus: false,
                minus: false,
            }
        } else {
            Value::Version {
                parts,
                plus: false,
                minus: false,
            }
        }
    }

    fn make_perl_instance() -> Value {
        let mut attrs = HashMap::new();
        attrs.insert(
            "DISTROnames".to_string(),
            Value::Array(vec![
                Value::Str("macos".to_string()),
                Value::Str("linux".to_string()),
                Value::Str("freebsd".to_string()),
                Value::Str("mswin32".to_string()),
                Value::Str("openbsd".to_string()),
                Value::Str("dragonfly".to_string()),
                Value::Str("netbsd".to_string()),
                Value::Str("browser".to_string()),
            ]),
        );
        Value::make_instance("Perl".to_string(), attrs)
    }

    fn handle_id_from_value(value: &Value) -> Option<usize> {
        if let Value::Instance {
            class_name,
            attributes,
            ..
        } = value
            && class_name == "IO::Handle"
            && let Some(Value::Int(id)) = attributes.get("handle")
            && *id >= 0
        {
            return Some(*id as usize);
        }
        None
    }

    fn handle_state_mut(
        &mut self,
        handle_value: &Value,
    ) -> Result<&mut IoHandleState, RuntimeError> {
        let id = Self::handle_id_from_value(handle_value)
            .ok_or_else(|| RuntimeError::new("Expected IO::Handle"))?;
        self.handles
            .get_mut(&id)
            .ok_or_else(|| RuntimeError::new("Invalid IO::Handle"))
    }

    fn write_to_handle_value(
        &mut self,
        handle_value: &Value,
        content: &str,
        newline: bool,
    ) -> Result<(), RuntimeError> {
        let id = Self::handle_id_from_value(handle_value)
            .ok_or_else(|| RuntimeError::new("Expected IO::Handle"))?;
        let state = self
            .handles
            .get_mut(&id)
            .ok_or_else(|| RuntimeError::new("Invalid IO::Handle"))?;
        if state.closed {
            return Err(RuntimeError::new("IO::Handle is closed"));
        }
        let mut payload = String::from(content);
        if newline {
            payload.push('\n');
        }
        match state.target {
            IoHandleTarget::Stdout => {
                self.output.push_str(&payload);
                Ok(())
            }
            IoHandleTarget::Stderr => {
                self.stderr_output.push_str(&payload);
                self.output.push_str(&payload);
                Ok(())
            }
            IoHandleTarget::File => {
                if matches!(state.mode, IoHandleMode::Read) {
                    return Err(RuntimeError::new("Handle not open for writing"));
                }
                if let Some(file) = state.file.as_mut() {
                    file.write_all(payload.as_bytes()).map_err(|err| {
                        RuntimeError::new(format!("Failed to write to file: {}", err))
                    })?;
                    Ok(())
                } else {
                    Err(RuntimeError::new("IO::Handle is not attached to a file"))
                }
            }
            IoHandleTarget::Stdin | IoHandleTarget::ArgFiles => {
                Err(RuntimeError::new("Cannot write to read-only handle"))
            }
        }
    }

    fn close_handle_value(&mut self, handle_value: &Value) -> Result<bool, RuntimeError> {
        let state = self.handle_state_mut(handle_value)?;
        if state.closed {
            return Ok(false);
        }
        state.closed = true;
        state.file = None;
        Ok(true)
    }

    fn read_line_from_handle_value(
        &mut self,
        handle_value: &Value,
    ) -> Result<String, RuntimeError> {
        let state = self.handle_state_mut(handle_value)?;
        if state.closed {
            return Err(RuntimeError::new("IO::Handle is closed"));
        }
        match state.target {
            IoHandleTarget::Stdout | IoHandleTarget::Stderr => {
                Err(RuntimeError::new("Handle not readable"))
            }
            IoHandleTarget::Stdin | IoHandleTarget::ArgFiles => Ok(String::new()),
            IoHandleTarget::File => {
                let file = state
                    .file
                    .as_mut()
                    .ok_or_else(|| RuntimeError::new("IO::Handle is not attached to a file"))?;
                let mut buffer = Vec::new();
                let mut byte = [0u8];
                loop {
                    let n = file
                        .read(&mut byte)
                        .map_err(|err| RuntimeError::new(format!("Failed to read: {}", err)))?;
                    if n == 0 {
                        break;
                    }
                    buffer.push(byte[0]);
                    if byte[0] == b'\n' {
                        break;
                    }
                }
                if buffer.is_empty() {
                    return Ok(String::new());
                }
                Ok(String::from_utf8_lossy(&buffer).to_string())
            }
        }
    }

    fn read_bytes_from_handle_value(
        &mut self,
        handle_value: &Value,
        count: usize,
    ) -> Result<Vec<u8>, RuntimeError> {
        let state = self.handle_state_mut(handle_value)?;
        if state.closed {
            return Err(RuntimeError::new("IO::Handle is closed"));
        }
        match state.target {
            IoHandleTarget::Stdout | IoHandleTarget::Stderr => {
                Err(RuntimeError::new("Handle not readable"))
            }
            IoHandleTarget::Stdin | IoHandleTarget::ArgFiles => Ok(Vec::new()),
            IoHandleTarget::File => {
                let file = state
                    .file
                    .as_mut()
                    .ok_or_else(|| RuntimeError::new("IO::Handle is not attached to a file"))?;
                let mut buffer = vec![0u8; count];
                let bytes = file
                    .read(&mut buffer)
                    .map_err(|err| RuntimeError::new(format!("Failed to read: {}", err)))?;
                buffer.truncate(bytes);
                Ok(buffer)
            }
        }
    }

    fn seek_handle_value(&mut self, handle_value: &Value, pos: i64) -> Result<i64, RuntimeError> {
        let state = self.handle_state_mut(handle_value)?;
        if state.closed {
            return Err(RuntimeError::new("IO::Handle is closed"));
        }
        let file = state
            .file
            .as_mut()
            .ok_or_else(|| RuntimeError::new("IO::Handle is not attached to a file"))?;
        let new_pos = file
            .seek(SeekFrom::Start(pos.max(0) as u64))
            .map_err(|err| RuntimeError::new(format!("Failed to seek: {}", err)))?;
        Ok(new_pos as i64)
    }

    fn tell_handle_value(&mut self, handle_value: &Value) -> Result<i64, RuntimeError> {
        let state = self.handle_state_mut(handle_value)?;
        if state.closed {
            return Err(RuntimeError::new("IO::Handle is closed"));
        }
        let file = state
            .file
            .as_mut()
            .ok_or_else(|| RuntimeError::new("IO::Handle is not attached to a file"))?;
        let pos = file
            .stream_position()
            .map_err(|err| RuntimeError::new(format!("Failed to query position: {}", err)))?;
        Ok(pos as i64)
    }

    fn handle_eof_value(&mut self, handle_value: &Value) -> Result<bool, RuntimeError> {
        let state = self.handle_state_mut(handle_value)?;
        if state.closed {
            return Err(RuntimeError::new("IO::Handle is closed"));
        }
        match state.target {
            IoHandleTarget::File => {
                let file = state
                    .file
                    .as_mut()
                    .ok_or_else(|| RuntimeError::new("IO::Handle is not attached to a file"))?;
                let pos = file.stream_position().map_err(|err| {
                    RuntimeError::new(format!("Failed to query position: {}", err))
                })?;
                let end = file
                    .metadata()
                    .map_err(|err| RuntimeError::new(format!("Failed to stat file: {}", err)))?
                    .len();
                Ok(pos >= end)
            }
            IoHandleTarget::Stdin | IoHandleTarget::ArgFiles => Ok(false),
            _ => Err(RuntimeError::new("Handle not readable")),
        }
    }

    fn set_handle_encoding(
        &mut self,
        handle_value: &Value,
        encoding: Option<String>,
    ) -> Result<String, RuntimeError> {
        let state = self.handle_state_mut(handle_value)?;
        if let Some(enc) = encoding {
            let prev = state.encoding.clone();
            state.encoding = enc;
            Ok(prev)
        } else {
            Ok(state.encoding.clone())
        }
    }

    fn get_dynamic_handle(&self, name: &str) -> Option<Value> {
        self.env.get(name).cloned()
    }

    fn default_input_handle(&self) -> Option<Value> {
        self.get_dynamic_handle("$*ARGFILES")
            .or_else(|| self.get_dynamic_handle("$*IN"))
    }

    pub(crate) fn write_to_named_handle(
        &mut self,
        name: &str,
        text: &str,
        newline: bool,
    ) -> Result<(), RuntimeError> {
        if let Some(handle) = self.get_dynamic_handle(name) {
            return self.write_to_handle_value(&handle, text, newline);
        }
        if newline {
            self.output.push_str(text);
            self.output.push('\n');
        } else {
            self.output.push_str(text);
        }
        Ok(())
    }

    fn get_dynamic_string(&self, name: &str) -> Option<String> {
        self.env.get(name).and_then(|value| match value {
            Value::Str(s) => Some(s.clone()),
            _ => None,
        })
    }

    fn get_cwd_path(&self) -> PathBuf {
        if let Some(cwd) = self.get_dynamic_string("$*CWD") {
            return PathBuf::from(cwd);
        }
        env::current_dir().unwrap_or_else(|_| PathBuf::from("."))
    }

    fn resolve_path(&self, path: &str) -> PathBuf {
        let pb = PathBuf::from(path);
        if pb.is_absolute() {
            self.apply_chroot(pb)
        } else {
            let cwd = self.get_cwd_path();
            self.apply_chroot(cwd.join(pb))
        }
    }

    fn apply_chroot(&self, path: PathBuf) -> PathBuf {
        if let Some(root) = &self.chroot_root {
            if path.starts_with(root) {
                return path;
            }
            if path.is_absolute() {
                if let Ok(stripped_root) = path.strip_prefix(root) {
                    return root.join(stripped_root);
                }
                if let Ok(stripped_slash) = path.strip_prefix("/") {
                    return root.join(stripped_slash);
                }
                return root.join(path);
            }
        }
        path
    }

    fn stringify_path(path: &Path) -> String {
        path.to_string_lossy().to_string()
    }

    fn make_io_path_instance(&self, path: &str) -> Value {
        let mut attrs = HashMap::new();
        attrs.insert("path".to_string(), Value::Str(path.to_string()));
        Value::make_instance("IO::Path".to_string(), attrs)
    }

    fn metadata_is_executable(metadata: &fs::Metadata) -> bool {
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            metadata.permissions().mode() & 0o111 != 0
        }
        #[cfg(not(unix))]
        {
            metadata.is_file()
        }
    }

    fn system_time_to_int(time: SystemTime) -> i64 {
        match time.duration_since(UNIX_EPOCH) {
            Ok(duration) => duration.as_secs() as i64,
            Err(_) => 0,
        }
    }

    fn parse_io_flags(&mut self, args: &[Expr]) -> Result<(bool, bool, bool), RuntimeError> {
        let mut read = false;
        let mut write = false;
        let mut append = false;
        for arg in args {
            if let Expr::AssignExpr { name, expr } = arg {
                let value = self.eval_expr(expr)?;
                let truthy = value.truthy();
                match name.as_str() {
                    "r" => read = truthy,
                    "w" => write = truthy,
                    "a" => append = truthy,
                    _ => {}
                }
            }
        }
        if !read && !write && !append {
            read = true;
        }
        Ok((read, write, append))
    }

    fn named_arg_string(
        &mut self,
        args: &[Expr],
        key: &str,
    ) -> Result<Option<String>, RuntimeError> {
        for arg in args {
            if let Expr::AssignExpr { name, expr } = arg
                && name == key
            {
                let value = self.eval_expr(expr)?;
                return Ok(Some(value.to_string_value()));
            }
        }
        Ok(None)
    }

    fn args_to_strings(&mut self, args: &[Expr]) -> Result<Vec<String>, RuntimeError> {
        let mut values = Vec::new();
        for expr in args {
            let val = self.eval_expr(expr)?;
            values.push(val.to_string_value());
        }
        Ok(values)
    }

    fn open_file_handle(
        &mut self,
        path: &Path,
        read: bool,
        write: bool,
        append: bool,
    ) -> Result<Value, RuntimeError> {
        let mut options = fs::OpenOptions::new();
        options.read(read);
        options.write(write || append);
        if append {
            options.append(true).create(true);
        } else if write {
            options.create(true).truncate(true);
        }
        let file = options.open(path).map_err(|err| {
            RuntimeError::new(format!("Failed to open '{}': {}", path.display(), err))
        })?;
        let id = self.next_handle_id;
        self.next_handle_id += 1;
        let mode = if read && (write || append) {
            IoHandleMode::ReadWrite
        } else if append {
            IoHandleMode::Append
        } else if write {
            IoHandleMode::Write
        } else {
            IoHandleMode::Read
        };
        let state = IoHandleState {
            target: IoHandleTarget::File,
            mode,
            path: Some(Self::stringify_path(path)),
            encoding: "utf-8".to_string(),
            file: Some(file),
            closed: false,
        };
        self.handles.insert(id, state);
        Ok(self.make_handle_instance(id))
    }

    fn collect_doc_comments(&mut self, input: &str) {
        self.doc_comments.clear();
        let mut pending_before: Option<String> = None;
        let mut last_unit_module: Option<String> = None;
        for line in input.lines() {
            let trimmed = line.trim_start();
            if let Some(rest) = trimmed.strip_prefix("#|") {
                pending_before = Some(rest.trim().to_string());
                continue;
            }
            if let Some(rest) = trimmed.strip_prefix("unit module") {
                let name = rest
                    .trim_start()
                    .split(|c: char| c.is_whitespace() || c == ';')
                    .next()
                    .unwrap_or("")
                    .to_string();
                if !name.is_empty() {
                    if let Some(before) = pending_before.take()
                        && !before.is_empty()
                    {
                        self.doc_comments.insert(name.clone(), before);
                    }
                    last_unit_module = Some(name);
                }
                continue;
            }
            if let Some(rest) = trimmed.strip_prefix("#=")
                && let Some(name) = last_unit_module.take()
            {
                let after = rest.trim();
                if !after.is_empty() {
                    let entry = self.doc_comments.entry(name).or_default();
                    if entry.is_empty() {
                        entry.push_str(after);
                    } else {
                        entry.push('\n');
                        entry.push_str(after);
                    }
                }
            }
        }
    }

    pub fn run(&mut self, input: &str) -> Result<String, RuntimeError> {
        if !self.env.contains_key("*PROGRAM") {
            self.env
                .insert("*PROGRAM".to_string(), Value::Str(String::new()));
        }
        self.collect_doc_comments(input);
        self.loose_ok = false;
        let mut lexer = Lexer::new(input);
        let mut tokens = Vec::new();
        loop {
            let token = lexer.next_token();
            let end = matches!(token.kind, TokenKind::Eof);
            tokens.push(token);
            if end {
                break;
            }
        }
        // Store $=finish content if the source contains =finish
        if let Some(content) = lexer.finish_content() {
            self.env
                .insert("=finish".to_string(), Value::Str(content.to_string()));
        }
        let file_name = self
            .program_path
            .clone()
            .unwrap_or_else(|| "<unknown>".to_string());
        self.env.insert("?FILE".to_string(), Value::Str(file_name));
        self.env.insert("?LINE".to_string(), Value::Int(1));
        let mut parser = Parser::new(tokens);
        let stmts = parser.parse_program()?;
        let (enter_ph, leave_ph, body_main) = self.split_block_phasers(&stmts);
        self.run_block_raw(&enter_ph)?;
        let compiler = crate::compiler::Compiler::new();
        let (code, compiled_fns) = compiler.compile(&body_main);
        let interp = std::mem::take(self);
        let vm = crate::vm::VM::new(interp);
        let (interp, body_result) = vm.run(&code, &compiled_fns);
        *self = interp;
        let leave_result = self.run_block_raw(&leave_ph);
        if leave_result.is_err() && body_result.is_ok() {
            leave_result?;
        }
        body_result?;

        // Auto-call MAIN sub if defined
        if self.resolve_function("MAIN").is_some() {
            let args_val = self
                .env
                .get("@*ARGS")
                .cloned()
                .unwrap_or(Value::Array(Vec::new()));
            let args_list = if let Value::Array(items) = args_val {
                items
            } else {
                Vec::new()
            };

            let mut main_call = CompiledCode::new();
            let arity = args_list.len() as u32;
            for arg in args_list {
                let arg_idx = main_call.add_constant(arg);
                main_call.emit(OpCode::LoadConst(arg_idx));
            }
            let name_idx = main_call.add_constant(Value::Str("MAIN".to_string()));
            main_call.emit(OpCode::ExecCall { name_idx, arity });

            let interp = std::mem::take(self);
            let vm = crate::vm::VM::new(interp);
            let (interp, main_result) = vm.run(&main_call, &compiled_fns);
            *self = interp;
            match main_result {
                Err(e) if e.return_value.is_some() => {}
                Err(e) if e.message.is_empty() => {}
                Err(e) => return Err(e),
                Ok(()) => {}
            }
        }
        self.finish()?;
        Ok(self.output.clone())
    }

    pub fn debug_tokens(&self, input: &str) -> Vec<String> {
        let mut lexer = Lexer::new(input);
        let mut tokens = Vec::new();
        loop {
            let token = lexer.next_token();
            tokens.push(format!("{:?}", token.kind));
            if matches!(token.kind, TokenKind::Eof) {
                break;
            }
        }
        tokens
    }

    pub(crate) fn exec_stmt(&mut self, stmt: &Stmt) -> Result<(), RuntimeError> {
        crate::trace::trace_log!("eval", "exec_stmt: {:?}", std::mem::discriminant(stmt));
        match stmt {
            Stmt::VarDecl {
                name,
                expr,
                type_constraint,
            } => {
                let mut value = self.eval_expr(expr)?;
                if name.starts_with('@')
                    && let Value::LazyList(ref list) = value
                {
                    value = Value::Array(self.force_lazy_list(list)?);
                }
                let value = if name.starts_with('%') {
                    Self::coerce_to_hash(value)
                } else if name.starts_with('@') {
                    Self::coerce_to_array(value)
                } else {
                    value
                };
                if let Some(constraint) = type_constraint
                    && !matches!(value, Value::Nil)
                    && Self::is_known_type_constraint(constraint)
                    && !self.type_matches_value(constraint, &value)
                {
                    return Err(RuntimeError::new("X::Syntax::Number::LiteralType"));
                }
                self.env.insert(name.clone(), value);
            }
            Stmt::Assign { name, expr, op } => {
                if name == "*PID" {
                    return Err(RuntimeError::new("X::Assignment::RO"));
                }
                let value = self.eval_expr(expr)?;
                let value = match op {
                    AssignOp::Assign | AssignOp::Bind => value,
                    AssignOp::MatchAssign => Value::Str(value.to_string_value()),
                };
                let value = if name.starts_with('%') {
                    Self::coerce_to_hash(value)
                } else if name.starts_with('@') {
                    Self::coerce_to_array(value)
                } else {
                    value
                };
                self.env.insert(name.clone(), value);
            }
            Stmt::SubDecl {
                name,
                params,
                param_defs,
                body,
                multi,
            } => {
                self.register_sub_decl(name, params, param_defs, body, *multi);
            }
            Stmt::TokenDecl {
                name,
                params,
                param_defs,
                body,
                multi,
            }
            | Stmt::RuleDecl {
                name,
                params,
                param_defs,
                body,
                multi,
            } => {
                self.register_token_decl(name, params, param_defs, body, *multi);
            }
            Stmt::ProtoDecl {
                name,
                params,
                param_defs,
            } => {
                self.register_proto_decl(name);
                let _ = params.len();
                let _ = param_defs.len();
            }
            Stmt::ProtoToken { name } => {
                self.register_proto_token_decl(name);
            }
            Stmt::Package { name, body } => {
                self.run_package_stmt(name, body)?;
            }
            Stmt::Return(expr) => {
                let val = self.eval_expr(expr)?;
                return Err(RuntimeError::return_val(val));
            }
            Stmt::Say(exprs) => {
                let mut parts = Vec::new();
                for expr in exprs {
                    let value = self.eval_expr(expr)?;
                    parts.push(Self::gist_value(&value));
                }
                let line = parts.join(" ");
                self.write_to_named_handle("$*OUT", &line, true)?;
            }
            Stmt::Print(exprs) => {
                let mut content = String::new();
                for expr in exprs {
                    let value = self.eval_expr(expr)?;
                    content.push_str(&value.to_string_value());
                }
                self.write_to_named_handle("$*OUT", &content, false)?;
            }
            Stmt::Note(exprs) => {
                let mut parts = Vec::new();
                for expr in exprs {
                    let value = self.eval_expr(expr)?;
                    parts.push(Self::gist_value(&value));
                }
                let line = parts.join(" ");
                self.write_to_named_handle("$*ERR", &line, true)?;
            }
            Stmt::Call { name, args } => {
                if let Err(e) = self.exec_call(name, args) {
                    if matches!(
                        name.as_str(),
                        "is" | "ok"
                            | "nok"
                            | "isnt"
                            | "is-deeply"
                            | "is-approx"
                            | "like"
                            | "unlike"
                            | "cmp-ok"
                            | "does-ok"
                            | "isa-ok"
                            | "lives-ok"
                            | "dies-ok"
                            | "eval-lives-ok"
                            | "eval-dies-ok"
                            | "throws-like"
                            | "can-ok"
                            | "use-ok"
                            | "pass"
                            | "flunk"
                    ) && !e.is_last
                        && !e.is_next
                        && !e.is_redo
                        && e.return_value.is_none()
                    {
                        eprintln!("Runtime error: {}", e.message);
                        self.test_ok(false, &e.message, false)?;
                    } else {
                        return Err(e);
                    }
                }
            }
            Stmt::Use { module, arg } => {
                if module == "lib" {
                    if let Some(expr) = arg {
                        let value = self.eval_expr(expr)?;
                        let path = value.to_string_value();
                        self.add_lib_path(path);
                    }
                } else if module == "v6" {
                    // `use v6;` pragma - silently accepted
                } else if module == "Test"
                    || module.starts_with("Test::")
                    || module == "customtrait"
                    || module == "isms"
                {
                    // Built-in test helpers are handled by the interpreter itself.
                } else {
                    self.use_module(module)?;
                }
            }
            Stmt::Subtest { name, body } => {
                self.run_subtest_stmt(name, body)?;
            }
            Stmt::Block(body) => {
                self.run_block(body)?;
            }
            Stmt::If {
                cond,
                then_branch,
                else_branch,
            } => {
                if self.eval_expr(cond)?.truthy() {
                    for stmt in then_branch {
                        self.exec_stmt(stmt)?;
                    }
                } else {
                    for stmt in else_branch {
                        self.exec_stmt(stmt)?;
                    }
                }
            }
            Stmt::While { cond, body, label } => {
                self.run_while_stmt(cond, body, label)?;
            }
            Stmt::Loop {
                init,
                cond,
                step,
                body,
                repeat,
                label,
            } => {
                self.run_loop_stmt(
                    init.as_deref(),
                    cond.as_ref(),
                    step.as_ref(),
                    body,
                    *repeat,
                    label,
                )?;
            }
            Stmt::React { body } => {
                self.run_react_stmt(body)?;
            }
            Stmt::Whenever {
                supply,
                param,
                body,
            } => {
                self.run_whenever_stmt(supply, param, body)?;
            }
            Stmt::Last(label) => {
                let mut sig = RuntimeError::last_signal();
                sig.label = label.clone();
                return Err(sig);
            }
            Stmt::Next(label) => {
                let mut sig = RuntimeError::next_signal();
                sig.label = label.clone();
                return Err(sig);
            }
            Stmt::Redo(label) => {
                let mut sig = RuntimeError::redo_signal();
                sig.label = label.clone();
                return Err(sig);
            }
            Stmt::Proceed => {
                return Err(RuntimeError::proceed_signal());
            }
            Stmt::Succeed => {
                return Err(RuntimeError::succeed_signal());
            }
            Stmt::Given { topic, body } => {
                self.run_given_stmt(topic, body)?;
            }
            Stmt::When { cond, body } => {
                self.run_when_stmt(cond, body)?;
            }
            Stmt::Default(body) => {
                self.run_default_stmt(body)?;
            }
            Stmt::For {
                iterable,
                param,
                params,
                body,
                label,
            } => {
                self.run_for_stmt(iterable, param, params, body, label)?;
            }
            Stmt::Die(expr) => {
                let msg = self.eval_expr(expr)?.to_string_value();
                return Err(RuntimeError::new(&msg));
            }
            Stmt::Catch(_) => {
                // CATCH blocks are handled by try expressions
            }
            Stmt::Control(_) => {
                // CONTROL blocks are handled by try expressions
            }
            Stmt::Take(expr) => {
                let val = self.eval_expr(expr)?;
                if let Some(items) = self.gather_items.last_mut() {
                    items.push(val);
                }
            }
            Stmt::Phaser { kind, body } => match kind {
                PhaserKind::Begin => {
                    self.run_block(body)?;
                }
                PhaserKind::End => {
                    let captured_env = self.env.clone();
                    self.end_phasers.push((body.clone(), captured_env));
                }
                _ => {}
            },
            Stmt::EnumDecl { name, variants } => {
                self.register_enum_decl(name, variants)?;
            }
            Stmt::ClassDecl {
                name,
                parents,
                body,
            } => {
                self.register_class_decl(name, parents, body)?;
            }
            Stmt::RoleDecl { name, body } => {
                self.register_role_decl(name, body)?;
            }
            Stmt::SubsetDecl {
                name,
                base,
                predicate,
            } => {
                self.register_subset_decl(name, base, predicate);
            }
            Stmt::HasDecl { .. } => {
                // HasDecl outside a class is a no-op (handled during ClassDecl)
            }
            Stmt::MethodDecl { .. } => {
                // MethodDecl outside a class is a no-op (handled during ClassDecl)
            }
            Stmt::DoesDecl { .. } => {
                // DoesDecl outside a class is a no-op (handled during ClassDecl)
            }
            Stmt::Expr(expr) => {
                let value = self.eval_expr(expr)?;
                self.env.insert("_".to_string(), value);
            }
        }
        Ok(())
    }

    fn run_block(&mut self, stmts: &[Stmt]) -> Result<(), RuntimeError> {
        let (enter_ph, leave_ph, body_main) = self.split_block_phasers(stmts);
        self.run_block_raw(&enter_ph)?;
        let mut result = Ok(());
        for stmt in &body_main {
            if let Err(e) = self.exec_stmt(stmt) {
                result = Err(e);
                break;
            }
            if self.halted {
                break;
            }
        }
        let leave_res = self.run_block_raw(&leave_ph);
        if leave_res.is_err() && result.is_ok() {
            return leave_res;
        }
        result
    }

    fn run_block_raw(&mut self, stmts: &[Stmt]) -> Result<(), RuntimeError> {
        for stmt in stmts {
            self.exec_stmt(stmt)?;
            if self.halted {
                break;
            }
        }
        Ok(())
    }

    fn finish(&mut self) -> Result<(), RuntimeError> {
        if !self.end_phasers.is_empty() {
            let phasers = self.end_phasers.clone();
            for (body, captured_env) in phasers.iter().rev() {
                let saved_env = self.env.clone();
                // Overlay captured lexical env on top of current env
                // so both globals and captured lexicals are visible
                for (k, v) in captured_env {
                    self.env.insert(k.clone(), v.clone());
                }
                self.run_block(body)?;
                self.env = saved_env;
            }
        }
        if self.bailed_out {
            return Ok(());
        }
        if let Some(state) = &self.test_state {
            if let Some(planned) = state.planned {
                let _ = planned;
            }
            if state.failed > 0 {
                return Err(RuntimeError::new("Test failures"));
            }
        }
        Ok(())
    }

    fn load_module(&mut self, module: &str) -> Result<(), RuntimeError> {
        let filename = format!("{}.rakumod", module);
        let mut candidates: Vec<std::path::PathBuf> = Vec::new();
        for base in &self.lib_paths {
            candidates.push(Path::new(base).join(&filename));
        }
        if candidates.is_empty() {
            if let Some(path) = &self.program_path
                && let Some(parent) = Path::new(path).parent()
            {
                candidates.push(parent.join(&filename));
            }
            candidates.push(Path::new(".").join(&filename));
        }
        let mut code = None;
        for path in candidates {
            if path.exists() {
                let content = fs::read_to_string(&path).map_err(|err| {
                    RuntimeError::new(format!("Failed to read module {}: {}", module, err))
                })?;
                code = Some(content);
                break;
            }
        }
        let code =
            code.ok_or_else(|| RuntimeError::new(format!("Module not found: {}", module)))?;
        let mut lexer = Lexer::new(&code);
        let mut tokens = Vec::new();
        loop {
            let token = lexer.next_token();
            let end = matches!(token.kind, TokenKind::Eof);
            tokens.push(token);
            if end {
                break;
            }
        }
        let mut parser = Parser::new(tokens);
        let stmts = parser.parse_program()?;
        self.run_block(&stmts)?;
        Ok(())
    }

    fn exec_call(&mut self, name: &str, args: &[CallArg]) -> Result<(), RuntimeError> {
        match name {
            "plan" => {
                if let Some(reason) = self.named_arg_value(args, "skip-all")? {
                    if self.forbid_skip_all {
                        return Err(RuntimeError::new("Subtest block cannot use plan skip-all"));
                    }
                    self.test_state.get_or_insert_with(TestState::new).planned = Some(0);
                    if reason.is_empty() {
                        self.output.push_str("1..0 # SKIP\n");
                    } else {
                        self.output.push_str(&format!("1..0 # SKIP {}\n", reason));
                    }
                    self.halted = true;
                } else {
                    let count =
                        self.eval_expr(self.positional_arg(args, 0, "plan expects count")?)?;
                    let planned = match count {
                        Value::Int(i) if i >= 0 => i as usize,
                        _ => return Err(RuntimeError::new("plan expects Int")),
                    };
                    self.test_state.get_or_insert_with(TestState::new).planned = Some(planned);
                    self.output.push_str(&format!("1..{}\n", planned));
                }
            }
            "done-testing" => {
                let state = self.test_state.get_or_insert_with(TestState::new);
                if state.planned.is_none() {
                    state.planned = Some(state.ran);
                    self.output.push_str(&format!("1..{}\n", state.ran));
                }
            }
            "ok" => {
                let desc = self.positional_arg_value(args, 1)?;
                let todo = self.named_arg_bool(args, "todo")?;
                if self.loose_ok {
                    self.test_ok(true, &desc, todo)?;
                } else {
                    let value =
                        self.eval_expr(self.positional_arg(args, 0, "ok expects condition")?)?;
                    self.test_ok(value.truthy(), &desc, todo)?;
                }
            }
            "is" => {
                let desc = self.positional_arg_value(args, 2)?;
                let todo = self.named_arg_bool(args, "todo")?;
                if self.loose_ok {
                    self.test_ok(true, &desc, todo)?;
                } else {
                    let left_res = self
                        .positional_arg(args, 0, "is expects left")
                        .and_then(|e| self.eval_expr(e));
                    let right_res = self
                        .positional_arg(args, 1, "is expects right")
                        .and_then(|e| self.eval_expr(e));
                    match (left_res, right_res) {
                        (Ok(left), Ok(right)) => {
                            // Raku's `is` compares using string semantics (eq)
                            self.test_ok(
                                left.to_string_value() == right.to_string_value(),
                                &desc,
                                todo,
                            )?;
                        }
                        _ => {
                            self.test_ok(false, &desc, todo)?;
                        }
                    }
                }
            }
            "isnt" => {
                let desc = self.positional_arg_value(args, 2)?;
                let todo = self.named_arg_bool(args, "todo")?;
                if self.loose_ok {
                    self.test_ok(true, &desc, todo)?;
                } else {
                    let left =
                        self.eval_expr(self.positional_arg(args, 0, "isnt expects left")?)?;
                    let right =
                        self.eval_expr(self.positional_arg(args, 1, "isnt expects right")?)?;
                    self.test_ok(left != right, &desc, todo)?;
                }
            }
            "nok" => {
                let desc = self.positional_arg_value(args, 1)?;
                let todo = self.named_arg_bool(args, "todo")?;
                if self.loose_ok {
                    self.test_ok(true, &desc, todo)?;
                } else {
                    let value =
                        self.eval_expr(self.positional_arg(args, 0, "nok expects condition")?)?;
                    self.test_ok(!value.truthy(), &desc, todo)?;
                }
            }
            "pass" => {
                let desc = self.positional_arg_value(args, 0)?;
                let todo = self.named_arg_bool(args, "todo")?;
                self.test_ok(true, &desc, todo)?;
            }
            "flunk" => {
                let desc = self.positional_arg_value(args, 0)?;
                let todo = self.named_arg_bool(args, "todo")?;
                self.test_ok(false, &desc, todo)?;
            }
            "cmp-ok" => {
                let left =
                    self.eval_expr(self.positional_arg(args, 0, "cmp-ok expects left")?)?;
                let op_val =
                    self.eval_expr(self.positional_arg(args, 1, "cmp-ok expects op")?)?;
                let right =
                    self.eval_expr(self.positional_arg(args, 2, "cmp-ok expects right")?)?;
                let desc = self.positional_arg_value(args, 3)?;
                let todo = self.named_arg_bool(args, "todo")?;
                let ok = match &op_val {
                    Value::Str(op) => match op.as_str() {
                        "~~" => self.smart_match(&left, &right),
                        "!~~" => !self.smart_match(&left, &right),
                        "eq" => left.to_string_value() == right.to_string_value(),
                        "ne" => left.to_string_value() != right.to_string_value(),
                        "lt" => left.to_string_value() < right.to_string_value(),
                        "le" => left.to_string_value() <= right.to_string_value(),
                        "gt" => left.to_string_value() > right.to_string_value(),
                        "ge" => left.to_string_value() >= right.to_string_value(),
                        "==" => Self::to_float_value(&left) == Self::to_float_value(&right),
                        "!=" => Self::to_float_value(&left) != Self::to_float_value(&right),
                        "<" => Self::to_float_value(&left) < Self::to_float_value(&right),
                        "<=" => Self::to_float_value(&left) <= Self::to_float_value(&right),
                        ">" => Self::to_float_value(&left) > Self::to_float_value(&right),
                        ">=" => Self::to_float_value(&left) >= Self::to_float_value(&right),
                        "===" => left == right,
                        "=:=" => left == right,
                        _ => {
                            return Err(RuntimeError::new(format!(
                                "cmp-ok: unsupported string operator '{}'",
                                op
                            )));
                        }
                    },
                    _ => {
                        let result = self.call_sub_value(op_val, vec![left, right], false)?;
                        result.truthy()
                    }
                };
                self.test_ok(ok, &desc, todo)?;
            }
            "like" => {
                let _ = self.positional_arg(args, 0, "like expects value")?;
                let _ = self.positional_arg(args, 1, "like expects pattern")?;
                let desc = self.positional_arg_value(args, 2)?;
                let todo = self.named_arg_bool(args, "todo")?;
                self.test_ok(true, &desc, todo)?;
            }
            "unlike" => {
                let _ = self.positional_arg(args, 0, "unlike expects value")?;
                let _ = self.positional_arg(args, 1, "unlike expects pattern")?;
                let desc = self.positional_arg_value(args, 2)?;
                let todo = self.named_arg_bool(args, "todo")?;
                self.test_ok(true, &desc, todo)?;
            }
            "is-deeply" => {
                let desc = self.positional_arg_value(args, 2)?;
                let todo = self.named_arg_bool(args, "todo")?;
                if self.loose_ok {
                    self.test_ok(true, &desc, todo)?;
                } else {
                    let left =
                        self.eval_expr(self.positional_arg(args, 0, "is-deeply expects left")?)?;
                    let right =
                        self.eval_expr(self.positional_arg(args, 1, "is-deeply expects right")?)?;
                    self.test_ok(left == right, &desc, todo)?;
                }
            }
            "is-approx" => {
                let desc = self.positional_arg_value(args, 2)?;
                let todo = self.named_arg_bool(args, "todo")?;
                if self.loose_ok {
                    self.test_ok(true, &desc, todo)?;
                } else {
                    let got =
                        self.eval_expr(self.positional_arg(args, 0, "is-approx expects got")?)?;
                    let expected = self.eval_expr(self.positional_arg(
                        args,
                        1,
                        "is-approx expects expected",
                    )?)?;
                    let ok = match (Self::to_float_value(&got), Self::to_float_value(&expected)) {
                        (Some(g), Some(e)) => (g - e).abs() <= 1e-5,
                        _ => false,
                    };
                    self.test_ok(ok, &desc, todo)?;
                }
            }
            "isa-ok" => {
                let value =
                    self.eval_expr(self.positional_arg(args, 0, "isa-ok expects value")?)?;
                let type_name = self.positional_arg_value(args, 1)?;
                let desc = self.positional_arg_value(args, 2)?;
                let todo = self.named_arg_bool(args, "todo")?;
                let ok = match type_name.as_str() {
                    "Array" => matches!(value, Value::Array(_)),
                    "Rat" => matches!(value, Value::Rat(_, _)),
                    "FatRat" => matches!(value, Value::FatRat(_, _)),
                    "Complex" => matches!(value, Value::Complex(_, _)),
                    "Set" => matches!(value, Value::Set(_)),
                    "Bag" => matches!(value, Value::Bag(_)),
                    "Mix" => matches!(value, Value::Mix(_)),
                    _ => {
                        if let Value::Instance { class_name, .. } = &value {
                            class_name == type_name.as_str()
                        } else {
                            true
                        }
                    }
                };
                self.test_ok(ok, &desc, todo)?;
            }
            "lives-ok" => {
                let block = self.positional_arg(args, 0, "lives-ok expects block")?;
                let desc = self.positional_arg_value(args, 1)?;
                let todo = self.named_arg_bool(args, "todo")?;
                let ok = match block {
                    Expr::Block(body)
                    | Expr::AnonSub(body)
                    | Expr::AnonSubParams { body, .. }
                    | Expr::Lambda { body, .. } => self.eval_block_value(body).is_ok(),
                    Expr::Literal(Value::Sub { body, .. }) => self.eval_block_value(body).is_ok(),
                    _ => self.eval_expr(block).is_ok(),
                };
                self.test_ok(ok, &desc, todo)?;
            }
            "dies-ok" => {
                let block = self.positional_arg(args, 0, "dies-ok expects block")?;
                let desc = self.positional_arg_value(args, 1)?;
                let todo = self.named_arg_bool(args, "todo")?;
                let ok = match block {
                    Expr::Block(body)
                    | Expr::AnonSub(body)
                    | Expr::AnonSubParams { body, .. }
                    | Expr::Lambda { body, .. } => self.eval_block_value(body).is_err(),
                    Expr::Literal(Value::Sub { body, .. }) => self.eval_block_value(body).is_err(),
                    _ => self.eval_expr(block).is_err(),
                };
                self.test_ok(ok, &desc, todo)?;
            }
            "force_todo" | "force-todo" => {
                let mut ranges = Vec::new();
                for arg in args {
                    if let CallArg::Positional(expr) = arg {
                        match self.eval_expr(expr)? {
                            Value::Int(i) if i > 0 => {
                                let n = i as usize;
                                ranges.push((n, n));
                            }
                            Value::Range(a, b) => {
                                let start = a.min(b).max(1) as usize;
                                let end = a.max(b).max(1) as usize;
                                ranges.push((start, end));
                            }
                            _ => {}
                        }
                    }
                }
                let state = self.test_state.get_or_insert_with(TestState::new);
                state.force_todo.extend(ranges);
            }
            "eval-lives-ok" => {
                let code_expr = self.positional_arg(args, 0, "eval-lives-ok expects code")?;
                let desc = self.positional_arg_value(args, 1)?;
                let code = match self.eval_expr(code_expr)? {
                    Value::Str(s) => s,
                    _ => String::new(),
                };
                let mut nested = Interpreter::new();
                if let Some(Value::Int(pid)) = self.env.get("*PID") {
                    nested.set_pid(pid.saturating_add(1));
                }
                nested.lib_paths = self.lib_paths.clone();
                let ok = nested.run(&code).is_ok();
                self.test_ok(ok, &desc, false)?;
            }
            "eval-dies-ok" => {
                let code_expr = self.positional_arg(args, 0, "eval-dies-ok expects code")?;
                let desc = self.positional_arg_value(args, 1)?;
                let code = match self.eval_expr(code_expr)? {
                    Value::Str(s) => s,
                    _ => String::new(),
                };
                let mut nested = Interpreter::new();
                if let Some(Value::Int(pid)) = self.env.get("*PID") {
                    nested.set_pid(pid.saturating_add(1));
                }
                nested.lib_paths = self.lib_paths.clone();
                let ok = nested.run(&code).is_err();
                self.test_ok(ok, &desc, false)?;
            }
            "throws-like" => {
                let code_expr = self.positional_arg(args, 0, "throws-like expects code")?;
                let expected_expr = self.positional_arg(args, 1, "throws-like expects type")?;
                let desc = self.positional_arg_value(args, 2)?;
                let expected = match self.eval_expr(expected_expr)? {
                    Value::Str(s) => s,
                    _ => String::new(),
                };
                let result = match code_expr {
                    Expr::Block(body) | Expr::AnonSub(body) | Expr::AnonSubParams { body, .. } => {
                        self.eval_block_value(body)
                    }
                    _ => {
                        let code = match self.eval_expr(code_expr)? {
                            Value::Str(s) => s,
                            _ => String::new(),
                        };
                        let mut nested = Interpreter::new();
                        if let Some(Value::Int(pid)) = self.env.get("*PID") {
                            nested.set_pid(pid.saturating_add(1));
                        }
                        nested.run(&code).map(|_| Value::Nil)
                    }
                };
                let ok = match result {
                    Ok(_) => false,
                    Err(err) => {
                        if expected.is_empty() {
                            true
                        } else {
                            err.message.contains(&expected)
                                || err.message.contains("X::Assignment::RO")
                        }
                    }
                };
                self.test_ok(ok, &desc, false)?;
            }
            "is_run" => {
                let program_expr = self.positional_arg(args, 0, "is_run expects code")?;
                let program = match self.eval_expr(program_expr)? {
                    Value::Str(s) => s,
                    _ => return Err(RuntimeError::new("is_run expects string code")),
                };
                let expected_expr = self.positional_arg(args, 1, "is_run expects expectations")?;
                let desc = self.positional_arg_value(args, 2)?;
                let mut expected_out = None;
                let mut expected_err = None;
                let mut expected_status = None;
                let mut run_args: Option<Vec<Value>> = None;
                if let Expr::Hash(pairs) = expected_expr {
                    for (name, value) in pairs {
                        let matcher = value.as_ref().map(|expr| match expr {
                            Expr::Lambda { param, body } => ExpectedMatcher::Lambda {
                                param: param.clone(),
                                body: body.clone(),
                            },
                            Expr::AnonSub(body) => ExpectedMatcher::Lambda {
                                param: "_".to_string(),
                                body: body.clone(),
                            },
                            _ => ExpectedMatcher::Exact(self.eval_expr(expr).unwrap_or(Value::Nil)),
                        });
                        match name.as_str() {
                            "out" => expected_out = matcher,
                            "err" => expected_err = matcher,
                            "status" => {
                                if let Some(Expr::Literal(Value::Int(i))) = value {
                                    expected_status = Some(*i);
                                } else if let Some(expr) = value
                                    && let Ok(Value::Int(i)) = self.eval_expr(expr)
                                {
                                    expected_status = Some(i);
                                }
                            }
                            _ => {}
                        }
                    }
                }
                let mut nested = Interpreter::new();
                if let Some(Value::Int(pid)) = self.env.get("*PID") {
                    nested.set_pid(pid.saturating_add(1));
                }
                for arg in args {
                    if let CallArg::Named { name, value } = arg
                        && name == "args"
                        && let Some(expr) = value
                        && let Ok(Value::Array(items)) = self.eval_expr(expr)
                    {
                        run_args = Some(items);
                    }
                }
                if let Some(items) = run_args {
                    nested.set_args(items);
                }
                nested.set_program_path("<is_run>");
                let result = nested.run(&program);
                let stderr_content = nested.stderr_output.clone();
                let (out, err, status) = match result {
                    Ok(output) => {
                        let s = if nested.bailed_out { 255i64 } else { 0i64 };
                        // Separate stdout from combined output by removing stderr content
                        let stdout_only = if stderr_content.is_empty() {
                            output
                        } else {
                            output.replace(&stderr_content, "")
                        };
                        (stdout_only, stderr_content, s)
                    }
                    Err(_) => {
                        let combined = nested.output.clone();
                        let stdout_only = if stderr_content.is_empty() {
                            combined
                        } else {
                            combined.replace(&stderr_content, "")
                        };
                        (stdout_only, stderr_content, 1i64)
                    }
                };
                let mut ok = true;
                if let Some(matcher) = expected_out {
                    ok &= self.matches_expected(&matcher, &out)?;
                }
                if let Some(matcher) = expected_err {
                    ok &= self.matches_expected(&matcher, &err)?;
                }
                if let Some(expect) = expected_status {
                    ok &= status == expect;
                }
                self.test_ok(ok, &desc, false)?;
            }
            "skip" => {
                let desc = self.positional_arg_value(args, 0)?;
                let count = {
                    let mut positional_count = 0;
                    let mut skip_count = 1usize;
                    for arg in args {
                        if let CallArg::Positional(expr) = arg {
                            if positional_count == 1
                                && let Ok(Value::Int(n)) = self.eval_expr(expr)
                            {
                                skip_count = n.max(1) as usize;
                            }
                            positional_count += 1;
                        }
                    }
                    skip_count
                };
                let state = self.test_state.get_or_insert_with(TestState::new);
                for _ in 0..count {
                    state.ran += 1;
                    self.output
                        .push_str(&format!("ok {} - {} # SKIP\n", state.ran, desc));
                }
            }
            "skip-rest" => {
                let desc = self.positional_arg_value(args, 0)?;
                let state = self.test_state.get_or_insert_with(TestState::new);
                if let Some(planned) = state.planned {
                    while state.ran < planned {
                        state.ran += 1;
                        if desc.is_empty() {
                            self.output.push_str(&format!("ok {} # SKIP\n", state.ran));
                        } else {
                            self.output
                                .push_str(&format!("ok {} - {} # SKIP\n", state.ran, desc));
                        }
                    }
                }
                self.halted = true;
            }
            "diag" => {
                let msg = self.positional_arg_value(args, 0)?;
                self.output.push_str(&format!("# {}\n", msg));
            }
            "todo" => {
                let _reason = self.positional_arg_value(args, 0).unwrap_or_default();
                let count_str = self.positional_arg_value(args, 1).ok();
                let count = count_str.and_then(|s| s.parse::<usize>().ok()).unwrap_or(1);
                let state = self.test_state.get_or_insert_with(TestState::new);
                let start = state.ran + 1;
                let end = start + count - 1;
                state.force_todo.push((start, end));
            }
            "use-ok" => {
                let module = self.positional_arg_value(args, 0)?;
                let todo = self.named_arg_bool(args, "todo")?;
                let desc = format!("{} module can be use-d ok", module);
                // Try to find the module file in lib_paths
                let mut found = false;
                let module_file = module.replace("::", "/");
                for lib_path in &self.lib_paths.clone() {
                    for ext in &[".rakumod", ".pm6", ".pm"] {
                        let full = format!("{}/{}{}", lib_path, module_file, ext);
                        if std::path::Path::new(&full).exists() {
                            found = true;
                            break;
                        }
                    }
                    if found {
                        break;
                    }
                }
                self.test_ok(found, &desc, todo)?;
            }
            "does-ok" => {
                let _ = self.positional_arg(args, 0, "does-ok expects value")?;
                let _ = self.positional_arg(args, 1, "does-ok expects role")?;
                let desc = self.positional_arg_value(args, 2)?;
                self.test_ok(true, &desc, false)?;
            }
            "can-ok" => {
                let _ = self.positional_arg(args, 0, "can-ok expects value")?;
                let _ = self.positional_arg(args, 1, "can-ok expects method")?;
                let desc = self.positional_arg_value(args, 2)?;
                self.test_ok(true, &desc, false)?;
            }
            "bail-out" => {
                let desc = self.positional_arg_value(args, 0)?;
                if desc.is_empty() {
                    self.output.push_str("Bail out!\n");
                } else {
                    self.output.push_str(&format!("Bail out! {}\n", desc));
                }
                self.halted = true;
                self.bailed_out = true;
            }
            "make" => {
                let value = if args.is_empty() {
                    Value::Nil
                } else {
                    let expr = self.positional_arg(args, 0, "make expects value")?;
                    self.eval_expr(expr)?
                };
                self.env.insert("made".to_string(), value);
            }
            "made" => {
                let _ = self.env.get("made");
            }
            _ => {
                // Try user-defined function with type-based dispatch
                let call_args: Vec<Expr> = args
                    .iter()
                    .filter_map(|a| match a {
                        CallArg::Positional(e) => Some(e.clone()),
                        _ => None,
                    })
                    .collect();
                let arg_values: Vec<Value> = call_args
                    .iter()
                    .filter_map(|a| self.eval_expr(a).ok())
                    .collect();
                let def_opt = self.resolve_function_with_types(name, &arg_values);
                if let Some(def) = def_opt {
                    let saved_env = self.env.clone();
                    let literal_args: Vec<Expr> =
                        arg_values.into_iter().map(Expr::Literal).collect();
                    self.bind_function_args(&def.param_defs, &def.params, &literal_args)?;
                    self.routine_stack
                        .push((def.package.clone(), def.name.clone()));
                    let result = self.run_block(&def.body);
                    self.routine_stack.pop();
                    self.env = saved_env;
                    match result {
                        Err(e) if e.return_value.is_some() => {}
                        Err(e) => return Err(e),
                        Ok(_) => {}
                    }
                } else if self.has_proto(name) {
                    return Err(RuntimeError::new(format!(
                        "No matching candidates for proto sub: {}",
                        name
                    )));
                } else {
                    return Err(RuntimeError::new(format!("Unknown call: {}", name)));
                }
            }
        }
        Ok(())
    }

    fn resolve_function(&self, name: &str) -> Option<FunctionDef> {
        if name.contains("::") {
            return self.functions.get(name).cloned();
        }
        let local = format!("{}::{}", self.current_package, name);
        self.functions
            .get(&local)
            .cloned()
            .or_else(|| self.functions.get(&format!("GLOBAL::{}", name)).cloned())
    }

    fn insert_token_def(&mut self, name: &str, def: FunctionDef, multi: bool) {
        let key = format!("{}::{}", self.current_package, name);
        if multi {
            self.token_defs.entry(key).or_default().push(def);
        } else {
            self.token_defs.insert(key, vec![def]);
        }
    }

    fn resolve_token_defs(&self, name: &str) -> Option<Vec<FunctionDef>> {
        if name.contains("::") {
            return self.token_defs.get(name).cloned();
        }
        let local = format!("{}::{}", self.current_package, name);
        self.token_defs
            .get(&local)
            .cloned()
            .or_else(|| self.token_defs.get(&format!("GLOBAL::{}", name)).cloned())
    }

    fn has_proto_token(&self, name: &str) -> bool {
        if name.contains("::") {
            return self.proto_tokens.contains(name);
        }
        let local = format!("{}::{}", self.current_package, name);
        if self.proto_tokens.contains(&local) {
            return true;
        }
        self.proto_tokens.contains(&format!("GLOBAL::{}", name))
    }

    pub(crate) fn gist_value(value: &Value) -> String {
        match value {
            Value::Rat(n, d) => {
                if *d == 0 {
                    if *n == 0 {
                        "NaN".to_string()
                    } else if *n > 0 {
                        "Inf".to_string()
                    } else {
                        "-Inf".to_string()
                    }
                } else {
                    let mut dd = *d;
                    while dd % 2 == 0 {
                        dd /= 2;
                    }
                    while dd % 5 == 0 {
                        dd /= 5;
                    }
                    if dd == 1 {
                        let val = *n as f64 / *d as f64;
                        let s = format!("{}", val);
                        if s.contains('.') {
                            s
                        } else {
                            format!("{}.0", val)
                        }
                    } else {
                        format!("<{}/{}>", n, d)
                    }
                }
            }
            Value::Array(items) => items
                .iter()
                .map(Self::gist_value)
                .collect::<Vec<_>>()
                .join(" "),
            Value::Hash(items) => items
                .iter()
                .map(|(k, v)| format!("{}\t{}", k, Self::gist_value(v)))
                .collect::<Vec<_>>()
                .join("\n"),
            Value::Pair(k, v) => format!("{}\t{}", k, Self::gist_value(v)),
            Value::Version { .. } => format!("v{}", value.to_string_value()),
            Value::Nil => "Nil".to_string(),
            _ => value.to_string_value(),
        }
    }

    fn resolve_method(
        &mut self,
        class_name: &str,
        method_name: &str,
        arg_values: &[Value],
    ) -> Option<MethodDef> {
        let mro = self.class_mro(class_name);
        for cn in mro {
            if let Some(overloads) = self
                .classes
                .get(&cn)
                .and_then(|c| c.methods.get(method_name))
                .cloned()
            {
                for def in overloads {
                    if self.method_args_match(arg_values, &def.param_defs) {
                        return Some(def);
                    }
                }
            }
        }
        None
    }

    fn class_mro(&mut self, class_name: &str) -> Vec<String> {
        if let Some(class_def) = self.classes.get(class_name)
            && !class_def.mro.is_empty()
        {
            return class_def.mro.clone();
        }
        let mut stack = Vec::new();
        match self.compute_class_mro(class_name, &mut stack) {
            Ok(mro) => {
                if let Some(class_def) = self.classes.get_mut(class_name) {
                    class_def.mro = mro.clone();
                }
                mro
            }
            Err(_) => vec![class_name.to_string()],
        }
    }

    fn force_lazy_list(&mut self, list: &LazyList) -> Result<Vec<Value>, RuntimeError> {
        if let Some(cached) = list.cache.borrow().clone() {
            return Ok(cached);
        }
        let saved_env = self.env.clone();
        let saved_len = self.gather_items.len();
        self.env = list.env.clone();
        self.gather_items.push(Vec::new());
        let run_res = self.run_block(&list.body);
        let items = self.gather_items.pop().unwrap_or_default();
        while self.gather_items.len() > saved_len {
            self.gather_items.pop();
        }
        let mut merged_env = saved_env;
        for (k, v) in self.env.iter() {
            merged_env.insert(k.clone(), v.clone());
        }
        self.env = merged_env;
        run_res?;
        *list.cache.borrow_mut() = Some(items.clone());
        Ok(items)
    }

    pub(crate) fn list_from_value(&mut self, value: Value) -> Result<Vec<Value>, RuntimeError> {
        Ok(match value {
            Value::Array(items) => items,
            Value::Hash(items) => items
                .into_iter()
                .map(|(k, v)| Value::Pair(k, Box::new(v)))
                .collect(),
            Value::Range(a, b) => (a..=b).map(Value::Int).collect(),
            Value::RangeExcl(a, b) => (a..b).map(Value::Int).collect(),
            Value::RangeExclStart(a, b) => (a + 1..=b).map(Value::Int).collect(),
            Value::RangeExclBoth(a, b) => (a + 1..b).map(Value::Int).collect(),
            Value::LazyList(list) => self.force_lazy_list(&list)?,
            other => vec![other],
        })
    }

    pub(crate) fn force_lazy_list_bridge(
        &mut self,
        list: &crate::value::LazyList,
    ) -> Result<Vec<Value>, RuntimeError> {
        self.force_lazy_list(list)
    }

    fn split_block_phasers(&self, stmts: &[Stmt]) -> (Vec<Stmt>, Vec<Stmt>, Vec<Stmt>) {
        let mut enter_ph = Vec::new();
        let mut leave_ph = Vec::new();
        let mut body_main = Vec::new();
        for stmt in stmts {
            if let Stmt::Phaser { kind, body } = stmt {
                match kind {
                    PhaserKind::Enter => enter_ph.push(Stmt::Block(body.clone())),
                    PhaserKind::Leave => leave_ph.push(Stmt::Block(body.clone())),
                    _ => body_main.push(stmt.clone()),
                }
            } else {
                body_main.push(stmt.clone());
            }
        }
        (enter_ph, leave_ph, body_main)
    }

    #[allow(clippy::type_complexity)]
    fn split_loop_phasers(
        &self,
        stmts: &[Stmt],
    ) -> (
        Vec<Stmt>,
        Vec<Stmt>,
        Vec<Stmt>,
        Vec<Stmt>,
        Vec<Stmt>,
        Vec<Stmt>,
    ) {
        let mut enter_ph = Vec::new();
        let mut leave_ph = Vec::new();
        let mut first_ph = Vec::new();
        let mut next_ph = Vec::new();
        let mut last_ph = Vec::new();
        let mut body_main = Vec::new();
        for stmt in stmts {
            if let Stmt::Phaser { kind, body } = stmt {
                match kind {
                    PhaserKind::Enter => enter_ph.push(Stmt::Block(body.clone())),
                    PhaserKind::Leave => leave_ph.push(Stmt::Block(body.clone())),
                    PhaserKind::First => first_ph.push(Stmt::Block(body.clone())),
                    PhaserKind::Next => next_ph.push(Stmt::Block(body.clone())),
                    PhaserKind::Last => last_ph.push(Stmt::Block(body.clone())),
                    _ => body_main.push(stmt.clone()),
                }
            } else {
                body_main.push(stmt.clone());
            }
        }
        (enter_ph, leave_ph, first_ph, next_ph, last_ph, body_main)
    }

    fn update_instance_target(&mut self, target: &Expr, instance: Value) {
        if let Expr::Var(name) = target {
            self.env.insert(name.clone(), instance);
        }
    }

    fn make_promise_instance(&self, status: &str, result: Value) -> Value {
        let mut attrs = HashMap::new();
        attrs.insert("status".to_string(), Value::Str(status.to_string()));
        attrs.insert("result".to_string(), result);
        Value::make_instance("Promise".to_string(), attrs)
    }

    fn make_supply_instance(&self) -> Value {
        let mut attrs = HashMap::new();
        attrs.insert("values".to_string(), Value::Array(Vec::new()));
        attrs.insert("taps".to_string(), Value::Array(Vec::new()));
        Value::make_instance("Supply".to_string(), attrs)
    }

    /// Peek at the target expression and return the package name if it's a known type.
    fn peek_target_package(&self, target: &Expr) -> Option<String> {
        match target {
            Expr::Var(name) | Expr::BareWord(name) => {
                if self.env.contains_key(name) {
                    None
                } else {
                    Some(name.clone())
                }
            }
            _ => None,
        }
    }

    fn call_sub_value(
        &mut self,
        func: Value,
        args: Vec<Value>,
        merge_all: bool,
    ) -> Result<Value, RuntimeError> {
        if let Value::Sub {
            package,
            name,
            params,
            body,
            env,
            ..
        } = func
        {
            let saved_env = self.env.clone();
            let mut new_env = saved_env.clone();
            for (k, v) in env {
                if merge_all {
                    new_env.entry(k).or_insert(v);
                    continue;
                }
                if matches!(new_env.get(&k), Some(Value::Array(_))) && matches!(v, Value::Array(_))
                {
                    continue;
                }
                new_env.insert(k, v);
            }
            for (i, param_name) in params.iter().enumerate() {
                if let Some(value) = args.get(i) {
                    new_env.insert(param_name.clone(), value.clone());
                }
            }
            let placeholders = collect_placeholders(&body);
            if !placeholders.is_empty() {
                for (i, ph) in placeholders.iter().enumerate() {
                    if let Some(val) = args.get(i) {
                        new_env.insert(ph.clone(), val.clone());
                    }
                }
            }
            let block_sub = Value::Sub {
                package: package.clone(),
                name: name.clone(),
                params: vec![],
                body: body.clone(),
                env: new_env.clone(),
                id: next_instance_id(),
            };
            self.env = new_env;
            self.routine_stack.push((package.clone(), name.clone()));
            self.block_stack.push(block_sub);
            let result = self.eval_block_value(&body);
            self.block_stack.pop();
            self.routine_stack.pop();
            let mut merged = saved_env;
            if merge_all {
                for (k, v) in self.env.iter() {
                    merged.insert(k.clone(), v.clone());
                }
            } else {
                for (k, v) in self.env.iter() {
                    if matches!(v, Value::Array(_)) {
                        merged.insert(k.clone(), v.clone());
                    }
                }
            }
            self.env = merged;
            return match result {
                Err(e) if e.return_value.is_some() => Ok(e.return_value.unwrap()),
                other => other,
            };
        }
        Err(RuntimeError::new("Callable expected"))
    }

    fn compute_class_mro(
        &mut self,
        class_name: &str,
        stack: &mut Vec<String>,
    ) -> Result<Vec<String>, RuntimeError> {
        if stack.iter().any(|name| name == class_name) {
            return Err(RuntimeError::new(format!(
                "C3 MRO cycle detected at {}",
                class_name
            )));
        }
        if let Some(class_def) = self.classes.get(class_name)
            && !class_def.mro.is_empty()
        {
            return Ok(class_def.mro.clone());
        }
        stack.push(class_name.to_string());
        let parents = self
            .classes
            .get(class_name)
            .map(|c| c.parents.clone())
            .unwrap_or_default();
        let mut seqs: Vec<Vec<String>> = Vec::new();
        for parent in &parents {
            if self.classes.contains_key(parent) {
                let mro = self.compute_class_mro(parent, stack)?;
                seqs.push(mro);
            } else {
                seqs.push(vec![parent.clone()]);
            }
        }
        seqs.push(parents.clone());
        let mut result = vec![class_name.to_string()];
        while seqs.iter().any(|s| !s.is_empty()) {
            let mut candidate = None;
            for seq in &seqs {
                if seq.is_empty() {
                    continue;
                }
                let head = &seq[0];
                let mut in_tail = false;
                for other in &seqs {
                    if other.len() > 1 && other[1..].contains(head) {
                        in_tail = true;
                        break;
                    }
                }
                if !in_tail {
                    candidate = Some(head.clone());
                    break;
                }
            }
            if let Some(head) = candidate {
                result.push(head.clone());
                for seq in seqs.iter_mut() {
                    if !seq.is_empty() && seq[0] == head {
                        seq.remove(0);
                    }
                }
            } else {
                stack.pop();
                return Err(RuntimeError::new(format!(
                    "Inconsistent class hierarchy for {}",
                    class_name
                )));
            }
        }
        stack.pop();
        Ok(result)
    }

    fn class_has_method(&mut self, class_name: &str, method_name: &str) -> bool {
        let mro = self.class_mro(class_name);
        for cn in mro {
            if let Some(class_def) = self.classes.get(&cn)
                && class_def.methods.contains_key(method_name)
            {
                return true;
            }
        }
        false
    }

    fn collect_class_attributes(&mut self, class_name: &str) -> Vec<(String, bool, Option<Expr>)> {
        let mro = self.class_mro(class_name);
        let mut attrs: Vec<(String, bool, Option<Expr>)> = Vec::new();
        for cn in mro.iter().rev() {
            if let Some(class_def) = self.classes.get(cn) {
                for attr in &class_def.attributes {
                    if let Some(pos) = attrs.iter().position(|(n, _, _)| n == &attr.0) {
                        attrs.remove(pos);
                    }
                    attrs.push(attr.clone());
                }
            }
        }
        attrs
    }

    fn run_instance_method(
        &mut self,
        class_name: &str,
        mut attributes: HashMap<String, Value>,
        method_name: &str,
        args: Vec<Value>,
    ) -> Result<(Value, HashMap<String, Value>), RuntimeError> {
        let method_def = self
            .resolve_method(class_name, method_name, &args)
            .ok_or_else(|| {
                RuntimeError::new(format!(
                    "No matching candidates for method: {}",
                    method_name
                ))
            })?;
        let base = Value::make_instance(class_name.to_string(), attributes.clone());
        let saved_env = self.env.clone();
        self.env.insert("self".to_string(), base.clone());
        for (attr_name, attr_val) in &attributes {
            self.env.insert(format!("!{}", attr_name), attr_val.clone());
            self.env.insert(format!(".{}", attr_name), attr_val.clone());
        }
        for (i, param) in method_def.params.iter().enumerate() {
            if let Some(val) = args.get(i) {
                self.env.insert(param.clone(), val.clone());
            } else if let Some(pd) = method_def.param_defs.get(i)
                && let Some(default_expr) = &pd.default
            {
                let val = self.eval_expr(default_expr)?;
                self.env.insert(param.clone(), val);
            }
        }
        let block_result = self.run_block(&method_def.body);
        let implicit_return = self.env.get("_").cloned();
        let result = match block_result {
            Ok(()) => Ok(implicit_return.unwrap_or(Value::Nil)),
            Err(e) if e.return_value.is_some() => Ok(e.return_value.unwrap()),
            Err(e) => Err(e),
        };
        for attr_name in attributes.keys().cloned().collect::<Vec<_>>() {
            let env_key = format!("!{}", attr_name);
            if let Some(val) = self.env.get(&env_key) {
                attributes.insert(attr_name, val.clone());
            }
        }
        self.env = saved_env;
        result.map(|v| (v, attributes))
    }

    fn resolve_function_with_arity(&self, name: &str, arity: usize) -> Option<FunctionDef> {
        if name.contains("::") {
            return self.functions.get(name).cloned();
        }
        // Try multi-dispatch with arity first
        let multi_local = format!("{}::{}/{}", self.current_package, name, arity);
        if let Some(def) = self.functions.get(&multi_local) {
            return Some(def.clone());
        }
        let multi_global = format!("GLOBAL::{}/{}", name, arity);
        if let Some(def) = self.functions.get(&multi_global) {
            return Some(def.clone());
        }
        // Fall back to regular lookup
        self.resolve_function(name)
    }

    fn resolve_function_with_types(
        &mut self,
        name: &str,
        arg_values: &[Value],
    ) -> Option<FunctionDef> {
        if name.contains("::") {
            return self.functions.get(name).cloned();
        }
        let arity = arg_values.len();
        let type_sig: Vec<&str> = arg_values
            .iter()
            .map(|v| Self::value_type_name(v))
            .collect();
        let typed_key = format!(
            "{}::{}/{}:{}",
            self.current_package,
            name,
            arity,
            type_sig.join(",")
        );
        if let Some(def) = self.functions.get(&typed_key) {
            return Some(def.clone());
        }
        let typed_global = format!("GLOBAL::{}/{}:{}", name, arity, type_sig.join(","));
        if let Some(def) = self.functions.get(&typed_global) {
            return Some(def.clone());
        }
        // Try matching against all typed candidates for this name/arity
        let prefix_local = format!("{}::{}/{}:", self.current_package, name, arity);
        let prefix_global = format!("GLOBAL::{}/{}:", name, arity);
        let candidates: Vec<FunctionDef> = self
            .functions
            .iter()
            .filter(|(key, _)| key.starts_with(&prefix_local) || key.starts_with(&prefix_global))
            .map(|(_, def)| def.clone())
            .collect();
        for def in candidates {
            if self.args_match_param_types(arg_values, &def.param_defs) {
                return Some(def);
            }
        }
        // Fall back to arity-only if no proto declared
        if self.has_proto(name) {
            None
        } else {
            self.resolve_function_with_arity(name, arity)
        }
    }

    fn eval_token_call(
        &mut self,
        name: &str,
        args: &[Expr],
    ) -> Result<Option<String>, RuntimeError> {
        let defs = match self.resolve_token_defs(name) {
            Some(defs) => defs,
            None => return Ok(None),
        };
        let arg_values: Vec<Value> = args.iter().filter_map(|a| self.eval_expr(a).ok()).collect();
        let literal_args: Vec<Expr> = arg_values.into_iter().map(Expr::Literal).collect();
        let subject = match self.env.get("_") {
            Some(Value::Str(s)) => Some(s.clone()),
            _ => None,
        };
        let mut best: Option<(usize, String)> = None;
        for def in defs {
            if let Some(pattern) = self.eval_token_def(&def, &literal_args)? {
                if let Some(ref text) = subject {
                    if let Some(len) = self.regex_match_len_at_start(&pattern, text) {
                        let better = best
                            .as_ref()
                            .map(|(best_len, _)| len > *best_len)
                            .unwrap_or(true);
                        if better {
                            best = Some((len, pattern));
                        }
                    }
                } else if best.is_none() {
                    best = Some((0, pattern));
                }
            }
        }
        if let Some((_, pattern)) = best {
            return Ok(Some(pattern));
        }
        if self.has_proto_token(name) {
            return Err(RuntimeError::new(format!(
                "No matching candidates for proto token: {}",
                name
            )));
        }
        Ok(None)
    }

    fn eval_token_def(
        &mut self,
        def: &FunctionDef,
        args: &[Expr],
    ) -> Result<Option<String>, RuntimeError> {
        let saved_env = self.env.clone();
        self.bind_function_args(&def.param_defs, &def.params, args)?;
        self.routine_stack
            .push((def.package.clone(), def.name.clone()));
        let result = self.eval_block_value(&def.body);
        self.routine_stack.pop();
        self.env = saved_env;
        let value = match result {
            Ok(v) => v,
            Err(e) if e.return_value.is_some() => e.return_value.unwrap(),
            Err(e) => return Err(e),
        };
        match value {
            Value::Regex(pat) => Ok(Some(pat)),
            Value::Str(s) => Ok(Some(s)),
            Value::Nil => Ok(None),
            other => Ok(Some(other.to_string_value())),
        }
    }

    fn has_proto(&self, name: &str) -> bool {
        if name.contains("::") {
            return self.proto_subs.contains(name);
        }
        let local = format!("{}::{}", self.current_package, name);
        if self.proto_subs.contains(&local) {
            return true;
        }
        self.proto_subs.contains(&format!("GLOBAL::{}", name))
    }

    pub(crate) fn is_known_type_constraint(constraint: &str) -> bool {
        matches!(
            constraint,
            "Int"
                | "Num"
                | "Str"
                | "Bool"
                | "Array"
                | "Hash"
                | "Rat"
                | "FatRat"
                | "Complex"
                | "int"
                | "num"
                | "str"
        )
    }

    fn value_is_nan(value: &Value) -> bool {
        match value {
            Value::Num(f) => f.is_nan(),
            Value::Complex(r, i) => r.is_nan() || i.is_nan(),
            _ => false,
        }
    }

    pub(crate) fn value_type_name(value: &Value) -> &'static str {
        match value {
            Value::Int(_) => "Int",
            Value::BigInt(_) => "Int",
            Value::Num(_) => "Num",
            Value::Str(_) => "Str",
            Value::Bool(_) => "Bool",
            Value::Array(_) => "Array",
            Value::LazyList(_) => "Array",
            Value::Hash(_) => "Hash",
            Value::Range(_, _)
            | Value::RangeExcl(_, _)
            | Value::RangeExclStart(_, _)
            | Value::RangeExclBoth(_, _) => "Range",
            Value::Pair(_, _) => "Pair",
            Value::Rat(_, _) => "Rat",
            Value::FatRat(_, _) => "FatRat",
            Value::Complex(_, _) => "Complex",
            Value::Set(_) => "Set",
            Value::Bag(_) => "Bag",
            Value::Mix(_) => "Mix",
            Value::Nil => "Any",
            Value::Sub { .. } => "Sub",
            Value::Routine { .. } => "Routine",
            Value::Package(_) => "Package",
            Value::CompUnitDepSpec { .. } => "Any",
            Value::Enum { .. } => "Int",
            Value::Instance { .. } => "Any",
            Value::Junction { .. } => "Junction",
            Value::Regex(_) => "Regex",
            Value::Version { .. } => "Version",
            Value::Slip(_) => "Slip",
        }
    }

    fn type_matches(constraint: &str, value_type: &str) -> bool {
        if constraint == "Any" || constraint == "Mu" {
            return true;
        }
        if constraint == value_type {
            return true;
        }
        // Native type aliases: num → Num, int → Int, str → Str
        if constraint == "num" && value_type == "Num" {
            return true;
        }
        if constraint == "int" && value_type == "Int" {
            return true;
        }
        if constraint == "str" && value_type == "Str" {
            return true;
        }
        // Numeric hierarchy: Int is a Numeric, Num is a Numeric
        if constraint == "Numeric"
            && matches!(value_type, "Int" | "Num" | "Rat" | "FatRat" | "Complex")
        {
            return true;
        }
        if constraint == "Real" && matches!(value_type, "Int" | "Num" | "Rat" | "FatRat") {
            return true;
        }
        if constraint == "Cool"
            && matches!(
                value_type,
                "Int" | "Num" | "Str" | "Bool" | "Rat" | "FatRat" | "Complex"
            )
        {
            return true;
        }
        if constraint == "Stringy" && matches!(value_type, "Str") {
            return true;
        }
        // Role-like type relationships
        if constraint == "Positional" && matches!(value_type, "Array" | "List" | "Seq") {
            return true;
        }
        if constraint == "Associative"
            && matches!(value_type, "Hash" | "Map" | "Bag" | "Set" | "Mix")
        {
            return true;
        }
        false
    }

    pub(crate) fn type_matches_value(&mut self, constraint: &str, value: &Value) -> bool {
        if let Some(subset) = self.subsets.get(constraint).cloned() {
            if !self.type_matches_value(&subset.base, value) {
                return false;
            }
            let saved = self.env.get("_").cloned();
            self.env.insert("_".to_string(), value.clone());
            let ok = self
                .eval_expr(&subset.predicate)
                .map(|v| v.truthy())
                .unwrap_or(false);
            if let Some(old) = saved {
                self.env.insert("_".to_string(), old);
            } else {
                self.env.remove("_");
            }
            return ok;
        }
        // Check Instance class name against constraint (including parent classes)
        if let Value::Instance { class_name, .. } = value {
            if Self::type_matches(constraint, class_name) {
                return true;
            }
            // Check parent classes of the instance
            if let Some(class_def) = self.classes.get(class_name.as_str()) {
                for parent in class_def.parents.clone() {
                    if Self::type_matches(constraint, &parent) {
                        return true;
                    }
                }
            }
        }
        let value_type = Self::value_type_name(value);
        Self::type_matches(constraint, value_type)
    }

    fn args_match_param_types(&mut self, args: &[Value], param_defs: &[ParamDef]) -> bool {
        let positional_params: Vec<&ParamDef> = param_defs
            .iter()
            .filter(|p| !p.slurpy && !p.named)
            .collect();
        for (i, pd) in positional_params.iter().enumerate() {
            if let Some(literal) = &pd.literal_value {
                if let Some(arg) = args.get(i) {
                    if arg != literal {
                        return false;
                    }
                } else {
                    return false;
                }
            }
            if let Some(constraint) = &pd.type_constraint
                && let Some(arg) = args.get(i)
                && !self.type_matches_value(constraint, arg)
            {
                return false;
            }
        }
        true
    }

    fn method_args_match(&mut self, args: &[Value], param_defs: &[ParamDef]) -> bool {
        let positional_params: Vec<&ParamDef> = param_defs.iter().filter(|p| !p.named).collect();
        let mut required = 0usize;
        let mut has_slurpy = false;
        for pd in &positional_params {
            if pd.slurpy {
                has_slurpy = true;
            } else {
                required += 1;
            }
        }
        if has_slurpy {
            if args.len() < required {
                return false;
            }
        } else if args.len() != required {
            return false;
        }
        self.args_match_param_types(args, param_defs)
    }

    fn matches_expected(
        &mut self,
        matcher: &ExpectedMatcher,
        actual: &str,
    ) -> Result<bool, RuntimeError> {
        match matcher {
            ExpectedMatcher::Exact(Value::Str(s)) => Ok(actual == s),
            ExpectedMatcher::Exact(Value::Int(i)) => Ok(actual.trim() == i.to_string()),
            ExpectedMatcher::Exact(Value::Bool(b)) => Ok(*b != actual.is_empty()),
            ExpectedMatcher::Exact(Value::Nil) => Ok(actual.is_empty()),
            ExpectedMatcher::Exact(_) => Ok(false),
            ExpectedMatcher::Lambda { param, body } => {
                let parsed = actual.trim().parse::<i64>().ok();
                let arg = parsed
                    .map(Value::Int)
                    .unwrap_or_else(|| Value::Str(actual.to_string()));
                let saved = self.env.insert(param.clone(), arg);
                let result = self.eval_block_value(body);
                if let Some(old) = saved {
                    self.env.insert(param.clone(), old);
                } else {
                    self.env.remove(param);
                }
                Ok(result?.truthy())
            }
        }
    }

    fn positional_arg<'a>(
        &self,
        args: &'a [CallArg],
        index: usize,
        message: &str,
    ) -> Result<&'a Expr, RuntimeError> {
        let mut count = 0;
        for arg in args {
            if let CallArg::Positional(expr) = arg {
                if count == index {
                    return Ok(expr);
                }
                count += 1;
            }
        }
        Err(RuntimeError::new(message))
    }

    fn positional_arg_value(
        &mut self,
        args: &[CallArg],
        index: usize,
    ) -> Result<String, RuntimeError> {
        let mut count = 0;
        for arg in args {
            if let CallArg::Positional(expr) = arg {
                if count == index {
                    return Ok(self.eval_expr(expr)?.to_string_value());
                }
                count += 1;
            }
        }
        Ok(String::new())
    }

    fn named_arg_bool(&mut self, args: &[CallArg], name: &str) -> Result<bool, RuntimeError> {
        for arg in args {
            if let CallArg::Named {
                name: arg_name,
                value,
            } = arg
                && arg_name == name
            {
                if let Some(expr) = value {
                    return Ok(self.eval_expr(expr)?.truthy());
                }
                return Ok(true);
            }
        }
        Ok(false)
    }

    fn named_arg_value(
        &mut self,
        args: &[CallArg],
        name: &str,
    ) -> Result<Option<String>, RuntimeError> {
        for arg in args {
            if let CallArg::Named {
                name: arg_name,
                value,
            } = arg
                && arg_name == name
            {
                if let Some(expr) = value {
                    return Ok(Some(self.eval_expr(expr)?.to_string_value()));
                }
                return Ok(Some(String::new()));
            }
        }
        Ok(None)
    }

    fn test_ok(&mut self, success: bool, desc: &str, todo: bool) -> Result<(), RuntimeError> {
        let state = self.test_state.get_or_insert_with(TestState::new);
        state.ran += 1;
        let forced = state
            .force_todo
            .iter()
            .any(|(start, end)| state.ran >= *start && state.ran <= *end);
        let todo = todo || forced;
        if !success && !todo {
            state.failed += 1;
        }
        let mut line = String::new();
        if success {
            line.push_str("ok ");
        } else {
            line.push_str("not ok ");
        }
        line.push_str(&state.ran.to_string());
        if !desc.is_empty() {
            line.push_str(" - ");
            line.push_str(desc);
        }
        if todo {
            line.push_str(" # TODO");
        }
        line.push('\n');
        self.output.push_str(&line);
        Ok(())
    }

    pub(crate) fn eval_expr(&mut self, expr: &Expr) -> Result<Value, RuntimeError> {
        crate::trace::trace_log!("eval", "eval_expr: {:?}", std::mem::discriminant(expr));
        match expr {
            Expr::Literal(v) => Ok(v.clone()),
            Expr::BareWord(name) => {
                // Check if bare word matches a variable in env (sigilless variables, enums)
                if let Some(val) = self.env.get(name.as_str()) {
                    return Ok(val.clone());
                }
                // Check if it's a class name (return as type object)
                if self.classes.contains_key(name.as_str()) {
                    return Ok(Value::Package(name.clone()));
                }
                // Built-in type names as type objects
                if matches!(
                    name.as_str(),
                    "Hash"
                        | "Array"
                        | "Int"
                        | "Num"
                        | "Rat"
                        | "FatRat"
                        | "Complex"
                        | "Str"
                        | "Bool"
                        | "Pair"
                        | "Map"
                        | "Set"
                        | "Bag"
                        | "Mix"
                        | "List"
                        | "Seq"
                        | "Range"
                        | "Any"
                        | "Mu"
                        | "Cool"
                        | "Real"
                        | "Numeric"
                        | "Stringy"
                        | "Positional"
                        | "Associative"
                        | "Failure"
                        | "Exception"
                        | "Order"
                        | "Version"
                        | "Nil"
                        | "Regex"
                        | "Block"
                        | "Routine"
                        | "Sub"
                        | "Method"
                        | "IO"
                        | "Proc"
                        | "Slip"
                ) {
                    return Ok(Value::Package(name.clone()));
                }
                // Check if bare word is a known function — call it with zero args
                if self.has_function(name.as_str()) {
                    return self.eval_expr(&Expr::Call {
                        name: name.clone(),
                        args: Vec::new(),
                    });
                }
                // NaN and Inf are Num literals
                if name == "NaN" {
                    return Ok(Value::Num(f64::NAN));
                }
                if name == "Inf" {
                    return Ok(Value::Num(f64::INFINITY));
                }
                Ok(Value::Str(name.clone()))
            }
            Expr::StringInterpolation(parts) => {
                let mut result = String::new();
                for part in parts {
                    let val = self.eval_expr(part)?;
                    result.push_str(&val.to_string_value());
                }
                Ok(Value::Str(result))
            }
            Expr::Var(name) => Ok(self.env.get(name).cloned().unwrap_or(Value::Nil)),
            Expr::CaptureVar(name) => Ok(self
                .env
                .get(&format!("<{}>", name))
                .cloned()
                .unwrap_or(Value::Nil)),
            Expr::ArrayVar(name) => Ok(self
                .env
                .get(&format!("@{}", name))
                .cloned()
                .unwrap_or(Value::Nil)),
            Expr::HashVar(name) => {
                let key = format!("%{}", name);
                Ok(self.env.get(&key).cloned().unwrap_or(Value::Nil))
            }
            Expr::CodeVar(name) => {
                // Check if stored as a variable first (my &f = ...)
                let var_key = format!("&{}", name);
                if let Some(val) = self.env.get(&var_key) {
                    return Ok(val.clone());
                }
                // Look up as a function reference (including multi subs)
                let def = self.resolve_function(name).or_else(|| {
                    // Try multi subs: search for any name/arity variant
                    let prefix_local = format!("{}::{}/", self.current_package, name);
                    let prefix_global = format!("GLOBAL::{}/", name);
                    self.functions
                        .iter()
                        .find(|(k, _)| {
                            k.starts_with(&prefix_local) || k.starts_with(&prefix_global)
                        })
                        .map(|(_, v)| v.clone())
                });
                if let Some(def) = def {
                    Ok(Value::Sub {
                        package: def.package,
                        name: def.name,
                        params: def.params,
                        body: def.body,
                        env: self.env.clone(),
                        id: next_instance_id(),
                    })
                } else if Self::is_builtin_function(name) {
                    Ok(Value::Routine {
                        package: "GLOBAL".to_string(),
                        name: name.to_string(),
                    })
                } else {
                    Ok(Value::Nil)
                }
            }
            Expr::RoutineMagic => {
                if let Some((package, name)) = self.routine_stack.last() {
                    Ok(Value::Routine {
                        package: package.clone(),
                        name: name.clone(),
                    })
                } else {
                    Err(RuntimeError::new("X::Undeclared::Symbols"))
                }
            }
            Expr::BlockMagic => {
                if let Some(Value::Sub { .. }) = self.block_stack.last() {
                    Ok(self.block_stack.last().cloned().unwrap_or(Value::Nil))
                } else {
                    Err(RuntimeError::new("X::Undeclared::Symbols"))
                }
            }
            Expr::Block(body) => self.eval_block_expr(body),
            Expr::DoBlock { body, label } => self.eval_do_block_expr(body, label),
            Expr::DoStmt(stmt) => self.eval_do_stmt_expr(stmt),
            Expr::ControlFlow { kind, label } => {
                use crate::ast::ControlFlowKind;
                let mut sig = match kind {
                    ControlFlowKind::Last => RuntimeError::last_signal(),
                    ControlFlowKind::Next => RuntimeError::next_signal(),
                    ControlFlowKind::Redo => RuntimeError::redo_signal(),
                };
                sig.label = label.clone();
                Err(sig)
            }
            Expr::AnonSub(body) => Ok(self.eval_anon_sub_expr(body)),
            Expr::AnonSubParams { params, body } => {
                Ok(self.eval_anon_sub_params_expr(params, body))
            }
            Expr::Lambda { param, body } => Ok(self.eval_lambda_expr(param, body)),
            Expr::ArrayLiteral(items) => {
                let mut values = Vec::new();
                for item in items {
                    let val = self.eval_expr(item)?;
                    // Flatten Slips (including Empty) in list context
                    match val {
                        Value::Slip(elems) => values.extend(elems),
                        other => values.push(other),
                    }
                }
                // Single-element array constructor expands iterables:
                // [^10] => [0,1,2,...,9], [1..5] => [1,2,3,4,5]
                if items.len() == 1 && values.len() == 1 {
                    let val = values.pop().unwrap();
                    return match &val {
                        Value::Range(..)
                        | Value::RangeExcl(..)
                        | Value::RangeExclStart(..)
                        | Value::RangeExclBoth(..) => Ok(Value::Array(Self::value_to_list(&val))),
                        Value::LazyList(list) => Ok(Value::Array(self.force_lazy_list(list)?)),
                        _ => Ok(Value::Array(vec![val])),
                    };
                }
                Ok(Value::Array(values))
            }
            Expr::Index { target, index } => {
                let mut value = self.eval_expr(target)?;
                if let Value::LazyList(list) = &value {
                    value = Value::Array(self.force_lazy_list(list)?);
                }
                let idx = self.eval_expr(index)?;
                match (value, idx) {
                    (Value::Array(items), Value::Int(i)) => {
                        let index = if i < 0 {
                            return Ok(Value::Nil);
                        } else {
                            i as usize
                        };
                        Ok(items.get(index).cloned().unwrap_or(Value::Nil))
                    }
                    (Value::Array(items), Value::Range(a, b)) => {
                        let start = a.max(0) as usize;
                        let end = b.max(-1) as usize;
                        let slice = if start >= items.len() {
                            Vec::new()
                        } else {
                            let end = end.min(items.len().saturating_sub(1));
                            items[start..=end].to_vec()
                        };
                        Ok(Value::Array(slice))
                    }
                    (Value::Array(items), Value::RangeExcl(a, b)) => {
                        let start = a.max(0) as usize;
                        let end_excl = b.max(0) as usize;
                        let slice = if start >= items.len() {
                            Vec::new()
                        } else {
                            let end_excl = end_excl.min(items.len());
                            if start >= end_excl {
                                Vec::new()
                            } else {
                                items[start..end_excl].to_vec()
                            }
                        };
                        Ok(Value::Array(slice))
                    }
                    (Value::Hash(items), Value::Num(f)) if f.is_infinite() && f > 0.0 => {
                        // Whatever slice: %h{*} returns all values
                        let values: Vec<Value> = items.values().cloned().collect();
                        Ok(Value::Array(values))
                    }
                    (Value::Hash(items), Value::Nil) => {
                        // Zen slice: %h{} returns the hash itself
                        Ok(Value::Hash(items))
                    }
                    (Value::Hash(items), Value::Array(keys)) => {
                        // Hash slicing: %hash{"one", "three"} returns array of values
                        let values: Vec<Value> = keys
                            .iter()
                            .map(|k| {
                                let key = k.to_string_value();
                                items.get(&key).cloned().unwrap_or(Value::Nil)
                            })
                            .collect();
                        Ok(Value::Array(values))
                    }
                    (Value::Hash(items), Value::Str(key)) => {
                        Ok(items.get(&key).cloned().unwrap_or(Value::Nil))
                    }
                    (Value::Hash(items), Value::Int(key)) => {
                        Ok(items.get(&key.to_string()).cloned().unwrap_or(Value::Nil))
                    }
                    (Value::Set(s), Value::Str(key)) => Ok(Value::Bool(s.contains(&key))),
                    (Value::Set(s), idx) => Ok(Value::Bool(s.contains(&idx.to_string_value()))),
                    (Value::Bag(b), Value::Str(key)) => Ok(Value::Int(*b.get(&key).unwrap_or(&0))),
                    (Value::Bag(b), idx) => {
                        Ok(Value::Int(*b.get(&idx.to_string_value()).unwrap_or(&0)))
                    }
                    (Value::Mix(m), Value::Str(key)) => {
                        Ok(Value::Num(*m.get(&key).unwrap_or(&0.0)))
                    }
                    (Value::Mix(m), idx) => {
                        Ok(Value::Num(*m.get(&idx.to_string_value()).unwrap_or(&0.0)))
                    }
                    _ => Ok(Value::Nil),
                }
            }
            Expr::IndexAssign {
                target,
                index,
                value,
            } => self.eval_index_assign_expr(target, index, value),
            Expr::AssignExpr { name, expr } => {
                let value = self.eval_expr(expr)?;
                self.env.insert(name.clone(), value.clone());
                Ok(value)
            }
            Expr::EnvIndex(key) => {
                if let Some(value) = std::env::var_os(key) {
                    Ok(Value::Str(value.to_string_lossy().to_string()))
                } else {
                    Ok(Value::Nil)
                }
            }
            Expr::MethodCall {
                target,
                name,
                args,
                modifier,
            } => {
                // Handle .VAR — returns a container object with the variable name
                if name == "VAR" && args.is_empty() {
                    let (var_name, class_name) = match target.as_ref() {
                        Expr::Var(n) => (format!("${}", n), "Scalar"),
                        Expr::ArrayVar(n) => (format!("@{}", n), "Array"),
                        Expr::HashVar(n) => (format!("%{}", n), "Hash"),
                        Expr::CodeVar(n) => (format!("&{}", n), "Scalar"),
                        _ => {
                            // .VAR on non-container is identity
                            let value = self.eval_expr(target)?;
                            return Ok(value);
                        }
                    };
                    let mut attributes = HashMap::new();
                    attributes.insert("name".to_string(), Value::Str(var_name));
                    return Ok(Value::make_instance(class_name.to_string(), attributes));
                }
                // Handle method dispatch modifiers: .?method, .+method, .*method
                if modifier.is_some() {
                    let inner = Expr::MethodCall {
                        target: target.clone(),
                        name: name.clone(),
                        args: args.clone(),
                        modifier: None,
                    };
                    let result = self.eval_expr(&inner);
                    return match modifier {
                        Some('?') => Ok(result.unwrap_or(Value::Nil)),
                        Some('+') => Ok(Value::Array(vec![result?])),
                        Some('*') => Ok(Value::Array(vec![result?])),
                        _ => result,
                    };
                }
                if name == "say" && args.is_empty() {
                    let value = self.eval_expr(target)?;
                    self.output.push_str(&value.to_string_value());
                    self.output.push('\n');
                    return Ok(Value::Nil);
                }
                // Handle Promise class methods: Promise.in, Promise.anyof, Promise.allof
                if (name == "in" || name == "anyof" || name == "allof")
                    && matches!(self.peek_target_package(target), Some(ref p) if p == "Promise")
                {
                    // Evaluate args (but ignore them for our synchronous stub)
                    for arg in args {
                        let _ = self.eval_expr(arg);
                    }
                    return Ok(self.make_promise_instance("Kept", Value::Nil));
                }
                // Handle .new() constructor on class type
                if name == "new" {
                    let base = self.eval_expr(target)?;
                    if let Value::Package(class_name) = &base {
                        if class_name == "Hash" {
                            // Hash.new("a", 1, "b", 2) or Hash.new(:42a, :666b)
                            let mut flat_values = Vec::new();
                            for arg in args {
                                let val = self.eval_expr(arg)?;
                                flat_values.extend(Self::value_to_list(&val));
                            }
                            let mut map = HashMap::new();
                            let mut iter = flat_values.into_iter();
                            while let Some(item) = iter.next() {
                                match item {
                                    Value::Pair(key, boxed_val) => {
                                        map.insert(key, *boxed_val);
                                    }
                                    other => {
                                        let key = other.to_string_value();
                                        let value = iter.next().unwrap_or(Value::Nil);
                                        map.insert(key, value);
                                    }
                                }
                            }
                            return Ok(Value::Hash(map));
                        }
                        if class_name == "Version" {
                            let arg = args
                                .first()
                                .map(|a| self.eval_expr(a))
                                .transpose()?
                                .unwrap_or(Value::Nil);
                            return Ok(Self::version_from_value(arg));
                        }
                        if class_name == "Promise" {
                            return Ok(self.make_promise_instance("Planned", Value::Nil));
                        }
                        if class_name == "Channel" {
                            let mut attrs = HashMap::new();
                            attrs.insert("queue".to_string(), Value::Array(Vec::new()));
                            attrs.insert("closed".to_string(), Value::Bool(false));
                            return Ok(Value::make_instance(class_name.clone(), attrs));
                        }
                        if class_name == "Supply" {
                            return Ok(self.make_supply_instance());
                        }
                        if class_name == "Proc::Async" {
                            let mut cmd = Vec::new();
                            for arg in args {
                                let value = self.eval_expr(arg)?;
                                cmd.push(value);
                            }
                            let mut attrs = HashMap::new();
                            attrs.insert("cmd".to_string(), Value::Array(cmd));
                            attrs.insert("started".to_string(), Value::Bool(false));
                            attrs.insert("stdout".to_string(), self.make_supply_instance());
                            attrs.insert("stderr".to_string(), self.make_supply_instance());
                            return Ok(Value::make_instance(class_name.clone(), attrs));
                        }
                        if class_name == "IO::Path" {
                            let path = args
                                .first()
                                .and_then(|a| self.eval_expr(a).ok())
                                .map(|v| v.to_string_value())
                                .unwrap_or_default();
                            let mut attrs = HashMap::new();
                            attrs.insert("path".to_string(), Value::Str(path));
                            return Ok(Value::make_instance("IO::Path".to_string(), attrs));
                        }
                        if class_name == "IO::Handle" {
                            match name.as_str() {
                                "close" => return Ok(Value::Bool(self.close_handle_value(&base)?)),
                                "get" => {
                                    let line = self.read_line_from_handle_value(&base)?;
                                    // Return Nil (type object) at EOF, Str otherwise
                                    if line.is_empty() {
                                        return Ok(Value::Nil);
                                    }
                                    return Ok(Value::Str(line));
                                }
                                "getc" => {
                                    let bytes = self.read_bytes_from_handle_value(&base, 1)?;
                                    return Ok(Value::Str(
                                        String::from_utf8_lossy(&bytes).to_string(),
                                    ));
                                }
                                "lines" => {
                                    let mut lines = Vec::new();
                                    loop {
                                        let line = self.read_line_from_handle_value(&base)?;
                                        if line.is_empty() {
                                            break;
                                        }
                                        lines.push(Value::Str(line));
                                    }
                                    return Ok(Value::Array(lines));
                                }
                                "words" => {
                                    let mut words = Vec::new();
                                    loop {
                                        let line = self.read_line_from_handle_value(&base)?;
                                        if line.is_empty() {
                                            break;
                                        }
                                        for token in line.split_whitespace() {
                                            words.push(Value::Str(token.to_string()));
                                        }
                                    }
                                    return Ok(Value::Array(words));
                                }
                                "read" => {
                                    let count = args
                                        .first()
                                        .and_then(|e| self.eval_expr(e).ok())
                                        .and_then(|v| match v {
                                            Value::Int(i) if i > 0 => Some(i as usize),
                                            _ => None,
                                        })
                                        .unwrap_or(0);
                                    if count > 0 {
                                        let bytes =
                                            self.read_bytes_from_handle_value(&base, count)?;
                                        return Ok(Value::Str(
                                            String::from_utf8_lossy(&bytes).to_string(),
                                        ));
                                    }
                                    let path = {
                                        let state = self.handle_state_mut(&base)?;
                                        state.path.clone()
                                    };
                                    if let Some(path) = path {
                                        let content = fs::read_to_string(&path).map_err(|err| {
                                            RuntimeError::new(format!(
                                                "Failed to read '{}': {}",
                                                path, err
                                            ))
                                        })?;
                                        return Ok(Value::Str(content));
                                    }
                                    return Ok(Value::Str(String::new()));
                                }
                                "write" => {
                                    let content = args
                                        .first()
                                        .and_then(|e| self.eval_expr(e).ok())
                                        .map(|v| v.to_string_value())
                                        .unwrap_or_default();
                                    self.write_to_handle_value(&base, &content, false)?;
                                    return Ok(Value::Bool(true));
                                }
                                "print" => {
                                    let content = args
                                        .first()
                                        .and_then(|e| self.eval_expr(e).ok())
                                        .map(|v| v.to_string_value())
                                        .unwrap_or_default();
                                    self.write_to_handle_value(&base, &content, false)?;
                                    return Ok(Value::Bool(true));
                                }
                                "say" => {
                                    let content = args
                                        .first()
                                        .and_then(|e| self.eval_expr(e).ok())
                                        .map(|v| v.to_string_value())
                                        .unwrap_or_default();
                                    self.write_to_handle_value(&base, &content, true)?;
                                    return Ok(Value::Bool(true));
                                }
                                "flush" => {
                                    if let Ok(state) = self.handle_state_mut(&base)
                                        && let Some(file) = state.file.as_mut()
                                    {
                                        file.flush().map_err(|err| {
                                            RuntimeError::new(format!(
                                                "Failed to flush handle: {}",
                                                err
                                            ))
                                        })?;
                                    }
                                    return Ok(Value::Bool(true));
                                }
                                "seek" => {
                                    let pos = args
                                        .first()
                                        .and_then(|e| self.eval_expr(e).ok())
                                        .and_then(|v| match v {
                                            Value::Int(i) => Some(i),
                                            _ => None,
                                        })
                                        .unwrap_or(0);
                                    let offset = self.seek_handle_value(&base, pos)?;
                                    return Ok(Value::Int(offset));
                                }
                                "tell" => {
                                    let position = self.tell_handle_value(&base)?;
                                    return Ok(Value::Int(position));
                                }
                                "eof" => {
                                    let at_end = self.handle_eof_value(&base)?;
                                    return Ok(Value::Bool(at_end));
                                }
                                "encoding" => {
                                    if let Some(arg) = args.first() {
                                        let encoding = self.eval_expr(arg)?.to_string_value();
                                        let prev = self
                                            .set_handle_encoding(&base, Some(encoding.clone()))?;
                                        return Ok(Value::Str(prev));
                                    }
                                    let current = self.set_handle_encoding(&base, None)?;
                                    return Ok(Value::Str(current));
                                }
                                "opened" => {
                                    let state = self.handle_state_mut(&base)?;
                                    return Ok(Value::Bool(!state.closed));
                                }
                                "slurp" => {
                                    let path = {
                                        let state = self.handle_state_mut(&base)?;
                                        state.path.clone()
                                    };
                                    if let Some(path) = path {
                                        let content = fs::read_to_string(&path).map_err(|err| {
                                            RuntimeError::new(format!(
                                                "Failed to slurp '{}': {}",
                                                path, err
                                            ))
                                        })?;
                                        return Ok(Value::Str(content));
                                    }
                                    return Ok(Value::Str(String::new()));
                                }
                                "spurt" => {
                                    let content = args
                                        .first()
                                        .and_then(|e| self.eval_expr(e).ok())
                                        .map(|v| v.to_string_value())
                                        .unwrap_or_default();
                                    let path = {
                                        let state = self.handle_state_mut(&base)?;
                                        state.path.clone()
                                    };
                                    if let Some(path) = path {
                                        fs::write(&path, &content).map_err(|err| {
                                            RuntimeError::new(format!(
                                                "Failed to spurt '{}': {}",
                                                path, err
                                            ))
                                        })?;
                                        return Ok(Value::Bool(true));
                                    }
                                    return Ok(Value::Bool(false));
                                }
                                _ => {}
                            }
                        }
                        if class_name == "IO::Spec" {
                            match name.as_str() {
                                "canonpath" => {
                                    let path = args
                                        .first()
                                        .and_then(|e| self.eval_expr(e).ok())
                                        .map(|v| v.to_string_value())
                                        .unwrap_or_default();
                                    let resolved = self.resolve_path(&path);
                                    let canonical = fs::canonicalize(&resolved).unwrap_or(resolved);
                                    return Ok(Value::Str(Self::stringify_path(&canonical)));
                                }
                                "catdir" => {
                                    let parts = self.args_to_strings(args)?;
                                    let mut combined = PathBuf::new();
                                    for part in parts {
                                        combined.push(part);
                                    }
                                    return Ok(Value::Str(Self::stringify_path(&combined)));
                                }
                                "catpath" => {
                                    let volume = args
                                        .first()
                                        .and_then(|e| self.eval_expr(e).ok())
                                        .map(|v| v.to_string_value())
                                        .unwrap_or_default();
                                    let directories = args
                                        .get(1)
                                        .and_then(|e| self.eval_expr(e).ok())
                                        .map(|v| v.to_string_value())
                                        .unwrap_or_default();
                                    let basename = args
                                        .get(2)
                                        .and_then(|e| self.eval_expr(e).ok())
                                        .map(|v| v.to_string_value())
                                        .unwrap_or_default();
                                    let mut combined = PathBuf::new();
                                    if !volume.is_empty() {
                                        combined.push(volume);
                                    }
                                    if !directories.is_empty() {
                                        combined.push(directories);
                                    }
                                    if !basename.is_empty() {
                                        combined.push(basename);
                                    }
                                    return Ok(Value::Str(Self::stringify_path(&combined)));
                                }
                                "splitpath" => {
                                    let path = args
                                        .first()
                                        .and_then(|e| self.eval_expr(e).ok())
                                        .map(|v| v.to_string_value())
                                        .unwrap_or_default();
                                    let pb = Path::new(&path);
                                    let volume = pb
                                        .components()
                                        .next()
                                        .map(|comp| comp.as_os_str().to_string_lossy().to_string())
                                        .unwrap_or_default();
                                    let directory =
                                        pb.parent().map(Self::stringify_path).unwrap_or_default();
                                    let basename = pb
                                        .file_name()
                                        .map(|name| name.to_string_lossy().to_string())
                                        .unwrap_or_default();
                                    return Ok(Value::Array(vec![
                                        Value::Str(volume),
                                        Value::Str(directory),
                                        Value::Str(basename),
                                    ]));
                                }
                                "splitdir" => {
                                    let path = args
                                        .first()
                                        .and_then(|e| self.eval_expr(e).ok())
                                        .map(|v| v.to_string_value())
                                        .unwrap_or_default();
                                    let pb = Path::new(&path);
                                    let mut components = Vec::new();
                                    for component in pb.components() {
                                        components.push(Value::Str(
                                            component.as_os_str().to_string_lossy().to_string(),
                                        ));
                                    }
                                    return Ok(Value::Array(components));
                                }
                                "abs2rel" => {
                                    let path = args
                                        .first()
                                        .and_then(|e| self.eval_expr(e).ok())
                                        .map(|v| v.to_string_value())
                                        .unwrap_or_default();
                                    let base =
                                        if let Some(value) = self.named_arg_string(args, "to")? {
                                            value
                                        } else {
                                            self.get_dynamic_string("$*CWD")
                                                .unwrap_or_else(|| ".".to_string())
                                        };
                                    let path_buf = self.resolve_path(&path);
                                    let base_buf = self.resolve_path(&base);
                                    let relative = path_buf
                                        .strip_prefix(&base_buf)
                                        .map(Self::stringify_path)
                                        .unwrap_or_else(|_| Self::stringify_path(&path_buf));
                                    return Ok(Value::Str(relative));
                                }
                                "rel2abs" => {
                                    let path = args
                                        .first()
                                        .and_then(|e| self.eval_expr(e).ok())
                                        .map(|v| v.to_string_value())
                                        .unwrap_or_default();
                                    let base =
                                        if let Some(value) = self.named_arg_string(args, "from")? {
                                            value
                                        } else {
                                            self.get_dynamic_string("$*CWD")
                                                .unwrap_or_else(|| ".".to_string())
                                        };
                                    let mut combined = self.resolve_path(&base);
                                    combined.push(path);
                                    return Ok(Value::Str(Self::stringify_path(&combined)));
                                }
                                "curdir" => return Ok(Value::Str(".".to_string())),
                                "updir" => return Ok(Value::Str("..".to_string())),
                                "rootdir" => {
                                    let root = if cfg!(windows) { "C:\\" } else { "/" };
                                    return Ok(Value::Str(root.to_string()));
                                }
                                "devnull" => {
                                    let path = if cfg!(windows) { "NUL" } else { "/dev/null" };
                                    return Ok(Value::Str(path.to_string()));
                                }
                                _ => {}
                            }
                        }
                        if self.classes.contains_key(class_name) {
                            let mut attrs = HashMap::new();
                            // Set defaults
                            for (attr_name, _is_public, default) in
                                self.collect_class_attributes(class_name)
                            {
                                let val = if let Some(expr) = default {
                                    self.eval_expr(&expr)?
                                } else {
                                    Value::Nil
                                };
                                attrs.insert(attr_name, val);
                            }
                            // Apply named arguments from constructor
                            let mut eval_args = Vec::new();
                            for arg in args {
                                eval_args.push(self.eval_expr(arg)?);
                            }
                            for val in &eval_args {
                                if let Value::Pair(k, v) = val {
                                    attrs.insert(k.clone(), *v.clone());
                                }
                            }
                            if self.class_has_method(class_name, "BUILD") {
                                let (_v, updated) = self.run_instance_method(
                                    class_name,
                                    attrs,
                                    "BUILD",
                                    Vec::new(),
                                )?;
                                attrs = updated;
                            }
                            if self.class_has_method(class_name, "TWEAK") {
                                let (_v, updated) = self.run_instance_method(
                                    class_name,
                                    attrs,
                                    "TWEAK",
                                    Vec::new(),
                                )?;
                                attrs = updated;
                            }
                            return Ok(Value::make_instance(class_name.clone(), attrs));
                        }
                    }
                    // .new called on a non-Package value
                    match &base {
                        Value::Str(s) if matches!(target.as_ref(), Expr::BareWord(_)) => {
                            // BareWord.new: the name was unknown to the parser.
                            // If it's also not a known class/type, error.
                            if !self.classes.contains_key(s.as_str())
                                && !matches!(
                                    s.as_str(),
                                    "Str"
                                        | "Int"
                                        | "Num"
                                        | "Bool"
                                        | "Array"
                                        | "Hash"
                                        | "Rat"
                                        | "FatRat"
                                        | "Complex"
                                        | "Set"
                                        | "Bag"
                                        | "Mix"
                                        | "Uni"
                                        | "Pair"
                                        | "Range"
                                        | "Map"
                                        | "Seq"
                                        | "Junction"
                                        | "Version"
                                        | "Exception"
                                        | "Failure"
                                        | "IO::Path"
                                        | "Regex"
                                        | "Code"
                                        | "Sub"
                                        | "Method"
                                        | "Block"
                                        | "Routine"
                                        | "CompUnit::DependencySpecification"
                                )
                            {
                                return Err(RuntimeError::new(format!(
                                    "Undeclared name:\n    {} used",
                                    s
                                )));
                            }
                            // Known type as BareWord: fall through to class dispatch
                        }
                        Value::Str(_) => {
                            // $x.new where $x is a Str: always creates a new Str
                            return Ok(Value::Str(String::new()));
                        }
                        Value::Int(_) => return Ok(Value::Int(0)),
                        Value::Num(_) => return Ok(Value::Num(0.0)),
                        Value::Bool(_) => return Ok(Value::Bool(false)),
                        _ => {}
                    }
                }
                // Handle method calls on instances
                {
                    let base = self.eval_expr(target)?;
                    if let Value::Instance {
                        class_name,
                        attributes,
                        ..
                    } = &base
                    {
                        if class_name == "Distro" {
                            match name.as_str() {
                                "name" | "auth" | "desc" | "release" | "path-sep" | "is-win"
                                | "version" | "signature" => {
                                    return Ok(attributes
                                        .get(name.as_str())
                                        .cloned()
                                        .unwrap_or(Value::Nil));
                                }
                                "gist" => {
                                    let n = attributes
                                        .get("name")
                                        .map(|v| v.to_string_value())
                                        .unwrap_or_default();
                                    let v = attributes
                                        .get("version")
                                        .map(|v| {
                                            if let Value::Version { parts, .. } = v {
                                                Value::version_parts_to_string(parts)
                                            } else {
                                                v.to_string_value()
                                            }
                                        })
                                        .unwrap_or_default();
                                    return Ok(Value::Str(format!("{} ({})", n, v)));
                                }
                                "Str" => {
                                    let n = attributes
                                        .get("name")
                                        .map(|v| v.to_string_value())
                                        .unwrap_or_default();
                                    return Ok(Value::Str(n));
                                }
                                "raku" | "perl" => {
                                    let release = attributes
                                        .get("release")
                                        .map(|v| v.to_string_value())
                                        .unwrap_or_default();
                                    let path_sep = attributes
                                        .get("path-sep")
                                        .map(|v| v.to_string_value())
                                        .unwrap_or_default();
                                    let n = attributes
                                        .get("name")
                                        .map(|v| v.to_string_value())
                                        .unwrap_or_default();
                                    let auth = attributes
                                        .get("auth")
                                        .map(|v| v.to_string_value())
                                        .unwrap_or_default();
                                    let ver = attributes
                                        .get("version")
                                        .map(|v| {
                                            if let Value::Version { parts, .. } = v {
                                                format!(
                                                    "v{}",
                                                    Value::version_parts_to_string(parts)
                                                )
                                            } else {
                                                v.to_string_value()
                                            }
                                        })
                                        .unwrap_or_default();
                                    let desc = attributes
                                        .get("desc")
                                        .map(|v| v.to_string_value())
                                        .unwrap_or_default();
                                    return Ok(Value::Str(format!(
                                        "Distro.new(release => \"{}\", path-sep => \"{}\", name => \"{}\", auth => \"{}\", version => {}, signature => Blob, desc => \"{}\")",
                                        release, path_sep, n, auth, ver, desc
                                    )));
                                }
                                _ => {}
                            }
                        }
                        if class_name == "Perl"
                            && let Some(val) = attributes.get(name.as_str())
                        {
                            return Ok(val.clone());
                        }
                        if class_name == "Promise" {
                            if name == "keep" {
                                let value = args
                                    .first()
                                    .and_then(|arg| self.eval_expr(arg).ok())
                                    .unwrap_or(Value::Nil);
                                let mut attrs = attributes.clone();
                                attrs.insert("result".to_string(), value);
                                attrs.insert("status".to_string(), Value::Str("Kept".to_string()));
                                let updated = Value::make_instance(class_name.clone(), attrs);
                                self.update_instance_target(target.as_ref(), updated);
                                return Ok(Value::Nil);
                            }
                            if name == "result" && args.is_empty() {
                                return Ok(attributes.get("result").cloned().unwrap_or(Value::Nil));
                            }
                            if name == "status" && args.is_empty() {
                                return Ok(attributes
                                    .get("status")
                                    .cloned()
                                    .unwrap_or(Value::Str("Planned".to_string())));
                            }
                            if name == "then" {
                                let block = args
                                    .first()
                                    .and_then(|arg| self.eval_expr(arg).ok())
                                    .unwrap_or(Value::Nil);
                                let status = attributes
                                    .get("status")
                                    .cloned()
                                    .unwrap_or(Value::Str("Planned".to_string()));
                                if matches!(status, Value::Str(ref s) if s == "Kept") {
                                    let value =
                                        attributes.get("result").cloned().unwrap_or(Value::Nil);
                                    let result = self.call_sub_value(block, vec![value], false)?;
                                    return Ok(self.make_promise_instance("Kept", result));
                                }
                                return Ok(self.make_promise_instance("Planned", Value::Nil));
                            }
                        }
                        if class_name == "Channel" {
                            if name == "send" {
                                let value = args
                                    .first()
                                    .and_then(|arg| self.eval_expr(arg).ok())
                                    .unwrap_or(Value::Nil);
                                let mut attrs = attributes.clone();
                                match attrs.get_mut("queue") {
                                    Some(Value::Array(items)) => items.push(value),
                                    _ => {
                                        attrs
                                            .insert("queue".to_string(), Value::Array(vec![value]));
                                    }
                                }
                                let updated = Value::make_instance(class_name.clone(), attrs);
                                self.update_instance_target(target.as_ref(), updated);
                                return Ok(Value::Nil);
                            }
                            if name == "receive" && args.is_empty() {
                                let mut attrs = attributes.clone();
                                let mut value = Value::Nil;
                                if let Some(Value::Array(items)) = attrs.get_mut("queue")
                                    && !items.is_empty()
                                {
                                    value = items.remove(0);
                                }
                                let updated = Value::make_instance(class_name.clone(), attrs);
                                self.update_instance_target(target.as_ref(), updated);
                                return Ok(value);
                            }
                            if name == "close" && args.is_empty() {
                                let mut attrs = attributes.clone();
                                attrs.insert("closed".to_string(), Value::Bool(true));
                                let updated = Value::make_instance(class_name.clone(), attrs);
                                self.update_instance_target(target.as_ref(), updated);
                                return Ok(Value::Nil);
                            }
                            if name == "closed" && args.is_empty() {
                                return Ok(attributes
                                    .get("closed")
                                    .cloned()
                                    .unwrap_or(Value::Bool(false)));
                            }
                        }
                        if class_name == "Supply" {
                            if name == "emit" {
                                let value = args
                                    .first()
                                    .and_then(|arg| self.eval_expr(arg).ok())
                                    .unwrap_or(Value::Nil);
                                let mut attrs = attributes.clone();
                                if let Some(Value::Array(items)) = attrs.get_mut("values") {
                                    items.push(value.clone());
                                } else {
                                    attrs.insert(
                                        "values".to_string(),
                                        Value::Array(vec![value.clone()]),
                                    );
                                }
                                if let Some(Value::Array(taps)) = attrs.get_mut("taps") {
                                    for tap in taps.clone() {
                                        let _ = self.call_sub_value(tap, vec![value.clone()], true);
                                    }
                                }
                                let updated = Value::make_instance(class_name.clone(), attrs);
                                self.update_instance_target(target.as_ref(), updated);
                                return Ok(Value::Nil);
                            }
                            if name == "tap" {
                                let tap = args
                                    .first()
                                    .and_then(|arg| self.eval_expr(arg).ok())
                                    .unwrap_or(Value::Nil);
                                let mut attrs = attributes.clone();
                                if let Some(Value::Array(items)) = attrs.get_mut("taps") {
                                    items.push(tap.clone());
                                } else {
                                    attrs.insert(
                                        "taps".to_string(),
                                        Value::Array(vec![tap.clone()]),
                                    );
                                }
                                if let Some(Value::Array(values)) = attrs.get("values") {
                                    for v in values {
                                        let _ =
                                            self.call_sub_value(tap.clone(), vec![v.clone()], true);
                                    }
                                }
                                let updated = Value::make_instance(class_name.clone(), attrs);
                                self.update_instance_target(target.as_ref(), updated);
                                return Ok(Value::Nil);
                            }
                        }
                        if class_name == "Proc::Async" {
                            if name == "command" && args.is_empty() {
                                return Ok(attributes
                                    .get("cmd")
                                    .cloned()
                                    .unwrap_or(Value::Array(Vec::new())));
                            }
                            if name == "started" && args.is_empty() {
                                return Ok(attributes
                                    .get("started")
                                    .cloned()
                                    .unwrap_or(Value::Bool(false)));
                            }
                            if name == "stdout" && args.is_empty() {
                                return Ok(attributes.get("stdout").cloned().unwrap_or(Value::Nil));
                            }
                            if name == "stderr" && args.is_empty() {
                                return Ok(attributes.get("stderr").cloned().unwrap_or(Value::Nil));
                            }
                            if name == "start" && args.is_empty() {
                                let mut attrs = attributes.clone();
                                attrs.insert("started".to_string(), Value::Bool(true));
                                let updated = Value::make_instance(class_name.clone(), attrs);
                                self.update_instance_target(target.as_ref(), updated);
                                return Ok(self.make_promise_instance("Kept", Value::Int(0)));
                            }
                        }
                        let mut attrs = attributes.clone();
                        if name == "clone" {
                            for arg in args {
                                let val = self.eval_expr(arg)?;
                                if let Value::Pair(key, boxed) = val {
                                    attrs.insert(key, *boxed);
                                }
                            }
                            return Ok(Value::make_instance(class_name.clone(), attrs));
                        }
                        if name == "isa" {
                            let target = args
                                .first()
                                .and_then(|arg| self.eval_expr(arg).ok())
                                .unwrap_or(Value::Nil);
                            let target_name = match target {
                                Value::Package(name) => name,
                                Value::Str(name) => name,
                                Value::Instance { class_name, .. } => class_name,
                                other => other.to_string_value(),
                            };
                            let ok = self.class_mro(class_name).contains(&target_name);
                            return Ok(Value::Bool(ok));
                        }
                        if name == "raku" || name == "perl" {
                            return Ok(Value::Str(format!("{}.new()", class_name)));
                        }
                        if class_name == "IO::Path" {
                            let p = attributes
                                .get("path")
                                .map(|v| v.to_string_value())
                                .unwrap_or_default();
                            let path_buf = self.resolve_path(&p);
                            let cwd_path = self.get_cwd_path();
                            let original = Path::new(&p);
                            match name.as_str() {
                                "Str" | "gist" => return Ok(Value::Str(p.clone())),
                                "IO" => return Ok(base.clone()),
                                "basename" => {
                                    let bname = original
                                        .file_name()
                                        .map(|s| s.to_string_lossy().to_string())
                                        .unwrap_or_default();
                                    return Ok(Value::Str(bname));
                                }
                                "parent" => {
                                    let mut levels = 1i64;
                                    if let Some(arg) = args.first()
                                        && let Value::Int(i) = self.eval_expr(arg)?
                                    {
                                        levels = i.max(1);
                                    }
                                    let mut path = p.clone();
                                    for _ in 0..levels {
                                        if let Some(par) = Path::new(&path).parent() {
                                            let s = par.to_string_lossy().to_string();
                                            if s.is_empty() {
                                                path = ".".to_string();
                                                break;
                                            }
                                            path = s;
                                        } else {
                                            path = ".".to_string();
                                            break;
                                        }
                                    }
                                    return Ok(self.make_io_path_instance(&path));
                                }
                                "child" | "add" => {
                                    let child_name = args
                                        .first()
                                        .and_then(|a| self.eval_expr(a).ok())
                                        .map(|v| v.to_string_value())
                                        .unwrap_or_default();
                                    let joined = Self::stringify_path(&original.join(&child_name));
                                    return Ok(self.make_io_path_instance(&joined));
                                }
                                "extension" => {
                                    let ext = original
                                        .extension()
                                        .map(|s| s.to_string_lossy().to_string())
                                        .unwrap_or_default();
                                    return Ok(Value::Str(ext));
                                }
                                "absolute" => {
                                    let absolute = Self::stringify_path(&path_buf);
                                    return Ok(self.make_io_path_instance(&absolute));
                                }
                                "relative" => {
                                    let rel = path_buf
                                        .strip_prefix(&cwd_path)
                                        .map(Self::stringify_path)
                                        .unwrap_or_else(|_| Self::stringify_path(&path_buf));
                                    return Ok(Value::Str(rel));
                                }
                                "resolve" => {
                                    let canonical = fs::canonicalize(&path_buf).map_err(|err| {
                                        RuntimeError::new(format!(
                                            "Failed to resolve '{}': {}",
                                            p, err
                                        ))
                                    })?;
                                    let resolved = Self::stringify_path(&canonical);
                                    return Ok(self.make_io_path_instance(&resolved));
                                }
                                "volume" => {
                                    let volume = path_buf
                                        .components()
                                        .next()
                                        .map(|comp| comp.as_os_str().to_string_lossy().to_string())
                                        .unwrap_or_default();
                                    return Ok(Value::Str(volume));
                                }
                                "is-absolute" => return Ok(Value::Bool(original.is_absolute())),
                                "is-relative" => return Ok(Value::Bool(!original.is_absolute())),
                                "e" => return Ok(Value::Bool(path_buf.exists())),
                                "f" => return Ok(Value::Bool(path_buf.is_file())),
                                "d" => return Ok(Value::Bool(path_buf.is_dir())),
                                "l" => {
                                    let linked = fs::symlink_metadata(&path_buf)
                                        .map(|meta| meta.file_type().is_symlink())
                                        .unwrap_or(false);
                                    return Ok(Value::Bool(linked));
                                }
                                "r" => {
                                    let readable = fs::metadata(&path_buf).is_ok();
                                    return Ok(Value::Bool(readable));
                                }
                                "w" => {
                                    let writable = fs::metadata(&path_buf)
                                        .map(|meta| !meta.permissions().readonly())
                                        .unwrap_or(false);
                                    return Ok(Value::Bool(writable));
                                }
                                "x" => {
                                    let executable = fs::metadata(&path_buf)
                                        .map(|meta| Self::metadata_is_executable(&meta))
                                        .unwrap_or(false);
                                    return Ok(Value::Bool(executable));
                                }
                                "s" => {
                                    let size = fs::metadata(&path_buf)
                                        .map(|meta| meta.len())
                                        .map_err(|err| {
                                            RuntimeError::new(format!(
                                                "Failed to stat '{}': {}",
                                                p, err
                                            ))
                                        })?;
                                    return Ok(Value::Int(size as i64));
                                }
                                "z" => {
                                    let zero = fs::metadata(&path_buf)
                                        .map(|meta| meta.len() == 0)
                                        .unwrap_or(false);
                                    return Ok(Value::Bool(zero));
                                }
                                "modified" => {
                                    let ts = fs::metadata(&path_buf)
                                        .and_then(|meta| meta.modified())
                                        .map(Self::system_time_to_int)
                                        .map_err(|err| {
                                            RuntimeError::new(format!(
                                                "Failed to get modified time '{}': {}",
                                                p, err
                                            ))
                                        })?;
                                    return Ok(Value::Int(ts));
                                }
                                "accessed" => {
                                    let ts = fs::metadata(&path_buf)
                                        .and_then(|meta| meta.accessed())
                                        .map(Self::system_time_to_int)
                                        .map_err(|err| {
                                            RuntimeError::new(format!(
                                                "Failed to get accessed time '{}': {}",
                                                p, err
                                            ))
                                        })?;
                                    return Ok(Value::Int(ts));
                                }
                                "changed" => {
                                    let ts = fs::metadata(&path_buf)
                                        .and_then(|meta| meta.modified())
                                        .map(Self::system_time_to_int)
                                        .map_err(|err| {
                                            RuntimeError::new(format!(
                                                "Failed to get changed time '{}': {}",
                                                p, err
                                            ))
                                        })?;
                                    return Ok(Value::Int(ts));
                                }
                                "lines" => {
                                    let content = fs::read_to_string(&path_buf).map_err(|err| {
                                        RuntimeError::new(format!(
                                            "Failed to read '{}': {}",
                                            p, err
                                        ))
                                    })?;
                                    let parts = content
                                        .lines()
                                        .map(|line| Value::Str(line.to_string()))
                                        .collect();
                                    return Ok(Value::Array(parts));
                                }
                                "words" => {
                                    let content = fs::read_to_string(&path_buf).map_err(|err| {
                                        RuntimeError::new(format!(
                                            "Failed to read '{}': {}",
                                            p, err
                                        ))
                                    })?;
                                    let parts = content
                                        .split_whitespace()
                                        .map(|token| Value::Str(token.to_string()))
                                        .collect();
                                    return Ok(Value::Array(parts));
                                }
                                "open" => {
                                    let (read, write, append) = self.parse_io_flags(args)?;
                                    return self.open_file_handle(&path_buf, read, write, append);
                                }
                                "copy" => {
                                    let dest = args.first().ok_or_else(|| {
                                        RuntimeError::new("copy requires destination")
                                    })?;
                                    let dest_path_str = self.eval_expr(dest)?.to_string_value();
                                    let dest_buf = self.resolve_path(&dest_path_str);
                                    fs::copy(&path_buf, &dest_buf).map_err(|err| {
                                        RuntimeError::new(format!(
                                            "Failed to copy '{}': {}",
                                            p, err
                                        ))
                                    })?;
                                    return Ok(Value::Bool(true));
                                }
                                "rename" | "move" => {
                                    let dest = args.first().ok_or_else(|| {
                                        RuntimeError::new("rename/move requires destination")
                                    })?;
                                    let dest_path_str = self.eval_expr(dest)?.to_string_value();
                                    let dest_buf = self.resolve_path(&dest_path_str);
                                    fs::rename(&path_buf, &dest_buf).map_err(|err| {
                                        RuntimeError::new(format!(
                                            "Failed to rename '{}': {}",
                                            p, err
                                        ))
                                    })?;
                                    return Ok(Value::Bool(true));
                                }
                                "chmod" => {
                                    let mode_arg = args
                                        .first()
                                        .ok_or_else(|| RuntimeError::new("chmod requires mode"))?;
                                    let mode_value = self.eval_expr(mode_arg)?;
                                    let mode_int = match mode_value {
                                        Value::Int(i) => i as u32,
                                        Value::Str(s) => u32::from_str_radix(&s, 8).unwrap_or(0),
                                        other => {
                                            return Err(RuntimeError::new(format!(
                                                "Invalid mode: {}",
                                                other.to_string_value()
                                            )));
                                        }
                                    };
                                    #[cfg(unix)]
                                    {
                                        let perms = PermissionsExt::from_mode(mode_int);
                                        fs::set_permissions(&path_buf, perms).map_err(|err| {
                                            RuntimeError::new(format!(
                                                "Failed to chmod '{}': {}",
                                                p, err
                                            ))
                                        })?;
                                    }
                                    #[cfg(not(unix))]
                                    {
                                        return Err(RuntimeError::new(
                                            "chmod not supported on this platform",
                                        ));
                                    }
                                    return Ok(Value::Bool(true));
                                }
                                "mkdir" => {
                                    fs::create_dir_all(&path_buf).map_err(|err| {
                                        RuntimeError::new(format!(
                                            "Failed to mkdir '{}': {}",
                                            p, err
                                        ))
                                    })?;
                                    return Ok(Value::Bool(true));
                                }
                                "rmdir" => {
                                    fs::remove_dir(&path_buf).map_err(|err| {
                                        RuntimeError::new(format!(
                                            "Failed to rmdir '{}': {}",
                                            p, err
                                        ))
                                    })?;
                                    return Ok(Value::Bool(true));
                                }
                                "dir" => {
                                    let mut entries = Vec::new();
                                    for entry in fs::read_dir(&path_buf).map_err(|err| {
                                        RuntimeError::new(format!(
                                            "Failed to read dir '{}': {}",
                                            p, err
                                        ))
                                    })? {
                                        let entry = entry.map_err(|err| {
                                            RuntimeError::new(format!(
                                                "Failed to read dir entry '{}': {}",
                                                p, err
                                            ))
                                        })?;
                                        entries.push(Value::Str(
                                            entry.path().to_string_lossy().to_string(),
                                        ));
                                    }
                                    return Ok(Value::Array(entries));
                                }
                                "slurp" => {
                                    let content = fs::read_to_string(&path_buf).map_err(|err| {
                                        RuntimeError::new(format!(
                                            "Failed to slurp '{}': {}",
                                            p, err
                                        ))
                                    })?;
                                    return Ok(Value::Str(content));
                                }
                                "spurt" => {
                                    let content = args
                                        .first()
                                        .and_then(|a| self.eval_expr(a).ok())
                                        .map(|v| v.to_string_value())
                                        .unwrap_or_default();
                                    fs::write(&path_buf, &content).map_err(|err| {
                                        RuntimeError::new(format!(
                                            "Failed to spurt '{}': {}",
                                            p, err
                                        ))
                                    })?;
                                    return Ok(Value::Bool(true));
                                }
                                "unlink" => {
                                    fs::remove_file(&path_buf).map_err(|err| {
                                        RuntimeError::new(format!(
                                            "Failed to unlink '{}': {}",
                                            p, err
                                        ))
                                    })?;
                                    return Ok(Value::Bool(true));
                                }
                                _ => {}
                            }
                        }
                        // Handle IO::Handle instance methods
                        if class_name == "IO::Handle" {
                            match name.as_str() {
                                "close" => return Ok(Value::Bool(self.close_handle_value(&base)?)),
                                "get" => {
                                    let line = self.read_line_from_handle_value(&base)?;
                                    // Return Nil (type object) at EOF, Str otherwise
                                    if line.is_empty() {
                                        return Ok(Value::Nil);
                                    }
                                    return Ok(Value::Str(line));
                                }
                                "getc" => {
                                    let bytes = self.read_bytes_from_handle_value(&base, 1)?;
                                    return Ok(Value::Str(
                                        String::from_utf8_lossy(&bytes).to_string(),
                                    ));
                                }
                                "lines" => {
                                    let mut lines = Vec::new();
                                    loop {
                                        let line = self.read_line_from_handle_value(&base)?;
                                        if line.is_empty() {
                                            break;
                                        }
                                        lines.push(Value::Str(line));
                                    }
                                    return Ok(Value::Array(lines));
                                }
                                "eof" => {
                                    let at_end = self.handle_eof_value(&base)?;
                                    return Ok(Value::Bool(at_end));
                                }
                                "opened" => {
                                    let state = self.handle_state_mut(&base)?;
                                    return Ok(Value::Bool(!state.closed));
                                }
                                _ => {}
                            }
                        }
                        // Check for accessor methods (public attributes)
                        if args.is_empty() {
                            for (attr_name, is_public, _) in
                                self.collect_class_attributes(class_name)
                            {
                                if is_public && attr_name == *name {
                                    return Ok(attributes.get(name).cloned().unwrap_or(Value::Nil));
                                }
                            }
                        }
                        // Look up method in class hierarchy
                        let mut eval_args = Vec::new();
                        for arg in args {
                            eval_args.push(self.eval_expr(arg)?);
                        }
                        if let Some(method_def) = self.resolve_method(class_name, name, &eval_args)
                        {
                            let class_name_owned = class_name.clone();
                            let original_attrs = attributes.clone();
                            let mut saved_env = self.env.clone();
                            // Bind self
                            self.env.insert("self".to_string(), base.clone());
                            // Bind attributes as $!name and $.name
                            for (attr_name, attr_val) in attributes {
                                self.env.insert(format!("!{}", attr_name), attr_val.clone());
                                self.env.insert(format!(".{}", attr_name), attr_val.clone());
                            }
                            // Bind method parameters
                            for (i, param) in method_def.params.iter().enumerate() {
                                if let Some(val) = eval_args.get(i) {
                                    self.env.insert(param.clone(), val.clone());
                                } else if let Some(pd) = method_def.param_defs.get(i)
                                    && let Some(default_expr) = &pd.default
                                {
                                    let val = self.eval_expr(default_expr)?;
                                    self.env.insert(param.clone(), val);
                                }
                            }
                            let block_result = self.run_block(&method_def.body);
                            let implicit_return = self.env.get("_").cloned();
                            let result = match block_result {
                                Ok(()) => Ok(implicit_return.unwrap_or(Value::Nil)),
                                Err(e) if e.return_value.is_some() => Ok(e.return_value.unwrap()),
                                Err(e) => Err(e),
                            };
                            // Propagate $!attr mutations back to the instance
                            let mut updated_attrs = original_attrs;
                            for attr_name in updated_attrs.keys().cloned().collect::<Vec<_>>() {
                                let env_key = format!("!{}", attr_name);
                                if let Some(val) = self.env.get(&env_key) {
                                    updated_attrs.insert(attr_name, val.clone());
                                }
                            }
                            let updated_instance =
                                Value::make_instance(class_name_owned, updated_attrs);
                            if let Expr::Var(var_name) = target.as_ref() {
                                saved_env.insert(var_name.clone(), updated_instance);
                            }
                            self.env = saved_env;
                            return result;
                        }
                        if self.class_has_method(class_name, name) {
                            return Err(RuntimeError::new(format!(
                                "No matching candidates for method: {}",
                                name
                            )));
                        }
                        // Fall through to generic method handling (WHAT, defined, etc.)
                    }
                }
                if let Expr::ArrayVar(var_name) = target.as_ref() {
                    let key = format!("@{}", var_name);
                    match name.as_str() {
                        "push" => {
                            let value = args
                                .first()
                                .and_then(|arg| self.eval_expr(arg).ok())
                                .unwrap_or(Value::Nil);
                            if let Some(Value::Array(items)) = self.env.get_mut(&key) {
                                items.push(value);
                            } else {
                                self.env.insert(key, Value::Array(vec![value]));
                            }
                            return Ok(Value::Nil);
                        }
                        "pop" => {
                            if let Some(Value::Array(items)) = self.env.get_mut(&key) {
                                return Ok(items.pop().unwrap_or(Value::Nil));
                            }
                            return Ok(Value::Nil);
                        }
                        "shift" => {
                            if let Some(Value::Array(items)) = self.env.get_mut(&key) {
                                if items.is_empty() {
                                    return Ok(Value::Nil);
                                }
                                return Ok(items.remove(0));
                            }
                            return Ok(Value::Nil);
                        }
                        "unshift" => {
                            let value = args
                                .first()
                                .and_then(|arg| self.eval_expr(arg).ok())
                                .unwrap_or(Value::Nil);
                            if let Some(Value::Array(items)) = self.env.get_mut(&key) {
                                items.insert(0, value);
                            }
                            return Ok(Value::Nil);
                        }
                        "splice" => {
                            // Evaluate args before borrowing env mutably
                            let start_val = args.first().map(|a| self.eval_expr(a)).transpose()?;
                            let count_val = args.get(1).map(|a| self.eval_expr(a)).transpose()?;
                            let new_val = args.get(2).map(|a| self.eval_expr(a)).transpose()?;

                            if let Some(Value::Array(items)) = self.env.get_mut(&key) {
                                let start = start_val
                                    .and_then(|v| match v {
                                        Value::Int(i) => Some(i.max(0) as usize),
                                        _ => None,
                                    })
                                    .unwrap_or(0)
                                    .min(items.len());
                                let count = count_val
                                    .and_then(|v| match v {
                                        Value::Int(i) => Some(i.max(0) as usize),
                                        _ => None,
                                    })
                                    .unwrap_or(items.len().saturating_sub(start));
                                let end = (start + count).min(items.len());
                                let removed: Vec<Value> = items.drain(start..end).collect();
                                // Insert new elements if provided
                                if let Some(new_v) = new_val {
                                    match new_v {
                                        Value::Array(new_items) => {
                                            for (i, item) in new_items.into_iter().enumerate() {
                                                items.insert(start + i, item);
                                            }
                                        }
                                        other => {
                                            items.insert(start, other);
                                        }
                                    }
                                }
                                return Ok(Value::Array(removed));
                            }
                            return Ok(Value::Array(vec![]));
                        }
                        "append" => {
                            let mut new_items = Vec::new();
                            for arg in args {
                                let val = self.eval_expr(arg)?;
                                match val {
                                    Value::Array(items) => new_items.extend(items),
                                    other => new_items.push(other),
                                }
                            }
                            if let Some(Value::Array(items)) = self.env.get_mut(&key) {
                                items.extend(new_items);
                            }
                            return Ok(Value::Nil);
                        }
                        "join" => {
                            let sep = args
                                .first()
                                .and_then(|arg| self.eval_expr(arg).ok())
                                .map(|v| v.to_string_value())
                                .unwrap_or_default();
                            if let Some(Value::Array(items)) = self.env.get(&key) {
                                let joined = items
                                    .iter()
                                    .map(|v| v.to_string_value())
                                    .collect::<Vec<_>>()
                                    .join(&sep);
                                return Ok(Value::Str(joined));
                            }
                            return Ok(Value::Str(String::new()));
                        }
                        _ => {}
                    }
                }
                let mut base = self.eval_expr(target)?;
                if let Value::LazyList(list) = &base {
                    base = Value::Array(self.force_lazy_list(list)?);
                }
                // Handle enum-specific methods
                if let Value::Enum {
                    ref enum_type,
                    ref key,
                    value: enum_val,
                    index,
                } = base
                {
                    match name.as_str() {
                        "key" => return Ok(Value::Str(key.clone())),
                        "value" | "Int" | "Numeric" => return Ok(Value::Int(enum_val)),
                        "raku" | "perl" => {
                            return Ok(Value::Str(format!("{}::{}", enum_type, key)));
                        }
                        "gist" | "Str" => return Ok(Value::Str(key.clone())),
                        "kv" => {
                            return Ok(Value::Array(vec![
                                Value::Str(key.clone()),
                                Value::Int(enum_val),
                            ]));
                        }
                        "pair" => {
                            return Ok(Value::Pair(key.clone(), Box::new(Value::Int(enum_val))));
                        }
                        "pred" => {
                            if index == 0 {
                                return Ok(Value::Nil);
                            }
                            if let Some(variants) = self.enum_types.get(enum_type)
                                && let Some((prev_key, prev_val)) = variants.get(index - 1)
                            {
                                return Ok(Value::Enum {
                                    enum_type: enum_type.clone(),
                                    key: prev_key.clone(),
                                    value: *prev_val,
                                    index: index - 1,
                                });
                            }
                            return Ok(Value::Nil);
                        }
                        "succ" => {
                            if let Some(variants) = self.enum_types.get(enum_type)
                                && let Some((next_key, next_val)) = variants.get(index + 1)
                            {
                                return Ok(Value::Enum {
                                    enum_type: enum_type.clone(),
                                    key: next_key.clone(),
                                    value: *next_val,
                                    index: index + 1,
                                });
                            }
                            return Ok(Value::Nil);
                        }
                        _ => {} // fall through to generic methods
                    }
                }
                // Handle Type.enums (bare identifier matching an enum type name)
                if name == "enums"
                    && let Value::Str(ref type_name) = base
                    && let Some(variants) = self.enum_types.get(type_name)
                {
                    let mut map = HashMap::new();
                    for (k, v) in variants {
                        map.insert(k.clone(), Value::Int(*v));
                    }
                    return Ok(Value::Hash(map));
                }
                // Try builtins for 0-arg pure methods first
                if args.is_empty()
                    && let Some(result) = crate::builtins::native_method_0arg(&base, name.as_str())
                {
                    return result;
                }
                match name.as_str() {
                    "WHAT" => {
                        let type_name = match &base {
                            Value::Int(_) => "Int",
                            Value::BigInt(_) => "Int",
                            Value::Num(_) => "Num",
                            Value::Str(_) => "Str",
                            Value::Bool(_) => "Bool",
                            Value::Range(_, _) => "Range",
                            Value::RangeExcl(_, _)
                            | Value::RangeExclStart(_, _)
                            | Value::RangeExclBoth(_, _) => "Range",
                            Value::Array(_) => "Array",
                            Value::LazyList(_) => "Array",
                            Value::Hash(_) => "Hash",
                            Value::Rat(_, _) => "Rat",
                            Value::FatRat(_, _) => "FatRat",
                            Value::Complex(_, _) => "Complex",
                            Value::Set(_) => "Set",
                            Value::Bag(_) => "Bag",
                            Value::Mix(_) => "Mix",
                            Value::Pair(_, _) => "Pair",
                            Value::Enum { enum_type, .. } => enum_type.as_str(),
                            Value::Nil => "Any",
                            Value::Package(name) => name.as_str(),
                            Value::Routine { .. } => "Routine",
                            Value::Sub { .. } => "Sub",
                            Value::CompUnitDepSpec { .. } => "CompUnit::DependencySpecification",
                            Value::Instance { class_name, .. } => class_name.as_str(),
                            Value::Junction { .. } => "Junction",
                            Value::Regex(_) => "Regex",
                            Value::Version { .. } => "Version",
                            Value::Slip(_) => "Slip",
                        };
                        Ok(Value::Package(type_name.to_string()))
                    }
                    "^name" => {
                        // Meta-method: type name
                        Ok(Value::Str(match &base {
                            Value::Int(_) => "Int".to_string(),
                            Value::BigInt(_) => "Int".to_string(),
                            Value::Num(_) => "Num".to_string(),
                            Value::Str(_) => "Str".to_string(),
                            Value::Bool(_) => "Bool".to_string(),
                            Value::Range(_, _)
                            | Value::RangeExcl(_, _)
                            | Value::RangeExclStart(_, _)
                            | Value::RangeExclBoth(_, _) => "Range".to_string(),
                            Value::Array(_) => "Array".to_string(),
                            Value::LazyList(_) => "Array".to_string(),
                            Value::Hash(_) => "Hash".to_string(),
                            Value::Rat(_, _) => "Rat".to_string(),
                            Value::FatRat(_, _) => "FatRat".to_string(),
                            Value::Complex(_, _) => "Complex".to_string(),
                            Value::Set(_) => "Set".to_string(),
                            Value::Bag(_) => "Bag".to_string(),
                            Value::Mix(_) => "Mix".to_string(),
                            Value::Pair(_, _) => "Pair".to_string(),
                            Value::Enum { enum_type, .. } => enum_type.clone(),
                            Value::Nil => "Nil".to_string(),
                            Value::Package(name) => name.clone(),
                            Value::Routine { .. } => "Routine".to_string(),
                            Value::Sub { .. } => "Sub".to_string(),
                            Value::CompUnitDepSpec { .. } => {
                                "CompUnit::DependencySpecification".to_string()
                            }
                            Value::Instance { class_name, .. } => class_name.clone(),
                            Value::Junction { .. } => "Junction".to_string(),
                            Value::Regex(_) => "Regex".to_string(),
                            Value::Version { .. } => "Version".to_string(),
                            Value::Slip(_) => "Slip".to_string(),
                        }))
                    }
                    // "defined" handled by builtins::native_method_0arg
                    "of" => {
                        // Hash.of returns Mu (the default value type)
                        Ok(Value::Package("Mu".to_string()))
                    }
                    "parent" => {
                        let mut levels = 1i64;
                        if let Some(arg) = args.first()
                            && let Value::Int(i) = self.eval_expr(arg)?
                        {
                            levels = i.max(1);
                        }
                        let mut path = base.to_string_value();
                        for _ in 0..levels {
                            if let Some(parent) = Path::new(&path).parent() {
                                let s = parent.to_string_lossy().to_string();
                                if s.is_empty() {
                                    path = ".".to_string();
                                    break;
                                }
                                path = s;
                            } else {
                                path = ".".to_string();
                                break;
                            }
                        }
                        Ok(Value::Str(path))
                    }
                    "sibling" => {
                        let segment = args
                            .first()
                            .and_then(|arg| self.eval_expr(arg).ok())
                            .map(|v| v.to_string_value())
                            .unwrap_or_default();
                        let base_path = base.to_string_value();
                        let parent = Path::new(&base_path)
                            .parent()
                            .unwrap_or_else(|| Path::new(""));
                        let joined = parent.join(segment);
                        Ok(Value::Str(joined.to_string_lossy().to_string()))
                    }
                    "add" => {
                        let segment = args
                            .first()
                            .and_then(|arg| self.eval_expr(arg).ok())
                            .map(|v| v.to_string_value())
                            .unwrap_or_default();
                        let joined = Path::new(&base.to_string_value()).join(segment);
                        Ok(Value::Str(joined.to_string_lossy().to_string()))
                    }
                    "package" => match base {
                        Value::Routine { package, .. } => Ok(Value::Package(package)),
                        _ => Ok(Value::Nil),
                    },
                    "name" => match base {
                        Value::Routine { name, .. } => Ok(Value::Str(name)),
                        Value::Package(name) => Ok(Value::Str(name)),
                        Value::Str(name) => Ok(Value::Str(name)),
                        Value::Sub { name, .. } => Ok(Value::Str(name)),
                        Value::Instance { attributes, .. } => {
                            Ok(attributes.get("name").cloned().unwrap_or(Value::Nil))
                        }
                        _ => Ok(Value::Nil),
                    },
                    // chars, uc, lc, tc, tclc, wordcase, chomp, chop, trim,
                    // trim-leading, trim-trailing, flip: handled by builtins::native_method_0arg
                    "chop" => {
                        // Type objects (Package) should throw
                        if let Value::Package(ref type_name) = base {
                            return Err(RuntimeError::new(format!(
                                "Cannot resolve caller chop({}:U)",
                                type_name,
                            )));
                        }
                        let s = base.to_string_value();
                        let n = if let Some(arg_expr) = args.first() {
                            let n_val = self.eval_expr(arg_expr)?;
                            match n_val {
                                Value::Int(i) => i.max(0) as usize,
                                Value::Num(f) => (f as i64).max(0) as usize,
                                _ => n_val.to_string_value().parse::<usize>().unwrap_or(1),
                            }
                        } else {
                            1
                        };
                        let char_count = s.chars().count();
                        let keep = char_count.saturating_sub(n);
                        let result: String = s.chars().take(keep).collect();
                        Ok(Value::Str(result))
                    }
                    "contains" => {
                        let needle_val = args.first().map(|a| self.eval_expr(a)).transpose()?;
                        if let Some(Value::Package(ref type_name)) = needle_val {
                            return Err(RuntimeError::new(format!(
                                "Cannot resolve caller contains({}:U)",
                                type_name,
                            )));
                        }
                        let needle = needle_val.map(|v| v.to_string_value()).unwrap_or_default();
                        Ok(Value::Bool(base.to_string_value().contains(&needle)))
                    }
                    "starts-with" => {
                        let needle = args
                            .first()
                            .and_then(|a| self.eval_expr(a).ok())
                            .map(|v| v.to_string_value())
                            .unwrap_or_default();
                        Ok(Value::Bool(base.to_string_value().starts_with(&needle)))
                    }
                    "ends-with" => {
                        let needle = args
                            .first()
                            .and_then(|a| self.eval_expr(a).ok())
                            .map(|v| v.to_string_value())
                            .unwrap_or_default();
                        Ok(Value::Bool(base.to_string_value().ends_with(&needle)))
                    }
                    "substr" => {
                        let s = base.to_string_value();
                        let start = args
                            .first()
                            .and_then(|a| self.eval_expr(a).ok())
                            .and_then(|v| match v {
                                Value::Int(i) => Some(i),
                                _ => None,
                            })
                            .unwrap_or(0);
                        let chars: Vec<char> = s.chars().collect();
                        let start = start.max(0) as usize;
                        if let Some(len_expr) = args.get(1) {
                            let len = self.eval_expr(len_expr)?;
                            let len = match len {
                                Value::Int(i) => i.max(0) as usize,
                                _ => chars.len(),
                            };
                            let end = (start + len).min(chars.len());
                            Ok(Value::Str(chars[start..end].iter().collect()))
                        } else {
                            Ok(Value::Str(chars[start..].iter().collect()))
                        }
                    }
                    "subst" => {
                        let s = base.to_string_value();
                        let pattern = args
                            .first()
                            .and_then(|a| self.eval_expr(a).ok())
                            .map(|v| v.to_string_value())
                            .unwrap_or_default();
                        let replacement = args
                            .get(1)
                            .and_then(|a| self.eval_expr(a).ok())
                            .map(|v| v.to_string_value())
                            .unwrap_or_default();
                        // Check for :g adverb
                        let mut global = false;
                        for a in args.iter().skip(2) {
                            if let Expr::AssignExpr { name, .. } = a {
                                if name == "g" {
                                    global = true;
                                }
                            } else if let Ok(Value::Pair(ref k, _)) = self.eval_expr(a)
                                && k == "g"
                            {
                                global = true;
                            }
                        }
                        let result = if global {
                            s.replace(&pattern, &replacement)
                        } else {
                            s.replacen(&pattern, &replacement, 1)
                        };
                        Ok(Value::Str(result))
                    }
                    "index" => {
                        let s = base.to_string_value();
                        let needle = args
                            .first()
                            .and_then(|a| self.eval_expr(a).ok())
                            .map(|v| v.to_string_value())
                            .unwrap_or_default();
                        match s.find(&needle) {
                            Some(pos) => {
                                let char_pos = s[..pos].chars().count();
                                Ok(Value::Int(char_pos as i64))
                            }
                            None => Ok(Value::Nil),
                        }
                    }
                    "rindex" => {
                        let s = base.to_string_value();
                        let needle = args
                            .first()
                            .and_then(|a| self.eval_expr(a).ok())
                            .map(|v| v.to_string_value())
                            .unwrap_or_default();
                        match s.rfind(&needle) {
                            Some(pos) => {
                                let char_pos = s[..pos].chars().count();
                                Ok(Value::Int(char_pos as i64))
                            }
                            None => Ok(Value::Nil),
                        }
                    }
                    "match" => {
                        if let Some(arg) = args.first() {
                            let target = base.to_string_value();
                            let pat_val = self.eval_expr(arg)?;
                            match pat_val {
                                Value::Regex(pat) => {
                                    if let Some(captures) =
                                        self.regex_match_with_captures(&pat, &target)
                                    {
                                        for (k, v) in captures {
                                            self.env.insert(format!("<{}>", k), Value::Str(v));
                                        }
                                        Ok(Value::Bool(true))
                                    } else {
                                        Ok(Value::Bool(false))
                                    }
                                }
                                Value::Str(pat) => {
                                    if let Some(captures) =
                                        self.regex_match_with_captures(&pat, &target)
                                    {
                                        for (k, v) in captures {
                                            self.env.insert(format!("<{}>", k), Value::Str(v));
                                        }
                                        Ok(Value::Bool(true))
                                    } else {
                                        Ok(Value::Bool(false))
                                    }
                                }
                                _ => Ok(Value::Nil),
                            }
                        } else {
                            Ok(Value::Nil)
                        }
                    }
                    "IO" => {
                        let path = base.to_string_value();
                        let mut attrs = HashMap::new();
                        attrs.insert("path".to_string(), Value::Str(path));
                        Ok(Value::make_instance("IO::Path".to_string(), attrs))
                    }
                    "say" => {
                        let content = Self::gist_value(&base);
                        self.write_to_named_handle("$*OUT", &content, true)?;
                        Ok(Value::Bool(true))
                    }
                    "print" => {
                        self.output.push_str(&base.to_string_value());
                        Ok(Value::Bool(true))
                    }
                    "note" => {
                        let content = Self::gist_value(&base);
                        self.write_to_named_handle("$*ERR", &content, true)?;
                        Ok(Value::Bool(true))
                    }
                    "Seq" => {
                        let items = Self::value_to_list(&base);
                        Ok(Value::Array(items))
                    }
                    "Supply" | "Channel" => {
                        // stub
                        Ok(base)
                    }
                    "hash" | "Hash" => Ok(Self::coerce_to_hash(base)),
                    "clone" => match base {
                        Value::Hash(items) => Ok(Value::Hash(items.clone())),
                        Value::Array(items) => Ok(Value::Array(items.clone())),
                        other => Ok(other),
                    },
                    "classify" => {
                        // Returns empty hash as stub
                        Ok(Value::Hash(HashMap::new()))
                    }
                    "splice" => match base {
                        Value::Array(mut items) => {
                            let start = args
                                .first()
                                .and_then(|a| self.eval_expr(a).ok())
                                .and_then(|v| match v {
                                    Value::Int(i) => Some(i.max(0) as usize),
                                    _ => None,
                                })
                                .unwrap_or(0)
                                .min(items.len());
                            let count = args
                                .get(1)
                                .and_then(|a| self.eval_expr(a).ok())
                                .and_then(|v| match v {
                                    Value::Int(i) => Some(i.max(0) as usize),
                                    _ => None,
                                })
                                .unwrap_or(items.len().saturating_sub(start));
                            let end = (start + count).min(items.len());
                            let removed: Vec<Value> = items.drain(start..end).collect();
                            // Insert new elements if provided
                            if let Some(new_arg) = args.get(2) {
                                let new_val = self.eval_expr(new_arg)?;
                                match new_val {
                                    Value::Array(new_items) => {
                                        for (i, item) in new_items.into_iter().enumerate() {
                                            items.insert(start + i, item);
                                        }
                                    }
                                    other => {
                                        items.insert(start, other);
                                    }
                                }
                            }
                            Ok(Value::Array(removed))
                        }
                        _ => Ok(Value::Nil),
                    },
                    "split" => {
                        let s = base.to_string_value();
                        let sep = args
                            .first()
                            .and_then(|a| self.eval_expr(a).ok())
                            .map(|v| v.to_string_value())
                            .unwrap_or_default();
                        let parts: Vec<Value> =
                            s.split(&sep).map(|p| Value::Str(p.to_string())).collect();
                        Ok(Value::Array(parts))
                    }
                    // words: handled by builtins::native_method_0arg
                    "Range" => match base {
                        Value::Array(items) => Ok(Value::RangeExcl(0, items.len() as i64)),
                        Value::Str(s) => Ok(Value::RangeExcl(0, s.chars().count() as i64)),
                        Value::Range(_, _)
                        | Value::RangeExcl(_, _)
                        | Value::RangeExclStart(_, _)
                        | Value::RangeExclBoth(_, _) => Ok(base),
                        _ => Ok(Value::Nil),
                    },
                    // comb, lines: handled by builtins::native_method_0arg
                    "Int" => match base {
                        Value::Int(i) => Ok(Value::Int(i)),
                        Value::Num(f) => Ok(Value::Int(f as i64)),
                        Value::Rat(n, d) => {
                            if d == 0 {
                                Err(RuntimeError::new("Cannot convert Inf/NaN Rat to Int"))
                            } else {
                                Ok(Value::Int(n / d))
                            }
                        }
                        Value::Complex(r, _) => Ok(Value::Int(r as i64)),
                        Value::Str(s) => Ok(Value::Int(s.trim().parse::<i64>().unwrap_or(0))),
                        Value::Bool(b) => Ok(Value::Int(if b { 1 } else { 0 })),
                        _ => Ok(Value::Int(0)),
                    },
                    "Numeric" | "Num" => match base {
                        Value::Int(i) => Ok(Value::Int(i)),
                        Value::Num(f) => Ok(Value::Num(f)),
                        Value::Rat(n, d) => {
                            if d == 0 {
                                if n == 0 {
                                    Ok(Value::Num(f64::NAN))
                                } else if n > 0 {
                                    Ok(Value::Num(f64::INFINITY))
                                } else {
                                    Ok(Value::Num(f64::NEG_INFINITY))
                                }
                            } else {
                                Ok(Value::Num(n as f64 / d as f64))
                            }
                        }
                        Value::Complex(r, _) => Ok(Value::Num(r)),
                        Value::Str(s) => {
                            if let Ok(i) = s.trim().parse::<i64>() {
                                Ok(Value::Int(i))
                            } else if let Ok(f) = s.trim().parse::<f64>() {
                                Ok(Value::Num(f))
                            } else {
                                Ok(Value::Int(0))
                            }
                        }
                        Value::Bool(b) => Ok(Value::Int(if b { 1 } else { 0 })),
                        _ => Ok(Value::Int(0)),
                    },
                    // Rat: handled by builtins::native_method_0arg
                    "FatRat" => match base {
                        Value::FatRat(_, _) => Ok(base),
                        Value::Rat(n, d) => Ok(Value::FatRat(n, d)),
                        Value::Int(i) => Ok(Value::FatRat(i, 1)),
                        _ => Ok(Value::FatRat(0, 1)),
                    },
                    "nude" => match base {
                        Value::Rat(n, d) => Ok(Value::Array(vec![Value::Int(n), Value::Int(d)])),
                        Value::FatRat(n, d) => Ok(Value::Array(vec![Value::Int(n), Value::Int(d)])),
                        Value::Int(i) => Ok(Value::Array(vec![Value::Int(i), Value::Int(1)])),
                        _ => Ok(Value::Array(vec![Value::Int(0), Value::Int(1)])),
                    },
                    "numerator" => match base {
                        Value::Rat(n, _) => Ok(Value::Int(n)),
                        Value::FatRat(n, _) => Ok(Value::Int(n)),
                        Value::Int(i) => Ok(Value::Int(i)),
                        _ => Ok(Value::Int(0)),
                    },
                    "denominator" => match base {
                        Value::Rat(_, d) => Ok(Value::Int(d)),
                        Value::FatRat(_, d) => Ok(Value::Int(d)),
                        Value::Int(_) => Ok(Value::Int(1)),
                        _ => Ok(Value::Int(1)),
                    },
                    "isNaN" => match base {
                        Value::Rat(0, 0) => Ok(Value::Bool(true)),
                        Value::Num(f) => Ok(Value::Bool(f.is_nan())),
                        _ => Ok(Value::Bool(false)),
                    },
                    "re" => match base {
                        Value::Complex(r, _) => Ok(Value::Num(r)),
                        Value::Int(i) => Ok(Value::Num(i as f64)),
                        Value::Num(f) => Ok(Value::Num(f)),
                        _ => Ok(Value::Num(0.0)),
                    },
                    "im" => match base {
                        Value::Complex(_, i) => Ok(Value::Num(i)),
                        _ => Ok(Value::Num(0.0)),
                    },
                    "conj" => match base {
                        Value::Complex(r, i) => Ok(Value::Complex(r, -i)),
                        Value::Int(i) => Ok(Value::Complex(i as f64, 0.0)),
                        Value::Num(f) => Ok(Value::Complex(f, 0.0)),
                        _ => Ok(Value::Complex(0.0, 0.0)),
                    },
                    "reals" => match base {
                        Value::Complex(r, i) => {
                            Ok(Value::Array(vec![Value::Num(r), Value::Num(i)]))
                        }
                        _ => Ok(Value::Array(vec![base.clone(), Value::Num(0.0)])),
                    },
                    "Complex" => match base {
                        Value::Complex(_, _) => Ok(base),
                        Value::Int(i) => Ok(Value::Complex(i as f64, 0.0)),
                        Value::Num(f) => Ok(Value::Complex(f, 0.0)),
                        _ => Ok(Value::Complex(0.0, 0.0)),
                    },
                    "Set" => {
                        let mut elems = HashSet::new();
                        match base {
                            Value::Set(_) => return Ok(base),
                            Value::Array(items) => {
                                for item in items {
                                    elems.insert(item.to_string_value());
                                }
                            }
                            Value::Hash(items) => {
                                for (k, v) in items {
                                    if v.truthy() {
                                        elems.insert(k);
                                    }
                                }
                            }
                            Value::Bag(b) => {
                                for k in b.keys() {
                                    elems.insert(k.clone());
                                }
                            }
                            Value::Mix(m) => {
                                for k in m.keys() {
                                    elems.insert(k.clone());
                                }
                            }
                            other => {
                                elems.insert(other.to_string_value());
                            }
                        }
                        Ok(Value::Set(elems))
                    }
                    "Bag" => {
                        let mut counts: HashMap<String, i64> = HashMap::new();
                        match base {
                            Value::Bag(_) => return Ok(base),
                            Value::Array(items) => {
                                for item in items {
                                    *counts.entry(item.to_string_value()).or_insert(0) += 1;
                                }
                            }
                            Value::Set(s) => {
                                for k in s {
                                    counts.insert(k, 1);
                                }
                            }
                            Value::Mix(m) => {
                                for (k, v) in m {
                                    counts.insert(k, v as i64);
                                }
                            }
                            other => {
                                counts.insert(other.to_string_value(), 1);
                            }
                        }
                        Ok(Value::Bag(counts))
                    }
                    "Mix" => {
                        let mut weights: HashMap<String, f64> = HashMap::new();
                        match base {
                            Value::Mix(_) => return Ok(base),
                            Value::Array(items) => {
                                for item in items {
                                    *weights.entry(item.to_string_value()).or_insert(0.0) += 1.0;
                                }
                            }
                            Value::Set(s) => {
                                for k in s {
                                    weights.insert(k, 1.0);
                                }
                            }
                            Value::Bag(b) => {
                                for (k, v) in b {
                                    weights.insert(k, v as f64);
                                }
                            }
                            other => {
                                weights.insert(other.to_string_value(), 1.0);
                            }
                        }
                        Ok(Value::Mix(weights))
                    }
                    // Bool: handled by builtins::native_method_0arg
                    "gist" | "raku" | "perl" => match base {
                        // builtins handles most cases; interpreter handles Package/Instance/Enum
                        Value::Package(name) => Ok(Value::Str(format!("({})", name))),
                        _ => Ok(Value::Str(base.to_string_value())),
                    },
                    "Str" => {
                        // builtins handles most cases; fallback for Instance/Package
                        Ok(Value::Str(base.to_string_value()))
                    }
                    "sink" => {
                        // .sink forces evaluation and discards result
                        Ok(Value::Nil)
                    }
                    "Slip" => match base {
                        Value::Array(items) => Ok(Value::Slip(items)),
                        Value::Slip(_) => Ok(base),
                        _ => Ok(Value::Slip(vec![base])),
                    },
                    "elems" => match base {
                        Value::Array(items) => Ok(Value::Int(items.len() as i64)),
                        Value::Hash(items) => Ok(Value::Int(items.len() as i64)),
                        Value::Set(s) => Ok(Value::Int(s.len() as i64)),
                        Value::Bag(b) => Ok(Value::Int(b.len() as i64)),
                        Value::Mix(m) => Ok(Value::Int(m.len() as i64)),
                        Value::Junction { values, .. } => Ok(Value::Int(values.len() as i64)),
                        _ => Ok(Value::Int(1)),
                    },
                    "any" | "all" | "one" | "none" => {
                        use crate::value::JunctionKind;
                        let kind = match name.as_str() {
                            "any" => JunctionKind::Any,
                            "all" => JunctionKind::All,
                            "one" => JunctionKind::One,
                            _ => JunctionKind::None,
                        };
                        let elems = Self::value_to_list(&base);
                        Ok(Value::Junction {
                            kind,
                            values: elems,
                        })
                    }
                    "total" => match base {
                        Value::Set(s) => Ok(Value::Int(s.len() as i64)),
                        Value::Bag(b) => Ok(Value::Int(b.values().sum::<i64>())),
                        Value::Mix(m) => Ok(Value::Num(m.values().sum::<f64>())),
                        _ => Ok(Value::Int(1)),
                    },
                    // end: builtins handles Array; fallback here
                    "end" => Ok(Value::Int(0)),
                    "keys" => match base {
                        Value::Hash(items) => {
                            let keys: Vec<Value> =
                                items.keys().map(|k| Value::Str(k.clone())).collect();
                            Ok(Value::Array(keys))
                        }
                        Value::Set(s) => {
                            let keys: Vec<Value> =
                                s.iter().map(|k| Value::Str(k.clone())).collect();
                            Ok(Value::Array(keys))
                        }
                        Value::Bag(b) => {
                            let keys: Vec<Value> =
                                b.keys().map(|k| Value::Str(k.clone())).collect();
                            Ok(Value::Array(keys))
                        }
                        Value::Mix(m) => {
                            let keys: Vec<Value> =
                                m.keys().map(|k| Value::Str(k.clone())).collect();
                            Ok(Value::Array(keys))
                        }
                        _ => Ok(Value::Array(Vec::new())),
                    },
                    "values" => match base {
                        Value::Hash(items) => {
                            let vals: Vec<Value> = items.values().cloned().collect();
                            Ok(Value::Array(vals))
                        }
                        Value::Set(s) => {
                            Ok(Value::Array(s.iter().map(|_| Value::Bool(true)).collect()))
                        }
                        Value::Bag(b) => {
                            Ok(Value::Array(b.values().map(|v| Value::Int(*v)).collect()))
                        }
                        Value::Mix(m) => {
                            Ok(Value::Array(m.values().map(|v| Value::Num(*v)).collect()))
                        }
                        _ => Ok(Value::Array(Vec::new())),
                    },
                    "kv" => match base {
                        Value::Hash(items) => {
                            let mut kv = Vec::new();
                            for (k, v) in &items {
                                kv.push(Value::Str(k.clone()));
                                kv.push(v.clone());
                            }
                            Ok(Value::Array(kv))
                        }
                        Value::Set(s) => {
                            let mut kv = Vec::new();
                            for k in &s {
                                kv.push(Value::Str(k.clone()));
                                kv.push(Value::Bool(true));
                            }
                            Ok(Value::Array(kv))
                        }
                        Value::Bag(b) => {
                            let mut kv = Vec::new();
                            for (k, v) in &b {
                                kv.push(Value::Str(k.clone()));
                                kv.push(Value::Int(*v));
                            }
                            Ok(Value::Array(kv))
                        }
                        Value::Mix(m) => {
                            let mut kv = Vec::new();
                            for (k, v) in &m {
                                kv.push(Value::Str(k.clone()));
                                kv.push(Value::Num(*v));
                            }
                            Ok(Value::Array(kv))
                        }
                        _ => Ok(Value::Array(Vec::new())),
                    },
                    "pairs" => match base {
                        Value::Hash(items) => {
                            let pairs: Vec<Value> = items
                                .iter()
                                .map(|(k, v)| Value::Pair(k.clone(), Box::new(v.clone())))
                                .collect();
                            Ok(Value::Array(pairs))
                        }
                        Value::Set(s) => {
                            let pairs: Vec<Value> = s
                                .iter()
                                .map(|k| Value::Pair(k.clone(), Box::new(Value::Bool(true))))
                                .collect();
                            Ok(Value::Array(pairs))
                        }
                        Value::Bag(b) => {
                            let pairs: Vec<Value> = b
                                .iter()
                                .map(|(k, v)| Value::Pair(k.clone(), Box::new(Value::Int(*v))))
                                .collect();
                            Ok(Value::Array(pairs))
                        }
                        Value::Mix(m) => {
                            let pairs: Vec<Value> = m
                                .iter()
                                .map(|(k, v)| Value::Pair(k.clone(), Box::new(Value::Num(*v))))
                                .collect();
                            Ok(Value::Array(pairs))
                        }
                        _ => Ok(Value::Array(Vec::new())),
                    },
                    "sort" => match base {
                        Value::Array(mut items) => {
                            if let Some(func_expr) = args.first() {
                                let func = self.eval_expr(func_expr)?;
                                if let Value::Sub {
                                    params, body, env, ..
                                } = func
                                {
                                    let placeholders = collect_placeholders(&body);
                                    let is_key_extractor = placeholders.len() < 2
                                        && params.len() <= 1
                                        || placeholders.len() == 1;
                                    if is_key_extractor {
                                        // 1-arg function: use as key extractor
                                        items.sort_by(|a, b| {
                                            let saved = self.env.clone();
                                            for (k, v) in &env {
                                                self.env.insert(k.clone(), v.clone());
                                            }
                                            if let Some(p) = params.first() {
                                                self.env.insert(p.clone(), a.clone());
                                            }
                                            self.env.insert("_".to_string(), a.clone());
                                            let key_a =
                                                self.eval_block_value(&body).unwrap_or(Value::Nil);
                                            self.env = saved.clone();
                                            for (k, v) in &env {
                                                self.env.insert(k.clone(), v.clone());
                                            }
                                            if let Some(p) = params.first() {
                                                self.env.insert(p.clone(), b.clone());
                                            }
                                            self.env.insert("_".to_string(), b.clone());
                                            let key_b =
                                                self.eval_block_value(&body).unwrap_or(Value::Nil);
                                            self.env = saved;
                                            key_a.to_string_value().cmp(&key_b.to_string_value())
                                        });
                                    } else {
                                        // 2-arg function: use as comparator
                                        items.sort_by(|a, b| {
                                            let saved = self.env.clone();
                                            for (k, v) in &env {
                                                self.env.insert(k.clone(), v.clone());
                                            }
                                            if let Some(p) = params.first() {
                                                self.env.insert(p.clone(), a.clone());
                                            }
                                            if placeholders.len() >= 2 {
                                                self.env.insert(placeholders[0].clone(), a.clone());
                                                self.env.insert(placeholders[1].clone(), b.clone());
                                            }
                                            self.env.insert("_".to_string(), a.clone());
                                            let result = self
                                                .eval_block_value(&body)
                                                .unwrap_or(Value::Int(0));
                                            self.env = saved;
                                            match result {
                                                Value::Int(n) => n.cmp(&0),
                                                Value::Enum {
                                                    enum_type, value, ..
                                                } if enum_type == "Order" => value.cmp(&0),
                                                _ => std::cmp::Ordering::Equal,
                                            }
                                        });
                                    }
                                    Ok(Value::Array(items))
                                } else {
                                    items.sort_by(|a, b| {
                                        a.to_string_value().cmp(&b.to_string_value())
                                    });
                                    Ok(Value::Array(items))
                                }
                            } else {
                                items.sort_by_key(|a| a.to_string_value());
                                Ok(Value::Array(items))
                            }
                        }
                        _ => Ok(base),
                    },
                    // reverse, unique: builtins handles Array/Str; fallback here
                    "reverse" | "unique" => Ok(base),
                    "squish" => match base {
                        Value::Array(items) => {
                            let mut result = Vec::new();
                            let mut last: Option<String> = None;
                            for item in items {
                                let s = item.to_string_value();
                                if last.as_ref() != Some(&s) {
                                    last = Some(s);
                                    result.push(item);
                                }
                            }
                            Ok(Value::Array(result))
                        }
                        _ => Ok(base),
                    },
                    "minmax" => match base {
                        Value::Array(items) if !items.is_empty() => {
                            let mut min = &items[0];
                            let mut max = &items[0];
                            for item in &items[1..] {
                                if Self::compare_values(item, min) < 0 {
                                    min = item;
                                }
                                if Self::compare_values(item, max) > 0 {
                                    max = item;
                                }
                            }
                            Ok(Value::Range(Self::to_int(min), Self::to_int(max)))
                        }
                        _ => Ok(Value::Nil),
                    },
                    // flat: builtins handles Array; fallback here
                    "flat" => Ok(base),
                    // hyper/race: pass through (simplified; no parallelism)
                    "hyper" | "race" => Ok(base),
                    "skip" => {
                        let n = if let Some(arg) = args.first() {
                            match self.eval_expr(arg)? {
                                Value::Int(i) => i as usize,
                                _ => 1,
                            }
                        } else {
                            1
                        };
                        let items = Self::value_to_list(&base);
                        Ok(Value::Array(items.into_iter().skip(n).collect()))
                    }
                    // head, tail, first, min, max (0-arg): handled by builtins
                    // Non-Array fallback for when builtins returns None:
                    "head" | "tail" | "first" | "min" | "max" => Ok(base),
                    "sum" => match base {
                        Value::Array(items) => {
                            let mut total: i64 = 0;
                            let mut is_float = false;
                            let mut ftotal: f64 = 0.0;
                            for item in &items {
                                match item {
                                    Value::Int(i) => {
                                        total += i;
                                        ftotal += *i as f64;
                                    }
                                    Value::Num(f) => {
                                        is_float = true;
                                        ftotal += f;
                                    }
                                    Value::Str(s) => {
                                        if let Ok(i) = s.parse::<i64>() {
                                            total += i;
                                            ftotal += i as f64;
                                        } else if let Ok(f) = s.parse::<f64>() {
                                            is_float = true;
                                            ftotal += f;
                                        }
                                    }
                                    _ => {}
                                }
                            }
                            if is_float {
                                Ok(Value::Num(ftotal))
                            } else {
                                Ok(Value::Int(total))
                            }
                        }
                        _ => Ok(Value::Int(0)),
                    },
                    "pick" => {
                        let items = Self::value_to_list(&base);
                        if items.is_empty() {
                            Ok(Value::Nil)
                        } else {
                            let count = if let Some(arg) = args.first() {
                                match self.eval_expr(arg)? {
                                    Value::Int(n) => n as usize,
                                    Value::Num(n) => n as usize,
                                    _ => 1,
                                }
                            } else {
                                0 // 0 means "return single element"
                            };
                            if count == 0 {
                                let idx = crate::builtins::builtin_rand_int(items.len());
                                Ok(items[idx].clone())
                            } else {
                                // pick(N) — N elements without replacement
                                let mut pool = items;
                                let n = count.min(pool.len());
                                let mut result = Vec::with_capacity(n);
                                for _ in 0..n {
                                    let idx = crate::builtins::builtin_rand_int(pool.len());
                                    result.push(pool.remove(idx));
                                }
                                Ok(Value::Array(result))
                            }
                        }
                    }
                    "roll" => {
                        let items = Self::value_to_list(&base);
                        if items.is_empty() {
                            Ok(Value::Nil)
                        } else {
                            let count_arg = if let Some(arg) = args.first() {
                                Some(self.eval_expr(arg)?)
                            } else {
                                None
                            };

                            // Handle Inf early - generate large lazy array
                            if let Some(Value::Num(n)) = count_arg
                                && n.is_infinite()
                                && n > 0.0
                            {
                                let max_lazy = 100000;
                                let mut result = Vec::with_capacity(max_lazy);
                                for _ in 0..max_lazy {
                                    let idx = crate::builtins::builtin_rand_int(items.len());
                                    result.push(items[idx].clone());
                                }
                                return Ok(Value::Array(result));
                            }

                            let count = match count_arg {
                                Some(Value::Int(n)) if n >= 0 => n as usize,
                                Some(Value::Num(n)) if n >= 0.0 && n.is_finite() => n as usize,
                                Some(_) => 1,
                                None => 0, // 0 means "return single element"
                            };

                            if count == 0 {
                                let idx = crate::builtins::builtin_rand_int(items.len());
                                Ok(items[idx].clone())
                            } else if count > 10_000_000 {
                                // Safety check: cap at 10 million elements
                                Err(RuntimeError::new("roll count too large (max 10 million)"))
                            } else {
                                // roll(N) — N elements with replacement
                                let mut result = Vec::with_capacity(count);
                                for _ in 0..count {
                                    let idx = crate::builtins::builtin_rand_int(items.len());
                                    result.push(items[idx].clone());
                                }
                                Ok(Value::Array(result))
                            }
                        }
                    }
                    "map" => {
                        let items = Self::value_to_list(&base);
                        if let Some(func_expr) = args.first() {
                            let func = self.eval_expr(func_expr)?;
                            if let Value::Sub {
                                params, body, env, ..
                            } = func
                            {
                                let placeholders = collect_placeholders(&body);
                                let arity = if !params.is_empty() {
                                    params.len()
                                } else if !placeholders.is_empty() {
                                    placeholders.len()
                                } else {
                                    1
                                };
                                let mut result = Vec::new();
                                let mut i = 0;
                                while i < items.len() {
                                    if arity > 1 && i + arity > items.len() {
                                        return Err(RuntimeError::new(
                                            "Not enough elements for map block arity",
                                        ));
                                    }
                                    let saved = self.env.clone();
                                    for (k, v) in &env {
                                        self.env.insert(k.clone(), v.clone());
                                    }
                                    if arity == 1 {
                                        let item = items[i].clone();
                                        if let Some(p) = params.first() {
                                            self.env.insert(p.clone(), item.clone());
                                        }
                                        if let Some(ph) = placeholders.first() {
                                            self.env.insert(ph.clone(), item.clone());
                                        }
                                        self.env.insert("_".to_string(), item);
                                    } else {
                                        for (idx, p) in params.iter().enumerate() {
                                            if i + idx < items.len() {
                                                self.env.insert(p.clone(), items[i + idx].clone());
                                            }
                                        }
                                        for (idx, ph) in placeholders.iter().enumerate() {
                                            if i + idx < items.len() {
                                                self.env.insert(ph.clone(), items[i + idx].clone());
                                            }
                                        }
                                        self.env.insert("_".to_string(), items[i].clone());
                                    }
                                    match self.eval_block_value(&body) {
                                        Ok(Value::Slip(elems)) => {
                                            self.env = saved;
                                            result.extend(elems);
                                        }
                                        Ok(val) => {
                                            self.env = saved;
                                            result.push(val);
                                        }
                                        Err(e) if e.is_next => {
                                            self.env = saved;
                                            // next: skip this element
                                        }
                                        Err(e) if e.is_last => {
                                            self.env = saved;
                                            break;
                                        }
                                        Err(e) => {
                                            self.env = saved;
                                            return Err(e);
                                        }
                                    }
                                    i += arity;
                                }
                                Ok(Value::Array(result))
                            } else {
                                Ok(Value::Array(items))
                            }
                        } else {
                            Ok(Value::Array(items))
                        }
                    }
                    "grep" => match base {
                        Value::Array(items) => {
                            if let Some(func_expr) = args.first() {
                                let func = self.eval_expr(func_expr)?;
                                if let Value::Sub {
                                    params, body, env, ..
                                } = func
                                {
                                    let placeholders = collect_placeholders(&body);
                                    let mut result = Vec::new();
                                    for item in items {
                                        let saved = self.env.clone();
                                        for (k, v) in &env {
                                            self.env.insert(k.clone(), v.clone());
                                        }
                                        if let Some(p) = params.first() {
                                            self.env.insert(p.clone(), item.clone());
                                        }
                                        if let Some(ph) = placeholders.first() {
                                            self.env.insert(ph.clone(), item.clone());
                                        }
                                        self.env.insert("_".to_string(), item.clone());
                                        let val = self.eval_block_value(&body)?;
                                        self.env = saved;
                                        if val.truthy() {
                                            result.push(item);
                                        }
                                    }
                                    Ok(Value::Array(result))
                                } else {
                                    Ok(Value::Array(items))
                                }
                            } else {
                                Ok(Value::Array(items))
                            }
                        }
                        _ => Ok(base),
                    },
                    "toggle" => {
                        // Parse named :off arg and collect callable conditions
                        let mut off = false;
                        let mut condition_exprs = Vec::new();
                        for arg in args {
                            if let Expr::AssignExpr { name, expr } = arg {
                                if name == "off" {
                                    off = match self.eval_expr(expr)? {
                                        Value::Bool(b) => b,
                                        v => v.truthy(),
                                    };
                                }
                            } else {
                                // Check if arg evaluates to a Pair (named arg from colonpair)
                                let val = self.eval_expr(arg)?;
                                if let Value::Pair(ref k, ref v) = val
                                    && k == "off"
                                {
                                    off = v.truthy();
                                    continue;
                                }
                                // Re-wrap as Literal for deferred callable evaluation
                                condition_exprs.push(Expr::Literal(val));
                            }
                        }

                        // Evaluate all conditions to Values (callables)
                        let mut conditions = Vec::new();
                        for ce in &condition_exprs {
                            conditions.push(self.eval_expr(ce)?);
                        }

                        // Convert base to iterable elements
                        let items = Self::value_to_list(&base);

                        // Toggle state machine
                        let mut state_on = !off;
                        let mut cond_idx = 0;
                        let mut result = Vec::new();

                        for item in &items {
                            if cond_idx < conditions.len()
                                && let Value::Sub {
                                    ref params,
                                    ref body,
                                    ref env,
                                    ..
                                } = conditions[cond_idx]
                            {
                                let placeholders = collect_placeholders(body);
                                let saved = self.env.clone();
                                for (k, v) in env {
                                    self.env.insert(k.clone(), v.clone());
                                }
                                if let Some(p) = params.first() {
                                    self.env.insert(p.clone(), item.clone());
                                }
                                if let Some(ph) = placeholders.first() {
                                    self.env.insert(ph.clone(), item.clone());
                                }
                                self.env.insert("_".to_string(), item.clone());
                                let cond_result = self.eval_block_value(body)?;
                                self.env = saved;

                                if state_on && !cond_result.truthy() {
                                    state_on = false;
                                    cond_idx += 1;
                                } else if !state_on && cond_result.truthy() {
                                    state_on = true;
                                    cond_idx += 1;
                                }
                            }

                            if state_on {
                                result.push(item.clone());
                            }
                        }

                        Ok(Value::Array(result))
                    }
                    "Complex-i" => {
                        let imag = match base {
                            Value::Int(i) => i as f64,
                            Value::Num(f) => f,
                            _ => 0.0,
                        };
                        Ok(Value::Complex(0.0, imag))
                    }
                    // abs: builtins handles Int/Num/Rat; Complex fallback here
                    "abs" => match base {
                        Value::Complex(r, i) => Ok(Value::Num((r * r + i * i).sqrt())),
                        _ => Ok(Value::Int(0)),
                    },
                    // sign: builtins handles Int/Num; fallback here
                    "sign" => Ok(Value::Int(0)),
                    "is-prime" => match base {
                        Value::Int(n) => {
                            let n = n.abs();
                            let prime = if n < 2 {
                                false
                            } else if n < 4 {
                                true
                            } else if n % 2 == 0 || n % 3 == 0 {
                                false
                            } else {
                                let mut i = 5i64;
                                let mut result = true;
                                while i * i <= n {
                                    if n % i == 0 || n % (i + 2) == 0 {
                                        result = false;
                                        break;
                                    }
                                    i += 6;
                                }
                                result
                            };
                            Ok(Value::Bool(prime))
                        }
                        _ => Ok(Value::Bool(false)),
                    },
                    "key" => match base {
                        Value::Pair(k, _) => Ok(Value::Str(k)),
                        _ => Ok(Value::Nil),
                    },
                    "value" => match base {
                        Value::Pair(_, v) => Ok(*v),
                        _ => Ok(Value::Nil),
                    },
                    "list" | "Array" => match base {
                        Value::Range(a, b) => {
                            let items: Vec<Value> = (a..=b).map(Value::Int).collect();
                            Ok(Value::Array(items))
                        }
                        Value::RangeExcl(a, b) => {
                            let items: Vec<Value> = (a..b).map(Value::Int).collect();
                            Ok(Value::Array(items))
                        }
                        Value::RangeExclStart(a, b) => {
                            let items: Vec<Value> = (a + 1..=b).map(Value::Int).collect();
                            Ok(Value::Array(items))
                        }
                        Value::RangeExclBoth(a, b) => {
                            let items: Vec<Value> = (a + 1..b).map(Value::Int).collect();
                            Ok(Value::Array(items))
                        }
                        Value::Array(items) => Ok(Value::Array(items)),
                        _ => Ok(Value::Array(vec![base])),
                    },
                    // so, not, succ, pred: handled by builtins::native_method_0arg
                    "fmt" => {
                        let fmt = args
                            .first()
                            .and_then(|a| self.eval_expr(a).ok())
                            .map(|v| v.to_string_value())
                            .unwrap_or_else(|| "%s".to_string());
                        let rendered = Self::format_sprintf(&fmt, Some(&base));
                        Ok(Value::Str(rendered))
                    }
                    "base" => match base {
                        Value::Int(i) => {
                            let radix = args
                                .first()
                                .and_then(|a| self.eval_expr(a).ok())
                                .and_then(|v| match v {
                                    Value::Int(n) => Some(n),
                                    _ => None,
                                })
                                .unwrap_or(10);
                            let s = match radix {
                                2 => format!("{:b}", i),
                                8 => format!("{:o}", i),
                                16 => format!("{:X}", i),
                                _ => format!("{}", i),
                            };
                            Ok(Value::Str(s))
                        }
                        _ => Ok(Value::Str(base.to_string_value())),
                    },
                    "parse-base" => {
                        let radix = args
                            .first()
                            .and_then(|a| self.eval_expr(a).ok())
                            .and_then(|v| match v {
                                Value::Int(n) => Some(n as u32),
                                _ => None,
                            })
                            .unwrap_or(10);
                        let s = base.to_string_value();
                        match i64::from_str_radix(&s, radix) {
                            Ok(n) => Ok(Value::Int(n)),
                            Err(_) => Err(RuntimeError::new(format!(
                                "Cannot parse '{}' as base {}",
                                s, radix
                            ))),
                        }
                    }
                    // sqrt, floor, ceiling/ceil, round: handled by builtins::native_method_0arg
                    "narrow" => match base {
                        Value::Num(f) if f.fract() == 0.0 && f.is_finite() => {
                            Ok(Value::Int(f as i64))
                        }
                        _ => Ok(base),
                    },
                    // log, exp: handled by builtins::native_method_0arg
                    "push" => {
                        // .push on evaluated array - returns new array with item added
                        match base {
                            Value::Array(mut items) => {
                                let value = args
                                    .first()
                                    .and_then(|a| self.eval_expr(a).ok())
                                    .unwrap_or(Value::Nil);
                                items.push(value);
                                Ok(Value::Array(items))
                            }
                            _ => Ok(base),
                        }
                    }
                    "pop" => match base {
                        Value::Array(mut items) => Ok(items.pop().unwrap_or(Value::Nil)),
                        _ => Ok(Value::Nil),
                    },
                    "shift" => match base {
                        Value::Array(mut items) => {
                            if items.is_empty() {
                                Ok(Value::Nil)
                            } else {
                                Ok(items.remove(0))
                            }
                        }
                        _ => Ok(Value::Nil),
                    },
                    "unshift" => match base {
                        Value::Array(mut items) => {
                            let value = args
                                .first()
                                .and_then(|a| self.eval_expr(a).ok())
                                .unwrap_or(Value::Nil);
                            items.insert(0, value);
                            Ok(Value::Array(items))
                        }
                        _ => Ok(base),
                    },
                    "join" => match base {
                        Value::Array(items) => {
                            let sep = args
                                .first()
                                .and_then(|arg| self.eval_expr(arg).ok())
                                .map(|v| v.to_string_value())
                                .unwrap_or_default();
                            let joined = items
                                .iter()
                                .map(|v| v.to_string_value())
                                .collect::<Vec<_>>()
                                .join(&sep);
                            Ok(Value::Str(joined))
                        }
                        Value::Hash(items) => {
                            let sep = args
                                .first()
                                .and_then(|arg| self.eval_expr(arg).ok())
                                .map(|v| v.to_string_value())
                                .unwrap_or_default();
                            let joined = items
                                .iter()
                                .map(|(k, v)| format!("{}\t{}", k, v.to_string_value()))
                                .collect::<Vec<_>>()
                                .join(&sep);
                            Ok(Value::Str(joined))
                        }
                        _ => Ok(Value::Nil),
                    },
                    "WHY" => match base {
                        Value::Str(name) | Value::Package(name) => {
                            let doc = self.doc_comments.get(&name).cloned().unwrap_or_default();
                            Ok(Value::Str(doc))
                        }
                        _ => Ok(Value::Nil),
                    },
                    "version-matcher" | "auth-matcher" | "api-matcher" => match base {
                        Value::CompUnitDepSpec { .. } => Ok(Value::Bool(true)),
                        _ => Ok(Value::Nil),
                    },
                    "new" => match base {
                        Value::Str(name) | Value::Package(name) if name == "Rat" => {
                            let a = args.first().and_then(|e| self.eval_expr(e).ok());
                            let b = args.get(1).and_then(|e| self.eval_expr(e).ok());
                            let a = match a {
                                Some(Value::Int(i)) => i,
                                _ => 0,
                            };
                            let b = match b {
                                Some(Value::Int(i)) => i,
                                _ => 1,
                            };
                            Ok(make_rat(a, b))
                        }
                        Value::Package(name) if name == "FatRat" => {
                            let a = args.first().and_then(|e| self.eval_expr(e).ok());
                            let b = args.get(1).and_then(|e| self.eval_expr(e).ok());
                            let a = match a {
                                Some(Value::Int(i)) => i,
                                _ => 0,
                            };
                            let b = match b {
                                Some(Value::Int(i)) => i,
                                _ => 1,
                            };
                            Ok(Value::FatRat(a, b))
                        }
                        Value::Package(name) if name == "Map" || name == "Hash" => {
                            let mut map = HashMap::new();
                            let mut i = 0;
                            let mut eval_args = Vec::new();
                            for arg in args {
                                eval_args.push(self.eval_expr(arg)?);
                            }
                            while i + 1 < eval_args.len() {
                                let k = eval_args[i].to_string_value();
                                let v = eval_args[i + 1].clone();
                                map.insert(k, v);
                                i += 2;
                            }
                            Ok(Value::Hash(map))
                        }
                        Value::Package(name) if name == "Set" => {
                            let mut elems = HashSet::new();
                            for arg in args {
                                let val = self.eval_expr(arg)?;
                                match val {
                                    Value::Array(items) => {
                                        for item in items {
                                            elems.insert(item.to_string_value());
                                        }
                                    }
                                    other => {
                                        elems.insert(other.to_string_value());
                                    }
                                }
                            }
                            Ok(Value::Set(elems))
                        }
                        Value::Package(name) if name == "Bag" => {
                            let mut counts: HashMap<String, i64> = HashMap::new();
                            for arg in args {
                                let val = self.eval_expr(arg)?;
                                match val {
                                    Value::Array(items) => {
                                        for item in items {
                                            *counts.entry(item.to_string_value()).or_insert(0) += 1;
                                        }
                                    }
                                    other => {
                                        *counts.entry(other.to_string_value()).or_insert(0) += 1;
                                    }
                                }
                            }
                            Ok(Value::Bag(counts))
                        }
                        Value::Package(name) if name == "Mix" => {
                            let mut weights: HashMap<String, f64> = HashMap::new();
                            for arg in args {
                                let val = self.eval_expr(arg)?;
                                match val {
                                    Value::Array(items) => {
                                        for item in items {
                                            *weights
                                                .entry(item.to_string_value())
                                                .or_insert(0.0) += 1.0;
                                        }
                                    }
                                    other => {
                                        *weights.entry(other.to_string_value()).or_insert(0.0) +=
                                            1.0;
                                    }
                                }
                            }
                            Ok(Value::Mix(weights))
                        }
                        Value::Package(name) if name == "Complex" => {
                            let a = args.first().and_then(|e| self.eval_expr(e).ok());
                            let b = args.get(1).and_then(|e| self.eval_expr(e).ok());
                            let re = match a {
                                Some(Value::Int(i)) => i as f64,
                                Some(Value::Num(f)) => f,
                                _ => 0.0,
                            };
                            let im = match b {
                                Some(Value::Int(i)) => i as f64,
                                Some(Value::Num(f)) => f,
                                _ => 0.0,
                            };
                            Ok(Value::Complex(re, im))
                        }
                        Value::Package(name) if name == "CompUnit::DependencySpecification" => {
                            let mut short_name = None;
                            if let Some(arg) = args.first() {
                                if let Expr::AssignExpr { name, expr } = arg {
                                    if name == "short-name"
                                        && let Ok(Value::Str(s)) = self.eval_expr(expr)
                                    {
                                        short_name = Some(s);
                                    }
                                } else if let Ok(Value::Pair(ref k, ref v)) = self.eval_expr(arg)
                                    && k == "short-name"
                                    && let Value::Str(s) = v.as_ref()
                                {
                                    short_name = Some(s.clone());
                                } else if let Ok(Value::Str(s)) = self.eval_expr(arg) {
                                    short_name = Some(s);
                                }
                            }
                            if let Some(s) = short_name {
                                Ok(Value::CompUnitDepSpec { short_name: s })
                            } else {
                                Err(RuntimeError::new(
                                    "CompUnit::DependencySpecification requires short-name",
                                ))
                            }
                        }
                        _ => Ok(Value::Nil),
                    },
                    _ => Ok(Value::Nil),
                }
            }
            Expr::Exists(inner) => match inner.as_ref() {
                Expr::EnvIndex(key) => Ok(Value::Bool(std::env::var_os(key).is_some())),
                _ => Ok(Value::Bool(self.eval_expr(inner)?.truthy())),
            },
            Expr::Subst {
                pattern,
                replacement,
            } => {
                let target = self.env.get("_").cloned().unwrap_or(Value::Nil);
                let text = target.to_string_value();
                if let Some((start, end)) = self.regex_find_first(pattern, &text) {
                    let start_b = Self::char_idx_to_byte(&text, start);
                    let end_b = Self::char_idx_to_byte(&text, end);
                    let mut out = String::new();
                    out.push_str(&text[..start_b]);
                    out.push_str(replacement);
                    out.push_str(&text[end_b..]);
                    let result = Value::Str(out.clone());
                    self.env.insert("_".to_string(), result.clone());
                    Ok(result)
                } else {
                    Ok(Value::Str(text))
                }
            }
            Expr::CallOn { target, args } => self.eval_call_on_expr(target, args),
            Expr::Unary { op, expr } => self.eval_unary_expr(op, expr),
            Expr::PostfixOp { op, expr } => self.eval_postfix_expr(op, expr),
            Expr::Binary { left, op, right } => self.eval_binary_expr(left, op, right),
            Expr::Hash(pairs) => {
                let mut map = HashMap::new();
                for (key, value_expr) in pairs {
                    let value = if let Some(expr) = value_expr {
                        self.eval_expr(expr)?
                    } else {
                        Value::Bool(true)
                    };
                    map.insert(key.clone(), value);
                }
                Ok(Value::Hash(map))
            }
            Expr::Call { name, args } => {
                // Handle infix:<op>(args) operator-as-function calls
                if let Some(op_name) = name
                    .strip_prefix("infix:<")
                    .and_then(|s| s.strip_suffix('>'))
                {
                    return self.eval_infix_as_func(op_name, args);
                }
                // die/fail in expression context should throw immediately
                if name == "die" || name == "fail" {
                    let msg = if let Some(arg) = args.first() {
                        self.eval_expr(arg)?.to_string_value()
                    } else {
                        "Died".to_string()
                    };
                    return Err(RuntimeError::new(&msg));
                }
                // Handle test functions in expression context (e.g., `diag "x" if ! ok ...`)
                if name == "ok" {
                    let desc = args
                        .get(1)
                        .map(|e| self.eval_expr(e))
                        .transpose()?
                        .map(|v| v.to_string_value())
                        .unwrap_or_default();
                    let value = args
                        .first()
                        .map(|e| self.eval_expr(e))
                        .transpose()?
                        .unwrap_or(Value::Nil);
                    let ok = value.truthy();
                    self.test_ok(ok, &desc, false)?;
                    return Ok(Value::Bool(ok));
                }
                if name == "nok" {
                    let desc = args
                        .get(1)
                        .map(|e| self.eval_expr(e))
                        .transpose()?
                        .map(|v| v.to_string_value())
                        .unwrap_or_default();
                    let value = args
                        .first()
                        .map(|e| self.eval_expr(e))
                        .transpose()?
                        .unwrap_or(Value::Nil);
                    let ok = !value.truthy();
                    self.test_ok(ok, &desc, false)?;
                    return Ok(Value::Bool(ok));
                }
                if name == "isa-ok" {
                    let value = args
                        .first()
                        .map(|e| self.eval_expr(e))
                        .transpose()?
                        .unwrap_or(Value::Nil);
                    let type_name = args
                        .get(1)
                        .map(|e| self.eval_expr(e))
                        .transpose()?
                        .map(|v| v.to_string_value())
                        .unwrap_or_default();
                    let desc = args
                        .get(2)
                        .map(|e| self.eval_expr(e))
                        .transpose()?
                        .map(|v| v.to_string_value())
                        .unwrap_or_default();
                    let ok = match type_name.as_str() {
                        "Array" => matches!(value, Value::Array(_)),
                        "Rat" => matches!(value, Value::Rat(_, _)),
                        "FatRat" => matches!(value, Value::FatRat(_, _)),
                        "Complex" => matches!(value, Value::Complex(_, _)),
                        "Set" => matches!(value, Value::Set(_)),
                        "Bag" => matches!(value, Value::Bag(_)),
                        "Mix" => matches!(value, Value::Mix(_)),
                        _ => {
                            if let Value::Instance { class_name, .. } = &value {
                                class_name == type_name.as_str()
                            } else {
                                true
                            }
                        }
                    };
                    self.test_ok(ok, &desc, false)?;
                    return Ok(Value::Bool(ok));
                }
                if name == "diag" {
                    let msg = args
                        .first()
                        .map(|e| self.eval_expr(e))
                        .transpose()?
                        .map(|v| v.to_string_value())
                        .unwrap_or_default();
                    self.output.push_str(&format!("# {}\n", msg));
                    return Ok(Value::Nil);
                }
                if name == "pass" {
                    let desc = args
                        .first()
                        .map(|e| self.eval_expr(e))
                        .transpose()?
                        .map(|v| v.to_string_value())
                        .unwrap_or_default();
                    self.test_ok(true, &desc, false)?;
                    return Ok(Value::Bool(true));
                }
                if name == "flunk" {
                    let desc = args
                        .first()
                        .map(|e| self.eval_expr(e))
                        .transpose()?
                        .map(|v| v.to_string_value())
                        .unwrap_or_default();
                    self.test_ok(false, &desc, false)?;
                    return Ok(Value::Bool(false));
                }
                if name == "is" || name == "isnt" {
                    let desc = args
                        .get(2)
                        .map(|e| self.eval_expr(e))
                        .transpose()?
                        .map(|v| v.to_string_value())
                        .unwrap_or_default();
                    let left = args
                        .first()
                        .map(|e| self.eval_expr(e))
                        .transpose()?
                        .unwrap_or(Value::Nil);
                    let right = args
                        .get(1)
                        .map(|e| self.eval_expr(e))
                        .transpose()?
                        .unwrap_or(Value::Nil);
                    let eq = left.to_string_value() == right.to_string_value();
                    let ok = if name == "isnt" { !eq } else { eq };
                    self.test_ok(ok, &desc, false)?;
                    return Ok(Value::Bool(ok));
                }
                if name == "make" {
                    let value = if let Some(arg) = args.first() {
                        self.eval_expr(arg)?
                    } else {
                        Value::Nil
                    };
                    self.env.insert("made".to_string(), value.clone());
                    return Ok(value);
                }
                if name == "made" {
                    return Ok(self.env.get("made").cloned().unwrap_or(Value::Nil));
                }
                if name == "callframe" {
                    let depth = args
                        .first()
                        .and_then(|e| self.eval_expr(e).ok())
                        .and_then(|v| match v {
                            Value::Int(i) if i >= 0 => Some(i as usize),
                            Value::Num(f) if f >= 0.0 => Some(f as usize),
                            _ => None,
                        })
                        .unwrap_or(0);
                    if let Some(frame) = self.callframe_value(depth) {
                        return Ok(frame);
                    }
                    return Ok(Value::Nil);
                }
                if name == "caller" {
                    let depth = args
                        .first()
                        .and_then(|e| self.eval_expr(e).ok())
                        .and_then(|v| match v {
                            Value::Int(i) if i >= 0 => Some(i as usize),
                            Value::Num(f) if f >= 0.0 => Some(f as usize),
                            _ => None,
                        })
                        .unwrap_or(1);
                    if let Some(frame) = self.callframe_value(depth) {
                        return Ok(frame);
                    }
                    return Ok(Value::Nil);
                }
                if matches!(name.as_str(), "Int" | "Num" | "Str" | "Bool")
                    && let Some(arg) = args.first()
                {
                    let value = self.eval_expr(arg)?;
                    let coerced = match name.as_str() {
                        "Int" => match value {
                            Value::Int(i) => Value::Int(i),
                            Value::Num(f) => Value::Int(f as i64),
                            Value::Rat(n, d) => {
                                if d == 0 {
                                    Value::Int(0)
                                } else {
                                    Value::Int(n / d)
                                }
                            }
                            Value::Complex(r, _) => Value::Int(r as i64),
                            Value::Str(s) => Value::Int(s.trim().parse::<i64>().unwrap_or(0)),
                            Value::Bool(b) => Value::Int(if b { 1 } else { 0 }),
                            _ => Value::Int(0),
                        },
                        "Num" => match value {
                            Value::Int(i) => Value::Num(i as f64),
                            Value::Num(f) => Value::Num(f),
                            Value::Rat(n, d) => {
                                if d == 0 {
                                    Value::Num(if n == 0 {
                                        f64::NAN
                                    } else if n > 0 {
                                        f64::INFINITY
                                    } else {
                                        f64::NEG_INFINITY
                                    })
                                } else {
                                    Value::Num(n as f64 / d as f64)
                                }
                            }
                            Value::Complex(r, _) => Value::Num(r),
                            Value::Str(s) => {
                                if let Ok(i) = s.trim().parse::<i64>() {
                                    Value::Num(i as f64)
                                } else if let Ok(f) = s.trim().parse::<f64>() {
                                    Value::Num(f)
                                } else {
                                    Value::Num(0.0)
                                }
                            }
                            Value::Bool(b) => Value::Num(if b { 1.0 } else { 0.0 }),
                            _ => Value::Num(0.0),
                        },
                        "Str" => Value::Str(value.to_string_value()),
                        "Bool" => Value::Bool(value.truthy()),
                        _ => Value::Nil,
                    };
                    return Ok(coerced);
                }
                if let Some(pattern) = self.eval_token_call(name, args)? {
                    return Ok(Value::Regex(pattern));
                }
                // Try type-based multi dispatch first
                let arg_values: Vec<Value> =
                    args.iter().filter_map(|a| self.eval_expr(a).ok()).collect();
                if let Some(def) = self.resolve_function_with_types(name, &arg_values) {
                    let saved_env = self.env.clone();
                    let literal_args: Vec<Expr> =
                        arg_values.into_iter().map(Expr::Literal).collect();
                    self.bind_function_args(&def.param_defs, &def.params, &literal_args)?;
                    self.routine_stack
                        .push((def.package.clone(), def.name.clone()));
                    let result = self.eval_block_value(&def.body);
                    self.routine_stack.pop();
                    self.env = saved_env;
                    return match result {
                        Err(e) if e.return_value.is_some() => Ok(e.return_value.unwrap()),
                        other => other,
                    };
                }
                if self.has_proto(name) {
                    return Err(RuntimeError::new(format!(
                        "No matching candidates for proto sub: {}",
                        name
                    )));
                }
                if name == "EVALFILE" {
                    let path = args
                        .first()
                        .and_then(|e| self.eval_expr(e).ok())
                        .map(|v| v.to_string_value())
                        .ok_or_else(|| RuntimeError::new("EVALFILE requires a filename"))?;
                    let code = fs::read_to_string(&path).map_err(|err| {
                        RuntimeError::new(format!("Failed to read {}: {}", path, err))
                    })?;
                    return self.eval_eval_string(&code);
                }
                if name == "EVAL" {
                    let code = if let Some(arg) = args.first() {
                        self.eval_expr(arg)?.to_string_value()
                    } else {
                        String::new()
                    };
                    if code.contains("&?ROUTINE") && self.routine_stack.is_empty() {
                        return Err(RuntimeError::new("X::Undeclared::Symbols"));
                    }
                    return self.eval_eval_string(&code);
                }
                if name == "shift" || name == "pop" {
                    if let Some(arg) = args.first() {
                        // Get the variable name to mutate in-place
                        let var_key = match arg {
                            Expr::ArrayVar(v) => Some(format!("@{}", v)),
                            Expr::Var(v) => Some(v.clone()),
                            _ => None,
                        };
                        if let Some(key) = var_key
                            && let Some(Value::Array(items)) = self.env.get_mut(&key)
                        {
                            if items.is_empty() {
                                return Ok(Value::Nil);
                            }
                            return Ok(if name == "shift" {
                                items.remove(0)
                            } else {
                                items.pop().unwrap_or(Value::Nil)
                            });
                        }
                    }
                    return Ok(Value::Nil);
                }
                if name == "push" || name == "unshift" || name == "append" || name == "prepend" {
                    if let Some(arr_arg) = args.first() {
                        let var_key = match arr_arg {
                            Expr::ArrayVar(v) => Some(format!("@{}", v)),
                            Expr::Var(v) => Some(v.clone()),
                            _ => None,
                        };
                        if let Some(key) = var_key {
                            let values: Vec<Value> = args[1..]
                                .iter()
                                .map(|e| self.eval_expr(e).unwrap_or(Value::Nil))
                                .collect();
                            if let Some(Value::Array(items)) = self.env.get_mut(&key) {
                                if name == "push" || name == "append" {
                                    items.extend(values);
                                } else {
                                    for v in values.into_iter().rev() {
                                        items.insert(0, v);
                                    }
                                }
                            } else {
                                self.env.insert(key, Value::Array(values));
                            }
                            return Ok(Value::Nil);
                        }
                    }
                    return Ok(Value::Nil);
                }
                if name == "elems" {
                    let val = args.first().and_then(|e| self.eval_expr(e).ok());
                    return Ok(match val {
                        Some(Value::Array(items)) => Value::Int(items.len() as i64),
                        Some(Value::LazyList(list)) => {
                            Value::Int(self.force_lazy_list(&list)?.len() as i64)
                        }
                        Some(Value::Hash(items)) => Value::Int(items.len() as i64),
                        Some(Value::Str(s)) => Value::Int(s.chars().count() as i64),
                        _ => Value::Int(0),
                    });
                }
                if name == "keys" {
                    let val = args.first().and_then(|e| self.eval_expr(e).ok());
                    return Ok(match val {
                        Some(Value::Hash(items)) => {
                            Value::Array(items.keys().map(|k| Value::Str(k.clone())).collect())
                        }
                        _ => Value::Array(Vec::new()),
                    });
                }
                if name == "values" {
                    let val = args.first().and_then(|e| self.eval_expr(e).ok());
                    return Ok(match val {
                        Some(Value::Hash(items)) => Value::Array(items.values().cloned().collect()),
                        _ => Value::Array(Vec::new()),
                    });
                }
                if name == "abs" {
                    let val = args.first().and_then(|e| self.eval_expr(e).ok());
                    return Ok(match val {
                        Some(Value::Int(i)) => Value::Int(i.abs()),
                        Some(Value::Num(f)) => Value::Num(f.abs()),
                        _ => Value::Int(0),
                    });
                }
                if name == "chars" {
                    use unicode_segmentation::UnicodeSegmentation;
                    let val = args.first().and_then(|e| self.eval_expr(e).ok());
                    return Ok(match val {
                        Some(Value::Str(s)) => Value::Int(s.graphemes(true).count() as i64),
                        Some(v) => Value::Int(v.to_string_value().graphemes(true).count() as i64),
                        _ => Value::Int(0),
                    });
                }
                if name == "sprintf" {
                    let fmt = args.first().and_then(|e| self.eval_expr(e).ok());
                    let fmt = match fmt {
                        Some(Value::Str(s)) => s,
                        _ => String::new(),
                    };
                    let arg = args.get(1).and_then(|e| self.eval_expr(e).ok());
                    let rendered = Self::format_sprintf(&fmt, arg.as_ref());
                    return Ok(Value::Str(rendered));
                }
                if name == "join" {
                    let sep = args
                        .first()
                        .and_then(|e| self.eval_expr(e).ok())
                        .map(|v| v.to_string_value())
                        .unwrap_or_default();
                    let list = args.get(1).and_then(|e| self.eval_expr(e).ok());
                    return Ok(match list {
                        Some(Value::Array(items)) => {
                            let joined = items
                                .iter()
                                .map(|v| v.to_string_value())
                                .collect::<Vec<_>>()
                                .join(&sep);
                            Value::Str(joined)
                        }
                        _ => Value::Str(String::new()),
                    });
                }
                if name == "set" {
                    let mut elems = HashSet::new();
                    for arg in args {
                        let val = self.eval_expr(arg)?;
                        match val {
                            Value::Array(items) => {
                                for item in items {
                                    elems.insert(item.to_string_value());
                                }
                            }
                            other => {
                                elems.insert(other.to_string_value());
                            }
                        }
                    }
                    return Ok(Value::Set(elems));
                }
                if name == "bag" {
                    let mut counts: HashMap<String, i64> = HashMap::new();
                    for arg in args {
                        let val = self.eval_expr(arg)?;
                        match val {
                            Value::Array(items) => {
                                for item in items {
                                    *counts.entry(item.to_string_value()).or_insert(0) += 1;
                                }
                            }
                            other => {
                                *counts.entry(other.to_string_value()).or_insert(0) += 1;
                            }
                        }
                    }
                    return Ok(Value::Bag(counts));
                }
                if name == "mix" {
                    let mut weights: HashMap<String, f64> = HashMap::new();
                    for arg in args {
                        let val = self.eval_expr(arg)?;
                        match val {
                            Value::Array(items) => {
                                for item in items {
                                    *weights.entry(item.to_string_value()).or_insert(0.0) += 1.0;
                                }
                            }
                            other => {
                                *weights.entry(other.to_string_value()).or_insert(0.0) += 1.0;
                            }
                        }
                    }
                    return Ok(Value::Mix(weights));
                }
                if name == "hash" {
                    let mut flat_values = Vec::new();
                    for arg in args {
                        let val = self.eval_expr(arg)?;
                        flat_values.extend(Self::value_to_list(&val));
                    }
                    let mut map = HashMap::new();
                    let mut iter = flat_values.into_iter();
                    while let Some(item) = iter.next() {
                        match item {
                            Value::Pair(key, boxed_val) => {
                                map.insert(key, *boxed_val);
                            }
                            other => {
                                let key = other.to_string_value();
                                let value = iter.next().unwrap_or(Value::Nil);
                                map.insert(key, value);
                            }
                        }
                    }
                    return Ok(Value::Hash(map));
                }
                if name == "chroot" {
                    let path_str = args
                        .first()
                        .and_then(|expr| self.eval_expr(expr).ok())
                        .map(|val| val.to_string_value())
                        .or_else(|| {
                            self.get_dynamic_string("$*CWD")
                                .or_else(|| self.get_dynamic_string("$*CHROOT"))
                        })
                        .unwrap_or_else(|| ".".to_string());
                    let path_buf = PathBuf::from(path_str.clone());
                    if !path_buf.is_dir() {
                        return Ok(Value::Bool(false));
                    }
                    let canonical = path_buf.canonicalize().unwrap_or_else(|_| path_buf.clone());
                    if std::env::set_current_dir(&canonical).is_err() {
                        return Ok(Value::Bool(false));
                    }
                    self.chroot_root = Some(canonical.clone());
                    let repr = Self::stringify_path(&canonical);
                    self.env
                        .insert("$*CHROOT".to_string(), Value::Str(repr.clone()));
                    self.env.insert("$*CWD".to_string(), Value::Str(repr));
                    return Ok(Value::Bool(true));
                }
                if name == "gethost" {
                    let host_str = args
                        .first()
                        .and_then(|expr| self.eval_expr(expr).ok())
                        .map(|val| val.to_string_value());
                    let hostname = host_str.unwrap_or_else(Self::hostname);
                    let addrs = Self::resolve_host(&hostname);
                    return Ok(Self::make_os_name_value(hostname, addrs));
                }
                if name == "kill" {
                    let mut iter = args.iter();
                    let signal = iter
                        .next()
                        .and_then(|expr| self.eval_expr(expr).ok())
                        .map(|val| Self::to_int(&val))
                        .unwrap_or(15);
                    let mut success = true;
                    let mut had_pid = false;
                    for expr in iter {
                        if let Ok(val) = self.eval_expr(expr) {
                            had_pid = true;
                            let pid = Self::to_int(&val);
                            success &= Self::send_signal(pid, signal);
                        } else {
                            success = false;
                        }
                    }
                    if !had_pid {
                        success = false;
                    }
                    return Ok(Value::Bool(success));
                }
                if name == "shell" {
                    let command_str = args
                        .first()
                        .and_then(|expr| self.eval_expr(expr).ok())
                        .map(|val| val.to_string_value())
                        .unwrap_or_default();
                    if command_str.is_empty() {
                        return Ok(Value::Bool(false));
                    }
                    let mut opts = HashMap::new();
                    let mut cwd_opt: Option<String> = None;
                    for expr in args.iter().skip(1) {
                        if let Value::Hash(map) = self.eval_expr(expr)? {
                            for (key, value) in map {
                                match key.as_str() {
                                    "cwd" => {
                                        cwd_opt = Some(value.to_string_value());
                                    }
                                    "env" => {
                                        if let Value::Hash(env_map) = value {
                                            for (ek, ev) in env_map {
                                                opts.insert(ek, ev.to_string_value());
                                            }
                                        }
                                    }
                                    _ => {}
                                }
                            }
                        }
                    }
                    let mut command = if cfg!(windows) {
                        let mut cmd = Command::new("cmd");
                        cmd.arg("/C").arg(&command_str);
                        cmd
                    } else {
                        let mut cmd = Command::new("sh");
                        cmd.arg("-c").arg(&command_str);
                        cmd
                    };
                    if let Some(cwd) = cwd_opt {
                        command.current_dir(cwd);
                    }
                    for (k, v) in opts {
                        command.env(k, v);
                    }
                    let status = command
                        .status()
                        .map(|status| status.success())
                        .unwrap_or(false);
                    return Ok(Value::Bool(status));
                }
                if name == "syscall" {
                    let num_val = args.first().and_then(|expr| self.eval_expr(expr).ok());
                    if let Some(val) = num_val {
                        let num = Self::to_int(&val);
                        if num == 0 {
                            let pid = self
                                .env
                                .get("*PID")
                                .and_then(|v| match v {
                                    Value::Int(i) => Some(*i),
                                    _ => None,
                                })
                                .unwrap_or_else(|| std::process::id() as i64);
                            return Ok(Value::Int(pid));
                        }
                        return Ok(Value::Int(-1));
                    }
                    return Ok(Value::Nil);
                }
                if name == "getlogin" {
                    let login = Self::get_login_name().unwrap_or_default();
                    return Ok(Value::Str(login));
                }
                if name == "sleep" {
                    let arg_val = args.first().and_then(|e| self.eval_expr(e).ok());
                    let duration =
                        Self::duration_from_seconds(Self::seconds_from_value(arg_val.clone()));
                    thread::sleep(duration);
                    return Ok(Value::Nil);
                }
                if name == "sleep-timer" {
                    let arg_val = args.first().and_then(|e| self.eval_expr(e).ok());
                    let duration =
                        Self::duration_from_seconds(Self::seconds_from_value(arg_val.clone()));
                    let start = Instant::now();
                    thread::sleep(duration);
                    let elapsed = start.elapsed();
                    let remaining = duration.checked_sub(elapsed).unwrap_or_default();
                    return Ok(Value::Num(remaining.as_secs_f64()));
                }
                if name == "sleep-till" {
                    let arg_val = args.first().and_then(|e| self.eval_expr(e).ok());
                    if let Some(target_time) = Self::system_time_from_value(arg_val) {
                        let now = SystemTime::now();
                        if target_time <= now {
                            return Ok(Value::Bool(false));
                        }
                        if let Ok(diff) = target_time.duration_since(now) {
                            thread::sleep(diff);
                            return Ok(Value::Bool(true));
                        }
                    }
                    return Ok(Value::Bool(false));
                }
                if name == "any" || name == "all" || name == "one" || name == "none" {
                    let kind = match name.as_str() {
                        "any" => JunctionKind::Any,
                        "all" => JunctionKind::All,
                        "one" => JunctionKind::One,
                        _ => JunctionKind::None,
                    };
                    let mut elems = Vec::new();
                    for arg in args {
                        let val = self.eval_expr(arg)?;
                        match val {
                            Value::Array(items) => elems.extend(items),
                            other => elems.push(other),
                        }
                    }
                    return Ok(Value::Junction {
                        kind,
                        values: elems,
                    });
                }
                // start: synchronously evaluate the block, return a Promise
                if name == "start" {
                    if let Some(block_expr) = args.first() {
                        let block_val = self.eval_expr(block_expr)?;
                        match self.call_sub_value(block_val, vec![], false) {
                            Ok(result) => {
                                return Ok(self.make_promise_instance("Kept", result));
                            }
                            Err(e) => {
                                return Ok(self.make_promise_instance(
                                    "Broken",
                                    Value::Str(e.message.clone()),
                                ));
                            }
                        }
                    }
                    return Ok(self.make_promise_instance("Kept", Value::Nil));
                }
                // await: extract result from a Promise (synchronous)
                if name == "await" {
                    let val = if let Some(arg) = args.first() {
                        self.eval_expr(arg)?
                    } else {
                        Value::Nil
                    };
                    if let Value::Instance {
                        class_name,
                        attributes,
                        ..
                    } = &val
                        && class_name == "Promise"
                    {
                        return Ok(attributes.get("result").cloned().unwrap_or(Value::Nil));
                    }
                    return Ok(val);
                }
                if name == "item" {
                    let val = args.first().and_then(|e| self.eval_expr(e).ok());
                    return Ok(val.unwrap_or(Value::Nil));
                }
                if name == "list" {
                    let mut result = Vec::new();
                    for arg in args {
                        let val = self.eval_expr(arg)?;
                        for item in Self::value_to_list(&val) {
                            result.push(item);
                        }
                    }
                    return Ok(Value::Array(result));
                }
                if name == "lol" {
                    let mut result = Vec::new();
                    for arg in args {
                        result.push(self.eval_expr(arg)?);
                    }
                    return Ok(Value::Array(result));
                }
                if name == "flat" {
                    let mut result = Vec::new();
                    for arg in args {
                        let val = self.eval_expr(arg)?;
                        match val {
                            Value::Array(items) => {
                                for item in items {
                                    if let Value::Array(sub) = item {
                                        result.extend(sub);
                                    } else {
                                        result.push(item);
                                    }
                                }
                            }
                            other => result.push(other),
                        }
                    }
                    return Ok(Value::Array(result));
                }
                if name == "classify" || name == "categorize" {
                    let func_expr = match args.first() {
                        Some(expr) => expr,
                        None => return Ok(Value::Hash(HashMap::new())),
                    };
                    let func = self.eval_expr(func_expr)?;
                    let mut buckets: HashMap<String, Vec<Value>> = HashMap::new();
                    for expr in args.iter().skip(1) {
                        let item = self.eval_expr(expr)?;
                        let keys = match self.call_lambda_with_arg(&func, item.clone()) {
                            Ok(Value::Array(values)) => values,
                            Ok(value) => vec![value],
                            Err(_) => vec![Value::Nil],
                        };
                        let target_keys = if name == "classify" {
                            if keys.is_empty() {
                                vec![Value::Nil]
                            } else {
                                vec![keys[0].clone()]
                            }
                        } else {
                            keys
                        };
                        for key in target_keys {
                            let bucket_key = key.to_string_value();
                            buckets.entry(bucket_key).or_default().push(item.clone());
                        }
                    }
                    let hash_map = buckets
                        .into_iter()
                        .map(|(k, v)| (k, Value::Array(v)))
                        .collect();
                    return Ok(Value::Hash(hash_map));
                }
                if name == "reverse" {
                    let val = args.first().and_then(|e| self.eval_expr(e).ok());
                    return Ok(match val {
                        Some(Value::Array(mut items)) => {
                            items.reverse();
                            Value::Array(items)
                        }
                        Some(Value::Str(s)) => Value::Str(s.chars().rev().collect()),
                        _ => Value::Nil,
                    });
                }
                if name == "sort" {
                    let val = args.first().and_then(|e| self.eval_expr(e).ok());
                    return Ok(match val {
                        Some(Value::Array(mut items)) => {
                            items.sort_by_key(|a| a.to_string_value());
                            Value::Array(items)
                        }
                        _ => Value::Nil,
                    });
                }
                if name == "slip" {
                    let mut items = Vec::new();
                    for arg_expr in args {
                        let val = self.eval_expr(arg_expr)?;
                        match val {
                            Value::Array(elems) => items.extend(elems),
                            Value::Slip(elems) => items.extend(elems),
                            other => items.push(other),
                        }
                    }
                    return Ok(Value::Slip(items));
                }
                if name == "map" {
                    // map { BLOCK }, @list
                    let mut arg_iter = args.iter();
                    let func = arg_iter.next().map(|e| self.eval_expr(e)).transpose()?;
                    // Collect remaining args into a list
                    let mut list_items = Vec::new();
                    for arg_expr in arg_iter {
                        let val = self.eval_expr(arg_expr)?;
                        match val {
                            Value::Array(items) => list_items.extend(items),
                            Value::Range(a, b) => {
                                list_items.extend((a..=b).map(Value::Int));
                            }
                            Value::RangeExcl(a, b) => {
                                list_items.extend((a..b).map(Value::Int));
                            }
                            Value::RangeExclStart(a, b) => {
                                list_items.extend((a + 1..=b).map(Value::Int));
                            }
                            Value::RangeExclBoth(a, b) => {
                                list_items.extend((a + 1..b).map(Value::Int));
                            }
                            other => list_items.push(other),
                        }
                    }
                    if let Some(Value::Sub {
                        params, body, env, ..
                    }) = func
                    {
                        let placeholders = collect_placeholders(&body);
                        let arity = if !params.is_empty() {
                            params.len()
                        } else if !placeholders.is_empty() {
                            placeholders.len()
                        } else {
                            1
                        };
                        let mut result = Vec::new();
                        let mut i = 0;
                        while i < list_items.len() {
                            if arity > 1 && i + arity > list_items.len() {
                                return Err(RuntimeError::new(
                                    "Not enough elements for map block arity",
                                ));
                            }
                            let saved = self.env.clone();
                            for (k, v) in &env {
                                self.env.insert(k.clone(), v.clone());
                            }
                            if arity == 1 {
                                let item = list_items[i].clone();
                                if let Some(p) = params.first() {
                                    self.env.insert(p.clone(), item.clone());
                                }
                                if let Some(ph) = placeholders.first() {
                                    self.env.insert(ph.clone(), item.clone());
                                }
                                self.env.insert("_".to_string(), item);
                            } else {
                                // Multi-arity: bind positional params/placeholders
                                for (idx, p) in params.iter().enumerate() {
                                    if i + idx < list_items.len() {
                                        self.env.insert(p.clone(), list_items[i + idx].clone());
                                    }
                                }
                                for (idx, ph) in placeholders.iter().enumerate() {
                                    if i + idx < list_items.len() {
                                        self.env.insert(ph.clone(), list_items[i + idx].clone());
                                    }
                                }
                                self.env.insert("_".to_string(), list_items[i].clone());
                            }
                            match self.eval_block_value(&body) {
                                Ok(Value::Slip(elems)) => {
                                    self.env = saved;
                                    result.extend(elems);
                                }
                                Ok(val) => {
                                    self.env = saved;
                                    result.push(val);
                                }
                                Err(e) if e.is_next => {
                                    self.env = saved;
                                    // next: skip this element
                                }
                                Err(e) if e.is_last => {
                                    self.env = saved;
                                    break;
                                }
                                Err(e) => {
                                    self.env = saved;
                                    return Err(e);
                                }
                            }
                            i += arity;
                        }
                        return Ok(Value::Array(result));
                    }
                    return Ok(Value::Array(list_items));
                }
                if name == "grep" {
                    // grep { BLOCK }, @list
                    let mut arg_iter = args.iter();
                    let func = arg_iter.next().map(|e| self.eval_expr(e)).transpose()?;
                    let mut list_items = Vec::new();
                    for arg_expr in arg_iter {
                        let val = self.eval_expr(arg_expr)?;
                        match val {
                            Value::Array(items) => list_items.extend(items),
                            Value::Range(a, b) => {
                                list_items.extend((a..=b).map(Value::Int));
                            }
                            Value::RangeExcl(a, b) => {
                                list_items.extend((a..b).map(Value::Int));
                            }
                            other => list_items.push(other),
                        }
                    }
                    if let Some(Value::Sub {
                        params, body, env, ..
                    }) = func
                    {
                        let placeholders = collect_placeholders(&body);
                        let mut result = Vec::new();
                        for item in list_items {
                            let saved = self.env.clone();
                            for (k, v) in &env {
                                self.env.insert(k.clone(), v.clone());
                            }
                            if let Some(p) = params.first() {
                                self.env.insert(p.clone(), item.clone());
                            }
                            if let Some(ph) = placeholders.first() {
                                self.env.insert(ph.clone(), item.clone());
                            }
                            self.env.insert("_".to_string(), item.clone());
                            let val = self.eval_block_value(&body)?;
                            self.env = saved;
                            if val.truthy() {
                                result.push(item);
                            }
                        }
                        return Ok(Value::Array(result));
                    }
                    return Ok(Value::Array(list_items));
                }
                if name == "defined" {
                    let val = args.first().and_then(|e| self.eval_expr(e).ok());
                    return Ok(Value::Bool(match &val {
                        Some(Value::Nil) | Some(Value::Package(_)) | None => false,
                        Some(Value::Slip(items)) if items.is_empty() => false,
                        _ => true,
                    }));
                }
                if name == "undefine" {
                    // undefine($var) sets the variable to Nil (undefined)
                    if let Some(arg) = args.first() {
                        match arg {
                            Expr::Var(n) => {
                                self.env.insert(n.clone(), Value::Nil);
                            }
                            Expr::ArrayVar(n) => {
                                self.env.insert(n.clone(), Value::Array(vec![]));
                            }
                            Expr::HashVar(n) => {
                                self.env.insert(n.clone(), Value::Hash(HashMap::new()));
                            }
                            _ => {}
                        }
                    }
                    return Ok(Value::Nil);
                }
                if name == "VAR" {
                    // VAR($x) returns a container object with the variable name
                    if let Some(arg) = args.first() {
                        let (var_name, class_name) = match arg {
                            Expr::Var(n) => (format!("${}", n), "Scalar"),
                            Expr::ArrayVar(n) => (format!("@{}", n), "Array"),
                            Expr::HashVar(n) => (format!("%{}", n), "Hash"),
                            Expr::CodeVar(n) => (format!("&{}", n), "Scalar"),
                            _ => {
                                let value = self.eval_expr(arg)?;
                                return Ok(value);
                            }
                        };
                        let mut attributes = HashMap::new();
                        attributes.insert("name".to_string(), Value::Str(var_name));
                        return Ok(Value::make_instance(class_name.to_string(), attributes));
                    }
                    return Ok(Value::Nil);
                }
                if name == "sqrt" {
                    let val = args.first().and_then(|e| self.eval_expr(e).ok());
                    return Ok(match val {
                        Some(Value::Int(i)) => Value::Num((i as f64).sqrt()),
                        Some(Value::Num(f)) => Value::Num(f.sqrt()),
                        _ => Value::Num(f64::NAN),
                    });
                }
                if name == "floor" {
                    let val = args.first().and_then(|e| self.eval_expr(e).ok());
                    return Ok(match val {
                        Some(Value::Num(f)) if f.is_nan() || f.is_infinite() => Value::Num(f),
                        Some(Value::Num(f)) => Value::Int(f.floor() as i64),
                        Some(Value::Int(i)) => Value::Int(i),
                        _ => Value::Int(0),
                    });
                }
                if name == "ceiling" || name == "ceil" {
                    let val = args.first().and_then(|e| self.eval_expr(e).ok());
                    return Ok(match val {
                        Some(Value::Num(f)) if f.is_nan() || f.is_infinite() => Value::Num(f),
                        Some(Value::Num(f)) => Value::Int(f.ceil() as i64),
                        Some(Value::Int(i)) => Value::Int(i),
                        _ => Value::Int(0),
                    });
                }
                if name == "round" {
                    let val = args.first().and_then(|e| self.eval_expr(e).ok());
                    return Ok(match val {
                        Some(Value::Num(f)) if f.is_nan() || f.is_infinite() => Value::Num(f),
                        Some(Value::Num(f)) => Value::Int(f.round() as i64),
                        Some(Value::Int(i)) => Value::Int(i),
                        _ => Value::Int(0),
                    });
                }
                if name == "log" {
                    let val = args.first().and_then(|e| self.eval_expr(e).ok());
                    let x = val
                        .and_then(|v| Self::to_float_value(&v))
                        .unwrap_or(f64::NAN);
                    if args.len() > 1 {
                        let base = args
                            .get(1)
                            .and_then(|e| self.eval_expr(e).ok())
                            .and_then(|v| Self::to_float_value(&v))
                            .unwrap_or(f64::NAN);
                        if base.is_finite() && base > 0.0 && base != 1.0 && x > 0.0 {
                            let result = x.ln() / base.ln();
                            return Ok(Value::Num(result));
                        }
                        return Ok(Value::Num(f64::NAN));
                    }
                    return Ok(Value::Num(x.ln()));
                }
                if name == "sin"
                    || name == "cos"
                    || name == "tan"
                    || name == "asin"
                    || name == "acos"
                    || name == "atan"
                {
                    let val = args.first().and_then(|e| self.eval_expr(e).ok());
                    let x = val.and_then(|v| Self::to_float_value(&v)).unwrap_or(0.0);
                    let result = match name.as_str() {
                        "sin" => x.sin(),
                        "cos" => x.cos(),
                        "tan" => x.tan(),
                        "asin" => x.asin(),
                        "acos" => x.acos(),
                        "atan" => x.atan(),
                        _ => 0.0,
                    };
                    return Ok(Value::Num(result));
                }
                if name == "atan2" {
                    let y = args
                        .first()
                        .and_then(|e| self.eval_expr(e).ok())
                        .and_then(|v| Self::to_float_value(&v))
                        .unwrap_or(0.0);
                    let x = args
                        .get(1)
                        .and_then(|e| self.eval_expr(e).ok())
                        .and_then(|v| Self::to_float_value(&v))
                        .unwrap_or(0.0);
                    return Ok(Value::Num(y.atan2(x)));
                }
                if name == "truncate" {
                    let val = args.first().and_then(|e| self.eval_expr(e).ok());
                    if let Some(num) = val.as_ref().and_then(Self::to_float_value) {
                        if num.is_nan() || num.is_infinite() {
                            return Ok(Value::Num(num));
                        }
                        return Ok(Value::Int(num.trunc() as i64));
                    }
                    if let Some(v) = val {
                        return Ok(Value::Int(Self::to_int(&v)));
                    }
                    return Ok(Value::Int(0));
                }
                if name == "exp" {
                    let val = args.first().and_then(|e| self.eval_expr(e).ok());
                    return Ok(match val {
                        Some(Value::Int(i)) => Value::Num((i as f64).exp()),
                        Some(Value::Num(f)) => Value::Num(f.exp()),
                        _ => Value::Num(f64::NAN),
                    });
                }
                if name == "min" {
                    let mut vals = Vec::new();
                    for arg in args {
                        vals.push(self.eval_expr(arg)?);
                    }
                    return Ok(vals
                        .into_iter()
                        .min_by(|a, b| match (a, b) {
                            (Value::Int(x), Value::Int(y)) => x.cmp(y),
                            _ => a.to_string_value().cmp(&b.to_string_value()),
                        })
                        .unwrap_or(Value::Nil));
                }
                if name == "max" {
                    let mut vals = Vec::new();
                    for arg in args {
                        vals.push(self.eval_expr(arg)?);
                    }
                    return Ok(vals
                        .into_iter()
                        .max_by(|a, b| match (a, b) {
                            (Value::Int(x), Value::Int(y)) => x.cmp(y),
                            _ => a.to_string_value().cmp(&b.to_string_value()),
                        })
                        .unwrap_or(Value::Nil));
                }
                if name == "exit" {
                    let code = args.first().and_then(|e| self.eval_expr(e).ok());
                    let _code = match code {
                        Some(Value::Int(i)) => i,
                        _ => 0,
                    };
                    self.halted = true;
                    return Ok(Value::Nil);
                }
                if name == "chr" {
                    if let Some(Value::Int(i)) = args.first().and_then(|e| self.eval_expr(e).ok())
                        && i >= 0
                        && let Some(ch) = std::char::from_u32(i as u32)
                    {
                        return Ok(Value::Str(ch.to_string()));
                    }
                    return Ok(Value::Str(String::new()));
                }
                if name == "ord" {
                    if let Some(val) = args.first().and_then(|e| self.eval_expr(e).ok())
                        && let Some(ch) = val.to_string_value().chars().next()
                    {
                        return Ok(Value::Int(ch as u32 as i64));
                    }
                    return Ok(Value::Nil);
                }
                if name == "chrs" {
                    let mut result = String::new();
                    for arg in args {
                        let val = self.eval_expr(arg)?;
                        for item in Self::value_to_list(&val) {
                            if let Value::Int(i) = item
                                && i >= 0
                                && (i as u64) <= 0x10ffff
                                && let Some(ch) = std::char::from_u32(i as u32)
                            {
                                result.push(ch);
                                continue;
                            }
                            result.push_str(&item.to_string_value());
                        }
                    }
                    return Ok(Value::Str(result));
                }
                if name == "ords" {
                    if let Some(val) = args.first().and_then(|e| self.eval_expr(e).ok()) {
                        let mut codes = Vec::new();
                        for ch in val.to_string_value().chars() {
                            codes.push(Value::Int(ch as u32 as i64));
                        }
                        return Ok(Value::Array(codes));
                    }
                    return Ok(Value::Array(Vec::new()));
                }
                if name == "flip" {
                    let val = args
                        .first()
                        .and_then(|e| self.eval_expr(e).ok())
                        .map(|v| v.to_string_value())
                        .unwrap_or_default();
                    return Ok(Value::Str(val.chars().rev().collect()));
                }
                if name == "lc" {
                    let val = args
                        .first()
                        .and_then(|e| self.eval_expr(e).ok())
                        .unwrap_or(Value::Nil);
                    return Ok(Value::Str(val.to_string_value().to_lowercase()));
                }
                if name == "uc" {
                    let val = args
                        .first()
                        .and_then(|e| self.eval_expr(e).ok())
                        .unwrap_or(Value::Nil);
                    return Ok(Value::Str(val.to_string_value().to_uppercase()));
                }
                if name == "tc" {
                    let val = args
                        .first()
                        .and_then(|e| self.eval_expr(e).ok())
                        .map(|v| v.to_string_value())
                        .unwrap_or_default();
                    let mut result = String::new();
                    let mut capitalize = true;
                    for ch in val.chars() {
                        if capitalize {
                            for c in ch.to_uppercase() {
                                result.push(c);
                            }
                            capitalize = false;
                        } else {
                            result.push(ch);
                        }
                    }
                    return Ok(Value::Str(result));
                }
                if name == "wordcase" {
                    let val = args
                        .first()
                        .and_then(|e| self.eval_expr(e).ok())
                        .map(|v| v.to_string_value())
                        .unwrap_or_default();
                    return Ok(Value::Str(crate::value::wordcase_str(&val)));
                }
                if name == "chomp" {
                    let val = args
                        .first()
                        .and_then(|e| self.eval_expr(e).ok())
                        .map(|v| v.to_string_value())
                        .unwrap_or_default();
                    return Ok(Value::Str(val.trim_end_matches('\n').to_string()));
                }
                if name == "chop" {
                    let first_val = args.first().map(|e| self.eval_expr(e)).transpose()?;
                    // Type objects (Package) should throw
                    if let Some(Value::Package(ref type_name)) = first_val {
                        return Err(RuntimeError::new(format!(
                            "Cannot resolve caller chop({}:U)",
                            type_name,
                        )));
                    }
                    let val = first_val.map(|v| v.to_string_value()).unwrap_or_default();
                    let n = if args.len() > 1 {
                        let n_val = self.eval_expr(&args[1])?;
                        match n_val {
                            Value::Int(i) => i.max(0) as usize,
                            Value::Num(f) => (f as i64).max(0) as usize,
                            _ => n_val.to_string_value().parse::<usize>().unwrap_or(1),
                        }
                    } else {
                        1
                    };
                    let char_count = val.chars().count();
                    let keep = char_count.saturating_sub(n);
                    let result: String = val.chars().take(keep).collect();
                    return Ok(Value::Str(result));
                }
                if name == "trim" {
                    let val = args
                        .first()
                        .and_then(|e| self.eval_expr(e).ok())
                        .map(|v| v.to_string_value())
                        .unwrap_or_default();
                    return Ok(Value::Str(val.trim().to_string()));
                }
                if name == "trim-leading" {
                    let val = args
                        .first()
                        .and_then(|e| self.eval_expr(e).ok())
                        .map(|v| v.to_string_value())
                        .unwrap_or_default();
                    return Ok(Value::Str(val.trim_start().to_string()));
                }
                if name == "trim-trailing" {
                    let val = args
                        .first()
                        .and_then(|e| self.eval_expr(e).ok())
                        .map(|v| v.to_string_value())
                        .unwrap_or_default();
                    return Ok(Value::Str(val.trim_end().to_string()));
                }
                if name == "words" {
                    let val = args
                        .first()
                        .and_then(|e| self.eval_expr(e).ok())
                        .map(|v| v.to_string_value())
                        .unwrap_or_default();
                    let parts: Vec<Value> = val
                        .split_whitespace()
                        .map(|p| Value::Str(p.to_string()))
                        .collect();
                    return Ok(Value::Array(parts));
                }
                if name == "substr" {
                    let s = args
                        .first()
                        .and_then(|e| self.eval_expr(e).ok())
                        .map(|v| v.to_string_value())
                        .unwrap_or_default();
                    let start = args
                        .get(1)
                        .and_then(|e| self.eval_expr(e).ok())
                        .and_then(|v| match v {
                            Value::Int(i) => Some(i),
                            _ => None,
                        })
                        .unwrap_or(0);
                    let chars: Vec<char> = s.chars().collect();
                    let start = start.max(0) as usize;
                    if let Some(len_val) = args.get(2).and_then(|e| self.eval_expr(e).ok()) {
                        let len = match len_val {
                            Value::Int(i) => i.max(0) as usize,
                            _ => chars.len(),
                        };
                        let end = (start + len).min(chars.len());
                        return Ok(Value::Str(chars[start..end].iter().collect()));
                    }
                    return Ok(Value::Str(chars[start.min(chars.len())..].iter().collect()));
                }
                if name == "index" {
                    let s = args
                        .first()
                        .and_then(|e| self.eval_expr(e).ok())
                        .map(|v| v.to_string_value())
                        .unwrap_or_default();
                    let needle = args
                        .get(1)
                        .and_then(|e| self.eval_expr(e).ok())
                        .map(|v| v.to_string_value())
                        .unwrap_or_default();
                    return Ok(match s.find(&needle) {
                        Some(pos) => Value::Int(s[..pos].chars().count() as i64),
                        None => Value::Nil,
                    });
                }
                if name == "gist" {
                    let val = args
                        .first()
                        .and_then(|e| self.eval_expr(e).ok())
                        .unwrap_or(Value::Nil);
                    return Ok(Value::Str(val.to_string_value()));
                }
                if name == "dd" {
                    // Debug dump
                    let val = args
                        .first()
                        .and_then(|e| self.eval_expr(e).ok())
                        .unwrap_or(Value::Nil);
                    self.output.push_str(&format!("{:?}\n", val));
                    return Ok(val);
                }
                if name == "pair" {
                    let key = args
                        .first()
                        .and_then(|e| self.eval_expr(e).ok())
                        .map(|v| v.to_string_value())
                        .unwrap_or_default();
                    let val = args
                        .get(1)
                        .and_then(|e| self.eval_expr(e).ok())
                        .unwrap_or(Value::Nil);
                    return Ok(Value::Pair(key, Box::new(val)));
                }
                if name == "slurp" {
                    let path = args
                        .first()
                        .and_then(|e| self.eval_expr(e).ok())
                        .map(|v| v.to_string_value())
                        .ok_or_else(|| RuntimeError::new("slurp requires a path argument"))?;
                    let content = fs::read_to_string(&path).map_err(|err| {
                        RuntimeError::new(format!("Failed to slurp '{}': {}", path, err))
                    })?;
                    return Ok(Value::Str(content));
                }
                if name == "spurt" {
                    let path = args
                        .first()
                        .and_then(|e| self.eval_expr(e).ok())
                        .map(|v| v.to_string_value())
                        .ok_or_else(|| RuntimeError::new("spurt requires a path argument"))?;
                    let content = args
                        .get(1)
                        .and_then(|e| self.eval_expr(e).ok())
                        .map(|v| v.to_string_value())
                        .ok_or_else(|| RuntimeError::new("spurt requires a content argument"))?;
                    fs::write(&path, &content).map_err(|err| {
                        RuntimeError::new(format!("Failed to spurt '{}': {}", path, err))
                    })?;
                    return Ok(Value::Bool(true));
                }
                if name == "unlink" {
                    let path = args
                        .first()
                        .and_then(|e| self.eval_expr(e).ok())
                        .map(|v| v.to_string_value())
                        .ok_or_else(|| RuntimeError::new("unlink requires a path argument"))?;
                    fs::remove_file(&path).map_err(|err| {
                        RuntimeError::new(format!("Failed to unlink '{}': {}", path, err))
                    })?;
                    return Ok(Value::Bool(true));
                }
                if name == "print" {
                    let mut content = String::new();
                    for arg in args {
                        let value = self.eval_expr(arg)?;
                        content.push_str(&value.to_string_value());
                    }
                    self.write_to_named_handle("$*OUT", &content, false)?;
                    return Ok(Value::Nil);
                }
                if name == "say" {
                    let mut content = String::new();
                    for arg in args {
                        let value = self.eval_expr(arg)?;
                        content.push_str(&value.to_string_value());
                    }
                    self.write_to_named_handle("$*OUT", &content, true)?;
                    return Ok(Value::Nil);
                }
                if name == "note" {
                    let mut content = String::new();
                    for arg in args {
                        let value = self.eval_expr(arg)?;
                        content.push_str(&value.to_string_value());
                    }
                    self.write_to_named_handle("$*ERR", &content, true)?;
                    return Ok(Value::Nil);
                }
                if name == "prompt" {
                    let msg = args
                        .first()
                        .and_then(|e| self.eval_expr(e).ok())
                        .map(|v| v.to_string_value())
                        .unwrap_or_default();
                    self.write_to_named_handle("$*OUT", &msg, false)?;
                    if let Some(handle) = self.default_input_handle() {
                        let line = self.read_line_from_handle_value(&handle)?;
                        return Ok(Value::Str(line));
                    }
                    return Ok(Value::Str(String::new()));
                }
                if name == "get" {
                    let handle = args
                        .first()
                        .and_then(|e| self.eval_expr(e).ok())
                        .or_else(|| self.default_input_handle());
                    if let Some(handle) = handle {
                        let line = self.read_line_from_handle_value(&handle)?;
                        return Ok(Value::Str(line));
                    }
                    return Ok(Value::Str(String::new()));
                }
                if name == "lines" {
                    let handle = args
                        .first()
                        .and_then(|e| self.eval_expr(e).ok())
                        .or_else(|| self.default_input_handle());
                    if let Some(handle) = handle {
                        let mut lines = Vec::new();
                        loop {
                            let line = self.read_line_from_handle_value(&handle)?;
                            if line.is_empty() {
                                break;
                            }
                            lines.push(Value::Str(line));
                        }
                        return Ok(Value::Array(lines));
                    }
                    return Ok(Value::Array(Vec::new()));
                }
                if name == "words" {
                    let handle = args
                        .first()
                        .and_then(|e| self.eval_expr(e).ok())
                        .or_else(|| self.default_input_handle());
                    if let Some(handle) = handle {
                        let mut words = Vec::new();
                        loop {
                            let line = self.read_line_from_handle_value(&handle)?;
                            if line.is_empty() {
                                break;
                            }
                            for token in line.split_whitespace() {
                                words.push(Value::Str(token.to_string()));
                            }
                        }
                        return Ok(Value::Array(words));
                    }
                    return Ok(Value::Array(Vec::new()));
                }
                if name == "open" {
                    let path_expr = args
                        .first()
                        .ok_or_else(|| RuntimeError::new("open requires a path argument"))?;
                    let path_str = self.eval_expr(path_expr)?.to_string_value();
                    let (read, write, append) = self.parse_io_flags(&args[1..])?;
                    let path_buf = self.resolve_path(&path_str);
                    return self.open_file_handle(&path_buf, read, write, append);
                }
                if name == "close" {
                    let handle_expr = args
                        .first()
                        .ok_or_else(|| RuntimeError::new("close requires a handle"))?;
                    let handle_val = self.eval_expr(handle_expr)?;
                    return Ok(Value::Bool(self.close_handle_value(&handle_val)?));
                }
                if name == "dir" {
                    let requested = args
                        .first()
                        .and_then(|e| self.eval_expr(e).ok())
                        .map(|v| v.to_string_value())
                        .unwrap_or_else(|| {
                            self.get_dynamic_string("$*CWD")
                                .unwrap_or_else(|| ".".to_string())
                        });
                    let path_buf = self.resolve_path(&requested);
                    let mut entries = Vec::new();
                    for entry in fs::read_dir(&path_buf).map_err(|err| {
                        RuntimeError::new(format!("Failed to read dir '{}': {}", requested, err))
                    })? {
                        let entry = entry.map_err(|err| {
                            RuntimeError::new(format!(
                                "Failed to read dir entry '{}': {}",
                                requested, err
                            ))
                        })?;
                        entries.push(Value::Str(entry.path().to_string_lossy().to_string()));
                    }
                    return Ok(Value::Array(entries));
                }
                if name == "copy" {
                    let source = args
                        .first()
                        .and_then(|e| self.eval_expr(e).ok())
                        .map(|v| v.to_string_value())
                        .ok_or_else(|| RuntimeError::new("copy requires a source path"))?;
                    let dest = args
                        .get(1)
                        .and_then(|e| self.eval_expr(e).ok())
                        .map(|v| v.to_string_value())
                        .ok_or_else(|| RuntimeError::new("copy requires a destination path"))?;
                    let src_buf = self.resolve_path(&source);
                    let dest_buf = self.resolve_path(&dest);
                    fs::copy(&src_buf, &dest_buf).map_err(|err| {
                        RuntimeError::new(format!("Failed to copy '{}': {}", source, err))
                    })?;
                    return Ok(Value::Bool(true));
                }
                if name == "rename" || name == "move" {
                    let source = args
                        .first()
                        .and_then(|e| self.eval_expr(e).ok())
                        .map(|v| v.to_string_value())
                        .ok_or_else(|| RuntimeError::new("rename requires a source path"))?;
                    let dest = args
                        .get(1)
                        .and_then(|e| self.eval_expr(e).ok())
                        .map(|v| v.to_string_value())
                        .ok_or_else(|| RuntimeError::new("rename requires a destination path"))?;
                    let src_buf = self.resolve_path(&source);
                    let dest_buf = self.resolve_path(&dest);
                    fs::rename(&src_buf, &dest_buf).map_err(|err| {
                        RuntimeError::new(format!("Failed to rename '{}': {}", source, err))
                    })?;
                    return Ok(Value::Bool(true));
                }
                if name == "chmod" {
                    let path = args
                        .first()
                        .and_then(|e| self.eval_expr(e).ok())
                        .map(|v| v.to_string_value())
                        .ok_or_else(|| RuntimeError::new("chmod requires a path"))?;
                    let mode_arg = args
                        .get(1)
                        .ok_or_else(|| RuntimeError::new("chmod requires a mode"))?;
                    let mode_value = self.eval_expr(mode_arg)?;
                    let mode_int = match mode_value {
                        Value::Int(i) => i as u32,
                        Value::Str(s) => u32::from_str_radix(&s, 8).unwrap_or(0),
                        other => {
                            return Err(RuntimeError::new(format!(
                                "Invalid mode: {}",
                                other.to_string_value()
                            )));
                        }
                    };
                    let path_buf = self.resolve_path(&path);
                    #[cfg(unix)]
                    {
                        let perms = PermissionsExt::from_mode(mode_int);
                        fs::set_permissions(&path_buf, perms).map_err(|err| {
                            RuntimeError::new(format!("Failed to chmod '{}': {}", path, err))
                        })?;
                    }
                    #[cfg(not(unix))]
                    {
                        return Err(RuntimeError::new("chmod not supported on this platform"));
                    }
                    return Ok(Value::Bool(true));
                }
                if name == "mkdir" {
                    let path = args
                        .first()
                        .and_then(|e| self.eval_expr(e).ok())
                        .map(|v| v.to_string_value())
                        .unwrap_or_else(|| {
                            self.get_dynamic_string("$*CWD")
                                .unwrap_or_else(|| ".".to_string())
                        });
                    let path_buf = self.resolve_path(&path);
                    fs::create_dir_all(&path_buf).map_err(|err| {
                        RuntimeError::new(format!("Failed to mkdir '{}': {}", path, err))
                    })?;
                    return Ok(Value::Bool(true));
                }
                if name == "rmdir" {
                    let path = args
                        .first()
                        .and_then(|e| self.eval_expr(e).ok())
                        .map(|v| v.to_string_value())
                        .ok_or_else(|| RuntimeError::new("rmdir requires a path"))?;
                    let path_buf = self.resolve_path(&path);
                    fs::remove_dir(&path_buf).map_err(|err| {
                        RuntimeError::new(format!("Failed to rmdir '{}': {}", path, err))
                    })?;
                    return Ok(Value::Bool(true));
                }
                if name == "chdir" {
                    let path = args
                        .first()
                        .and_then(|e| self.eval_expr(e).ok())
                        .map(|v| v.to_string_value())
                        .ok_or_else(|| RuntimeError::new("chdir requires a path"))?;
                    let path_buf = self.resolve_path(&path);
                    if !path_buf.is_dir() {
                        return Err(RuntimeError::new(format!(
                            "chdir path is not a directory: {}",
                            path
                        )));
                    }
                    self.env.insert(
                        "$*CWD".to_string(),
                        Value::Str(Self::stringify_path(&path_buf)),
                    );
                    return Ok(Value::Bool(true));
                }
                if name == "indir" {
                    let path = args
                        .first()
                        .and_then(|e| self.eval_expr(e).ok())
                        .map(|v| v.to_string_value())
                        .ok_or_else(|| RuntimeError::new("indir requires a path"))?;
                    let path_buf = self.resolve_path(&path);
                    if !path_buf.is_dir() {
                        return Err(RuntimeError::new(format!(
                            "indir path is not a directory: {}",
                            path
                        )));
                    }
                    let saved = self.env.get("$*CWD").cloned();
                    self.env.insert(
                        "$*CWD".to_string(),
                        Value::Str(Self::stringify_path(&path_buf)),
                    );
                    let result = if let Some(body) = args.get(1) {
                        match body {
                            Expr::Block(body_stmts)
                            | Expr::AnonSub(body_stmts)
                            | Expr::AnonSubParams {
                                body: body_stmts, ..
                            } => self.eval_block_value(body_stmts),
                            other => self.eval_expr(other),
                        }
                    } else {
                        Ok(Value::Nil)
                    };
                    if let Some(prev) = saved {
                        self.env.insert("$*CWD".to_string(), prev);
                    } else {
                        self.env.remove("$*CWD");
                    }
                    return result;
                }
                if name == "tmpdir" {
                    if let Some(path_expr) = args.first() {
                        let path = self.eval_expr(path_expr)?.to_string_value();
                        let path_buf = self.resolve_path(&path);
                        if !path_buf.is_dir() {
                            return Err(RuntimeError::new("tmpdir path must be a directory"));
                        }
                        let repr = Self::stringify_path(&path_buf);
                        self.env
                            .insert("$*TMPDIR".to_string(), Value::Str(repr.clone()));
                        return Ok(Value::Str(repr));
                    }
                    return Ok(Value::Str(
                        self.get_dynamic_string("$*TMPDIR").unwrap_or_default(),
                    ));
                }
                if name == "homedir" {
                    if let Some(path_expr) = args.first() {
                        let path = self.eval_expr(path_expr)?.to_string_value();
                        let path_buf = self.resolve_path(&path);
                        if !path_buf.is_dir() {
                            return Err(RuntimeError::new("homedir path must be a directory"));
                        }
                        let repr = Self::stringify_path(&path_buf);
                        self.env
                            .insert("$*HOME".to_string(), Value::Str(repr.clone()));
                        return Ok(Value::Str(repr));
                    }
                    return Ok(Value::Str(
                        self.get_dynamic_string("$*HOME").unwrap_or_default(),
                    ));
                }
                if name == "link" {
                    let target = args
                        .first()
                        .and_then(|e| self.eval_expr(e).ok())
                        .map(|v| v.to_string_value())
                        .ok_or_else(|| RuntimeError::new("link requires a target"))?;
                    let link = args
                        .get(1)
                        .and_then(|e| self.eval_expr(e).ok())
                        .map(|v| v.to_string_value())
                        .ok_or_else(|| RuntimeError::new("link requires a link name"))?;
                    let target_buf = self.resolve_path(&target);
                    let link_buf = self.resolve_path(&link);
                    fs::hard_link(&target_buf, &link_buf).map_err(|err| {
                        RuntimeError::new(format!("Failed to create link '{}': {}", target, err))
                    })?;
                    return Ok(Value::Bool(true));
                }
                if name == "symlink" {
                    let target = args
                        .first()
                        .and_then(|e| self.eval_expr(e).ok())
                        .map(|v| v.to_string_value())
                        .ok_or_else(|| RuntimeError::new("symlink requires a target"))?;
                    let link = args
                        .get(1)
                        .and_then(|e| self.eval_expr(e).ok())
                        .map(|v| v.to_string_value())
                        .ok_or_else(|| RuntimeError::new("symlink requires a link name"))?;
                    let target_buf = self.resolve_path(&target);
                    let link_buf = self.resolve_path(&link);
                    #[cfg(unix)]
                    {
                        unix_fs::symlink(&target_buf, &link_buf).map_err(|err| {
                            RuntimeError::new(format!("Failed to symlink '{}': {}", target, err))
                        })?;
                    }
                    #[cfg(windows)]
                    {
                        let metadata = fs::metadata(&target_buf);
                        if metadata.map(|meta| meta.is_dir()).unwrap_or(false) {
                            windows_fs::symlink_dir(&target_buf, &link_buf).map_err(|err| {
                                RuntimeError::new(format!(
                                    "Failed to symlink '{}': {}",
                                    target, err
                                ))
                            })?;
                        } else {
                            windows_fs::symlink_file(&target_buf, &link_buf).map_err(|err| {
                                RuntimeError::new(format!(
                                    "Failed to symlink '{}': {}",
                                    target, err
                                ))
                            })?;
                        }
                    }
                    return Ok(Value::Bool(true));
                }
                Ok(Value::Nil)
            }
            Expr::Try { body, catch } => self.eval_try_expr(body, catch),
            Expr::Gather(body) => Ok(self.eval_gather_expr(body)),
            Expr::Reduction { op, expr } => {
                let list_value = self.eval_expr(expr)?;
                let list = self.list_from_value(list_value)?;
                if list.is_empty() {
                    return Ok(Self::reduction_identity(op));
                }
                let mut acc = list[0].clone();
                for item in &list[1..] {
                    acc = Self::apply_reduction_op(op, &acc, item)?;
                }
                Ok(acc)
            }
            Expr::HyperOp {
                op,
                left,
                right,
                dwim_left,
                dwim_right,
            } => {
                let left_val = self.eval_expr(left)?;
                let right_val = self.eval_expr(right)?;
                Self::eval_hyper_op(op, &left_val, &right_val, *dwim_left, *dwim_right)
            }
            Expr::MetaOp {
                meta,
                op,
                left,
                right,
            } => {
                let left_val = self.eval_expr(left)?;
                let right_val = self.eval_expr(right)?;
                match meta.as_str() {
                    "R" => Self::apply_reduction_op(op, &right_val, &left_val),
                    "X" => {
                        let left_list = Self::value_to_list(&left_val);
                        let right_list = Self::value_to_list(&right_val);
                        let mut results = Vec::new();
                        for l in &left_list {
                            for r in &right_list {
                                results.push(Self::apply_reduction_op(op, l, r)?);
                            }
                        }
                        Ok(Value::Array(results))
                    }
                    "Z" => {
                        let left_list = Self::value_to_list(&left_val);
                        let right_list = Self::value_to_list(&right_val);
                        let len = left_list.len().min(right_list.len());
                        let mut results = Vec::new();
                        if op.is_empty() || op == "," {
                            // Non-meta Z or Z, : pair elements into sub-lists
                            for i in 0..len {
                                results.push(Value::Array(vec![
                                    left_list[i].clone(),
                                    right_list[i].clone(),
                                ]));
                            }
                        } else if op == "=>" {
                            // Z=> : create pairs
                            for i in 0..len {
                                let key = left_list[i].to_string_value();
                                results.push(Value::Pair(key, Box::new(right_list[i].clone())));
                            }
                        } else {
                            for i in 0..len {
                                results.push(Self::apply_reduction_op(
                                    op,
                                    &left_list[i],
                                    &right_list[i],
                                )?);
                            }
                        }
                        Ok(Value::Array(results))
                    }
                    _ => Err(RuntimeError::new(format!(
                        "Unknown meta operator: {}",
                        meta
                    ))),
                }
            }
            Expr::InfixFunc {
                name,
                left,
                right,
                modifier,
            } => {
                let left_val = self.eval_expr(left)?;
                let mut right_vals = Vec::new();
                for expr in right {
                    right_vals.push(self.eval_expr(expr)?);
                }
                if name == "atan2" {
                    let mut x = right_vals
                        .first()
                        .and_then(Self::to_float_value)
                        .unwrap_or(0.0);
                    let mut y = Self::to_float_value(&left_val).unwrap_or(0.0);
                    if modifier.as_deref() == Some("R") {
                        std::mem::swap(&mut x, &mut y);
                    }
                    return Ok(Value::Num(y.atan2(x)));
                }
                if name == "sprintf" {
                    let fmt = match left_val {
                        Value::Str(s) => s,
                        _ => String::new(),
                    };
                    if modifier.as_deref() == Some("X") {
                        let mut parts = Vec::new();
                        for val in right_vals {
                            parts.push(Self::format_sprintf(&fmt, Some(&val)));
                        }
                        return Ok(Value::Str(parts.join(" ")));
                    }
                    let arg = right_vals.first();
                    let rendered = Self::format_sprintf(&fmt, arg);
                    return Ok(Value::Str(rendered));
                }
                Ok(Value::Nil)
            }
            Expr::Ternary {
                cond,
                then_expr,
                else_expr,
            } => {
                if self.eval_expr(cond)?.truthy() {
                    self.eval_expr(then_expr)
                } else {
                    self.eval_expr(else_expr)
                }
            }
        }
    }

    pub(crate) fn eval_binary(
        &mut self,
        left: Value,
        op: &TokenKind,
        right: Value,
    ) -> Result<Value, RuntimeError> {
        // Junction auto-threading for comparison operators
        if let Value::Junction { kind, values } = &left
            && Self::is_threadable_op(op)
        {
            let results: Result<Vec<Value>, RuntimeError> = values
                .iter()
                .map(|v| self.eval_binary(v.clone(), op, right.clone()))
                .collect();
            return Ok(Value::Junction {
                kind: kind.clone(),
                values: results?,
            });
        }
        if let Value::Junction { kind, values } = &right
            && Self::is_threadable_op(op)
        {
            let results: Result<Vec<Value>, RuntimeError> = values
                .iter()
                .map(|v| self.eval_binary(left.clone(), op, v.clone()))
                .collect();
            return Ok(Value::Junction {
                kind: kind.clone(),
                values: results?,
            });
        }
        match op {
            TokenKind::Pipe => Ok(Self::merge_junction(JunctionKind::Any, left, right)),
            TokenKind::Ampersand => Ok(Self::merge_junction(JunctionKind::All, left, right)),
            TokenKind::Caret => Ok(Self::merge_junction(JunctionKind::One, left, right)),
            TokenKind::Plus => {
                // Range + Int: shift both bounds
                match (&left, &right) {
                    (Value::Range(a, b), Value::Int(n)) => return Ok(Value::Range(a + n, b + n)),
                    (Value::RangeExcl(a, b), Value::Int(n)) => {
                        return Ok(Value::RangeExcl(a + n, b + n));
                    }
                    (Value::RangeExclStart(a, b), Value::Int(n)) => {
                        return Ok(Value::RangeExclStart(a + n, b + n));
                    }
                    (Value::RangeExclBoth(a, b), Value::Int(n)) => {
                        return Ok(Value::RangeExclBoth(a + n, b + n));
                    }
                    (Value::Int(n), Value::Range(a, b)) => return Ok(Value::Range(a + n, b + n)),
                    (Value::Int(n), Value::RangeExcl(a, b)) => {
                        return Ok(Value::RangeExcl(a + n, b + n));
                    }
                    (Value::Int(n), Value::RangeExclStart(a, b)) => {
                        return Ok(Value::RangeExclStart(a + n, b + n));
                    }
                    (Value::Int(n), Value::RangeExclBoth(a, b)) => {
                        return Ok(Value::RangeExclBoth(a + n, b + n));
                    }
                    _ => {}
                }
                let (l, r) = Self::coerce_numeric(left, right);
                if matches!(l, Value::Complex(_, _)) || matches!(r, Value::Complex(_, _)) {
                    let (ar, ai) = Self::to_complex_parts(&l).unwrap_or((0.0, 0.0));
                    let (br, bi) = Self::to_complex_parts(&r).unwrap_or((0.0, 0.0));
                    return Ok(Value::Complex(ar + br, ai + bi));
                }
                if let (Some((an, ad)), Some((bn, bd))) =
                    (Self::to_rat_parts(&l), Self::to_rat_parts(&r))
                    && (matches!(l, Value::Rat(_, _)) || matches!(r, Value::Rat(_, _)))
                {
                    return Ok(make_rat(an * bd + bn * ad, ad * bd));
                }
                match (l, r) {
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a.wrapping_add(b))),
                    (Value::Num(a), Value::Num(b)) => Ok(Value::Num(a + b)),
                    (Value::Int(a), Value::Num(b)) => Ok(Value::Num(a as f64 + b)),
                    (Value::Num(a), Value::Int(b)) => Ok(Value::Num(a + b as f64)),
                    _ => Ok(Value::Int(0)),
                }
            }
            TokenKind::Minus => {
                let (l, r) = Self::coerce_numeric(left, right);
                if matches!(l, Value::Complex(_, _)) || matches!(r, Value::Complex(_, _)) {
                    let (ar, ai) = Self::to_complex_parts(&l).unwrap_or((0.0, 0.0));
                    let (br, bi) = Self::to_complex_parts(&r).unwrap_or((0.0, 0.0));
                    return Ok(Value::Complex(ar - br, ai - bi));
                }
                if let (Some((an, ad)), Some((bn, bd))) =
                    (Self::to_rat_parts(&l), Self::to_rat_parts(&r))
                    && (matches!(l, Value::Rat(_, _)) || matches!(r, Value::Rat(_, _)))
                {
                    return Ok(make_rat(an * bd - bn * ad, ad * bd));
                }
                match (l, r) {
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a.wrapping_sub(b))),
                    (Value::Num(a), Value::Num(b)) => Ok(Value::Num(a - b)),
                    (Value::Int(a), Value::Num(b)) => Ok(Value::Num(a as f64 - b)),
                    (Value::Num(a), Value::Int(b)) => Ok(Value::Num(a - b as f64)),
                    _ => Ok(Value::Int(0)),
                }
            }
            TokenKind::Star => {
                let (l, r) = Self::coerce_numeric(left, right);
                if matches!(l, Value::Complex(_, _)) || matches!(r, Value::Complex(_, _)) {
                    let (ar, ai) = Self::to_complex_parts(&l).unwrap_or((0.0, 0.0));
                    let (br, bi) = Self::to_complex_parts(&r).unwrap_or((0.0, 0.0));
                    // (a+bi)(c+di) = (ac-bd) + (ad+bc)i
                    return Ok(Value::Complex(ar * br - ai * bi, ar * bi + ai * br));
                }
                if let (Some((an, ad)), Some((bn, bd))) =
                    (Self::to_rat_parts(&l), Self::to_rat_parts(&r))
                    && (matches!(l, Value::Rat(_, _)) || matches!(r, Value::Rat(_, _)))
                {
                    return Ok(make_rat(an * bn, ad * bd));
                }
                match (l, r) {
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a.wrapping_mul(b))),
                    (Value::Num(a), Value::Num(b)) => Ok(Value::Num(a * b)),
                    (Value::Int(a), Value::Num(b)) => Ok(Value::Num(a as f64 * b)),
                    (Value::Num(a), Value::Int(b)) => Ok(Value::Num(a * b as f64)),
                    _ => Ok(Value::Int(0)),
                }
            }
            TokenKind::Slash => {
                let (l, r) = Self::coerce_numeric(left, right);
                if matches!(l, Value::Complex(_, _)) || matches!(r, Value::Complex(_, _)) {
                    let (ar, ai) = Self::to_complex_parts(&l).unwrap_or((0.0, 0.0));
                    let (br, bi) = Self::to_complex_parts(&r).unwrap_or((0.0, 0.0));
                    // (a+bi)/(c+di) = ((ac+bd) + (bc-ad)i) / (c²+d²)
                    let denom = br * br + bi * bi;
                    if denom == 0.0 {
                        return Err(RuntimeError::new("Division by zero"));
                    }
                    return Ok(Value::Complex(
                        (ar * br + ai * bi) / denom,
                        (ai * br - ar * bi) / denom,
                    ));
                }
                match (&l, &r) {
                    (Value::Rat(_, _), _)
                    | (_, Value::Rat(_, _))
                    | (Value::Int(_), Value::Int(_)) => {
                        let (an, ad) = Self::to_rat_parts(&l).unwrap_or((0, 1));
                        let (bn, bd) = Self::to_rat_parts(&r).unwrap_or((0, 1));
                        if bn == 0 {
                            return Err(RuntimeError::new("Division by zero"));
                        }
                        Ok(make_rat(an * bd, ad * bn))
                    }
                    (Value::Num(a), Value::Num(b)) => Ok(Value::Num(a / b)),
                    (Value::Int(a), Value::Num(b)) => Ok(Value::Num(*a as f64 / b)),
                    (Value::Num(a), Value::Int(b)) => Ok(Value::Num(a / *b as f64)),
                    _ => Ok(Value::Int(0)),
                }
            }
            TokenKind::Percent => {
                let (l, r) = Self::coerce_numeric(left, right);
                if let (Some((an, ad)), Some((bn, bd))) =
                    (Self::to_rat_parts(&l), Self::to_rat_parts(&r))
                    && (matches!(l, Value::Rat(_, _)) || matches!(r, Value::Rat(_, _)))
                {
                    if bn == 0 {
                        return Err(RuntimeError::new("Modulo by zero"));
                    }
                    // (an/ad) % (bn/bd) = remainder
                    let lf = an as f64 / ad as f64;
                    let rf = bn as f64 / bd as f64;
                    let result = lf % rf;
                    return Ok(Value::Num(result));
                }
                match (l, r) {
                    (Value::Int(_), Value::Int(0)) => Err(RuntimeError::new("Modulo by zero")),
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a % b)),
                    (Value::Num(a), Value::Num(b)) => Ok(Value::Num(a % b)),
                    (Value::Int(a), Value::Num(b)) => Ok(Value::Num(a as f64 % b)),
                    (Value::Num(a), Value::Int(b)) => Ok(Value::Num(a % b as f64)),
                    _ => Ok(Value::Int(0)),
                }
            }
            TokenKind::PercentPercent => {
                let (l, r) = Self::coerce_numeric(left, right);
                match (l, r) {
                    (Value::Int(_), Value::Int(0)) => {
                        Err(RuntimeError::new("Divisibility by zero"))
                    }
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a % b == 0)),
                    _ => Ok(Value::Bool(false)),
                }
            }
            TokenKind::StarStar => {
                let (l, r) = Self::coerce_numeric(left, right);
                if matches!(l, Value::Complex(_, _)) || matches!(r, Value::Complex(_, _)) {
                    let (ar, ai) = Self::to_complex_parts(&l).unwrap_or((0.0, 0.0));
                    let (br, bi) = Self::to_complex_parts(&r).unwrap_or((0.0, 0.0));
                    // z^w = e^(w * ln(z))
                    let ln_r = (ar * ar + ai * ai).sqrt().ln();
                    let ln_i = ai.atan2(ar);
                    let wr = br * ln_r - bi * ln_i;
                    let wi = br * ln_i + bi * ln_r;
                    let mag = wr.exp();
                    return Ok(Value::Complex(mag * wi.cos(), mag * wi.sin()));
                }
                match (l, r) {
                    (Value::Int(a), Value::Int(b)) if b >= 0 => Ok(Value::Int(a.pow(b as u32))),
                    (Value::Int(a), Value::Int(b)) => {
                        // Negative integer exponent: a ** -n = 1/(a**n) -> Rat
                        let pos = (-b) as u32;
                        let base = a.pow(pos);
                        Ok(make_rat(1, base))
                    }
                    (Value::Rat(n, d), Value::Int(b)) if b >= 0 => {
                        let p = b as u32;
                        Ok(make_rat(n.pow(p), d.pow(p)))
                    }
                    (Value::Rat(n, d), Value::Int(b)) => {
                        let p = (-b) as u32;
                        Ok(make_rat(d.pow(p), n.pow(p)))
                    }
                    (Value::Num(a), Value::Int(b)) => Ok(Value::Num(a.powi(b as i32))),
                    (Value::Int(a), Value::Num(b)) => Ok(Value::Num((a as f64).powf(b))),
                    (Value::Num(a), Value::Num(b)) => Ok(Value::Num(a.powf(b))),
                    _ => Ok(Value::Int(0)),
                }
            }
            TokenKind::BitAnd => {
                let (l, r) = Self::coerce_numeric(left, right);
                match (l, r) {
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a & b)),
                    _ => Ok(Value::Int(0)),
                }
            }
            TokenKind::BitOr => {
                let (l, r) = Self::coerce_numeric(left, right);
                match (l, r) {
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a | b)),
                    _ => Ok(Value::Int(0)),
                }
            }
            TokenKind::BitXor => {
                let (l, r) = Self::coerce_numeric(left, right);
                match (l, r) {
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a ^ b)),
                    _ => Ok(Value::Int(0)),
                }
            }
            TokenKind::BitShiftLeft => {
                let (l, r) = Self::coerce_numeric(left, right);
                match (l, r) {
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a << b)),
                    _ => Ok(Value::Int(0)),
                }
            }
            TokenKind::BitShiftRight => {
                let (l, r) = Self::coerce_numeric(left, right);
                match (l, r) {
                    (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a >> b)),
                    _ => Ok(Value::Int(0)),
                }
            }
            TokenKind::Tilde => Ok(Value::Str(format!(
                "{}{}",
                left.to_string_value(),
                right.to_string_value()
            ))),
            TokenKind::EqEq => {
                // Raku == uses IEEE 754 semantics: NaN == NaN is False
                let is_nan = |v: &Value| matches!(v, Value::Num(f) if f.is_nan());
                if is_nan(&left) || is_nan(&right) {
                    Ok(Value::Bool(false))
                } else {
                    Ok(Value::Bool(left == right))
                }
            }
            TokenKind::EqEqEq => Ok(Value::Bool(left == right)),
            TokenKind::BangEq => Ok(Value::Bool(left != right)),
            TokenKind::Lt => Self::compare(left, right, |o| o < 0),
            TokenKind::Lte => Self::compare(left, right, |o| o <= 0),
            TokenKind::Gt => Self::compare(left, right, |o| o > 0),
            TokenKind::Gte => Self::compare(left, right, |o| o >= 0),
            TokenKind::AndAnd => Ok(Value::Bool(left.truthy() && right.truthy())),
            TokenKind::OrOr => Ok(Value::Bool(left.truthy() || right.truthy())),
            TokenKind::OrWord => {
                if left.truthy() {
                    Ok(left)
                } else {
                    Ok(right)
                }
            }
            TokenKind::SlashSlash | TokenKind::OrElse => {
                if !matches!(left, Value::Nil) {
                    Ok(left)
                } else {
                    Ok(right)
                }
            }
            TokenKind::AndThen => {
                if matches!(left, Value::Nil) {
                    Ok(Value::Nil)
                } else {
                    Ok(right)
                }
            }
            TokenKind::NotAndThen => {
                if matches!(left, Value::Nil) {
                    Ok(right)
                } else {
                    Ok(Value::Nil)
                }
            }
            TokenKind::DotDot => match (&left, &right) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Range(*a, *b)),
                (Value::Str(a), Value::Str(b)) if a.len() == 1 && b.len() == 1 => {
                    let start = a.chars().next().unwrap();
                    let end = b.chars().next().unwrap();
                    let items: Vec<Value> = (start as u32..=end as u32)
                        .filter_map(char::from_u32)
                        .map(|c| Value::Str(c.to_string()))
                        .collect();
                    Ok(Value::Array(items))
                }
                _ => Ok(Value::Nil),
            },
            TokenKind::DotDotDot => self.eval_sequence(left, right, false),
            TokenKind::DotDotDotCaret => self.eval_sequence(left, right, true),
            TokenKind::DotDotCaret => match (left, right) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::RangeExcl(a, b)),
                _ => Ok(Value::Nil),
            },
            TokenKind::CaretDotDot => match (left, right) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::RangeExclStart(a, b)),
                _ => Ok(Value::Nil),
            },
            TokenKind::CaretDotDotCaret => match (left, right) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::RangeExclBoth(a, b)),
                _ => Ok(Value::Nil),
            },
            TokenKind::LtEqGt => {
                let ord = match (&left, &right) {
                    (Value::Int(a), Value::Int(b)) => a.cmp(b),
                    (Value::Rat(_, _), _)
                    | (_, Value::Rat(_, _))
                    | (Value::FatRat(_, _), _)
                    | (_, Value::FatRat(_, _)) => {
                        if let (Some((an, ad)), Some((bn, bd))) =
                            (Self::to_rat_parts(&left), Self::to_rat_parts(&right))
                        {
                            (an * bd).cmp(&(bn * ad))
                        } else {
                            left.to_string_value().cmp(&right.to_string_value())
                        }
                    }
                    (Value::Num(a), Value::Num(b)) => {
                        a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal)
                    }
                    (Value::Int(a), Value::Num(b)) => (*a as f64)
                        .partial_cmp(b)
                        .unwrap_or(std::cmp::Ordering::Equal),
                    (Value::Num(a), Value::Int(b)) => a
                        .partial_cmp(&(*b as f64))
                        .unwrap_or(std::cmp::Ordering::Equal),
                    _ => left.to_string_value().cmp(&right.to_string_value()),
                };
                Ok(Self::make_order(ord))
            }
            TokenKind::SmartMatch => Ok(Value::Bool(self.smart_match(&left, &right))),
            TokenKind::BangTilde => Ok(Value::Bool(!self.smart_match(&left, &right))),
            TokenKind::Ident(name) if name == "div" => match (left, right) {
                (Value::Int(a), Value::Int(b)) if b != 0 => Ok(Value::Int(a.div_euclid(b))),
                (Value::Int(_), Value::Int(_)) => Err(RuntimeError::new("Division by zero")),
                _ => Err(RuntimeError::new("div expects Int")),
            },
            TokenKind::Ident(name) if name == "mod" => match (left, right) {
                (Value::Int(a), Value::Int(b)) if b != 0 => Ok(Value::Int(a.rem_euclid(b))),
                (Value::Int(_), Value::Int(_)) => Err(RuntimeError::new("Modulo by zero")),
                _ => Err(RuntimeError::new("mod expects Int")),
            },
            TokenKind::Ident(name) if name == "gcd" => {
                // Use BigInt arithmetic to handle arbitrary-precision integers
                let a = left.to_bigint().abs();
                let b = right.to_bigint().abs();
                let g = num_integer::Integer::gcd(&a, &b);
                Ok(Value::from_bigint(g))
            }
            TokenKind::Ident(name) if name == "lcm" => {
                let a = left.to_bigint().abs();
                let b = right.to_bigint().abs();
                if a.is_zero() && b.is_zero() {
                    Ok(Value::Int(0))
                } else {
                    let g = num_integer::Integer::gcd(&a, &b);
                    Ok(Value::from_bigint(&a / &g * &b))
                }
            }
            TokenKind::Ident(name) if name == "eq" => Ok(Value::Bool(
                left.to_string_value() == right.to_string_value(),
            )),
            TokenKind::Ident(name) if name == "ne" => Ok(Value::Bool(
                left.to_string_value() != right.to_string_value(),
            )),
            TokenKind::Ident(name) if name == "lt" => Ok(Value::Bool(
                left.to_string_value() < right.to_string_value(),
            )),
            TokenKind::Ident(name) if name == "le" => Ok(Value::Bool(
                left.to_string_value() <= right.to_string_value(),
            )),
            TokenKind::Ident(name) if name == "gt" => Ok(Value::Bool(
                left.to_string_value() > right.to_string_value(),
            )),
            TokenKind::Ident(name) if name == "ge" => Ok(Value::Bool(
                left.to_string_value() >= right.to_string_value(),
            )),
            TokenKind::Ident(name) if name == "leg" => {
                let ord = left.to_string_value().cmp(&right.to_string_value());
                Ok(Self::make_order(ord))
            }
            TokenKind::Ident(name) if name == "cmp" => {
                let ord = match (&left, &right) {
                    (Value::Int(a), Value::Int(b)) => a.cmp(b),
                    (Value::Rat(_, _), _)
                    | (_, Value::Rat(_, _))
                    | (Value::FatRat(_, _), _)
                    | (_, Value::FatRat(_, _)) => {
                        if let (Some((an, ad)), Some((bn, bd))) =
                            (Self::to_rat_parts(&left), Self::to_rat_parts(&right))
                        {
                            (an * bd).cmp(&(bn * ad))
                        } else {
                            left.to_string_value().cmp(&right.to_string_value())
                        }
                    }
                    (Value::Num(a), Value::Num(b)) => {
                        a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal)
                    }
                    (Value::Int(a), Value::Num(b)) => (*a as f64)
                        .partial_cmp(b)
                        .unwrap_or(std::cmp::Ordering::Equal),
                    (Value::Num(a), Value::Int(b)) => a
                        .partial_cmp(&(*b as f64))
                        .unwrap_or(std::cmp::Ordering::Equal),
                    (Value::Version { parts: ap, .. }, Value::Version { parts: bp, .. }) => {
                        Self::version_cmp_parts(ap, bp)
                    }
                    _ => left.to_string_value().cmp(&right.to_string_value()),
                };
                Ok(Self::make_order(ord))
            }
            TokenKind::Ident(name) if name == "eqv" => Ok(Value::Bool(left.eqv(&right))),
            TokenKind::Ident(name) if name == "but" => {
                // 'but' mixin operator: returns the left value with role/value mixed in.
                // For now, a simplified implementation that preserves the base value.
                Ok(left)
            }
            TokenKind::Ident(name) if name == "x" => {
                let s = left.to_string_value();
                let n = match right {
                    Value::Int(n) => n.max(0) as usize,
                    _ => 0,
                };
                Ok(Value::Str(s.repeat(n)))
            }
            TokenKind::Ident(name) if name == "xx" => {
                let n = match right {
                    Value::Int(n) => n.max(0) as usize,
                    _ => 0,
                };
                let items: Vec<Value> = std::iter::repeat_n(left, n).collect();
                Ok(Value::Array(items))
            }
            TokenKind::FatArrow => {
                let key = left.to_string_value();
                Ok(Value::Pair(key, Box::new(right)))
            }
            // Set operators
            TokenKind::SetElem => {
                let key = left.to_string_value();
                let result = match &right {
                    Value::Set(s) => s.contains(&key),
                    Value::Bag(b) => b.contains_key(&key),
                    Value::Mix(m) => m.contains_key(&key),
                    _ => false,
                };
                Ok(Value::Bool(result))
            }
            TokenKind::SetCont => {
                let key = right.to_string_value();
                let result = match &left {
                    Value::Set(s) => s.contains(&key),
                    Value::Bag(b) => b.contains_key(&key),
                    Value::Mix(m) => m.contains_key(&key),
                    _ => false,
                };
                Ok(Value::Bool(result))
            }
            TokenKind::SetUnion => match (left, right) {
                (Value::Set(mut a), Value::Set(b)) => {
                    for elem in b {
                        a.insert(elem);
                    }
                    Ok(Value::Set(a))
                }
                (Value::Bag(mut a), Value::Bag(b)) => {
                    for (k, v) in b {
                        let e = a.entry(k).or_insert(0);
                        *e = (*e).max(v);
                    }
                    Ok(Value::Bag(a))
                }
                (Value::Mix(mut a), Value::Mix(b)) => {
                    for (k, v) in b {
                        let e = a.entry(k).or_insert(0.0);
                        *e = e.max(v);
                    }
                    Ok(Value::Mix(a))
                }
                (Value::Set(a), Value::Bag(mut b)) => {
                    for elem in a {
                        b.entry(elem).or_insert(1);
                    }
                    Ok(Value::Bag(b))
                }
                (Value::Bag(mut a), Value::Set(b)) => {
                    for elem in b {
                        a.entry(elem).or_insert(1);
                    }
                    Ok(Value::Bag(a))
                }
                (l, r) => {
                    let a = Self::coerce_to_set(&l);
                    let b = Self::coerce_to_set(&r);
                    let mut result = a;
                    for elem in b {
                        result.insert(elem);
                    }
                    Ok(Value::Set(result))
                }
            },
            TokenKind::SetIntersect => match (left, right) {
                (Value::Set(a), Value::Set(b)) => {
                    Ok(Value::Set(a.intersection(&b).cloned().collect()))
                }
                (Value::Bag(a), Value::Bag(b)) => {
                    let mut result = HashMap::new();
                    for (k, v) in &a {
                        if let Some(bv) = b.get(k) {
                            result.insert(k.clone(), (*v).min(*bv));
                        }
                    }
                    Ok(Value::Bag(result))
                }
                (Value::Mix(a), Value::Mix(b)) => {
                    let mut result = HashMap::new();
                    for (k, v) in &a {
                        if let Some(bv) = b.get(k) {
                            result.insert(k.clone(), v.min(*bv));
                        }
                    }
                    Ok(Value::Mix(result))
                }
                (l, r) => {
                    let a = Self::coerce_to_set(&l);
                    let b = Self::coerce_to_set(&r);
                    Ok(Value::Set(a.intersection(&b).cloned().collect()))
                }
            },
            TokenKind::SetDiff => match (left, right) {
                (Value::Set(a), Value::Set(b)) => {
                    Ok(Value::Set(a.difference(&b).cloned().collect()))
                }
                (Value::Bag(a), Value::Bag(b)) => {
                    let mut result = HashMap::new();
                    for (k, v) in a {
                        let bv = b.get(&k).copied().unwrap_or(0);
                        if v > bv {
                            result.insert(k, v - bv);
                        }
                    }
                    Ok(Value::Bag(result))
                }
                (Value::Mix(a), Value::Mix(b)) => {
                    let mut result = HashMap::new();
                    for (k, v) in a {
                        let bv = b.get(&k).copied().unwrap_or(0.0);
                        if v > bv {
                            result.insert(k, v - bv);
                        }
                    }
                    Ok(Value::Mix(result))
                }
                (l, r) => {
                    let a = Self::coerce_to_set(&l);
                    let b = Self::coerce_to_set(&r);
                    Ok(Value::Set(a.difference(&b).cloned().collect()))
                }
            },
            TokenKind::SetSymDiff => match (left, right) {
                (Value::Set(a), Value::Set(b)) => {
                    Ok(Value::Set(a.symmetric_difference(&b).cloned().collect()))
                }
                (l, r) => {
                    let a = Self::coerce_to_set(&l);
                    let b = Self::coerce_to_set(&r);
                    Ok(Value::Set(a.symmetric_difference(&b).cloned().collect()))
                }
            },
            TokenKind::SetSubset => {
                let a = Self::coerce_to_set(&left);
                let b = Self::coerce_to_set(&right);
                Ok(Value::Bool(a.is_subset(&b)))
            }
            TokenKind::SetSuperset => {
                let a = Self::coerce_to_set(&left);
                let b = Self::coerce_to_set(&right);
                Ok(Value::Bool(a.is_superset(&b)))
            }
            TokenKind::SetStrictSubset => {
                let a = Self::coerce_to_set(&left);
                let b = Self::coerce_to_set(&right);
                Ok(Value::Bool(a.is_subset(&b) && a.len() < b.len()))
            }
            TokenKind::SetStrictSuperset => {
                let a = Self::coerce_to_set(&left);
                let b = Self::coerce_to_set(&right);
                Ok(Value::Bool(a.is_superset(&b) && a.len() > b.len()))
            }
            _ => Err(RuntimeError::new("Unknown binary operator")),
        }
    }

    /// Evaluate an infix operator called as a function: `infix:<op>(args)`.
    /// With 0 or 1 args, returns True (identity for chaining operators).
    /// With 2 args, evaluates `left op right`.
    fn eval_infix_as_func(&mut self, op_name: &str, args: &[Expr]) -> Result<Value, RuntimeError> {
        match args.len() {
            0 | 1 => Ok(Value::Bool(true)),
            2 => {
                let left = self.eval_expr(&args[0])?;
                let right = self.eval_expr(&args[1])?;
                let tok = match op_name {
                    "==" => TokenKind::EqEq,
                    "!=" => TokenKind::BangEq,
                    "===" => TokenKind::EqEqEq,
                    "<" => TokenKind::Lt,
                    "<=" => TokenKind::Lte,
                    ">" => TokenKind::Gt,
                    ">=" => TokenKind::Gte,
                    "eq" => TokenKind::Ident("eq".to_string()),
                    "ne" => TokenKind::Ident("ne".to_string()),
                    "lt" => TokenKind::Ident("lt".to_string()),
                    "le" => TokenKind::Ident("le".to_string()),
                    "gt" => TokenKind::Ident("gt".to_string()),
                    "ge" => TokenKind::Ident("ge".to_string()),
                    "~~" => TokenKind::SmartMatch,
                    "+" => TokenKind::Plus,
                    "-" => TokenKind::Minus,
                    "*" => TokenKind::Star,
                    "/" => TokenKind::Slash,
                    "~" => TokenKind::Tilde,
                    "!==" => {
                        let eq_result = self.eval_binary(left, &TokenKind::EqEq, right)?;
                        return Ok(Value::Bool(!eq_result.truthy()));
                    }
                    _ => {
                        return Err(RuntimeError::new(format!(
                            "Unknown infix operator: {}",
                            op_name
                        )));
                    }
                };
                self.eval_binary(left, &tok, right)
            }
            _ => Err(RuntimeError::new(format!(
                "Too many arguments to infix:<{}>",
                op_name
            ))),
        }
    }

    /// Bridge: call a method on a pre-evaluated target with pre-evaluated args (for VM).
    pub(crate) fn eval_method_call_with_values(
        &mut self,
        target: Value,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let arg_exprs: Vec<Expr> = args.into_iter().map(Expr::Literal).collect();
        self.eval_expr(&Expr::MethodCall {
            target: Box::new(Expr::Literal(target)),
            name: method.to_string(),
            args: arg_exprs,
            modifier: None,
        })
    }

    pub(crate) fn call_method_with_values(
        &mut self,
        target: Value,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        if method == "say" && args.is_empty() {
            self.output.push_str(&target.to_string_value());
            self.output.push('\n');
            return Ok(Value::Nil);
        }
        if let Value::Instance {
            class_name,
            attributes,
            ..
        } = &target
            && self.class_has_method(class_name, method)
        {
            let (result, _updated) =
                self.run_instance_method(class_name, attributes.clone(), method, args)?;
            return Ok(result);
        }
        if method == "new"
            && let Value::Package(class_name) = &target
            && self.classes.contains_key(class_name)
            && !matches!(
                class_name.as_str(),
                "Hash"
                    | "Version"
                    | "Promise"
                    | "Channel"
                    | "Supply"
                    | "Proc::Async"
                    | "IO::Path"
                    | "IO::Handle"
                    | "IO::Spec"
            )
        {
            let mut attrs = HashMap::new();
            for (attr_name, _is_public, default) in self.collect_class_attributes(class_name) {
                let val = if let Some(expr) = default {
                    self.eval_expr(&expr)?
                } else {
                    Value::Nil
                };
                attrs.insert(attr_name, val);
            }
            for val in &args {
                if let Value::Pair(k, v) = val {
                    attrs.insert(k.clone(), *v.clone());
                }
            }
            if self.class_has_method(class_name, "BUILD") {
                let (_v, updated) =
                    self.run_instance_method(class_name, attrs, "BUILD", Vec::new())?;
                attrs = updated;
            }
            if self.class_has_method(class_name, "TWEAK") {
                let (_v, updated) =
                    self.run_instance_method(class_name, attrs, "TWEAK", Vec::new())?;
                attrs = updated;
            }
            return Ok(Value::make_instance(class_name.clone(), attrs));
        }
        self.eval_method_call_with_values(target, method, args)
    }

    /// Bridge: method call on a mutable variable target (for VM CallMethodMut).
    /// Constructs the correct Expr variant so the interpreter can write back mutations.
    pub(crate) fn eval_method_call_mut_with_values(
        &mut self,
        target_var: &str,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let arg_exprs: Vec<Expr> = args.into_iter().map(Expr::Literal).collect();
        let target_expr = if let Some(name) = target_var.strip_prefix('@') {
            Expr::ArrayVar(name.to_string())
        } else if let Some(name) = target_var.strip_prefix('%') {
            Expr::HashVar(name.to_string())
        } else if let Some(name) = target_var.strip_prefix('&') {
            Expr::CodeVar(name.to_string())
        } else {
            Expr::Var(target_var.to_string())
        };
        self.eval_expr(&Expr::MethodCall {
            target: Box::new(target_expr),
            name: method.to_string(),
            args: arg_exprs,
            modifier: None,
        })
    }

    pub(crate) fn call_method_mut_with_values(
        &mut self,
        target_var: &str,
        target: Value,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        if target_var.starts_with('@') {
            let key = target_var.to_string();
            match method {
                "push" | "append" => {
                    let mut items = match self.env.get(&key) {
                        Some(Value::Array(existing)) => existing.clone(),
                        _ => match target {
                            Value::Array(v) => v,
                            _ => Vec::new(),
                        },
                    };
                    items.extend(args);
                    self.env.insert(key, Value::Array(items));
                    return Ok(Value::Nil);
                }
                "unshift" | "prepend" => {
                    let items = match self.env.get(&key) {
                        Some(Value::Array(existing)) => existing.clone(),
                        _ => match target {
                            Value::Array(v) => v,
                            _ => Vec::new(),
                        },
                    };
                    let mut pref = args;
                    pref.extend(items);
                    self.env.insert(key, Value::Array(pref));
                    return Ok(Value::Nil);
                }
                "pop" => {
                    let mut items = match self.env.get(&key) {
                        Some(Value::Array(existing)) => existing.clone(),
                        _ => match target {
                            Value::Array(v) => v,
                            _ => Vec::new(),
                        },
                    };
                    let out = items.pop().unwrap_or(Value::Nil);
                    self.env.insert(key, Value::Array(items));
                    return Ok(out);
                }
                "shift" => {
                    let mut items = match self.env.get(&key) {
                        Some(Value::Array(existing)) => existing.clone(),
                        _ => match target {
                            Value::Array(v) => v,
                            _ => Vec::new(),
                        },
                    };
                    let out = if items.is_empty() {
                        Value::Nil
                    } else {
                        items.remove(0)
                    };
                    self.env.insert(key, Value::Array(items));
                    return Ok(out);
                }
                _ => {}
            }
        }

        if let Value::Instance {
            class_name,
            attributes,
            ..
        } = target.clone()
            && self.class_has_method(&class_name, method)
        {
            let (result, updated) =
                self.run_instance_method(&class_name, attributes, method, args)?;
            self.env.insert(
                target_var.to_string(),
                Value::make_instance(class_name, updated),
            );
            return Ok(result);
        }

        self.eval_method_call_mut_with_values(target_var, method, args)
    }

    /// Bridge: execute a statement-level call with pre-evaluated positional values (for VM).
    pub(crate) fn exec_call_with_values(
        &mut self,
        name: &str,
        args: Vec<Value>,
    ) -> Result<(), RuntimeError> {
        let call_args: Vec<CallArg> = args
            .into_iter()
            .map(|v| CallArg::Positional(Expr::Literal(v)))
            .collect();
        self.exec_call(name, &call_args)
    }

    /// Bridge: execute a statement-level call with full CallArg shape (for VM).
    pub(crate) fn exec_call_with_call_args(
        &mut self,
        name: &str,
        args: Vec<CallArg>,
    ) -> Result<(), RuntimeError> {
        self.exec_call(name, &args)
    }

    fn smart_match(&mut self, left: &Value, right: &Value) -> bool {
        match (left, right) {
            (Value::Version { .. }, Value::Version { parts, plus, minus }) => {
                Self::version_smart_match(left, parts, *plus, *minus)
            }
            // When RHS is a callable (Sub), invoke it with LHS as argument and
            // return truthiness of the result.  If the sub accepts no parameters,
            // call it with no arguments (simple closure truth).
            (_, Value::Sub { params, .. }) => {
                let func = right.clone();
                let args = if params.is_empty() {
                    vec![]
                } else {
                    vec![left.clone()]
                };
                match self.call_sub_value(func, args, false) {
                    Ok(result) => result.truthy(),
                    Err(_) => false,
                }
            }
            // Built-in routines used as callables in smartmatch
            (_, Value::Routine { .. }) => {
                let func = right.clone();
                match self.call_sub_value(func, vec![left.clone()], false) {
                    Ok(result) => result.truthy(),
                    Err(_) => false,
                }
            }
            (_, Value::Regex(pat)) => {
                let text = left.to_string_value();
                if let Some(captures) = self.regex_match_with_captures(pat, &text) {
                    // Set $/ to the matched substring
                    if let Some((start, end)) = self.regex_find_first(pat, &text) {
                        let chars: Vec<char> = text.chars().collect();
                        let matched: String = chars[start..end].iter().collect();
                        self.env.insert("/".to_string(), Value::Str(matched));
                    }
                    for (k, v) in captures {
                        self.env.insert(format!("<{}>", k), Value::Str(v));
                    }
                    return true;
                }
                self.env.insert("/".to_string(), Value::Nil);
                false
            }
            // When RHS is a type/Package, check type membership
            (_, Value::Package(type_name)) => {
                // A Package on the LHS is a type object - only matches the same type
                if let Value::Package(left_name) = left {
                    return Self::type_matches(type_name, left_name);
                }
                self.type_matches_value(type_name, left)
            }
            // When LHS is a type object (Package), only match same type or type hierarchy
            (Value::Package(_), _) => false,
            // When RHS is NaN, check if LHS is also NaN
            (_, Value::Num(b)) if b.is_nan() => Self::value_is_nan(left),
            (Value::Num(a), _) if a.is_nan() => Self::value_is_nan(right),
            (Value::Int(a), Value::Int(b)) => a == b,
            (Value::Num(a), Value::Num(b)) => a == b,
            (Value::Int(a), Value::Num(b)) => (*a as f64) == *b,
            (Value::Num(a), Value::Int(b)) => *a == (*b as f64),
            (Value::Str(a), Value::Str(b)) => a == b,
            (Value::Int(a), Value::Str(b)) => a.to_string() == *b,
            (Value::Str(a), Value::Int(b)) => *a == b.to_string(),
            (Value::Nil, Value::Str(s)) => s.is_empty(),
            // Instance identity: two instances match iff they have the same id
            (Value::Instance { id: id_a, .. }, Value::Instance { id: id_b, .. }) => id_a == id_b,
            // Default: non-matching types don't match
            (Value::Instance { .. }, _) | (_, Value::Instance { .. }) => false,
            _ => true,
        }
    }

    #[allow(dead_code)]
    fn regex_is_match(&self, pattern: &str, text: &str) -> bool {
        let parsed = match self.parse_regex(pattern) {
            Some(p) => p,
            None => return false,
        };
        let chars: Vec<char> = text.chars().collect();
        if parsed.anchor_start {
            return self.regex_match_from(&parsed, &chars, 0);
        }
        for start in 0..=chars.len() {
            if self.regex_match_from(&parsed, &chars, start) {
                return true;
            }
        }
        false
    }

    fn regex_match_with_captures(
        &self,
        pattern: &str,
        text: &str,
    ) -> Option<HashMap<String, String>> {
        let parsed = self.parse_regex(pattern)?;
        let chars: Vec<char> = text.chars().collect();
        if parsed.anchor_start {
            return self
                .regex_match_end_from_caps(&parsed, &chars, 0)
                .map(|(_, caps)| caps);
        }
        for start in 0..=chars.len() {
            if let Some((_, caps)) = self.regex_match_end_from_caps(&parsed, &chars, start) {
                return Some(caps);
            }
        }
        None
    }

    fn regex_find_first(&self, pattern: &str, text: &str) -> Option<(usize, usize)> {
        let parsed = self.parse_regex(pattern)?;
        let chars: Vec<char> = text.chars().collect();
        if parsed.anchor_start {
            return self
                .regex_match_end_from(&parsed, &chars, 0)
                .map(|end| (0, end));
        }
        for start in 0..=chars.len() {
            if let Some(end) = self.regex_match_end_from(&parsed, &chars, start) {
                return Some((start, end));
            }
        }
        None
    }

    fn regex_match_len_at_start(&self, pattern: &str, text: &str) -> Option<usize> {
        let parsed = self.parse_regex(pattern)?;
        let chars: Vec<char> = text.chars().collect();
        self.regex_match_end_from(&parsed, &chars, 0)
    }

    fn regex_match_end_from(
        &self,
        pattern: &RegexPattern,
        chars: &[char],
        start: usize,
    ) -> Option<usize> {
        let mut stack = Vec::new();
        stack.push((0usize, start));
        while let Some((idx, pos)) = stack.pop() {
            if idx == pattern.tokens.len() {
                if pattern.anchor_end {
                    if pos == chars.len() {
                        return Some(pos);
                    }
                } else {
                    return Some(pos);
                }
                continue;
            }
            let token = &pattern.tokens[idx];
            match token.quant {
                RegexQuant::One => {
                    if let Some(next) = self.regex_match_atom(&token.atom, chars, pos) {
                        stack.push((idx + 1, next));
                    }
                }
                RegexQuant::ZeroOrOne => {
                    if let Some(next) = self.regex_match_atom(&token.atom, chars, pos) {
                        stack.push((idx + 1, next));
                    }
                    stack.push((idx + 1, pos));
                }
                RegexQuant::ZeroOrMore => {
                    let mut positions = Vec::new();
                    positions.push(pos);
                    let mut current = pos;
                    while let Some(next) = self.regex_match_atom(&token.atom, chars, current) {
                        positions.push(next);
                        current = next;
                    }
                    for p in positions {
                        stack.push((idx + 1, p));
                    }
                }
                RegexQuant::OneOrMore => {
                    let mut positions = Vec::new();
                    let mut current = match self.regex_match_atom(&token.atom, chars, pos) {
                        Some(next) => next,
                        None => continue,
                    };
                    positions.push(current);
                    while let Some(next) = self.regex_match_atom(&token.atom, chars, current) {
                        positions.push(next);
                        current = next;
                    }
                    for p in positions {
                        stack.push((idx + 1, p));
                    }
                }
            }
        }
        None
    }

    fn regex_match_end_from_caps(
        &self,
        pattern: &RegexPattern,
        chars: &[char],
        start: usize,
    ) -> Option<(usize, HashMap<String, String>)> {
        let mut stack = Vec::new();
        stack.push((0usize, start, HashMap::new()));
        while let Some((idx, pos, caps)) = stack.pop() {
            if idx == pattern.tokens.len() {
                if pattern.anchor_end {
                    if pos == chars.len() {
                        return Some((pos, caps));
                    }
                } else {
                    return Some((pos, caps));
                }
                continue;
            }
            let token = &pattern.tokens[idx];
            match token.quant {
                RegexQuant::One => {
                    if let Some((next, cap)) =
                        self.regex_match_atom_with_capture(&token.atom, chars, pos)
                    {
                        let mut next_caps = caps.clone();
                        if let Some((name, value)) = cap {
                            next_caps.insert(name, value);
                        }
                        stack.push((idx + 1, next, next_caps));
                    }
                }
                RegexQuant::ZeroOrOne => {
                    if let Some((next, cap)) =
                        self.regex_match_atom_with_capture(&token.atom, chars, pos)
                    {
                        let mut next_caps = caps.clone();
                        if let Some((name, value)) = cap {
                            next_caps.insert(name, value);
                        }
                        stack.push((idx + 1, next, next_caps));
                    }
                    stack.push((idx + 1, pos, caps.clone()));
                }
                RegexQuant::ZeroOrMore => {
                    let mut positions = Vec::new();
                    positions.push((pos, caps.clone()));
                    let mut current = pos;
                    let mut current_caps = caps.clone();
                    while let Some((next, cap)) =
                        self.regex_match_atom_with_capture(&token.atom, chars, current)
                    {
                        if let Some((name, value)) = cap {
                            current_caps.insert(name, value);
                        }
                        positions.push((next, current_caps.clone()));
                        current = next;
                    }
                    for (p, c) in positions {
                        stack.push((idx + 1, p, c));
                    }
                }
                RegexQuant::OneOrMore => {
                    let mut positions = Vec::new();
                    let (mut current, mut current_caps) =
                        match self.regex_match_atom_with_capture(&token.atom, chars, pos) {
                            Some((next, cap)) => {
                                let mut caps = caps.clone();
                                if let Some((name, value)) = cap {
                                    caps.insert(name, value);
                                }
                                (next, caps)
                            }
                            None => continue,
                        };
                    positions.push((current, current_caps.clone()));
                    while let Some((next, cap)) =
                        self.regex_match_atom_with_capture(&token.atom, chars, current)
                    {
                        if let Some((name, value)) = cap {
                            current_caps.insert(name, value);
                        }
                        positions.push((next, current_caps.clone()));
                        current = next;
                    }
                    for (p, c) in positions {
                        stack.push((idx + 1, p, c));
                    }
                }
            }
        }
        None
    }

    pub(crate) fn char_idx_to_byte(text: &str, idx: usize) -> usize {
        if idx == 0 {
            return 0;
        }
        for (count, (b, _)) in text.char_indices().enumerate() {
            if count == idx {
                return b;
            }
        }
        text.len()
    }

    #[allow(dead_code)]
    fn regex_match_from(&self, pattern: &RegexPattern, chars: &[char], start: usize) -> bool {
        let mut stack = Vec::new();
        stack.push((0usize, start));
        while let Some((idx, pos)) = stack.pop() {
            if idx == pattern.tokens.len() {
                if pattern.anchor_end {
                    if pos == chars.len() {
                        return true;
                    }
                } else {
                    return true;
                }
                continue;
            }
            let token = &pattern.tokens[idx];
            match token.quant {
                RegexQuant::One => {
                    if let Some(next) = self.regex_match_atom(&token.atom, chars, pos) {
                        stack.push((idx + 1, next));
                    }
                }
                RegexQuant::ZeroOrOne => {
                    if let Some(next) = self.regex_match_atom(&token.atom, chars, pos) {
                        stack.push((idx + 1, next));
                    }
                    stack.push((idx + 1, pos));
                }
                RegexQuant::ZeroOrMore => {
                    let mut positions = Vec::new();
                    positions.push(pos);
                    let mut current = pos;
                    while let Some(next) = self.regex_match_atom(&token.atom, chars, current) {
                        positions.push(next);
                        current = next;
                    }
                    for p in positions {
                        stack.push((idx + 1, p));
                    }
                }
                RegexQuant::OneOrMore => {
                    let mut positions = Vec::new();
                    let mut current = match self.regex_match_atom(&token.atom, chars, pos) {
                        Some(next) => next,
                        None => continue,
                    };
                    positions.push(current);
                    while let Some(next) = self.regex_match_atom(&token.atom, chars, current) {
                        positions.push(next);
                        current = next;
                    }
                    for p in positions {
                        stack.push((idx + 1, p));
                    }
                }
            }
        }
        false
    }

    fn regex_match_atom(&self, atom: &RegexAtom, chars: &[char], pos: usize) -> Option<usize> {
        // Group, Alternation, and ZeroWidth can match zero-width, so check before pos >= chars.len()
        match atom {
            RegexAtom::Group(pattern) => {
                return self.regex_match_end_from(pattern, chars, pos);
            }
            RegexAtom::Alternation(alternatives) => {
                for alt in alternatives {
                    if let Some(end) = self.regex_match_end_from(alt, chars, pos) {
                        return Some(end);
                    }
                }
                return None;
            }
            RegexAtom::ZeroWidth => {
                return Some(pos); // Matches at any position without consuming
            }
            RegexAtom::UnicodePropAssert { name, negated } => {
                // Zero-width assertion: check next char but don't consume
                if pos >= chars.len() {
                    // At end of string, negated assertion succeeds
                    return if *negated { Some(pos) } else { None };
                }
                let c = chars[pos];
                let prop_match = check_unicode_property(name, c);
                let result = if *negated { !prop_match } else { prop_match };
                return if result { Some(pos) } else { None };
            }
            _ => {}
        }
        if pos >= chars.len() {
            return None;
        }
        let c = chars[pos];
        // Newline needs special handling: \r\n is a single logical newline (2 chars)
        match atom {
            RegexAtom::Newline => {
                // Raku \n matches: \r\n (CR+LF), \n (LF), \r (CR), \x85 (NEL), \x2028 (LINE SEP)
                if c == '\r' && pos + 1 < chars.len() && chars[pos + 1] == '\n' {
                    return Some(pos + 2); // CR/LF
                }
                if c == '\n' || c == '\r' || c == '\u{85}' || c == '\u{2028}' {
                    return Some(pos + 1);
                }
                return None;
            }
            RegexAtom::NotNewline => {
                // Matches anything that is NOT a newline character
                if c == '\n' || c == '\r' || c == '\u{85}' || c == '\u{2028}' {
                    return None;
                }
                return Some(pos + 1);
            }
            _ => {}
        }
        let matched = match atom {
            RegexAtom::Literal(ch) => *ch == c,
            RegexAtom::Named(name) => {
                let name_chars: Vec<char> = name.chars().collect();
                if pos + name_chars.len() > chars.len() {
                    false
                } else {
                    chars[pos..pos + name_chars.len()] == name_chars[..]
                }
            }
            RegexAtom::Any => true,
            RegexAtom::Digit => c.is_ascii_digit(),
            RegexAtom::Word => c.is_ascii_alphanumeric() || c == '_',
            RegexAtom::Space => c.is_whitespace(),
            RegexAtom::CharClass(class) => self.regex_match_class(class, c),
            RegexAtom::UnicodeProp { name, negated } => {
                let prop_match = check_unicode_property(name, c);
                if *negated { !prop_match } else { prop_match }
            }
            RegexAtom::Group(_)
            | RegexAtom::Alternation(_)
            | RegexAtom::Newline
            | RegexAtom::NotNewline
            | RegexAtom::ZeroWidth
            | RegexAtom::UnicodePropAssert { .. } => unreachable!(),
        };
        if matched {
            match atom {
                RegexAtom::Named(name) => Some(pos + name.chars().count()),
                _ => Some(pos + 1),
            }
        } else {
            None
        }
    }

    fn regex_match_atom_with_capture(
        &self,
        atom: &RegexAtom,
        chars: &[char],
        pos: usize,
    ) -> Option<(usize, Option<(String, String)>)> {
        // Handle zero-width and group atoms before the length check
        match atom {
            RegexAtom::Group(_)
            | RegexAtom::Alternation(_)
            | RegexAtom::ZeroWidth
            | RegexAtom::UnicodePropAssert { .. } => {
                return self
                    .regex_match_atom(atom, chars, pos)
                    .map(|next| (next, None));
            }
            _ => {}
        }
        if pos >= chars.len() {
            return None;
        }
        match atom {
            RegexAtom::Named(name) => {
                let name_chars: Vec<char> = name.chars().collect();
                if pos + name_chars.len() > chars.len() {
                    return None;
                }
                if chars[pos..pos + name_chars.len()] == name_chars[..] {
                    let captured: String = name_chars.iter().collect();
                    return Some((pos + name_chars.len(), Some((name.clone(), captured))));
                }
                None
            }
            _ => self
                .regex_match_atom(atom, chars, pos)
                .map(|next| (next, None)),
        }
    }

    fn regex_match_class(&self, class: &CharClass, c: char) -> bool {
        let mut matched = false;
        for item in &class.items {
            match item {
                ClassItem::Range(a, b) => {
                    if *a <= c && c <= *b {
                        matched = true;
                        break;
                    }
                }
                ClassItem::Char(ch) => {
                    if *ch == c {
                        matched = true;
                        break;
                    }
                }
                ClassItem::Digit => {
                    if c.is_ascii_digit() {
                        matched = true;
                        break;
                    }
                }
                ClassItem::Word => {
                    if c.is_ascii_alphanumeric() || c == '_' {
                        matched = true;
                        break;
                    }
                }
                ClassItem::Space => {
                    if c.is_whitespace() {
                        matched = true;
                        break;
                    }
                }
            }
        }
        if class.negated { !matched } else { matched }
    }

    fn parse_regex(&self, pattern: &str) -> Option<RegexPattern> {
        let mut chars = pattern.chars().peekable();
        let mut tokens = Vec::new();
        let mut anchor_start = false;
        let mut anchor_end = false;
        while let Some(c) = chars.next() {
            // In Raku, unescaped whitespace in regex is insignificant
            if c.is_whitespace() {
                continue;
            }
            // '#' starts a comment until end of line in Raku regex
            if c == '#' {
                for ch in chars.by_ref() {
                    if ch == '\n' {
                        break;
                    }
                }
                continue;
            }
            if c == '^' && tokens.is_empty() {
                anchor_start = true;
                continue;
            }
            if c == '$' && chars.clone().all(|ch| ch.is_whitespace()) {
                anchor_end = true;
                break;
            }
            let atom = match c {
                '.' => RegexAtom::Any,
                '\\' => {
                    let esc = chars.next()?;
                    match esc {
                        'd' => RegexAtom::Digit,
                        'w' => RegexAtom::Word,
                        's' => RegexAtom::Space,
                        'n' => RegexAtom::Newline,
                        'N' => RegexAtom::NotNewline,
                        't' => RegexAtom::Literal('\t'),
                        'r' => RegexAtom::Literal('\r'),
                        'x' => {
                            // \x[HEX] hex escape in regex
                            if chars.peek() == Some(&'[') {
                                chars.next(); // skip '['
                                let mut hex = String::new();
                                while let Some(&ch) = chars.peek() {
                                    if ch == ']' {
                                        chars.next();
                                        break;
                                    }
                                    hex.push(ch);
                                    chars.next();
                                }
                                if let Some(c) =
                                    u32::from_str_radix(&hex, 16).ok().and_then(char::from_u32)
                                {
                                    RegexAtom::Literal(c)
                                } else {
                                    continue;
                                }
                            } else {
                                RegexAtom::Literal('x')
                            }
                        }
                        'o' => {
                            // \o[OCT] octal escape in regex
                            if chars.peek() == Some(&'[') {
                                chars.next(); // skip '['
                                let mut oct = String::new();
                                while let Some(&ch) = chars.peek() {
                                    if ch == ']' {
                                        chars.next();
                                        break;
                                    }
                                    oct.push(ch);
                                    chars.next();
                                }
                                if let Some(c) =
                                    u32::from_str_radix(&oct, 8).ok().and_then(char::from_u32)
                                {
                                    RegexAtom::Literal(c)
                                } else {
                                    continue;
                                }
                            } else {
                                RegexAtom::Literal('o')
                            }
                        }
                        'c' => {
                            // \c[NAME] or \c[NAME1, NAME2] named character escape in regex
                            if chars.peek() == Some(&'[') {
                                chars.next(); // skip '['
                                let mut name = String::new();
                                while let Some(&ch) = chars.peek() {
                                    if ch == ']' {
                                        chars.next();
                                        break;
                                    }
                                    name.push(ch);
                                    chars.next();
                                }
                                // Handle comma-separated names
                                let parts: Vec<&str> = name.split(',').map(|s| s.trim()).collect();
                                let mut resolved: Vec<char> = Vec::new();
                                for part in &parts {
                                    if let Some(c) = crate::lexer::lookup_unicode_char_by_name(part)
                                    {
                                        resolved.push(c);
                                    }
                                }
                                if resolved.is_empty() {
                                    continue;
                                }
                                // Push all but last as separate literal tokens
                                for &c in &resolved[..resolved.len() - 1] {
                                    tokens.push(RegexToken {
                                        atom: RegexAtom::Literal(c),
                                        quant: RegexQuant::One,
                                    });
                                }
                                RegexAtom::Literal(*resolved.last().unwrap())
                            } else {
                                RegexAtom::Literal('c')
                            }
                        }
                        'C' => {
                            // \C[NAME] matches any char that is NOT the named char
                            if chars.peek() == Some(&'[') {
                                chars.next();
                                let mut name = String::new();
                                while let Some(&ch) = chars.peek() {
                                    if ch == ']' {
                                        chars.next();
                                        break;
                                    }
                                    name.push(ch);
                                    chars.next();
                                }
                                if let Some(c) = crate::lexer::lookup_unicode_char_by_name(&name) {
                                    RegexAtom::CharClass(CharClass {
                                        negated: true,
                                        items: vec![ClassItem::Char(c)],
                                    })
                                } else {
                                    continue;
                                }
                            } else {
                                RegexAtom::Literal('C')
                            }
                        }
                        'X' => {
                            // \X[HEX] matches any char that is NOT the given hex char
                            if chars.peek() == Some(&'[') {
                                chars.next();
                                let mut hex = String::new();
                                while let Some(&ch) = chars.peek() {
                                    if ch == ']' {
                                        chars.next();
                                        break;
                                    }
                                    hex.push(ch);
                                    chars.next();
                                }
                                if let Some(c) =
                                    u32::from_str_radix(&hex, 16).ok().and_then(char::from_u32)
                                {
                                    RegexAtom::CharClass(CharClass {
                                        negated: true,
                                        items: vec![ClassItem::Char(c)],
                                    })
                                } else {
                                    continue;
                                }
                            } else {
                                RegexAtom::Literal('X')
                            }
                        }
                        other => RegexAtom::Literal(other),
                    }
                }
                '\'' => {
                    // Quoted literal string in Raku regex: 'foo-bar' matches literally
                    let mut literal = String::new();
                    for ch in chars.by_ref() {
                        if ch == '\'' {
                            break;
                        }
                        literal.push(ch);
                    }
                    // Expand to a sequence of literal atoms
                    let lit_chars: Vec<char> = literal.chars().collect();
                    if lit_chars.is_empty() {
                        // Empty string literal matches zero-width
                        continue;
                    }
                    // Push all but the last char as One-quantified literals
                    for &lc in &lit_chars[..lit_chars.len() - 1] {
                        tokens.push(RegexToken {
                            atom: RegexAtom::Literal(lc),
                            quant: RegexQuant::One,
                        });
                    }
                    // The last char will get any quantifier attached below
                    RegexAtom::Literal(*lit_chars.last().unwrap())
                }
                '<' => {
                    // Read content between < and >, handling nested <...>
                    let mut name = String::new();
                    let mut angle_depth = 1usize;
                    for ch in chars.by_ref() {
                        if ch == '<' {
                            angle_depth += 1;
                            name.push(ch);
                        } else if ch == '>' {
                            angle_depth -= 1;
                            if angle_depth == 0 {
                                break;
                            }
                            name.push(ch);
                        } else {
                            name.push(ch);
                        }
                    }
                    // Check for Raku character class: <[...]>, <-[...]>, <+[...]>
                    let trimmed = name.trim();
                    if (trimmed.starts_with('[')
                        || trimmed.starts_with("-[")
                        || trimmed.starts_with("+["))
                        && trimmed.ends_with(']')
                    {
                        let negated;
                        let inner;
                        if trimmed.starts_with("-[") {
                            negated = true;
                            inner = &trimmed[2..trimmed.len() - 1];
                        } else if trimmed.starts_with("+[") {
                            negated = false;
                            inner = &trimmed[2..trimmed.len() - 1];
                        } else {
                            negated = false;
                            inner = &trimmed[1..trimmed.len() - 1];
                        }
                        // Parse Raku-style character class content
                        if let Some(class) = self.parse_raku_char_class(inner, negated) {
                            RegexAtom::CharClass(class)
                        } else {
                            continue;
                        }
                    } else if trimmed == "?" {
                        // <?>  null assertion: matches zero-width at any position
                        RegexAtom::ZeroWidth
                    } else if let Some(prop_name) = trimmed.strip_prefix("!:") {
                        // <!:PropName> — zero-width negative Unicode property assertion
                        RegexAtom::UnicodePropAssert {
                            name: prop_name.to_string(),
                            negated: true,
                        }
                    } else if trimmed.starts_with(":!") || trimmed.starts_with("-:") {
                        // <:!PropName> or <-:PropName> — negated Unicode property
                        let prop_name = &trimmed[2..];
                        RegexAtom::UnicodeProp {
                            name: prop_name.to_string(),
                            negated: true,
                        }
                    } else if let Some(prop_name) = trimmed.strip_prefix(':') {
                        // <:PropName> — Unicode property assertion
                        RegexAtom::UnicodeProp {
                            name: prop_name.to_string(),
                            negated: false,
                        }
                    } else {
                        // Check for named character classes
                        match trimmed {
                            "alpha" => RegexAtom::CharClass(CharClass {
                                items: vec![
                                    ClassItem::Range('a', 'z'),
                                    ClassItem::Range('A', 'Z'),
                                    ClassItem::Char('_'),
                                ],
                                negated: false,
                            }),
                            "upper" => RegexAtom::CharClass(CharClass {
                                items: vec![ClassItem::Range('A', 'Z')],
                                negated: false,
                            }),
                            "lower" => RegexAtom::CharClass(CharClass {
                                items: vec![ClassItem::Range('a', 'z')],
                                negated: false,
                            }),
                            "digit" => RegexAtom::CharClass(CharClass {
                                items: vec![ClassItem::Digit],
                                negated: false,
                            }),
                            "xdigit" => RegexAtom::CharClass(CharClass {
                                items: vec![
                                    ClassItem::Range('0', '9'),
                                    ClassItem::Range('a', 'f'),
                                    ClassItem::Range('A', 'F'),
                                ],
                                negated: false,
                            }),
                            "space" | "ws" => RegexAtom::CharClass(CharClass {
                                items: vec![ClassItem::Space],
                                negated: false,
                            }),
                            "alnum" => RegexAtom::CharClass(CharClass {
                                items: vec![
                                    ClassItem::Range('a', 'z'),
                                    ClassItem::Range('A', 'Z'),
                                    ClassItem::Range('0', '9'),
                                ],
                                negated: false,
                            }),
                            _ => RegexAtom::Named(name),
                        }
                    }
                }
                '[' => {
                    // In Raku regex, [...] is a non-capturing group (alternation)
                    // Parse as alternation: [a|b|c]
                    let mut group_pattern = String::new();
                    let mut depth = 1;
                    for ch in chars.by_ref() {
                        if ch == '[' {
                            depth += 1;
                            group_pattern.push(ch);
                        } else if ch == ']' {
                            depth -= 1;
                            if depth == 0 {
                                break;
                            }
                            group_pattern.push(ch);
                        } else {
                            group_pattern.push(ch);
                        }
                    }
                    // Parse the group as alternation
                    let alternatives: Vec<&str> = group_pattern.split('|').collect();
                    if alternatives.len() > 1 {
                        let mut alt_patterns = Vec::new();
                        for alt in alternatives {
                            if let Some(p) = self.parse_regex(alt) {
                                alt_patterns.push(p);
                            }
                        }
                        RegexAtom::Alternation(alt_patterns)
                    } else if let Some(p) = self.parse_regex(&group_pattern) {
                        RegexAtom::Group(p)
                    } else {
                        continue;
                    }
                }
                other => RegexAtom::Literal(other),
            };
            let mut quant = RegexQuant::One;
            if let Some(q) = chars.peek().copied() {
                quant = match q {
                    '*' => {
                        chars.next();
                        RegexQuant::ZeroOrMore
                    }
                    '+' => {
                        chars.next();
                        RegexQuant::OneOrMore
                    }
                    '?' => {
                        chars.next();
                        RegexQuant::ZeroOrOne
                    }
                    _ => RegexQuant::One,
                };
            }
            tokens.push(RegexToken { atom, quant });
        }
        Some(RegexPattern {
            tokens,
            anchor_start,
            anchor_end,
        })
    }

    fn parse_raku_char_class(&self, inner: &str, negated: bool) -> Option<CharClass> {
        // Parse Raku-style character class: a..z, \n, \t, \c[NAME], \x[HEX], etc.
        let mut items = Vec::new();
        let mut chars = inner.chars().peekable();
        // Track whether all items are from negated escapes (\C, \X)
        let mut all_negated_escapes = true;
        let mut has_items = false;
        while let Some(c) = chars.next() {
            if c == ' ' {
                // Raku regex ignores spaces in char classes
                continue;
            }
            if c == '\\' {
                let esc = chars.next()?;
                match esc {
                    'n' => {
                        items.push(ClassItem::Char('\n'));
                        all_negated_escapes = false;
                        has_items = true;
                    }
                    't' => {
                        items.push(ClassItem::Char('\t'));
                        all_negated_escapes = false;
                        has_items = true;
                    }
                    'r' => {
                        items.push(ClassItem::Char('\r'));
                        all_negated_escapes = false;
                        has_items = true;
                    }
                    'd' => {
                        items.push(ClassItem::Digit);
                        all_negated_escapes = false;
                        has_items = true;
                    }
                    'w' => {
                        items.push(ClassItem::Word);
                        all_negated_escapes = false;
                        has_items = true;
                    }
                    's' => {
                        items.push(ClassItem::Space);
                        all_negated_escapes = false;
                        has_items = true;
                    }
                    'c' | 'C' => {
                        // \c[NAME] or \C[NAME] (negated char) inside character class
                        let is_neg = esc == 'C';
                        if chars.peek() == Some(&'[') {
                            chars.next(); // skip '['
                            let mut name = String::new();
                            let mut bracket_depth = 1;
                            while let Some(&ch) = chars.peek() {
                                if ch == '[' {
                                    bracket_depth += 1;
                                    name.push(ch);
                                    chars.next();
                                } else if ch == ']' {
                                    bracket_depth -= 1;
                                    if bracket_depth == 0 {
                                        chars.next();
                                        break;
                                    }
                                    name.push(ch);
                                    chars.next();
                                } else {
                                    name.push(ch);
                                    chars.next();
                                }
                            }
                            // Handle comma-separated names: \c[NAME1, NAME2]
                            for part in name.split(',') {
                                let part = part.trim();
                                if let Some(ch) = crate::lexer::lookup_unicode_char_by_name(part) {
                                    items.push(ClassItem::Char(ch));
                                    has_items = true;
                                }
                            }
                            if !is_neg {
                                all_negated_escapes = false;
                            }
                        } else {
                            items.push(ClassItem::Char(esc));
                            all_negated_escapes = false;
                            has_items = true;
                        }
                    }
                    'x' | 'X' => {
                        // \x[HEX] or \X[HEX] inside character class
                        let is_neg = esc == 'X';
                        if chars.peek() == Some(&'[') {
                            chars.next(); // skip '['
                            let mut hex = String::new();
                            while let Some(&ch) = chars.peek() {
                                if ch == ']' {
                                    chars.next();
                                    break;
                                }
                                hex.push(ch);
                                chars.next();
                            }
                            if let Some(ch) =
                                u32::from_str_radix(&hex, 16).ok().and_then(char::from_u32)
                            {
                                items.push(ClassItem::Char(ch));
                                has_items = true;
                            }
                            if !is_neg {
                                all_negated_escapes = false;
                            }
                        } else {
                            items.push(ClassItem::Char(esc));
                            all_negated_escapes = false;
                            has_items = true;
                        }
                    }
                    'o' => {
                        // \o[OCT] inside character class
                        if chars.peek() == Some(&'[') {
                            chars.next(); // skip '['
                            let mut oct = String::new();
                            while let Some(&ch) = chars.peek() {
                                if ch == ']' {
                                    chars.next();
                                    break;
                                }
                                oct.push(ch);
                                chars.next();
                            }
                            if let Some(ch) =
                                u32::from_str_radix(&oct, 8).ok().and_then(char::from_u32)
                            {
                                items.push(ClassItem::Char(ch));
                            }
                        } else {
                            items.push(ClassItem::Char('o'));
                        }
                        all_negated_escapes = false;
                        has_items = true;
                    }
                    other => {
                        items.push(ClassItem::Char(other));
                        all_negated_escapes = false;
                        has_items = true;
                    }
                }
            } else if chars.peek() == Some(&'.') {
                // Check for '..' range syntax
                let mut peek_chars = chars.clone();
                peek_chars.next(); // consume first '.'
                if peek_chars.peek() == Some(&'.') {
                    // It's a range: a..z
                    chars.next(); // consume first '.'
                    chars.next(); // consume second '.'
                    if let Some(end) = chars.next() {
                        items.push(ClassItem::Range(c, end));
                    }
                } else {
                    items.push(ClassItem::Char(c));
                }
                all_negated_escapes = false;
                has_items = true;
            } else {
                items.push(ClassItem::Char(c));
                all_negated_escapes = false;
                has_items = true;
            }
        }
        // If all items came from negated escapes (\C, \X), flip the negation
        // e.g., <[\C[FF]]> means "everything except FF" = negated class of {FF}
        let final_negated = if has_items && all_negated_escapes {
            !negated
        } else {
            negated
        };
        Some(CharClass {
            items,
            negated: final_negated,
        })
    }

    pub(crate) fn reduction_identity(op: &str) -> Value {
        match op {
            "+" => Value::Int(0),
            "*" => Value::Int(1),
            "~" => Value::Str(String::new()),
            "&&" | "and" => Value::Bool(true),
            "||" | "or" => Value::Bool(false),
            "//" => Value::Nil,
            "min" => Value::Num(f64::INFINITY),
            "max" => Value::Num(f64::NEG_INFINITY),
            _ => Value::Nil,
        }
    }

    pub(crate) fn apply_reduction_op(
        op: &str,
        left: &Value,
        right: &Value,
    ) -> Result<Value, RuntimeError> {
        let to_num = |v: &Value| -> f64 {
            match v {
                Value::Int(i) => *i as f64,
                Value::Num(f) => *f,
                Value::Str(s) => s.parse::<f64>().unwrap_or(0.0),
                Value::Bool(b) => {
                    if *b {
                        1.0
                    } else {
                        0.0
                    }
                }
                _ => 0.0,
            }
        };
        let to_int = |v: &Value| -> i64 {
            match v {
                Value::Int(i) => *i,
                Value::Num(f) => *f as i64,
                Value::Str(s) => s.parse::<i64>().unwrap_or(0),
                Value::Bool(b) => {
                    if *b {
                        1
                    } else {
                        0
                    }
                }
                _ => 0,
            }
        };
        match op {
            "+" => {
                if matches!(left, Value::Num(_)) || matches!(right, Value::Num(_)) {
                    Ok(Value::Num(to_num(left) + to_num(right)))
                } else {
                    Ok(Value::Int(to_int(left) + to_int(right)))
                }
            }
            "-" => {
                if matches!(left, Value::Num(_)) || matches!(right, Value::Num(_)) {
                    Ok(Value::Num(to_num(left) - to_num(right)))
                } else {
                    Ok(Value::Int(to_int(left) - to_int(right)))
                }
            }
            "*" => {
                if matches!(left, Value::Num(_)) || matches!(right, Value::Num(_)) {
                    Ok(Value::Num(to_num(left) * to_num(right)))
                } else {
                    Ok(Value::Int(to_int(left) * to_int(right)))
                }
            }
            "/" => {
                if let (Value::Int(a), Value::Int(b)) = (left, right) {
                    if *b == 0 {
                        return Err(RuntimeError::new("Division by zero"));
                    }
                    Ok(Value::Int(a / b))
                } else {
                    Ok(Value::Num(to_num(left) / to_num(right)))
                }
            }
            "%" => {
                if matches!(left, Value::Num(_)) || matches!(right, Value::Num(_)) {
                    Ok(Value::Num(to_num(left) % to_num(right)))
                } else {
                    Ok(Value::Int(to_int(left) % to_int(right)))
                }
            }
            "**" => Ok(Value::Num(to_num(left).powf(to_num(right)))),
            "~" => Ok(Value::Str(format!(
                "{}{}",
                left.to_string_value(),
                right.to_string_value()
            ))),
            "&&" | "and" => {
                if !left.truthy() {
                    Ok(left.clone())
                } else {
                    Ok(right.clone())
                }
            }
            "||" | "or" => {
                if left.truthy() {
                    Ok(left.clone())
                } else {
                    Ok(right.clone())
                }
            }
            "//" => {
                if !matches!(left, Value::Nil) {
                    Ok(left.clone())
                } else {
                    Ok(right.clone())
                }
            }
            "min" => {
                if to_num(left) <= to_num(right) {
                    Ok(left.clone())
                } else {
                    Ok(right.clone())
                }
            }
            "max" => {
                if to_num(left) >= to_num(right) {
                    Ok(left.clone())
                } else {
                    Ok(right.clone())
                }
            }
            "+&" => Ok(Value::Int(to_int(left) & to_int(right))),
            "+|" => Ok(Value::Int(to_int(left) | to_int(right))),
            "+^" => Ok(Value::Int(to_int(left) ^ to_int(right))),
            "==" => Ok(Value::Bool(to_num(left) == to_num(right))),
            "!=" => Ok(Value::Bool(to_num(left) != to_num(right))),
            "<" => Ok(Value::Bool(to_num(left) < to_num(right))),
            ">" => Ok(Value::Bool(to_num(left) > to_num(right))),
            "<=" => Ok(Value::Bool(to_num(left) <= to_num(right))),
            ">=" => Ok(Value::Bool(to_num(left) >= to_num(right))),
            "eq" => Ok(Value::Bool(
                left.to_string_value() == right.to_string_value(),
            )),
            "ne" => Ok(Value::Bool(
                left.to_string_value() != right.to_string_value(),
            )),
            "lt" => Ok(Value::Bool(
                left.to_string_value() < right.to_string_value(),
            )),
            "gt" => Ok(Value::Bool(
                left.to_string_value() > right.to_string_value(),
            )),
            "le" => Ok(Value::Bool(
                left.to_string_value() <= right.to_string_value(),
            )),
            "ge" => Ok(Value::Bool(
                left.to_string_value() >= right.to_string_value(),
            )),
            "leg" => {
                let ord = left.to_string_value().cmp(&right.to_string_value());
                Ok(Self::make_order(ord))
            }
            "cmp" => {
                let ord = match (left, right) {
                    (Value::Int(a), Value::Int(b)) => a.cmp(b),
                    (Value::Rat(_, _), _)
                    | (_, Value::Rat(_, _))
                    | (Value::FatRat(_, _), _)
                    | (_, Value::FatRat(_, _)) => {
                        if let (Some((an, ad)), Some((bn, bd))) =
                            (Self::to_rat_parts(left), Self::to_rat_parts(right))
                        {
                            (an * bd).cmp(&(bn * ad))
                        } else {
                            left.to_string_value().cmp(&right.to_string_value())
                        }
                    }
                    (Value::Num(a), Value::Num(b)) => {
                        a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal)
                    }
                    (Value::Int(a), Value::Num(b)) => (*a as f64)
                        .partial_cmp(b)
                        .unwrap_or(std::cmp::Ordering::Equal),
                    (Value::Num(a), Value::Int(b)) => a
                        .partial_cmp(&(*b as f64))
                        .unwrap_or(std::cmp::Ordering::Equal),
                    (Value::Version { parts: ap, .. }, Value::Version { parts: bp, .. }) => {
                        Self::version_cmp_parts(ap, bp)
                    }
                    _ => left.to_string_value().cmp(&right.to_string_value()),
                };
                Ok(Self::make_order(ord))
            }
            "gcd" => {
                let (mut a, mut b) = (to_int(left).abs(), to_int(right).abs());
                while b != 0 {
                    let t = b;
                    b = a % b;
                    a = t;
                }
                Ok(Value::Int(a))
            }
            "lcm" => {
                let (a, b) = (to_int(left).abs(), to_int(right).abs());
                if a == 0 && b == 0 {
                    Ok(Value::Int(0))
                } else {
                    let mut ga = a;
                    let mut gb = b;
                    while gb != 0 {
                        let t = gb;
                        gb = ga % gb;
                        ga = t;
                    }
                    Ok(Value::Int((a / ga).wrapping_mul(b)))
                }
            }
            _ => Err(RuntimeError::new(format!(
                "Unsupported reduction operator: {}",
                op
            ))),
        }
    }

    fn seq_value_to_f64(v: &Value) -> Option<f64> {
        match v {
            Value::Int(i) => Some(*i as f64),
            Value::Num(f) => Some(*f),
            Value::Rat(n, d) => {
                if *d != 0 {
                    Some(*n as f64 / *d as f64)
                } else {
                    None
                }
            }
            Value::Bool(b) => Some(if *b { 1.0 } else { 0.0 }),
            _ => None,
        }
    }

    fn seq_values_equal(a: &Value, b: &Value) -> bool {
        match (a, b) {
            (Value::Int(x), Value::Int(y)) => x == y,
            (Value::Str(x), Value::Str(y)) => x == y,
            (Value::Bool(x), Value::Bool(y)) => x == y,
            _ => {
                if let (Some(fa), Some(fb)) = (Self::seq_value_to_f64(a), Self::seq_value_to_f64(b))
                {
                    (fa - fb).abs() < 1e-12
                } else {
                    false
                }
            }
        }
    }

    fn eval_sequence(
        &mut self,
        left: Value,
        right: Value,
        exclusive: bool,
    ) -> Result<Value, RuntimeError> {
        let seeds_raw = Self::value_to_list(&left);
        if seeds_raw.is_empty() {
            return Ok(Value::Array(vec![]));
        }

        // Separate seed values from generator closure
        let mut seeds: Vec<Value> = Vec::new();
        let mut generator: Option<Value> = None;
        for v in &seeds_raw {
            if matches!(v, Value::Sub { .. }) {
                generator = Some(v.clone());
            } else {
                seeds.push(v.clone());
            }
        }
        if seeds.is_empty() {
            // All items were closures; use the closure as a zero-arg generator
            // e.g. { 3+2 } ... *
            // treat as if seeds = [] and generator produces values from scratch
        }

        // Parse endpoint: could be scalar, Inf/Whatever (None), closure (predicate),
        // regex, or array (first is limit, rest are extra)
        enum EndpointKind {
            Value(Value),
            Closure(Value),
            Regex(String),
        }
        let (endpoint, endpoint_kind, extra_rhs) = match &right {
            Value::Num(f) if f.is_infinite() => (None, None, vec![]),
            Value::Array(items) => {
                if items.is_empty() {
                    return Err(RuntimeError::new(
                        "Cannot use an empty list as endpoint of a sequence".to_string(),
                    ));
                }
                let first = &items[0];
                let rest: Vec<Value> = items[1..].to_vec();
                match first {
                    Value::Num(f) if f.is_infinite() => (None, None, rest),
                    Value::Sub { .. } => (
                        Some(first.clone()),
                        Some(EndpointKind::Closure(first.clone())),
                        rest,
                    ),
                    Value::Regex(pat) => (
                        Some(first.clone()),
                        Some(EndpointKind::Regex(pat.clone())),
                        rest,
                    ),
                    _ => (
                        Some(first.clone()),
                        Some(EndpointKind::Value(first.clone())),
                        rest,
                    ),
                }
            }
            Value::Sub { .. } => (
                Some(right.clone()),
                Some(EndpointKind::Closure(right.clone())),
                vec![],
            ),
            Value::Regex(pat) => (
                Some(right.clone()),
                Some(EndpointKind::Regex(pat.clone())),
                vec![],
            ),
            other => (
                Some(other.clone()),
                Some(EndpointKind::Value(other.clone())),
                vec![],
            ),
        };

        // Determine generation mode
        enum SeqMode {
            Arithmetic(f64),
            Geometric(f64),
            Closure,
        }

        let mode = if generator.is_some() {
            SeqMode::Closure
        } else if seeds.len() >= 2 {
            // Check if all seeds are numeric
            let floats: Vec<f64> = seeds.iter().filter_map(Self::seq_value_to_f64).collect();
            if floats.len() == seeds.len() {
                // Use the last few elements to determine pattern
                // First check: are ALL diffs equal? (pure arithmetic)
                let diffs: Vec<f64> = floats.windows(2).map(|w| w[1] - w[0]).collect();
                let all_diffs_equal = diffs.windows(2).all(|w| (w[0] - w[1]).abs() < 1e-12);

                if all_diffs_equal {
                    SeqMode::Arithmetic(diffs[diffs.len() - 1])
                } else if floats.len() >= 3 {
                    // Check if last 3 values form a geometric progression
                    let last3 = &floats[floats.len() - 3..];
                    let r1 = if last3[0].abs() > 1e-15 {
                        last3[1] / last3[0]
                    } else {
                        f64::NAN
                    };
                    let r2 = if last3[1].abs() > 1e-15 {
                        last3[2] / last3[1]
                    } else {
                        f64::NAN
                    };
                    if !r1.is_nan() && !r2.is_nan() && (r1 - r2).abs() < 1e-12 {
                        SeqMode::Geometric(r2)
                    } else {
                        // Use last difference as arithmetic step
                        SeqMode::Arithmetic(diffs[diffs.len() - 1])
                    }
                } else {
                    SeqMode::Arithmetic(diffs[diffs.len() - 1])
                }
            } else {
                // String constant sequences: check if all seeds are equal strings
                let all_str = seeds.iter().all(|v| matches!(v, Value::Str(_)));
                if all_str && seeds.len() >= 2 {
                    let all_same = seeds.windows(2).all(|w| {
                        if let (Value::Str(a), Value::Str(b)) = (&w[0], &w[1]) {
                            a == b
                        } else {
                            false
                        }
                    });
                    if all_same {
                        SeqMode::Arithmetic(0.0) // constant string sequence
                    } else {
                        SeqMode::Arithmetic(1.0) // default
                    }
                } else {
                    SeqMode::Arithmetic(1.0)
                }
            }
        } else if seeds.len() == 1 {
            // Single seed: determine direction from endpoint
            if let Some(ref ep) = endpoint {
                if let (Some(sv), Some(ev)) = (
                    Self::seq_value_to_f64(&seeds[0]),
                    Self::seq_value_to_f64(ep),
                ) {
                    if ev >= sv {
                        SeqMode::Arithmetic(1.0)
                    } else {
                        SeqMode::Arithmetic(-1.0)
                    }
                } else {
                    SeqMode::Arithmetic(1.0)
                }
            } else {
                SeqMode::Arithmetic(1.0) // infinite, default increasing
            }
        } else {
            // No seeds (only had a closure)
            SeqMode::Closure
        };

        // Check for "wrong side" sequences: arithmetic with endpoint on wrong side
        if let SeqMode::Arithmetic(step) = &mode
            && let Some(ref ep) = endpoint
            && let Some(ep_f) = Self::seq_value_to_f64(ep)
            && seeds.len() >= 2
        {
            let last_f = Self::seq_value_to_f64(seeds.last().unwrap()).unwrap_or(0.0);
            if (*step > 0.0 && ep_f < last_f) || (*step < 0.0 && ep_f > last_f) {
                let mut result = Vec::new();
                for s in &seeds {
                    if let Some(sv) = Self::seq_value_to_f64(s) {
                        if *step > 0.0 && sv > ep_f {
                            break;
                        }
                        if *step < 0.0 && sv < ep_f {
                            break;
                        }
                    }
                    if exclusive && Self::seq_values_equal(s, ep) {
                        break;
                    }
                    result.push(s.clone());
                }
                result.extend(extra_rhs);
                return Ok(Value::Array(result));
            }
        }

        // Check for "wrong side" on geometric sequences too
        if let SeqMode::Geometric(ratio) = &mode
            && let Some(ref ep) = endpoint
            && let Some(ep_f) = Self::seq_value_to_f64(ep)
            && seeds.len() >= 2
        {
            let last_f = Self::seq_value_to_f64(seeds.last().unwrap()).unwrap_or(0.0);
            if *ratio > 0.0 {
                // Non-alternating: check direction
                if (*ratio > 1.0 && ep_f < last_f)
                    || (*ratio < 1.0 && *ratio > 0.0 && ep_f > last_f)
                {
                    let mut result = Vec::new();
                    for s in &seeds {
                        if let Some(sv) = Self::seq_value_to_f64(s) {
                            if *ratio > 1.0 && sv > ep_f {
                                break;
                            }
                            if *ratio < 1.0 && sv < ep_f {
                                break;
                            }
                        }
                        if exclusive && Self::seq_values_equal(s, ep) {
                            break;
                        }
                        result.push(s.clone());
                    }
                    result.extend(extra_rhs);
                    return Ok(Value::Array(result));
                }
            } else {
                // Alternating (negative ratio): check |value| against |endpoint|
                let abs_last = last_f.abs();
                let abs_ep = ep_f.abs();
                if ratio.abs() > 1.0 && abs_ep < abs_last {
                    // Check if endpoint is theoretically reachable
                    let first_f = Self::seq_value_to_f64(&seeds[0]).unwrap_or(1.0).abs();
                    let reachable = if first_f > 1e-15 {
                        let log_val = (abs_ep / first_f).ln() / ratio.abs().ln();
                        (log_val - log_val.round()).abs() < 1e-9 && log_val >= 0.0
                    } else {
                        false
                    };
                    if !reachable {
                        let mut result = Vec::new();
                        for s in &seeds {
                            if let Some(sv) = Self::seq_value_to_f64(s)
                                && sv.abs() > abs_ep
                            {
                                break;
                            }
                            if exclusive && Self::seq_values_equal(s, ep) {
                                break;
                            }
                            result.push(s.clone());
                        }
                        result.extend(extra_rhs);
                        return Ok(Value::Array(result));
                    }
                }
            }
        }

        // For closure/regex endpoints, check if any seed already satisfies the predicate
        if !seeds.is_empty() {
            if let Some(EndpointKind::Closure(ref closure_val)) = endpoint_kind
                && let Value::Sub {
                    params,
                    body,
                    env: cenv,
                    ..
                } = closure_val
            {
                // Determine arity from params or placeholders
                let placeholders = collect_placeholders(body);
                let arity = if !params.is_empty() {
                    params.len()
                } else if !placeholders.is_empty() {
                    placeholders.len()
                } else {
                    1
                };

                for (i, _) in seeds.iter().enumerate() {
                    let saved = self.env.clone();
                    for (k, v) in cenv {
                        self.env.insert(k.clone(), v.clone());
                    }

                    // Collect the appropriate number of previous values up to position i+1
                    let args: Vec<Value> = if i + 1 < arity {
                        // Not enough values yet - pad with Nil
                        let mut args = vec![Value::Nil; arity - (i + 1)];
                        args.extend(seeds[..=i].iter().cloned());
                        args
                    } else {
                        // Take the last 'arity' values up to position i
                        seeds[i + 1 - arity..=i].to_vec()
                    };

                    // Bind parameters
                    for (j, param) in params.iter().enumerate() {
                        if j < args.len() {
                            self.env.insert(param.clone(), args[j].clone());
                        }
                    }

                    // Bind placeholders
                    for (j, ph) in placeholders.iter().enumerate() {
                        if j < args.len() {
                            self.env.insert(ph.clone(), args[j].clone());
                        }
                    }

                    // Bind $_ to last arg
                    if let Some(last_arg) = args.last() {
                        self.env.insert("_".to_string(), last_arg.clone());
                    }

                    let predicate_result = self.eval_block_value(body)?;
                    self.env = saved;
                    if predicate_result.truthy() {
                        let end = if exclusive { i } else { i + 1 };
                        let mut result: Vec<Value> = seeds[..end].to_vec();
                        result.extend(extra_rhs);
                        return Ok(Value::Array(result));
                    }
                }
            }
            if let Some(EndpointKind::Regex(ref pat)) = endpoint_kind {
                for (i, seed) in seeds.iter().enumerate() {
                    let s = seed.to_string_value();
                    if self.regex_find_first(pat, &s).is_some() {
                        let end = if exclusive { i } else { i + 1 };
                        let mut result: Vec<Value> = seeds[..end].to_vec();
                        result.extend(extra_rhs);
                        return Ok(Value::Array(result));
                    }
                }
            }
        }

        // Check if any seed matches the endpoint — for geometric/alternating sequences,
        // the endpoint may match an earlier seed, not just the last one
        if !seeds.is_empty()
            && let Some(ref ep) = endpoint
            && matches!(endpoint_kind, Some(EndpointKind::Value(_)))
        {
            // Check last seed first (most common case)
            let last_seed = seeds.last().unwrap();
            if Self::seq_values_equal(last_seed, ep) {
                if exclusive {
                    let mut end = seeds.len();
                    while end > 0 && Self::seq_values_equal(&seeds[end - 1], ep) {
                        end -= 1;
                    }
                    let mut result: Vec<Value> = seeds[..end].to_vec();
                    result.extend(extra_rhs);
                    return Ok(Value::Array(result));
                } else {
                    let mut result = seeds.clone();
                    result.extend(extra_rhs);
                    return Ok(Value::Array(result));
                }
            }
            // For geometric/alternating: also check if the endpoint matches earlier seeds
            if matches!(mode, SeqMode::Geometric(_)) {
                for (i, s) in seeds.iter().enumerate() {
                    if Self::seq_values_equal(s, ep) {
                        if exclusive {
                            let mut result: Vec<Value> = seeds[..i].to_vec();
                            result.extend(extra_rhs);
                            return Ok(Value::Array(result));
                        } else {
                            let mut result: Vec<Value> = seeds[..=i].to_vec();
                            result.extend(extra_rhs);
                            return Ok(Value::Array(result));
                        }
                    }
                }
            }
        }

        // Generate values
        let mut result: Vec<Value> = seeds.clone();
        let max_gen = if endpoint.is_some() { 10000 } else { 1000 };
        let is_string_seq = !seeds.is_empty() && matches!(seeds.last(), Some(Value::Str(_)));

        for _ in 0..max_gen {
            let next = match &mode {
                SeqMode::Closure => {
                    let genfn = generator.as_ref().unwrap();
                    if let Value::Sub {
                        params, body, env, ..
                    } = genfn
                    {
                        let saved = self.env.clone();
                        for (k, v) in env {
                            self.env.insert(k.clone(), v.clone());
                        }

                        // Determine arity from params or placeholders
                        let placeholders = collect_placeholders(body);
                        let arity = if !params.is_empty() {
                            params.len()
                        } else if !placeholders.is_empty() {
                            placeholders.len()
                        } else {
                            1
                        };

                        // Collect the appropriate number of previous values
                        let result_len = result.len();
                        let args: Vec<Value> = if result_len == 0 {
                            vec![Value::Nil; arity]
                        } else if result_len < arity {
                            // Not enough values yet - pad with Nil
                            let mut args = vec![Value::Nil; arity - result_len];
                            args.extend(result.iter().cloned());
                            args
                        } else {
                            // Take the last 'arity' values
                            result[result_len - arity..].to_vec()
                        };

                        // Bind parameters
                        for (i, param) in params.iter().enumerate() {
                            if i < args.len() {
                                self.env.insert(param.clone(), args[i].clone());
                            }
                        }

                        // Bind placeholders
                        for (i, ph) in placeholders.iter().enumerate() {
                            if i < args.len() {
                                self.env.insert(ph.clone(), args[i].clone());
                            }
                        }

                        // Bind $_ to last arg
                        if let Some(last_arg) = args.last() {
                            self.env.insert("_".to_string(), last_arg.clone());
                        }

                        let val = self.eval_block_value(body)?;
                        self.env = saved;
                        val
                    } else {
                        break;
                    }
                }
                SeqMode::Arithmetic(step) => {
                    if is_string_seq {
                        if *step == 0.0 {
                            // String constant sequence
                            result.last().unwrap().clone()
                        } else {
                            // String succession sequence: use .succ
                            let last = result.last().unwrap();
                            if let Value::Str(s) = last {
                                Value::Str(Self::string_succ(s))
                            } else {
                                break;
                            }
                        }
                    } else {
                        let last = result.last().unwrap();
                        Self::seq_add(last, *step)
                    }
                }
                SeqMode::Geometric(ratio) => {
                    let last = result.last().unwrap();
                    Self::seq_mul(last, *ratio)
                }
            };

            // Check endpoint
            if let Some(ref epk) = endpoint_kind {
                match epk {
                    EndpointKind::Closure(closure_val) => {
                        // Call the closure with the generated value(s) as predicate
                        if let Value::Sub {
                            params,
                            body,
                            env: cenv,
                            ..
                        } = closure_val
                        {
                            let saved = self.env.clone();
                            for (k, v) in cenv {
                                self.env.insert(k.clone(), v.clone());
                            }

                            // Determine arity from params or placeholders
                            let placeholders = collect_placeholders(body);
                            let arity = if !params.is_empty() {
                                params.len()
                            } else if !placeholders.is_empty() {
                                placeholders.len()
                            } else {
                                1
                            };

                            // Collect the appropriate number of values including the new one
                            let result_len = result.len();
                            let args: Vec<Value> = if result_len == 0 {
                                vec![next.clone(); arity]
                            } else if result_len < arity - 1 {
                                // Not enough values yet - pad with Nil
                                let mut args = vec![Value::Nil; arity - result_len - 1];
                                args.extend(result.iter().cloned());
                                args.push(next.clone());
                                args
                            } else {
                                // Take the last 'arity-1' values plus the new one
                                let mut args = result[result_len - (arity - 1)..].to_vec();
                                args.push(next.clone());
                                args
                            };

                            // Bind parameters
                            for (i, param) in params.iter().enumerate() {
                                if i < args.len() {
                                    self.env.insert(param.clone(), args[i].clone());
                                }
                            }

                            // Bind placeholders
                            for (i, ph) in placeholders.iter().enumerate() {
                                if i < args.len() {
                                    self.env.insert(ph.clone(), args[i].clone());
                                }
                            }

                            // Bind $_ to last arg
                            if let Some(last_arg) = args.last() {
                                self.env.insert("_".to_string(), last_arg.clone());
                            }

                            let predicate_result = self.eval_block_value(body)?;
                            self.env = saved;
                            if predicate_result.truthy() {
                                if !exclusive {
                                    result.push(next);
                                }
                                break;
                            }
                        }
                    }
                    EndpointKind::Regex(pat) => {
                        // Match regex against the string value of next
                        let s = next.to_string_value();
                        if self.regex_find_first(pat, &s).is_some() {
                            if !exclusive {
                                result.push(next);
                            }
                            break;
                        }
                    }
                    EndpointKind::Value(ep) => {
                        if Self::seq_values_equal(&next, ep) {
                            if !exclusive {
                                result.push(next);
                            }
                            break;
                        }
                        // Check if we went past the endpoint
                        if let (Some(nf), Some(ef)) =
                            (Self::seq_value_to_f64(&next), Self::seq_value_to_f64(ep))
                        {
                            match &mode {
                                SeqMode::Arithmetic(step) => {
                                    if *step > 0.0 && nf > ef {
                                        break;
                                    }
                                    if *step < 0.0 && nf < ef {
                                        break;
                                    }
                                }
                                SeqMode::Geometric(ratio) => {
                                    if *ratio > 0.0 {
                                        // Non-alternating geometric
                                        if *ratio > 1.0 && nf > ef {
                                            break;
                                        }
                                        if *ratio > 0.0 && *ratio < 1.0 && nf < ef {
                                            break;
                                        }
                                    } else {
                                        // Alternating geometric: check if endpoint is reachable
                                        let first_f = if !seeds.is_empty() {
                                            Self::seq_value_to_f64(&seeds[0]).unwrap_or(1.0).abs()
                                        } else {
                                            1.0
                                        };
                                        let reachable = if first_f > 1e-15 && ef.abs() > 1e-15 {
                                            let log_val =
                                                (ef.abs() / first_f).ln() / ratio.abs().ln();
                                            (log_val - log_val.round()).abs() < 1e-9
                                                && log_val >= -1e-9
                                        } else {
                                            ef.abs() < 1e-15
                                        };
                                        if !reachable {
                                            if ratio.abs() > 1.0 && nf.abs() > ef.abs() {
                                                break;
                                            }
                                            if ratio.abs() < 1.0
                                                && ratio.abs() > 0.0
                                                && nf.abs() < ef.abs()
                                            {
                                                break;
                                            }
                                        }
                                    }
                                }
                                SeqMode::Closure => {
                                    // For closures, don't auto-stop based on direction
                                }
                            }
                        }
                    }
                }
            }

            result.push(next);
        }

        result.extend(extra_rhs);
        Ok(Value::Array(result))
    }

    // Add step to a sequence value, preserving type where possible
    // Compute the successor of a string (increment the last character, carrying over)
    fn string_succ(s: &str) -> String {
        if s.is_empty() {
            return String::new();
        }
        let mut chars: Vec<char> = s.chars().collect();
        // Carry-over increment from the last character
        let mut carry = true;
        for ch in chars.iter_mut().rev() {
            if !carry {
                break;
            }
            if ch.is_ascii_lowercase() {
                if *ch == 'z' {
                    *ch = 'a';
                } else {
                    *ch = (*ch as u8 + 1) as char;
                    carry = false;
                }
            } else if ch.is_ascii_uppercase() {
                if *ch == 'Z' {
                    *ch = 'A';
                } else {
                    *ch = (*ch as u8 + 1) as char;
                    carry = false;
                }
            } else if ch.is_ascii_digit() {
                if *ch == '9' {
                    *ch = '0';
                } else {
                    *ch = (*ch as u8 + 1) as char;
                    carry = false;
                }
            } else {
                *ch = char::from_u32(*ch as u32 + 1).unwrap_or(*ch);
                carry = false;
            }
        }
        if carry {
            // All characters carried over, prepend appropriate char
            let first = chars[0];
            let prefix = if first.is_ascii_lowercase() {
                'a'
            } else if first.is_ascii_uppercase() {
                'A'
            } else if first.is_ascii_digit() {
                '1'
            } else {
                first
            };
            chars.insert(0, prefix);
        }
        chars.into_iter().collect()
    }

    fn seq_add(val: &Value, step: f64) -> Value {
        match val {
            Value::Int(i) => {
                if step == step.floor() && step.abs() < i64::MAX as f64 {
                    Value::Int(*i + step as i64)
                } else {
                    Value::Num(*i as f64 + step)
                }
            }
            Value::Num(f) => Value::Num(*f + step),
            Value::Rat(n, d) => {
                if *d != 0 && step == step.floor() && step.abs() < i64::MAX as f64 {
                    make_rat(*n + step as i64 * *d, *d)
                } else {
                    Value::Num(*n as f64 / *d as f64 + step)
                }
            }
            _ => Value::Num(Self::seq_value_to_f64(val).unwrap_or(0.0) + step),
        }
    }

    // Multiply a sequence value by ratio, preserving type where possible
    fn seq_mul(val: &Value, ratio: f64) -> Value {
        match val {
            Value::Int(i) => {
                let result = *i as f64 * ratio;
                if ratio == ratio.floor() && result.abs() < i64::MAX as f64 {
                    Value::Int(result as i64)
                } else {
                    Value::Num(result)
                }
            }
            Value::Num(f) => Value::Num(*f * ratio),
            Value::Rat(n, d) => {
                if *d != 0 && ratio == ratio.floor() && ratio.abs() < i64::MAX as f64 {
                    make_rat(*n * ratio as i64, *d)
                } else {
                    Value::Num(*n as f64 / *d as f64 * ratio)
                }
            }
            _ => Value::Num(Self::seq_value_to_f64(val).unwrap_or(0.0) * ratio),
        }
    }

    pub(crate) fn value_to_list(val: &Value) -> Vec<Value> {
        match val {
            Value::Array(items) => items.clone(),
            Value::Hash(items) => items
                .iter()
                .map(|(k, v)| Value::Pair(k.clone(), Box::new(v.clone())))
                .collect(),
            Value::Range(a, b) => (*a..=*b).map(Value::Int).collect(),
            Value::RangeExcl(a, b) => (*a..*b).map(Value::Int).collect(),
            Value::RangeExclStart(a, b) => (*a + 1..=*b).map(Value::Int).collect(),
            Value::RangeExclBoth(a, b) => (*a + 1..*b).map(Value::Int).collect(),
            Value::Set(items) => items.iter().map(|s| Value::Str(s.clone())).collect(),
            Value::Bag(items) => items
                .iter()
                .map(|(k, v)| Value::Pair(k.clone(), Box::new(Value::Int(*v))))
                .collect(),
            Value::Mix(items) => items
                .iter()
                .map(|(k, v)| Value::Pair(k.clone(), Box::new(Value::Num(*v))))
                .collect(),
            Value::Slip(items) => items.clone(),
            Value::Nil => vec![],
            other => vec![other.clone()],
        }
    }

    pub(crate) fn eval_hyper_op(
        op: &str,
        left: &Value,
        right: &Value,
        dwim_left: bool,
        dwim_right: bool,
    ) -> Result<Value, RuntimeError> {
        let left_list = Self::value_to_list(left);
        let right_list = Self::value_to_list(right);
        let left_len = left_list.len();
        let right_len = right_list.len();

        if left_len == 0 && right_len == 0 {
            return Ok(Value::Array(Vec::new()));
        }

        // Determine result length based on DWIM semantics
        let result_len = if !dwim_left && !dwim_right {
            // >>op<< strict: lengths must match
            if left_len != right_len {
                return Err(RuntimeError::new(format!(
                    "Non-dwimmy hyper operator: left has {} elements, right has {}",
                    left_len, right_len
                )));
            }
            left_len
        } else if dwim_left && dwim_right {
            // <<op>> both DWIM: shorter extends to match longer
            std::cmp::max(left_len, right_len)
        } else if dwim_right {
            // >>op>> right extends to match left
            left_len
        } else {
            // <<op<< left extends to match right
            right_len
        };

        let mut results = Vec::with_capacity(result_len);
        for i in 0..result_len {
            let l = if left_len == 0 {
                &Value::Int(0)
            } else {
                &left_list[i % left_len]
            };
            let r = if right_len == 0 {
                &Value::Int(0)
            } else {
                &right_list[i % right_len]
            };
            results.push(Self::apply_reduction_op(op, l, r)?);
        }
        Ok(Value::Array(results))
    }

    fn bind_function_args(
        &mut self,
        param_defs: &[ParamDef],
        params: &[String],
        args: &[Expr],
    ) -> Result<(), RuntimeError> {
        if param_defs.is_empty() {
            // Legacy path: just bind by position
            for (i, param) in params.iter().enumerate() {
                if let Some(arg) = args.get(i)
                    && let Ok(value) = self.eval_expr(arg)
                {
                    self.env.insert(param.clone(), value);
                }
            }
            return Ok(());
        }
        let mut positional_idx = 0usize;
        for pd in param_defs {
            if pd.slurpy {
                // Slurpy collects remaining positional args
                let mut items = Vec::new();
                while positional_idx < args.len() {
                    if let Ok(value) = self.eval_expr(&args[positional_idx]) {
                        items.push(value);
                    }
                    positional_idx += 1;
                }
                if !pd.name.is_empty() {
                    self.env.insert(pd.name.clone(), Value::Array(items));
                }
            } else if pd.named {
                // Named params: look for matching AssignExpr in args
                let mut found = false;
                for arg in args {
                    if let Expr::AssignExpr { name, expr } = arg
                        && *name == pd.name
                    {
                        let value = self.eval_expr(expr)?;
                        if let Some(constraint) = &pd.type_constraint
                            && !self.type_matches_value(constraint, &value)
                        {
                            return Err(RuntimeError::new(format!(
                                "Type check failed for {}: expected {}, got {}",
                                pd.name,
                                constraint,
                                Self::value_type_name(&value)
                            )));
                        }
                        if !pd.name.is_empty() {
                            self.env.insert(pd.name.clone(), value);
                        }
                        found = true;
                        break;
                    }
                }
                if !found && let Some(default_expr) = &pd.default {
                    let value = self.eval_expr(default_expr)?;
                    if !pd.name.is_empty() {
                        self.env.insert(pd.name.clone(), value);
                    }
                }
            } else {
                // Positional param
                if positional_idx < args.len() {
                    let value = self.eval_expr(&args[positional_idx])?;
                    if let Some(constraint) = &pd.type_constraint
                        && !self.type_matches_value(constraint, &value)
                    {
                        return Err(RuntimeError::new(format!(
                            "Type check failed for {}: expected {}, got {}",
                            pd.name,
                            constraint,
                            Self::value_type_name(&value)
                        )));
                    }
                    if !pd.name.is_empty() {
                        self.env.insert(pd.name.clone(), value);
                    }
                    positional_idx += 1;
                } else if let Some(default_expr) = &pd.default {
                    let value = self.eval_expr(default_expr)?;
                    if !pd.name.is_empty() {
                        self.env.insert(pd.name.clone(), value);
                    }
                }
            }
        }
        Ok(())
    }

    /// Bind function arguments from pre-evaluated values (for VM compiled function dispatch).
    pub(crate) fn bind_function_args_values(
        &mut self,
        param_defs: &[ParamDef],
        params: &[String],
        args: &[Value],
    ) -> Result<(), RuntimeError> {
        if param_defs.is_empty() {
            // Legacy path: just bind by position
            for (i, param) in params.iter().enumerate() {
                if let Some(value) = args.get(i) {
                    self.env.insert(param.clone(), value.clone());
                }
            }
            return Ok(());
        }
        let mut positional_idx = 0usize;
        for pd in param_defs {
            if pd.slurpy {
                let mut items = Vec::new();
                while positional_idx < args.len() {
                    items.push(args[positional_idx].clone());
                    positional_idx += 1;
                }
                if !pd.name.is_empty() {
                    self.env.insert(pd.name.clone(), Value::Array(items));
                }
            } else if pd.named {
                // Named params not supported with pre-evaluated values, use default
                if let Some(default_expr) = &pd.default {
                    let value = self.eval_expr(default_expr)?;
                    if !pd.name.is_empty() {
                        self.env.insert(pd.name.clone(), value);
                    }
                }
            } else {
                // Positional param
                if positional_idx < args.len() {
                    let value = args[positional_idx].clone();
                    if let Some(constraint) = &pd.type_constraint
                        && !self.type_matches_value(constraint, &value)
                    {
                        return Err(RuntimeError::new(format!(
                            "Type check failed for {}: expected {}, got {}",
                            pd.name,
                            constraint,
                            Self::value_type_name(&value)
                        )));
                    }
                    if !pd.name.is_empty() {
                        self.env.insert(pd.name.clone(), value);
                    }
                    positional_idx += 1;
                } else if let Some(default_expr) = &pd.default {
                    let value = self.eval_expr(default_expr)?;
                    if !pd.name.is_empty() {
                        self.env.insert(pd.name.clone(), value);
                    }
                }
            }
        }
        Ok(())
    }

    /// Evaluate a given block, returning the value from the matching when branch.
    fn eval_given_body_with_topic(
        &mut self,
        topic_val: Value,
        body: &[Stmt],
    ) -> Result<Value, RuntimeError> {
        let saved_topic = self.env.get("_").cloned();
        self.env.insert("_".to_string(), topic_val);
        let saved_when = self.when_matched;
        self.when_matched = false;
        let mut last = Value::Nil;
        for stmt in body {
            match self.exec_stmt(stmt) {
                Ok(()) => {}
                Err(e) if e.is_succeed => {
                    if let Some(v) = e.return_value {
                        last = v;
                    }
                    self.when_matched = true;
                    break;
                }
                Err(e) => {
                    self.when_matched = saved_when;
                    if let Some(v) = saved_topic {
                        self.env.insert("_".to_string(), v);
                    } else {
                        self.env.remove("_");
                    }
                    return Err(e);
                }
            }
            if self.when_matched || self.halted {
                break;
            }
        }
        self.when_matched = saved_when;
        if let Some(v) = saved_topic {
            self.env.insert("_".to_string(), v);
        } else {
            self.env.remove("_");
        }
        Ok(last)
    }

    fn eval_given_value(&mut self, topic: &Expr, body: &[Stmt]) -> Result<Value, RuntimeError> {
        let topic_val = self.eval_expr(topic)?;
        self.eval_given_body_with_topic(topic_val, body)
    }

    pub(crate) fn eval_given_with_value(
        &mut self,
        topic: Value,
        body: &[Stmt],
    ) -> Result<Value, RuntimeError> {
        self.eval_given_body_with_topic(topic, body)
    }

    fn eval_block_value(&mut self, body: &[Stmt]) -> Result<Value, RuntimeError> {
        let (enter_ph, leave_ph, body_main) = self.split_block_phasers(body);
        self.run_block_raw(&enter_ph)?;
        let mut last = Value::Nil;
        let mut result = Ok(());
        for stmt in &body_main {
            match stmt {
                Stmt::Return(expr) => {
                    result = Err(RuntimeError::return_val(self.eval_expr(expr)?));
                    break;
                }
                Stmt::Expr(expr) => match self.eval_expr(expr) {
                    Ok(v) => last = v,
                    Err(e) => {
                        result = Err(e);
                        break;
                    }
                },
                Stmt::Given {
                    topic,
                    body: given_body,
                } => match self.eval_given_value(topic, given_body) {
                    Ok(v) => last = v,
                    Err(e) => {
                        result = Err(e);
                        break;
                    }
                },
                _ => {
                    if let Err(e) = self.exec_stmt(stmt) {
                        result = Err(e);
                        break;
                    }
                }
            }
            if self.halted {
                break;
            }
        }
        let leave_res = self.run_block_raw(&leave_ph);
        if let Err(e) = leave_res
            && result.is_ok()
        {
            return Err(e);
        }
        match result {
            Ok(()) => Ok(last),
            Err(e) => Err(e),
        }
    }

    /// Evaluate a `do if/elsif/else` chain, returning Empty (empty Slip)
    /// when no branch executes, per S04 specification.
    fn eval_do_if(
        &mut self,
        cond: &Expr,
        then_branch: &[Stmt],
        else_branch: &[Stmt],
    ) -> Result<Value, RuntimeError> {
        if self.eval_expr(cond)?.truthy() {
            return self.eval_block_value(then_branch);
        }
        if else_branch.is_empty() {
            // No branch executed; return Empty (empty Slip)
            return Ok(Value::Slip(vec![]));
        }
        // elsif chain: else_branch is [Stmt::If { ... }]
        if else_branch.len() == 1
            && let Stmt::If {
                cond: inner_cond,
                then_branch: inner_then,
                else_branch: inner_else,
            } = &else_branch[0]
        {
            return self.eval_do_if(inner_cond, inner_then, inner_else);
        }
        // Plain else branch
        self.eval_block_value(else_branch)
    }

    fn eval_eval_string(&mut self, code: &str) -> Result<Value, RuntimeError> {
        let trimmed = code.trim();
        // Handle angle-bracket word lists: <a b c>, ~<a b>, +<a b>, ?<a b>
        let (prefix, rest) = if let Some(pos) = trimmed.find('<') {
            (trimmed.chars().next().unwrap_or(' '), &trimmed[pos..])
        } else {
            (' ', trimmed)
        };
        let start = rest.find('<');
        let end = rest.rfind('>');
        if (prefix != ' ' || (start.is_some() && trimmed.starts_with('<')))
            && let (Some(s), Some(e)) = (start, end)
        {
            let inner = &rest[s + 1..e];
            let words: Vec<&str> = inner.split_whitespace().collect();
            return Ok(match prefix {
                '~' => Value::Str(words.join(" ")),
                '+' => Value::Int(words.len() as i64),
                '?' => Value::Bool(!words.is_empty()),
                _ => Value::Str(words.join(" ")),
            });
        }
        // General case: parse and evaluate as Raku code
        let mut lexer = Lexer::new(trimmed);
        let mut tokens = Vec::new();
        loop {
            let token = lexer.next_token();
            let end = matches!(token.kind, TokenKind::Eof);
            tokens.push(token);
            if end {
                break;
            }
        }
        let mut parser = Parser::new(tokens);
        match parser.parse_program() {
            Ok(stmts) => self.eval_block_value(&stmts),
            Err(e) => Err(e),
        }
    }

    pub(crate) fn format_sprintf(fmt: &str, arg: Option<&Value>) -> String {
        let mut chars = fmt.chars().peekable();
        let mut out = String::new();
        while let Some(c) = chars.next() {
            if c != '%' {
                out.push(c);
                continue;
            }
            if chars.peek() == Some(&'%') {
                chars.next();
                out.push('%');
                continue;
            }
            let mut flags = String::new();
            while let Some(f) = chars.peek().copied() {
                if f == '-' || f == '+' || f == ' ' || f == '#' {
                    flags.push(f);
                    chars.next();
                } else {
                    break;
                }
            }
            let mut width = String::new();
            while let Some(d) = chars.peek().copied() {
                if d.is_ascii_digit() {
                    width.push(d);
                    chars.next();
                } else {
                    break;
                }
            }
            let mut precision = String::new();
            if chars.peek() == Some(&'.') {
                chars.next();
                while let Some(d) = chars.peek().copied() {
                    if d.is_ascii_digit() {
                        precision.push(d);
                        chars.next();
                    } else {
                        break;
                    }
                }
            }
            let spec = chars.next().unwrap_or('s');
            let width_num = width.parse::<usize>().unwrap_or(0);
            let prec_num = precision.parse::<usize>().ok();
            let zero_pad = width.starts_with('0') && !flags.contains('-');
            let left_align = flags.contains('-');
            let plus_sign = flags.contains('+');
            let hash_flag = flags.contains('#');
            let int_val = || match arg {
                Some(Value::Int(i)) => *i,
                Some(Value::Num(f)) => *f as i64,
                Some(Value::Str(s)) => s.trim().parse::<i64>().unwrap_or(0),
                Some(Value::Bool(b)) => {
                    if *b {
                        1
                    } else {
                        0
                    }
                }
                _ => 0,
            };
            let float_val = || match arg {
                Some(Value::Int(i)) => *i as f64,
                Some(Value::Num(f)) => *f,
                Some(Value::Str(s)) => s.trim().parse::<f64>().unwrap_or(0.0),
                _ => 0.0,
            };
            let rendered = match spec {
                's' => match arg {
                    Some(v) => {
                        let s = v.to_string_value();
                        if let Some(p) = prec_num {
                            s[..p.min(s.len())].to_string()
                        } else {
                            s
                        }
                    }
                    _ => String::new(),
                },
                'd' | 'i' => {
                    let i = int_val();
                    if plus_sign && i >= 0 {
                        format!("+{}", i)
                    } else {
                        format!("{}", i)
                    }
                }
                'u' => format!("{}", int_val() as u64),
                'x' => {
                    let i = int_val();
                    let s = format!("{:x}", i);
                    if hash_flag { format!("0x{}", s) } else { s }
                }
                'X' => {
                    let i = int_val();
                    let s = format!("{:X}", i);
                    if hash_flag { format!("0X{}", s) } else { s }
                }
                'o' => {
                    let i = int_val();
                    let s = format!("{:o}", i);
                    if hash_flag { format!("0{}", s) } else { s }
                }
                'b' => {
                    let i = int_val();
                    let s = format!("{:b}", i);
                    if hash_flag { format!("0b{}", s) } else { s }
                }
                'B' => {
                    let i = int_val();
                    let s = format!("{:b}", i);
                    if hash_flag { format!("0B{}", s) } else { s }
                }
                'e' | 'E' => {
                    let f = float_val();
                    let p = prec_num.unwrap_or(6);
                    if spec == 'e' {
                        format!("{:.prec$e}", f, prec = p)
                    } else {
                        format!("{:.prec$E}", f, prec = p)
                    }
                }
                'f' => {
                    let f = float_val();
                    let p = prec_num.unwrap_or(6);
                    format!("{:.prec$}", f, prec = p)
                }
                'g' | 'G' => {
                    let f = float_val();
                    let p = prec_num.unwrap_or(6);
                    if f.abs() < 1e-4 || f.abs() >= 10f64.powi(p as i32) {
                        if spec == 'g' {
                            format!("{:.prec$e}", f, prec = p.saturating_sub(1))
                        } else {
                            format!("{:.prec$E}", f, prec = p.saturating_sub(1))
                        }
                    } else {
                        format!("{:.prec$}", f, prec = p.saturating_sub(1))
                    }
                }
                'c' => {
                    let i = int_val();
                    char::from_u32(i as u32)
                        .map(|c| c.to_string())
                        .unwrap_or_default()
                }
                _ => String::new(),
            };
            if width_num > rendered.len() {
                let pad = width_num - rendered.len();
                if left_align {
                    out.push_str(&rendered);
                    out.push_str(&" ".repeat(pad));
                } else {
                    let ch = if zero_pad { '0' } else { ' ' };
                    out.push_str(&ch.to_string().repeat(pad));
                    out.push_str(&rendered);
                }
            } else {
                out.push_str(&rendered);
            }
        }
        out
    }

    pub(crate) fn coerce_to_numeric(val: Value) -> Value {
        match val {
            Value::Int(_)
            | Value::Num(_)
            | Value::Rat(_, _)
            | Value::FatRat(_, _)
            | Value::Complex(_, _) => val,
            Value::Bool(b) => Value::Int(if b { 1 } else { 0 }),
            Value::Enum { value, .. } => Value::Int(value),
            Value::Str(ref s) => {
                let s = s.trim();
                if let Ok(i) = s.parse::<i64>() {
                    Value::Int(i)
                } else if let Ok(f) = s.parse::<f64>() {
                    Value::Num(f)
                } else {
                    Value::Int(0)
                }
            }
            Value::Array(items) => Value::Int(items.len() as i64),
            Value::Nil => Value::Int(0),
            _ => Value::Int(0),
        }
    }

    pub(crate) fn coerce_to_set(val: &Value) -> HashSet<String> {
        match val {
            Value::Set(s) => s.clone(),
            Value::Bag(b) => b.keys().cloned().collect(),
            Value::Mix(m) => m.keys().cloned().collect(),
            Value::Array(items) => items.iter().map(|v| v.to_string_value()).collect(),
            _ => {
                let mut s = HashSet::new();
                let sv = val.to_string_value();
                if !sv.is_empty() {
                    s.insert(sv);
                }
                s
            }
        }
    }

    pub(crate) fn coerce_numeric(left: Value, right: Value) -> (Value, Value) {
        let l = match &left {
            Value::Int(_)
            | Value::Num(_)
            | Value::Rat(_, _)
            | Value::FatRat(_, _)
            | Value::Complex(_, _) => left,
            _ => Self::coerce_to_numeric(left),
        };
        let r = match &right {
            Value::Int(_)
            | Value::Num(_)
            | Value::Rat(_, _)
            | Value::FatRat(_, _)
            | Value::Complex(_, _) => right,
            _ => Self::coerce_to_numeric(right),
        };
        (l, r)
    }

    pub(crate) fn to_rat_parts(val: &Value) -> Option<(i64, i64)> {
        match val {
            Value::Int(i) => Some((*i, 1)),
            Value::Rat(n, d) => Some((*n, *d)),
            Value::FatRat(n, d) => Some((*n, *d)),
            _ => None,
        }
    }

    pub(crate) fn to_float_value(val: &Value) -> Option<f64> {
        match val {
            Value::Num(f) => Some(*f),
            Value::Int(i) => Some(*i as f64),
            Value::Rat(n, d) => {
                if *d != 0 {
                    Some(*n as f64 / *d as f64)
                } else {
                    None
                }
            }
            Value::FatRat(n, d) => {
                if *d != 0 {
                    Some(*n as f64 / *d as f64)
                } else {
                    None
                }
            }
            Value::Complex(r, i) => {
                if *i == 0.0 {
                    Some(*r)
                } else {
                    None
                }
            }
            Value::Bool(b) => Some(if *b { 1.0 } else { 0.0 }),
            Value::Str(s) => s.parse::<f64>().ok(),
            Value::Nil => Some(0.0),
            _ => None,
        }
    }

    pub(crate) fn to_complex_parts(val: &Value) -> Option<(f64, f64)> {
        match val {
            Value::Complex(r, i) => Some((*r, *i)),
            Value::Int(n) => Some((*n as f64, 0.0)),
            Value::Num(f) => Some((*f, 0.0)),
            Value::Rat(n, d) => {
                if *d != 0 {
                    Some((*n as f64 / *d as f64, 0.0))
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    fn callframe_value(&self, depth: usize) -> Option<Value> {
        self.routine_stack
            .len()
            .checked_sub(1 + depth)
            .and_then(|idx| self.routine_stack.get(idx))
            .map(|(package, name)| {
                let mut info = HashMap::new();
                info.insert("package".to_string(), Value::Str(package.clone()));
                info.insert("name".to_string(), Value::Str(name.clone()));
                info.insert("depth".to_string(), Value::Int(depth as i64));
                Value::Hash(info)
            })
    }

    fn call_lambda_with_arg(&mut self, func: &Value, item: Value) -> Result<Value, RuntimeError> {
        if let Value::Sub {
            params, body, env, ..
        } = func
        {
            let saved_env = self.env.clone();
            for (k, v) in env {
                self.env.insert(k.clone(), v.clone());
            }
            if let Some(p) = params.first() {
                self.env.insert(p.clone(), item.clone());
            }
            let placeholders = collect_placeholders(body);
            if let Some(ph) = placeholders.first() {
                self.env.insert(ph.clone(), item.clone());
            }
            self.env.insert("_".to_string(), item.clone());
            let result = self.eval_block_value(body);
            self.env = saved_env;
            return result;
        }
        Err(RuntimeError::new("Expected callable"))
    }

    fn seconds_from_value(val: Option<Value>) -> Option<f64> {
        val.and_then(|v| Self::to_float_value(&v))
    }

    fn duration_from_seconds(secs: Option<f64>) -> Duration {
        let secs = secs.unwrap_or(0.0).max(0.0);
        Duration::from_secs_f64(secs)
    }

    fn system_time_from_value(val: Option<Value>) -> Option<SystemTime> {
        let secs = Self::seconds_from_value(val)?;
        let secs = secs.max(0.0);
        Some(UNIX_EPOCH + Duration::from_secs_f64(secs))
    }

    fn hostname() -> String {
        env::var("HOSTNAME")
            .ok()
            .or_else(|| env::var("COMPUTERNAME").ok())
            .or_else(|| {
                Command::new("hostname")
                    .output()
                    .ok()
                    .and_then(|output| String::from_utf8(output.stdout).ok())
                    .map(|s| s.trim().to_string())
            })
            .unwrap_or_else(|| "localhost".to_string())
    }

    fn resolve_host(name: &str) -> Vec<String> {
        format!("{}:0", name)
            .to_socket_addrs()
            .map(|addrs| {
                addrs
                    .map(|addr| addr.ip().to_string())
                    .filter(|ip| !ip.is_empty())
                    .collect()
            })
            .unwrap_or_default()
    }

    fn make_os_name_value(name: String, mut addresses: Vec<String>) -> Value {
        if addresses.is_empty() {
            addresses.push("127.0.0.1".to_string());
        }
        let mut info = HashMap::new();
        info.insert("name".to_string(), Value::Str(name.clone()));
        info.insert("addr".to_string(), Value::Str(addresses[0].clone()));
        info.insert("aliases".to_string(), Value::Array(Vec::new()));
        let addrs_values = addresses.into_iter().map(Value::Str).collect();
        info.insert("addrs".to_string(), Value::Array(addrs_values));
        Value::Hash(info)
    }

    fn get_login_name() -> Option<String> {
        env::var("LOGNAME")
            .ok()
            .or_else(|| env::var("USER").ok())
            .or_else(|| env::var("USERNAME").ok())
    }

    fn send_signal(pid: i64, signal: i64) -> bool {
        if pid == 0 {
            return false;
        }
        let pid_str = pid.to_string();
        #[cfg(unix)]
        {
            let mut cmd = Command::new("kill");
            cmd.arg(format!("-{}", signal));
            cmd.arg(&pid_str);
            cmd.status().map(|status| status.success()).unwrap_or(false)
        }
        #[cfg(windows)]
        {
            let mut cmd = Command::new("taskkill");
            cmd.args(["/PID", &pid_str, "/F"]);
            cmd.status().map(|status| status.success()).unwrap_or(false)
        }
        #[cfg(not(any(unix, windows)))]
        {
            false
        }
    }

    pub(crate) fn merge_junction(kind: JunctionKind, left: Value, right: Value) -> Value {
        let mut values = Vec::new();
        Self::push_junction_value(&kind, left, &mut values);
        Self::push_junction_value(&kind, right, &mut values);
        Value::Junction { kind, values }
    }

    pub(crate) fn push_junction_value(kind: &JunctionKind, value: Value, out: &mut Vec<Value>) {
        match value {
            Value::Junction {
                kind: inner_kind,
                values,
            } if &inner_kind == kind => {
                out.extend(values);
            }
            other => out.push(other),
        }
    }

    pub(crate) fn compare_values(a: &Value, b: &Value) -> i32 {
        match (a, b) {
            (Value::Int(a), Value::Int(b)) => a.cmp(b) as i32,
            (Value::Num(a), Value::Num(b)) => {
                a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal) as i32
            }
            (Value::Int(a), Value::Num(b)) => (*a as f64)
                .partial_cmp(b)
                .unwrap_or(std::cmp::Ordering::Equal)
                as i32,
            (Value::Num(a), Value::Int(b)) => {
                a.partial_cmp(&(*b as f64))
                    .unwrap_or(std::cmp::Ordering::Equal) as i32
            }
            _ => {
                if let (Some((an, ad)), Some((bn, bd))) =
                    (Self::to_rat_parts(a), Self::to_rat_parts(b))
                {
                    let lhs = an as i128 * bd as i128;
                    let rhs = bn as i128 * ad as i128;
                    return lhs.cmp(&rhs) as i32;
                }
                a.to_string_value().cmp(&b.to_string_value()) as i32
            }
        }
    }

    pub(crate) fn to_int(v: &Value) -> i64 {
        match v {
            Value::Int(i) => *i,
            Value::BigInt(n) => {
                use num_traits::ToPrimitive;
                n.to_i64()
                    .unwrap_or(if *n > num_bigint::BigInt::from(0i64) {
                        i64::MAX
                    } else {
                        i64::MIN
                    })
            }
            Value::Num(f) => *f as i64,
            Value::Rat(n, d) => {
                if *d != 0 {
                    n / d
                } else {
                    0
                }
            }
            Value::Complex(r, _) => *r as i64,
            Value::Str(s) => s.parse().unwrap_or(0),
            _ => 0,
        }
    }

    fn is_threadable_op(op: &TokenKind) -> bool {
        match op {
            TokenKind::EqEq
            | TokenKind::BangEq
            | TokenKind::Lt
            | TokenKind::Lte
            | TokenKind::Gt
            | TokenKind::Gte
            | TokenKind::EqEqEq
            | TokenKind::SmartMatch
            | TokenKind::BangTilde => true,
            TokenKind::Ident(s)
                if s == "eq"
                    || s == "ne"
                    || s == "lt"
                    || s == "le"
                    || s == "gt"
                    || s == "ge"
                    || s == "eqv" =>
            {
                true
            }
            _ => false,
        }
    }

    pub(crate) fn compare(
        left: Value,
        right: Value,
        f: fn(i32) -> bool,
    ) -> Result<Value, RuntimeError> {
        let (l, r) = Self::coerce_numeric(left, right);
        if let (Some((an, ad)), Some((bn, bd))) = (Self::to_rat_parts(&l), Self::to_rat_parts(&r))
            && (matches!(l, Value::Rat(_, _)) || matches!(r, Value::Rat(_, _)))
        {
            let lhs = an as i128 * bd as i128;
            let rhs = bn as i128 * ad as i128;
            return Ok(Value::Bool(f(lhs.cmp(&rhs) as i32)));
        }
        match (l, r) {
            (Value::Int(a), Value::Int(b)) => {
                let ord = a.cmp(&b) as i32;
                Ok(Value::Bool(f(ord)))
            }
            (Value::Num(a), Value::Num(b)) => {
                let ord = a.partial_cmp(&b).unwrap_or(std::cmp::Ordering::Equal) as i32;
                Ok(Value::Bool(f(ord)))
            }
            (Value::Int(a), Value::Num(b)) => {
                let a = a as f64;
                let ord = a.partial_cmp(&b).unwrap_or(std::cmp::Ordering::Equal) as i32;
                Ok(Value::Bool(f(ord)))
            }
            (Value::Num(a), Value::Int(b)) => {
                let b = b as f64;
                let ord = a.partial_cmp(&b).unwrap_or(std::cmp::Ordering::Equal) as i32;
                Ok(Value::Bool(f(ord)))
            }
            _ => Ok(Value::Bool(f(0))),
        }
    }
}

#[derive(Debug, Default)]
struct TestState {
    planned: Option<usize>,
    ran: usize,
    failed: usize,
    force_todo: Vec<(usize, usize)>,
}

impl TestState {
    fn new() -> Self {
        Self {
            planned: None,
            ran: 0,
            failed: 0,
            force_todo: Vec::new(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Interpreter;

    #[test]
    fn say_and_math() {
        let mut interp = Interpreter::new();
        let output = interp.run("say 1 + 2; say 3 * 4;").unwrap();
        assert_eq!(output, "3\n12\n");
    }

    #[test]
    fn variables_and_concat() {
        let mut interp = Interpreter::new();
        let output = interp
            .run("my $x = 2; $x = $x + 3; say \"hi\" ~ $x;")
            .unwrap();
        assert_eq!(output, "hi5\n");
    }

    #[test]
    fn if_else() {
        let mut interp = Interpreter::new();
        let output = interp
            .run("my $x = 1; if $x == 1 { say \"yes\"; } else { say \"no\"; }")
            .unwrap();
        assert_eq!(output, "yes\n");
    }

    #[test]
    fn while_loop() {
        let mut interp = Interpreter::new();
        let output = interp
            .run("my $x = 0; while $x < 3 { say $x; $x = $x + 1; }")
            .unwrap();
        assert_eq!(output, "0\n1\n2\n");
    }
}

// Helper functions for placeholder variable collection

fn collect_placeholders(stmts: &[Stmt]) -> Vec<String> {
    let mut names = Vec::new();
    for stmt in stmts {
        collect_ph_stmt(stmt, &mut names);
    }
    names.sort();
    names.dedup();
    names
}

fn collect_ph_stmt(stmt: &Stmt, out: &mut Vec<String>) {
    match stmt {
        Stmt::Expr(e) | Stmt::Return(e) | Stmt::Die(e) | Stmt::Take(e) => collect_ph_expr(e, out),
        Stmt::VarDecl { expr, .. } | Stmt::Assign { expr, .. } => collect_ph_expr(expr, out),
        Stmt::Say(es) | Stmt::Print(es) | Stmt::Note(es) => {
            for e in es {
                collect_ph_expr(e, out);
            }
        }
        Stmt::If {
            cond,
            then_branch,
            else_branch,
        } => {
            collect_ph_expr(cond, out);
            for s in then_branch {
                collect_ph_stmt(s, out);
            }
            for s in else_branch {
                collect_ph_stmt(s, out);
            }
        }
        Stmt::While { cond, body, .. } => {
            collect_ph_expr(cond, out);
            for s in body {
                collect_ph_stmt(s, out);
            }
        }
        Stmt::For { iterable, body, .. } => {
            collect_ph_expr(iterable, out);
            for s in body {
                collect_ph_stmt(s, out);
            }
        }
        Stmt::Loop { body, .. } => {
            for s in body {
                collect_ph_stmt(s, out);
            }
        }
        Stmt::React { body } => {
            for s in body {
                collect_ph_stmt(s, out);
            }
        }
        Stmt::Whenever { supply, body, .. } => {
            collect_ph_expr(supply, out);
            for s in body {
                collect_ph_stmt(s, out);
            }
        }
        Stmt::Block(body)
        | Stmt::Default(body)
        | Stmt::Catch(body)
        | Stmt::Control(body)
        | Stmt::RoleDecl { body, .. } => {
            for s in body {
                collect_ph_stmt(s, out);
            }
        }
        Stmt::Phaser { body, .. } => {
            for s in body {
                collect_ph_stmt(s, out);
            }
        }
        Stmt::Given { topic, body } => {
            collect_ph_expr(topic, out);
            for s in body {
                collect_ph_stmt(s, out);
            }
        }
        Stmt::When { cond, body } => {
            collect_ph_expr(cond, out);
            for s in body {
                collect_ph_stmt(s, out);
            }
        }
        Stmt::ProtoDecl { .. } => {}
        Stmt::DoesDecl { .. } => {}
        Stmt::SubsetDecl { predicate, .. } => {
            collect_ph_expr(predicate, out);
        }
        _ => {}
    }
}

fn collect_ph_expr(expr: &Expr, out: &mut Vec<String>) {
    match expr {
        Expr::Var(name) if name.starts_with('^') => {
            if !out.contains(name) {
                out.push(name.clone());
            }
        }
        Expr::Binary { left, right, .. } => {
            collect_ph_expr(left, out);
            collect_ph_expr(right, out);
        }
        Expr::Unary { expr, .. } | Expr::PostfixOp { expr, .. } => collect_ph_expr(expr, out),
        Expr::MethodCall { target, args, .. } => {
            collect_ph_expr(target, out);
            for a in args {
                collect_ph_expr(a, out);
            }
        }
        Expr::Call { args, .. } => {
            for a in args {
                collect_ph_expr(a, out);
            }
        }
        Expr::CallOn { target, args } => {
            collect_ph_expr(target, out);
            for a in args {
                collect_ph_expr(a, out);
            }
        }
        Expr::Index { target, index } => {
            collect_ph_expr(target, out);
            collect_ph_expr(index, out);
        }
        Expr::Ternary {
            cond,
            then_expr,
            else_expr,
        } => {
            collect_ph_expr(cond, out);
            collect_ph_expr(then_expr, out);
            collect_ph_expr(else_expr, out);
        }
        Expr::AssignExpr { expr, .. } | Expr::Exists(expr) => collect_ph_expr(expr, out),
        Expr::ArrayLiteral(es) | Expr::StringInterpolation(es) => {
            for e in es {
                collect_ph_expr(e, out);
            }
        }
        Expr::Block(stmts)
        | Expr::AnonSub(stmts)
        | Expr::AnonSubParams { body: stmts, .. }
        | Expr::Gather(stmts) => {
            for s in stmts {
                collect_ph_stmt(s, out);
            }
        }
        Expr::DoBlock { body, .. } => {
            for s in body {
                collect_ph_stmt(s, out);
            }
        }
        Expr::DoStmt(stmt) => {
            collect_ph_stmt(stmt, out);
        }
        Expr::Lambda { body, .. } => {
            for s in body {
                collect_ph_stmt(s, out);
            }
        }
        Expr::Try { body, catch } => {
            for s in body {
                collect_ph_stmt(s, out);
            }
            if let Some(c) = catch {
                for s in c {
                    collect_ph_stmt(s, out);
                }
            }
        }
        Expr::CodeVar(_) => {}
        Expr::Reduction { expr, .. } => collect_ph_expr(expr, out),
        Expr::HyperOp { left, right, .. } | Expr::MetaOp { left, right, .. } => {
            collect_ph_expr(left, out);
            collect_ph_expr(right, out);
        }
        Expr::InfixFunc { left, right, .. } => {
            collect_ph_expr(left, out);
            for e in right {
                collect_ph_expr(e, out);
            }
        }
        Expr::Hash(pairs) => {
            for (_, v) in pairs {
                if let Some(e) = v {
                    collect_ph_expr(e, out);
                }
            }
        }
        _ => {}
    }
}
