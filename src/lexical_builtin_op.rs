//! The built-in operators whose user candidates a block-scoped `use` binds
//! lexically (ADR-8751 stage 2).
//!
//! `{ use M; ... }` where `M` exports `multi infix:<+>(Foo, Foo)` adds the
//! candidates to `+` inside the block only. That works for an operator only
//! when every form of its use in the block can read the block's import set:
//!
//! - an infix whose ops reach user candidates through `try_user_infix` (its
//!   own opcode, the compound assignment, the reductions and the hypers), all
//!   of which the compiler prefixes with `OpCode::LexImportBuiltinOp`
//!   ([`infix_of_opcode`], [`infix_of_compound`]); `&infix:<+>` reads the set
//!   through `LexImportCall`;
//! - a prefix, whose every use the parser turns into a call of its full name
//!   (`prefix:<->($x)`) once a candidate is imported, and which
//!   `LexImportCall` reads.
//!
//! The string operators (`~`, `eq` ... `ge`, `leg`) are infixes of the first
//! kind: `infix_concat`, `str_cmp_values` and `str_leg` ask `try_user_infix`
//! before they stringify their operands.
//!
//! Every other built-in operator keeps its imported candidates in the
//! registry: its uses reach them by name (the set operators, a postfix,
//! `postcircumfix`).

use crate::opcode::{CompoundBaseOp, OpCode};

/// Whether the user candidates of the operator routine `name` (`infix:<+>`,
/// `prefix:<->`) bind lexically when a block-scoped `use` imports them.
// Cost: O(m), m = bytes of `name`.
pub(crate) fn binds_lexically(name: &str) -> bool {
    let Some((category, rest)) = name.split_once(":<") else {
        return false;
    };
    let Some(op) = rest.strip_suffix('>') else {
        return false;
    };
    match category {
        "infix" => is_lexical_infix(op),
        "prefix" => is_lexical_prefix(op),
        _ => false,
    }
}

/// The built-in infixes whose every op consults `try_user_infix`.
// Cost: O(1).
fn is_lexical_infix(op: &str) -> bool {
    matches!(
        op,
        "+" | "-"
            | "*"
            | "/"
            | "%"
            | "**"
            | "=="
            | "!="
            | "<"
            | "<="
            | ">"
            | ">="
            | "cmp"
            | "div"
            | "mod"
            | "gcd"
            | "lcm"
            | "eqv"
            | "min"
            | "max"
            | "~"
            | "eq"
            | "ne"
            | "lt"
            | "gt"
            | "le"
            | "ge"
            | "leg"
    )
}

/// The built-in prefixes a dispatcher over imported candidates can fall back
/// to (`Interpreter::core_prefix_op`). `++`/`--` mutate their operand
/// through opcodes of their own.
// Cost: O(1).
fn is_lexical_prefix(op: &str) -> bool {
    matches!(op, "!" | "+" | "-" | "−" | "~" | "?" | "so" | "not")
}

/// The `infix:<op>` routine the binary opcode `op` applies, when its user
/// candidates bind lexically.
// Cost: O(1).
pub(crate) fn infix_of_opcode(op: &OpCode) -> Option<&'static str> {
    Some(match op {
        OpCode::Add => "infix:<+>",
        OpCode::Sub => "infix:<->",
        OpCode::Mul => "infix:<*>",
        OpCode::Div => "infix:</>",
        OpCode::Mod => "infix:<%>",
        OpCode::Pow => "infix:<**>",
        OpCode::NumEq => "infix:<==>",
        OpCode::NumNe => "infix:<!=>",
        OpCode::NumLt => "infix:<<>",
        OpCode::NumLe => "infix:<<=>",
        OpCode::NumGt => "infix:<>>",
        OpCode::NumGe => "infix:<>=>",
        OpCode::Cmp => "infix:<cmp>",
        OpCode::IntDiv => "infix:<div>",
        OpCode::IntMod => "infix:<mod>",
        OpCode::Gcd => "infix:<gcd>",
        OpCode::Lcm => "infix:<lcm>",
        OpCode::Eqv => "infix:<eqv>",
        OpCode::InfixMin => "infix:<min>",
        OpCode::InfixMax => "infix:<max>",
        OpCode::Concat => "infix:<~>",
        OpCode::StrEq => "infix:<eq>",
        OpCode::StrNe => "infix:<ne>",
        OpCode::StrLt => "infix:<lt>",
        OpCode::StrGt => "infix:<gt>",
        OpCode::StrLe => "infix:<le>",
        OpCode::StrGe => "infix:<ge>",
        OpCode::Leg => "infix:<leg>",
        _ => return None,
    })
}

/// The `infix:<op>` routine the compound assignment over `op` applies, when
/// its user candidates bind lexically.
// Cost: O(1).
pub(crate) fn infix_of_compound(op: CompoundBaseOp) -> Option<&'static str> {
    Some(match op {
        CompoundBaseOp::Add => "infix:<+>",
        CompoundBaseOp::Sub => "infix:<->",
        CompoundBaseOp::Mul => "infix:<*>",
        CompoundBaseOp::Div => "infix:</>",
        CompoundBaseOp::Mod => "infix:<%>",
        CompoundBaseOp::Pow => "infix:<**>",
        CompoundBaseOp::IntDiv => "infix:<div>",
        CompoundBaseOp::IntMod => "infix:<mod>",
        CompoundBaseOp::Gcd => "infix:<gcd>",
        CompoundBaseOp::Lcm => "infix:<lcm>",
        CompoundBaseOp::InfixMin => "infix:<min>",
        CompoundBaseOp::InfixMax => "infix:<max>",
        CompoundBaseOp::Concat => "infix:<~>",
        _ => return None,
    })
}

/// The `infix:<op>` routine a reduction or hyper over the built-in `op`
/// applies, when its user candidates bind lexically.
// Cost: O(m), m = bytes of `op`.
pub(crate) fn infix_of_meta(op: &str) -> Option<String> {
    is_lexical_infix(op).then(|| format!("infix:<{op}>"))
}
