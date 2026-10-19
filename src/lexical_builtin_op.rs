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
//! before they stringify their operands. So are the ASCII spellings of the
//! set operators whose ops ask it (`(elem)`, `(cont)`, `(|)`, `(&)`, `(+)`,
//! `(.)`, `(-)`, `(^)`).
//!
//! The Unicode spellings of the set operators and every spelling of the set
//! relations (`∪`, `∈`, `⊆`, `(<=)`, `≡`, ...) are a third kind
//! ([`is_call_spelled_infix`]): once a block imports a candidate, the parser
//! turns each binary use of the spelling into a call of the user operator
//! (`OpCode::LexImportInfix` reads the set), and a reduction or hyper over it
//! runs the bound dispatcher (`OpCode::LexImportMeta`). The dispatcher falls
//! back to the built-in, so a call none of the candidates accepts is the
//! operator's own.
//!
//! The repetition operators `x` and `xx` are of the third kind too: an
//! imported candidate turns their uses into calls of the user operator. So is
//! the built-in postfix `i`, whose uses the parser turns into calls of
//! `postfix:<i>` ([`is_lexical_postfix`]); its dispatcher falls back to the
//! built-in imaginary-unit multiplication.
//!
//! The subscript operators `postcircumfix:<[ ]>` and `postcircumfix:<{ }>`
//! are of the first kind ([`is_lexical_postcircumfix`]): the subscript op
//! (`OpCode::Index`) asks the operator scope `OpCode::LexImportBuiltinOp`
//! opens before it asks the registry, and its dispatcher falls back to the
//! built-in subscript.
//!
//! The increment operators `++`/`--` (both positions) are call-spelled too;
//! their dispatcher ranks a candidate against the native increment's core
//! candidates (`Interpreter::core_increment_wins_over`) and falls back to it.
//!
//! The built-in infixes whose op asks no user candidate at all (the bitwise
//! operators, the ranges, `=>`, `===`, the junction constructors, ...) are a
//! fourth kind ([`is_replaced_infix`]): the imported dispatcher replaces the
//! op through `OpCode::LexImportInfix`, and falls back to the built-in.
//!
//! Every other built-in operator keeps its imported candidates in the
//! registry: its uses reach them by name (`postcircumfix:<< >>`, the thunky
//! operators, `does`).

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
        "infix" => is_lexical_infix(op) || is_call_spelled_infix(op) || is_replaced_infix(op),
        "prefix" => is_lexical_prefix(op),
        "postfix" => is_lexical_postfix(op),
        "postcircumfix" => is_lexical_postcircumfix(op),
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
            | "(elem)"
            | "(cont)"
            | "(|)"
            | "(&)"
            | "(+)"
            | "(.)"
            | "(-)"
            | "(^)"
    )
}

/// The built-in infixes whose uses in a block that imports a candidate are
/// calls of the imported user operator (see the module docs): the Unicode
/// spellings of the set operators, the set relations in every spelling, and
/// the repetition operators.
// Cost: O(1).
pub(crate) fn is_call_spelled_infix(op: &str) -> bool {
    matches!(
        op,
        "x" | "xx"
            | "∪"
            | "∩"
            | "⊎"
            | "⊍"
            | "∖"
            | "⊖"
            | "∈"
            | "∋"
            | "⊆"
            | "⊇"
            | "⊂"
            | "⊃"
            | "⊄"
            | "⊅"
            | "⊈"
            | "⊉"
            | "(<=)"
            | "(>=)"
            | "(<)"
            | "(>)"
            | "(==)"
            | "≡"
            | "≢"
    )
}

/// The built-in infixes whose op asks no user candidate, and which an
/// imported dispatcher therefore *replaces* in a block that imports a
/// candidate of them: the compiler prefixes the op with
/// `OpCode::LexImportInfix` ([`replaced_infix_of_opcode`]), or the parser
/// already spells the use as a call (`~^`, `minmax`), and on a hit the bound
/// dispatcher takes both operands and the op is skipped. The dispatcher falls
/// back to the built-in (`Interpreter::call_builtin_operator`), so a call
/// none of the candidates accepts is the operator's own. A thunky operator
/// (`&&`, `//`, `andthen`, ...), an assignment, `~~`, `,`, `=:=` and `does`
/// (whose variable form rebinds the variable) are not listed: their operands
/// or their operation are not a plain two-argument call.
// Cost: O(1).
pub(crate) fn is_replaced_infix(op: &str) -> bool {
    matches!(
        op,
        "%%" | "+&"
            | "+|"
            | "+^"
            | "+<"
            | "+>"
            | "~&"
            | "~|"
            | "~^"
            | "~<"
            | "~>"
            | "?&"
            | "?|"
            | "?^"
            | ".."
            | "..^"
            | "^.."
            | "^..^"
            | "..."
            | "...^"
            | "…"
            | "…^"
            | "<=>"
            | "==="
            | "=~="
            | "≅"
            | "=>"
            | "|"
            | "&"
            | "^"
            | "before"
            | "after"
            | "but"
            | "coll"
            | "unicmp"
            | "minmax"
            | "o"
            | "∘"
    )
}

/// The `infix:<op>` routine of the [`is_replaced_infix`] operator the binary
/// opcode `op` applies.
// Cost: O(1).
pub(crate) fn replaced_infix_of_opcode(op: &OpCode) -> Option<&'static str> {
    Some(match op {
        OpCode::DivisibleBy => "infix:<%%>",
        OpCode::BitAnd => "infix:<+&>",
        OpCode::BitOr => "infix:<+|>",
        OpCode::BitXor => "infix:<+^>",
        OpCode::BitShiftLeft => "infix:<+<>",
        OpCode::BitShiftRight => "infix:<+>>",
        OpCode::StrBitAnd => "infix:<~&>",
        OpCode::StrBitOr => "infix:<~|>",
        OpCode::StrBitXor => "infix:<~^>",
        OpCode::StrShiftLeft => "infix:<~<>",
        OpCode::StrShiftRight => "infix:<~>>",
        OpCode::BoolBitAnd => "infix:<?&>",
        OpCode::BoolBitOr => "infix:<?|>",
        OpCode::BoolBitXor => "infix:<?^>",
        OpCode::MakeRange => "infix:<..>",
        OpCode::MakeRangeExcl => "infix:<..^>",
        OpCode::MakeRangeExclStart => "infix:<^..>",
        OpCode::MakeRangeExclBoth => "infix:<^..^>",
        OpCode::Sequence { exclude_end: false } => "infix:<...>",
        OpCode::Sequence { exclude_end: true } => "infix:<...^>",
        OpCode::Spaceship => "infix:<<=>>",
        OpCode::StrictEq => "infix:<===>",
        OpCode::ApproxEq => "infix:<=~=>",
        OpCode::MakePair => "infix:<=>>",
        OpCode::JunctionAny => "infix:<|>",
        OpCode::JunctionAll => "infix:<&>",
        OpCode::JunctionOne => "infix:<^>",
        OpCode::Before => "infix:<before>",
        OpCode::After => "infix:<after>",
        OpCode::ButMixin => "infix:<but>",
        OpCode::Coll => "infix:<coll>",
        OpCode::Unicmp => "infix:<unicmp>",
        OpCode::FunctionCompose => "infix:<o>",
        _ => return None,
    })
}

/// Whether a reduction, triangle reduction or hyper over the built-in `op`
/// in a block that imports candidates of it runs the bound dispatcher
/// (`OpCode::LexImportMeta`), rather than the built-in with the candidates in
/// scope (`OpCode::LexImportBuiltinOp`): the set operator spellings of
/// [`is_call_spelled_infix`], and `(elem)`/`(cont)`, whose chained reduction
/// applies the dispatcher pair by pair.
// Cost: O(1).
pub(crate) fn meta_runs_bound_dispatcher(op: &str) -> bool {
    is_call_spelled_infix(op) || is_replaced_infix(op) || matches!(op, "(elem)" | "(cont)")
}

/// The built-in postfixes whose uses in a block that imports a candidate are
/// calls of the imported user operator, and whose dispatcher can fall back to
/// the built-in (`Interpreter::core_postfix_op`, and the native increment for
/// `++`/`--`).
// Cost: O(1).
pub(crate) fn is_lexical_postfix(op: &str) -> bool {
    matches!(op, "i" | "++" | "--")
}

/// The built-in postcircumfixes (by their space-separated delimiters) whose
/// subscript op reads the operator scope `OpCode::LexImportBuiltinOp` opens.
// Cost: O(1).
pub(crate) fn is_lexical_postcircumfix(delims: &str) -> bool {
    matches!(delims, "[ ]" | "{ }")
}

/// The `postcircumfix` routine a subscript op applies.
// Cost: O(1).
pub(crate) fn postcircumfix_of_subscript(is_positional: bool) -> &'static str {
    if is_positional {
        "postcircumfix:<[ ]>"
    } else {
        "postcircumfix:<{ }>"
    }
}

/// The built-in prefixes a dispatcher over imported candidates can fall back
/// to (`Interpreter::core_prefix_op`, and the native increment
/// `Interpreter::run_core_increment` for `++`/`--`, whose uses the parser
/// turns into calls of the operator once a candidate is imported).
// Cost: O(1).
fn is_lexical_prefix(op: &str) -> bool {
    matches!(
        op,
        "!" | "+" | "-" | "−" | "~" | "?" | "so" | "not" | "++" | "--" | "^" | "+^" | "~^" | "?^"
    )
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
        OpCode::SetElem => "infix:<(elem)>",
        OpCode::SetCont => "infix:<(cont)>",
        OpCode::SetUnion(false) => "infix:<(|)>",
        OpCode::SetUnion(true) => "infix:<∪>",
        OpCode::SetIntersect(false) => "infix:<(&)>",
        OpCode::SetIntersect(true) => "infix:<∩>",
        OpCode::SetAddition(false) => "infix:<(+)>",
        OpCode::SetAddition(true) => "infix:<⊎>",
        OpCode::SetMultiply(false) => "infix:<(.)>",
        OpCode::SetMultiply(true) => "infix:<⊍>",
        OpCode::SetDiff(false) => "infix:<(-)>",
        OpCode::SetDiff(true) => "infix:<∖>",
        OpCode::SetSymDiff(false) => "infix:<(^)>",
        OpCode::SetSymDiff(true) => "infix:<⊖>",
        OpCode::StringRepeat => "infix:<x>",
        _ => return None,
    })
}

/// The `infix:<op>` routine of the ASCII set relation the opcode `op` applies.
/// The parser turns a use of an imported relation into a call of the user
/// operator, but the RakuAST frontend lowers `(<=)` back to its opcode, which
/// asks no user candidate; `OpCode::LexImportInfix` in front of it reads the
/// block's import set instead.
// Cost: O(1).
pub(crate) fn relation_of_opcode(op: &OpCode) -> Option<&'static str> {
    Some(match op {
        OpCode::SetSubset => "infix:<(<=)>",
        OpCode::SetSuperset => "infix:<(>=)>",
        OpCode::SetStrictSubset => "infix:<(<)>",
        OpCode::SetStrictSuperset => "infix:<(>)>",
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
        CompoundBaseOp::StringRepeat => "infix:<x>",
        _ => return None,
    })
}

/// The `infix:<op>` routine a reduction or hyper over the built-in `op`
/// applies, when its user candidates bind lexically.
// Cost: O(m), m = bytes of `op`.
pub(crate) fn infix_of_meta(op: &str) -> Option<String> {
    is_lexical_infix(op).then(|| format!("infix:<{op}>"))
}
