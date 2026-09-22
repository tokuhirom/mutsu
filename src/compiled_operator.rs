//! Source-fixed operators, decoded once by the compiler instead of once per
//! execution.
//!
//! # Why this module exists
//!
//! `[!after]`, `>>+<<`, `R-`, `Zxx` and friends have a *statically known*
//! shape: the parser already decided which meta-operators wrap which base
//! operator, and nothing at run time can change that. Until this module
//! existed, the compiler flattened that shape back into a string
//! (`Value::str("\\_sc_&&")`, `Value::str("R")`) and stored it in the constant
//! pool, so the VM re-derived it on every execution of the opcode:
//!
//! - allocate a fresh `String` from the pooled constant
//!   (`Self::const_str(code, op_idx).to_string()`);
//! - strip the `\` scan marker, the `!` negation marker (behind a linear scan
//!   of an 80-entry `KNOWN_BASE_OPS` table), and the compiler's own `_sc_`
//!   thunked-short-circuit marker;
//! - fold the Unicode operator aliases (`×` → `*`, `≤` → `<=`, …);
//! - strip the statically-decidable `R` (reverse) meta-operator prefixes;
//! - `format!("infix:<{}>", op)` — up to *four* separate times per reduction,
//!   across [`crate::runtime::Interpreter::reduction_op_associativity`] and
//!   `reduction_callable_for_op`.
//!
//! All of that is a function of the source text. [`ReductionSpec::lower`] and
//! [`MetaKind::lower`] run it once at compile time; [`infix_names`] memoizes
//! the derived `infix:<op>` spellings per operator for the paths that still
//! reach an operator name at run time (a user-defined infix, or an operator
//! spelling computed by a helper rather than written in the source).
//!
//! [`InfixShape`] is the runtime-side counterpart, for the operator spellings
//! the compiler does not own: it decodes the `[op]` / `R` / `Z` / `>>op<<`
//! structure of an operator string ONCE, so the element loops of a hyper, a
//! zip, a cross or a fold walk a decoded shape instead of re-parsing the
//! spelling per element.
//!
//! This is the same finding [`crate::qualified`] records for package-qualified
//! names, in a different namespace: a name derived from other names is built
//! once, not per execution.
//!
//! # The guardrail
//!
//! [`ReductionSpec`]'s fields are decided *only* by [`ReductionSpec::lower`],
//! and the reduction / hyper / meta opcodes no longer take a constant-pool
//! index for their operator. So a new statically-spelled operator path cannot
//! be added by emitting one more string for the VM to re-parse: it has to
//! extend the lowering here first.

use crate::symbol::Symbol;

mod infix_shape;

pub(crate) use infix_shape::{InfixRef, InfixShape, MetaLayer};

/// The structural meta-operator a [`crate::opcode::OpCode::MetaOp`] (and its
/// assignment / n-ary siblings) applies to its base operator.
///
/// Replaces the `Value::str("R")` / `"X"` / `"Z"` / `"reduce"` constant the
/// compiler used to emit and the VM used to `match` as text.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum MetaKind {
    /// `[op]=` compound assignment (`$x [+]= 6`), lowered as a reduction of the
    /// base op over exactly two operands — i.e. one application of the base op.
    Reduce,
    /// `R` — reverse: apply the base op with the operands swapped.
    Reverse,
    /// `X` — cross.
    Cross,
    /// `Z` — zip.
    Zip,
    /// `!` — negate the base op's result. No parser path produces this today;
    /// the arm is kept because the VM has always had it.
    Negate,
}

impl MetaKind {
    /// The meta-operator this spelling names, or `None` when it names none.
    ///
    /// Only the compiler calls this; the VM matches the enum.
    pub(crate) fn lower(meta: &str) -> Option<Self> {
        match meta {
            "reduce" => Some(Self::Reduce),
            "R" => Some(Self::Reverse),
            "X" => Some(Self::Cross),
            "Z" => Some(Self::Zip),
            "!" => Some(Self::Negate),
            _ => None,
        }
    }

    /// The source spelling, for diagnostics and for the `&str`-keyed operator
    /// helpers that have not been converted yet.
    pub(crate) fn as_str(self) -> &'static str {
        match self {
            Self::Reduce => "reduce",
            Self::Reverse => "R",
            Self::Cross => "X",
            Self::Zip => "Z",
            Self::Negate => "!",
        }
    }
}

/// A reduction operator (`[+]`, `[\~]`, `[!after]`, `[R/]`), fully decoded.
///
/// Built only by [`ReductionSpec::lower`], from the operator spelling the
/// parser recorded. Every field below used to be re-derived from that spelling
/// on each execution of [`crate::opcode::OpCode::Reduction`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct ReductionSpec {
    /// `[\op]` — the triangle/scan form, which yields every intermediate.
    pub(crate) scan: bool,
    /// `[!op]` — negate each step's result (`[!after]`, `[!eqv]`).
    ///
    /// Only set when what follows the `!` is itself a known base operator:
    /// `!=` is its own operator and must not be split.
    pub(crate) negate: bool,
    /// The compiler's thunked short-circuit reduction form (`[||]`, `[&&]`,
    /// `[//]`, `[^^]`, … over a literal argument list), whose operand is an
    /// array of thunks rather than a list of values. Spelled `_sc_<op>` in the
    /// old string encoding.
    pub(crate) shortcircuit: bool,
    /// Whether an odd number of *statically decidable* `R` (reverse) meta-op
    /// prefixes wrapped the base operator, which reverses the whole fold. The
    /// prefixes themselves are already stripped from [`Self::base`].
    ///
    /// A `R&callable` inner cannot be decided here — whether `&foo` names a
    /// callable is a runtime question — so [`Self::base`] can still carry a
    /// leading `R` that the VM strips.
    pub(crate) reverse: bool,
    /// The base operator, with the markers above removed and the Unicode
    /// aliases folded to their ASCII spelling.
    pub(crate) base: Symbol,
}

/// The base operators `[!op]` may negate.
///
/// Deliberately *not* [`is_builtin_infix`]'s table: `!=`, `!=:=` and `=`/`:=`
/// differ between the two, and folding them together would change which
/// `[!...]` spellings split. This is the list the reduction opcode has always
/// consulted for the negation prefix.
const NEGATABLE_BASE_OPS: &[&str] = &[
    "+", "-", "*", "/", "%", "~", "||", "&&", "//", "%%", "**", "^^", "+&", "+|", "+^", "+<", "+>",
    "~&", "~|", "~^", "~<", "~>", "?&", "?|", "?^", "==", "!=", "<", ">", "<=", ">=", "<=>", "===",
    "=:=", "!=:=", "=>", "eqv", "eq", "ne", "lt", "gt", "le", "ge", "leg", "cmp", "~~", "min",
    "max", "gcd", "lcm", "and", "or", "not", "andthen", "orelse", "xor", "minmax", ",", "after",
    "before", "X", "Z", "x", "xx", "&", "|", "^", "o", "∘", "(-)", "∖", "(|)", "∪", "(&)", "∩",
    "(^)", "⊖", "(.)", "⊍", "(==)", "≡", "≢",
];

/// The ASCII spelling a Unicode operator alias folds to, or `op` unchanged.
///
/// `∘`→`o`, `×`→`*`, `÷`→`/`, `−`(U+2212)→`-`, `≤`→`<=`, `≥`→`>=`, `≠`→`!=`.
/// Without the fold a `[×]` reduction reaches an `infix:<×>` lookup that does
/// not exist.
pub(crate) fn canonical_infix(op: &str) -> &str {
    match op {
        "\u{2218}" => "o",
        "\u{00D7}" => "*",
        "\u{00F7}" => "/",
        "\u{2212}" => "-",
        "\u{2264}" => "<=",
        "\u{2265}" => ">=",
        "\u{2260}" => "!=",
        other => other,
    }
}

/// Strip hyper-operator delimiters (`>>op<<`, `>>op>>`, `<<op<<`, `<<op>>`)
/// and their Unicode variants, returning the inner operator if found.
pub(crate) fn strip_hyper_delimiters(s: &str) -> Option<&str> {
    let after_left = s
        .strip_prefix(">>")
        .or_else(|| s.strip_prefix("<<"))
        .or_else(|| s.strip_prefix('\u{00BB}'))
        .or_else(|| s.strip_prefix('\u{00AB}'))?;
    let inner = after_left
        .strip_suffix(">>")
        .or_else(|| after_left.strip_suffix("<<"))
        .or_else(|| after_left.strip_suffix('\u{00BB}'))
        .or_else(|| after_left.strip_suffix('\u{00AB}'))?;
    if inner.is_empty() {
        return None;
    }
    Some(inner)
}

/// Whether `op` names a builtin infix the reduction / hyper / meta machinery
/// implements itself, rather than one it must resolve as a user routine.
///
/// Recurses through the `R`/`Z`/`X` meta prefixes and the hyper delimiters, so
/// `RZ+` and `>>+<<` answer `true` as well.
pub(crate) fn is_builtin_infix(op: &str) -> bool {
    if let Some(inner) = op
        .strip_prefix('R')
        .or_else(|| op.strip_prefix('Z'))
        .or_else(|| op.strip_prefix('X'))
        && !inner.is_empty()
        && is_builtin_infix(inner)
    {
        return true;
    }
    if let Some(inner) = strip_hyper_delimiters(op)
        && is_builtin_infix(inner)
    {
        return true;
    }
    matches!(
        op,
        "+" | "-"
            | "*"
            | "/"
            | "%"
            | "~"
            | "||"
            | "&&"
            | "//"
            | "%%"
            | "**"
            | "^^"
            | "+&"
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
            | "=="
            | "!="
            | "<"
            | ">"
            | "<="
            | ">="
            | "<=>"
            | "==="
            | "=:="
            | "!=:="
            | "=>"
            | "eqv"
            | "eq"
            | "ne"
            | "lt"
            | "gt"
            | "le"
            | "ge"
            | "leg"
            | "cmp"
            | "~~"
            | "min"
            | "max"
            | "div"
            | "mod"
            | "gcd"
            | "lcm"
            | "and"
            | "or"
            | "not"
            | "andthen"
            | "orelse"
            | "notandthen"
            | "xor"
            | "="
            | "minmax"
            | ","
            | "after"
            | "before"
            | "X"
            | "Z"
            | "x"
            | "xx"
            | "&"
            | "|"
            | "^"
            | "o"
            | "∘"
            | "(-)"
            | "∖"
            | "(|)"
            | "∪"
            | "(&)"
            | "∩"
            | "(^)"
            | "⊖"
            | "(.)"
            | "⊍"
            | "(+)"
            | "⊎"
            | "(==)"
            | "≡"
            | "≢"
    )
}

impl ReductionSpec {
    /// Decode the reduction operator spelling the parser (or
    /// `compile_thunk_reduction`) produced.
    ///
    /// The marker order matches the sequence the VM used to apply, so the
    /// decoded shape is identical to what each execution used to compute:
    /// scan, then negation, then the Unicode fold, then the `_sc_` marker,
    /// then the statically decidable `R` prefixes.
    pub(crate) fn lower(op: &str) -> Self {
        let (scan, rest) = match op.strip_prefix('\\') {
            Some(stripped) => (true, stripped),
            None => (false, op),
        };
        // Only treat '!' as a negation prefix when the remainder is itself a
        // known operator (`[!after]`, `[!==]`, `[!eqv]`). Operators like `!=`
        // are their own base operators and must not be split.
        let (negate, rest) = match rest.strip_prefix('!') {
            Some(stripped) if NEGATABLE_BASE_OPS.contains(&stripped) => (true, stripped),
            _ => (false, rest),
        };
        let rest = canonical_infix(rest);
        let (shortcircuit, mut base) = match rest.strip_prefix("_sc_") {
            Some(stripped) => (true, stripped),
            None => (false, rest),
        };
        // The `R` (reverse) meta-op on a reduction reverses the whole fold:
        // `[R op] @list` == `[op] @list.reverse`. Reversing the operand list
        // and stripping the `R` is correct for the non-commutative ops where a
        // per-step operand swap would not be (`[R/] 100,10,2` is `2/10/100`).
        // An even number of prefixes cancels out.
        //
        // Only builtin inners are decidable here; `R&callable` is left for the
        // VM, which can ask whether `&foo` resolves to a callable.
        let mut reverse = false;
        if !shortcircuit {
            while let Some(inner) = base.strip_prefix('R') {
                if inner.is_empty() || !is_builtin_infix(inner) {
                    break;
                }
                reverse = !reverse;
                base = inner;
            }
        }
        Self {
            scan,
            negate,
            shortcircuit,
            reverse,
            base: Symbol::intern(base),
        }
    }

    /// The base operator's spelling. `Symbol::as_str` hands back the
    /// interner's own `&'static str`, so this allocates nothing.
    pub(crate) fn base_str(&self) -> &'static str {
        self.base.as_str()
    }
}

/// The routine-name spellings an operator resolves through, built once per
/// operator.
#[derive(Debug, Clone, Copy)]
pub(crate) struct InfixNames {
    /// `infix:<op>`
    pub(crate) infix: Symbol,
    /// `&infix:<op>`
    pub(crate) amp_infix: Symbol,
    /// `&op`
    pub(crate) amp_op: Symbol,
}

/// The `infix:<op>` / `&infix:<op>` / `&op` spellings for `op`, memoized.
///
/// Symbols are global and append-only, so each of these has one answer for the
/// life of the process. Resolving a reduction's operator used to `format!`
/// them on every execution — up to three in `reduction_callable_for_op` plus
/// one in `reduction_op_associativity`.
///
/// Keyed by the spelling rather than by an interned `Symbol`: interning the
/// lookup key would itself be the per-execution work this exists to remove.
pub(crate) fn infix_names(op: &str) -> InfixNames {
    thread_local! {
        static NAMES: std::cell::RefCell<rustc_hash::FxHashMap<Box<str>, InfixNames>> =
            std::cell::RefCell::new(rustc_hash::FxHashMap::default());
    }
    if let Some(names) = NAMES.with(|c| c.borrow().get(op).copied()) {
        return names;
    }
    let infix_text = format!("infix:<{}>", op);
    let names = InfixNames {
        infix: Symbol::intern(&infix_text),
        amp_infix: Symbol::intern(&format!("&{}", infix_text)),
        amp_op: Symbol::intern(&format!("&{}", op)),
    };
    NAMES.with(|c| {
        c.borrow_mut().insert(op.into(), names);
    });
    names
}

#[cfg(test)]
mod tests;
