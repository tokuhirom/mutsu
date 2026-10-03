//! Rakudo's precedence hashes for the built-in operators, as `Routine.prec`
//! answers them.
//!
//! Generated from Rakudo 2026.09 by walking `CORE::` for every
//! `&infix:`/`&prefix:`/`&postfix:`/`&circumfix:`/`&postcircumfix:` symbol
//! and printing its non-empty `.prec`:
//!
//! ```raku
//! for CORE::.keys.grep(/^'&'(infix|prefix|postfix|circumfix|postcircumfix)':'/).sort -> $k {
//!     my $p = CORE::{$k}.prec;
//!     say $k, "\t", $p.sort.map({ .key ~ '=' ~ .value }).join("|") if $p;
//! }
//! ```
//!
//! [`BUILTIN`] is sorted by `(category, symbol)` byte order, which
//! `builtin_table_is_sorted` pins so [`super::builtin`] can binary-search it.

use super::PrecEntries;

pub(super) const ADDITIVE: PrecEntries = &[("assoc", "left"), ("dba", "additive"), ("prec", "t=")];
pub(super) const ADDITIVE_IFFY: PrecEntries = &[
    ("assoc", "left"),
    ("dba", "additive-iffy"),
    ("iffy", "1"),
    ("prec", "t="),
];
pub(super) const AUTOINCREMENT: PrecEntries =
    &[("assoc", "unary"), ("dba", "autoincrement"), ("prec", "x=")];
pub(super) const CHAINING: PrecEntries = &[
    ("assoc", "chain"),
    ("dba", "chaining"),
    ("diffy", "1"),
    ("iffy", "1"),
    ("prec", "m="),
];
pub(super) const COMMA: PrecEntries = &[
    ("assoc", "list"),
    ("dba", "comma"),
    ("nextterm", "nulltermish"),
    ("prec", "g="),
];
pub(super) const CONCATENATION: PrecEntries =
    &[("assoc", "left"), ("dba", "concatenation"), ("prec", "r=")];
pub(super) const DEFAULT_CIRCUMFIX: PrecEntries = &[("dba", "default-circumfix")];
pub(super) const DEFAULT_PREFIX: PrecEntries = &[
    ("assoc", "unary"),
    ("dba", "default-prefix"),
    ("prec", "v="),
];
pub(super) const EXPONENTIATION: PrecEntries = &[
    ("assoc", "right"),
    ("dba", "exponentiation"),
    ("prec", "w="),
];
pub(super) const ITEM_ASSIGNMENT: PrecEntries = &[
    ("assoc", "right"),
    ("dba", "item-assignment"),
    ("prec", "i="),
];
pub(super) const JUNCTIVE_AND: PrecEntries =
    &[("assoc", "list"), ("dba", "junctive-and"), ("prec", "q=")];
pub(super) const JUNCTIVE_AND_IFFY: PrecEntries = &[
    ("assoc", "list"),
    ("dba", "junctive-and-iffy"),
    ("iffy", "1"),
    ("prec", "q="),
];
pub(super) const JUNCTIVE_OR: PrecEntries =
    &[("assoc", "list"), ("dba", "junctive-or"), ("prec", "p=")];
pub(super) const JUNCTIVE_OR_IFFY: PrecEntries = &[
    ("assoc", "list"),
    ("dba", "junctive-or-iffy"),
    ("iffy", "1"),
    ("prec", "p="),
];
pub(super) const LIST_ASSIGNMENT: PrecEntries = &[
    ("assoc", "right"),
    ("dba", "list assignment"),
    ("fiddly", "1"),
    ("prec", "i="),
    ("sub", "e="),
];
pub(super) const LIST_INFIX: PrecEntries =
    &[("assoc", "list"), ("dba", "list-infix"), ("prec", "f=")];
pub(super) const LOOSE_AND: PrecEntries = &[
    ("assoc", "left"),
    ("dba", "loose-and"),
    ("iffy", "1"),
    ("prec", "d="),
    ("thunky", ".t"),
];
pub(super) const LOOSE_ANDTHEN: PrecEntries = &[
    ("assoc", "list"),
    ("dba", "loose-andthen"),
    ("prec", "d="),
    ("thunky", ".b"),
];
pub(super) const LOOSE_OR: PrecEntries = &[
    ("assoc", "left"),
    ("dba", "loose-or"),
    ("iffy", "1"),
    ("prec", "c="),
    ("thunky", ".t"),
];
pub(super) const LOOSE_ORELSE: PrecEntries = &[
    ("assoc", "list"),
    ("dba", "loose-orelse"),
    ("prec", "c="),
    ("thunky", ".b"),
];
pub(super) const LOOSE_UNARY: PrecEntries =
    &[("assoc", "unary"), ("dba", "loose-unary"), ("prec", "h=")];
pub(super) const LOOSE_XOR: PrecEntries = &[
    ("assoc", "list"),
    ("dba", "loose-xor"),
    ("iffy", "1"),
    ("prec", "c="),
    ("thunky", ".t"),
];
pub(super) const METHODCALL: PrecEntries = &[
    ("assoc", "unary"),
    ("dba", "methodcall"),
    ("fiddly", "1"),
    ("prec", "y="),
];
pub(super) const MULTIPLICATIVE: PrecEntries =
    &[("assoc", "left"), ("dba", "multiplicative"), ("prec", "u=")];
pub(super) const MULTIPLICATIVE_IFFY: PrecEntries = &[
    ("assoc", "left"),
    ("dba", "multiplicative-iffy"),
    ("iffy", "1"),
    ("prec", "u="),
];
pub(super) const REPLICATION_X: PrecEntries =
    &[("assoc", "left"), ("dba", "replication-x"), ("prec", "s=")];
pub(super) const REPLICATION_XX: PrecEntries = &[
    ("assoc", "left"),
    ("dba", "replication-xx"),
    ("prec", "s="),
    ("thunky", "t."),
];
pub(super) const STRUCTURAL: PrecEntries = &[
    ("assoc", "non"),
    ("dba", "structural"),
    ("diffy", "1"),
    ("prec", "n="),
];
pub(super) const SYMBOLIC_UNARY: PrecEntries = &[
    ("assoc", "unary"),
    ("dba", "symbolic-unary"),
    ("prec", "v="),
];
pub(super) const TIGHT_AND: PrecEntries = &[
    ("assoc", "left"),
    ("dba", "tight-and"),
    ("iffy", "1"),
    ("prec", "l="),
    ("thunky", ".t"),
];
pub(super) const TIGHT_DEFOR: PrecEntries = &[
    ("assoc", "left"),
    ("dba", "tight-defor"),
    ("prec", "k="),
    ("thunky", ".t"),
];
pub(super) const TIGHT_MINMAX: PrecEntries =
    &[("assoc", "list"), ("dba", "tight-minmax"), ("prec", "k=")];
pub(super) const TIGHT_OR: PrecEntries = &[
    ("assoc", "left"),
    ("dba", "tight-or"),
    ("iffy", "1"),
    ("prec", "k="),
    ("thunky", ".t"),
];
pub(super) const TIGHT_XOR: PrecEntries = &[
    ("assoc", "list"),
    ("dba", "tight-xor"),
    ("iffy", "1"),
    ("prec", "k="),
    ("thunky", "..t"),
];

pub(super) static BUILTIN: &[(&str, &str, PrecEntries)] = &[
    ("circumfix", ":{ }", DEFAULT_CIRCUMFIX),
    ("circumfix", "[ ]", METHODCALL),
    ("circumfix", "{ }", METHODCALL),
    ("infix", "!=", CHAINING),
    ("infix", "!~~", CHAINING),
    ("infix", "%", MULTIPLICATIVE),
    ("infix", "%%", MULTIPLICATIVE_IFFY),
    ("infix", "&", JUNCTIVE_AND_IFFY),
    ("infix", "&&", TIGHT_AND),
    ("infix", "(&)", JUNCTIVE_AND),
    ("infix", "(+)", JUNCTIVE_OR),
    ("infix", "(-)", JUNCTIVE_OR),
    ("infix", "(.)", JUNCTIVE_AND),
    ("infix", "(<)", CHAINING),
    ("infix", "(<+)", CHAINING),
    ("infix", "(<=)", CHAINING),
    ("infix", "(==)", CHAINING),
    ("infix", "(>)", CHAINING),
    ("infix", "(>+)", CHAINING),
    ("infix", "(>=)", CHAINING),
    ("infix", "(^)", JUNCTIVE_OR),
    ("infix", "(cont)", CHAINING),
    ("infix", "(elem)", CHAINING),
    ("infix", "(|)", JUNCTIVE_OR),
    ("infix", "*", MULTIPLICATIVE),
    ("infix", "**", EXPONENTIATION),
    ("infix", "+", ADDITIVE),
    ("infix", "+&", MULTIPLICATIVE),
    ("infix", "+<", MULTIPLICATIVE),
    ("infix", "+>", MULTIPLICATIVE),
    ("infix", "+^", ADDITIVE),
    ("infix", "+|", ADDITIVE),
    ("infix", ",", COMMA),
    ("infix", "-", ADDITIVE),
    ("infix", "..", STRUCTURAL),
    ("infix", "...", LIST_INFIX),
    ("infix", "...^", LIST_INFIX),
    ("infix", "..^", STRUCTURAL),
    ("infix", "/", MULTIPLICATIVE),
    ("infix", "//", TIGHT_DEFOR),
    ("infix", "<", CHAINING),
    ("infix", "<=", CHAINING),
    ("infix", "<=>", STRUCTURAL),
    ("infix", "=", LIST_ASSIGNMENT),
    ("infix", "=:=", CHAINING),
    ("infix", "==", CHAINING),
    ("infix", "===", CHAINING),
    ("infix", "=>", ITEM_ASSIGNMENT),
    ("infix", "=~", ITEM_ASSIGNMENT),
    ("infix", "=~=", CHAINING),
    ("infix", ">", CHAINING),
    ("infix", ">=", CHAINING),
    ("infix", "?&", MULTIPLICATIVE_IFFY),
    ("infix", "?^", ADDITIVE_IFFY),
    ("infix", "?|", ADDITIVE_IFFY),
    ("infix", "X", LIST_INFIX),
    ("infix", "Z", LIST_INFIX),
    ("infix", "^", JUNCTIVE_OR_IFFY),
    ("infix", "^..", STRUCTURAL),
    ("infix", "^...", LIST_INFIX),
    ("infix", "^...^", LIST_INFIX),
    ("infix", "^..^", STRUCTURAL),
    ("infix", "^^", TIGHT_XOR),
    ("infix", "^…", LIST_INFIX),
    ("infix", "^…^", LIST_INFIX),
    ("infix", "after", CHAINING),
    ("infix", "and", LOOSE_AND),
    ("infix", "andthen", LOOSE_ANDTHEN),
    ("infix", "before", CHAINING),
    ("infix", "but", STRUCTURAL),
    ("infix", "cmp", STRUCTURAL),
    ("infix", "coll", STRUCTURAL),
    ("infix", "div", MULTIPLICATIVE),
    ("infix", "does", STRUCTURAL),
    ("infix", "eq", CHAINING),
    ("infix", "eqv", CHAINING),
    ("infix", "gcd", MULTIPLICATIVE),
    ("infix", "ge", CHAINING),
    ("infix", "gt", CHAINING),
    ("infix", "lcm", MULTIPLICATIVE),
    ("infix", "le", CHAINING),
    ("infix", "leg", STRUCTURAL),
    ("infix", "lt", CHAINING),
    ("infix", "max", TIGHT_MINMAX),
    ("infix", "min", TIGHT_MINMAX),
    ("infix", "minmax", LIST_INFIX),
    ("infix", "mod", MULTIPLICATIVE),
    ("infix", "ne", CHAINING),
    ("infix", "notandthen", LOOSE_ANDTHEN),
    ("infix", "o", CONCATENATION),
    ("infix", "or", LOOSE_OR),
    ("infix", "orelse", LOOSE_ORELSE),
    ("infix", "unicmp", STRUCTURAL),
    ("infix", "x", REPLICATION_X),
    ("infix", "xor", LOOSE_XOR),
    ("infix", "xx", REPLICATION_XX),
    ("infix", "|", JUNCTIVE_OR_IFFY),
    ("infix", "||", TIGHT_OR),
    ("infix", "~", CONCATENATION),
    ("infix", "~&", MULTIPLICATIVE),
    ("infix", "~<", MULTIPLICATIVE),
    ("infix", "~>", MULTIPLICATIVE),
    ("infix", "~^", ADDITIVE),
    ("infix", "~|", ADDITIVE),
    ("infix", "~~", CHAINING),
    ("infix", "×", MULTIPLICATIVE),
    ("infix", "÷", MULTIPLICATIVE),
    ("infix", "…", LIST_INFIX),
    ("infix", "…^", LIST_INFIX),
    ("infix", "⇒", ITEM_ASSIGNMENT),
    ("infix", "∈", CHAINING),
    ("infix", "∉", CHAINING),
    ("infix", "∊", CHAINING),
    ("infix", "∋", CHAINING),
    ("infix", "∌", CHAINING),
    ("infix", "∍", CHAINING),
    ("infix", "−", ADDITIVE),
    ("infix", "∖", JUNCTIVE_OR),
    ("infix", "∘", CONCATENATION),
    ("infix", "∩", JUNCTIVE_AND),
    ("infix", "∪", JUNCTIVE_OR),
    ("infix", "≅", CHAINING),
    ("infix", "≠", CHAINING),
    ("infix", "≡", CHAINING),
    ("infix", "≢", CHAINING),
    ("infix", "≤", CHAINING),
    ("infix", "≥", CHAINING),
    ("infix", "≼", CHAINING),
    ("infix", "≽", CHAINING),
    ("infix", "⊂", CHAINING),
    ("infix", "⊃", CHAINING),
    ("infix", "⊄", CHAINING),
    ("infix", "⊅", CHAINING),
    ("infix", "⊆", CHAINING),
    ("infix", "⊇", CHAINING),
    ("infix", "⊈", CHAINING),
    ("infix", "⊉", CHAINING),
    ("infix", "⊍", JUNCTIVE_AND),
    ("infix", "⊎", JUNCTIVE_OR),
    ("infix", "⊖", JUNCTIVE_OR),
    ("infix", "⚛+=", ITEM_ASSIGNMENT),
    ("infix", "⚛-=", ITEM_ASSIGNMENT),
    ("infix", "⚛=", ITEM_ASSIGNMENT),
    ("infix", "⚛−=", ITEM_ASSIGNMENT),
    ("infix", "⩵", CHAINING),
    ("infix", "⩶", CHAINING),
    ("postcircumfix", "[ ]", METHODCALL),
    ("postcircumfix", "[; ]", METHODCALL),
    ("postcircumfix", "{ }", METHODCALL),
    ("postcircumfix", "{; }", METHODCALL),
    ("postfix", "++", AUTOINCREMENT),
    ("postfix", "--", AUTOINCREMENT),
    ("postfix", "i", METHODCALL),
    ("postfix", "ⁿ", AUTOINCREMENT),
    ("postfix", "⚛++", AUTOINCREMENT),
    ("postfix", "⚛--", AUTOINCREMENT),
    ("prefix", "!", SYMBOLIC_UNARY),
    ("prefix", "+", SYMBOLIC_UNARY),
    ("prefix", "++", AUTOINCREMENT),
    ("prefix", "++⚛", AUTOINCREMENT),
    ("prefix", "+^", SYMBOLIC_UNARY),
    ("prefix", "-", SYMBOLIC_UNARY),
    ("prefix", "--", AUTOINCREMENT),
    ("prefix", "--⚛", AUTOINCREMENT),
    ("prefix", "?", SYMBOLIC_UNARY),
    ("prefix", "?^", SYMBOLIC_UNARY),
    ("prefix", "^", SYMBOLIC_UNARY),
    ("prefix", "let", DEFAULT_PREFIX),
    ("prefix", "not", LOOSE_UNARY),
    ("prefix", "so", LOOSE_UNARY),
    ("prefix", "temp", DEFAULT_PREFIX),
    ("prefix", "|", SYMBOLIC_UNARY),
    ("prefix", "~", SYMBOLIC_UNARY),
    ("prefix", "~^", SYMBOLIC_UNARY),
    ("prefix", "−", SYMBOLIC_UNARY),
    ("prefix", "⚛", SYMBOLIC_UNARY),
];
