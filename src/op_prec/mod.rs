//! An operator's precedence hash: what `Routine.prec` answers.
//!
//! Rakudo describes an operator's place in the grammar with a small hash:
//! `prec` (the level, a string such as `t=` for additive), `assoc`, `dba`
//! (the level's name, used in error messages) and a few flags (`iffy`,
//! `diffy`, `fiddly`, `thunky`, ...). `&infix:<+>.prec` answers
//! `{assoc => left, dba => additive, prec => t=}`.
//!
//! The built-in operators' hashes are [`builtin_table`]. A user operator
//! starts from its category's default and is changed by its traits, in the
//! way Rakudo's `trait_mod:<is>` candidates change it:
//!
//! - `is equiv(&op)` copies `&op`'s hash;
//! - `is tighter(&op)` / `is looser(&op)` copy it, insert `@` / `:` before
//!   the `=` of its `prec` (`t=` becomes `t@=` / `t:=`), and reset `assoc` to
//!   `left`;
//! - `is assoc<...>` sets `assoc`, whatever order the traits are written in.
//!
//! The parser computes a user operator's hash when it reads the declaration
//! (it is the layer that knows which operator a lexical name refers to) and
//! hands it to the runtime as the `__prec` trait, in the form
//! [`OpPrec::encode`] writes.
//!
//! This module is a leaf: it names nothing above it, so the parser and the
//! runtime share it.

mod builtin_table;

/// A precedence hash as static `(key, value)` pairs, sorted by key.
pub(crate) type PrecEntries = &'static [(&'static str, &'static str)];

/// The keys whose value Rakudo stores as the integer `1`, not a string.
pub(crate) fn is_int_key(key: &str) -> bool {
    matches!(key, "iffy" | "diffy" | "fiddly")
}

const DEFAULT_INFIX: PrecEntries = &[("assoc", "left"), ("dba", "default-infix"), ("prec", "t=")];
const DEFAULT_POSTFIX: PrecEntries = &[
    ("assoc", "unary"),
    ("dba", "default-postfix"),
    ("prec", "x="),
];
const DEFAULT_POSTCIRCUMFIX: PrecEntries = &[("dba", "default-postcircumfix"), ("prec", "y=")];

/// Split an operator's routine name into its category and symbol:
/// `infix:<+>` and `infix:«<=»` give `("infix", "+")` and `("infix", "<=")`.
// Cost: O(n), n = name length.
pub(crate) fn split_op_name(name: &str) -> Option<(&str, &str)> {
    let (category, rest) = name.split_once(':')?;
    if !matches!(
        category,
        "infix" | "prefix" | "postfix" | "circumfix" | "postcircumfix"
    ) {
        return None;
    }
    let symbol = rest
        .strip_prefix("<<")
        .and_then(|s| s.strip_suffix(">>"))
        .or_else(|| rest.strip_prefix('<').and_then(|s| s.strip_suffix('>')))
        .or_else(|| {
            rest.strip_prefix('\u{ab}')
                .and_then(|s| s.strip_suffix('\u{bb}'))
        })?;
    Some((category, symbol))
}

/// The precedence hash of the built-in `category:<symbol>` operator.
// Cost: O(log n), n = built-in operator count (~180).
pub(crate) fn builtin(category: &str, symbol: &str) -> Option<PrecEntries> {
    let table = builtin_table::BUILTIN;
    table
        .binary_search_by(|(c, s, _)| (*c, *s).cmp(&(category, symbol)))
        .ok()
        .map(|idx| table[idx].2)
}

/// The precedence hash a user operator of `category` has before any trait.
/// `None` for a name that is not an operator category (`term`, `trait_mod`).
// Cost: O(1).
pub(crate) fn category_default(category: &str) -> Option<PrecEntries> {
    match category {
        "infix" => Some(DEFAULT_INFIX),
        "prefix" => Some(builtin_table::DEFAULT_PREFIX),
        "postfix" => Some(DEFAULT_POSTFIX),
        "circumfix" => Some(builtin_table::DEFAULT_CIRCUMFIX),
        "postcircumfix" => Some(DEFAULT_POSTCIRCUMFIX),
        _ => None,
    }
}

/// The precedence hash an operator routine named `name` answers when nothing
/// declared one for it: the built-in operator's, else its category's default.
// Cost: O(log n + m), n = built-in operator count, m = name length.
pub(crate) fn for_name(name: &str) -> Option<PrecEntries> {
    let (category, symbol) = split_op_name(name)?;
    builtin(category, symbol).or_else(|| category_default(category))
}

/// The routine name an `is equiv/tighter/looser` argument refers to:
/// `&[+]`, `&infix:<+>`, `infix:<+>` and a bare `+` (from `is tighter<+>`)
/// all name `infix:<+>`.
// Cost: O(n), n = argument length.
pub(crate) fn trait_target_name(arg: &str) -> String {
    let op = arg.trim();
    let op = op.strip_prefix('&').unwrap_or(op);
    if let Some(symbol) = op.strip_prefix('[').and_then(|s| s.strip_suffix(']')) {
        return format!("infix:<{symbol}>");
    }
    if split_op_name(op).is_some() {
        return op.to_string();
    }
    format!("infix:<{op}>")
}

/// An owned precedence hash, sorted by key.
#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub(crate) struct OpPrec(Vec<(String, String)>);

impl OpPrec {
    pub(crate) fn from_entries(entries: PrecEntries) -> Self {
        Self(
            entries
                .iter()
                .map(|(k, v)| (k.to_string(), v.to_string()))
                .collect(),
        )
    }

    pub(crate) fn entries(&self) -> impl Iterator<Item = (&str, &str)> {
        self.0.iter().map(|(k, v)| (k.as_str(), v.as_str()))
    }

    fn set(&mut self, key: &str, value: String) {
        match self.0.binary_search_by(|(k, _)| k.as_str().cmp(key)) {
            Ok(idx) => self.0[idx].1 = value,
            Err(idx) => self.0.insert(idx, (key.to_string(), value)),
        }
    }

    /// Apply an `is equiv` / `is tighter` / `is looser` trait to the hash of
    /// the operator it names.
    // Cost: O(k), k = entry count.
    pub(crate) fn relative_to(relation: &str, mut base: OpPrec) -> OpPrec {
        let mark = match relation {
            "tighter" => '@',
            "looser" => ':',
            _ => return base,
        };
        if let Some((_, prec)) = base.0.iter_mut().find(|(k, _)| k == "prec") {
            match prec.find('=') {
                Some(idx) => prec.insert(idx, mark),
                None => prec.push(mark),
            }
        }
        base.set("assoc", "left".to_string());
        base
    }

    /// Apply an `is assoc<...>` trait.
    pub(crate) fn with_assoc(mut self, assoc: &str) -> OpPrec {
        self.set("assoc", assoc.to_string());
        self
    }

    /// The `__prec` trait argument the parser hands to the runtime.
    pub(crate) fn encode(&self) -> String {
        let mut out = String::new();
        for (idx, (k, v)) in self.0.iter().enumerate() {
            if idx > 0 {
                out.push('\u{1e}');
            }
            out.push_str(k);
            out.push('\u{1f}');
            out.push_str(v);
        }
        out
    }

    /// The inverse of [`Self::encode`].
    pub(crate) fn decode(text: &str) -> OpPrec {
        let mut entries: Vec<(String, String)> = text
            .split('\u{1e}')
            .filter_map(|pair| pair.split_once('\u{1f}'))
            .map(|(k, v)| (k.to_string(), v.to_string()))
            .collect();
        entries.sort();
        OpPrec(entries)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn builtin_table_is_sorted() {
        let table = builtin_table::BUILTIN;
        for pair in table.windows(2) {
            assert!(
                (pair[0].0, pair[0].1) < (pair[1].0, pair[1].1),
                "{:?} before {:?}",
                (pair[0].0, pair[0].1),
                (pair[1].0, pair[1].1)
            );
        }
    }

    #[test]
    fn names_and_relations() {
        assert_eq!(split_op_name("infix:«<=»"), Some(("infix", "<=")));
        assert_eq!(split_op_name("foo"), None);
        assert_eq!(trait_target_name("&[~]"), "infix:<~>");
        assert_eq!(trait_target_name("+"), "infix:<+>");
        assert_eq!(trait_target_name("&prefix:<->"), "prefix:<->");
        let additive = OpPrec::from_entries(for_name("infix:<+>").unwrap());
        let tighter = OpPrec::relative_to("tighter", additive.clone());
        assert_eq!(
            tighter.encode(),
            "assoc\u{1f}left\u{1e}dba\u{1f}additive\u{1e}prec\u{1f}t@="
        );
        assert_eq!(OpPrec::decode(&tighter.encode()), tighter);
        let looser = OpPrec::relative_to(
            "looser",
            OpPrec::from_entries(for_name("infix:<**>").unwrap()),
        );
        assert_eq!(
            looser.entries().find(|(k, _)| *k == "prec"),
            Some(("prec", "w:="))
        );
        assert_eq!(
            looser.entries().find(|(k, _)| *k == "assoc"),
            Some(("assoc", "left"))
        );
    }
}
