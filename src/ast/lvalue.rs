//! The variable-name atoms and lvalue/index spines shared by the parser and
//! the compiler (#10468): the one place that spells "`$x` is stored as `x`,
//! `@a` as `@a`", "which variable does this subscript chain write through"
//! and "which wrappers is an lvalue transparent through". Before this module
//! the `Var → "x"` / `ArrayVar → "@a"` / `HashVar → "%h"` match was
//! hand-written at ~45 call sites, and four lvalue spines peeled four
//! different, partly accidental, sets of wrappers.

use super::{Expr, Stmt};
use crate::token_kind::TokenKind;

impl Expr {
    /// The env storage key of a plain container variable: `$x` → `x` (a
    /// scalar is stored sigil-less), `@a` → `@a`, `%h` → `%h`. Twigils stay
    /// part of the name (`$!x` → `!x`). `None` for every other expression,
    /// including `&f` (see [`Expr::var_key`]).
    // Cost: O(|name|).
    pub fn container_var_key(&self) -> Option<String> {
        match self {
            Expr::Var(name) => Some(name.clone()),
            Expr::ArrayVar(name) => Some(format!("@{name}")),
            Expr::HashVar(name) => Some(format!("%{name}")),
            _ => None,
        }
    }

    /// [`Expr::container_var_key`], plus a code variable `&f` → `&f`.
    // Cost: O(|name|).
    pub fn var_key(&self) -> Option<String> {
        match self {
            Expr::CodeVar(name) => Some(format!("&{name}")),
            other => other.container_var_key(),
        }
    }

    /// The source spelling of a plain container variable, as a diagnostic
    /// names it: `$x`, `@a`, `%h` (the reserved `$self` lexical key already
    /// carries its sigil). `None` for every other expression.
    // Cost: O(|name|).
    pub fn sigiled_var_name(&self) -> Option<String> {
        match self {
            Expr::Var(name) => Some(crate::env::sigiled_scalar_name(name)),
            Expr::ArrayVar(_) | Expr::HashVar(_) => self.container_var_key(),
            _ => None,
        }
    }

    /// The source key of a single literal-subscripted element of a plain
    /// variable, `@a[1]` → `"@a\0idx\01"`, `%h<k>` → `"%h\0idx\0k"`: the
    /// encoding bind metadata and the `=:=` container-identity check use to
    /// name one element. `None` for any other shape.
    // Cost: O(|name| + |index|).
    pub fn element_source_key(&self) -> Option<String> {
        let Expr::Index { target, index, .. } = self else {
            return None;
        };
        let target_name = target.container_var_key()?;
        let idx_str = match index.as_ref() {
            Expr::Literal(lit) => match lit.view() {
                crate::value::ValueView::Int(n) => n.to_string(),
                crate::value::ValueView::Str(s) => s.to_string(),
                _ => return None,
            },
            _ => return None,
        };
        Some(format!("{target_name}\x00idx\x00{idx_str}"))
    }

    /// The root of a chain of `Index` subscripts (`@a` for `@a[0]<k>[1]`);
    /// the expression itself when it is not a subscript.
    // Cost: O(d), d = subscript depth.
    pub fn index_root(&self) -> &Expr {
        let mut expr = self;
        while let Expr::Index { target, .. } = expr {
            expr = target;
        }
        expr
    }

    /// [`Expr::index_root`], also appending each subscript's `(index,
    /// is_positional)` to `path` in source order -- innermost (closest to the
    /// root) first, so `@a[0]<k>` appends `[(0, true), (k, false)]`.
    // Cost: O(d), d = subscript depth.
    pub fn index_path<'a>(&'a self, path: &mut Vec<(&'a Expr, bool)>) -> &'a Expr {
        let start = path.len();
        let mut expr = self;
        while let Expr::Index {
            target,
            index,
            is_positional,
        } = expr
        {
            path.push((index, *is_positional));
            expr = target;
        }
        path[start..].reverse();
        expr
    }

    /// The variable an lvalue expression ultimately writes through, looking
    /// through exactly the wrappers `peel` names. The root is a plain
    /// container variable ([`Expr::container_var_key`]), the name an
    /// assignment or declaration writes ([`LvaluePeel::ASSIGN`],
    /// [`LvaluePeel::DECL`]), or a sigil-less term ([`LvaluePeel::SIGILLESS`]),
    /// which only the compiler can resolve to its storage key.
    ///
    /// The peel set is the caller's to choose because the wrappers are not
    /// interchangeable: an `ASSIGN`/`DECL` root has a side effect, so only a
    /// caller that evaluates the target before writing through the name may
    /// accept one; `temp (...)` saves the variable when evaluated, so the
    /// same holds for `TEMP`.
    // Cost: O(w), w = number of wrappers peeled (plus the declaration's
    // traits for a `DECL` root, see `Stmt::declared_var_key`).
    pub fn lvalue_root(&self, peel: LvaluePeel) -> Option<LvalueRoot<'_>> {
        match self {
            Expr::Var(_) | Expr::ArrayVar(_) | Expr::HashVar(_) => {
                self.container_var_key().map(LvalueRoot::Key)
            }
            Expr::BareWord(name) if peel.has(LvaluePeel::SIGILLESS) => {
                Some(LvalueRoot::Sigilless(name))
            }
            Expr::Grouped(inner) if peel.has(LvaluePeel::GROUPED) => inner.lvalue_root(peel),
            Expr::AssignExpr { name, .. } if peel.has(LvaluePeel::ASSIGN) => {
                Some(LvalueRoot::Key(name.clone()))
            }
            // A compound-assignment marker (`$x += 1`) is transparent: its
            // expansion carries the lvalue it writes.
            Expr::CompoundAssign { expanded, .. } if peel.has(LvaluePeel::ASSIGN) => {
                expanded.lvalue_root(peel)
            }
            Expr::DoStmt(stmt) if peel.has(LvaluePeel::DECL) => {
                stmt.declared_var_key().map(LvalueRoot::Key)
            }
            Expr::Binary {
                left,
                op: TokenKind::AndThen | TokenKind::OrElse | TokenKind::NotAndThen,
                ..
            } if peel.has(LvaluePeel::TOPIC_CHAIN) => left.lvalue_root(peel),
            Expr::Call { name, args } if peel.has(LvaluePeel::TEMP) && name == "temp" => {
                args.first()?.lvalue_root(peel)
            }
            _ => None,
        }
    }
}

impl Stmt {
    /// The storage key of the variable a statement declares or assigns, as
    /// it appears in an expression position (`(my $x = 1)`, `(my \x = ...)`,
    /// `($x = 1)` lowered to a statement): a `VarDecl`'s storage name (the
    /// term key for a sigil-less `constant`), an `Assign`'s target, or the
    /// one such statement of a `SyntheticBlock` lowering a single declaration
    /// (`my \x = ...` with its markers, `my $x := ...`). A `SyntheticBlock`
    /// that declares several variables (`(my $y = 1, my $z = 2)`) is a list,
    /// not one variable, so it has none.
    // Cost: O(s + t), s = statements of a SyntheticBlock (recursively),
    // t = custom traits on the declaration.
    pub fn declared_var_key(&self) -> Option<String> {
        match self {
            Stmt::VarDecl { .. } => crate::runtime::term_names::stmt_decl_storage_name(self),
            Stmt::Assign { name, .. } => Some(name.clone()),
            Stmt::SyntheticBlock(stmts) => {
                let mut keys = stmts.iter().filter_map(Stmt::declared_var_key);
                let key = keys.next()?;
                keys.next().is_none().then_some(key)
            }
            _ => None,
        }
    }
}

/// The root [`Expr::lvalue_root`] found.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LvalueRoot<'a> {
    /// The env storage key (`x`, `@a`, `%h`, a declaration's storage name).
    Key(String),
    /// A sigil-less term (`x` of `my \x`, or of `constant x`): its storage
    /// key depends on what the name is bound to in scope, which only the
    /// compiler knows.
    Sigilless(&'a str),
}

impl LvalueRoot<'_> {
    /// The storage key, resolving a sigil-less root by its spelling. Callers
    /// that can see compiler scope resolve a sigil-less `constant` to its
    /// term key instead (`Compiler::lvalue_root_key`).
    // Cost: O(|name|).
    pub fn into_spelled_key(self) -> String {
        match self {
            LvalueRoot::Key(key) => key,
            LvalueRoot::Sigilless(name) => name.to_string(),
        }
    }
}

/// The wrappers [`Expr::lvalue_root`] looks through, as a small bit set.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LvaluePeel(u8);

impl LvaluePeel {
    /// `(...)` parenthesization.
    pub const GROUPED: Self = Self(1);
    /// An assignment expression `($x = ...)` and a compound-assignment
    /// marker `($x += ...)`.
    pub const ASSIGN: Self = Self(1 << 1);
    /// An inline declaration or statement assignment `(my $x = ...)`.
    pub const DECL: Self = Self(1 << 2);
    /// A sigil-less term (`x[0]` for `my \x` / `constant x`).
    pub const SIGILLESS: Self = Self(1 << 3);
    /// The left operand of `andthen` / `orelse` / `notandthen`.
    pub const TOPIC_CHAIN: Self = Self(1 << 4);
    /// `temp (...)`.
    pub const TEMP: Self = Self(1 << 5);

    // Cost: O(1).
    pub const fn union(self, other: Self) -> Self {
        Self(self.0 | other.0)
    }

    // Cost: O(1).
    pub const fn has(self, flag: Self) -> bool {
        self.0 & flag.0 == flag.0
    }
}

impl std::ops::BitOr for LvaluePeel {
    type Output = Self;
    // Cost: O(1).
    fn bitor(self, other: Self) -> Self {
        self.union(other)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn arr(name: &str) -> Expr {
        Expr::ArrayVar(name.to_string())
    }

    #[test]
    fn atoms() {
        assert_eq!(
            Expr::Var("x".into()).container_var_key().as_deref(),
            Some("x")
        );
        assert_eq!(arr("a").container_var_key().as_deref(), Some("@a"));
        assert_eq!(Expr::HashVar("h".into()).var_key().as_deref(), Some("%h"));
        assert_eq!(Expr::CodeVar("f".into()).container_var_key(), None);
        assert_eq!(Expr::CodeVar("f".into()).var_key().as_deref(), Some("&f"));
        assert_eq!(
            Expr::Var("x".into()).sigiled_var_name().as_deref(),
            Some("$x")
        );
        assert_eq!(
            Expr::Var(crate::env::LEX_SELF.into())
                .sigiled_var_name()
                .as_deref(),
            Some("$self")
        );
    }

    #[test]
    fn index_path_is_innermost_first() {
        let idx = |target: Expr, i: i64, pos: bool| Expr::Index {
            target: Box::new(target),
            index: Box::new(Expr::Literal(crate::value::Value::int(i))),
            is_positional: pos,
        };
        let e = idx(idx(arr("a"), 0, true), 1, false);
        let mut path = Vec::new();
        assert!(matches!(e.index_path(&mut path), Expr::ArrayVar(n) if n == "a"));
        assert!(matches!(e.index_root(), Expr::ArrayVar(n) if n == "a"));
        let flags: Vec<bool> = path.iter().map(|(_, p)| *p).collect();
        assert_eq!(flags, vec![true, false]);
    }

    #[test]
    fn peel_set_decides_the_root() {
        let grouped = Expr::Grouped(Box::new(arr("a")));
        assert_eq!(grouped.lvalue_root(LvaluePeel::ASSIGN), None);
        assert_eq!(
            grouped.lvalue_root(LvaluePeel::GROUPED),
            Some(LvalueRoot::Key("@a".into()))
        );
        let bare = Expr::BareWord("x".into());
        assert_eq!(bare.lvalue_root(LvaluePeel::ASSIGN), None);
        assert_eq!(
            bare.lvalue_root(LvaluePeel::SIGILLESS),
            Some(LvalueRoot::Sigilless("x"))
        );
        let temp = Expr::Call {
            name: crate::symbol::Symbol::intern("temp"),
            args: vec![arr("a")],
        };
        assert_eq!(temp.lvalue_root(LvaluePeel::ASSIGN), None);
        assert_eq!(
            temp.lvalue_root(LvaluePeel::TEMP),
            Some(LvalueRoot::Key("@a".into()))
        );
    }
}
