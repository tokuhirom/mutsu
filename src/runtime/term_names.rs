//! The term namespace for sigil-less `constant`s.
//!
//! In Raku `constant b = 256` declares a *term* `b`; `my $b` declares a
//! `$`-sigiled variable. They are different symbols, so a same-named scalar —
//! a parameter, a `my`, the value a `where` clause is testing — can neither
//! see nor shadow the constant, and `b` keeps meaning `256` inside
//! `sub g(Int $b where $b == b div 8)`.
//!
//! mutsu stores a scalar `$b` sigil-stripped, under the key `b`, in both a
//! compiled frame's `local_map` and the runtime `env`. A sigil-less constant
//! used to be stored under that very key, so the two symbols were one
//! ([#9962](https://github.com/tokuhirom/mutsu/issues/9962)): the later
//! declaration or binding won, and a bare `b` read whichever it was.
//!
//! The fix is a namespace, as it was for enum keys
//! (`runtime::enum_bare_names`, #7914): a sigil-less constant is stored under
//! [`term_key`] — its name behind a `\` prefix, Raku's own spelling of a
//! sigil-less declaration (`my \b`), which no scalar key can start with. The
//! compiler allocates its local slot under that key, the runtime `env` and the
//! module-scope tables hold it under that key, and only term resolution (a
//! bare `b`, `::('b')`, `MY::<b>`, an export) translates between the spelling
//! and the key.
//!
//! Unlike the enum-key prefix this one is deliberately NOT a `__mutsu_`
//! metadata key: a constant is a user-visible lexical, so everything that
//! carries user lexicals around — closure capture, thread clones, block-scope
//! restore, package-block rollback (the key holds no `::`, so it is dropped on
//! package exit exactly like the plain key was) — must keep carrying it.
//!
//! Qualified package stores (`Pkg::b`) are unchanged: a qualified constant and
//! an `our $b` of the same package still share a key. That collision needs no
//! lexical scoping to hit and is left for the package-stash work.
// TODO: sigil-less `my \x` bindings and `\x` parameters still share the scalar
// key space; moving them here is the other half of #9962's "decide once".

use crate::ast::Expr;
use crate::runtime::Interpreter;
use crate::symbol::Symbol;
use crate::value::{Value, ValueView};

/// The prefix a sigil-less constant's storage key carries.
pub(crate) const TERM_PREFIX: char = '\\';

/// The storage key of the sigil-less constant spelled `name`.
///
/// A qualified name (`Pkg::b`) is returned unchanged: package stores are not
/// part of this namespace.
pub(crate) fn term_key(name: &str) -> String {
    if crate::runtime::utils::has_double_colon(name) {
        return name.to_string();
    }
    let mut key = String::with_capacity(name.len() + 1);
    key.push(TERM_PREFIX);
    key.push_str(name);
    key
}

/// [`term_key`] as a pre-interned [`Symbol`], memoized per name so a hot
/// bareword read pays no `format!` or string hash.
// Cost: O(1) expected.
pub(crate) fn term_key_sym(name: Symbol) -> Symbol {
    thread_local! {
        static KEYS: std::cell::RefCell<rustc_hash::FxHashMap<Symbol, Symbol>> =
            std::cell::RefCell::new(rustc_hash::FxHashMap::default());
    }
    if let Some(sym) = KEYS.with(|c| c.borrow().get(&name).copied()) {
        return sym;
    }
    let sym = name.with_str(|n| Symbol::intern(&term_key(n)));
    KEYS.with(|c| {
        c.borrow_mut().insert(name, sym);
    });
    sym
}

/// The spelling of a term-namespace storage key (`\b` → `b`), or `None` when
/// `key` is not one.
// Cost: O(1).
pub(crate) fn term_spelling(key: &str) -> Option<&str> {
    key.strip_prefix(TERM_PREFIX)
        .filter(|rest| !rest.is_empty())
}

/// Whether a `VarDecl` named `name` with `custom_traits` declares a
/// sigil-less constant (`constant b`, `my constant b`, `our constant b`),
/// the declaration stored in the term namespace. A `constant $b` is a
/// scalar — the parser strips its `$` too, but records it in
/// `__constant_sigil` — and `@`/`%`/`&` constants keep their sigil in the
/// name, so none of those are terms.
// Cost: O(t), t = custom traits on the declaration.
pub(crate) fn is_term_constant_decl(name: &str, custom_traits: &[(String, Option<Expr>)]) -> bool {
    if name.is_empty()
        || name.starts_with(['$', '@', '%', '&', '*', '!', '.', '?', '^', '='])
        || crate::runtime::utils::has_double_colon(name)
        || !custom_traits.iter().any(|(t, _)| t == "__constant")
    {
        return false;
    }
    let sigil = custom_traits
        .iter()
        .find(|(t, _)| t == "__constant_sigil")
        .and_then(|(_, e)| match e {
            Some(Expr::Literal(lit)) => match lit.view() {
                ValueView::Str(s) => Some(s.is_empty()),
                _ => None,
            },
            _ => None,
        });
    sigil.unwrap_or(true)
}

/// The storage key of a `VarDecl` named `name`: its term key for a sigil-less
/// constant, `name` itself for every other declaration. Every compile path
/// that reads a just-declared variable back BY its declaration's name (a
/// block's tail value, a declaration used as an argument) goes through here.
// Cost: O(t + |name|), t = custom traits on the declaration.
pub(crate) fn decl_storage_name(name: &str, custom_traits: &[(String, Option<Expr>)]) -> String {
    if is_term_constant_decl(name, custom_traits) {
        term_key(name)
    } else {
        name.to_string()
    }
}

/// [`decl_storage_name`] for a statement, or `None` when it is not a `VarDecl`.
// Cost: O(t + |name|), t = custom traits on the declaration.
pub(crate) fn stmt_decl_storage_name(stmt: &crate::ast::Stmt) -> Option<String> {
    match stmt {
        crate::ast::Stmt::VarDecl {
            name,
            custom_traits,
            ..
        } => Some(decl_storage_name(name, custom_traits)),
        _ => None,
    }
}

impl Interpreter {
    /// The value of the sigil-less constant spelled `name`, if one is in
    /// scope: its live `env` binding, else — for an `our`-scoped constant
    /// declared in a block that has since exited — its package store. This is
    /// how term resolution reaches a constant; a plain probe under `name`
    /// finds a same-named `$`-scalar instead.
    // Cost: O(1) expected.
    pub(crate) fn term_value(&self, name: &str) -> Option<&Value> {
        if name.is_empty()
            || name.starts_with(['$', '@', '%', '&', TERM_PREFIX])
            || crate::runtime::utils::has_double_colon(name)
        {
            return None;
        }
        let key = term_key_sym(Symbol::intern(name));
        self.env()
            .get_sym(key)
            .or_else(|| self.get_our_var(key.as_str()))
    }

    /// [`Self::term_value`], falling back to the running module's own (or
    /// imported) module-scope copy of the constant, and to the enclosing
    /// package blocks' lexicals — what a module or package routine sees once
    /// the body that declared the constant has exited.
    // Cost: O(1) expected, plus O(p) for the module-scope probe, p = packages the running routine could belong to.
    pub(crate) fn term_binding(&self, name: &str) -> Option<Value> {
        if let Some(v) = self.term_value(name) {
            return Some(v.clone());
        }
        if name.is_empty()
            || name.starts_with(['$', '@', '%', '&', TERM_PREFIX])
            || crate::runtime::utils::has_double_colon(name)
        {
            return None;
        }
        let key = term_key(name);
        self.module_imported_lexical(&key)
            .or_else(|| self.module_scope_lexical(&key))
            .cloned()
            // A package block's own `my constant`, kept for its routines in
            // `package_lexicals` once the block has exited.
            .or_else(|| self.package_chain_var_fallback(&key))
    }

    /// What a bare `name` written where a TYPE goes is bound to: a sigil-less
    /// constant ([`Self::term_binding`]) first, else a plain `env` binding (an
    /// imported short type name, a `::T` capture). A constant used as a type
    /// alias or a value constraint is found here even when a same-named
    /// `$`-scalar is in scope (#9962).
    // Cost: O(1) expected, plus [`Self::term_binding`]'s module-scope probe on a miss.
    pub(crate) fn type_name_binding(&self, name: &str) -> Option<Value> {
        self.term_binding(name)
            .or_else(|| self.env().get(name).cloned())
    }
}
