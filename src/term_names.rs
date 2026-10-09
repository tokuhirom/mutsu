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
use crate::symbol::Symbol;
use crate::value::ValueView;

/// The prefix a sigil-less constant's storage key carries.
pub(crate) const TERM_PREFIX: char = '\\';

/// The storage key of the sigil-less constant spelled `name` (an unqualified
/// name: package stores, `Pkg::b`, are not part of this namespace).
pub(crate) fn term_key(name: &str) -> String {
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
        || !custom_traits.iter().any(|(t, _)| t == "__constant")
        || crate::qualified::is_qualified(Symbol::intern(name))
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

/// The prefix a lexical (`my class`/`my role`) type's own `env` key carries.
/// `\u{1}` cannot start a scalar, term or enum key.
pub(crate) const LEXICAL_TYPE_PREFIX: char = '\u{1}';

/// The `env` key that holds the storage name of the lexical type spelled
/// `name`. A `my class foo` is also bound under the bare `foo`, the very key a
/// same-named `$foo` shares, so the bare binding alone cannot say which type
/// is in scope once the scalar overwrites it (#12109). Readers that must find
/// the type consult this key first.
pub(crate) fn lexical_type_key(name: &str) -> String {
    let mut key = String::with_capacity(name.len() + 1);
    key.push(LEXICAL_TYPE_PREFIX);
    key.push_str(name);
    key
}

/// Bind every lexical (`my class` / `my role`) type recorded under its
/// [`lexical_type_key`] in `env` to its bare name too, unless the bare name is
/// already bound.
///
/// A routine created inside a module keeps the type-only key in its captured
/// scope, but the bare binding is withdrawn from the importing scope once the
/// module is loaded. Code handed to `^add_method` runs as a method of an
/// unrelated class, so it must carry the bare binding itself or a `MC.new` in
/// its body no longer finds the module's `my class MC` (hide-methods).
// Cost: O(e), e = entries of `env`.
pub(crate) fn bind_lexical_types_by_bare_name(env: &mut crate::env::Env) {
    let missing: Vec<(String, crate::value::Value)> = env
        .iter()
        .filter_map(|(key, value)| {
            let bare = key.with_str(|k| k.strip_prefix(LEXICAL_TYPE_PREFIX).map(str::to_string))?;
            matches!(value.view(), crate::value::ValueView::Package(_))
                .then(|| (bare, value.clone()))
        })
        .filter(|(bare, _)| env.get(bare.as_str()).is_none())
        .collect();
    for (bare, value) in missing {
        env.insert(bare, value);
    }
}
