//! Declarator doc attachment: the parser, not a source re-scan, decides which
//! declaration a `#|` / `#=` comment documents (ADR-0134).
//!
//! Everything is keyed by source offset, so backtracking and memoization
//! cannot disturb it (a recorded fact is a fact about the source text,
//! recorded again identically by every parse that reaches it):
//!
//! 1. `ws` reports every declarator comment it skips ([`note_comment`]). A
//!    `#|` remembers the offset of the token that follows it.
//! 2. Each declarator parser, once it has parsed its declaration, records
//!    the declaration's extent ([`attach_stmt`], [`attach_anon`],
//!    [`attach_param`]): where it starts, where it ends, and how far past its
//!    end a `#=` still documents it (one `;`, `,` or invocant `:`, then
//!    whitespace).
//! 3. When the compilation unit is parsed, [`finish_unit`] hands out the
//!    comments, as Rakudo does:
//!    - a `#|` documents the next declaration to start after it -- wherever
//!      that is, so `#| doc` above `my $x = anon sub {}` documents the
//!      variable, while `my $x = #| doc\n anon sub {}` and `#| doc\n is sub
//!      {}, ...` document the sub;
//!    - a `#=` documents the declaration that started last before it among
//!      those whose extent (or the whitespace after it) it lies in, so in
//!      `class C {\n#= doc\n has $.a; #= attr\n}` the first comment is
//!      `C`'s and the second `$!a`'s.
//!
//!    Variables are declarations too: they take their comments (so no later
//!    declaration does), though nothing reads a variable's documentation yet.
//!    It then names every documented declaration by the structure the parse
//!    recorded -- its enclosing packages, its multi candidate index, its
//!    owning routine -- and fills the documentation slots of anonymous code
//!    nodes.
//!
//! Recording is switched off for a unit whose source contains no `#|`/`#=`
//! at all, so an ordinary parse pays one substring search.

mod resolve;

use std::cell::RefCell;
use std::collections::BTreeMap;

use super::helpers::ws;
use super::primary::source_offset;
use crate::ast::{Expr, ParamDef, Stmt};
use crate::decl_doc::{DocComment, DocSlot};

/// A `#|` or `#=` comment the parser skipped.
struct Comment {
    trailing: bool,
    text: String,
    /// For a `#|`: the source offset of the token after it (and after any
    /// further comments). `usize::MAX` until the `ws` call that skipped it
    /// reaches that token.
    next_token: usize,
}

/// What a declaration is, as far as naming its documentation needs.
#[derive(Clone, Debug)]
pub(in crate::parser) enum SiteIdent {
    /// class/role/grammar/module/package (`container`), or enum/subset.
    Package {
        name: String,
        container: bool,
        is_role: bool,
    },
    Routine {
        name: String,
        /// `Method` / `Submethod`; `None` for a sub.
        callable: Option<&'static str>,
        multi: bool,
        proto: bool,
    },
    GrammarRule {
        name: String,
    },
    Attr {
        /// `$!name`, `@!name`, ...
        sigiled: String,
    },
    Param {
        /// `$name`, or a bare sigil for an anonymous parameter.
        sigiled: String,
    },
    Anon {
        block: bool,
        callable: Option<&'static str>,
        return_type: Option<String>,
        /// The node's documentation slot, filled by [`finish_unit`].
        slot: DocSlot,
    },
    /// A variable: takes the comments that document it, reports nothing.
    Variable,
}

/// One declaration the parse recorded, keyed by its start offset.
struct Site {
    end: usize,
    /// End of the whitespace after the declaration in which a `#=` still
    /// documents it.
    claim_end: usize,
    ident: SiteIdent,
}

struct Table {
    /// Source length: a `unit` package extends to it.
    len: usize,
    comments: BTreeMap<usize, Comment>,
    /// `#|` comments whose following token is not known yet.
    pending_leading: Vec<usize>,
    sites: BTreeMap<usize, Site>,
}

thread_local! {
    /// One entry per compilation-unit parse in progress (nested parses —
    /// a BEGIN-time EVAL, a module load — push their own). `None` when the
    /// unit has no declarator comments.
    static TABLES: RefCell<Vec<Option<Table>>> = const { RefCell::new(Vec::new()) };
    /// The documented declarations of the unit parsed last.
    static LAST_UNIT_DOCS: RefCell<Vec<DocComment>> = const { RefCell::new(Vec::new()) };
}

/// Pops the unit's table when its parse ends, however it ends.
pub(in crate::parser) struct UnitGuard;

impl Drop for UnitGuard {
    fn drop(&mut self) {
        TABLES.with(|t| t.borrow_mut().pop());
    }
}

/// Start collecting declarator docs for the compilation unit `source`.
pub(in crate::parser) fn begin_unit(source: &str) -> UnitGuard {
    let table = (source.contains("#|") || source.contains("#=")).then(|| Table {
        len: source.len(),
        comments: BTreeMap::new(),
        pending_leading: Vec::new(),
        sites: BTreeMap::new(),
    });
    TABLES.with(|t| t.borrow_mut().push(table));
    LAST_UNIT_DOCS.with(|d| d.borrow_mut().clear());
    UnitGuard
}

/// A nested best-effort parse of another buffer: record nothing until the
/// guard drops.
pub(in crate::parser) fn mute_unit() -> UnitGuard {
    TABLES.with(|t| t.borrow_mut().push(None));
    UnitGuard
}

/// Whether the unit being parsed records declarator docs at all.
pub(in crate::parser) fn active() -> bool {
    TABLES.with(|t| matches!(t.borrow().last(), Some(Some(_))))
}

fn with_table<R>(f: impl FnOnce(&mut Table) -> R) -> Option<R> {
    TABLES.with(|t| t.borrow_mut().last_mut().and_then(Option::as_mut).map(f))
}

/// The documented declarations of the compilation unit parsed last, in source
/// order. Taken by the runtime to build `.WHY` and `$=pod`.
pub(crate) fn take_unit_docs() -> Vec<DocComment> {
    LAST_UNIT_DOCS.with(|d| std::mem::take(&mut *d.borrow_mut()))
}

/// Put back docs taken with [`take_unit_docs`] (a precompiled unit replays
/// the ones its parse produced).
pub(crate) fn set_unit_docs(docs: Vec<DocComment>) {
    LAST_UNIT_DOCS.with(|d| *d.borrow_mut() = docs);
}

/// Split a declarator comment at the start of `input` into its kind and text.
/// Returns `(trailing, text)`; `None` when `input` is an ordinary comment:
/// `#|`/`#=` must be followed by horizontal whitespace (the line form) or an
/// opening bracket (the block form, whose extent `after` gives), as in
/// Rakudo, so `#====` and `#|x` are plain comments.
fn comment_text(input: &str, after: &str) -> Option<(bool, String)> {
    let trailing = input.starts_with("#=");
    if !trailing && !input.starts_with("#|") {
        return None;
    }
    let body = &input[2..input.len() - after.len()];
    let text = if body.starts_with([' ', '\t']) {
        body.trim().to_string()
    } else {
        // Block form: strip the opening and closing bracket runs.
        let open = body.chars().next()?;
        let run = body.chars().take_while(|&c| c == open).count();
        let inner = body.get(run * open.len_utf8()..)?;
        let inner = inner.get(..inner.len().checked_sub(run * open.len_utf8())?)?;
        inner.split_whitespace().collect::<Vec<_>>().join(" ")
    };
    (!text.is_empty()).then_some((trailing, text))
}

/// Record a declarator comment `ws` skipped: `input` starts at the `#`,
/// `after` is the input after the comment. Returns true for a `#|`, whose
/// following token [`note_next_token`] must then supply.
pub(in crate::parser) fn note_comment(input: &str, after: &str) -> bool {
    if !active() {
        return false;
    }
    let Some((trailing, text)) = comment_text(input, after) else {
        return false;
    };
    let Some(at) = source_offset(input) else {
        return false;
    };
    with_table(|t| {
        if let std::collections::btree_map::Entry::Vacant(slot) = t.comments.entry(at) {
            slot.insert(Comment {
                trailing,
                text,
                next_token: usize::MAX,
            });
            if !trailing {
                t.pending_leading.push(at);
            }
        }
    });
    !trailing
}

/// `ws` stopped at `input`, the token after the `#|` comments it noted.
pub(in crate::parser) fn note_next_token(input: &str) {
    let Some(at) = source_offset(input) else {
        return;
    };
    with_table(|t| {
        for pending in std::mem::take(&mut t.pending_leading) {
            if let Some(comment) = t.comments.get_mut(&pending) {
                comment.next_token = at;
            }
        }
    });
}

/// End of the whitespace after a declaration ending at `end` in which a `#=`
/// still documents it: past one `;` or `,` (`has $.a; #= doc`,
/// `Str $p, #= doc`) or an invocant marker (`$self: #= doc`).
fn claim_end(end: &str) -> &str {
    let after_ws = |s| ws(s).map_or(s, |(r, _)| r);
    let r = after_ws(end);
    match r.as_bytes().first() {
        Some(b';' | b',') => after_ws(&r[1..]),
        Some(b':') if r[1..].starts_with(char::is_whitespace) => after_ws(&r[1..]),
        _ => r,
    }
}

/// Record the declaration spanning `start..end` (to the end of the source
/// when `extends_to_eof`).
fn attach(start: &str, end: &str, ident: SiteIdent, extends_to_eof: bool) {
    let (Some(s), Some(e)) = (source_offset(start), source_offset(end)) else {
        return;
    };
    // A block's documentation is written inside it (`my $b = {;\n#= doc\n}`):
    // a `#=` after a block used as a value -- the `where { ... }` of a
    // `subset` -- documents the declaration the block is part of.
    let claim = match ident {
        SiteIdent::Anon { block: true, .. } => e,
        _ => source_offset(claim_end(end)).unwrap_or(e),
    };
    with_table(|t| {
        let e = if extends_to_eof { t.len } else { e };
        t.sites.insert(
            s,
            Site {
                end: e,
                claim_end: claim.max(e),
                ident,
            },
        );
    });
}

/// What a statement declares, if it is a documentable declaration.
fn stmt_ident(stmt: &Stmt) -> Option<(SiteIdent, bool)> {
    let routine = |name: &crate::symbol::Symbol, callable, multi, proto| SiteIdent::Routine {
        name: name.resolve(),
        callable,
        multi,
        proto,
    };
    let package = |name: &crate::symbol::Symbol, container, is_role| SiteIdent::Package {
        name: name.resolve(),
        container,
        is_role,
    };
    let ident = match stmt {
        Stmt::SubDecl { name, multi, .. } => routine(name, None, *multi, false),
        Stmt::ProtoDecl {
            name, is_method, ..
        } => routine(name, is_method.then_some("Method"), false, true),
        Stmt::MethodDecl {
            name,
            multi,
            is_submethod,
            ..
        } => routine(
            name,
            Some(if *is_submethod { "Submethod" } else { "Method" }),
            *multi,
            false,
        ),
        Stmt::TokenDecl { name, .. } | Stmt::RuleDecl { name, .. } => SiteIdent::GrammarRule {
            name: name.resolve(),
        },
        Stmt::ClassDecl { name, .. } => package(name, true, false),
        Stmt::RoleDecl { name, .. } => package(name, true, true),
        Stmt::Package { name, is_unit, .. } => return Some((package(name, true, false), *is_unit)),
        Stmt::EnumDecl { name, .. } | Stmt::SubsetDecl { name, .. } => package(name, false, false),
        Stmt::HasDecl { name, sigil, .. } => SiteIdent::Attr {
            sigiled: format!("{sigil}!{}", name.resolve()),
        },
        Stmt::VarDecl { .. } => SiteIdent::Variable,
        // `class Foo:ver<1> { }` and similar come back as their metadata
        // setters followed by the declaration itself.
        Stmt::SyntheticBlock(stmts) => return stmts.last().and_then(stmt_ident),
        _ => return None,
    };
    let named = match &ident {
        SiteIdent::Package { name, .. }
        | SiteIdent::Routine { name, .. }
        | SiteIdent::GrammarRule { name } => !name.is_empty(),
        _ => true,
    };
    named.then_some((ident, false))
}

/// A statement parsed from `start` to `end`: record it if it is a declaration.
/// A unit-scoped declaration (`unit module M;`) extends to the end of the
/// source, so `extends_to_eof` is forced for it.
pub(in crate::parser) fn attach_stmt(start: &str, end: &str, stmt: &Stmt) {
    if !active() {
        return;
    }
    if let Some((ident, unit)) = stmt_ident(stmt) {
        attach(start, end, ident, unit);
    }
}

/// `unit class`/`unit role`/`unit grammar`: the declaration's extent is the
/// rest of the compilation unit, parsed after it.
pub(in crate::parser) fn attach_unit_stmt(start: &str, stmt: &Stmt) {
    if !active() {
        return;
    }
    if let Some((ident, _)) = stmt_ident(stmt) {
        attach(start, start, ident, true);
    }
}

/// An anonymous routine or block term parsed from `start` to `end`: record
/// it, and give the node the documentation slot [`finish_unit`] fills; the
/// compiler carries it to the code object, where `.WHY` reads it.
pub(in crate::parser) fn attach_anon(start: &str, end: &str, expr: &mut Expr) {
    // Checked before `active()`: this runs for every primary term parsed.
    if !matches!(expr, Expr::AnonSub { .. } | Expr::AnonSubParams { .. }) || !active() {
        return;
    }
    let slot = DocSlot::pending();
    let ident = match expr {
        Expr::AnonSub { is_block, doc, .. } => {
            *doc = slot.clone();
            SiteIdent::Anon {
                block: *is_block,
                callable: None,
                return_type: None,
                slot,
            }
        }
        Expr::AnonSubParams {
            return_type,
            declarator,
            custom_traits,
            ..
        } => {
            custom_traits.set_doc_slot(slot.clone());
            SiteIdent::Anon {
                block: !declarator.is_routine(),
                callable: match declarator {
                    crate::ast::RoutineDeclarator::Method => Some("Method"),
                    crate::ast::RoutineDeclarator::Submethod => Some("Submethod"),
                    _ => None,
                },
                return_type: return_type.clone(),
                slot,
            }
        }
        _ => return,
    };
    attach(start, end, ident, false);
}

/// A signature parameter parsed from `start` to `end`.
pub(in crate::parser) fn attach_param(start: &str, end: &str, param: &ParamDef) {
    if !active() {
        return;
    }
    let sig = crate::value::signature::param_def_to_sig_param(param);
    let sigiled = format!("{}{}", sig.sigil, sig.name);
    attach(start, end, SiteIdent::Param { sigiled }, false);
}

/// Name every documented declaration of the unit being parsed and publish
/// them for [`take_unit_docs`].
pub(in crate::parser) fn finish_unit() {
    let docs = with_table(|t| resolve::resolve(t)).unwrap_or_default();
    LAST_UNIT_DOCS.with(|d| *d.borrow_mut() = docs);
}
