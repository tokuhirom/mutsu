//! The unit-level BEGIN prologue (ADR-0134, slices 1 to 3).
//!
//! Rakudo runs a `BEGIN` block as soon as the parser reaches its end, before
//! any run-time code of the compilation unit. The block sees every lexical in
//! its *static* state: declared, but with no run-time initializer applied.
//! mutsu parses a whole unit before running it, so it gets the same order by
//! moving the unit's BEGIN-time effects to the head of the unit, in source
//! order. The effects then run first, in the unit's own frame, and share its
//! lexical slots. No second environment exists to keep in sync.
//!
//! [`take_unit_prologue`] performs that split on a unit's top-level statement
//! list. Everything up to and including the last top-level BEGIN-time effect
//! is partitioned into two parts:
//!
//! - the **prologue**, which holds the `BEGIN` phasers themselves, the
//!   declarations a BEGIN can observe (`use`, routines, packages, types,
//!   `constant`), and the *static* half of each variable declaration;
//! - the **run-time remainder**: every other statement, plus the initializer
//!   of each split variable declaration as an assignment at its original
//!   position.
//!
//! A `use` (other than a positional pragma such as `use strict`) and a
//! `constant` are BEGIN-time effects too (slice 3), so the bound reaches the
//! last of them as well. Statements after the last effect are left as they
//! are. No effect observes them, so moving them would only change run-time
//! order without gaining anything.
//!
//! A class or package declaration moves without its body's run-time
//! statements: those stay at the declaration's position ([`package_body`]).
//! Rakudo composes the class at BEGIN time but runs `class A { say 2 }`'s
//! `say` at run time.
//!
//! BEGINs nested inside a top-level statement, and value-form BEGINs, are
//! lifted into the prologue by [`nested`] ahead of that statement.

mod nested;
mod nested_exports;
mod package_body;
mod package_phasers;
mod use_if;

use crate::ast::{Expr, PhaserKind, Stmt};
use std::collections::HashSet;
pub(crate) use use_if::if_condition_slot;
pub(crate) use nested::var_traits::{lift_package_var_traits, lift_unit_var_traits};
use use_if::{if_condition_check, next_if_condition_slot};

/// Split `stmts` (one compilation unit's top level) into its BEGIN prologue and
/// run-time remainder, as described in the module docs. The prologue is
/// returned, and `stmts` is left holding the remainder. If the unit has no
/// BEGIN-time effect, the prologue is empty and `stmts` is
/// untouched.
pub(crate) fn take_unit_prologue(stmts: &mut Vec<Stmt>, is_eval: bool) -> Vec<Stmt> {
    // Lift the BEGINs nested in each top-level statement first (slice 2):
    // each lifted effect joins the prologue just ahead of its statement.
    if is_eval {
        value_tail_begin(stmts);
    }
    let unit_names = unit_lexical_names(stmts);
    let mut unit = nested::UnitContext {
        is_eval,
        strict_off: false,
    };
    let mut lifted = nested::Lifted::default();
    let mut effects: Vec<Vec<Stmt>> = Vec::with_capacity(stmts.len());
    let mut package_effects: Vec<Vec<Stmt>> = Vec::with_capacity(stmts.len());
    // The compile-time composition of the `our` types declared in each
    // statement's code (#10494) is a BEGIN-time effect too. It is collected
    // after the lift, so a type declared in a lifted BEGIN registers there
    // alone.
    let mut type_shells: Vec<Option<Stmt>> = Vec::with_capacity(stmts.len());
    // The INIT and CHECK phasers of each type or package declaration move out
    // ahead of it (#10552). One that re-enters its package makes the
    // declaration a BEGIN-time effect, so the package is composed by then.
    let mut moved_slots = Vec::new();
    let mut moved_phasers: Vec<Vec<Stmt>> = Vec::with_capacity(stmts.len());
    let mut composed_early: Vec<bool> = Vec::with_capacity(stmts.len());
    let mut seen_runtime_stmt = false;
    for stmt in stmts.iter_mut() {
        let composes_after_runtime = seen_runtime_stmt && composes_roles(stmt);
        seen_runtime_stmt |= !crate::compiler::Compiler::runs_no_user_code(stmt);
        let mut moved = package_phasers::Moved::default();
        package_phasers::move_package_phasers(stmt, &mut moved);
        moved_slots.append(&mut moved.slots);
        let before = lifted.effects.len();
        nested::lift_in_stmt(stmt, &unit_names, unit, &mut lifted);
        // The phasers nested code gave up because they read a lexical of it
        // (#10562) follow the ones moved out of a declaration.
        moved.phasers.append(&mut lifted.phasers);
        composed_early.push(
            moved.needs_prologue
                || std::mem::take(&mut lifted.needs_prologue)
                || composes_after_runtime,
        );
        moved_phasers.push(moved.phasers);
        if let Some(off) = strict_pragma(stmt) {
            unit.strict_off = off;
        }
        effects.push(lifted.effects.split_off(before));
        // A BEGIN of a package's routine follows the declaration it re-enters
        // (#10328).
        package_effects.push(std::mem::take(&mut lifted.package_effects));
        let shells = crate::compiler::nested_type_decls(stmt);
        type_shells.push((!shells.is_empty()).then_some(Stmt::NestedTypeShells(shells)));
    }
    let decls = lifted.decls;
    // A `use` and a `constant` are BEGIN-time effects on their own (slice 3),
    // so the prologue reaches the last of them too.
    let trait_parents = DeferredTraitParents::scan(stmts);
    let last_effect = stmts
        .iter()
        .rposition(|stmt| is_begin_time_effect(stmt) || trait_parents.names_trait(stmt));
    let last_lifted = effects
        .iter()
        .zip(&package_effects)
        .rposition(|(before, after)| !before.is_empty() || !after.is_empty());
    let last_shell = type_shells.iter().rposition(Option::is_some);
    let last_composed = composed_early.iter().rposition(|early| *early);
    let Some(last) = last_effect
        .max(last_lifted)
        .max(last_shell)
        .max(last_composed)
    else {
        // No prologue: the moved phasers still precede their declarations, and
        // the cells and slots they read are declared ahead of them.
        let mut out = moved_slots;
        out.extend(decls);
        for (stmt, phasers) in std::mem::take(stmts).into_iter().zip(moved_phasers) {
            out.extend(phasers);
            out.push(stmt);
        }
        *stmts = out;
        return Vec::new();
    };
    // An EVAL'd unit's value is its last statement's. A trailing `constant`
    // moves into the prologue whole, so the run-time remainder would end on
    // an earlier statement; it ends on a read of the constant instead.
    let tail_constant_read = if is_eval {
        crate::ast::last_value_stmt_index(stmts, crate::ast::TailSkip::Markers)
            .and_then(|i| constant_read(&stmts[i]))
    } else {
        None
    };
    let mut tail = Vec::new();
    for (stmt, phasers) in stmts
        .split_off(last + 1)
        .into_iter()
        .zip(moved_phasers.split_off(last + 1))
    {
        tail.extend(phasers);
        tail.push(stmt);
    }
    let mut prologue = moved_slots;
    prologue.extend(decls);
    let mut rest = Vec::new();
    for ((((stmt, stmt_effects), late_effects), shells), phasers) in std::mem::take(stmts)
        .into_iter()
        .zip(effects)
        .zip(package_effects)
        .zip(type_shells)
        .zip(moved_phasers)
    {
        // A moved INIT or CHECK is a run-time statement of the unit, which the
        // unit's phaser reordering puts in place. It goes ahead of the
        // statement's line marker, which `partition_stmt` carries along.
        let at = rest.len() - usize::from(matches!(rest.last(), Some(Stmt::SetLine(..))));
        rest.splice(at..at, phasers);
        prologue.extend(stmt_effects);
        partition_stmt(stmt, &mut prologue, &mut rest);
        // After the statement's own declaration part: a class whose methods
        // declare the nested types is registered by then.
        prologue.extend(shells);
        prologue.extend(late_effects);
    }
    rest.extend(tail);
    if let Some(read) = tail_constant_read {
        rest.push(read);
    }
    *stmts = rest;
    prologue
}

/// An EVAL'd unit ending in a `BEGIN` block has the block's value (#12900).
/// The statement form moves into the prologue and leaves no value behind; the
/// expression form `(BEGIN { ... })` runs there too but yields its value in
/// place, so the tail is rewritten to it.
// Cost: O(n), n = statements of the unit.
fn value_tail_begin(stmts: &mut [Stmt]) {
    let Some(i) = crate::ast::last_value_stmt_index(stmts, crate::ast::TailSkip::Markers) else {
        return;
    };
    if let Stmt::Phaser {
        kind: PhaserKind::Begin,
        body,
        condition: None,
        ..
    } = &mut stmts[i]
    {
        let body = std::mem::take(body);
        stmts[i] = Stmt::Expr(Expr::Grouped(Box::new(Expr::PhaserExpr {
            kind: PhaserKind::Begin,
            body,
        })));
    }
}

/// A statement that reads the value of the constant `stmt` declares, or
/// `None` when `stmt` is not a constant declaration.
// Cost: O(1).
fn constant_read(stmt: &Stmt) -> Option<Stmt> {
    let Stmt::VarDecl { name, custom_traits, .. } = stmt else {
        return None;
    };
    if !custom_traits.iter().any(|(t, _)| t == "__constant") {
        return None;
    }
    let read = match name.as_bytes().first()? {
        b'@' => Expr::ArrayVar(name[1..].to_string()),
        b'%' => Expr::HashVar(name[1..].to_string()),
        b'&' | b'$' => return None,
        _ => Expr::BareWord(name.clone()),
    };
    Some(Stmt::Expr(read))
}

/// Whether `stmt` is a class declaration that composes a role. Rakudo composes
/// it at BEGIN time and runs the role's body then, once, over the unit's
/// lexicals in their static state; a declaration after a run-time statement
/// therefore joins the prologue, rather than being shelled ahead of the
/// mainline and composed again in place (#10667).
// Cost: O(b), b = statements in the class body.
fn composes_roles(stmt: &Stmt) -> bool {
    matches!(
        stmt,
        Stmt::ClassDecl {
            is_lexical: false,
            is_unit: false,
            name_expr: None,
            body,
            does_parents,
            ..
        } if !does_parents.is_empty()
            || body.iter().any(|s| matches!(s, Stmt::DoesDecl { .. }))
    )
}

/// What a statement does to `strict`: `Some(true)` for `no strict`,
/// `Some(false)` for `use strict`, `None` for any other statement.
fn strict_pragma(stmt: &Stmt) -> Option<bool> {
    match stmt {
        Stmt::No { module, arg: None } if module == "strict" => Some(true),
        Stmt::Use {
            module,
            condition: None,
            ..
        } if module == "strict" => Some(false),
        _ => None,
    }
}

/// The lexical names a unit declares at its top level, in `VarDecl` naming
/// (`x`, `@a`, `&f`). A lifted BEGIN may read these, because the prologue runs
/// in the unit's frame.
///
/// A `unit module` body is not looked into: a BEGIN in a package body is never
/// lifted (see `nested::walk`), so no lifted BEGIN reads its lexicals.
// Cost: O(n), n = number of statements in the unit's own scope.
fn unit_lexical_names(stmts: &[Stmt]) -> HashSet<String> {
    let mut names = HashSet::new();
    for stmt in crate::ast::scope_members(stmts) {
        match stmt {
            Stmt::VarDecl { name, .. } => {
                names.insert(name.clone());
            }
            Stmt::SubDecl { name, .. } => {
                names.insert(format!("&{}", name.resolve()));
            }
            _ => {}
        }
    }
    names
}

/// Give every exported declaration nested in a unit's code a BEGIN-time
/// registration at the statement that contains it (#10543). Runs once per
/// unit, ahead of its first [`take_unit_prologue`]: a second run would lift
/// the same declarations again.
// Cost: O(n), n = size of the unit's AST.
pub(crate) fn lift_nested_exports(stmts: &mut Vec<Stmt>) {
    nested_exports::lift_nested_exports(stmts);
}

/// Put a compilation unit's BEGIN prologue at its head. This is the whole of
/// the reordering a module's top level gets; the mainline and EVAL get it as
/// part of `phasers::reorder_phasers`. Returns the prologue's length.
pub(crate) fn order_unit(stmts: &mut Vec<Stmt>) -> usize {
    lift_nested_exports(stmts);
    let mut prologue = take_unit_prologue(stmts, false);
    let prologue_len = prologue.len();
    if prologue_len > 0 {
        prologue.append(stmts);
        *stmts = prologue;
    }
    prologue_len
}

/// Separate package-body run-time statements from their declarations without
/// changing the unit's statement order.
pub(crate) fn split_package_bodies(stmts: &mut Vec<Stmt>) {
    package_body::split_unit_bodies(stmts);
}

fn partition_stmt(stmt: Stmt, prologue: &mut Vec<Stmt>, rest: &mut Vec<Stmt>) {
    let stmt = match stmt {
        Stmt::Use {
            module,
            arg,
            tags,
            condition: Some(condition),
            if_imports,
        } => {
            let slot = next_if_condition_slot(prologue);
            prologue.extend(if_condition_check(*condition, &slot));
            Stmt::Use {
                module,
                arg,
                tags,
                condition: Some(Box::new(Expr::Var(slot))),
                if_imports,
            }
        }
        other => other,
    };
    if is_begin_time_stmt(&stmt) && !is_positional_pragma(&stmt) {
        let declared_class = match &stmt {
            Stmt::ClassDecl { name, .. } => Some(name.to_string()),
            _ => None,
        };
        // Carry the line marker with the statement, so an error raised in the
        // prologue still names the statement's own line.
        if let Some(line @ Stmt::SetLine(..)) = rest.last() {
            prologue.push(line.clone());
        }
        // A class or package body keeps its run-time statements in place.
        let (stmt, runtime) = package_body::split_package_decl(stmt);
        prologue.push(stmt);
        rest.extend(runtime);
        // The class declaration's value is its type object. It now runs in
        // the prologue, so read that value at the declaration's old position.
        if let Some(name) = declared_class {
            rest.push(Stmt::Expr(Expr::Var(name)));
        }
        return;
    }
    if let Some((static_decl, assign)) = crate::runtime::phasers::split_var_decl(&stmt) {
        prologue.push(static_decl);
        rest.extend(assign);
        return;
    }
    // A binding declaration (`my $x := …`) arrives wrapped in its bookkeeping
    // statements. Its static half is a plain declaration; the whole binding
    // declaration stays at its position and re-declares the same slot. Without
    // the static half a routine the prologue took was compiled before the name
    // was declared, so it lost its capture of the binding (#11263).
    if let Some(Stmt::VarDecl {
        name,
        type_constraint,
        is_state: false,
        is_our: false,
        is_dynamic,
        ..
    }) = crate::ast::bind_decl::declaration(&stmt)
        && !type_constraint
            .as_deref()
            .is_some_and(|tc| tc.starts_with(|c: char| c.is_ascii_lowercase()))
    {
        prologue.push(static_bind_decl(name, type_constraint.clone(), *is_dynamic));
        rest.push(stmt);
        return;
    }
    let stmt = match stmt {
        Stmt::SyntheticBlock(inner) if is_will_begin_group(&inner) => {
            partition_will_begin(inner, prologue, rest);
            return;
        }
        other => other,
    };
    // A group declaration `my ($a, @b);` arrives as a `SyntheticBlock` of plain
    // declarations. It splits member by member.
    if let Stmt::SyntheticBlock(inner) = &stmt
        && crate::ast::is_group_declaration(inner)
    {
        let Stmt::SyntheticBlock(inner) = stmt else {
            unreachable!()
        };
        for member in inner {
            // The source-form record goes with the group it described.
            if !matches!(member, Stmt::SourceForm(_)) {
                partition_stmt(member, prologue, rest);
            }
        }
        return;
    }
    rest.push(stmt);
}

/// The static half of the binding declaration of `name`: the declaration with
/// no initializer, holding what an unbound variable of its sigil holds.
// Cost: O(1).
fn static_bind_decl(name: &str, type_constraint: Option<String>, is_dynamic: bool) -> Stmt {
    let expr = match name.as_bytes().first() {
        Some(b'@') => Expr::Literal(crate::value::Value::real_array(Vec::new())),
        Some(b'%') => Expr::Literal(crate::value::Value::hash(crate::value::ValueMap::default())),
        _ => Expr::Literal(crate::value::Value::NIL),
    };
    Stmt::VarDecl {
        anonymous: false,
        name: name.to_string(),
        expr,
        type_constraint,
        is_state: false,
        is_our: false,
        is_dynamic,
        is_export: false,
        export_tags: Vec::new(),
        custom_traits: Vec::new(),
        where_constraint: None,
    }
}

/// A statement that extends the prologue's bound: a `BEGIN`, a `constant`, and
/// every module load (`use`, `need`, `import`) except a positional pragma
/// (ADR-0134 §2.1.1). A conditional `use` (`use Foo:if(EXPR)`) is one of them;
/// its condition is evaluated in the prologue too.
fn is_begin_time_effect(stmt: &Stmt) -> bool {
    match stmt {
        Stmt::Phaser { kind, .. } => *kind == PhaserKind::Begin,
        Stmt::Use { .. } | Stmt::Need { .. } | Stmt::Import { .. } => !is_positional_pragma(stmt),
        Stmt::VarDecl { custom_traits, .. } => custom_traits.iter().any(|(t, _)| t == "__constant"),
        Stmt::SyntheticBlock(inner) => is_will_begin_group(inner),
        // A user `trait_mod:<is>` runs when the declaration is compiled, so its
        // argument sees the unit's lexicals in their static state (#14146).
        Stmt::ClassDecl { custom_traits, .. }
        | Stmt::RoleDecl { custom_traits, .. }
        | Stmt::Package { custom_traits, .. } => !custom_traits.is_empty(),
        _ => false,
    }
}

/// The unit's own `trait_mod:<is>` routines and declared types, to tell which
/// lowercase `is NAME` of a class header is a trait. The parser keeps an
/// argument-less `class C is flagged` as a parent name, and the runtime defers
/// a name no type answers to the unit's `trait_mod:<is>` (see
/// `validate_class_parents`); the prologue has to place that declaration the
/// same way a parenthesized `is flagged(...)` one is placed (#14369).
struct DeferredTraitParents {
    has_is_trait_mod: bool,
    declared_types: HashSet<String>,
    /// The named parameters of the unit's own `trait_mod:<is>` routines
    /// (`:$marked!`): the trait names a declaration of this unit may use.
    trait_names: HashSet<String>,
}

impl DeferredTraitParents {
    // Cost: O(n), n = top-level statements of the unit.
    fn scan(stmts: &[Stmt]) -> Self {
        let mut has_is_trait_mod = false;
        let mut declared_types = HashSet::new();
        let mut trait_names = HashSet::new();
        for stmt in crate::ast::scope_members(stmts) {
            match stmt {
                Stmt::SubDecl {
                    name, param_defs, ..
                } => {
                    if name.resolve().starts_with("trait_mod:<is>") {
                        has_is_trait_mod = true;
                        trait_names.extend(
                            param_defs
                                .iter()
                                .filter(|p| p.named)
                                .map(|p| p.name.clone()),
                        );
                    }
                }
                Stmt::ClassDecl { name, .. }
                | Stmt::RoleDecl { name, .. }
                | Stmt::Package { name, .. } => {
                    declared_types.insert(name.resolve().to_string());
                }
                _ => {}
            }
        }
        Self {
            has_is_trait_mod,
            declared_types,
            trait_names,
        }
    }

    // Cost: O(p), p = parents of the declaration.
    fn names_trait(&self, stmt: &Stmt) -> bool {
        match stmt {
            // A routine or variable that uses a trait the unit's own
            // `trait_mod:<is>` takes runs it when the declaration is
            // compiled, like a type's (#14146).
            Stmt::SubDecl {
                custom_traits,
                param_defs,
                ..
            } => self.uses_unit_trait(custom_traits) || self.params_use_unit_trait(param_defs),
            Stmt::VarDecl { custom_traits, .. } => self.uses_unit_trait(custom_traits),
            Stmt::ClassDecl {
                parents,
                does_parents,
                body,
                ..
            } => {
                self.has_is_trait_mod
                    && (parents.iter().any(|parent| {
                        parent.starts_with(|c: char| c.is_lowercase())
                            && !self.declared_types.contains(parent)
                            && !does_parents.contains(parent)
                    }) || self.members_use_unit_trait(body))
            }
            Stmt::RoleDecl { body, .. } | Stmt::Package { body, .. } => {
                self.has_is_trait_mod && self.members_use_unit_trait(body)
            }
            _ => false,
        }
    }

    /// Whether a method, attribute or routine declared in a type's body, or a
    /// parameter of one, takes a trait of the unit's own `trait_mod:<is>`: the
    /// trait runs when the type is composed, so the type is a BEGIN-time
    /// effect (#14146).
    // Cost: O(m), m = members of the body.
    fn members_use_unit_trait(&self, body: &[Stmt]) -> bool {
        crate::ast::scope_members(body).any(|member| match member {
            Stmt::MethodDecl {
                custom_traits,
                param_defs,
                ..
            }
            | Stmt::SubDecl {
                custom_traits,
                param_defs,
                ..
            } => self.uses_unit_trait(custom_traits) || self.params_use_unit_trait(param_defs),
            Stmt::HasDecl { unknown_traits, .. } => unknown_traits
                .iter()
                .any(|(kind, name, _)| kind == "is" && self.trait_names.contains(name)),
            _ => false,
        })
    }

    // Cost: O(p), p = parameters.
    fn params_use_unit_trait(&self, param_defs: &[crate::ast::ParamDef]) -> bool {
        self.has_is_trait_mod
            && param_defs
                .iter()
                .any(|p| p.traits.iter().any(|t| self.trait_names.contains(t)))
    }

    // Cost: O(t), t = traits of the declaration.
    fn uses_unit_trait(&self, custom_traits: &[(String, Option<Expr>)]) -> bool {
        self.has_is_trait_mod
            && custom_traits
                .iter()
                .any(|(t, _)| self.trait_names.contains(t))
    }
}

/// A lexical pragma that mutsu applies as run-time state at the statement's
/// own position (`use strict`, `no strict`, `use fatal`, `use soft`, ...). It
/// stays where it is: moving it into the prologue would switch the mode on for
/// the run-time statements that precede it. Only the pragmas of
/// [`is_known_pragma_name`] count, so `use lib` (which extends the search path
/// the prologue's loads resolve against) and `use if` (which enables the `:if`
/// adverb of a later conditional `use`) stay BEGIN-time effects, as does a
/// lowercase-named real module such as `vars`.
fn is_positional_pragma(stmt: &Stmt) -> bool {
    let module = match stmt {
        Stmt::Use { module, .. } | Stmt::No { module, .. } | Stmt::Need { module } => module,
        _ => return false,
    };
    is_known_pragma_name(module)
}

/// The lowercase names mutsu applies as positional run-time state, plus a
/// language version (`v6.d`, `v6.*`). Any other lowercase name is an ordinary
/// module that happens to be spelled like a pragma (`use vars <$x @y>`, whose
/// `sub EXPORT` a later BEGIN observes), so it is a BEGIN-time load.
// Cost: O(1), a fixed set of names compared against one string.
pub(crate) fn is_known_pragma_name(module: &str) -> bool {
    if let Some(rest) = module.strip_prefix('v')
        && rest.starts_with(|c: char| c.is_ascii_digit())
    {
        return true;
    }
    matches!(
        module,
        "strict"
            | "fatal"
            | "soft"
            | "nqp"
            | "isms"
            | "oo"
            | "class"
            | "experimental"
            | "customtrait"
            | "warnings"
            | "worries"
            | "precompilation"
            | "trace"
            | "internals"
            | "variables"
            | "attributes"
            | "dynamic-scope"
            | "newline"
    )
}

/// `my $x will begin { ... }` parses to a `SyntheticBlock` of the declaration
/// followed by its trait phasers. Its `begin` phaser is a BEGIN-time effect.
fn is_will_begin_group(inner: &[Stmt]) -> bool {
    matches!(inner.first(), Some(Stmt::VarDecl { .. }))
        && inner
            .iter()
            .skip(1)
            .all(|s| matches!(s, Stmt::Phaser { .. }))
        && inner.iter().any(|s| {
            matches!(
                s,
                Stmt::Phaser {
                    kind: PhaserKind::Begin,
                    ..
                }
            )
        })
}

/// Split a `will begin` group: the declaration's static half and each `begin`
/// phaser go to the prologue, in source order, with `$_` bound to the declared
/// container. The initializer and the other trait phasers stay at run time.
fn partition_will_begin(inner: Vec<Stmt>, prologue: &mut Vec<Stmt>, rest: &mut Vec<Stmt>) {
    let mut members = inner.into_iter();
    let Some(decl) = members.next() else {
        return;
    };
    let name = match &decl {
        Stmt::VarDecl { name, .. } => name.clone(),
        _ => {
            rest.push(decl);
            rest.extend(members);
            return;
        }
    };
    partition_stmt(decl, prologue, rest);
    let mut later = Vec::new();
    for member in members {
        match member {
            Stmt::Phaser { spans: _,
                kind: PhaserKind::Begin,
                mut body,
                condition,
                end_index,
            } => {
                if let [Stmt::Given { topic, .. }] = body.as_mut_slice() {
                    *topic = Expr::Var(name.clone());
                }
                prologue.push(Stmt::Phaser { spans: Default::default(),
                    kind: PhaserKind::Begin,
                    body,
                    condition,
                    end_index,
                });
            }
            other => later.push(other),
        }
    }
    rest.extend(later);
}

/// A statement whose whole effect happens at BEGIN time in Rakudo: the `BEGIN`
/// phaser itself, a module load, and every declarator a BEGIN can observe.
fn is_begin_time_stmt(stmt: &Stmt) -> bool {
    match stmt {
        Stmt::Phaser { kind, .. } => *kind == PhaserKind::Begin,
        Stmt::VarDecl { custom_traits, .. } => custom_traits.iter().any(|(t, _)| t == "__constant"),
        // A `unit module Foo;` is a body-less marker that puts every following
        // top-level statement into `Foo`; it has to precede them in the prologue
        // just as it does in the source.
        Stmt::Package { .. }
        | Stmt::Use { .. }
        | Stmt::No { .. }
        | Stmt::Need { .. }
        | Stmt::Import { .. }
        | Stmt::SubDecl { .. }
        | Stmt::ProtoDecl { .. }
        | Stmt::TokenDecl { .. }
        | Stmt::RuleDecl { .. }
        | Stmt::ProtoToken { .. }
        | Stmt::ClassDecl { .. }
        | Stmt::RoleDecl { .. }
        | Stmt::EnumDecl { .. }
        | Stmt::SubsetDecl { .. }
        | Stmt::AugmentClass { .. } => true,
        // An exported type declaration (`class C is export { }`) arrives as the
        // declaration followed by its `__MUTSU_EXPORT_TYPE__` marker; the pair is
        // one declaration. A versioned/authored one (`class C:ver<1>`) is
        // preceded by its `__MUTSU_SET_META__` call, so a BEGIN block sees the
        // class just as it does an unversioned one.
        Stmt::SyntheticBlock(inner) => {
            !inner.is_empty()
                && inner.iter().all(|s| {
                    is_begin_time_stmt(s)
                        || matches!(s, Stmt::Expr(Expr::Call { name, .. })
                        if matches!(
                            name.resolve().as_str(),
                            "__MUTSU_EXPORT_TYPE__" | "__MUTSU_SET_META__"
                        ))
                })
                && inner.iter().any(is_begin_time_stmt)
        }
        _ => false,
    }
}

impl crate::runtime::Interpreter {
    /// Run only a unit's BEGIN prologue. Used when a post-parse check rejects
    /// the unit: in Rakudo the BEGIN-time effects already ran while the unit
    /// was being parsed, before that check could report (ADR-0134 §2.1.5).
    pub(crate) fn run_begin_prologue_only(
        &mut self,
        prologue: &[Stmt],
    ) -> Result<(), crate::value::RuntimeError> {
        let mut compiler = crate::compiler::Compiler::new();
        compiler.set_current_package(self.current_package());
        compiler.is_mainline = true;
        let (code, compiled_fns) = compiler.compile(prologue);
        self.run_top(&code, &compiled_fns).map(|_| ())
    }
}
