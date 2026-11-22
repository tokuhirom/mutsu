//! A statement-form `BEGIN` of the unit's top level runs when the parser
//! reaches it, as on rakudo (ADR-17558 slice 5), when the loader takes it: no
//! effect the unit's BEGIN-time prologue runs precedes it, and running it ahead
//! of the unit's frame is unobservable (the loader decides that). It then
//! leaves the prologue nothing but the values it left in the unit's variables
//! (§7.17), and a later `use` still loads at parse time. Any other `BEGIN` keeps its prologue position, and a `use` after it
//! loads in the prologue too, so no module runs ahead of it (§7.6).

use crate::ast::{PhaserKind, Stmt};

thread_local! {
    /// Whether the unit being parsed has, before the parse position, any effect
    /// its BEGIN-time prologue runs after the parse (`UNIT_BEGIN_SEEN`'s, or
    /// one [`super::prologue_effects::has_prologue_effect`] finds in a finished
    /// top-level statement). A `BEGIN` after one keeps its prologue position
    /// rather than running at parse time ahead of it.
    pub(super) static UNIT_PROLOGUE_EFFECT_SEEN: std::cell::Cell<bool> =
        const { std::cell::Cell::new(false) };
    /// The unit's top-level `use` statements before the parse position that
    /// loaded at parse time, in source order: a parse-time `BEGIN` that calls
    /// a routine one of them imports runs that import first (§7.16).
    pub(super) static UNIT_PARSE_TIME_USES: std::cell::RefCell<Vec<Stmt>> =
        const { std::cell::RefCell::new(Vec::new()) };
    /// The unit's plain top-level `my` variables declared before the parse
    /// position (by the compiler's name), each with the value a parse-time
    /// `BEGIN` left in it (§7.17).
    pub(super) static UNIT_VARS: std::cell::RefCell<UnitVars> =
        const { std::cell::RefCell::new(Vec::new()) };
    /// The statement the parser returns next is the replay a parse-time
    /// `BEGIN` left for the prologue, which is no prologue effect of its own.
    static REPLAY_RETURNED: std::cell::Cell<bool> = const { std::cell::Cell::new(false) };
}

/// See [`UNIT_VARS`].
pub(super) type UnitVars = Vec<(String, Option<crate::value::Value>)>;

/// Note `stmt`, a finished statement of the unit's top level, if it declares
/// a plain `my` variable a parse-time `BEGIN` may reach: a scalar, array or
/// hash with no type, trait, shape, `where` or binding (its initializer runs
/// at run time, so a `BEGIN` sees the variable without it, as on rakudo).
// Cost: O(1).
fn note_unit_var(stmt: &Stmt) {
    let Stmt::VarDecl {
        name,
        anonymous: false,
        type_constraint: None,
        is_state: false,
        is_our: false,
        is_dynamic: false,
        custom_traits,
        constant: None,
        bind: None,
        marks,
        where_constraint: None,
        ..
    } = stmt
    else {
        return;
    };
    let plain_name = match name.as_bytes() {
        [b'@' | b'%', c, ..] | [c, ..] => c.is_ascii_alphabetic() || *c == b'_',
        [] => false,
    };
    let plain_marks = !(marks.shaped
        || marks.implicit_value_type
        || marks.type_from_trait
        || marks.type_from_of
        || marks.name_colonpairs.is_some()
        || marks.dynamic_argument.is_some()
        || marks.pointy_copy
        || marks.pointy_native_param
        || marks.pointy_param
        || marks.signature_element
        || marks.sigilless_decl
        || marks.begin_static
        || marks.begin_static_definite
        || marks.static_bind_half
        || marks.bound);
    if !plain_name || !custom_traits.is_empty() || !plain_marks {
        return;
    }
    UNIT_VARS.with(|vars| {
        let mut vars = vars.borrow_mut();
        // A redeclaration is a new variable.
        vars.retain(|(var, _)| var != name);
        vars.push((name.clone(), None));
    });
}

/// Note `use_stmt`, a `use` the loader loaded at parse time and that imports
/// into the scope it stands in, if that is the unit's top level.
// Cost: O(n), n = size of `use_stmt`, at the top level; O(1) elsewhere.
pub(in crate::parser) fn note_parse_time_use(use_stmt: &Stmt) {
    if !super::in_nested_scope()
        && !crate::parser::in_fragment_parse()
        && super::statement_package().is_none()
    {
        UNIT_PARSE_TIME_USES.with(|uses| uses.borrow_mut().push(use_stmt.clone()));
    }
}

/// What became of a unit-level `BEGIN` the parser finished.
pub(in crate::parser) enum UnitBegin {
    /// It ran now; the parse keeps in its place only these statements, which
    /// the prologue runs at its position: they write the values it left into
    /// the unit's variables (§7.17).
    Ran(Vec<Stmt>),
    /// It died: the unit's compile fails at the `BEGIN` with this message
    /// (the loader keeps the error for the parse's caller).
    Failed(String),
    /// It stays in the parsed tree, for the prologue.
    Kept,
}

/// Run the `BEGIN` phaser with `body` (`bare`: written without braces) now,
/// if it stands at the unit's top level and the loader takes it.
// Cost: O(n) besides the loader's run, n = size of `body`.
pub(in crate::parser) fn run_unit_begin(body: &[Stmt], bare: bool) -> UnitBegin {
    let candidate = !super::in_nested_scope()
        && !UNIT_PROLOGUE_EFFECT_SEEN.with(std::cell::Cell::get)
        && !crate::parser::in_fragment_parse()
        && super::statement_package().is_none();
    if !candidate {
        super::note_unit_compile_time_effect();
        return UnitBegin::Kept;
    }
    // It runs before the unit is finished, so its `use trace` hooks get their
    // numbers now; the parsed tree keeps its offsets for the unit's own
    // numbering in case the loader leaves it.
    let mut begin = Stmt::Phaser {
        spans: None,
        kind: PhaserKind::Begin,
        body: body.to_vec(),
        condition: None,
        end_index: None,
        bare,
        scope: 0,
    };
    crate::parser::stmt::trace::number_statements_so_far(&mut begin);
    let uses = UNIT_PARSE_TIME_USES.with(|uses| uses.borrow().clone());
    let vars = UNIT_VARS.with(|vars| vars.borrow().clone());
    let unit = crate::compile_time_loader::UnitAtParse {
        uses: &uses,
        vars: &vars,
    };
    match crate::compile_time_loader::run_begin(&begin, &unit) {
        crate::compile_time_loader::BeginOutcome::Ran(left) => {
            let replay: Vec<Stmt> = left
                .iter()
                .map(|(name, value)| Stmt::Assign {
                    name: name.clone(),
                    expr: crate::ast::Expr::Literal(value.clone()),
                    op: crate::ast::AssignOp::Assign,
                    target_is_sigilless: false,
                })
                .collect();
            UNIT_VARS.with(|vars| {
                for (var, value) in vars.borrow_mut().iter_mut() {
                    if let Some((_, left)) = left.iter().find(|(name, _)| name == var) {
                        *value = Some(left.clone());
                    }
                }
            });
            if !replay.is_empty() {
                REPLAY_RETURNED.with(|flag| flag.set(true));
            }
            UnitBegin::Ran(replay)
        }
        crate::compile_time_loader::BeginOutcome::Failed(error) => {
            let message = error.message.to_string();
            crate::compile_time_loader::note_failure(error);
            UnitBegin::Failed(message)
        }
        crate::compile_time_loader::BeginOutcome::NotTaken => {
            super::note_unit_compile_time_effect();
            UnitBegin::Kept
        }
    }
}

/// Note the prologue effects of `stmt`, a finished statement of the unit's top
/// level (see [`UNIT_PROLOGUE_EFFECT_SEEN`]).
// Cost: O(n), n = size of `stmt`, until the first effect; O(1) after it.
pub(in crate::parser) fn note_unit_prologue_effects_of(stmt: &Stmt) {
    note_unit_var(stmt);
    // The replay of a parse-time `BEGIN` (a `BEGIN` of plain assignments)
    // is not one.
    if REPLAY_RETURNED.with(|flag| flag.replace(false))
        && matches!(stmt, Stmt::Phaser { kind: PhaserKind::Begin, .. })
    {
        return;
    }
    if !UNIT_PROLOGUE_EFFECT_SEEN.with(std::cell::Cell::get)
        && super::prologue_effects::has_prologue_effect(stmt)
    {
        UNIT_PROLOGUE_EFFECT_SEEN.with(|seen| seen.set(true));
    }
}

/// Both "effect seen" flags of the unit (`UNIT_BEGIN_SEEN` and
/// [`UNIT_PROLOGUE_EFFECT_SEEN`]), cleared, to save around a nested parse that
/// resets them ([`restore_unit_effects_seen`]).
// Cost: O(1).
pub(in crate::parser) fn take_unit_effects_seen() -> (bool, bool) {
    (
        super::UNIT_BEGIN_SEEN.with(|seen| seen.replace(false)),
        UNIT_PROLOGUE_EFFECT_SEEN.with(|seen| seen.replace(false)),
    )
}

/// Put back what [`take_unit_effects_seen`] returned.
// Cost: O(1).
pub(in crate::parser) fn restore_unit_effects_seen((begin, prologue): (bool, bool)) {
    super::UNIT_BEGIN_SEEN.with(|seen| seen.set(begin));
    UNIT_PROLOGUE_EFFECT_SEEN.with(|seen| seen.set(prologue));
}

/// The unit's parse-time `use`s so far ([`UNIT_PARSE_TIME_USES`]), cleared,
/// to save around a nested parse that resets them
/// ([`restore_parse_time_uses`]).
// Cost: O(1).
pub(in crate::parser) fn take_parse_time_uses() -> (Vec<Stmt>, UnitVars) {
    (
        UNIT_PARSE_TIME_USES.with(|uses| std::mem::take(&mut *uses.borrow_mut())),
        UNIT_VARS.with(|vars| std::mem::take(&mut *vars.borrow_mut())),
    )
}

/// Put back what [`take_parse_time_uses`] returned.
// Cost: O(1).
pub(in crate::parser) fn restore_parse_time_uses((uses, vars): (Vec<Stmt>, UnitVars)) {
    UNIT_PARSE_TIME_USES.with(|saved| *saved.borrow_mut() = uses);
    UNIT_VARS.with(|saved| *saved.borrow_mut() = vars);
}
