//! The ADR-0048 D2 placeholder-scope oracle: which constructs' `{ ... }`
//! bodies may carry a placeholder-derived signature of their own, and what
//! the construct supplies them with. Every placeholder-scope walk
//! ([`super::placeholders`], `crate::placeholder_order`) consults this one
//! table instead of re-deriving the descend-or-stop decision.

use super::{Expr, Stmt};

/// ADR-0048 D2: how much of a construct's own argument supply a `$^name`
/// placeholder inside it can see.
///
/// `ArgSupply` is only meaningful when the construct is `Signature`-capable
/// (see [`PlaceholderBodyKind`]); it names *what value* the construct hands
/// its body when it invokes it. Not every variant is exercised yet — Phase 1
/// classified `Condition`, `Elements`, `Topic` and `CallerArgs`; Phase 3
/// (D3/D6) put `None` to work for the zero-argument bodies (`when`, the bare
/// `{}` statement). `ConditionAfterFirstPass` (`repeat {} while/until`'s `Mu`
/// first pass) is still reserved for D4/Phase 4.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ArgSupply {
    /// The enclosing block's own arguments (routine bodies, closure values).
    CallerArgs,
    /// One argument: the raw (un-boolified) condition value.
    Condition,
    /// One argument: `Mu` on the first pass, then the condition value.
    ConditionAfterFirstPass,
    /// One argument: the topic.
    Topic,
    /// N arguments per iteration, N = the body's own placeholder count.
    Elements,
    /// One `Mu` per declared placeholder: a `role` body, which raku runs once
    /// at composition (ADR-0048 D7). Never under-supplied, so it never raises
    /// an arity failure. (Rakudo actually leaves each parameter as an
    /// uninitialized `VMNull` register: it gists as `(Mu)` and `$^c === Mu` is
    /// `True`, but `$^c.^name` says `VMNull` and `$^c.defined` throws. mutsu
    /// does not supply the value at all yet — see
    /// role-body-placeholder-mu-supply (#7550) — so this variant
    /// currently only records that a role body never under-supplies.)
    AllMu,
    /// Zero arguments.
    None,
}

/// ADR-0048 D2: classifies whether a construct's `{ ... }` body may carry a
/// placeholder-derived signature of its own, and if so what it is supplied
/// with. This is the single table consulted by every placeholder-scope walk
/// (through `super::placeholders::walk_stmt_placeholder_scope` /
/// `walk_expr_placeholder_scope`, which the placeholder collectors and the
/// ordering checks in `placeholder_order.rs` share) instead of each
/// independently re-deriving the same descend-or-stop decision — see
/// `docs/adr/0048-placeholder-scope-is-a-block-invocation-contract.md`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum PlaceholderBodyKind {
    /// No block of its own: descend, placeholders belong to the enclosing
    /// scope. Covers statement MODIFIERS (`if`/`for`/`given` with
    /// `is_statement_modifier: true`, which have no block at all — mirroring
    /// the `For`/`Given` modifier rule below), the parser's synthetic
    /// `SyntheticBlock` desugar wrapper (no `{ ... }` in the source at all),
    /// and also `while`/`RoleDecl`/WhateverCode closures, which mutsu
    /// currently (WRONGLY, per ADR-0048's raku audit) treats the same way:
    /// they leak their placeholders to the enclosing scope (or, for
    /// `RoleDecl`, over-reject) instead of matching raku's rule. Phase 1 was
    /// a pure refactor of the existing (partly wrong) behaviour into one
    /// table; Phase 2 corrected `loop`/`react`/`default`/`Catch`/`Control`/
    /// `Phaser` (moved to `NoSignature`, see below); Phase 3 corrected `when`
    /// and the bare `{}` statement (moved to `Signature(ArgSupply::None)`);
    /// the remaining wrong entries are corrected by ADR-0048's later phases
    /// (D4 for `while`/`repeat`, D7 for `RoleDecl`), not here.
    Transparent,
    /// A boundary that takes a signature; the construct supplies `ArgSupply`
    /// when it invokes its body. `if`/`elsif`/`unless`/`with`/`without`
    /// (non-modifier) supply the raw condition (`ArgSupply::Condition`);
    /// `for` (non-modifier) supplies one N-tuple of elements per iteration
    /// (`ArgSupply::Elements`); `given`/`with` (non-modifier) and `whenever`
    /// supply the topic/emitted value (`ArgSupply::Topic`); real closures
    /// (`sub {}`/`-> {}`/named subs) supply the caller's own arguments
    /// (`ArgSupply::CallerArgs`); `when` bodies and the bare `{}` statement
    /// are invoked with nothing (`ArgSupply::None`, ADR-0048 Phase 3/D6).
    /// The *value* half of the contract lives in the compiler's shared
    /// `Compiler::emit_inlined_body_placeholder_binds` (ADR-0048 D3), which
    /// binds as many of the body's placeholders as the construct supplies and
    /// raises raku's `Too few positionals passed` for the rest.
    Signature(ArgSupply),
    /// A boundary that may not take a signature at all: a placeholder used
    /// directly inside it is `X::Placeholder::Block`. `class`/`RoleDecl`
    /// bodies fall to this variant via the catch-all below (RoleDecl is a
    /// deliberate Phase-1 over-reject, corrected only in Phase 5/D7).
    /// ADR-0048 Phase 2 moved `loop`, `try`, `react`, `once`, `default`,
    /// `CATCH`/`CONTROL` (standalone, and `Stmt::Phaser`'s BEGIN/CHECK/INIT/
    /// ENTER/END/PRE/POST kinds), `gather`, and `module`/`package`/`grammar`
    /// into this variant too, each reusing the same
    /// `placeholder_scope_error("block", ph)` helper `do {}`'s existing
    /// (separately-implemented) rejection already used. The statement-prefix
    /// group that desugars its body into a real closure at PARSE time
    /// (`start`, `sink`, `supply`, `lazy`, `eager`) cannot be classified here
    /// at all — by the time this oracle runs the placeholder has already been
    /// consumed as that closure's own signature — so Phase 2 rejects those at
    /// their compiler call sites instead (see the `emit_block_placeholder_die`
    /// call sites in `src/compiler/expr_call.rs`/`expr.rs`/`supply.rs`), not
    /// via this table. `race { }` (the bare, non-`for` statement-prefix form)
    /// has no dedicated AST construct in mutsu at all yet — `race` parses as
    /// an ordinary bareword, so it is left unaddressed by Phase 2.
    ///
    /// `do {}` (`Expr::DoBlock`) is *not* classified `NoSignature` here even
    /// though it already rejects a stray placeholder at runtime: that
    /// rejection is implemented by a wholly separate, unconditional check in
    /// `compile_do_block_expr` (`collect_unattached_placeholders` on the
    /// do-block's own body), which exempts a placeholder already "attached"
    /// as the *enclosing* block's own parameter. That attachment is only
    /// possible because THIS shallow walk treats `DoBlock` as `Transparent`
    /// — the parser's chained-comparison desugar
    /// (`src/parser/expr/precedence/chain_cmp.rs`) wraps `0 <= $^p <= 5`'s
    /// placeholder in a synthetic `DoBlock`, so a `where`/`subset` predicate
    /// written that way relies on `$^p` leaking through it to become the
    /// enclosing block's own placeholder parameter (pinned by
    /// `t/subset-where-placeholder-chain.t`; broke Cro::Core's `Cro::Port`
    /// when tried). Reclassifying `DoBlock` as `NoSignature` here would stop
    /// that leak and make every such chained comparison in a placeholder
    /// block newly reject with `X::Placeholder::Block` — a real behaviour
    /// change Phase 1 must not make. Untangling this is left to whichever
    /// later phase gives `do {}` a real `NoSignature` classification.
    NoSignature,
}

/// ADR-0048 D2 oracle for `Stmt`. See [`PlaceholderBodyKind`] for the
/// per-variant rationale (moved here from the individual match arms below,
/// per the ADR: "move them, do not duplicate them").
// Cost: O(d), d = number of labels wrapping the statement.
pub(crate) fn placeholder_body_kind(stmt: &Stmt) -> PlaceholderBodyKind {
    // A label names the statement it wraps; the body kind is that statement's.
    let mut stmt = stmt;
    while let Stmt::Label { stmt: inner, .. } = stmt {
        stmt = inner;
    }
    match stmt {
        Stmt::If {
            is_statement_modifier: true,
            ..
        } => PlaceholderBodyKind::Transparent,
        Stmt::If { .. } => PlaceholderBodyKind::Signature(ArgSupply::Condition),
        // ADR-0048 D4/Phase 4: a `while`/`until` BLOCK is a real Block that
        // the loop invokes with the *raw* (un-boolified) condition value on
        // every pass — `while 42 { $^c }` prints 42, `until False { $^c }`
        // prints `False`, and `{ while 42 { $^c } }.arity` is 0 because the
        // name never reaches the enclosing block. A `while`/`until`
        // STATEMENT MODIFIER introduces no block at all, so its placeholders
        // are the enclosing block's own parameters
        // (`sub f { say "$^a" while $i++ < 2 }; f(7)` prints 7 twice) —
        // exactly the `if`/`for`/`given` modifier rule above.
        Stmt::While {
            is_statement_modifier: true,
            ..
        } => PlaceholderBodyKind::Transparent,
        Stmt::While { .. } => PlaceholderBodyKind::Signature(ArgSupply::Condition),
        Stmt::For {
            is_statement_modifier: true,
            ..
        } => PlaceholderBodyKind::Transparent,
        Stmt::For { .. } => PlaceholderBodyKind::Signature(ArgSupply::Elements),
        // ADR-0048 Phase 2: `loop {}` (headerless and C-style) does not take
        // a signature in raku — flip from the Phase-1 (wrong) `Transparent`
        // classification to `NoSignature` via the catch-all below. `repeat
        // {} while/until` (`repeat: true`) is a DIFFERENT construct that
        // stays `Transparent` here: per the ADR's evidence table it IS
        // signature-capable (`ArgSupply::ConditionAfterFirstPass` — `Mu` on
        // the first pass, then the condition value), so it belongs with D4
        // (Phase 4), not this rejecting set. Verified against `raku`:
        // `repeat while $b < 10 { $tracker = $^a; $b++ }` does NOT reject
        // `$^a` (pins `roast/S04-statements/repeat.t`'s "placeholders and
        // 'repeat while' mix" subtest, which would otherwise regress).
        //
        // ADR-0048 Phase 3 had to promote it from that placeholder
        // `Transparent` to its real `Signature` classification: once the bare
        // `{ ... }` STATEMENT became a zero-argument boundary (D6, below), a
        // `repeat` nested in one — exactly the shape of
        // `roast/S04-statements/repeat.t`'s subtest and of
        // `t/placeholder-scope-rejecting.t`'s accepting pin — leaked its
        // `$^a` out to the enclosing bare block, which then reported it as a
        // parameter nothing supplies. This is the *classification* half of
        // D4 only: the `ArgSupply::ConditionAfterFirstPass` bind itself (`Mu`
        // on the first pass, the raw condition afterwards) is still Phase 4's
        // work, so a placeholder in a `repeat` body is a parameter of that
        // body that nothing binds yet, rather than the enclosing block's.
        Stmt::Loop { repeat: true, .. } => {
            PlaceholderBodyKind::Signature(ArgSupply::ConditionAfterFirstPass)
        }
        // `loop {}` (`repeat: false`, both headerless and C-style) and
        // `react {}` fall through to the `NoSignature` catch-all below.
        // The `whenever` body is its own block scope, supplied the emitted
        // value (aliased as the topic) — but mutsu's shallow walks never
        // descend into it today (only the `supply` header is collected in
        // this scope), so this classification's practical effect in Phase 1
        // is identical to `NoSignature`: a boundary, body not visited here.
        Stmt::Whenever { .. } => PlaceholderBodyKind::Signature(ArgSupply::Topic),
        // `Default`/`Catch`/`Control`/`Phaser` do not take a signature in
        // raku either — ADR-0048 Phase 2 flips them to `NoSignature` via the
        // catch-all below.
        //
        // ADR-0048 Phase 3 (D3/D6): a bare `{ ... }` STATEMENT and a `when`
        // body are real Blocks that raku invokes with ZERO arguments, so a
        // placeholder in one is that block's own unsatisfied parameter, not
        // the enclosing block's — `{ $^c }` and `given 5 { when 5 { $^c } }`
        // both die with "Too few positionals passed; expected 1 argument but
        // got 0", and `{ when 5 { $^c } }.arity` is 0. Hence
        // `Signature(ArgSupply::None)`: a boundary the shallow walks stop at,
        // whose arity failure `emit_inlined_body_placeholder_binds` raises at
        // the body's own compile site.
        //
        // `SyntheticBlock` is NOT included: it is a parser desugar wrapper
        // (destructuring declarations, `has` attribute lowering, package
        // meta-statements, ...) with no `{ ... }` in the source at all, so a
        // placeholder inside one still belongs to the enclosing block.
        // `RoleDecl` stays `Transparent` too (a deliberate Phase-1
        // over-reject via its own `emit_block_placeholder_die` call site —
        // correcting it is Phase 5/D7, not here).
        Stmt::Block(_) | Stmt::When { .. } => PlaceholderBodyKind::Signature(ArgSupply::None),
        // ADR-0048 D7/Phase 5: a `role` body IS signature-capable in raku
        // (`role R { $^c }; class D does R {}` compiles and runs at
        // composition), unlike the `class`/`module`/`package`/`grammar`
        // bodies that fall to `NoSignature` below. Every placeholder it
        // declares is supplied the same value, so it never raises an arity
        // failure — see `ArgSupply::AllMu`. Only this SCOPE half is
        // implemented: the boundary stops `$^c` leaking onto the enclosing
        // block (`{ role R { $^c } }.arity` is 0, as in raku), but the
        // compiler still rejects a role body that actually uses a
        // placeholder, because the value cannot be supplied from the
        // `Stmt::RoleDecl` compile site — see the comment on that arm in
        // `src/compiler/stmt.rs` and
        // role-body-placeholder-mu-supply (#7550).
        Stmt::RoleDecl { .. } => PlaceholderBodyKind::Signature(ArgSupply::AllMu),
        Stmt::SyntheticBlock(_) => PlaceholderBodyKind::Transparent,
        Stmt::Given {
            is_statement_modifier: true,
            ..
        } => PlaceholderBodyKind::Transparent,
        Stmt::Given { .. } => PlaceholderBodyKind::Signature(ArgSupply::Topic),
        // Every other `Stmt` kind has no body visited by the shallow walks
        // today: real routine/method/class/package bodies are collected by
        // their own dedicated compile-time pass, never by this shallow one.
        _ => PlaceholderBodyKind::NoSignature,
    }
}

/// ADR-0048 D2 oracle for `Expr` (the sibling of [`placeholder_body_kind`]
/// for expression-position bodies: closures, `Try`, `Gather`, `DoBlock`,
/// phasers-as-expressions).
// Cost: O(1).
pub(crate) fn placeholder_body_kind_expr(expr: &Expr) -> PlaceholderBodyKind {
    match expr {
        // A WhateverCode (`*`-derived) closure owns only its `*`-derived
        // params, not `$^name` placeholders, which belong to the nearest
        // enclosing *explicit* block — so it is transparent here.
        Expr::AnonSubParams {
            is_whatever_code: true,
            ..
        }
        | Expr::Lambda {
            is_whatever_code: true,
            ..
        } => PlaceholderBodyKind::Transparent,
        // A real closure (`sub {}`/`-> {}`/an already-signatured block)
        // supplies the caller's own arguments; it is its own placeholder
        // scope already, so the shallow walks never need to look inside it.
        Expr::AnonSub { .. } | Expr::AnonSubParams { .. } | Expr::Lambda { .. } => {
            PlaceholderBodyKind::Signature(ArgSupply::CallerArgs)
        }
        // The bare `{}` TERM (an `Expr::Block` in value position) stays
        // `Transparent`: `compile_expr_block` turns a placeholder-bearing one
        // into a real closure with those placeholders as its signature
        // (`{ $^c }.arity` is 1), so it is not the zero-argument statement
        // Block that ADR-0048 Phase 3/D6 reclassified. `Gather`
        // (`gather {}`) does NOT — ADR-0048 Phase 2 flips it to `NoSignature`
        // via the catch-all below (raku: a placeholder inside `gather {}` is
        // `X::Placeholder::Block`, not the enclosing block's own param).
        Expr::Block(_) => PlaceholderBodyKind::Transparent,
        // Not `NoSignature` — see the long note on `PlaceholderBodyKind::NoSignature`
        // above: the chained-comparison desugar's synthetic `DoBlock` relies
        // on this leak to attach `$^p` to the enclosing block.
        Expr::DoBlock { .. } => PlaceholderBodyKind::Transparent,
        // `Try`/`PhaserExpr`/`Once` do not take a signature in raku — ADR-0048
        // Phase 2 flips them to `NoSignature` via the catch-all below.
        _ => PlaceholderBodyKind::NoSignature,
    }
}
