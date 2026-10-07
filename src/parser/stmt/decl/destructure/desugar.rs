//! The expansion of a signature declaration (`my ($a, @b) = …`) into
//! ordinary declarations reading a staging temporary. One implementation,
//! shared by the parser and by `rakuast::lower` (ADR-10723 Stage 1): the
//! RakuAST layer reads the declaration from the [`SourceForm`] record the
//! expansion starts with, and lowers a `VarDeclaration::Signature` back
//! through [`signature_decl`].

use super::bind_arity::{optional_param_default, push_bind_arity_check, staged_exists};
use super::native_type_default;
use crate::ast::{Expr, ParamTrait, SignatureDecl, SignatureInit, SourceForm, Stmt};
use crate::symbol::Symbol;
use crate::token_kind::TokenKind;
use crate::value::Value;

/// The source-form record that opens an expansion.
pub(crate) fn source_form(decl: SignatureDecl) -> Stmt {
    Stmt::SourceForm(Box::new(SourceForm::SignatureDecl(decl)))
}

/// The whole expansion of `decl`, opened by its source-form record.
pub(crate) fn signature_decl(decl: SignatureDecl) -> Stmt {
    let mut stmts = match &decl.init {
        None => expand_bare(&decl),
        Some(init) => {
            let (mut stmts, result) = expand_assigned(&decl, init);
            stmts.push(Stmt::Expr(result));
            stmts
        }
    };
    stmts.insert(0, source_form(decl));
    Stmt::SyntheticBlock(stmts)
}

/// The statements of an initialized declaration and the expression its block
/// yields. The caller appends the expression (a parser may first fold a
/// trailing word-logical into it).
pub(crate) fn expand_with_rhs(decl: &SignatureDecl) -> (Vec<Stmt>, Expr) {
    match &decl.init {
        Some(init) => expand_assigned(decl, init),
        None => (expand_bare(decl), Expr::Literal(Value::NIL)),
    }
}

/// `my ($a, @b);` -- one declaration per element, holding its default.
fn expand_bare(decl: &SignatureDecl) -> Vec<Stmt> {
    let vars = &decl.vars;
    let is_state = decl.is_state;
    let type_constraint = &decl.type_constraint;
    let group_default_expr = &decl.group_default;
    let mut stmts = Vec::new();
    for dvar in vars {
        let effective_tc = dvar
            .per_var_type_constraint
            .clone()
            .or_else(|| type_constraint.clone());
        let expr = if let Some(def_expr) = group_default_expr {
            dvar.default.clone().unwrap_or_else(|| def_expr.clone())
        } else if let Some(default) = &dvar.default {
            default.clone()
        } else if dvar.name.starts_with('@') {
            Expr::Literal(Value::real_array(Vec::new()))
        } else if dvar.name.starts_with('%') {
            Expr::Hash(Vec::new(), crate::ast::HashSpelling::Composer)
        } else {
            native_type_default(&effective_tc)
        };
        let traits = if let Some(def_expr) = group_default_expr {
            vec![("default".to_string(), Some(def_expr.clone()))]
        } else {
            Vec::new()
        };
        stmts.push(Stmt::VarDecl {
            name: dvar.name.clone(),
            expr,
            type_constraint: effective_tc,
            is_state,
            is_our: false,
            is_dynamic: false,
            is_export: false,
            export_tags: Vec::new(),
            custom_traits: traits,
            where_constraint: None,
        });
    }
    stmts
}

/// `my (…) = RHS` / `my (…) := RHS` (positional elements).
fn expand_assigned(decl: &SignatureDecl, init: &SignatureInit) -> (Vec<Stmt>, Expr) {
    let vars = &decl.vars;
    let is_state = decl.is_state;
    let is_our = decl.is_our;
    let type_constraint = &decl.type_constraint;
    let has_nested_group = decl.has_nested_group;
    let is_binding = init.is_binding;
    let raw_rhs = init.rhs.clone();
    // List-assignment iterates the RHS with one level of decont (Rakudo
    // List.STORE): `my ($a, $b) = $row` where `$row` holds an itemized Array
    // flattens into its elements, while `= $row,` (a comma list) keeps the
    // itemized value whole. `__mutsu_list_assign_rhs` deitemizes exactly the
    // single-itemized-container shape and passes everything else through —
    // unlike `.list`, it leaves a Failure RHS intact (`my ($x) = @e.shift`
    // on an empty array stores the Failure, it does not throw). Binding
    // (`:=`) keeps its historical `.list` wrap; named destructuring keeps
    // the raw value (it subscripts it).
    let rhs = if is_binding {
        Expr::MethodCall {
            target: Box::new(raw_rhs),
            name: Symbol::intern("list"),
            args: vec![],
            modifier: None,
            quoted: false,
            on_topic: false,
        }
    } else {
        Expr::Call {
            name: Symbol::intern("__mutsu_list_assign_rhs"),
            args: vec![raw_rhs],
            listop: false,
        }
    };

    // Positional destructuring
    let tmp_name = "@__destructure_tmp__".to_string();
    let array_bare = "__destructure_tmp__".to_string();
    // NOTE: this staging temp is NOT a user `Array` -- it IS the RHS list, and
    // every target below reads a VALUE out of it. ADR-0040 slice 2's
    // element-itemization is therefore deliberately suppressed for it; see
    // `Interpreter::is_destructure_staging_temp`.
    let tmp_decl = Stmt::VarDecl {
        name: tmp_name,
        expr: rhs,
        type_constraint: None,
        is_state: false,
        is_our: false,
        is_dynamic: false,
        is_export: false,
        export_tags: Vec::new(),
        custom_traits: Vec::new(),
        where_constraint: None,
    };
    // In BINDING mode the staging temp must keep the RHS elements' CONTAINERS,
    // not copies of their values: `my (\a, \b) := ($x, $y)` makes `a` an alias
    // of `$x`, so `a = 10` has to reach `$x`. Declaring the temp with `MarkBind`
    // (the same marker `my @t := (...)` uses) keeps the element cells the RHS
    // list already carries; a plain assigning declaration deitemizes them away.
    // Targets that read a VALUE out of the temp are unaffected -- a `$` target
    // in binding mode is a read-only COPY in raku too (`my ($a,$b) := ($x,$y);
    // $x = 7` leaves `$a` at its original value).
    let mut stmts = Vec::new();
    // In a signature declaration the new variables are already in scope on the
    // RHS and hold their defaults (`my $x = 5; { my ($x, $y) = $x, 2 }` reads
    // the new `$x`, i.e. `(Any)`), so declare the plain targets before the RHS
    // runs. The real declarations below then assign into them.
    if !is_binding && !is_state && !is_our {
        for dvar in vars {
            if dvar.literal_value.is_some()
                || dvar.sigilless
                || dvar.per_var_type_constraint.is_some()
                || type_constraint.is_some()
                || dvar.where_constraint.is_some()
                || dvar.is_slurpy
                || dvar.name.starts_with('&')
            {
                continue;
            }
            let expr = if dvar.name.starts_with('@') {
                Expr::ArrayLiteral(Vec::new())
            } else if dvar.name.starts_with('%') {
                Expr::Hash(Vec::new(), crate::ast::HashSpelling::Composer)
            } else {
                Expr::Literal(Value::NIL)
            };
            stmts.push(Stmt::VarDecl {
                name: dvar.name.clone(),
                expr,
                type_constraint: None,
                is_state: false,
                is_our: false,
                is_dynamic: false,
                is_export: false,
                export_tags: Vec::new(),
                custom_traits: Vec::new(),
                where_constraint: None,
            });
        }
    }
    if is_binding {
        stmts.push(Stmt::SyntheticBlock(vec![Stmt::MarkBind, tmp_decl]));
    } else {
        stmts.push(tmp_decl);
    }
    // A declarator list bound with `:=` is a signature: a positional count
    // outside its required..max range dies before anything is bound, exactly
    // as a routine call does (`my ($p, $q) := (1,)` is "Too few positionals
    // passed"). Assignment (`=`) stays lenient. A nested group is skipped:
    // its leaves are flattened into `vars`, so their count is not the arity.
    if is_binding && !has_nested_group {
        push_bind_arity_check(&mut stmts, vars, &array_bare);
    }
    // List ASSIGNMENT (`=`) and signature BINDING (`:=`) differ here:
    //  - assignment: the FIRST `@`/`%` target is greedy — it slurps all
    //    remaining RHS values, and every target after it receives an empty
    //    container / Nil (`my ($a, @b, $c) = 1..4` → `@b` = `[2,3,4]`, `$c` = Any).
    //  - binding: a plain `@`/`%` binds ONE positional argument; only an
    //    explicit `*@rest` is slurpy (`my ($x, @y, *@r) := (42,[13,17],5,6,7)`
    //    → `@y` = `[13,17]`, `@r` = `[5,6,7]`).
    // So the greedy behaviour applies only in assignment mode. In binding mode a
    // trailing `@x` is NOT slurpy: `my (@a, @b) := (@x, @y)` binds `@b` to `@y`,
    // not to `(@y,)`. (Rakudo type-checks each element as Positional, so the
    // shapes where the distinction is invisible are the ones it rejects outright.)
    let mut seen_slurpy = false;
    for (i, dvar) in vars.iter().enumerate() {
        if let Some(lit) = &dvar.literal_value {
            // A bare literal element (`my ("foo") = ...`) is a postconstraint: the
            // i-th assigned value must smartmatch the literal, else
            // X::TypeCheck::Assignment. Emit a throwaway declaration whose
            // where-constraint IS the literal (identical to `$ where "foo"`, which
            // already enforces this), reading the i-th temp element. (subtypes.t 90)
            let read = Expr::Index {
                target: Box::new(Expr::ArrayVar(array_bare.clone())),
                index: Box::new(Expr::Literal(Value::int(i as i64))),
                is_positional: true,
                spelling: Default::default(),
            };
            stmts.push(Stmt::VarDecl {
                name: format!("__destructure_lit_{i}"),
                expr: read,
                type_constraint: None,
                is_state,
                is_our: false,
                is_dynamic: false,
                is_export: false,
                export_tags: Vec::new(),
                custom_traits: Vec::new(),
                where_constraint: Some(Box::new(lit.clone())),
            });
            continue;
        }

        let is_array = dvar.name.starts_with('@');
        let is_hash = dvar.name.starts_with('%');
        let is_implicit_slurpy = !is_binding && !seen_slurpy && (is_array || is_hash);

        let effective_tc = dvar
            .per_var_type_constraint
            .clone()
            .or_else(|| type_constraint.clone());
        let expr = if !is_binding && seen_slurpy {
            // A target after a greedy slurp (assignment mode) gets an empty
            // container / Nil.
            if is_array {
                Expr::ArrayLiteral(Vec::new())
            } else if is_hash {
                Expr::Hash(Vec::new(), crate::ast::HashSpelling::Composer)
            } else {
                Expr::Literal(Value::NIL)
            }
        } else if dvar.is_slurpy || is_implicit_slurpy {
            seen_slurpy = true;
            Expr::Index {
                target: Box::new(Expr::ArrayVar(array_bare.clone())),
                index: Box::new(Expr::Binary {
                    left: Box::new(Expr::Literal(Value::int(i as i64))),
                    op: TokenKind::DotDot,
                    right: Box::new(Expr::Whatever),
                    form: Default::default(),
                }),
                is_positional: true,
                spelling: Default::default(),
            }
        } else {
            let read = Expr::Index {
                target: Box::new(Expr::ArrayVar(array_bare.clone())),
                index: Box::new(Expr::Literal(Value::int(i as i64))),
                is_positional: true,
                spelling: Default::default(),
            };
            // A *typed* element whose RHS ran out of values gets the type's
            // DEFAULT, not the `Any` an out-of-range Array read now yields
            // (`my Str ($a) = ()` → `$a` is `Str`, not the un-assignable `Any`).
            // Untyped vars keep the raw `Any`. The `// default` fallback fires
            // only for an undefined (missing) read, so present values pass through.
            let read = if effective_tc.is_some() {
                Expr::Binary {
                    left: Box::new(read),
                    op: TokenKind::SlashSlash,
                    right: Box::new(native_type_default(&effective_tc)),
                    form: Default::default(),
                }
            } else {
                read
            };
            // A bound optional element (`$y?`, `$y = 5`) the RHS did not
            // reach takes its default, as an optional parameter does: the
            // default expression, else the constraint's type object (`Mu`
            // when untyped). The arity check above already refused a short
            // RHS for every required element.
            if is_binding && (dvar.is_optional || dvar.default.is_some()) {
                let fallback = dvar
                    .default
                    .clone()
                    .unwrap_or_else(|| optional_param_default(&effective_tc));
                Expr::Ternary {
                    cond: Box::new(staged_exists(&array_bare, i)),
                    then_expr: Box::new(read),
                    else_expr: Box::new(fallback),
                }
            } else {
                read
            }
        };
        // In BINDING mode a non-slurpy `@`/`%` target BINDS the staged element
        // rather than assigning it: `my @x = 1, 2; my (@a,) := (@x,);
        // @a.push(3)` writes through to `@x` in raku, so `@a` must be the
        // element itself and not a copy. `MarkBind` is the same marker the
        // plain `my @a := expr` declaration uses.
        //
        // A slurpy `*@rest` is excluded: its read is a SLICE of the staging
        // temp (a freshly built `List`), and raku gives `@rest` an `Array`
        // there (`my ($x, @y, *@rest) := (42, [13,17], 5, 6, 7)` leaves
        // `@rest.raku` as `[5, 6, 7]`), which is what the assigning form's
        // `coerce_to_array` produces.
        // Pinned by `t/list-bind-trailing-array.t` and
        // `roast/S02-names-vars/signature.t`.
        //
        // A SIGILLESS target binds the same way for the same reason: `my (\a,
        // \b) := ($x, $y)` aliases `$x`/`$y`, exactly as the single-variable
        // `my \a := $x` does. That form emits `MarkBind` + the declaration +
        // `MarkSigilless` (see `my_decl_helpers::build_sigilless_bind_stmt`),
        // which leaves writability to the runtime `MarkSigillessBind` check --
        // so a non-container element (`my (\a) := (5,)`) still stays immutable.
        //
        // A `$` target carrying `is rw` / `is raw` (`my ($a is rw) := ($x,)`)
        // is a signature parameter that binds the argument's container too, so
        // it aliases the staged element like a sigilless target does; the
        // element's own writability then decides whether `$a = 5` succeeds.
        // TODO: rakudo refuses `is rw` against a non-container at BIND time
        // (X::Parameter::RW); here the refusal only comes at the first write.
        let binds_container_trait =
            matches!(dvar.param_trait, Some(ParamTrait::Rw | ParamTrait::Raw));
        let binds_element = is_binding
            && !dvar.is_slurpy
            && !is_implicit_slurpy
            && (dvar.sigilless || binds_container_trait || dvar.name.starts_with(['@', '%']));
        let effective_where = dvar.where_constraint.clone().map(Box::new);
        // A `$` target that binds its element (`is rw` / `is raw`) is the
        // same scalar bind `my $a := EXPR` lowers to, and carries the same
        // `__scalar_bind` marker, so an immutable element (`my ($a is rw) :=
        // (5,)`) stays immutable instead of getting a fresh container.
        let custom_traits =
            if binds_element && !dvar.sigilless && !dvar.name.starts_with(['@', '%']) {
                vec![("__scalar_bind".to_string(), None)]
            } else {
                Vec::new()
            };
        let decl = Stmt::VarDecl {
            name: dvar.name.clone(),
            expr,
            type_constraint: effective_tc,
            is_state,
            is_our,
            is_dynamic: false,
            is_export: false,
            export_tags: Vec::new(),
            custom_traits,
            where_constraint: effective_where,
        };
        let decl = if binds_element && dvar.sigilless {
            // The same block shape `my \a := $x` uses
            // (`my_decl_helpers::build_sigilless_bind_stmt`): the trailing
            // `MarkSigilless` has to sit INSIDE the block, because that is how
            // the compiler learns -- before compiling the declaration -- that
            // this bind's target is sigilless.
            Stmt::SyntheticBlock(vec![
                Stmt::MarkBind,
                decl,
                Stmt::MarkSigilless(dvar.name.clone()),
            ])
        } else if binds_element {
            Stmt::SyntheticBlock(vec![Stmt::MarkBind, decl])
        } else {
            decl
        };
        stmts.push(decl);
        if dvar.sigilless && !binds_element {
            stmts.push(Stmt::MarkSigillessReadonly(dvar.name.clone()));
        }
        // `is copy` / `is readonly` fall through to the read-only copy: rakudo
        // does not give an `is copy` element of a `my (...)` bind a writable
        // container either (`my ($a is copy) := ($x,); $a = 3` dies).
        if is_binding && !binds_element && dvar.name.starts_with(|c: char| c != '@' && c != '%') {
            stmts.push(Stmt::MarkReadonly(
                dvar.name.clone(),
                crate::ast::ReadonlyKind::Immutable,
            ));
        }
    }
    // Yield the assigned list as the block's value (`(my ($a,$b) = 1,2)` is `(1 2)`,
    // not the last element). This also keeps the per-element check declarations off
    // the block-final position, so a postconstraint (`where`/literal) on the LAST
    // element still enforces in value context — e.g. an EVAL'd `my (\b, "foo") =
    // ...` whose trailing `MarkSigillessReadonly` would otherwise leave a
    // constrained decl block-final and skip its check. (subtypes.t 90)
    //
    // In ASSIGNMENT mode the value is the LHS after the assignment -- the
    // declared targets themselves, as Rakudo's `List.STORE` returns its
    // invocant: `(my ($x, $y) = 1, 2, 3)` is `$(1, 2)`, and an infinite RHS
    // (`my ($x, $y) = 1 xx *`) must not leak out, since sinking it would force
    // it (#9342). A literal postconstraint element yields its staged value.
    // Binding mode keeps yielding the staged RHS list.
    let result = if is_binding {
        Expr::ArrayVar(array_bare)
    } else {
        Expr::ArrayLiteral(
            vars.iter()
                .enumerate()
                .map(|(i, dvar)| {
                    if dvar.literal_value.is_some() {
                        Expr::Index {
                            target: Box::new(Expr::ArrayVar(array_bare.clone())),
                            index: Box::new(Expr::Literal(Value::int(i as i64))),
                            is_positional: true,
                            spelling: Default::default(),
                        }
                    } else if dvar.sigilless {
                        Expr::BareWord(dvar.name.clone())
                    } else if let Some(n) = dvar.name.strip_prefix('@') {
                        Expr::ArrayVar(n.to_string())
                    } else if let Some(n) = dvar.name.strip_prefix('%') {
                        Expr::HashVar(n.to_string())
                    } else if let Some(n) = dvar.name.strip_prefix('&') {
                        Expr::CodeVar(n.to_string())
                    } else {
                        Expr::Var(dvar.name.clone())
                    }
                })
                .collect(),
        )
    };
    (stmts, result)
}
