//! Lowering a signature's destructuring sub-signature into ordinary statements.
//!
//! A parameter with a `sub_signature` (`-> ($a, $b)`, `-> (:$key, :$value)`,
//! `-> % [:@dists]`) unpacks one bound value into several lexicals. That unpack
//! is the same operation wherever it appears, so it lives here once instead of
//! being restated per construct: `for`'s loop-parameter lowering
//! (`Compiler::build_for_bind_stmts`) and the topic-binding control statements
//! (`given` / `with` / `without`, in the parser's `pointy_topic_bind`) both
//! call it.
//!
//! Each of those used to carry its own copy. `given` dropped the sub-signature
//! outright, so `given $obj -> (:@list) { }` bound nothing; `with` kept a copy
//! that called a method literally named `@list` (sigil included) and knew
//! nothing of hash fallback, `|` captures or defaults. The `for` copy was the
//! complete one, and it is the one that survives here.

use crate::ast::{AssignOp, Expr, Stmt};
use crate::symbol::Symbol;
use crate::value::Value;

/// Bind one destructure target, assigning unless the target must be declared.
pub(crate) fn bind_stmt(name: String, expr: Expr) -> Stmt {
    // A destructured signature parameter DECLARES its target, so a
    // dynamic-twigil target (`:value($*PATH)`) must be introduced as a
    // fresh dynamic var (VarDecl with is_dynamic), not treated as a bare
    // assignment to a pre-existing dynamic var (which would wrongly throw
    // X::Dynamic::NotFound). `&` targets likewise declare.
    let is_dynamic_target = name
        .trim_start_matches(['$', '@', '%', '&'])
        .starts_with('*');
    if name.starts_with('&') || is_dynamic_target {
        Stmt::VarDecl {
            name,
            expr,
            type_constraint: None,
            is_state: false,
            is_our: false,
            is_dynamic: is_dynamic_target,
            is_export: false,
            export_tags: Vec::new(),
            custom_traits: Vec::new(),
            where_constraint: None,
        }
    } else {
        Stmt::Assign {
            name,
            expr,
            op: AssignOp::Assign,
            target_is_sigilless: false,
        }
    }
}

/// Declare one destructure target.
///
/// A destructured sub-signature target (`-> % [:@dists]`, `-> @ ($a,@b)`)
/// is a fresh block-scoped lexical that must SHADOW any outer variable of
/// the same name -- a plain `Stmt::Assign` would instead resolve up the
/// scope chain and clobber the outer var (e.g. zef's
/// `my Candidate @dists = gather for @x -> % [:@dists] {...}`, where the
/// inner `:@dists` collided with the outer typed `@dists`).
pub(crate) fn decl_stmt(name: String, expr: Expr) -> Stmt {
    let is_dynamic_target = name
        .trim_start_matches(['$', '@', '%', '&'])
        .starts_with('*');
    Stmt::VarDecl {
        name,
        expr,
        type_constraint: None,
        is_state: false,
        is_our: false,
        is_dynamic: is_dynamic_target,
        is_export: false,
        export_tags: Vec::new(),
        custom_traits: Vec::new(),
        where_constraint: None,
    }
}

/// Apply a sub-parameter's COERCION type to the value extracted for it.
///
/// A coercion constraint is recorded by the parameter parser as `Target()` or
/// `Target(Source)`, so the target is whatever precedes the `(`. A plain
/// nominal constraint (`-> (Int $a)`) is left alone: rakudo type-CHECKS that,
/// it does not coerce, and calling `.Int` on the value would silently convert
/// what should have been a binding error.
fn apply_coercion(sub: &crate::ast::ParamDef, value: Expr) -> Expr {
    let Some(tc) = sub.type_constraint.as_deref() else {
        return value;
    };
    let Some(target) = tc.strip_suffix(')').and_then(|t| t.split('(').next()) else {
        return value;
    };
    // An indirect type constraint is recorded as `::(EXPR)`, which also ends
    // in `)`; its "target" would be `::`. Only a real type name coerces.
    if !target.starts_with(|c: char| c.is_alphabetic() || c == '_') {
        return value;
    }
    Expr::MethodCall {
        target: Box::new(value),
        name: Symbol::intern(target),
        args: Vec::new(),
        modifier: None,
        quoted: false,
        sugar: false,
    }
}

/// Unpack the value bound to `target_name` into the sub-signature's lexicals,
/// appending one statement per sub-parameter to `bind_stmts`.
pub(crate) fn destructure_binds(
    target_name: &str,
    sub_params: &[crate::ast::ParamDef],
    bind_stmts: &mut Vec<Stmt>,
) {
    let mut positional_index = 0usize;
    for sub in sub_params {
        if sub.name.is_empty() {
            continue;
        }
        if sub.named {
            // Named destructuring `:$key` binds via the accessor method
            // when the object provides one (Pair.key/.value, object
            // attribute readers), otherwise by hash key (Hash/Map, which
            // have no method named after an arbitrary key). Decide at
            // runtime: `$_.^can("key") ?? $_.key !! $_<key>`.
            //
            // A scalar named sub-param `:$curi` stores its name sigil-
            // stripped ("curi"), but an `@`/`%` named sub-param `:@dists`
            // keeps its sigil in `sub.name` ("@dists"). The accessor and
            // hash key must use the *key* name ("dists"), so strip a
            // leading array/hash sigil (and any twigil) before looking up;
            // the sigil is kept only for the bind target below so the
            // value lands in an `@`/`%` container.
            let lookup_name = named_lookup_name(&sub.name);
            let method_call = Expr::MethodCall {
                target: Box::new(Expr::Var(target_name.to_string())),
                name: Symbol::intern(&lookup_name),
                args: Vec::new(),
                modifier: None,
                quoted: false,
                sugar: false,
            };
            let hash_lookup = Expr::Index {
                target: Box::new(Expr::Var(target_name.to_string())),
                index: Box::new(Expr::Literal(Value::str(lookup_name.clone()))),
                is_positional: false,
                spelling: Default::default(),
            };
            let method_result = Expr::Ternary {
                cond: Box::new(Expr::MethodCall {
                    target: Box::new(Expr::Var(target_name.to_string())),
                    name: Symbol::intern("can"),
                    args: vec![Expr::Literal(Value::str(lookup_name.clone()))],
                    modifier: Some('^'),
                    quoted: false,
                    sugar: false,
                }),
                then_expr: Box::new(method_call),
                else_expr: Box::new(hash_lookup),
            };
            let method_result = apply_coercion(sub, method_result);
            // Whether the value's Capture carries this named argument at all
            // (`\(:key, :value)` for a Pair, the pairs of a Hash, the public
            // attributes of an object) — what a default or a `!` keys off.
            let passed = || Expr::MethodCall {
                target: Box::new(Expr::MethodCall {
                    target: Box::new(Expr::MethodCall {
                        target: Box::new(Expr::Var(target_name.to_string())),
                        name: Symbol::intern("Capture"),
                        args: Vec::new(),
                        modifier: None,
                        quoted: false,
                        sugar: false,
                    }),
                    name: Symbol::intern("hash"),
                    args: Vec::new(),
                    modifier: None,
                    quoted: false,
                    sugar: false,
                }),
                name: Symbol::intern("EXISTS-KEY"),
                args: vec![Expr::Literal(Value::str(lookup_name.clone()))],
                modifier: None,
                quoted: false,
                sugar: false,
            };
            if sub.required {
                bind_stmts.push(Stmt::Expr(Expr::Ternary {
                    cond: Box::new(passed()),
                    then_expr: Box::new(Expr::Literal(Value::NIL)),
                    else_expr: Box::new(Expr::Call {
                        name: Symbol::intern("die"),
                        args: vec![Expr::Literal(Value::str(format!(
                            "Required named argument '{lookup_name}' not passed in sub-signature"
                        )))],
                        listop: false,
                    }),
                }));
            }
            // `:$rule = 'TOP'`: the default stands in for an absent argument.
            let method_result = match &sub.default {
                Some(default) => Expr::Ternary {
                    cond: Box::new(passed()),
                    then_expr: Box::new(method_result),
                    else_expr: Box::new(default.clone()),
                },
                None => method_result,
            };
            // If the named param has a sub_signature (e.g. :key($k)),
            // bind to the sub_signature variable instead of the param name.
            if let Some(inner_params) = &sub.sub_signature {
                for inner in inner_params {
                    if inner.name.is_empty() {
                        continue;
                    }
                    // `:value(($n))`: the inner parameter is itself a
                    // sub-signature — bind the value to a temp, then unpack it.
                    if let Some(nested) = &inner.sub_signature {
                        let temp = nested_temp_name(target_name, &lookup_name, positional_index);
                        bind_stmts.push(decl_stmt(temp.clone(), method_result.clone()));
                        destructure_binds(&temp, nested, bind_stmts);
                    } else {
                        bind_stmts.push(decl_stmt(inner.name.clone(), method_result.clone()));
                    }
                }
            } else {
                // An `@`-sigil named sub-param binds like a signature
                // parameter: it flattens the (Positional) value's elements
                // into the array (shallow), unlike plain `my @x = $val`
                // assignment which keeps an itemized List as one element.
                // e.g. zef's `-> % [:@dists]` over `dists => $repo.installed`
                // (a 1-element List) must yield `@dists[0]` = the dist, not
                // a List wrapping it. `.list` gives the shallow flatten.
                let target_expr = if sub.name.starts_with('@') {
                    Expr::MethodCall {
                        target: Box::new(method_result),
                        name: Symbol::intern("list"),
                        args: Vec::new(),
                        modifier: None,
                        quoted: false,
                        sugar: false,
                    }
                } else {
                    method_result
                };
                bind_stmts.push(decl_stmt(sub.name.clone(), target_expr));
            }
        } else if sub.slurpy && sub.sigilless {
            // |rest capture parameter: collect remaining elements into a Capture
            // Generates: rest = \(|target[positional_index..*])
            let slice_expr = Expr::Index {
                target: Box::new(Expr::Var(target_name.to_string())),
                index: Box::new(Expr::Binary {
                    left: Box::new(Expr::Literal(Value::int(positional_index as i64))),
                    op: crate::token_kind::TokenKind::DotDot,
                    right: Box::new(Expr::Whatever),
                    form: Default::default(),
                }),
                is_positional: true,
                spelling: Default::default(),
            };
            let capture_expr = Expr::CaptureLiteral(
                vec![Expr::Unary {
                    op: crate::token_kind::TokenKind::Pipe,
                    expr: Box::new(slice_expr),
                    word: false,
                }],
                true,
            );
            bind_stmts.push(bind_stmt(sub.name.clone(), capture_expr));
            // No need to increment positional_index; capture consumes all remaining
        } else if sub.is_variadic() && sub.name.starts_with('%') {
            // `*%rest`: the named part of the value's Capture minus the keys
            // the sub-signature's named parameters already took
            // (`-> % (:$rule!, *%expected)` over a Hash, PDF::Grammar).
            let named_part = Expr::MethodCall {
                target: Box::new(Expr::MethodCall {
                    target: Box::new(Expr::Var(target_name.to_string())),
                    name: Symbol::intern("Capture"),
                    args: Vec::new(),
                    modifier: None,
                    quoted: false,
                    sugar: false,
                }),
                name: Symbol::intern("hash"),
                args: Vec::new(),
                modifier: None,
                quoted: false,
                sugar: false,
            };
            bind_stmts.push(decl_stmt(sub.name.clone(), named_part));
            let taken: Vec<Expr> = sub_params
                .iter()
                .filter(|p| p.named && !p.is_variadic())
                .map(|p| Expr::Literal(Value::str(named_lookup_name(&p.name))))
                .collect();
            if !taken.is_empty() {
                bind_stmts.push(Stmt::Expr(Expr::MethodCall {
                    target: Box::new(Expr::Index {
                        target: Box::new(Expr::HashVar(sub.name[1..].to_string())),
                        index: Box::new(Expr::ArrayLiteral(taken)),
                        is_positional: false,
                        spelling: Default::default(),
                    }),
                    name: Symbol::intern("DELETE-KEY"),
                    args: Vec::new(),
                    modifier: None,
                    quoted: false,
                    sugar: false,
                }));
            }
        } else if sub.is_variadic() && sub.name.starts_with('@') {
            // `*@rest`: every positional element from here on, not just the
            // one at this index.
            let rest_slice = Expr::Index {
                target: Box::new(Expr::Var(target_name.to_string())),
                index: Box::new(Expr::Binary {
                    left: Box::new(Expr::Literal(Value::int(positional_index as i64))),
                    op: crate::token_kind::TokenKind::DotDot,
                    right: Box::new(Expr::Whatever),
                    form: Default::default(),
                }),
                is_positional: true,
                spelling: Default::default(),
            };
            bind_stmts.push(decl_stmt(sub.name.clone(), rest_slice));
        } else {
            let element_expr = Expr::Index {
                target: Box::new(Expr::Var(target_name.to_string())),
                index: Box::new(Expr::Literal(Value::int(positional_index as i64))),
                is_positional: false,
                spelling: Default::default(),
            };
            let element_expr = apply_coercion(sub, element_expr);
            // A container-sigil sub-parameter binds the element's aggregate,
            // not the Scalar/itemized holder used when that aggregate lives in
            // a positional slot. This is especially visible for an Array
            // literal such as `[ [3, 4], 0 ]`: `@index` must receive
            // `[3, 4]`, not a one-element array containing it.
            let element_expr = if sub.name.starts_with('@') {
                Expr::DeitemizeForBind(Box::new(element_expr))
            } else {
                element_expr
            };
            // An optional destructure param (`-> ($a, $b?)`) seeds its
            // type object (Mu for untyped — this is a block) when the
            // source has no element at this slot; a default binds the
            // default expression instead.
            let value_expr = if sub.default.is_some() || sub.optional_marker {
                let fallback = match &sub.default {
                    Some(default_expr) => default_expr.clone(),
                    None => {
                        let mut marked = sub.clone();
                        marked.mark_block_param();
                        Expr::Literal(crate::runtime::Interpreter::missing_optional_param_value(
                            &marked,
                        ))
                    }
                };
                Expr::Ternary {
                    cond: Box::new(Expr::Binary {
                        left: Box::new(Expr::MethodCall {
                            target: Box::new(Expr::Var(target_name.to_string())),
                            name: Symbol::intern("elems"),
                            args: Vec::new(),
                            modifier: None,
                            quoted: false,
                            sugar: false,
                        }),
                        op: crate::token_kind::TokenKind::Gt,
                        right: Box::new(Expr::Literal(Value::int(positional_index as i64))),
                        form: Default::default(),
                    }),
                    then_expr: Box::new(element_expr),
                    else_expr: Box::new(fallback),
                }
            } else {
                element_expr
            };
            if let Some(nested) = &sub.sub_signature {
                // `-> ($a, ($b, $c))` / `-> ($a, [$b, $c])`: the element is
                // unpacked again. Every nested pattern shares the parser's
                // placeholder name, so bind each to its own temp — reusing the
                // placeholder would let an inner unpack clobber the value a
                // later sibling still reads from.
                let temp = nested_temp_name(target_name, &sub.name, positional_index);
                bind_stmts.push(decl_stmt(temp.clone(), value_expr));
                destructure_binds(&temp, nested, bind_stmts);
            } else {
                bind_stmts.push(positional_decl_stmt(sub, value_expr));
            }
            positional_index += 1;
        }
    }
}

/// The key a named sub-parameter reads: its name without an `@`/`%` sigil or
/// a `!`/`.` twigil (`:@dists` reads `dists`).
// Cost: O(1).
fn named_lookup_name(name: &str) -> String {
    let after_sigil = name
        .strip_prefix('@')
        .or_else(|| name.strip_prefix('%'))
        .unwrap_or(name);
    after_sigil
        .strip_prefix('!')
        .or_else(|| after_sigil.strip_prefix('.'))
        .unwrap_or(after_sigil)
        .to_string()
}

/// Declare one positional destructure target (`-> ($a, @b, %c)`).
///
/// A sub-signature parameter is a fresh lexical of the block, so it is
/// declared rather than assigned. A bare `Stmt::Assign` resolved the name
/// dynamically instead: inside a routine it wrote an undeclared env name, and
/// a same-named readonly binding of the *caller* (a `for <x> -> $f { f() }`
/// loop alias) made the callee's `-> ($f, $w)` die with "Cannot assign to a
/// readonly variable or a value" (Prettier::Table's `!stringify-hrule`,
/// called from a test looping `-> $field`).
///
/// This follows the multi-parameter pointy-block rule in
/// `Compiler::build_for_bind_stmts`: an `@`/`%` target (or an `is rw` scalar)
/// BINDS the element, keeping its container -- a `%a is raw` sub-parameter
/// must keep the bound hash's `is default` (roast S02-names/is_default.t) --
/// while a plain scalar declares a fresh variable holding the element.
fn positional_decl_stmt(sub: &crate::ast::ParamDef, value_expr: Expr) -> Stmt {
    let name = &sub.name;
    let is_dynamic_target = name
        .trim_start_matches(['$', '@', '%', '&'])
        .starts_with('*');
    if name.starts_with('&') || is_dynamic_target {
        return bind_stmt(name.clone(), value_expr);
    }
    let binds_container = name.starts_with(['@', '%'])
        || sub.sigilless
        || sub.traits.iter().any(|t| t == "rw" || t == "raw");
    if binds_container {
        Stmt::SyntheticBlock(vec![Stmt::MarkBind, decl_stmt(name.clone(), value_expr)])
    } else {
        decl_stmt(name.clone(), value_expr)
    }
}

/// The temp a nested destructuring pattern is bound to before it is unpacked.
/// Derived from the enclosing target and the pattern's position so sibling and
/// nested patterns never share one.
fn nested_temp_name(target_name: &str, sub_name: &str, position: usize) -> String {
    let base = target_name.trim_start_matches(['$', '@', '%', '&', '\\']);
    format!("__destructure_{base}_{sub_name}_{position}")
}
