//! `$?LANG` as a language a later compilation unit can be parsed in (#17087).
//!
//! A REPL session hands a unit's language on to the next unit the way rakudo's
//! `REPL` and Jupyter::Kernel do:
//!
//! ```raku
//! $lang := $?LANG;                                   # in the unit that ran `use Slang::X`
//! $compiler.parsegrammar($lang.slang_grammar('MAIN'));
//! $compiler.eval($next-line, :outer_ctx($ctx));      # parsed with Slang::X active
//! ```
//!
//! mutsu's grammar is not an object, and a slang is a set of parser modes and
//! productions its `use` installed (ADR-0026). What the language of a point in
//! the unit *is*, then, is the list of slang activations in force there: the
//! `use`d module and its literal arguments, in order. `$?LANG` carries that
//! list (as literal arguments of `__mutsu_comp_lang`, so it is a constant of
//! the place it is written, like `$?LINE`), the grammar handle
//! `slang_grammar` answers carries it on, and `Perl6::Compiler.eval` seeds
//! the next unit's parse with it, which activates each slang again before the
//! unit's first statement. Re-activating a slang installs its grammar; it
//! imports nothing, as setting `parsegrammar` imports nothing in rakudo.

use super::slang_use::SlangUseArg;
use crate::ast::Expr;
use crate::symbol::Symbol;
use crate::value::{Value, ValueView};
use std::cell::RefCell;

/// One slang activation: the `use`d module and the `use` statement's literal
/// arguments (`None` when it had none).
pub(crate) type SlangUseSpec = (String, Option<Vec<SlangUseArg>>);

thread_local! {
    /// The slangs the next unit parse starts with, consumed by that parse.
    static SLANG_PRESEED: RefCell<Vec<SlangUseSpec>> = const { RefCell::new(Vec::new()) };
}

/// Seed the slangs the next unit parse starts with; an empty list clears a
/// seed no parse consumed.
// Cost: O(1).
pub(crate) fn set_slang_preseed(activations: Vec<SlangUseSpec>) {
    SLANG_PRESEED.with(|p| *p.borrow_mut() = activations);
}

/// Activate the seeded slangs for the unit whose parse just started.
// Cost: one slang activation per seeded slang.
pub(in crate::parser) fn apply_slang_preseed() -> Result<(), String> {
    let seed = SLANG_PRESEED.with(|p| std::mem::take(&mut *p.borrow_mut()));
    for (module, args) in seed {
        super::slang_use::activate_slang_module(&module, args)?;
    }
    Ok(())
}

/// `$?LANG` where the parser is, when a slang is active there: the call that
/// builds the language object with that slang list. `None` is the stock
/// language, which the compiler builds from the plain variable.
// Cost: O(a), a = slang activations in force.
pub(in crate::parser) fn comp_lang_expr() -> Option<Expr> {
    let activations = super::slang_hooks::slang_activations();
    if activations.is_empty() {
        return None;
    }
    Some(Expr::Call {
        name: Symbol::intern("__mutsu_comp_lang"),
        args: encode_slang_activations(&activations)
            .into_iter()
            .map(Expr::Literal)
            .collect(),
        listop: false,
    })
}

/// The flat literal form of an activation list: per activation the module
/// name and its argument count (`-1` for none), then per argument a tag
/// (`s`/`i`/`n`/`b`, or `r` for a routine) and its value (a routine's name and
/// declaration text).
// Cost: O(a), a = activations and their arguments.
pub(crate) fn encode_slang_activations(activations: &[SlangUseSpec]) -> Vec<Value> {
    let mut out = Vec::new();
    for (module, args) in activations {
        out.push(Value::str(module.clone()));
        out.push(Value::int(args.as_ref().map_or(-1, |a| a.len() as i64)));
        for arg in args.iter().flatten() {
            match arg {
                SlangUseArg::Str(s) => out.extend([Value::str_from("s"), Value::str(s.clone())]),
                SlangUseArg::Int(n) => out.extend([Value::str_from("i"), Value::int(*n)]),
                SlangUseArg::Num(f) => out.extend([Value::str_from("n"), Value::num(*f)]),
                SlangUseArg::Bool(b) => out.extend([Value::str_from("b"), Value::truth(*b)]),
                SlangUseArg::Routine { name, source } => out.extend([
                    Value::str_from("r"),
                    Value::str(name.clone()),
                    Value::str(source.clone()),
                ]),
            }
        }
    }
    out
}

/// The activation list [`encode_slang_activations`] flattened, or as much of
/// it as is well formed.
// Cost: O(v), v = values.
pub(crate) fn decode_slang_activations(values: &[Value]) -> Vec<SlangUseSpec> {
    let mut out = Vec::new();
    let mut it = values.iter().map(Value::view);
    while let Some(ValueView::Str(module)) = it.next() {
        let Some(ValueView::Int(count)) = it.next() else {
            break;
        };
        let mut args = (count >= 0).then(Vec::new);
        for _ in 0..count.max(0) {
            let arg = match (it.next(), it.next()) {
                (Some(ValueView::Str(tag)), Some(value)) => match (tag.as_str(), value) {
                    ("s", ValueView::Str(s)) => SlangUseArg::Str(s.to_string()),
                    ("i", ValueView::Int(n)) => SlangUseArg::Int(n),
                    ("n", ValueView::Num(f)) => SlangUseArg::Num(f),
                    ("b", ValueView::Bool(b)) => SlangUseArg::Bool(b),
                    ("r", ValueView::Str(name)) => match it.next() {
                        Some(ValueView::Str(source)) => SlangUseArg::Routine {
                            name: name.to_string(),
                            source: source.to_string(),
                        },
                        _ => return out,
                    },
                    _ => return out,
                },
                _ => return out,
            };
            if let Some(args) = args.as_mut() {
                args.push(arg);
            }
        }
        out.push((module.to_string(), args));
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn activation_lists_round_trip() {
        let list = vec![
            ("Slang::A".to_string(), None),
            (
                "Slang::B".to_string(),
                Some(vec![
                    SlangUseArg::Str("x".to_string()),
                    SlangUseArg::Int(3),
                    SlangUseArg::Bool(true),
                    SlangUseArg::Routine {
                        name: "f".to_string(),
                        source: "sub f { }".to_string(),
                    },
                ]),
            ),
            ("Slang::C".to_string(), Some(Vec::new())),
        ];
        assert_eq!(
            decode_slang_activations(&encode_slang_activations(&list)),
            list
        );
    }
}
