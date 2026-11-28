//! Compile-time values of external and imported RakuAST declarations.

use super::super::Named;
use crate::runtime::Interpreter;
use crate::runtime::lexical_imports::LexicalImport;
use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value, ValueView};

/// The value a declaration's `lexical-name` (`&say`, `&infix:<==>`, `Str`)
/// denotes in the asking compilation scope.
// Cost: O(s) for an imported routine, s = visible lexical import sets;
// otherwise the interpreter's name lookup cost.
pub(super) fn external_value(
    interp: &mut Interpreter,
    target: &Value,
    _args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    // A module-built Import can carry its value directly (CompUnit::Util).
    if let Some(value) =
        super::rakuast_compile_state::built_import_field(target, "compile-time-value")
    {
        return Some(Ok(value));
    }
    let ValueView::Instance {
        class_name,
        attributes,
        ..
    } = target.view()
    else {
        return None;
    };
    let name = attributes
        .as_map()
        .get("lexical-name")
        .map(|n| n.to_string_value())?;
    if let Some(routine) = name.strip_prefix('&') {
        if class_name == Symbol::intern("RakuAST::Declaration::Import") {
            let chain = interp.compile_time_import_chain();
            if let Some(binding) = interp.lexical_import_binding(&chain, Symbol::intern(routine)) {
                let value = match binding {
                    LexicalImport::Code(value) => value,
                    LexicalImport::Routine(def) => {
                        interp.sub_value_from_function_def((*def).clone())
                    }
                };
                return Some(Ok(value));
            }
        }
        let found = interp.resolve_code_var(routine);
        if !matches!(found.view(), ValueView::Sub(_))
            // Analysis can run in a module's scope while its import is in the caller's.
            && let Some(imported) = interp.unit_import_routine(crate::runtime::main_unit(), routine)
        {
            return Some(Ok(imported));
        }
        return Some(Ok(found));
    }
    if crate::core_type_names::contains(&name) {
        return Some(Ok(Value::package(Symbol::intern(&name))));
    }
    if let Some(term) = interp.compile_time_term(&name) {
        return Some(Ok(term));
    }
    Some(interp.eval_code_string(&name))
}
