//! `RakuAST::Doc::LegacyRow.Str(&divider)`: a table row as written, each
//! divider passed through the callable. The deparser calls it with its
//! highlighter (`$ast.Str: { self.hsyn('rakudoc-divider', $_) }`), so a table
//! block deparses (#14880). The text is `crate::rakuast::legacy_row_text`,
//! which the plain `.Str` uses too.

use super::super::{Handler, MethodRow, Named, RowFlags};
use super::rakuast_visit::native_node;
use crate::runtime::Interpreter;
use crate::value::{RuntimeError, Value, ValueView};

// Cost: O(n) plus one call of the callable per divider and line, n = size of
// the cells.
fn row_str_with(
    interp: &mut Interpreter,
    target: &Value,
    args: &[Value],
    _named: Named<'_>,
) -> Option<Result<Value, RuntimeError>> {
    let base = native_node(target)?;
    let ValueView::RakuAst(node) = base.view() else {
        return None;
    };
    if node.class != crate::rakuast::RakuAstClass::DocLegacyRow {
        return None;
    }
    let callable = args.first()?.clone();
    let text = crate::rakuast::legacy_row_text(node, &mut |divider| {
        interp
            .call_sub_value(callable.clone(), vec![Value::str(divider.to_string())])
            .map(|v| v.to_string_value())
    });
    Some(text.map(Value::str))
}

pub(super) static ROWS: &[MethodRow] = &[MethodRow {
    owner: "RakuAST::Doc::LegacyRow",
    name: "Str",
    arity: 1,
    handler: Handler::Interp(row_str_with),
    flags: RowFlags::OWNER_ONLY,
    named: &[],
}];
