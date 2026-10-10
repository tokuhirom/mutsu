//! Source spelling and parsed expressions for parameterized type arguments.

use super::convert::{build_type_node, convert_expr, is_simple_type, node_field, unsupported};
use super::{RakuAstClass, RakuAstNode};
use crate::ast::Expr;
use crate::value::RuntimeError;

/// Yield comma-separated arguments without splitting inside brackets or quotes.
struct ArgSources<'a> {
    rest: &'a str,
}

impl<'a> Iterator for ArgSources<'a> {
    type Item = &'a str;

    fn next(&mut self) -> Option<Self::Item> {
        if self.rest.is_empty() {
            return None;
        }
        let mut depth = 0usize;
        let mut quote = None;
        let mut escaped = false;
        for (i, ch) in self.rest.char_indices() {
            if escaped {
                escaped = false;
                continue;
            }
            if ch == '\\' {
                escaped = true;
                continue;
            }
            if let Some(open) = quote {
                if ch == open {
                    quote = None;
                }
                continue;
            }
            match ch {
                '\'' | '"' => quote = Some(ch),
                '(' | '[' | '{' | '<' => depth += 1,
                ')' | ']' | '}' | '>' => depth = depth.saturating_sub(1),
                ',' if depth == 0 => {
                    let source = &self.rest[..i];
                    self.rest = &self.rest[i + 1..];
                    return Some(source.trim());
                }
                _ => {}
            }
        }
        let source = self.rest.trim();
        self.rest = "";
        Some(source)
    }
}

/// Whether a bareword argument's source spells a type: a simple one (`Int`)
/// or one with a definedness smiley (`Map:D`), which rakudo renders as a
/// `Type::Definedness`.
fn is_type_spelling(source: &str) -> bool {
    let base = source
        .strip_suffix(":D")
        .or_else(|| source.strip_suffix(":U"))
        .unwrap_or(source);
    is_simple_type(base)
}

/// Convert a type application using its parsed argument expressions. Only a
/// type-only constraint without parsed arguments needs the spelling scanner.
pub(super) fn parameterized_type_node(
    spelling: &str,
    parsed: Option<&[Expr]>,
) -> Result<RakuAstNode, RuntimeError> {
    let open = spelling
        .find('[')
        .ok_or_else(|| unsupported("malformed parameterised type"))?;
    let inner = spelling
        .strip_suffix(']')
        .ok_or_else(|| unsupported("malformed parameterised type"))?;
    let base = &spelling[..open];
    if !is_simple_type(base) {
        return Err(unsupported("parameterised type over a non-simple base"));
    }
    let sources = ArgSources {
        rest: &inner[open + 1..],
    };
    let mut fields = Vec::new();
    if let Some(parsed) = parsed {
        for expr in parsed {
            // BinaryForm already retains every colonpair spelling, including
            // booleans, variable pairs and bracketed values. Use the same
            // conversion as a pair outside a type application.
            let node = if let Expr::BareWord(name) = expr
                && is_type_spelling(name)
            {
                build_type_node(name)?
            } else {
                convert_expr(expr)?
            };
            fields.push(node_field(None, node));
        }
    } else {
        for source in sources {
            fields.push(node_field(None, build_type_node(source)?));
        }
    }
    Ok(RakuAstNode {
        class: RakuAstClass::TypeParameterized,
        fields: vec![
            node_field(Some("base-type"), super::bareword::simple_type_node(base)),
            node_field(
                Some("args"),
                RakuAstNode {
                    class: RakuAstClass::ArgList,
                    fields,
                },
            ),
        ],
    })
}
