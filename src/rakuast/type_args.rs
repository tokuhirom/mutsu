//! Source spelling and parsed expressions for parameterized type arguments.

use crate::scope::NameScope;
use super::convert::{convert_expr, is_simple_type, node_field, unsupported};
use super::type_build::{MAX_TYPE_DEPTH, application_open, build_type_node_at};
use super::{RakuAstClass, RakuAstNode};
use crate::ast::Expr;
use crate::value::RuntimeError;

/// Yield comma-separated arguments without splitting inside brackets or quotes.
struct ArgSources<'a> {
    rest: &'a str,
    separator: char,
}

pub(super) fn type_sources(source: &str, separator: char) -> impl Iterator<Item = &str> {
    ArgSources {
        rest: source,
        separator,
    }
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
        let mut previous = None;
        for (i, ch) in self.rest.char_indices() {
            let opens_quote = super::type_build::opens_quote(previous, ch);
            previous = Some(ch);
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
                '\'' | '"' if opens_quote => quote = Some(ch),
                '(' | '[' | '{' | '<' => depth += 1,
                ')' | ']' | '}' | '>' => depth = depth.saturating_sub(1),
                ch if ch == self.separator && depth == 0 => {
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
    let base = [":D", ":U", ":_"]
        .iter()
        .find_map(|smiley| super::type_build::strip_smiley(source, smiley))
        .unwrap_or(source);
    is_simple_type(base)
}

/// A colonpair argument (`:coerce(&f)`, `:!flag`) of a type application that
/// only survives as source text: parse it as the expression it is.
// Cost: O(n), n = argument spelling length.
fn colonpair_argument(names: &NameScope, source: &str) -> Result<RakuAstNode, RuntimeError> {
    let (statements, _) = crate::parser::parse_program(source)?;
    let mut expressions = statements
        .iter()
        .filter(|statement| !matches!(statement, crate::ast::Stmt::SetLine(..)));
    match (expressions.next(), expressions.next()) {
        (Some(crate::ast::Stmt::Expr(expr)), None) => convert_expr(names, expr),
        _ => Err(unsupported(&format!("type argument `{source}`"))),
    }
}

/// Convert a type application using its parsed argument expressions. Only a
/// type-only constraint without parsed arguments needs the spelling scanner.
pub(super) fn parameterized_type_node(names: &NameScope, 
    spelling: &str,
    parsed: Option<&[Expr]>,
) -> Result<RakuAstNode, RuntimeError> {
    parameterized_type_node_at(names, spelling, parsed, 0)
}

pub(super) fn parameterized_type_node_at(names: &NameScope, 
    spelling: &str,
    parsed: Option<&[Expr]>,
    depth: usize,
) -> Result<RakuAstNode, RuntimeError> {
    if depth >= MAX_TYPE_DEPTH {
        return Err(unsupported("type composition exceeds 128 levels"));
    }
    let (open, delimiter) =
        application_open(spelling)?.ok_or_else(|| unsupported("malformed parameterised type"))?;
    if delimiter != '[' {
        return Err(unsupported("malformed parameterised type"));
    }
    let inner = spelling
        .strip_suffix(']')
        .ok_or_else(|| unsupported("malformed parameterised type"))?;
    let base = &spelling[..open];
    let base = build_type_node_at(names, base, depth + 1)?;
    let sources = type_sources(&inner[open + 1..], ',');
    // Role-to-role forwarding keeps its ::T spelling even though the parser
    // represents the executable argument as the bare name T.
    let captures: Vec<_> = type_sources(&inner[open + 1..], ',')
        .enumerate()
        .filter_map(|(index, source)| {
            crate::qualified::type_capture_name(source).map(|name| (index, name))
        })
        .collect();
    let mut fields = Vec::new();
    if let Some(parsed) = parsed {
        for (index, expr) in parsed.iter().enumerate() {
            // BinaryForm already retains every colonpair spelling, including
            // booleans, variable pairs and bracketed values. Use the same
            // conversion as a pair outside a type application.
            let capture = captures.iter().find(|(position, _)| *position == index);
            let node = if let Some((_, source)) = capture {
                if !matches!(expr, Expr::BareWord(name) if *source == name.as_str()) {
                    return Err(unsupported("ambiguous type capture argument spelling"));
                }
                RakuAstNode {
                    origin: Default::default(),
                    class: RakuAstClass::TypeSimple,
                    fields: vec![node_field(
                        None,
                        super::name_parts::name_from_parts(vec![
                            super::name_parts::leading_empty(),
                            super::name_parts::simple_part(source),
                        ]),
                    )],
                }
            } else if let Expr::BareWord(name) = expr
                && is_type_spelling(name)
            {
                build_type_node_at(names, name, depth + 1)?
            } else {
                convert_expr(names, expr)?
            };
            fields.push(node_field(None, node));
        }
    } else {
        for source in sources {
            let node = if let Some(value) = crate::parser::numeric_literal_value(source) {
                super::convert::convert_literal(names, &value)?
            } else if let Some(value) = super::type_build::string_literal_value(source) {
                super::convert::quoted_string(crate::value::Value::str(value))
            } else if source.starts_with(':') {
                colonpair_argument(names, source)?
            } else {
                match build_type_node_at(names, source, depth + 1) {
                    Ok(node) => node,
                    // Not a type spelling: a colonpair (`:coerce(&f)`) or other
                    // term argument of the application.
                    Err(err) => term_argument(names, source).ok_or(err)?,
                }
            };
            fields.push(node_field(None, node));
        }
    }
    // Rakudo omits `args` for an empty application (`R[]`).
    let mut node_fields = vec![node_field(Some("base-type"), base)];
    if !fields.is_empty() {
        node_fields.push(node_field(
            Some("args"),
            RakuAstNode {
                origin: Default::default(),
                class: RakuAstClass::ArgList,
                fields,
            },
        ));
    }
    Ok(RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::TypeParameterized,
        fields: node_fields,
    })
}

/// A non-type argument of a type application, parsed from its spelling as a
/// single expression statement. `None` when it is not exactly one expression.
// Cost: O(n), n = spelling length.
fn term_argument(names: &NameScope, source: &str) -> Option<RakuAstNode> {
    let (stmts, _) = crate::parser::parse_program_spelled(source).ok()?;
    let mut exprs = stmts
        .iter()
        .filter(|stmt| !matches!(stmt, crate::ast::Stmt::SetLine(..)));
    match (exprs.next(), exprs.next()) {
        (Some(crate::ast::Stmt::Expr(expr)), None) => convert_expr(names, expr).ok(),
        _ => None,
    }
}
