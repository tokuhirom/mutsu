use super::super::super::expr::expression;
use super::super::super::helpers::ws;
use super::super::super::parse_result::{PError, PResult, take_while1};
use super::super::ident;
use crate::ast::HandleSpec;

/// Parse a single handle spec item (colon-pair, word list, regex, wildcard, etc.)
fn parse_single_handle_spec<'a>(input: &'a str, specs: &mut Vec<HandleSpec>) -> PResult<'a, ()> {
    let r = input;
    if r.starts_with('<') {
        return parse_handle_word_list(r, specs);
    }
    // Capture slips in a parenthesized list, e.g. `|SomeType.methods`.
    // The slip itself is only syntax here; declaration composition expands the
    // resulting value into individual method names.
    if let Some(after_slip) = r.strip_prefix('|') {
        let (rest, expr) = expression(after_slip)?;
        specs.push(HandleSpec::Expr(Box::new(expr)));
        return Ok((rest, ()));
    }
    // Quoted method name inside a parenthesized list: `handles('title', "author")`.
    if r.starts_with('\'') || r.starts_with('"') {
        let quote = r.as_bytes()[0] as char;
        let after_open = &r[1..];
        let end = after_open
            .find(quote)
            .ok_or_else(|| PError::expected("closing quote in handles"))?;
        let exposed = after_open[..end].to_string();
        let after_name = &after_open[end + 1..];
        let (after_name, _) = ws(after_name)?;
        if let Some(after_arrow) = after_name.strip_prefix("=>") {
            let (after_arrow, _) = ws(after_arrow)?;
            let (rest, target) = if after_arrow.starts_with('\'') || after_arrow.starts_with('"') {
                let quote = after_arrow.as_bytes()[0] as char;
                let after_quote = &after_arrow[1..];
                let end = after_quote
                    .find(quote)
                    .ok_or_else(|| PError::expected("closing quote in handles pair"))?;
                (&after_quote[end + 1..], after_quote[..end].to_string())
            } else {
                let (rest, target) = take_while1(after_arrow, |c: char| {
                    c.is_alphanumeric() || c == '_' || c == '-'
                })?;
                (rest, target.to_string())
            };
            specs.push(HandleSpec::Rename { exposed, target });
            return Ok((rest, ()));
        }
        specs.push(HandleSpec::Name(exposed));
        return Ok((after_name, ()));
    }
    // Colon-pair: :exposed<target> or :exposed('target')
    if let Some(after_colon) = r.strip_prefix(':') {
        let (after_name, name) = take_while1(after_colon, |c: char| {
            c.is_alphanumeric() || c == '_' || c == '-'
        })?;
        if let Some(after_angle) = after_name.strip_prefix('<') {
            // :name<target>
            let end = after_angle
                .find('>')
                .ok_or_else(|| PError::expected("closing > in handles pair"))?;
            let target = after_angle[..end].to_string();
            specs.push(HandleSpec::Rename {
                exposed: name.to_string(),
                target,
            });
            return Ok((&after_angle[end + 1..], ()));
        } else if let Some(after_paren) = after_name.strip_prefix('(') {
            // :name('target')
            let after_paren = after_paren.trim_start();
            if after_paren.starts_with('\'') || after_paren.starts_with('"') {
                let quote = after_paren.as_bytes()[0] as char;
                let after_quote = &after_paren[1..];
                let end = after_quote
                    .find(quote)
                    .ok_or_else(|| PError::expected("closing quote in handles pair"))?;
                let target = after_quote[..end].to_string();
                let after_close_quote = &after_quote[end + 1..];
                let after_close_quote = after_close_quote.trim_start();
                let after_close_paren = after_close_quote
                    .strip_prefix(')')
                    .ok_or_else(|| PError::expected("closing ) in handles pair"))?;
                specs.push(HandleSpec::Rename {
                    exposed: name.to_string(),
                    target,
                });
                return Ok((after_close_paren, ()));
            }
        }
        return Err(PError::expected("handles pair value after colon-pair name"));
    }
    // Fat-arrow pair: `exposed => 'target'` / `exposed => target`
    // (`handles (dog_name => 'name')` — rename delegation, Rakudo syntax).
    let (after_name, name) = take_while1(r, |c: char| c.is_alphanumeric() || c == '_' || c == '-')?;
    let (after_name, _) = ws(after_name)?;
    if let Some(after_arrow) = after_name.strip_prefix("=>") {
        let (after_arrow, _) = ws(after_arrow)?;
        let (rest, target) = if after_arrow.starts_with('\'') || after_arrow.starts_with('"') {
            let quote = after_arrow.as_bytes()[0] as char;
            let after_quote = &after_arrow[1..];
            let end = after_quote
                .find(quote)
                .ok_or_else(|| PError::expected("closing quote in handles pair"))?;
            (&after_quote[end + 1..], after_quote[..end].to_string())
        } else {
            let (r_target, target) = take_while1(after_arrow, |c: char| {
                c.is_alphanumeric() || c == '_' || c == '-'
            })?;
            (r_target, target.to_string())
        };
        specs.push(HandleSpec::Rename {
            exposed: name.to_string(),
            target,
        });
        return Ok((rest, ()));
    }
    // Bare identifier with no fat-arrow inside a parenthesized list is a plain
    // method name: `handles(title, author)`.
    specs.push(HandleSpec::Name(name.to_string()));
    Ok((after_name, ()))
}

/// Parse handle specifications after the `handles` keyword.
pub(in crate::parser) fn parse_handle_specs<'a>(
    input: &'a str,
    specs: &mut Vec<HandleSpec>,
    rest_out: &mut &'a str,
) -> Result<(), PError> {
    let r = input;
    if let Some(after_slip) = r.strip_prefix('|') {
        let (rest, expr) = expression(after_slip)?;
        specs.push(HandleSpec::Expr(Box::new(expr)));
        *rest_out = rest;
    } else if r.starts_with(['$', '@', '%']) {
        // A variable holding the names: `handles @coercers` (Config::BINDish
        // fills `our @coercers` in a BEGIN block).
        let (rest, expr) = expression(r)?;
        specs.push(HandleSpec::Expr(Box::new(expr)));
        *rest_out = rest;
    } else if r.starts_with('<') {
        let (rest, ()) = parse_handle_word_list(r, specs)?;
        *rest_out = rest;
    } else if let Some(after_star) = r.strip_prefix('*') {
        // Wildcard: *
        specs.push(HandleSpec::Wildcard);
        *rest_out = after_star;
    } else if let Some(after_slash) = r.strip_prefix('/') {
        // Regex: /pattern/
        // Scan with the regex-literal scanner so `\/`, quoted strings and
        // character classes containing `/` do not end the pattern early.
        let (pattern, rest) =
            crate::parser::primary::regex::scan_to_delim(after_slash, '/', '/', false)
                .ok_or_else(|| PError::expected("closing / in handles regex"))?;
        specs.push(HandleSpec::Regex(pattern.to_string()));
        *rest_out = rest;
    } else if let Some(after_paren) = r.strip_prefix('(') {
        // Parenthesized list: (:name<target>, :name2('target2'))
        let mut cursor = after_paren;
        loop {
            let (r_ws, _) = ws(cursor)?;
            cursor = r_ws;
            if let Some(r_end) = cursor.strip_prefix(')') {
                *rest_out = r_end;
                break;
            }
            // Skip commas
            if let Some(after_comma) = cursor.strip_prefix(',') {
                let (r_ws, _) = ws(after_comma)?;
                cursor = r_ws;
                continue;
            }
            let (after_spec, _) = parse_single_handle_spec(cursor, specs)?;
            cursor = after_spec;
        }
    } else if r.starts_with(':') {
        // Single colon-pair: :exposed<target>
        let (after_spec, _) = parse_single_handle_spec(r, specs)?;
        *rest_out = after_spec;
    } else if r.starts_with('\'') || r.starts_with('"') {
        // Quoted string: 'method' or "method"
        let quote = r.as_bytes()[0] as char;
        let after_open = &r[1..];
        let end = after_open
            .find(quote)
            .ok_or_else(|| PError::expected("closing quote in handles"))?;
        let method_name = &after_open[..end];
        specs.push(HandleSpec::Name(method_name.to_string()));
        *rest_out = &after_open[end + 1..];
    } else {
        // Bare identifier: could be a method name or type name
        let (r_name, method_name) = ident(r)?;
        specs.push(HandleSpec::Name(method_name));
        *rest_out = r_name;
    }
    Ok(())
}

// Cost: O(n), n = word-list source bytes.
fn parse_handle_word_list<'a>(input: &'a str, specs: &mut Vec<HandleSpec>) -> PResult<'a, ()> {
    let (rest, term) = crate::parser::primary::angle_list(input)?;
    let names = handle_specs_from_term(&term)
        .ok_or_else(|| PError::expected("method names in handles word list"))?;
    specs.extend(names);
    Ok((rest, ()))
}

/// The written term of one `handles` clause that spans `input` up to `rest`,
/// when reading it as a term rebuilds exactly the clause's `specs`; `None`
/// for a spelling the term does not capture.
// Cost: O(n), n = length of the clause.
pub(in crate::parser) fn handles_clause_term(
    input: &str,
    rest: &str,
    specs: &[HandleSpec],
) -> Option<crate::ast::Expr> {
    let (after, term) = crate::parser::primary::primary(input).ok()?;
    if after.len() != rest.len() {
        return None;
    }
    let rebuilt = match &term {
        crate::ast::Expr::RegexLiteral { .. } => {
            let after_open = input.strip_prefix('/')?;
            let (pattern, _) =
                crate::parser::primary::regex::scan_to_delim(after_open, '/', '/', false)?;
            vec![HandleSpec::Regex(pattern.to_string())]
        }
        _ => handle_specs_from_term(&term)?,
    };
    let same = rebuilt.len() == specs.len()
        && rebuilt.iter().zip(specs).all(|(a, b)| match (a, b) {
            (HandleSpec::Name(x), HandleSpec::Name(y)) => x == y,
            (
                HandleSpec::Rename {
                    exposed: a,
                    target: b,
                },
                HandleSpec::Rename {
                    exposed: c,
                    target: d,
                },
            ) => a == c && b == d,
            (HandleSpec::Regex(a), HandleSpec::Regex(b)) => a == b,
            (HandleSpec::Wildcard, HandleSpec::Wildcard) => true,
            (HandleSpec::Expr(a), HandleSpec::Expr(b)) => format!("{a:?}") == format!("{b:?}"),
            _ => false,
        });
    same.then_some(term)
}

/// The specs a `handles` term names, flattening nested positional lists.
/// Spelled quote words and parentheses retain their original AST alongside
/// these static names, so the parser and RakuAST lowering share one rule.
// Cost: O(n), n = size of the term.
pub(crate) fn handle_specs_from_term(term: &crate::ast::Expr) -> Option<Vec<HandleSpec>> {
    use crate::ast_visit::Visit;
    let mut collector = HandleTerms {
        specs: Vec::new(),
        depth: 0,
        valid: true,
    };
    collector.visit_expr(term);
    collector.valid.then_some(collector.specs)
}

struct HandleTerms {
    specs: Vec<HandleSpec>,
    depth: usize,
    valid: bool,
}

impl<'ast> crate::ast_visit::Visit<'ast> for HandleTerms {
    fn visit_expr(&mut self, term: &'ast crate::ast::Expr) {
        use crate::ast::Expr;
        // RakuAST can construct a deeper list than the source parser allows.
        // Refuse it with the caller's catchable parse/lowering error rather
        // than overflowing the Rust stack.
        if !self.valid || self.depth >= 128 {
            self.valid = false;
            return;
        }
        self.depth += 1;
        let term = match term.peel_parens() {
            Expr::PositionalPair(inner) => inner.peel_parens(),
            other => other,
        };
        match term {
            Expr::ArrayLiteral(_) => crate::ast_visit::walk_expr(self, term),
            Expr::Whatever => self.specs.push(HandleSpec::Wildcard),
            Expr::BareWord(name) => self.specs.push(HandleSpec::Name(name.clone())),
            Expr::Literal(value) => {
                if let Some(name) = handle_method_name(value) {
                    self.specs.push(HandleSpec::Name(name));
                } else {
                    self.valid = false;
                }
            }
            Expr::Binary {
                left,
                op: crate::token_kind::TokenKind::FatArrow,
                right,
                ..
            } => {
                let exposed = match left.as_ref() {
                    Expr::BareWord(name) => Some(name.clone()),
                    Expr::Literal(value) => handle_method_name(value),
                    _ => None,
                };
                let target = match right.as_ref() {
                    Expr::Literal(value) => handle_method_name(value),
                    _ => None,
                };
                if let (Some(exposed), Some(target)) = (exposed, target) {
                    self.specs.push(HandleSpec::Rename { exposed, target });
                } else {
                    self.valid = false;
                }
            }
            Expr::Unary {
                op: crate::token_kind::TokenKind::Pipe,
                expr,
                ..
            } => self
                .specs
                .push(HandleSpec::Expr(Box::new((**expr).clone()))),
            // These are existing dynamic handles forms. Quote-word lists
            // above become static names and never enter runtime evaluation.
            Expr::Var(_)
            | Expr::ArrayVar(_)
            | Expr::HashVar(_)
            | Expr::MethodCall { .. }
            | Expr::Call { .. } => self.specs.push(HandleSpec::Expr(Box::new(term.clone()))),
            Expr::RegexLiteral { value, .. } => match value.view() {
                crate::value::ValueView::Regex(pattern) => {
                    self.specs.push(HandleSpec::Regex(pattern.to_string()));
                }
                _ => self.valid = false,
            },
            _ => self.valid = false,
        }
        self.depth -= 1;
    }
}

fn handle_method_name(value: &crate::value::Value) -> Option<String> {
    if let Some(name) = value.as_str() {
        return Some(name.to_string());
    }
    // Numeric quote words are Str allomorphs: keep their written name rather
    // than the numeric component (for example, `<1e2>` names `1e2`).
    if let crate::value::ValueView::Mixin(inner, mixins) = value.view()
        && crate::value::types::allomorph_type_name(inner, mixins).is_some()
    {
        return mixins.get("Str")?.as_str().map(str::to_string);
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::Expr;
    use crate::value::Value;

    #[test]
    fn nested_handle_lists_produce_static_names() {
        let term = Expr::ArrayLiteral(vec![
            Expr::Literal(Value::str("keys".to_string())),
            Expr::ArrayLiteral(vec![Expr::Literal(Value::str("Bool".to_string()))]),
        ]);
        let specs = handle_specs_from_term(&term).expect("static nested list");
        assert!(
            matches!(&specs[..], [HandleSpec::Name(a), HandleSpec::Name(b)] if a == "keys" && b == "Bool")
        );
    }

    #[test]
    fn excessively_nested_handle_lists_are_refused() {
        let mut term = Expr::Literal(Value::str("keys".to_string()));
        for _ in 0..129 {
            term = Expr::ArrayLiteral(vec![term]);
        }
        assert!(handle_specs_from_term(&term).is_none());
    }
}
