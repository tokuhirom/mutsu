/// Array (@), hash (%), and code (&) sigil variable parsing.
///
/// Covers `@array`, `%hash`, `&code`, sigil-context coercions, symbolic
/// derefs, and operator-code-ref syntax (`&infix:<+>`, `&[op]`).
use std::sync::atomic::{AtomicU64, Ordering};

use crate::ast::Expr;
use crate::parser::helpers::{is_raku_identifier_start, normalize_raku_identifier, ws};
use crate::parser::parse_result::{PError, PResult, parse_char};
use crate::value::Value;

use super::adverb::parse_var_name_adverb_suffixes;
use super::ident::{
    is_pseudo_package, parse_ident_with_hyphens, parse_qualified_ident_with_hyphens,
};
use super::perl5::detect_perl5_sigil_var;
use super::scalar::scalar_var;

static ANON_ARRAY_COUNTER: AtomicU64 = AtomicU64::new(0);

/// Find the `]` that closes the outer `[` of an `&[op]` operator reference,
/// balancing nested brackets. `input` starts just past the outer `[`. This lets
/// a metaop whose operand is itself bracketed parse correctly: `&[R[~~]]` has
/// the operator `R[~~]` (reverse of the reduce `[~~]`), not `R[~~` cut at the
/// first `]`. Returns the byte offset of the matching `]`, or `None` if
/// unbalanced.
fn matching_op_bracket_end(input: &str) -> Option<usize> {
    let mut depth = 0usize;
    for (idx, ch) in input.char_indices() {
        match ch {
            '[' => depth += 1,
            ']' => {
                if depth == 0 {
                    return Some(idx);
                }
                depth -= 1;
            }
            _ => {}
        }
    }
    None
}

/// Parse a leading-`::` qualified variable name after a sigil, i.e. the
/// `::Pkg::name` / `::Pkg::('name')` form (`input` starts at the first `::`).
///
/// Returns `(rest, static_name, dynamic)`:
/// - `static_name` is the fully-qualified name with `::` separators (e.g.
///   `Pkg::name`), used when every segment is a static identifier.
/// - `dynamic` is `Some(expr)` when any segment is a runtime `::('expr')` lookup;
///   the expression evaluates to the qualified name string (`"Pkg::" ~ name`),
///   for a `SymbolicDeref`.
///
/// Mirrors the qualified-name deduction in `ident/term_literals.rs` so
/// `@::Pkg::('name')` resolves the same variable as `@Pkg::name`.
fn parse_leading_colon_qualified(input: &str) -> Option<(&str, String, Option<Expr>)> {
    let after = input.strip_prefix("::")?;
    // The first segment must be a static identifier (`::Pkg…`); `::(...)` alone
    // is the already-handled `@::(expr)` symbolic form.
    let (mut r, first) = parse_ident_with_hyphens(after).ok()?;
    let mut full_name = first.to_string();
    let mut dynamic: Option<Expr> = None;
    while let Some(r2) = r.strip_prefix("::") {
        if let Some(after_paren) = r2.strip_prefix('(') {
            let (r3, _) = ws(after_paren).ok()?;
            let (r3, seg) = crate::parser::expr::expression(r3).ok()?;
            let (r3, _) = ws(r3).ok()?;
            let (r3, _) = parse_char(r3, ')').ok()?;
            let base = match dynamic.take() {
                Some(prev) => Expr::Binary {
                    left: Box::new(prev),
                    op: crate::token_kind::TokenKind::Tilde,
                    right: Box::new(Expr::Literal(Value::str("::".to_string()))),
                },
                None => Expr::Literal(Value::str(format!("{}::", full_name))),
            };
            dynamic = Some(Expr::Binary {
                left: Box::new(base),
                op: crate::token_kind::TokenKind::Tilde,
                right: Box::new(seg),
            });
            r = r3;
        } else if let Ok((r2b, part)) = parse_ident_with_hyphens(r2) {
            if let Some(dyn_expr) = dynamic.as_mut() {
                let old = std::mem::replace(dyn_expr, Expr::Literal(Value::NIL));
                *dyn_expr = Expr::Binary {
                    left: Box::new(old),
                    op: crate::token_kind::TokenKind::Tilde,
                    right: Box::new(Expr::Literal(Value::str(format!("::{}", part)))),
                };
            } else {
                full_name = format!("{}::{}", full_name, part);
            }
            r = r2b;
        } else {
            break;
        }
    }
    Some((r, full_name, dynamic))
}

/// Parse an @array variable reference.
pub(crate) fn array_var(input: &str) -> PResult<'_, Expr> {
    let (input, _) = parse_char(input, '@')?;
    // Detect Perl 5 special array variables (@-, @+) and throw X::Syntax::Perl5Var.
    if let Some(err) = detect_perl5_sigil_var('@', input) {
        return Err(PError::fatal(err));
    }
    // Symbolic variable dereference: @::("name")
    if let Some(after_colons) = input.strip_prefix("::(") {
        let (after_expr, expr) = crate::parser::expr::expression(after_colons)?;
        let (after_expr, _) = ws(after_expr)?;
        let (rest, _) = parse_char(after_expr, ')')?;
        return Ok((
            rest,
            Expr::SymbolicDeref {
                sigil: "@".to_string(),
                expr: Box::new(expr),
            },
        ));
    }
    // Leading-`::` qualified array: `@::Pkg::bar` (static) / `@::Pkg::('bar')`
    // (symbolic). The leading `::` is optional in Raku, so `@::Pkg::bar` names
    // the same `@Pkg::bar`; a `::('expr')` segment makes it a runtime lookup.
    if input.starts_with("::")
        && input
            .as_bytes()
            .get(2)
            .is_some_and(|&b| b.is_ascii_alphabetic() || b == b'_')
        && let Some((rest, name, dynamic)) = parse_leading_colon_qualified(input)
    {
        return Ok((
            rest,
            match dynamic {
                Some(expr) => Expr::SymbolicDeref {
                    sigil: "@".to_string(),
                    expr: Box::new(expr),
                },
                None => Expr::ArrayVar(name),
            },
        ));
    }
    // Handle `.` twigil: @.attr → self.attr (in list context)
    if input.starts_with('.')
        && input.len() > 1
        && (input.as_bytes()[1].is_ascii_alphabetic() || input.as_bytes()[1] == b'_')
    {
        let after_dot = &input[1..];
        let (rest, name) = parse_qualified_ident_with_hyphens(after_dot)?;
        return Ok((rest, Expr::ArrayVar(format!(".{}", name))));
    }
    // Handle twigils
    let (rest, twigil) = if input.starts_with('*')
        || input.starts_with('?')
        || input.starts_with('!')
        || input.starts_with('^')
        || (input.starts_with('~') && input.len() > 1 && input.as_bytes()[1].is_ascii_alphabetic())
    {
        (&input[1..], &input[..1])
    } else {
        (input, "")
    };
    // Contextualized scalar specials (e.g., @$/, @$_): parse `$...` then lift
    // to an array variable targeting the same underlying name.
    if twigil.is_empty() && rest.starts_with('$') {
        // Handle @$<name> — list coercion of a named capture variable
        if let Ok((r2, expr @ Expr::CaptureVar(_))) = scalar_var(rest) {
            return Ok((
                r2,
                Expr::MethodCall {
                    target: Box::new(expr),
                    name: crate::symbol::Symbol::intern("list"),
                    args: vec![],
                    modifier: None,
                    quoted: false,
                },
            ));
        }
        if let Ok((r2, Expr::Var(name))) = scalar_var(rest) {
            return Ok((r2, Expr::ArrayVar(name)));
        }
    }
    // @{expr} is Perl 5 array dereference syntax — throw X::Obsolete
    if twigil.is_empty()
        && rest.starts_with('{')
        && let Ok((_r2, inner)) = crate::parser::primary::misc::block_or_hash_expr(rest)
        && !matches!(inner, crate::ast::Expr::Hash(_))
    {
        return Err(PError::fatal(
            "X::Obsolete: Unsupported use of @{expr}. In Raku please use: @(expr).".to_string(),
        ));
    }
    // Handle @<name> — list coercion of a named capture variable from $/
    // (e.g., after a regex match, @<fie> gives the positional elements of $<fie>)
    if twigil.is_empty()
        && rest.starts_with('<')
        && let Some(after_lt) = rest.strip_prefix('<')
        && let Some(end) = after_lt.find('>')
    {
        let name = &after_lt[..end];
        if !name.is_empty() {
            let after_gt = &after_lt[end + 1..];
            return Ok((
                after_gt,
                Expr::MethodCall {
                    target: Box::new(Expr::CaptureVar(name.to_string())),
                    name: crate::symbol::Symbol::intern("list"),
                    args: vec![],
                    modifier: None,
                    quoted: false,
                },
            ));
        }
    }
    // Bare @ (anonymous array variable) — each occurrence gets a unique name
    let next_is_ident =
        !rest.is_empty() && rest.chars().next().is_some_and(is_raku_identifier_start);
    if !next_is_ident && twigil.is_empty() {
        let id = ANON_ARRAY_COUNTER.fetch_add(1, Ordering::Relaxed);
        return Ok((rest, Expr::ArrayVar(format!("__ANON_ARRAY_{id}__"))));
    }
    let (rest, name) = parse_qualified_ident_with_hyphens(rest)?;
    let (rest, name) = parse_var_name_adverb_suffixes(rest, name);
    let full_name = if twigil.is_empty() {
        name
    } else {
        format!("{}{}", twigil, name)
    };
    Ok((rest, Expr::ArrayVar(full_name)))
}

/// Parse a %hash variable reference.
pub(crate) fn hash_var(input: &str) -> PResult<'_, Expr> {
    let (input, _) = parse_char(input, '%')?;
    // Detect Perl 5 special hash variables (%-, %+, %!) and throw X::Syntax::Perl5Var.
    if let Some(err) = detect_perl5_sigil_var('%', input) {
        return Err(PError::fatal(err));
    }
    // Symbolic variable dereference: %::("name")
    if let Some(after_colons) = input.strip_prefix("::(") {
        let (after_expr, expr) = crate::parser::expr::expression(after_colons)?;
        let (after_expr, _) = ws(after_expr)?;
        let (rest, _) = parse_char(after_expr, ')')?;
        return Ok((
            rest,
            Expr::SymbolicDeref {
                sigil: "%".to_string(),
                expr: Box::new(expr),
            },
        ));
    }
    // Leading-`::` qualified hash: `%::Pkg::h` (static) / `%::Pkg::('h')`.
    if input.starts_with("::")
        && input
            .as_bytes()
            .get(2)
            .is_some_and(|&b| b.is_ascii_alphabetic() || b == b'_')
        && let Some((rest, name, dynamic)) = parse_leading_colon_qualified(input)
    {
        return Ok((
            rest,
            match dynamic {
                Some(expr) => Expr::SymbolicDeref {
                    sigil: "%".to_string(),
                    expr: Box::new(expr),
                },
                None => Expr::HashVar(name),
            },
        ));
    }
    // Handle `.` twigil: %.attr → self.attr (in hash context)
    if input.starts_with('.')
        && input.len() > 1
        && (input.as_bytes()[1].is_ascii_alphabetic() || input.as_bytes()[1] == b'_')
    {
        let after_dot = &input[1..];
        let (rest, name) = parse_qualified_ident_with_hyphens(after_dot)?;
        return Ok((rest, Expr::HashVar(format!(".{}", name))));
    }
    // Handle twigils
    let (rest, twigil) = if input.starts_with('*')
        || input.starts_with('?')
        || input.starts_with('!')
        || input.starts_with('^')
        || (input.starts_with('~') && input.len() > 1 && input.as_bytes()[1].is_ascii_alphabetic())
    {
        (&input[1..], &input[..1])
    } else {
        (input, "")
    };
    // Contextualized scalar specials (e.g., %$h, %$/, %$_): parse `$...` then
    // coerce to hash context via a `.hash` method call.
    if twigil.is_empty()
        && rest.starts_with('$')
        && let Ok((r2, expr)) = scalar_var(rest)
    {
        return Ok((
            r2,
            Expr::MethodCall {
                target: Box::new(expr),
                name: crate::symbol::Symbol::intern("hash"),
                args: vec![],
                modifier: None,
                quoted: false,
            },
        ));
    }
    // %{...} — hash coercion of a block expression
    // In Raku, %{expr} treats the block as a hash initializer.
    if twigil.is_empty()
        && rest.starts_with('{')
        && let Ok((r2, inner)) = crate::parser::primary::misc::block_or_hash_expr(rest)
    {
        // Wrap in hash() call — this will fail at runtime if odd number of elements
        return Ok((
            r2,
            Expr::Call {
                name: crate::symbol::Symbol::intern("hash"),
                args: vec![inner],
            },
        ));
    }
    // Handle %<name> — hash coercion of a named capture variable from $/
    // (e.g., after a regex match, %<foe> gives the hash view of $<foe>)
    if twigil.is_empty()
        && rest.starts_with('<')
        && let Some(after_lt) = rest.strip_prefix('<')
        && let Some(end) = after_lt.find('>')
    {
        let name = &after_lt[..end];
        if !name.is_empty() {
            let after_gt = &after_lt[end + 1..];
            return Ok((
                after_gt,
                Expr::MethodCall {
                    target: Box::new(Expr::CaptureVar(name.to_string())),
                    name: crate::symbol::Symbol::intern("hash"),
                    args: vec![],
                    modifier: None,
                    quoted: false,
                },
            ));
        }
    }
    // Bare % (anonymous hash variable)
    let next_is_ident =
        !rest.is_empty() && rest.chars().next().is_some_and(is_raku_identifier_start);
    if !next_is_ident && twigil.is_empty() {
        if let Some(after_colons) = rest.strip_prefix("::") {
            // `%::name` is the root-namespace-qualified form of `%name`.
            // `%::` followed by anything else (`%::{''}`, `%::<x>`) is rakudo's
            // undeclared bare variable `%` — compile it to a typed
            // X::Undeclared die (integration/error-reporting.t test 25).
            if after_colons
                .chars()
                .next()
                .is_some_and(is_raku_identifier_start)
            {
                let (rest2, name) = parse_qualified_ident_with_hyphens(after_colons)?;
                let (rest2, name) = parse_var_name_adverb_suffixes(rest2, name);
                return Ok((rest2, Expr::HashVar(name)));
            }
            return Ok((
                rest,
                Expr::Call {
                    name: crate::symbol::Symbol::intern("__mutsu_undeclared_var_die"),
                    args: vec![Expr::Literal(Value::str_from("%"))],
                },
            ));
        }
        return Ok((rest, Expr::HashVar("__ANON_HASH__".to_string())));
    }
    // Special: %*ENV
    let (rest, name) = parse_qualified_ident_with_hyphens(rest)?;
    let (rest, name) = parse_var_name_adverb_suffixes(rest, name);
    let full_name = if twigil.is_empty() {
        name
    } else {
        format!("{}{}", twigil, name)
    };
    Ok((rest, Expr::HashVar(full_name)))
}

/// Parse a &code variable reference.
/// Handles `&foo`, twigil forms `&?BLOCK`, operator references like `&infix:<+>`,
/// package-qualified code refs like `&SETTING::OUTER::name`, and indirect package
/// lookups like `&::($expr)::name` or `&CALLER::($expr)::name`.
pub(crate) fn code_var(input: &str) -> PResult<'_, Expr> {
    let (input, _) = parse_char(input, '&')?;
    // `.` twigil: `&.name` → the public accessor of a `has &.name` callable
    // attribute, i.e. `self.name`, mirroring `$.attr`. Desugaring to the method
    // call (rather than a `.`-named CodeVar) reuses the accessor dispatch, so a
    // following `.( ... )` invokes the returned callable (`&.function.($x)`).
    if input.starts_with('.')
        && input.len() > 1
        && (input.as_bytes()[1].is_ascii_alphabetic() || input.as_bytes()[1] == b'_')
    {
        let after_dot = &input[1..];
        let (rest, name) = parse_qualified_ident_with_hyphens(after_dot)?;
        return Ok((
            rest,
            Expr::MethodCall {
                target: Box::new(Expr::BareWord("self".to_string())),
                name: crate::symbol::Symbol::intern(&name),
                args: Vec::new(),
                modifier: None,
                quoted: false,
            },
        ));
    }
    // Dereference callable stored in a variable/expression:
    // &$x, &@x, &%x, &($expr)
    if input.starts_with('$') {
        return scalar_var(input);
    }
    if input.starts_with('@') {
        return array_var(input);
    }
    if input.starts_with('%') {
        return hash_var(input);
    }
    if let Some(after_paren) = input.strip_prefix('(') {
        let (rest, expr) = crate::parser::expr::expression(after_paren)?;
        let (rest, _) = ws(rest)?;
        let (rest, _) = parse_char(rest, ')')?;
        return Ok((rest, expr));
    }
    // Callable block literal dereference: &{ ... }
    if input.starts_with('{') {
        let (rest, body) = crate::parser::primary::misc::parse_block_body(input)?;
        return Ok((
            rest,
            Expr::AnonSub {
                body,
                is_rw: false,
                is_block: true,
            },
        ));
    }
    // Handle &[op] — short form for &infix:<op>
    if let Some(after_bracket) = input.strip_prefix('[')
        && let Some(end_pos) = matching_op_bracket_end(after_bracket)
    {
        let op_name = &after_bracket[..end_pos];
        let rest = &after_bracket[end_pos + 1..];
        // A bare lowercase *word* inside `&[...]` must name a real word infix
        // (`&[cmp]`, `&[min]`, ...) or a user-declared `infix:<word>`. An unknown
        // word (`&[doesntexist]`) is a compile error: "Missing infix inside []".
        // Restricted to all-lowercase-letter names so symbolic ops (`&[+]`) and
        // uppercase meta-ops (`&[Z]`, `&[Xcmp]`) are never affected.
        if !op_name.is_empty()
            && op_name.bytes().all(|b| b.is_ascii_lowercase())
            && !is_known_word_infix(op_name)
            && !crate::parser::stmt::simple::is_user_defined_infix(op_name)
        {
            let message = "Missing infix inside []".to_string();
            let exception = crate::value::Value::make_exception(
                "X::Syntax::Missing",
                &[
                    ("what", crate::value::Value::str("infix".to_string())),
                    ("message", crate::value::Value::str(message.clone())),
                ],
            );
            return Err(PError::fatal_with_exception(message, Box::new(exception)));
        }
        let full_name = format!("infix:<{}>", op_name);
        return Ok((rest, Expr::CodeVar(full_name)));
    }
    // Handle &CALLER::($expr)::name — CALLER prefix followed by indirect lookup
    if let Some(after_caller) = input.strip_prefix("CALLER::(") {
        let (after_expr, expr) = crate::parser::expr::expression(after_caller)?;
        let (after_expr, _) = parse_char(after_expr, ')')?;
        let (after_expr, _) = ws(after_expr)?;
        if let Some(after_sep) = after_expr.strip_prefix("::") {
            let (rest, name) = parse_ident_with_hyphens(after_sep)?;
            return Ok((
                rest,
                Expr::IndirectCodeLookup {
                    package: Box::new(expr),
                    name: normalize_raku_identifier(name),
                },
            ));
        }
        return Err(PError::expected(":: after indirect package lookup"));
    }
    // Handle &::($expr)::name — indirect package lookup
    // Handle &::($expr) — symbolic code dereference (looks up &name at runtime)
    if let Some(after_colons) = input.strip_prefix("::(") {
        let (after_expr, expr) = crate::parser::expr::expression(after_colons)?;
        let (after_expr, _) = parse_char(after_expr, ')')?;
        let (after_expr, _) = ws(after_expr)?;
        if let Some(after_sep) = after_expr.strip_prefix("::") {
            let (rest, name) = parse_ident_with_hyphens(after_sep)?;
            return Ok((
                rest,
                Expr::IndirectCodeLookup {
                    package: Box::new(expr),
                    name: normalize_raku_identifier(name),
                },
            ));
        }
        // No ::name suffix — this is &::("name"), a symbolic code variable deref
        return Ok((
            after_expr,
            Expr::SymbolicDeref {
                sigil: "&".to_string(),
                expr: Box::new(expr),
            },
        ));
    }
    // Handle package-qualified code refs: &SETTING::OUTER::...::name
    // or &CALLER::SETTING::OUTER::...::name
    if let Ok((_, first_ident)) = parse_ident_with_hyphens(input)
        && is_pseudo_package(first_ident)
    {
        let after_first = &input[first_ident.len()..];
        if after_first.starts_with("::") {
            // Consume the full Package::Package::...::name chain
            let mut pos = 0;
            let mut qualified = String::new();
            loop {
                if let Ok((_, ident)) = parse_ident_with_hyphens(&input[pos..]) {
                    let after_ident = &input[pos + ident.len()..];
                    if after_ident.starts_with("::") && is_pseudo_package(ident) {
                        // This is a pseudo-package, consume it and the ::
                        qualified.push_str(ident);
                        qualified.push_str("::");
                        pos += ident.len() + 2;
                        continue;
                    }
                    // This is the final name
                    qualified.push_str(&normalize_raku_identifier(ident));
                    pos += ident.len();
                    return Ok((&input[pos..], Expr::CodeVar(qualified)));
                }
                break;
            }
        }
    }
    // Handle package-qualified code refs: &ClassName::method, &Foo::Bar::method, etc.
    if let Ok((_, first_ident)) = parse_ident_with_hyphens(input) {
        let after_first = &input[first_ident.len()..];
        if after_first.starts_with("::") {
            let mut pos = 0;
            let mut parts: Vec<String> = Vec::new();
            // Consume all Ident:: segments
            loop {
                if let Ok((_, ident)) = parse_ident_with_hyphens(&input[pos..]) {
                    let after_ident = &input[pos + ident.len()..];
                    if after_ident.starts_with("::") {
                        parts.push(normalize_raku_identifier(ident));
                        pos += ident.len() + 2; // skip ident and ::
                        continue;
                    }
                    // Final identifier (no :: after it)
                    parts.push(normalize_raku_identifier(ident));
                    pos += ident.len();
                    if parts.len() >= 2 {
                        let qualified = parts.join("::");
                        return Ok((&input[pos..], Expr::CodeVar(qualified)));
                    }
                }
                break;
            }
        }
    }
    // Handle twigils on code vars: &?BLOCK, &!DISPATCHER, &^x, &*FOO
    let (rest, twigil) = if let Some(stripped) = input.strip_prefix('?') {
        (stripped, "?")
    } else if let Some(stripped) = input.strip_prefix('!') {
        (stripped, "!")
    } else if let Some(stripped) = input.strip_prefix('^') {
        (stripped, "^")
    } else if let Some(stripped) = input.strip_prefix('*') {
        (stripped, "*")
    } else {
        (input, "")
    };
    if twigil.is_empty()
        && let Ok((op_rest, op_name)) = crate::parser::stmt::parse_sub_name_pub(rest)
        && matches!(
            op_name.as_str(),
            n if n.starts_with("infix:<")
                || n.starts_with("prefix:<")
                || n.starts_with("postfix:<")
                || n.starts_with("term:<")
                || n.starts_with("circumfix:<")
                || n.starts_with("postcircumfix:<")
                || n.starts_with("trait_mod:<")
        )
        // `parse_sub_name_pub` only resolves a `:<<...>>`/`:«...»` operator
        // name through the compile-time-*constant* registry (`constant`
        // declarations), falling back to the raw, unresolved text when the
        // name isn't a registered constant (e.g. `BEGIN my $plus = '+'`).
        // A leftover interpolation sigil in the resolved name means that
        // fallback fired, so skip this static path and let the slower
        // `parse_operator_code_ref_suffix` path below build a genuine
        // runtime lookup instead.
        && !op_name.contains(['$', '@', '%'])
    {
        return Ok((op_rest, Expr::CodeVar(op_name)));
    }

    let (rest, name) = parse_ident_with_hyphens(rest)?;
    // Check for operator reference:
    // &infix:<OP>, &infix:<<OP>>, &infix:«OP», &[op], ...
    if twigil.is_empty()
        && matches!(
            name,
            "infix" | "prefix" | "postfix" | "term" | "circumfix" | "postcircumfix" | "trait_mod"
        )
        && let Some((r, suffix)) = parse_operator_code_ref_suffix(name, rest)
    {
        let code_var_expr = match suffix {
            OperatorCodeRefSuffix::Static(full_name) => Expr::CodeVar(full_name),
            // `:<<...>>` / `:«...»` / a bare `:[...]` interpolate/evaluate their
            // content at runtime (unlike the literal `:<...>` form), so the
            // operator name is only known at runtime. Build it as string
            // concatenation and resolve it the same way `&::("infix:<+>")`
            // does (SymbolicDeref), rather than a compile-time CodeVar
            // constant.
            OperatorCodeRefSuffix::Dynamic(name_expr) => Expr::SymbolicDeref {
                sigil: "&".to_string(),
                expr: Box::new(Expr::Binary {
                    left: Box::new(Expr::Binary {
                        left: Box::new(Expr::Literal(Value::str(format!("{name}:<")))),
                        op: crate::token_kind::TokenKind::Tilde,
                        right: Box::new(name_expr),
                    }),
                    op: crate::token_kind::TokenKind::Tilde,
                    right: Box::new(Expr::Literal(Value::str(">".to_string()))),
                }),
            },
        };
        return Ok((r, code_var_expr));
    }
    let full_name = if twigil.is_empty() {
        normalize_raku_identifier(name)
    } else {
        format!("{}{}", twigil, normalize_raku_identifier(name))
    };
    Ok((rest, Expr::CodeVar(full_name)))
}

/// The operator name suffix (`:<+>`, `:<<$plus>>`, `:«$plus»`, `:[$plus]`)
/// following an extended identifier category (`infix`, `prefix`, ...).
enum OperatorCodeRefSuffix {
    /// `:<...>` (and the quoted-string `:['...']`/`:["..."]` forms) — the
    /// operator name is known at compile time.
    Static(String),
    /// `:<<...>>` / `:«...»` (qq-style interpolation) or a bare `:[expr]`
    /// (a runtime expression) — the operator name is only known at
    /// runtime.
    Dynamic(Expr),
}

fn parse_operator_code_ref_suffix<'a>(
    category: &str,
    rest: &'a str,
) -> Option<(&'a str, OperatorCodeRefSuffix)> {
    if let Some(after_open) = rest.strip_prefix(":<<")
        && let Some(end_pos) = after_open.find(">>")
    {
        let symbol = after_open[..end_pos].trim();
        let after_close = &after_open[end_pos + 2..];
        let name_expr = crate::parser::primary::quote_adverbs::process_content_with_flags(
            symbol,
            &crate::parser::primary::quote_adverbs::QuoteFlags::qq_double(),
        );
        return Some((after_close, OperatorCodeRefSuffix::Dynamic(name_expr)));
    }

    if let Some(after_open) = rest.strip_prefix(":<") {
        // Nested <...> is valid in categorical operator names.
        let mut depth = 1u32;
        let mut chars = after_open.char_indices();
        while let Some((i, c)) = chars.next() {
            match c {
                '>' => {
                    depth -= 1;
                    if depth == 0 {
                        let symbol = &after_open[..i];
                        let after_close = &after_open[i + 1..];
                        return Some((
                            after_close,
                            OperatorCodeRefSuffix::Static(format!("{category}:<{symbol}>")),
                        ));
                    }
                }
                '<' => depth += 1,
                '\\' => {
                    chars.next();
                }
                _ => {}
            }
        }
    }

    if let Some(after_open) = rest.strip_prefix(":\u{ab}")
        && let Some(end_pos) = after_open.find('\u{bb}')
    {
        let symbol = after_open[..end_pos].trim();
        let after_close = &after_open[end_pos + '\u{bb}'.len_utf8()..];
        let name_expr = crate::parser::primary::quote_adverbs::process_content_with_flags(
            symbol,
            &crate::parser::primary::quote_adverbs::QuoteFlags::qq_double(),
        );
        return Some((after_close, OperatorCodeRefSuffix::Dynamic(name_expr)));
    }

    if let Some(after_open) = rest.strip_prefix(":['")
        && let Some(end_pos) = after_open.find("']")
    {
        let symbol = after_open[..end_pos]
            .replace("\\'", "'")
            .replace("\\\\", "\\");
        let after_close = &after_open[end_pos + 2..];
        return Some((
            after_close,
            OperatorCodeRefSuffix::Static(format!("{category}:<{symbol}>")),
        ));
    }

    if let Some(after_open) = rest.strip_prefix(":[\"")
        && let Some(end_pos) = after_open.find("\"]")
    {
        let symbol = &after_open[..end_pos];
        let after_close = &after_open[end_pos + 2..];
        return Some((
            after_close,
            OperatorCodeRefSuffix::Static(format!("{category}:<{symbol}>")),
        ));
    }

    // A bare expression inside `[...]` (no quotes), e.g. `&infix:[$plus]` —
    // the operator name is a runtime value, not a string literal.
    if let Some(after_open) = rest.strip_prefix(":[")
        && let Ok((after_expr, expr)) = crate::parser::expr::expression(after_open)
        && let Some(after_close) = after_expr.strip_prefix(']')
    {
        return Some((after_close, OperatorCodeRefSuffix::Dynamic(expr)));
    }

    None
}

/// The complete set of all-lowercase-letter word infix operators, used to
/// validate the `&[word]` operator-reference form. Symbolic infixes (`&[+]`)
/// and uppercase meta-ops (`&[Z]`, `&[Xcmp]`) are handled separately and are
/// never checked against this list.
fn is_known_word_infix(name: &str) -> bool {
    matches!(
        name,
        "div"
            | "mod"
            | "gcd"
            | "lcm"
            | "min"
            | "max"
            | "minmax"
            | "cmp"
            | "leg"
            | "coll"
            | "unicmp"
            | "eqv"
            | "before"
            | "after"
            | "and"
            | "or"
            | "xor"
            | "andthen"
            | "orelse"
            | "notandthen"
            | "x"
            | "xx"
            | "but"
            | "does"
            | "o"
            | "eq"
            | "ne"
            | "lt"
            | "gt"
            | "le"
            | "ge"
    )
}
