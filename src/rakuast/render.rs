//! RakuAST node → `.gist`/`.raku`/`.Str` rendering (ADR-0011).
//!
//! Reproduces raku's constructor-form gist exactly: 2-space indent per level,
//! named-arg keys left-padded to the class's alignment width, and
//! `List`-valued fields printed as a parenthesised trailing-comma list. A node
//! renders inline on one line iff every field is a positional leaf literal (no
//! named field, no child node, no list); any named field, child node, or list
//! forces the multi-line form (e.g. `Postfix.new(operator => "++")` is
//! multi-line despite its single leaf, because the field is named).

use super::{Constructor, RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::value::{Value, ValueView};

pub(super) fn render_node(node: &RakuAstNode, indent: usize) -> String {
    // rakudo 2026.09 prints a priming `*` (its `WhateverCode::Argument`, still
    // the node's `.^name`) as `RakuAST::Term::Whatever.new`, exactly like a
    // `*` value.
    if node.class == RakuAstClass::WhateverCodeArgument {
        let shown = RakuAstNode {
            class: RakuAstClass::TermWhatever,
            fields: node.fields.clone(),
        };
        return render_node(&shown, indent);
    }
    if node.class == RakuAstClass::VarDeclarationSimple
        && let Some(shown) = super::attribute::without_implicit_traits(node)
    {
        return render_node(&shown, indent);
    }
    let name = node.class.printed_name();
    if node.class == RakuAstClass::Name
        && !node.fields.iter().any(|f| f.name == Some("colonpairs"))
        && let Some(rendered) = render_name_parts(node, indent)
    {
        return rendered;
    }
    if node.class == RakuAstClass::Name
        && let Some(rendered) = render_name_with_colonpairs(node, indent)
    {
        return rendered;
    }
    // A bare-class-name node (e.g. `RakuAST::Parameter::Slurpy::Flattened`) has no
    // constructor call at all.
    if node.class.renders_bare() {
        return name.to_string();
    }
    let ctor = match node.class.constructor() {
        Constructor::New => "new",
        Constructor::FromIdentifier => "from-identifier",
    };

    // A class-specific gist quirk: raku's `Assignment` list form omits even the
    // empty parens (`RakuAST::Assignment.new`), unlike the generic `.new()`.
    if node.fields.is_empty() && node.class.empty_parens_omitted() {
        return format!("{name}.{ctor}");
    }

    // `Term::Enum.from-identifier('True')` — raku renders the enum identifier in
    // single quotes (unlike `Name.from-identifier("say")`, which uses double).
    if node.class == RakuAstClass::TermEnum
        && let Some(f) = node.fields.first()
        && let RakuAstFieldValue::Node(v) = &f.value
        && let ValueView::Str(s) = v.view()
    {
        return format!("{name}.{ctor}('{}')", s.as_str());
    }

    // An empty `{}` composer: raku renders its absent `expression` as a blank
    // positional line rather than as empty parens.
    if node.class == RakuAstClass::CircumfixHashComposer && node.fields.is_empty() {
        let pad = " ".repeat(indent + 2);
        return format!("{name}.{ctor}(\n{pad}\n{})", " ".repeat(indent));
    }

    // Inline when every field is a positional leaf or a colonpair adverb
    // (`Assignment.new(:item)`); any named field, child node, or list forces
    // the multi-line form.
    let fields = rendered_fields(node);
    // Rakudo renders a char-class `Character`, a `Var::Dynamic`, a
    // `Var::Attribute`, a placeholder and a regex back-reference on their own
    // lines all the same.
    if !matches!(
        node.class,
        RakuAstClass::RegexCharClassEnumerationElementCharacter
            | RakuAstClass::VarDynamic
            | RakuAstClass::VarAttribute
            | RakuAstClass::VarDeclarationPlaceholderPositional
            | RakuAstClass::VarDeclarationPlaceholderNamed
            | RakuAstClass::VarDeclarationPlaceholderSlurpyArray
            | RakuAstClass::VarDeclarationPlaceholderSlurpyHash
            | RakuAstClass::RegexBackReferenceNamed
            | RakuAstClass::RegexBackReferencePositional
    ) && fields
        .iter()
        .all(|f| f.name.is_none() && is_inline_field(f))
    {
        let inner = fields
            .iter()
            .map(|field| render_inline_field(field))
            .collect::<Vec<_>>()
            .join(", ");
        return format!("{name}.{ctor}({inner})");
    }

    let width = field_align_width(node);
    let mut s = format!("{name}.{ctor}(\n");
    let child_indent = indent + 2;
    let pad = " ".repeat(child_indent);
    for (i, f) in fields.iter().enumerate() {
        s.push_str(&pad);
        s.push_str(&render_field_line(f, child_indent, width));
        if i + 1 != fields.len() {
            s.push(',');
        }
        s.push('\n');
    }
    s.push_str(&" ".repeat(indent));
    s.push(')');
    s
}

/// A name written with colonpairs (`class A:ver<1.0> { }`), which rakudo
/// renders with the pairs inside the constructor call:
///
/// ```text
/// RakuAST::Name.from-identifier("A", colonpairs => (
///   RakuAST::ColonPair::Value.new(...),
/// ))
/// ```
///
/// `indent` is the column of the line the name sits on.
// Cost: O(n), n = size of the colonpairs.
fn render_name_with_colonpairs(node: &RakuAstNode, indent: usize) -> Option<String> {
    let pairs = node.fields.iter().find(|f| f.name == Some("colonpairs"))?;
    let RakuAstFieldValue::List(pairs) = &pairs.value else {
        return None;
    };
    let head = if let Some(parts) = node.fields.iter().find(|f| f.name == Some("parts")) {
        let RakuAstFieldValue::List(parts) = &parts.value else {
            return None;
        };
        let identifiers = parts
            .iter()
            .map(simple_part_identifier)
            .collect::<Option<Vec<_>>>()?;
        format!(
            "RakuAST::Name.from-identifier-parts({}, colonpairs => (\n",
            identifiers.join(",")
        )
    } else {
        let identifier = node.fields.iter().find(|f| f.name.is_none())?;
        let RakuAstFieldValue::Node(identifier) = &identifier.value else {
            return None;
        };
        format!(
            "RakuAST::Name.from-identifier({}, colonpairs => (\n",
            render_leaf(identifier)
        )
    };
    let mut s = head;
    let pad = " ".repeat(indent + 2);
    for pair in pairs {
        s.push_str(&pad);
        match pair.view() {
            ValueView::RakuAst(pair) => s.push_str(&render_node(pair, indent + 2)),
            _ => s.push_str(&render_leaf(pair)),
        }
        s.push_str(",\n");
    }
    s.push_str(&" ".repeat(indent));
    s.push_str("))");
    Some(s)
}

/// Render a `RakuAST::Name` built from a `parts` list, picking the spelling
/// Rakudo's `.raku` uses: `from-identifier("x")` for one identifier part,
/// `from-identifier-parts("A","B")` (no space after the comma) for several,
/// and the general `Name.new(part, ...)` as soon as any part is not an
/// identifier — the empty edge of `::Foo` / `Foo::` or a `::(...)` expression.
fn render_name_parts(node: &RakuAstNode, indent: usize) -> Option<String> {
    let field = node
        .fields
        .iter()
        .find(|field| field.name == Some("parts"))?;
    let RakuAstFieldValue::List(parts) = &field.value else {
        return None;
    };
    if parts.is_empty() {
        return Some("RakuAST::Name.new()".to_string());
    }
    let identifiers = parts
        .iter()
        .map(simple_part_identifier)
        .collect::<Option<Vec<_>>>();
    if let Some(identifiers) = identifiers {
        return Some(if let [only] = identifiers.as_slice() {
            format!("RakuAST::Name.from-identifier({only})")
        } else {
            format!(
                "RakuAST::Name.from-identifier-parts({})",
                identifiers.join(",")
            )
        });
    }
    let child_indent = indent + 2;
    let pad = " ".repeat(child_indent);
    let rendered = parts
        .iter()
        .map(|part| {
            let part = match part.view() {
                ValueView::RakuAst(part) => render_node(part, child_indent),
                _ => render_leaf(part),
            };
            format!("{pad}{part}")
        })
        .collect::<Vec<_>>();
    Some(format!(
        "RakuAST::Name.new(\n{}\n{})",
        rendered.join(",\n"),
        " ".repeat(indent)
    ))
}

/// The rendered string literal of a `Name::Part::Simple`, or `None` for any
/// other part.
fn simple_part_identifier(part: &Value) -> Option<String> {
    let ValueView::RakuAst(part) = part.view() else {
        return None;
    };
    if part.class != RakuAstClass::NamePartSimple {
        return None;
    }
    let field = part.fields.first()?;
    if field.name.is_some() {
        return None;
    }
    let RakuAstFieldValue::Node(value) = &field.value else {
        return None;
    };
    matches!(value.view(), ValueView::Str(_)).then(|| render_leaf(value))
}

/// Rakudo keeps `Regex::NamedCapture.array` observable through its accessor,
/// but omits that implementation-detail field from the constructor-form gist.
/// Keep the field in the model so lowering and reflection retain the source
/// sigil while matching Rakudo's renderer.
fn rendered_fields(node: &RakuAstNode) -> Vec<&RakuAstField> {
    node.fields
        .iter()
        .filter(|field| {
            !(super::origin::is_origin(field)
                || super::declared_routines::is_user_call_field(field)
                || super::postfix_grouping::is_source_field(field)
                || super::compound_stmt::is_source_field(field)
                || super::use_stmt::is_source_field(field)
                || super::type_call::is_marker(field)
                || super::thunk::is_marker(field)
                || node.class == RakuAstClass::RegexNamedCapture && field.name == Some("array")
                || super::regex_code::is_source(node, field)
                || super::phaser_condition::is_source(node, field)
                || node.class == RakuAstClass::RegexAssertionNamedRegexArg
                    && field.name == Some("capturing"))
        })
        .collect()
}

/// The `key => value` alignment width for a node: the max length over its
/// *shown* named-field keys, floored by the class's `min_align_width` (which
/// covers the few classes that pad to a declared-but-omitted attribute).
fn field_align_width(node: &RakuAstNode) -> usize {
    let shown = rendered_fields(node)
        .iter()
        .filter_map(|f| f.name.map(str::len))
        .max()
        .unwrap_or(0);
    shown.max(node.class.min_align_width())
}

fn is_inline_field(f: &RakuAstField) -> bool {
    match &f.value {
        RakuAstFieldValue::List(_) => false,
        RakuAstFieldValue::Node(v) => !matches!(v.view(), ValueView::RakuAst(_)),
        RakuAstFieldValue::Adverb(_) => true,
    }
}

fn render_inline_field(f: &RakuAstField) -> String {
    let val = match &f.value {
        RakuAstFieldValue::Node(v) => render_leaf(v),
        RakuAstFieldValue::Adverb(name) => return format!(":{name}"),
        // Unreachable while all fields are inline leaves, but keep it total.
        RakuAstFieldValue::List(_) => "()".to_string(),
    };
    match f.name {
        Some(key) => format!("{key} => {val}"),
        None => val,
    }
}

fn render_field_line(f: &RakuAstField, indent: usize, width: usize) -> String {
    let val = if f.name == Some("processors") {
        render_processors(&f.value, indent)
    } else {
        render_field_value(&f.value, indent)
    };
    match f.name {
        Some(key) => format!("{key:width$} => {val}"),
        None => val,
    }
}

/// Rakudo renders a QuotedString's processor names as a word list (`<words
/// val>`), rather than the ordinary parenthesized list form used by other
/// RakuAST list-valued fields.
fn render_processors(fv: &RakuAstFieldValue, indent: usize) -> String {
    let RakuAstFieldValue::List(items) = fv else {
        return render_field_value(fv, indent);
    };
    let values = items
        .iter()
        .map(|item| match item.view() {
            ValueView::Str(value) => value.to_string(),
            _ => render_leaf(item),
        })
        .collect::<Vec<_>>();
    // A single processor is a one-element `List`, which `.raku` parenthesizes.
    if let [only] = values.as_slice() {
        return format!("(\"{only}\",)");
    }
    format!("<{}>", values.join(" "))
}

fn render_field_value(fv: &RakuAstFieldValue, indent: usize) -> String {
    match fv {
        RakuAstFieldValue::Node(v) => match v.view() {
            ValueView::RakuAst(node) => render_node(node, indent),
            _ => render_leaf(v),
        },
        RakuAstFieldValue::List(items) => render_paren_list(items, indent),
        RakuAstFieldValue::Adverb(name) => format!(":{name}"),
    }
}

/// A parenthesised, trailing-comma list — every element gets a trailing comma
/// (so a single element reads `( x, )`). An *empty* list is `()`, as raku
/// 2026.09's gist prints it (e.g. the `parameters` of a parameter-less
/// `RakuAST::Signature`); older rakudo printed the itemized `$( )`.
fn render_paren_list(items: &[Value], indent: usize) -> String {
    if items.is_empty() {
        return "()".to_string();
    }
    let child_indent = indent + 2;
    let pad = " ".repeat(child_indent);
    let mut s = String::from("(\n");
    for item in items {
        s.push_str(&pad);
        match item.view() {
            ValueView::RakuAst(node) => s.push_str(&render_node(node, child_indent)),
            _ => s.push_str(&render_leaf(item)),
        }
        s.push_str(",\n");
    }
    s.push_str(&" ".repeat(indent));
    s.push(')');
    s
}

fn render_leaf(v: &Value) -> String {
    match v.view() {
        ValueView::Str(s) => render_str_literal(&s),
        // A TYPE OBJECT field value renders as its bare class name, not as the
        // `(Name)` form `Mu.gist` gives a type object on its own: rakudo's
        // `Parameter` gist shows `slurpy => RakuAST::Parameter::Slurpy::Flattened`
        // while `.slurpy.gist` alone is `(Flattened)`. See `slurpy_marker_value`
        // for why the field holds a type object at all (GH #8157).
        ValueView::Package(name) => name.resolve(),
        // A version leaf renders as its literal (`LanguageVersion.new(v6.d)`),
        // which is its `.gist`, not its `.Str` (`6.d`).
        ValueView::Version { .. } => format!("v{}", v.to_string_value()),
        // A Num leaf renders as its literal (`NumLiteral.new(1e0)`, `Inf`).
        ValueView::Num(f) => crate::builtins::methods_0arg::raku_repr::format_num_raku(f),
        // A complex leaf renders as the angle-bracket literal that is its
        // `.raku` (`ComplexLiteral.new(<1+2i>)`).
        ValueView::Complex(..) => format!("<{}>", v.to_string_value()),
        _ => v.to_string_value(),
    }
}

/// Render a Raku double-quoted string literal. Rakudo renders a `StrLiteral`
/// as its string's `.raku`, so this is `Str.raku`'s own escaping.
// Cost: O(n), n = chars of `s`.
fn render_str_literal(s: &str) -> String {
    crate::value::raku_repr::escape_raku_str(s)
}
