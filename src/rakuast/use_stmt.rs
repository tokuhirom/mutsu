//! `use` / `no` statements across the RakuAST boundary.
//!
//! Measured against rakudo 2026.09, a `use` statement is one of three nodes:
//!
//! - `RakuAST::Pragma` for a core pragma (`use strict`, `use lib "lib"`,
//!   `use variables :D`), with `off => True` for its `no` form;
//! - `RakuAST::Statement::LanguageVersion` for `use v6.d`;
//! - `RakuAST::Statement::Use` for every other module, including
//!   `experimental` and `newline`.
//!
//! The parser keeps import tags (`:ALL`) apart from any other `use` argument,
//! while raku has a single `argument` expression: one `ColonPair::True` per
//! tag, a comma list for several. Both directions translate between the two.

use crate::scope::NameScope;
use super::convert::{convert_expr, leaf_field, name_from_identifier, node_field, plain_infix};
use super::lower::{leaf_str, list_field, lower_expr, named_child, positional_leaf};
use super::name_parts::{self, NameShape};
use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::ast::{Expr, Stmt};
use crate::symbol::Symbol;
use crate::value::{RuntimeError, Value, ValueView};

/// The core pragmas that rakudo represents as `RakuAST::Pragma`.
pub(super) fn is_pragma_name(name: &str) -> bool {
    matches!(
        name,
        "strict"
            | "fatal"
            | "nqp"
            | "soft"
            | "MONKEY"
            | "MONKEY-GUTS"
            | "MONKEY-TYPING"
            | "MONKEY-SEE-NO-EVAL"
            | "dynamic-scope"
            | "isms"
            | "precompilation"
            | "worries"
            | "trace"
            | "internals"
            | "lib"
    )
}

/// `use MODULE [ARG | :TAG...]` -> `Pragma`, `Statement::LanguageVersion` or
/// `Statement::Use`.
pub(super) fn convert_use(names: &NameScope, 
    module: &str,
    arg: Option<&Expr>,
    tags: &[String],
    condition: Option<&Expr>,
    if_imports: &[String],
) -> Result<RakuAstNode, RuntimeError> {
    // The parser keeps `use v6.d`'s version text as the argument of a `v6`
    // pseudo-module.
    if module == "v6" && tags.is_empty() {
        if condition.is_some() || !if_imports.is_empty() {
            return Err(super::convert::unsupported("conditional language version"));
        }
        if let Some(Expr::Literal(version)) = arg
            && let ValueView::Str(text) = version.view()
        {
            return Ok(RakuAstNode {
                origin: Default::default(),
                class: RakuAstClass::StatementLanguageVersion,
                fields: vec![leaf_field(None, Value::version_from_str(&text))],
            });
        }
        return Err(super::convert::unsupported("language version argument"));
    }
    // The smiley pragmas (`use variables :D`): the parser keeps the smiley as
    // a `":D"` string argument, which only the colonpair spelling produces (a
    // written string is a parse error), and rakudo's node is
    // `Pragma(argument => ColonPair::True("D"))`.
    if is_smiley_pragma(module) {
        let smiley = match arg {
            Some(Expr::Literal(value)) if tags.is_empty() && condition.is_none() => value
                .as_str()
                .and_then(|text| text.strip_prefix(':'))
                .filter(|smiley| matches!(*smiley, "D" | "U" | "_"))
                .map(str::to_string),
            _ => None,
        };
        let Some(smiley) = smiley else {
            return Err(super::convert::unsupported("smiley pragma"));
        };
        let mut node = pragma_node(module);
        node.fields
            .push(node_field(Some("argument"), colonpair_true(&smiley)));
        return Ok(node);
    }
    // `use Foo:from<NQP>`: the parser splits the `:from` adverb of the module
    // name into an argument and a tag; rakudo keeps it on the name.
    let from_spelling = match (arg.map(Expr::peel_parens), tags) {
        (Some(Expr::Literal(v)), [tag]) if tag == "from" && !is_pragma_name(module) => v
            .as_str()
            .filter(|text| !text.is_empty() && !text.contains(['<', '>', ' ', '\'', '"', '\\']))
            .map(str::to_string),
        _ => None,
    };
    let from_text = from_spelling;
    let (arg, tags) = if from_text.is_some() {
        (None, &[][..])
    } else {
        (arg, tags)
    };
    let argument = use_argument(names, arg, tags)?;
    let mut node = if is_pragma_name(module) {
        pragma_node(module)
    } else {
        RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::StatementUse,
            fields: vec![node_field(
                Some("module-name"),
                from_text.as_deref().map_or_else(
                    || name_from_identifier(module),
                    |text| name_with_from(module, text),
                ),
            )],
        }
    };
    if let Some(argument) = argument {
        node.fields.push(node_field(Some("argument"), argument));
    }
    if let Some(condition) = condition {
        node.fields.push(node_field(
            Some("source-if-condition"),
            convert_expr(names, condition)?,
        ));
    }
    if !if_imports.is_empty() {
        node.fields.push(RakuAstField {
            name: Some("source-if-imports"),
            value: RakuAstFieldValue::List(
                if_imports
                    .iter()
                    .map(|name| Value::str(name.clone()))
                    .collect(),
            ),
        });
    }
    Ok(node)
}

/// An inline `proto sub` is the `use` argument itself in Rakudo's tree. The
/// parser declares it ahead of the import in a transparent synthetic block;
/// replace that block with the public `Statement::Use` shape.
// Cost: O(a), a = direct import arguments.
pub(super) fn convert_inline_proto(names: &NameScope, 
    proto: &Stmt,
    use_stmt: &Stmt,
) -> Result<RakuAstNode, RuntimeError> {
    let Stmt::ProtoDecl { name, .. } = proto else {
        return Err(super::convert::unsupported("inline proto argument"));
    };
    let Stmt::Use {
        module,
        arg,
        tags,
        condition,
        if_imports,
    } = use_stmt
    else {
        return Err(super::convert::unsupported("inline proto use statement"));
    };
    let mut node = convert_use(names, module, arg.as_ref(), tags, condition.as_deref(), if_imports)?;
    let declaration = super::convert::convert_stmt(names, proto)?
        .ok_or_else(|| super::convert::unsupported("inline proto declaration"))?;
    let proto_node = named_child(&declaration, "expression")?.clone();
    let argument = node
        .fields
        .iter_mut()
        .find(|f| f.name == Some("argument"))
        .ok_or_else(|| super::convert::unsupported("inline proto without argument"))?;
    let RakuAstFieldValue::Node(value) = &mut argument.value else {
        return Err(super::convert::unsupported("inline proto argument"));
    };
    let ValueView::RakuAst(arg_node) = value.view() else {
        return Err(super::convert::unsupported("inline proto argument"));
    };
    let mut arg_node = arg_node.clone();
    if !replace_code_var(&mut arg_node, &name.resolve(), proto_node) {
        return Err(super::convert::unsupported("inline proto code argument"));
    }
    *value = Value::rakuast(Box::new(arg_node));
    Ok(node)
}

fn replace_code_var(arg: &mut RakuAstNode, name: &str, proto: RakuAstNode) -> bool {
    let expected = format!("&{name}");
    let matches_var = |node: &RakuAstNode| {
        node.class == RakuAstClass::VarLexical
            && positional_leaf(node).ok().is_some_and(
                |value| matches!(value.view(), ValueView::Str(text) if text.as_str() == expected),
            )
    };
    if matches_var(arg) {
        *arg = proto;
        return true;
    }
    if arg.class == RakuAstClass::ApplyListInfix
        && let Some(RakuAstField {
            value: RakuAstFieldValue::List(operands),
            ..
        }) = arg.fields.iter_mut().find(|f| f.name == Some("operands"))
    {
        for operand in operands {
            if let ValueView::RakuAst(candidate) = operand.view()
                && matches_var(candidate)
            {
                *operand = Value::rakuast(Box::new(proto));
                return true;
            }
        }
    }
    false
}

// Cost: O(1).
pub(super) fn is_source_field(field: &RakuAstField) -> bool {
    matches!(
        field.name,
        Some("source-if-condition" | "source-if-imports")
    )
}

// Cost: O(i), i = imported names recorded by the `if` adverb.
fn lower_source_if(node: &RakuAstNode) -> Result<(Option<Expr>, Vec<String>), RuntimeError> {
    let condition = node
        .fields
        .iter()
        .find(|field| field.name == Some("source-if-condition"))
        .map(|_| named_child(node, "source-if-condition").and_then(|node| lower_expr(names, node)))
        .transpose()?;
    let imports = match node
        .fields
        .iter()
        .find(|f| f.name == Some("source-if-imports"))
    {
        None => Vec::new(),
        Some(_) => list_field(node, "source-if-imports")?
            .iter()
            .map(|value| match value.view() {
                ValueView::Str(name) => Ok(name.to_string()),
                _ => Err(super::lower::unsupported(node)),
            })
            .collect::<Result<Vec<_>, _>>()?,
    };
    if condition.is_none() && !imports.is_empty() {
        return Err(super::lower::unsupported(node));
    }
    Ok((condition, imports))
}

/// `need MODULE;` -> `Statement::Need(module-names => (Name,))`.
pub(super) fn convert_need(module: &str) -> RakuAstNode {
    RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::StatementNeed,
        fields: vec![RakuAstField {
            name: Some("module-names"),
            value: RakuAstFieldValue::List(vec![Value::rakuast(Box::new(name_from_identifier(
                module,
            )))]),
        }],
    }
}

/// A statically named `require` keeps its package target through the round trip.
// Cost: O(1).
pub(super) fn convert_require(names: &NameScope, args: &[Expr]) -> Result<Option<RakuAstNode>, RuntimeError> {
    let (Expr::Literal(target), rest) = (match args {
        [first, rest @ ..] => (first, rest),
        [] => return Ok(None),
    }) else {
        return Ok(None);
    };
    let ValueView::Package(module) = target.view() else {
        return Ok(None);
    };
    let mut node = RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::StatementRequire,
        fields: vec![node_field(
            Some("module-name"),
            name_from_identifier(&module.resolve()),
        )],
    };
    match rest {
        [] => {}
        [import_list] => node
            .fields
            .push(node_field(Some("argument"), convert_expr(names, import_list)?)),
        _ => return Ok(None),
    }
    Ok(Some(node))
}

/// Lower `Statement::Require` to the parser's package-valued call operand.
// Cost: O(1).
pub(super) fn lower_require(names: &NameScope, node: &RakuAstNode) -> Result<Expr, RuntimeError> {
    if node
        .fields
        .iter()
        .any(|field| matches!(field.name, Some("file")))
    {
        return Err(super::lower::unsupported(node));
    }
    let module = match name_parts::name_shape(named_child(node, "module-name")?) {
        Some(NameShape::Identifier(name)) => name,
        _ => return Err(super::lower::unsupported(node)),
    };
    let mut args = vec![Expr::Literal(Value::package(Symbol::intern(&module)))];
    if let Ok(import_list) = named_child(node, "argument") {
        args.push(lower_expr(names, import_list)?);
    }
    Ok(Expr::Call {
        name: Symbol::intern("require"),
        args,
        listop: false,
    })
}

/// `import MODULE [:TAG...];` -> `Statement::Import`, its tags as the `use`
/// statement's `argument`.
pub(super) fn convert_import(module: &str, tags: &[String]) -> RakuAstNode {
    let mut node = RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::StatementImport,
        fields: vec![node_field(
            Some("module-name"),
            name_from_identifier(module),
        )],
    };
    if let Some(argument) = tag_argument(tags) {
        node.fields.push(node_field(Some("argument"), argument));
    }
    node
}

/// `no PRAGMA` -> `Pragma(off => True, name => PRAGMA)`.
pub(super) fn convert_no(module: &str) -> RakuAstNode {
    let mut node = pragma_node(module);
    node.fields.insert(0, leaf_field(Some("off"), Value::TRUE));
    node
}

fn pragma_node(name: &str) -> RakuAstNode {
    RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::Pragma,
        fields: vec![leaf_field(Some("name"), Value::str(name.to_string()))],
    }
}

/// `Foo:from<NQP>` as a `Name` with a `from` colonpair, measured on 2026.09:
/// `Name.from-identifier("Foo", colonpairs => (ColonPair::Value(key => "from",
/// value => QuotedString<words val>)))`.
// Cost: O(n), n = length of the module and language names.
fn name_with_from(module: &str, language: &str) -> RakuAstNode {
    let mut name = name_from_identifier(module);
    name.fields.push(RakuAstField {
        name: Some("colonpairs"),
        value: RakuAstFieldValue::List(vec![Value::rakuast(Box::new(RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::ColonPairValue,
            fields: vec![
                leaf_field(Some("key"), Value::str("from".to_string())),
                node_field(Some("value"), super::convert::word_quote(language)),
            ],
        }))]),
    });
    name
}

/// The language of a module name's `from` colonpair (the inverse of
/// [`name_with_from`]).
// Cost: O(1).
fn from_language(name: &RakuAstNode) -> Option<String> {
    let field = name.fields.iter().find(|f| f.name == Some("colonpairs"))?;
    let RakuAstFieldValue::List(pairs) = &field.value else {
        return None;
    };
    let [pair] = pairs.as_slice() else {
        return None;
    };
    let ValueView::RakuAst(pair) = pair.view() else {
        return None;
    };
    if pair.class != RakuAstClass::ColonPairValue || leaf_str(pair, "key").ok()? != "from" {
        return None;
    }
    let ValueView::RakuAst(value) = pair
        .fields
        .iter()
        .find(|f| f.name == Some("value"))
        .and_then(|f| match &f.value {
            RakuAstFieldValue::Node(v) => Some(v),
            _ => None,
        })?
        .view()
    else {
        return None;
    };
    name_parts::quoted_text(value)
}

/// The `argument` of a `use` statement. Both an argument and tags at once
/// (`use Foo :tag<x>`) is a shape the parser does not keep distinct.
fn use_argument(names: &NameScope, arg: Option<&Expr>, tags: &[String]) -> Result<Option<RakuAstNode>, RuntimeError> {
    match (arg, tags) {
        (None, []) => Ok(None),
        (Some(arg), []) => Ok(Some(convert_expr(names, arg)?)),
        (None, tags) => Ok(tag_argument(tags)),
        (Some(_), _) => Err(super::convert::unsupported(
            "use with both an argument and import tags",
        )),
    }
}

/// Import tags as one `argument`: a `ColonPair::True` for one tag, a comma
/// list of them for several, nothing for none.
fn tag_argument(tags: &[String]) -> Option<RakuAstNode> {
    match tags {
        [] => None,
        [tag] => Some(colonpair_true(tag)),
        tags => Some(RakuAstNode {
            origin: Default::default(),
            class: RakuAstClass::ApplyListInfix,
            fields: vec![
                node_field(Some("infix"), plain_infix(",")),
                RakuAstField {
                    name: Some("operands"),
                    value: RakuAstFieldValue::List(
                        tags.iter()
                            .map(|tag| Value::rakuast(Box::new(colonpair_true(tag))))
                            .collect(),
                    ),
                },
            ],
        }),
    }
}

fn colonpair_true(tag: &str) -> RakuAstNode {
    RakuAstNode {
        origin: Default::default(),
        class: RakuAstClass::ColonPairTrue,
        fields: vec![leaf_field(None, Value::str(tag.to_string()))],
    }
}

/// The pragmas whose argument is a type smiley (`use variables :D`).
// Cost: O(1).
fn is_smiley_pragma(name: &str) -> bool {
    matches!(name, "variables" | "attributes" | "parameters" | "invocant")
}

/// `RakuAST::Pragma` -> `Stmt::Use`, or `Stmt::No` when it is switched off.
pub(super) fn lower_pragma(names: &NameScope, node: &RakuAstNode) -> Result<Stmt, RuntimeError> {
    let module = leaf_str(node, "name")?;
    let off = node.fields.iter().any(|f| {
        f.name == Some("off") && matches!(&f.value, RakuAstFieldValue::Node(v) if v.truthy())
    });
    let (mut arg, mut tags) = lower_argument(names, node)?;
    // `Foo:from<NQP>`: the parser keeps the language as an argument and a tag.
    if let Some(language) = node
        .fields
        .iter()
        .any(|f| f.name == Some("module-name"))
        .then(|| named_child(node, "module-name"))
        .transpose()?
        .and_then(from_language)
    {
        if arg.is_some() || !tags.is_empty() {
            return Err(super::lower::unsupported(node));
        }
        arg = Some(Expr::Literal(Value::str(language)));
        tags.push("from".to_string());
    }
    let (condition, if_imports) = lower_source_if(node)?;
    // `use variables :D`: the parser's `":D"` argument, not an import tag.
    if is_smiley_pragma(&module) {
        let [smiley] = tags.as_slice() else {
            return Err(super::lower::unsupported(node));
        };
        if off || arg.is_some() || !matches!(smiley.as_str(), "D" | "U" | "_") {
            return Err(super::lower::unsupported(node));
        }
        arg = Some(Expr::Literal(Value::str(format!(":{smiley}"))));
        tags.clear();
    }
    if off {
        if arg.is_some() || !tags.is_empty() || condition.is_some() || !if_imports.is_empty() {
            return Err(super::lower::unsupported(node));
        }
        return Ok(Stmt::No { module, arg: None });
    }
    Ok(Stmt::Use {
        module,
        arg,
        tags,
        condition: condition.map(Box::new),
        if_imports,
    })
}

/// `RakuAST::Statement::Use` -> `Stmt::Use`.
pub(super) fn lower_use(names: &NameScope, node: &RakuAstNode) -> Result<Stmt, RuntimeError> {
    let mut node = node.clone();
    let inline_proto = take_inline_proto(names, &mut node)?;
    let node = &node;
    let module = match name_parts::name_shape(named_child(node, "module-name")?) {
        Some(NameShape::Identifier(name)) => name,
        _ => return Err(super::lower::unsupported(node)),
    };
    let (mut arg, mut tags) = lower_argument(names, node)?;
    // `Foo:from<NQP>`: the parser keeps the language as an argument and a tag.
    if let Some(language) = from_language(named_child(node, "module-name")?) {
        if arg.is_some() || !tags.is_empty() {
            return Err(super::lower::unsupported(node));
        }
        arg = Some(Expr::Literal(Value::str(language)));
        tags.push("from".to_string());
    }
    let (condition, if_imports) = lower_source_if(node)?;
    // `use newline :crlf` keeps its pair as the argument, where any other
    // module's `:tag` is an import tag.
    if module == "newline"
        && arg.is_none()
        && let [tag] = tags.as_slice()
    {
        arg = Some(Expr::Binary {
            left: Box::new(Expr::Literal(Value::str(tag.clone()))),
            op: crate::token_kind::TokenKind::FatArrow,
            right: Box::new(Expr::Literal(Value::TRUE)),
            form: crate::ast::BinaryForm::ColonPairTrue,
        });
        tags.clear();
    }
    let use_stmt = Stmt::Use {
        module,
        arg,
        tags,
        condition: condition.map(Box::new),
        if_imports,
    };
    Ok(match inline_proto {
        Some(proto) => super::lower_parsed_stmt(crate::ast::parsed::ParsedStmt::InlineProtoUse {
            proto: Box::new(proto),
            use_stmt: Box::new(use_stmt),
        })?,
        None => use_stmt,
    })
}

fn take_inline_proto(names: &NameScope, node: &mut RakuAstNode) -> Result<Option<Stmt>, RuntimeError> {
    let Some(field) = node.fields.iter_mut().find(|f| f.name == Some("argument")) else {
        return Ok(None);
    };
    let RakuAstFieldValue::Node(value) = &mut field.value else {
        return Err(super::lower::unsupported(node));
    };
    let ValueView::RakuAst(argument) = value.view() else {
        return Err(super::lower::unsupported(node));
    };
    let mut argument = argument.clone();
    let proto = take_proto_operand(names, &mut argument)?;
    if proto.is_some() {
        *value = Value::rakuast(Box::new(argument));
    }
    Ok(proto)
}

fn take_proto_operand(names: &NameScope, argument: &mut RakuAstNode) -> Result<Option<Stmt>, RuntimeError> {
    let is_proto =
        |node: &RakuAstNode| node.class == RakuAstClass::Sub && super::proto::is_proto(node);
    if is_proto(argument) {
        let proto = super::proto::lower(names, argument)?;
        let Stmt::ProtoDecl { name, .. } = &proto else {
            unreachable!("proto lowerer")
        };
        *argument = super::convert::var_lexical("&", &name.resolve());
        return Ok(Some(proto));
    }
    if argument.class == RakuAstClass::ApplyListInfix
        && let Some(RakuAstField {
            value: RakuAstFieldValue::List(operands),
            ..
        }) = argument
            .fields
            .iter_mut()
            .find(|f| f.name == Some("operands"))
    {
        for operand in operands {
            if let ValueView::RakuAst(candidate) = operand.view()
                && is_proto(candidate)
            {
                let proto = super::proto::lower(names, candidate)?;
                let Stmt::ProtoDecl { name, .. } = &proto else {
                    unreachable!("proto lowerer")
                };
                *operand =
                    Value::rakuast(Box::new(super::convert::var_lexical("&", &name.resolve())));
                return Ok(Some(proto));
            }
        }
    }
    Ok(None)
}

/// `RakuAST::Statement::Need` -> `Stmt::Need`. Only the one-module form the
/// converter renders; several names or a version colonpair stay the boundary.
pub(super) fn lower_need(node: &RakuAstNode) -> Result<Stmt, RuntimeError> {
    let names = list_field(node, "module-names")?;
    let [only] = names else {
        return Err(super::lower::unsupported(node));
    };
    let ValueView::RakuAst(name) = only.view() else {
        return Err(super::lower::unsupported(node));
    };
    match name_parts::name_shape(name) {
        Some(NameShape::Identifier(module)) => Ok(Stmt::Need { module }),
        _ => Err(super::lower::unsupported(node)),
    }
}

/// `RakuAST::Statement::Import` -> `Stmt::Import`.
pub(super) fn lower_import(names: &NameScope, node: &RakuAstNode) -> Result<Stmt, RuntimeError> {
    let module = match name_parts::name_shape(named_child(node, "module-name")?) {
        Some(NameShape::Identifier(name)) => name,
        _ => return Err(super::lower::unsupported(node)),
    };
    let (arg, tags) = lower_argument(names, node)?;
    if arg.is_some() {
        return Err(super::lower::unsupported(node));
    }
    Ok(Stmt::Import { module, tags })
}

/// `RakuAST::Statement::LanguageVersion` -> the parser's `use v6` form.
pub(super) fn lower_language_version(node: &RakuAstNode) -> Result<Stmt, RuntimeError> {
    let version = positional_leaf(node)?;
    if !matches!(version.view(), ValueView::Version { .. }) {
        return Err(super::lower::unsupported(node));
    }
    Ok(Stmt::Use {
        module: "v6".to_string(),
        arg: Some(Expr::Literal(Value::str(version.to_string_value()))),
        tags: Vec::new(),
        condition: None,
        if_imports: Vec::new(),
    })
}

/// Split a `use` statement's `argument` back into the parser's argument and
/// import tags: a `ColonPair::True`, or a comma list made only of them, is a
/// tag list; anything else is the argument.
fn lower_argument(names: &NameScope, node: &RakuAstNode) -> Result<(Option<Expr>, Vec<String>), RuntimeError> {
    let Ok(argument) = named_child(node, "argument") else {
        return Ok((None, Vec::new()));
    };
    if let Some(tag) = colonpair_true_key(argument) {
        return Ok((None, vec![tag]));
    }
    if argument.class == RakuAstClass::ApplyListInfix
        && let Ok(operands) = list_field(argument, "operands")
        && !operands.is_empty()
    {
        let tags = operands
            .iter()
            .map(|operand| match operand.view() {
                ValueView::RakuAst(operand) => colonpair_true_key(operand),
                _ => None,
            })
            .collect::<Option<Vec<_>>>();
        if let Some(tags) = tags {
            return Ok((None, tags));
        }
    }
    let lowered = lower_expr(names, argument)?;
    // The parser keeps the parentheses of an import list (`use Foo ()`, `use Foo
    // (1, 2)`); an empty one is what suppresses the module's default exports.
    let lowered = if argument.class == RakuAstClass::CircumfixParentheses
        && !matches!(lowered, Expr::Grouped(_))
    {
        Expr::Grouped(Box::new(lowered))
    } else {
        lowered
    };
    Ok((Some(lowered), Vec::new()))
}

fn colonpair_true_key(node: &RakuAstNode) -> Option<String> {
    if node.class != RakuAstClass::ColonPairTrue {
        return None;
    }
    match positional_leaf(node).ok()?.view() {
        ValueView::Str(key) => Some(key.to_string()),
        _ => None,
    }
}
