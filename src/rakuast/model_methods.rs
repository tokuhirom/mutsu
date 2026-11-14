//! Constructor metadata and the accessor schema used to generate method rows.

use super::*;

/// Constructors exposed by a model class; getter declarations live in rows.
// Cost: O(c), c = model classes.
pub fn constructor_method_names(class_name: &str) -> Option<Vec<&'static str>> {
    let class = class_from_name(class_name);
    if class.is_none() && hierarchy::entry(class_name).is_none() {
        return None;
    }
    let mut names = Vec::new();
    if let Some(class) = class {
        match class.constructor() {
            Constructor::FromIdentifier
                if matches!(class, RakuAstClass::Name | RakuAstClass::TermEnum) =>
            {
                names.push("from-identifier");
                if class == RakuAstClass::Name {
                    names.push("from-identifier-parts");
                    names.push("new");
                }
            }
            Constructor::New if constructor_is_supported(class) => names.push("new"),
            _ => {}
        }
    }
    names.sort_unstable();
    names.dedup();
    Some(names)
}

/// Getter schema used to generate and verify static method rows.
// Cost: O(f log f), f = accessor names of the class.
#[cfg(test)]
pub(super) fn accessor_method_names(class: RakuAstClass) -> Vec<&'static str> {
    let mut names = accessor_names(class);
    names.extend(named_ctor::field_names(class));
    if let Some((_, fields)) = multi_field_schema(class.printed_name(), "new") {
        names.extend(fields);
    }
    if let Some((_, fields)) = control_signature::constructor_schema(class.printed_name()) {
        names.extend(fields);
    }
    if class == RakuAstClass::ArgList {
        names.push("args");
    }
    match class {
        RakuAstClass::TypeDefinedness => names.extend(["base-type", "definite"]),
        RakuAstClass::TypeAnyDefinedness => names.push("base-type"),
        RakuAstClass::TypeCoercion => names.extend(["base-type", "constraint"]),
        RakuAstClass::Heredoc => names.push("stop"),
        RakuAstClass::Block => names.push("implicit-topic"),
        _ => {}
    }
    // Term::Self accepts private constructor state, not a public getter.
    if class == RakuAstClass::TermSelf {
        names.retain(|name| *name != "variable");
    }
    // The accessor over a class's single positional field (`Infix.operator`).
    if let Some(accessor) = fields::positional_accessor(class)
        && !names.contains(&accessor)
    {
        names.push(accessor);
    }
    names.extend(derived_methods::names(class));
    if class == RakuAstClass::CompUnit {
        names.push("sorries");
        names.push("worries");
        names.push("has-check-time-problems");
    }
    if matches!(class, RakuAstClass::StatementList | RakuAstClass::SemiList) {
        names.push("code-statements");
    }
    // (A statement's own `.^methods` does not list it: rakudo inherits it.)
    if matches!(class, RakuAstClass::Block | RakuAstClass::PointyBlock) {
        names.push("is-block-statement");
    }
    if class == RakuAstClass::Block {
        names.push("is-immediate-block-user-body");
        names.push("may-have-signature");
    }
    if class == RakuAstClass::StatementExpression {
        names.push("is-block-statement");
    }
    if class == RakuAstClass::Name {
        names.push("canonicalize");
    }
    if class == RakuAstClass::VarAttribute {
        names.push("return-type");
    }
    if class == RakuAstClass::WhateverCodeArgument {
        names.push("name");
    }
    if class == RakuAstClass::VarDeclarationSimple {
        for name in ["container-type", "return-type", "original-type"] {
            if !names.contains(&name) {
                names.push(name);
            }
        }
    }
    if matches!(
        class,
        RakuAstClass::VarDeclarationSimple
            | RakuAstClass::ParameterTargetVar
            | RakuAstClass::Sub
            | RakuAstClass::Method
            | RakuAstClass::Submethod
            | RakuAstClass::TypeCapture
            | RakuAstClass::VarDeclarationConstant
            | RakuAstClass::Block
            | RakuAstClass::PointyBlock
            | RakuAstClass::TypeSubset
            | RakuAstClass::Class
            | RakuAstClass::Role
            | RakuAstClass::Grammar
            | RakuAstClass::Module
            | RakuAstClass::Package
            | RakuAstClass::RoleBody
            | RakuAstClass::DeclarationResolvedConstant
            | RakuAstClass::DeclarationImport
    ) {
        names.push("compile-time-value");
    }
    if class == RakuAstClass::Nqp {
        names.push("op");
        names.push("args");
    }
    // The hidden name and signature a role's body holds in the unit tree.
    if class == RakuAstClass::RoleBody {
        names.push("name");
        names.push("signature");
    }
    if is_resolvable_lookup(class) {
        names.push("is-resolved");
        names.push("resolution");
    }
    if matches!(
        class,
        RakuAstClass::Parameter | RakuAstClass::Block | RakuAstClass::PointyBlock
    ) {
        names.push("meta-object");
    }
    if class.is_simple_variable() {
        names.push("sigil");
    }
    if matches!(
        class,
        RakuAstClass::RegexAssertionNamed | RakuAstClass::RegexAssertionNamedArgs
    ) {
        names.push("get-implicit-lookups");
    }
    if class == RakuAstClass::Parameter {
        names.push("signature-constraint");
    }
    names.sort_unstable();
    names.dedup();
    names
}

#[cfg(test)]
pub(crate) fn accessor_schema() -> Vec<(&'static str, &'static str, Vec<&'static str>)> {
    let mut schema: Vec<_> = RAKUAST_CLASSES
        .iter()
        .map(|&class| {
            let name = class.printed_name();
            (name, measured_name(name), accessor_method_names(class))
        })
        .collect();
    schema.push(("RakuAST::Statement", "RakuAST::Statement", vec!["labels"]));
    schema.push(("RakuAST::Node", "RakuAST::Node", vec!["origin"]));
    schema
}
