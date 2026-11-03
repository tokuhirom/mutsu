//! RakuAST model method declarations shared by reflection and method rows.

use super::*;

/// The implemented constructors, getters and mutators declared by a model
/// class, used by reflection even when it has no ordinary registry entry.
// Cost: O(f log f), f = local model methods.
pub fn local_method_names(class_name: &str) -> Option<Vec<&'static str>> {
    if let Some(names) = inherited_fields::local_names(class_name) {
        return Some(names);
    }
    // The core declarations a resolved lookup answers (see [`resolve`]).
    if matches!(
        class_name,
        "RakuAST::Declaration::External::Setting"
            | "RakuAST::Declaration::External::Constant"
            | "RakuAST::Declaration::Import"
            | "RakuAST::Declaration::LexicalPackage"
    ) {
        return Some(vec!["compile-time-value"]);
    }
    let class = class_from_name(class_name)?;
    let mut names = Vec::new();

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
    names.extend(accessor_method_names(class));
    if class == RakuAstClass::CompUnit {
        names.extend(["add-check-code", "add-sorry", "add-worry", "replace-statement-list"]);
    }
    if class == RakuAstClass::StatementExpression {
        names.push("set-expression");
    }
    if class == RakuAstClass::ArgList {
        names.push("push");
    }
    if matches!(
        class,
        RakuAstClass::ApplyInfix | RakuAstClass::ApplyListInfix
    ) {
        names.push("add-colonpair");
    }
    if class == RakuAstClass::StatementList {
        names.push("add-statement");
        names.push("unshift-statement");
    }
    names.sort_unstable();
    names.dedup();
    Some(names)
}

/// Getter metadata shared by reflection and static row generation.
// Cost: O(f log f), f = accessor names of the class.
pub(super) fn accessor_method_names(class: RakuAstClass) -> Vec<&'static str> {
    let mut names = accessor_names(class);
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
    names.sort_unstable();
    names.dedup();
    names
}

#[cfg(test)]
pub(crate) fn accessor_schema() -> Vec<(&'static str, &'static str, Vec<&'static str>)> {
    let mut schema: Vec<_> = RAKUAST_CLASSES.iter().map(|&class| {
        let name = class.printed_name();
        (name, measured_name(name), accessor_method_names(class))
    }).collect();
    schema.push(("RakuAST::Statement", "RakuAST::Statement", vec!["labels"]));
    schema
}
