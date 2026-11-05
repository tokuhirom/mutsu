//! The declarator docs of a RakuAST tree that `EVAL` runs.
//!
//! A parsed unit hands the runtime a table of documented declarations
//! (`parser::decl_doc`), keyed by the name `.WHY` and `$=pod` look them up by
//! (`&sub`, `Pkg::method`, `Pkg::$!attr`, `R/role.1`, `&m/multi.0`). A tree that
//! never went through the parser carries its docs in the hidden
//! `declarator-docs-*` fields of the declaration nodes, so this builds the
//! same table from them, with the parser's keys: candidate and role-variant
//! indices count every declaration, documented or not, in source order.

use super::{
    RakuAstClass as C, RakuAstField, RakuAstFieldValue, RakuAstNode, declarator_docs,
    is_unvisited_field, resolve,
};
use crate::decl_doc::{DocComment, DocDeclKind};
use crate::value::ValueView;
use std::collections::HashMap;

#[derive(Default)]
struct Collector {
    packages: Vec<String>,
    multi_counters: HashMap<String, usize>,
    role_counters: HashMap<String, usize>,
    out: Vec<DocComment>,
}

fn str_field(node: &RakuAstNode, name: &str) -> Option<String> {
    node.fields.iter().find_map(|f| match (&f.name, &f.value) {
        (Some(n), RakuAstFieldValue::Node(v)) if *n == name => match v.view() {
            ValueView::Str(s) => Some(s.to_string()),
            _ => None,
        },
        _ => None,
    })
}

fn is_package(class: C) -> bool {
    matches!(
        class,
        C::Class | C::Role | C::Grammar | C::Module | C::Package
    )
}

impl Collector {
    fn member_key(&self, name: &str) -> String {
        match self.packages.last() {
            Some(pkg) => format!("{pkg}::{name}"),
            None => name.to_string(),
        }
    }

    // Cost: O(size of the subtree).
    fn visit(&mut self, node: &RakuAstNode) {
        let doc = declarator_docs::doc(node);
        let mut entered_package = false;
        match node.class {
            c if is_package(c) => {
                if let Some(name) = resolve::package_name(node) {
                    let full = match self.packages.last() {
                        Some(parent) if !name.contains("::") => format!("{parent}::{name}"),
                        _ => name,
                    };
                    let mut key = full.clone();
                    if c == C::Role {
                        let counter = self.role_counters.entry(full.clone()).or_insert(0);
                        if *counter > 0 {
                            key = format!("{full}/role.{counter}");
                        }
                        *counter += 1;
                    }
                    if let Some(doc) = doc {
                        self.out.push(DocComment {
                            doc,
                            wherefore_name: full.clone(),
                            key,
                            kind: DocDeclKind::Package,
                            ..DocComment::default()
                        });
                    }
                    self.packages.push(full);
                    entered_package = true;
                }
            }
            C::Sub | C::Method | C::Submethod => {
                if let Some(name) = resolve::package_name(node) {
                    let callable = match node.class {
                        C::Method => Some("Method"),
                        C::Submethod => Some("Submethod"),
                        _ => None,
                    };
                    let base = match callable {
                        Some(_) => self.member_key(&name),
                        None => format!("&{name}"),
                    };
                    let multiness = str_field(node, "multiness");
                    let mut key = base.clone();
                    if multiness.as_deref() == Some("multi") {
                        let counter = self.multi_counters.entry(base.clone()).or_insert(0);
                        key = format!("{base}/multi.{counter}");
                        *counter += 1;
                    }
                    if let Some(doc) = doc {
                        self.out.push(DocComment {
                            doc,
                            wherefore_name: base,
                            key,
                            kind: DocDeclKind::Sub,
                            is_proto: multiness.as_deref() == Some("proto"),
                            callable_type_override: callable.map(str::to_string),
                            ..DocComment::default()
                        });
                    }
                }
            }
            C::TokenDeclaration | C::RuleDeclaration | C::RegexDeclaration => {
                if let (Some(name), Some(doc)) = (resolve::package_name(node), doc) {
                    let key = self.member_key(&name);
                    self.out.push(DocComment {
                        doc,
                        wherefore_name: key.clone(),
                        key,
                        kind: DocDeclKind::GrammarRule,
                        ..DocComment::default()
                    });
                }
            }
            C::VarDeclarationSimple if str_field(node, "scope").as_deref() == Some("has") => {
                if let (Some(name), Some(doc)) = (resolve::package_name(node).or_else(|| {
                    // `desigilname` is the attribute's name.
                    attribute_name(node)
                }), doc)
                {
                    let sigil = str_field(node, "sigil").unwrap_or_default();
                    let key = self.member_key(&format!("{sigil}!{name}"));
                    self.out.push(DocComment {
                        doc,
                        wherefore_name: key.clone(),
                        key,
                        kind: DocDeclKind::Attr,
                        ..DocComment::default()
                    });
                }
            }
            _ => {}
        }
        for field in &node.fields {
            self.visit_field(node, field);
        }
        if entered_package {
            self.packages.pop();
        }
    }

    fn visit_field(&mut self, node: &RakuAstNode, field: &RakuAstField) {
        if is_unvisited_field(node, field) || declarator_docs::is_field(field) {
            return;
        }
        let values: Vec<&crate::value::Value> = match &field.value {
            RakuAstFieldValue::Node(v) => vec![v],
            RakuAstFieldValue::List(items) => items.iter().collect(),
            RakuAstFieldValue::Adverb(_) => Vec::new(),
        };
        for value in values {
            if let ValueView::RakuAst(child) = value.view() {
                self.visit(child);
            }
        }
    }
}

/// The attribute name of a `has` declaration (`has $.a` is `a`).
fn attribute_name(node: &RakuAstNode) -> Option<String> {
    let field = node.fields.iter().find(|f| f.name == Some("desigilname"))?;
    let RakuAstFieldValue::Node(name) = &field.value else {
        return None;
    };
    let ValueView::RakuAst(name) = name.view() else {
        return None;
    };
    match super::name_parts::name_shape(name) {
        Some(super::name_parts::NameShape::Identifier(name)) => Some(name),
        _ => None,
    }
}

/// The table of documented declarations of the tree rooted at `root`, in
/// source order.
// Cost: O(n), n = nodes of the tree.
pub(crate) fn collect(root: &RakuAstNode) -> Vec<DocComment> {
    let mut collector = Collector::default();
    collector.visit(root);
    collector.out
}
