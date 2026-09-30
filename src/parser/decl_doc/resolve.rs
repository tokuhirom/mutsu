//! [`resolve`]: hand the recorded comments to the declarations they
//! document, and name each documented declaration by the structure the parse
//! recorded (see the parent module's docs for the rules).

use std::collections::{BTreeMap, HashMap};

use super::{Site, SiteIdent, Table};
use crate::decl_doc::{DeclDoc, DocComment, DocDeclKind};

/// The innermost site of `sites` enclosing offset `at` that `pick` accepts.
fn enclosing(
    sites: &BTreeMap<usize, Site>,
    at: usize,
    pick: impl Fn(&SiteIdent) -> bool,
) -> Option<(usize, &Site)> {
    sites
        .range(..at)
        .rev()
        .find(|(_, site)| at < site.end && pick(&site.ident))
        .map(|(s, site)| (*s, site))
}

/// Hand every comment to the declaration it documents (see the module docs).
fn assign_comments(t: &Table) -> HashMap<usize, DeclDoc> {
    let mut leading: HashMap<usize, Vec<&str>> = HashMap::new();
    let mut trailing: HashMap<usize, Vec<&str>> = HashMap::new();
    for (&at, comment) in &t.comments {
        let owner = if comment.trailing {
            t.sites
                .range(..at)
                .rev()
                .find(|(_, site)| at < site.claim_end)
                .map(|(s, _)| *s)
        } else {
            t.sites.range(comment.next_token..).next().map(|(s, _)| *s)
        };
        if let Some(owner) = owner {
            let texts = if comment.trailing {
                &mut trailing
            } else {
                &mut leading
            };
            texts.entry(owner).or_default().push(&comment.text);
        }
    }
    let mut docs: HashMap<usize, DeclDoc> = HashMap::new();
    for (owner, texts) in leading {
        docs.entry(owner).or_default().leading = Some(texts.join(" "));
    }
    for (owner, texts) in trailing {
        docs.entry(owner).or_default().trailing = Some(texts.join(" "));
    }
    docs
}

pub(super) fn resolve(t: &Table) -> Vec<DocComment> {
    let mut docs = assign_comments(t);
    let sites = &t.sites;
    let is_container = |i: &SiteIdent| {
        matches!(
            i,
            SiteIdent::Package {
                container: true,
                ..
            }
        )
    };
    // Qualified package names, outermost first so an inner one can extend
    // its parent's.
    let mut qualified: HashMap<usize, String> = HashMap::new();
    for (&s, site) in sites {
        if let SiteIdent::Package {
            name,
            container: true,
            ..
        } = &site.ident
        {
            let parent = enclosing(sites, s, is_container).and_then(|(p, _)| qualified.get(&p));
            let full = match parent {
                Some(parent) if !name.contains("::") => format!("{parent}::{name}"),
                _ => name.clone(),
            };
            qualified.insert(s, full);
        }
    }
    let package_of =
        |s: usize| enclosing(sites, s, is_container).map(|(p, _)| qualified[&p].clone());
    let member_key = |s: usize, name: &str| match package_of(s) {
        Some(pkg) => format!("{pkg}::{name}"),
        None => name.to_string(),
    };
    // A routine's base key, before a multi candidate's `/multi.N` suffix.
    let routine_key = |s: usize, name: &str, callable: Option<&str>| match callable {
        Some(_) => member_key(s, name),
        None => format!("&{name}"),
    };
    // Candidate and role-variant indices count every declaration, documented
    // or not, in source order -- the order the runtime numbers them in.
    let mut multi_index: HashMap<usize, usize> = HashMap::new();
    let mut role_index: HashMap<usize, usize> = HashMap::new();
    let mut multi_counters: HashMap<String, usize> = HashMap::new();
    let mut role_counters: HashMap<String, usize> = HashMap::new();
    for (&s, site) in sites {
        match &site.ident {
            SiteIdent::Routine {
                name,
                callable,
                multi: true,
                ..
            } => {
                let counter = multi_counters
                    .entry(routine_key(s, name, *callable))
                    .or_insert(0);
                multi_index.insert(s, *counter);
                *counter += 1;
            }
            SiteIdent::Package { is_role: true, .. } => {
                let counter = role_counters.entry(qualified[&s].clone()).or_insert(0);
                role_index.insert(s, *counter);
                *counter += 1;
            }
            _ => {}
        }
    }
    let mut out = Vec::new();
    for (&s, site) in sites {
        let Some(doc) = docs.remove(&s) else {
            continue;
        };
        let mut entry = DocComment {
            doc,
            ..DocComment::default()
        };
        match &site.ident {
            SiteIdent::Package {
                name, container, ..
            } => {
                let full = if *container {
                    qualified[&s].clone()
                } else {
                    name.clone()
                };
                entry.key = match role_index.get(&s) {
                    Some(&n) if n > 0 => format!("{full}/role.{n}"),
                    _ => full.clone(),
                };
                entry.wherefore_name = full;
                entry.kind = DocDeclKind::Package;
            }
            SiteIdent::Routine {
                name,
                callable,
                proto,
                ..
            } => {
                let base = routine_key(s, name, *callable);
                entry.key = match multi_index.get(&s) {
                    Some(n) => format!("{base}/multi.{n}"),
                    None => base.clone(),
                };
                entry.wherefore_name = base;
                entry.kind = DocDeclKind::Sub;
                entry.is_proto = *proto;
                entry.callable_type_override = callable.map(str::to_string);
            }
            SiteIdent::GrammarRule { name } => {
                entry.key = member_key(s, name);
                entry.wherefore_name = entry.key.clone();
                entry.kind = DocDeclKind::GrammarRule;
            }
            SiteIdent::Attr { sigiled } => {
                entry.key = member_key(s, sigiled);
                entry.wherefore_name = entry.key.clone();
                entry.kind = DocDeclKind::Attr;
            }
            SiteIdent::Param { sigiled } => {
                let owner = enclosing(sites, s, |i| {
                    matches!(i, SiteIdent::Routine { .. } | SiteIdent::Anon { .. })
                });
                entry.key = match owner.map(|(o, site)| (o, &site.ident)) {
                    Some((o, SiteIdent::Routine { name, callable, .. })) => {
                        format!("{}::{sigiled}", routine_key(o, name, *callable))
                    }
                    Some(_) => format!("&<anon>::{sigiled}"),
                    None => sigiled.clone(),
                };
                entry.wherefore_name = sigiled.clone();
                entry.kind = DocDeclKind::Param;
            }
            SiteIdent::Variable => continue,
            SiteIdent::Anon {
                block,
                callable,
                return_type,
                slot,
            } => {
                slot.fill(entry.doc.clone());
                entry.key = if *block { "<block>" } else { "&<anon>" }.to_string();
                entry.wherefore_name = entry.key.clone();
                entry.kind = if *block {
                    DocDeclKind::Block
                } else {
                    DocDeclKind::Sub
                };
                entry.callable_type_override = callable.map(str::to_string);
                entry.return_type = return_type.clone();
                entry.is_anonymous = true;
            }
        }
        out.push(entry);
    }
    out
}
