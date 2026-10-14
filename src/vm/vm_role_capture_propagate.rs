//! A role's lexical captures, given to the classes that composed it before the
//! role's declaring scope ran (#13358).
//!
//! A class declaration can be composed ahead of its source position: an
//! `our class E does R {}` is registered at program start by its hoisted
//! forward shell, and a nested declaration by its compile-time shell. When the
//! role it composes is declared in a block (`{ my $h = 5; role R { method h {
//! $h } } }`), that composition ran before the block did, so the methods and
//! attribute defaults it copied out of the role carry no capture of the
//! block's variables. The role's own registration, when the block runs, files
//! those captures (`capture_declared_role_method_envs`,
//! `capture_attr_decl_lexicals`); this pass hands them on to the copies the
//! earlier composition made, as Rakudo's role methods close over the role
//! body's scope however early the composing class was composed.

use crate::runtime::Interpreter;
use crate::symbol::Symbol;

impl Interpreter {
    /// Give every class that composed `role` the captures the role's current
    /// registration holds: a composed method takes its role method's
    /// `captured_env` (matched by name and candidate order), a composed attribute its
    /// role attribute's default capture (matched by name and sigil).
    // Cost: O(c * (r + m + a)), c = classes with composed roles, r = roles each
    // composed, m = methods and a = attributes of a class composing `role`.
    pub(crate) fn give_role_captures_to_composed_classes(&mut self, role: &str) {
        let (method_envs, attr_envs) = {
            let registry = self.registry();
            let Some(role_def) = registry.roles.get(role) else {
                return;
            };
            // Per method name, the role's own candidates' captures in
            // declaration order (`None` for a candidate with no capture).
            let method_envs: Vec<(Symbol, Vec<Option<crate::env::Env>>)> = role_def
                .methods
                .iter()
                .map(|(name, defs)| {
                    let envs = defs
                        .iter()
                        .filter(|def| def.role_origin.is_none())
                        .map(|def| def.captured_env.clone())
                        .collect::<Vec<_>>();
                    (Symbol::intern(name), envs)
                })
                .filter(|(_, envs)| envs.iter().any(Option::is_some))
                .collect();
            let attr_envs: Vec<(String, char, crate::env_capture_view::CapturedScope)> = role_def
                .attributes
                .iter()
                .filter_map(|attr| {
                    let env = attr.captured_env.clone()?;
                    Some((attr.name.clone(), attr.sigil, env))
                })
                .collect();
            (method_envs, attr_envs)
        };
        if method_envs.is_empty() && attr_envs.is_empty() {
            return;
        }
        let classes: Vec<String> = self
            .registry()
            .class_composed_roles
            .iter()
            .filter(|(_, roles)| {
                roles
                    .iter()
                    .any(|r| r.split_once('[').map_or(r.as_str(), |(base, _)| base) == role)
            })
            .map(|(class, _)| class.clone())
            .collect();
        let mut registry = self.registry_mut();
        if !method_envs.is_empty() {
            registry.bump_method_generation();
        }
        for class in classes {
            let owner = Symbol::intern(&class);
            for (name, envs) in &method_envs {
                let Some(defs) = registry.user_methods_mut(owner, *name) else {
                    continue;
                };
                // The composed copies keep the role's candidate order.
                let composed = defs.iter_mut().filter(|def| {
                    def.role_origin.as_deref() == Some(role)
                        && def.original_role.as_deref().is_none_or(|r| r == role)
                });
                for (def, env) in composed.zip(envs) {
                    if let Some(env) = env {
                        def.captured_env = Some(env.clone());
                    }
                }
            }
            if !attr_envs.is_empty()
                && let Some(class_def) = registry.classes.get_mut(&class)
            {
                for attr in class_def.attributes.iter_mut() {
                    if let Some((_, _, env)) = attr_envs
                        .iter()
                        .find(|(name, sigil, _)| *name == attr.name && *sigil == attr.sigil)
                    {
                        attr.captured_env = Some(env.clone());
                    }
                }
            }
        }
    }
}
