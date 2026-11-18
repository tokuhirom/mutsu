//! Which methods of a slang *actions* role handle a grammar production
//! (ADR-13220).
//!
//! Rakudo calls an action only for the production that just matched, looked
//! up by that production's name, so an actions-role method is a production
//! handler exactly when the slang-extended grammar has a production of that
//! name. Any other method — Test::Async::Decl's `add_phaser`, which the
//! `package_declarator:sym<...>` actions call as `self.add_phaser($/)` — is a
//! helper that no parse ever calls. Rakudo's own actions classes carry such
//! helpers too, so "one method per production" (ADR-0098 §2.2) does not hold.
//!
//! mutsu's parser is not a grammar with named productions, so the host side
//! of that test reads `rakudo_grammar_productions.txt`: the production names
//! of Rakudo's grammar under both frontends, generated from the `raku` oracle
//! by `scripts/gen-rakudo-grammar-productions.raku`.

use std::collections::HashSet;
use std::sync::LazyLock;

const PRODUCTIONS_TXT: &str = include_str!("rakudo_grammar_productions.txt");

/// The host grammar's production names, and the proto families
/// (`statement_control` for `statement_control:sym<use>`) among them.
struct HostGrammar {
    productions: HashSet<&'static str>,
    protos: HashSet<&'static str>,
}

static HOST_GRAMMAR: LazyLock<HostGrammar> = LazyLock::new(|| {
    let productions: HashSet<&'static str> = production_lines().collect();
    let protos = productions.iter().filter_map(|name| proto_of(name)).collect();
    HostGrammar {
        productions,
        protos,
    }
});

fn production_lines() -> impl Iterator<Item = &'static str> {
    PRODUCTIONS_TXT
        .lines()
        .filter(|line| !line.is_empty() && !line.starts_with('#'))
}

/// The proto a `<proto>:sym<...>` candidate name belongs to.
fn proto_of(name: &str) -> Option<&str> {
    name.split_once(":sym<").map(|(proto, _)| proto)
}

/// Whether the actions-role method `name` handles a production of the grammar
/// it is registered with, given the productions the slang's own grammar half
/// declares (`slang_productions`).
///
/// A candidate of a proto family the host grammar has
/// (`statement_control:sym<frobnicate>`) counts as a production even when no
/// grammar declares it. Rakudo would never call such an action, but treating it
/// as a handler keeps it a loud "unsupported override" rather than an ignored
/// helper if the table predates the Rakudo release that added the candidate.
///
/// Cost: O(len(name)), name = the method name (two hash lookups).
pub(crate) fn is_production_handler(name: &str, slang_productions: &HashSet<String>) -> bool {
    let host = &*HOST_GRAMMAR;
    slang_productions.contains(name)
        || host.productions.contains(name)
        || proto_of(name).is_some_and(|proto| host.protos.contains(proto))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn table_is_sorted_and_unique() {
        let names: Vec<&str> = production_lines().collect();
        assert!(!names.is_empty());
        for pair in names.windows(2) {
            assert!(pair[0] < pair[1], "not sorted/unique: {:?}", pair);
        }
    }

    /// A recognized override the table did not list would be classified as a
    /// helper and silently ignored instead of switching its parser mode.
    #[test]
    fn every_recognized_override_is_a_production() {
        for name in crate::parser::RECOGNIZED_PRODUCTION_OVERRIDES {
            assert!(
                HOST_GRAMMAR.productions.contains(name),
                "recognized override {name} is not in rakudo_grammar_productions.txt"
            );
        }
    }

    #[test]
    fn classifies_handlers_and_helpers() {
        let none = HashSet::new();
        assert!(is_production_handler("statement-control:sym<use>", &none));
        assert!(is_production_handler("statement_control:sym<use>", &none));
        assert!(is_production_handler("blockoid", &none));
        assert!(is_production_handler("statement_control:sym<frobnicate>", &none));
        assert!(!is_production_handler("add_phaser", &none));
        assert!(!is_production_handler("frobnicate:sym<x>", &none));
        let own: HashSet<String> = ["frobnicate:sym<x>".to_string()].into();
        assert!(is_production_handler("frobnicate:sym<x>", &own));
    }
}
