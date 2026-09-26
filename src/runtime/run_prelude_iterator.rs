//! The `Iterator` role's default methods, as real Raku source.
//!
//! A user class that `does Iterator` supplies `pull-one` and nothing else;
//! rakudo's role (`src/core.c/Iterator.rakumod`) builds every other protocol
//! method on top of it. They are declared here the same way rakudo declares
//! them -- ordinary role methods -- so they compose into the class, compile to
//! bytecode like any user method, lose to a method the class writes itself,
//! and show up in `.can` / `.^methods` (#9466). The built-in iterators
//! (`(1, 2).iterator`, `.squish.iterator`) keep their native protocol
//! methods: they are not instances of a class that composes this role.

use super::source_code_text::CodeText;
use super::*;

/// Each body follows rakudo's own: the return values (`IterationEnd` from the
/// push/sink methods, the `Int` 1/0 of `skip-one` and `skip-at-least`, the
/// `is-lazy` value `push-until-lazy` answers for a lazy iterator) are part of
/// the protocol, and were measured against rakudo 2026.07.
const ITERATOR_ROLE_PRELUDE: &str = r#"
role GLOBAL::Iterator {
    method push-exactly(\target, Int:D $count) {
        my $pulled;
        my int $i = 0;
        while $i < $count && !(($pulled := self.pull-one) =:= IterationEnd) {
            target.push($pulled);
            $i = $i + 1;
        }
        $pulled =:= IterationEnd ?? IterationEnd !! $count
    }
    method push-at-least(\target, Int:D $count) { self.push-exactly(target, $count) }
    method push-all(\target) {
        my $pulled;
        until ($pulled := self.pull-one) =:= IterationEnd {
            target.push($pulled);
        }
        IterationEnd
    }
    method push-until-lazy(\target) { self.is-lazy || self.push-all(target) }
    method sink-all() {
        until self.pull-one =:= IterationEnd { }
        IterationEnd
    }
    method skip-one() { self.pull-one =:= IterationEnd ?? 0 !! 1 }
    method skip-at-least(Int:D $toskip) {
        my int $left = $toskip;
        while $left && self.skip-one {
            $left = $left - 1;
        }
        $left ?? 0 !! 1
    }
    method skip-at-least-pull-one(Int:D $toskip) {
        self.skip-at-least($toskip) ?? self.pull-one !! IterationEnd
    }
    method is-lazy() { False }
    method is-deterministic() { True }
    method is-monotonically-increasing() { False }
}
"#;

impl Interpreter {
    /// Prepend the `Iterator` role's defaults ([`ITERATOR_ROLE_PRELUDE`]) to a
    /// program (or module) that composes `Iterator` (`does Iterator`,
    /// `but Iterator`). Injected for modules too: the class that composes the
    /// role usually lives in a module the program only `use`s. Parsed once and
    /// cached, like the other role preludes in `run_prelude.rs`.
    ///
    /// Skipped when the compunit declares its own package named `Iterator`
    /// (`class Iterator does Iterator`, `role Iterator`): the prelude's
    /// `GLOBAL::Iterator` would be a redeclaration of it. A package merely
    /// nested under the name (`class Iterator::Chunked`) is no collision.
    ///
    // TODO: the defaults belong to the CORE-scope `Iterator` role, outside
    // every compunit, where rakudo keeps them; a user `class Iterator {}` then
    // shadows it instead of colliding. Declaring them as `GLOBAL::Iterator` is
    // why this gate must key on composition rather than on any mention of the
    // name (`Test::Util` names `Iterator:D` in a signature while its test file
    // declares `class Iterator {}`), and a module that composes the role still
    // collides with a program that declares its own global `class Iterator`.
    pub(super) fn inject_iterator_role_prelude(source: &CodeText<'_>, stmts: &mut Vec<Stmt>) {
        let composes =
            source.contains_name("does Iterator") || source.contains_name("but Iterator");
        if !composes
            || source.contains_name("role Iterator")
            || source.contains_name("class Iterator")
        {
            return;
        }
        use std::sync::OnceLock;
        static ITERATOR_STMTS: OnceLock<Vec<Stmt>> = OnceLock::new();
        let prelude = ITERATOR_STMTS.get_or_init(|| {
            crate::parse_dispatch::parse_source(ITERATOR_ROLE_PRELUDE)
                .map(|(s, _)| s)
                .unwrap_or_default()
        });
        if prelude.is_empty() {
            return;
        }
        let mut combined = prelude.clone();
        combined.append(stmts);
        *stmts = combined;
    }
}
