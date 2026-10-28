//! `Hash::Ordered`, rakudo's insertion-ordered `Hash` subclass, as real Raku
//! source.
//!
//! rakudo ships it in core (`use Hash::Ordered` answers "is a builtin type"),
//! so a program names it with no `use`: `my %h is Hash::Ordered`,
//! `class Tuple is Hash::Ordered`. mutsu's `Hash` storage deliberately has no
//! stable order, so the class keeps the key order beside the hash and answers
//! every enumeration from it; point lookups and stores go to the `Hash` it
//! inherits. Declared as `GLOBAL::Hash::Ordered` and injected into a compunit
//! that names it, like the other source preludes (`run_prelude_iterator`).

use super::source_code_text::CodeText;
use super::*;

const HASH_ORDERED_PRELUDE: &str = r#"
class GLOBAL::Hash::Ordered is Hash {
    has @!order;
    method !track(Mu \key) {
        @!order.push(key) unless self.Hash::EXISTS-KEY(key);
    }
    method new(*@args, *%named) {
        my \o = self.bless;
        o.STORE((|@args, |%named.pairs));
        o
    }
    method STORE(\iter, :$INITIALIZE) {
        @!order = ();
        self.Hash::STORE(());
        my @items = iter.list;
        my $i = 0;
        while $i < @items.elems {
            my \item = @items[$i];
            if item ~~ Pair {
                self.ASSIGN-KEY(item.key, item.value);
                $i++;
            }
            else {
                self.ASSIGN-KEY(item, $i + 1 < @items.elems ?? @items[$i + 1] !! Any);
                $i += 2;
            }
        }
        self
    }
    method ASSIGN-KEY(\key, Mu \value) is raw {
        self!track(key);
        self.Hash::ASSIGN-KEY(key, value)
    }
    method BIND-KEY(\key, Mu \value) is raw {
        self!track(key);
        self.Hash::BIND-KEY(key, value)
    }
    method DELETE-KEY(\key) {
        my \removed = self.Hash::DELETE-KEY(key);
        @!order = @!order.grep({ !($_ eqv key) });
        removed
    }
    method keys() { [@!order] }
    method values() { @!order.map({ self.AT-KEY($_) }).Seq }
    method pairs() { @!order.map({ Pair.new($_, self.AT-KEY($_)) }).Seq }
    method kv() { @!order.map({ |($_, self.AT-KEY($_)) }).Seq }
    method antipairs() { @!order.map({ Pair.new(self.AT-KEY($_), $_) }).Seq }
    method invert() { @!order.map({ self.AT-KEY($_) => $_ }).Seq }
    method iterator() { self.pairs.iterator }
    method list() { self.pairs.List }
    method List() { self.pairs.List }
    method Seq() { self.pairs }
    method map(&block) { self.pairs.map(&block) }
    method grep(Mu $test) { self.pairs.grep($test) }
    method first(Mu $test = True, *%a) { self.pairs.first($test, |%a) }
    method raku() { self.^name ~ ".new((" ~ self.pairs.map(*.raku).join(",") ~ (@!order.elems == 1 ?? "," !! "") ~ "))" }
    method perl() { self.raku }
}
"#;

impl Interpreter {
    /// Prepend [`HASH_ORDERED_PRELUDE`] to a program (or module) that names
    /// `Hash::Ordered`. Skipped when the compunit declares its own package of
    /// that name. Parsed once and cached.
    // Cost: O(n) in the source scan, n = source bytes; the parse is once per process.
    pub(super) fn inject_hash_ordered_prelude(source: &CodeText<'_>, stmts: &mut Vec<Stmt>) {
        if !source.contains_name("Hash::Ordered")
            || source.contains_name("class Hash::Ordered")
            || source.contains_name("role Hash::Ordered")
            || source.contains_name("module Hash::Ordered")
            || source.contains_name("package Hash::Ordered")
        {
            return;
        }
        use std::sync::OnceLock;
        static HASH_ORDERED_STMTS: OnceLock<Vec<Stmt>> = OnceLock::new();
        let prelude = HASH_ORDERED_STMTS.get_or_init(|| {
            crate::runtime::prelude_source::parse_prelude_source(HASH_ORDERED_PRELUDE)
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
