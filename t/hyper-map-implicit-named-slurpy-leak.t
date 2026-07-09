use v6;
use Test;

# A method's implicit `*%_` slurpy is call-frame-local: a nested method call
# made from inside a `hyper`/`race` map block must bind its OWN `%_` (its own
# leftover named args), not inherit the enclosing method's `%_`.
#
# Regression: `clone_for_thread` migrated every env var into shared cells but
# skipped `_`/`@_` and not `%_`, so the enclosing method's `%_` leaked into the
# batch thread and shadowed each nested callee's own `%_`. The callee then saw
# the outer named args (e.g. the outer method's `:upgrade`) instead of its own.

plan 4;

class Inner {
    has $.name;
    has %.meta;
    method new(*%_) { self.bless(|%_, :meta(%_)) }
}

class Manager {
    method build(*@ids) {   # implicit %_ absorbs :flag when called with it
        my @repo = 1;
        my $r := @repo.hyper(:batch(1)).map: -> $x {
            @ids.map(-> $as { Inner.new(:name($as)) });
        }
        return $r.flat;
    }
}

my @c = Manager.new.build(<Available>, :flag);
is @c[0].name, "Available", "nested .new inside hyper map keeps its own :name arg";
is @c[0].meta.keys.sort.join(","), "name",
    "nested callee slurpy is its own leftover named args, not the enclosing method's";
nok @c[0].meta<flag>:exists, "enclosing method's :flag did not leak into the callee";

# A method with an explicit `:$name` plus `*%_` under hyper: named binds, %_ stays clean.
class T { method mk(:$name, *%_) { "$name/" ~ %_.elems } }
class M2 {
    method run(*@ids) {
        my @repo = 1;
        my $r := @repo.hyper(:batch(1)).map: -> $x { T.mk(:name(@ids[0])) };
        return $r.flat;
    }
}
is M2.new.run(<Available>, :upgrade)[0], "Available/0",
    "explicit named binds and implicit slurpy is empty (no enclosing slurpy leak)";
