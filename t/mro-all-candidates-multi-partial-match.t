use Test;

plan 5;

# `.*name`/`.+name` (all-candidates dispatch) resolves a multi method
# independently at EACH MRO level. If one level's own candidate set doesn't
# cover the call's arguments, real Rakudo still runs every level that DID
# resolve (side effects included, in MRO order) before raising a dispatch
# error for the whole expression -- it does not silently discard already-
# successful matches. See
# news/2026-08/mro-level-any-failed-partial-match.md.

class Base {
    multi method rt(Numeric $a) { say 'Numeric'; 'num' }
}
class Mid is Base {
    multi method rt()       { say 'empty'; 'empty' }
    multi method rt(Str $a) { say 'Str'; 'str' }
}

dies-ok { Mid.new.*rt }, '.*rt on a partial-match multi still fails overall';

{
    # The earlier-in-MRO level (Mid) still ran and produced its side effect
    # before the later level (Base) failed to resolve -- confirmed by
    # capturing stdout via run().
    my $prog = q:to/RAKU/;
        class Base {
            multi method rt(Numeric $a) { say 'Numeric' }
        }
        class Mid is Base {
            multi method rt()       { say 'empty' }
            multi method rt(Str $a) { say 'Str' }
        }
        my $b = Mid.new;
        try { $b.*rt };
        say 'after';
        RAKU
    my $proc = run $*EXECUTABLE, '-e', $prog, :out, :err;
    my $out = $proc.out.slurp(:close);
    $proc.err.slurp(:close);
    is $out, "empty\nafter\n",
        'the resolvable MRO level still executes (side effects preserved) before the overall dispatch error';
}

# Every level resolving cleanly is unaffected (regression guard for the
# ordinary, non-partial-match case).
role R5 {
    multi method rt()       { 'empty' }
    multi method rt(Str $a) { 'Str' }
}
role R6 {
    multi method rt(Numeric $a) { 'Numeric' }
}
class C { }
my C $c .= new;
$c does (R5, R6);
is-deeply $c.*rt, ('empty',), '.*rt still succeeds when every level resolves';

# A completely undefined method is unaffected by the partial-match fix: it
# stays the empty list, not a dispatch error (regression guard).
class D {
    multi method other() { }
}
is-deeply D.new.*rt, (), '.*rt on an undefined method is still the empty list';

# Type-object (.^ candidate) path mirrors the instance path.
dies-ok { Mid.*rt }, '.*rt on the type object also fails overall on a partial match';
