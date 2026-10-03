use Test;

# A grammar method overriding a built-in rule (`ws`, `alpha`, `ident`, ...)
# defers to the built-in with `callsame` / `nextsame`: the built-in is the
# last candidate of the deferral chain, as `Match`'s own method is in Rakudo.
# Found through DSL::Entity::Foods, whose grammar composes
# DSL::Shared::Roles::ErrorHandling's high-water-mark `method ws`.

plan 12;

# The `$*HIGHWATER` write reaching `parse` is pinned by
# t/grammar/grammar-method-subrule-dynvar-write.t (#11326).
role HighWater {
    method ws() {
        if self.pos > $*HIGHWATER {
            $*HIGHWATER = self.pos;
        }
        callsame;
    }
    method parse($target, |c) {
        my $*HIGHWATER = 0;
        callsame;
    }
}

grammar Words does HighWater {
    regex word { <[a..z]>+ }
    rule two { <word> <word> }
}

ok Words.parse('abc def', rule => 'two'), 'role `method ws` with callsame keeps sigspace working';
nok Words.parse('abc 123', rule => 'two'), 'a real mismatch still fails';

grammar Traced {
    regex word { \w+ }
    method ws() {
        my $r = callsame;
        @*SEEN.push: self.pos => $r.pos;
        $r;
    }
    rule two { <word> <word> }
}

my @*SEEN;
ok Traced.parse('abc def', rule => 'two'), 'grammar `method ws` with callsame parses';
is-deeply @*SEEN, [3 => 4, 7 => 7], 'callsame answers a cursor advanced past the whitespace';

grammar NextSame {
    regex word { \w+ }
    method ws() { nextsame }
    rule two { <word> <word> }
}
ok NextSame.parse('abc def', rule => 'two'), 'nextsame reaches the built-in ws too';
nok NextSame.subparse('abcdef', rule => 'two'), 'the built-in ws still fails between word chars';
grammar NextSameMid { method ws() { nextsame }; token t { 'abcde' <.ws> 'f' } }
nok NextSameMid.subparse('abcdef', rule => 't'),
    'a failed cursor from nextsame is a failed call, not a zero-width match';

grammar Classes {
    method alpha() { callsame }
    method ident() { callsame }
    token ad { <alpha> <digit> }
    token opt { <alpha>? <digit> }
    token idt { <ident> }
}
ok Classes.parse('a1', rule => 'ad'), 'callsame from method alpha matches one alpha char';
nok Classes.parse('11', rule => 'ad'), 'a failed built-in is a failed cursor, not a zero-width match';
ok Classes.parse('1', rule => 'opt'), 'an optional overridden rule may fail and be skipped';
is ~Classes.parse('a_b9', rule => 'idt'), 'a_b9', 'callsame from method ident consumes the identifier';
nok Classes.parse('9a', rule => 'idt'), 'an identifier cannot start with a digit';
