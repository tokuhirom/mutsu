use v6;
use Test;

plan 7;

# Issue #8752: a grammar's `method TOP` override used to make `.parse` throw
# X::Method::NotFound before TOP was ever invoked at all. TOP is a
# well-established idiom for running setup code (binding a dynamic variable
# from named args, say) before delegating to the real entry rule.
my @called;
grammar Setup {
    method TOP () {
        @called.push('TOP');
        Nil
    }
}
# rakudo requires the start rule to hand back a Match/Cursor, so a method
# that returns Nil still dies -- but only AFTER actually running, and with a
# different, unrelated message than the old "Unknown method value dispatch".
dies-ok { Setup.parse("123") },
    'method-shaped TOP runs and then dies for returning a non-Match';
is @called, ['TOP'], 'the method-shaped TOP actually ran before dying';
try { Setup.parse("123") };
ok ~$! ~~ /"returned a Nil object"/,
    'the die is the rakudo "wrong return type" one, not X::Method::NotFound';

# The real-world idiom (the `IP::Addr` zef distribution): TOP runs setup
# code, then delegates to a real rule via `self.<rule>`, whose Match becomes
# the parse result.
role Digits {
    token digit { \d }
}
grammar Delegating does Digits {
    method TOP (Bool :$validate = False) {
        my $*VALIDATE = $validate;
        self.ip-variants
    }
    rule ip-variants { <digit>+ }
}

my $m = Delegating.parse("123");
ok $m, 'delegating method-shaped TOP produces a successful match';
is $m.^name, 'Delegating', 'the match is typed as the grammar, not plain Match';
is $m<digit>.join(','), '1,2,3', 'named captures from the delegated rule are present';

# A method-shaped TOP that only matches a PREFIX of the input still fails
# `.parse` overall, same as a regex-shaped TOP would.
nok Delegating.parse("123x").defined, 'a partial match still fails .parse';
