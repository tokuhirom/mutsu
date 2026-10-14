use Test;

plan 13;

# A nested `[:m …]` / `(:m …)` group, or an interpolation inside a whole-`:m`
# rule, enters its ends lazily like a rule frame: a code block in it runs once
# per end entered, not once per end computed (#13216). Each expectation was
# checked against rakudo.

my @log;

grammar G1 { regex TOP { <r> 'x' }; regex r { [:m a+ { @log.push: $/.to }] } }
@log = ();
ok G1.parse("a\x[301]ax"), 'a :m group in a plain rule matches over a marked subject';
is @log.join(','), '2', '... and its code block ran only at the end entered';

grammar G2 { regex TOP { <r> 'x' }; regex r { b [:m a+ { @log.push: $/.to }] 'c' } }
@log = ();
ok G2.parse("ba\x[301]acx"), 'a :m group between plain atoms';
is @log.join(','), '3', '... entering one end';

grammar G3 { regex TOP { <r> 'x' }; regex r { :m [ a | b ]+ { @log.push: $/.to } } }
@log = ();
ok G3.parse("a\x[301]bax"), 'a quantified alternation in a :m rule';
is @log.join(','), '3', '... entering one end';

grammar G4 { regex TOP { <r> 'x' }; regex r { b (:m a+ { @log.push: $/.to }) 'c' } }
@log = ();
my $m = G4.parse("ba\x[301]acx");
ok $m, 'a :m capture group';
is ~$m<r>[0], "a\x[301]a", '... captures the marked span';
is @log.join(','), '3', '... entering one end';

# Interpolations inside a whole-`:m` rule.
my @alts = <a aa>;
grammar I1 { regex TOP { <r> 'x' }; regex r { :m @alts { @log.push: $/.to } } }
@log = ();
ok I1.parse("a\x[301]ax"), '@alts in a :m rule over a marked subject';
is @log.join(','), '2', '... entering one end';

my $lit = 'a';
grammar I2 { regex TOP { <r> 'x' }; regex r { :m $lit+ { @log.push: $/.to } } }
@log = ();
is ~I2.parse("a\x[301]a\x[300]ax")<r>, "a\x[301]a\x[300]a", '$lit+ compares bases';
is @log.join(','), '3', '... entering one end';
