use Test;

plan 20;

# A BEGIN nested in a routine runs at compile time (ADR-0134, slice 2), even
# when it calls a routine of its scope that is a multi, an `our` or exported
# sub, or an operator, or reaches the scope's names through EVAL or a symbolic
# lookup (#10395). None of the enclosing routines is ever called here, so each
# value was stored by the lifted BEGIN alone.

my ($multi, $both, $proto, $outer-multi, $our, $export);
sub with-multi { multi sub helper(Int) { 'int' }; BEGIN $multi = helper(5) }
is $multi, 'int', 'a multi candidate of the scope';

sub with-candidates {
    multi sub helper(Int) { 'i' }
    multi sub helper(Str) { 's' }
    BEGIN $both = helper('x') ~ helper(1);
}
is $both, 'si', 'every candidate of the scope';

sub with-proto {
    proto sub helper($) { 'p' ~ {*} }
    multi sub helper(Int) { 'i' }
    BEGIN $proto = helper(1);
}
is $proto, 'pi', 'the proto of the scope dispatches to its candidates';

multi sub shared(Str) { 'unit' }
sub with-outer-candidate {
    multi sub shared(Int) { 'inner' }
    { multi sub shared(Rat) { 'block' }; BEGIN $outer-multi = (shared('x'), shared(1), shared(0.5)).join(',') }
}
is $outer-multi, 'unit,inner,block', 'candidates of the unit and of every enclosing scope';

sub with-our { our sub helper { 'our' }; BEGIN $our = helper() }
is $our, 'our', 'an `our sub`';

sub with-export { sub helper is export { 'export' }; BEGIN $export = helper() }
is $export, 'export', 'an exported sub';

my ($op, $assoc);
sub with-operator {
    my $k = 1;
    sub infix:<plus>($a, $b) { ($a + $b) ~ $k.raku }
    BEGIN $op = 1 plus 2;
}
is $op, '3Any', 'an operator used through its syntax sees the static value of what it closes over';

sub with-assoc {
    sub infix:<cat>($a, $b) is assoc<right> { "($a$b)" }
    BEGIN $assoc = 1 cat 2 cat 3;
}
is $assoc, '(1(23))', 'an operator keeps its associativity';

my ($eval, $eval-var, $method-eval, $symbolic);
sub with-eval { sub helper { 'eval' }; BEGIN $eval = EVAL 'helper()' }
is $eval, 'eval', 'EVAL reaches a routine of the scope';

sub with-eval-var($p) {
    my $x = 1;
    sub helper { $x }
    BEGIN $eval-var = EVAL q[(helper(), $p, $x).map(*.raku).join(',')];
}
is $eval-var, 'Any,Any,Any', 'EVAL sees the static state of the scope';

sub with-method-eval { multi sub helper(Str) { 'm' }; BEGIN $method-eval = q[helper('x')].EVAL }
is $method-eval, 'm', '.EVAL reaches a multi of the scope';

sub with-symbolic { sub sym-helper { 'sym' }; BEGIN $symbolic = &::('sym-helper').() }
is $symbolic, 'sym', 'a symbolic lookup reaches a routine of the scope';

# A token, rule or regex of the scope is declared again in the lifted block,
# whether or not the body seems to use it: a regex reaches it as a subrule
# (#13096).
my ($token, $rule, $after-token);
sub with-token { my token t { x+ }; BEGIN $token = so 'axxb' ~~ /<t>/ }
ok $token, 'a token of the scope, used as a subrule';

sub with-rule { my token d { \d+ }; my rule p { <d> '+' <d> }; BEGIN $rule = ~('1 + 2' ~~ /<p>/) }
is $rule, '1 + 2', 'a rule calling a token of the scope';

sub declares-token { my token u { y }; BEGIN 1 }
sub after-token { BEGIN $after-token = 'lifted' }
is $after-token, 'lifted', 'a scope declaring a token no longer keeps later BEGINs in place';

# A token whose pattern runs code or interpolates a variable closes over the
# scope's lexicals, which the lifted block supplies as it does for a routine:
# the variables the code blocks read and the ones the pattern interpolates.
my ($code-token, $code-count, $interp-token, $array-token, $quant-token, $after-code-token);
sub with-code-token {
    my $c = 0;
    my token ct { x { $c++ } };
    BEGIN { $code-token = so 'xx' ~~ /<ct>+/; $code-count = $c }
}
is-deeply ($code-token, $code-count), (True, 2), 'a token code block updates a lexical of the scope';

sub with-interp-token {
    my $w; BEGIN $w = 'ab';
    my token it { $w <?{ $w.chars == 2 }> };
    BEGIN $interp-token = so 'zab' ~~ /<it>/;
}
ok $interp-token, 'a token interpolating a lexical, and asserting on it';

sub with-array-token {
    my @alts; BEGIN @alts = <p q>;
    my regex ar { @alts+ };
    BEGIN $array-token = ~('ppq' ~~ /<ar>/);
}
is $array-token, 'ppq', 'a regex interpolating an array of the scope';

sub with-quant-token {
    my $n; BEGIN $n = 2;
    my token qt { x ** {$n} };
    BEGIN $quant-token = ~('xxx' ~~ /<qt>/);
}
is $quant-token, 'xx', 'a quantifier block reading a lexical of the scope';

sub declares-code-token { my token cu { y { 1 } }; BEGIN 1 }
sub after-code-token { BEGIN $after-code-token = 'lifted' }
is $after-code-token, 'lifted', 'a scope declaring a token with a code block no longer keeps later BEGINs in place';
