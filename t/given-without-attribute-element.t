use Test;

# `given %!h{$k}` / `with`/`without %!h{$k}` (and their statement-modifier
# forms) used an element-source writeback optimization that looked the
# container up by NAME in the locals store. An instance attribute (`%!h`,
# `@!a`) lives in the instance attribute store, not in locals, so the lookup
# read an empty container and bound `$_` to Nil — breaking the very common
# `return Nil without %!params{$k}` accessor idiom (Humming-Bird's
# Request.param / Request.query).

plan 8;

class R {
    has %.h;
    has @.a;
    method setup { %!h<name> = 'world'; @!a[0] = 'first'; }
    method via-given-hash       { given %!h<name> { return $_ } }
    method via-without-hash($k) { return 'MISSING' without %!h{$k}; %!h{$k} }
    method via-with-hash($k)    { my $r = 'NONE'; $r = 'got:' ~ %!h{$k} with %!h{$k}; $r }
    method via-given-array      { given @!a[0] { return $_ } }
}

my $r = R.new;
$r.setup;

is $r.via-given-hash,        'world', 'given on attr hash element (cross-method)';
is $r.via-without-hash('name'), 'world', 'without stmt-mod sees defined attr element';
is $r.via-without-hash('nope'), 'MISSING', 'without stmt-mod fires on undefined element';
is $r.via-with-hash('name'),  'got:world', 'with stmt-mod sees defined attr element';
is $r.via-given-array,        'first', 'given on attr array element';

# block forms also correct
class S {
    has %.p;
    method check($k) {
        %!p{$k} = 'X';
        my $out;
        without %!p{$k} { $out = 'undef' }
        with %!p{$k}    { $out = 'def' }
        $out;
    }
}
is S.new.check('a'), 'def', 'with block on attr element when defined';

# plain lexical hash element still works (unchanged behavior)
my %lex = :y(1);
my $z = 'NO';
$z = 'YES' with %lex<y>;
is $z, 'YES', 'with on lexical hash element';
my $w = 'NO';
$w = 'RAN' without %lex<missing>;
is $w, 'RAN', 'without on missing lexical hash element';
