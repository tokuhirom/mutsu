use Test;

# A `:my $*x = ...;` inside a group of a rule is declared at the rule's entry
# (the uninitialised `Any`) but initialised when the group is reached: it
# must not be set when the group never matches, and the rules the group calls
# see the value (#16307, Intl::Format::Number's `$*negative`). Checked against
# rakudo.

plan 9;

my @seen;
grammar G {
    token TOP { <a> [ ';' :my $*neg = True; <b> ]? }
    token a { 'x' { @seen.push: "a:" ~ $*neg.raku } }
    token b { 'y' { @seen.push: "b:" ~ $*neg.raku } }
}

G.parse('x');
is-deeply @seen, ['a:Any'], 'an unreached nested :my is the uninitialised Any';

@seen = ();
G.parse('x;y');
is-deeply @seen, ['a:Any', 'b:Bool::True'],
    'the reached group initialises it, and the called rule sees the value';

@seen = ();
grammar H {
    token TOP { <a> [ ';' :my @*l = 1, 2; <b> ]? }
    token a { 'x' }
    token b { 'y' { @seen.push: @*l.raku } }
}
H.parse('x;y');
is-deeply @seen, ['[1, 2]'], 'an array declaration is shaped as an Array';

# The positive branch is guarded by the (unset) negative flag, as in
# Intl::Format::Number's pattern grammar.
grammar P {
    token TOP {
        :my %*d = n => 0, hg => 0;
        <pats>
        { make %*d }
    }
    token pats { <pat> [ ';' :my $*neg = True; <pat> ]? }
    token pat { <el>+ }
    token el {
        [ <?{$*neg}> <[#0..9]>+
        | [ '#' <.dd> | <idigit> ]+
          <.hg>
        ]
    }
    method dd { %*d<n>++; self }
    method hg { %*d<hg> = 1; self }
    token idigit { <[0..9]> { } <.hi($/.Str.Int)> }
    method hi($d) { %*d<n> = %*d<n> * 10 + $d; self }
}
my %r = P.parse('#00').made;
is %r<n>, 100, 'the positive pattern runs when the negative branch is not reached';
is %r<hg>, 1, 'methods called after it run';

%r = P.parse('#00;#').made;
ok %r<n> >= 100, 'a pattern with a negative part still parses';

# A top-level declaration is initialised at the rule's entry, once.
my $inits = 0;
sub bump { $inits++; 5 }
grammar T {
    token TOP { :my $*top = bump(); <t> }
    token t { 'z' { @seen.push: $*top } }
}
@seen = ();
T.parse('z');
is $inits, 1, 'a top-level :my initializer runs once';
is-deeply @seen, [5], '...and its value is visible';

grammar U {
    token TOP { [ 'q' :my $*q = 3; <u> ] }
    token u { 'r' { @seen.push: $*q } }
}
@seen = ();
U.parse('qr');
is-deeply @seen, [3], 'a group that is the whole rule initialises it';
