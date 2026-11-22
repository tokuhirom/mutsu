use Test;

# #18591: the coercions `Nil` inherits from `Cool`, called with arguments,
# bind or refuse them as `Cool`'s candidates do; none is absorbed into Nil.

plan 12;

my @warned;
my @rounded;
{
    @rounded.push: Nil.round(1);
    @rounded.push: Nil.round(0.1);
    CONTROL { when CX::Warn { @warned.push: .message; .resume } }
}
is-deeply @rounded, [0, 0.0], 'round($scale) is 0 rounded to the scale';
is-deeply @warned, ['Use of Nil in numeric context' xx 2], '... after the numeric-context warning';

for <Num Rat abs floor sign ords> -> $m {
    throws-like { Nil."$m"(1) }, X::AdHoc,
        message => /'Too many positionals passed'/, "$m refuses a positional";
}
throws-like { Nil.Real(1) }, X::Multi::NoMatch, 'Real(1) fails the multi dispatch';
throws-like { Nil.Stringy(1) }, X::Multi::NoMatch, 'Stringy(1) fails the multi dispatch';
throws-like { Nil.round(1, 2) }, X::Multi::NoMatch, 'round with two arguments';
quietly is Nil.Num, 0e0, 'the zero-argument form still answers';
