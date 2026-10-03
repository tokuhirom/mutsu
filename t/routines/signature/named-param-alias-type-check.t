use Test;

# An aliased named parameter's type applies whichever of its names the caller
# uses: `Int :l(:$level)` rejects `level => "foo"` as well as `l => "foo"`.
# Found via the CSS::Minifier distribution's CLI (`Int :l(:$level) = 2`).

plan 6;

sub f(Int :l(:$level) = 2) { $level }

is f(level => 5), 5, 'the inner name binds a matching value';
is f(l => 6), 6, 'the outer name binds a matching value';
is f(), 2, 'the default still applies';
throws-like { f(level => 'foo') }, X::TypeCheck::Binding::Parameter,
    'the inner name is type-checked';

sub g(Int() :l(:$level)) { $level.^name }
is g(level => '7'), 'Int', 'a coercion type coerces through the inner name';

sub h(:a(:b(:$c))) { $c }
is h(b => 3), 3, 'an untyped nested alias still binds';
