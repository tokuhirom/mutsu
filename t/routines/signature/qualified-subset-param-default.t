use Test;

# From HTML::Canvas: `method createPattern(HTML::Canvas::Pattern::Repetition
# $repetition = 'repeat')`, where the package-qualified type is a subset. The
# parse-time default check took every `::` name for a class the default cannot
# satisfy; and the runtime check cut `X::AdHoc` down to `X` at its first `:`.
plan 8;

class Pat {
    subset Repetition of Str where 'repeat'|'repeat-x'|'no-repeat';
    subset Small of Str where { $_ eq 'a' | 'b' };
    subset Starts of Str where /^ 'r' /;
}

sub rep(Pat::Repetition $r = 'repeat') { $r }
sub small(Pat::Small $r = 'a') { $r }
sub starts(Pat::Starts $r = 'ra') { $r }
is rep(), 'repeat', 'qualified subset with a junction where, default accepted';
is small(), 'a', 'with a block where';
is starts(), 'ra', 'with a regex where';
is rep('no-repeat'), 'no-repeat', 'explicit argument still accepted';
throws-like { rep('sideways') }, X::TypeCheck::Binding::Parameter, 'and still checked';

class Meth { method create(Pat::Repetition $r = 'repeat') { $r } }
is Meth.new.create, 'repeat', 'a method parameter too';

# A default that cannot satisfy a qualified CLASS is still rejected at declaration.
throws-like 'sub f(X::AdHoc $p = "x") { }', X::Parameter::Default::TypeCheck,
    'qualified class, mismatched default';
throws-like 'sub f(Int $p = "x") { }', X::Parameter::Default::TypeCheck,
    'plain class, mismatched default';

# vim: expandtab shiftwidth=4
