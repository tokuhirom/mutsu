use lib 'roast/packages/Test-Helpers/lib';
use Test;
use Test::Util;

# `dd` of a user-class instance dumps its `.raku` representation
# (`F.new(x => 3)`), not the type-object form `F()`. The value part needs the
# class registry to collect public attributes, so `dd` dispatches the instance
# `.raku` method rather than the static raku renderer.

plan 5;

is_run 'class F { has $.x }; dd F.new(:x(3))',
    { :err("F.new(x => 3)\n"), :out(""), :status(0) },
    'dd of an instance dumps ClassName.new(attr => value)';

is_run 'class F { has $.a; has $.b }; dd F.new(:a(1), :b(2))',
    { :err("F.new(a => 1, b => 2)\n"), :status(0) },
    'dd of a multi-attribute instance';

is_run 'class Inner { has $.x }; class Outer { has $.i }; dd Outer.new(:i(Inner.new(:x(7))))',
    { :err("Outer.new(i => Inner.new(x => 7))\n"), :status(0) },
    'dd renders nested instances recursively';

# non-instance values are unaffected
is_run 'dd [1, 2, 3]', { :err("[1, 2, 3]\n"), :status(0) }, 'dd of an array is unchanged';
is_run 'dd 42', { :err("42\n"), :status(0) }, 'dd of an Int is unchanged';
