use Test;

# A built-in exception class declares its attributes on first access, not at
# startup (#18956). Each case runs in a fresh process, so the operation it
# names is the class's first touch, and must see the same declaration the
# eager startup gave: accessors, built named arguments, `.can`, composition
# into a subclass, a method added before the attributes were read, and two
# threads racing on the first touch.

my @cases =
    'an accessor call',
        'say X::NYI.new(feature => "f").feature',
        "f\n",
    'a private attribute built from its named argument',
        'say X::AdHoc.new(payload => "p").payload',
        "p\n",
    '.can',
        'say X::OutOfRange.can("range").elems',
        "1\n",
    'a subclass declared first',
        'class E is X::NYI { has $.z }; my $e = E.new(feature => "a", z => 1); say $e.feature, $e.z',
        "a1\n",
    'a method added first',
        'X::NYI.^add_method("extra", method { 42 }); my $e = X::NYI.new(feature => "f"); say $e.feature, $e.extra',
        "f42\n",
    'a user class under X::',
        'class X::Mine is Exception { has $.a }; say X::Mine.new(a => 1).a',
        "1\n",
    'two threads at once',
        'say await (^2).map: { start X::Redeclaration.new(symbol => "s$_").symbol }',
        "(s0 s1)\n",
    '.^attributes',
        'say X::NYI.^attributes.map(*.name).grep(none("\$!ex", "\$!bt")).join(" ")',
        "\$!feature \$!did-you-mean \$!workaround\n";

plan @cases / 3;

for @cases -> $what, $code, $expected {
    my $proc = run $*EXECUTABLE, '-e', $code, :out, :err;
    my $out = $proc.out.slurp(:close);
    my $err = $proc.err.slurp(:close);
    is $out, $expected, "first touch by $what" or diag $err;
}
