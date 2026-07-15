use v6;
use Test;

# Pins for integration/error-reporting.t items fixed together (tests 21/25/30
# of that file): return-signal propagation out of `say`-forced lazy Seqs,
# the `%::{''}` undeclared bare variable, and compile-time rejection of
# inheriting from a type-capture parameter.

plan 6;

# --- say must not swallow a return signal raised while forcing a lazy Seq ---
{
    my $code = 'sub foo { (1, 2).map({ if ($_ > 0) { return $_ } else { return } }) }; say foo';
    my $p = run $*EXECUTABLE, '-e', $code, :err, :out;
    isnt $p.exitcode, 0, 'say of a return-throwing lazy Seq dies';
    like $p.err.slurp(:close), rx:i/Attempt\sto\sreturn\soutside\N+Routine.*in\sblock/,
        'the error is X::ControlFlow::Return with a backtrace';
}

# --- %::{''} is the undeclared bare variable '%' ---
throws-like ‘%::{''}’, X::Undeclared, line => /^\d+$/,
    Q|%::{''} throws X::Undeclared with a line|;

{
    my %h = a => 1;
    is %::h<a>, 1, '%::name is the root-namespace-qualified form of %name';
}

# --- inheriting from a type capture is a compile-time error ---
throws-like '-> ::TC129906 { class :: is TC129906 {} }',
    X::Inheritance::Unsupported, message => /TC129906/,
    'class :: is <type-capture> dies at compile time';
throws-like 'sub f(::T $x) { class C-tc is T {} }',
    X::Inheritance::Unsupported, message => /'C-tc'/,
    'named class inheriting a sub type capture dies at compile time';

done-testing;
