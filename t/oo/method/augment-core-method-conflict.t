use Test;

# #10234: `augment class` of a core type may not redeclare a method that the
# type itself declares. A plain or `only` method cannot join any method of that
# name in the type's own method table (multi dispatcher or not); a `multi`
# candidate cannot join an `only` method. A name the type only inherits, or
# declares as a multi (for a `multi`), stays legal. Expected results are
# rakudo's.

plan 12;

# Each declaration runs in its own process: a failed augment leaves the core
# type half-augmented, which would leak into the next case.
class Outcome { has $.message }
sub compiles(Str $code) {
    my $p = run $*EXECUTABLE, '-e', "use MONKEY-TYPING; $code", :out, :err;
    my $err = $p.err.slurp(:close);
    $p.out.slurp(:close);
    $p.exitcode == 0 ?? Nil !! Outcome.new(message => $err)
}

like compiles('augment class Str { method uc { "x" } }').message,
    /"Package 'Str' already has a method 'uc' (did you mean to declare a multi method?)"/,
    'a plain method next to a declared multi';
like compiles('augment class Str { only method uc { "x" } }').message,
    /"already has a method 'uc'"/, 'an only method likewise';
like compiles('augment class Int { method Str { "x" } }').message,
    /"Package 'Int' already has a method 'Str'"/, 'another core type';
like compiles('augment class Str { multi method Int(Str:D:) { 1 } }').message,
    /"Cannot have a multi candidate for 'Int' when an only method is also in the package 'Str'"/,
    'a multi candidate next to a declared only method';

nok compiles('augment class Array { method sort { 1 } }'),
    'a name the type only inherits (Array.sort is List\'s)';
nok compiles('augment class Str { method FatRat { 1 } }'),
    'Str.FatRat is declared on Cool';
nok compiles('augment class Str { method brand-new-x { 1 } }'), 'a new name';
nok compiles('augment class Int { multi method Str(Int:D: Int $x) { "m" } }'),
    'a multi candidate next to a declared multi';
like compiles('augment class Str { proto method uc(|) {*} }').message,
    /"Package 'Str' already has a method 'uc'"/,
    '#11596: a proto method conflicts like a plain one';
nok compiles('augment class Str { proto method brand-new-p(|) {*}; multi method brand-new-p { 1 } }'),
    'a proto for a new name';
nok compiles('augment class Str { method !uc { "x" } }'),
    'a private method lives in its own namespace';

is run($*EXECUTABLE, '-e',
    'use MONKEY-TYPING; augment class Int { multi method Str(Int:D: Int $x) { "m" } }; print 1.Str(1)',
    :out).out.slurp(:close), 'm', 'the added multi candidate dispatches';
