use Test;

# ADR-0134 slice 2 (#10394): a BEGIN nested in a scope that declares a type, a
# package, an import or a code variable ahead of it is still lifted to BEGIN
# time. It runs once, before the unit's run time, whether or not the enclosing
# scope ever runs. A type it names is the type the scope declares, and an
# import it relies on is in effect.

BEGIN plan 18;

my @log;

sub after-class { my class K { method m { 'k' } }; BEGIN @log.push('class') }
sub after-package { my $x; package P { }; BEGIN @log.push('package') }
sub after-use { use Test; BEGIN @log.push('use') }
sub after-code-var { my &g = { 1 }; BEGIN @log.push('code-var') }
is @log.join(','), 'class,package,use,code-var',
    'a BEGIN after a type, package, import or code variable runs though its scope never does';

my $name;
sub names-type { my class K { }; BEGIN $name = K.^name }
is $name, 'K', 'a BEGIN may name a lexical class declared ahead of it';

sub same-type { my class K { }; my $t = BEGIN K; $t === K }
ok same-type(), 'the type it sees is the type the scope declares';
ok same-type(), '... on every entry of the scope';

my $made;
sub makes { my class K { has $.v = 3; method m { 'm' ~ $!v } }; BEGIN $made = K.new.m }
is $made, 'm3', 'it may construct the class and call its methods';

my $inherited;
sub inherits { my class B { method x { 'bx' } }; my class K is B { }; BEGIN $inherited = K.x }
is $inherited, 'bx', 'a class it names brings the class it inherits from';

my $enum;
sub enums { my enum E <a b>; BEGIN $enum = b.value }
is $enum, 1, 'an enum key declared ahead of it';

my $subset;
sub subsets { my subset Big of Int where * > 2; BEGIN $subset = 3 ~~ Big }
ok $subset, 'a subset declared ahead of it';

my $pkg;
sub packages { package Q { our sub x { 7 } }; BEGIN $pkg = Q::x() }
is $pkg, 7, 'a routine of a package declared ahead of it';

my $outer-k;
my class OK { method n { 'outer' } }
sub shadows { my class OK { method n { 'inner' } }; BEGIN $outer-k = OK.n }
is $outer-k, 'inner', 'a type of the scope shadows a unit-level one';

my $tested;
sub imports { use Test; BEGIN $tested = &is-deeply.name }
is $tested, 'is-deeply', 'an import of the scope is in effect';

my $code-var;
sub code-var { my &g; BEGIN &g = { 7 }; g() }
is code-var(), 7, 'a BEGIN assigns a code variable of the scope';
is code-var(), 7, '... which every entry of the scope starts from';

my $code-type;
sub code-type { my &g = { 5 }; BEGIN $code-type = &g.^name }
is $code-type, 'Callable', 'a code variable is in its static state';

my $count;
sub counted { my class K { }; BEGIN $count++ }
counted(); counted();
is $count, 1, 'such a BEGIN does not run again when its scope does';

my $with-sub;
sub with-sub { sub h { 2 }; my class K { }; BEGIN $with-sub = K.^name ~ h() }
is $with-sub, 'K2', 'a routine and a type declared ahead of it together';

# A class whose body runs code is not declared again ahead of its scope. A
# BEGIN that does not name it is lifted all the same; one that names it keeps
# running in place (and, since that is later than BEGIN time, so does every
# BEGIN after it in the unit).
my @body;
my $after;
sub not-named { my class K { @body.push('body') }; BEGIN $after = 'lifted' }
is $after, 'lifted', 'a BEGIN that does not name such a class is lifted';

sub runs-body { my class K { @body.push('body') }; BEGIN K }
is @body.elems, 0, 'a class body with a run-time statement does not run at BEGIN time';
