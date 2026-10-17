use Test;

# A routine's own variable -- a parameter, or a `my` of any sigil -- takes its
# type from its own declaration, never from a same-named typed variable of a
# routine that called it, whichever form the store takes and whichever nested
# routine, closure or block makes it (ADR-13757). Checked against rakudo.

plan 16;

sub g($v is rw) { $v = 'str' }
multi f(Seq:D $got) { f($got.cache) }
multi f(Mu $got is copy) { g($got); $got }
is f((1, 2).Seq), 'str', 'an is-rw callee of a Mu copy ignores a caller Seq:D';

sub outer-copy(Int $x) { mid-copy('s') }
sub mid-copy(Mu $x is copy) { $x = 'str'; $x }
is outer-copy(1), 'str', 'a Mu copy parameter ignores a caller Int';

sub outer-closure(Int $x) { mid-closure('s') }
sub mid-closure(Mu $x is copy) { my &c = { $x = 'str' }; c(); $x }
is outer-closure(1), 'str', 'a closure over a Mu copy parameter';

sub outer-for(Int $n) { mid-for('s') }
sub mid-for(Mu $n is copy) { for ^1 { $n = 'str' }; $n }
is outer-for(1), 'str', 'a loop body storing a Mu copy parameter';

sub outer-map(Int $n) { mid-map('s') }
sub mid-map(Mu $n is copy) { (^1).map({ $n = 'str' }).eager; $n }
is outer-map(1), 'str', 'a map body storing a Mu copy parameter';

sub outer-arr(Int @a) { mid-arr() }
sub mid-arr() { my @a; @a[0] = 'str'; my &c = { @a.push('p') }; c(); @a.join(',') }
my Int @ints = 1;
is outer-arr(@ints), 'str,p', 'an untyped my @a ignores a caller Int @a parameter';

sub outer-nested() { my Int @a; mid-nested() }
sub mid-nested() { my @a; my sub inner() { @a = 'x', 'y'; @a.join(',') }; inner() }
is outer-nested(), 'x,y', 'a my sub assigning its outer my @a';

sub outer-hash() { my Int %h; mid-hash() }
sub mid-hash() { my %h; my sub inner() { %h = a => 'x'; %h<a> }; inner() }
is outer-hash(), 'x', 'a my sub assigning its outer my %h';

sub outer-key() { my %h{Int}; mid-key() }
sub mid-key() { my %h; %h{1} = 'str'; %h.keys[0].^name }
is outer-key(), 'Str', 'an untyped my %h ignores a caller key type';

sub outer-scalar() { my Int $x = 1; mid-scalar() }
sub mid-scalar() { my $x; my sub inner() { $x = 'str' }; inner(); $x }
is outer-scalar(), 'str', 'a my sub storing its outer untyped my $x';

sub outer-eval() { my Int $x = 1; mid-eval() }
sub mid-eval() { my $x; my sub inner() { EVAL q[$x = 'str'] }; inner(); $x }
is outer-eval(), 'str', 'an EVAL in a my sub storing its outer my $x';

sub outer-push() { my Int @a; mid-push() }
sub mid-push() {
    my @a;
    my sub inner() { @a.push('x'); @a.unshift('w'); @a.append('y'); @a.join(',') }
    inner()
}
is outer-push(), 'w,x,y', 'push, unshift and append through a my sub';

sub outer-incr() { my Str $x = 'a'; mid-incr() }
sub mid-incr() { my $x = 0; my sub inner() { $x++; $x += 2 }; inner(); $x }
is outer-incr(), 3, 'increments ignore a caller Str';

sub outer-native() { my int $x = 1; mid-native() }
sub mid-native() { my $x = 2**70; my sub inner() { $x++ }; inner(); $x }
is outer-native(), 2**70 + 1, 'an untyped scalar does not wrap like a caller int';

sub own-typed(Int $x is copy) { $x = 's' }
throws-like { own-typed(1) }, X::TypeCheck::Assignment,
    'a typed copy parameter still checks its own type';

sub own-typed-closure(Int $x is copy) { my &c = { $x = 's' }; c() }
throws-like { own-typed-closure(1) }, X::TypeCheck::Assignment,
    'and so does a closure storing it';
