use Test;

# #10962: a package-qualified `@`/`%` variable nobody has written is an empty
# Scalar slot in raku, so it reads as `Any` (an undeclared *lexical* under
# `no strict` is the one that defaults to an empty container).

plan 14;

is %GLOBAL::nv.raku, 'Any', 'never-written %GLOBAL:: hash reads as Any';
is @GLOBAL::nv.raku, 'Any', 'never-written @GLOBAL:: array reads as Any';
is %P::nv.raku,      'Any', 'never-written %P:: hash reads as Any';
is %GLOBAL::nv<a>.raku, 'Any', 'element read of it is Any';
is %GLOBAL::nv.elems, 1, '.elems of Any is 1';
nok %GLOBAL::nv.defined, 'it is undefined';
{
    my $n = 0;
    for @GLOBAL::nv { $n++ }
    is $n, 1, 'for iterates the Any once';
}
is (@GLOBAL::nv // 'dflt'), 'dflt', '// falls through';

# A mutator vivifies an itemized Array in the slot.
push @GLOBAL::p, 1;
is @GLOBAL::p.raku, '$[1]', 'push vivifies an itemized Array';
@GLOBAL::q.push(2);
is @GLOBAL::q.raku, '$[2]', '.push vivifies an itemized Array';
%GLOBAL::r.push((a => 1));
is %GLOBAL::r.raku, '$[:a(1)]', '.push on a % slot vivifies an itemized Array too';

# Writes keep working, from any frame.
%GLOBAL::w<a> = 1;
is %GLOBAL::w.raku, '${:a(1)}', 'element write stores an itemized Hash';
sub add { push @GLOBAL::k, 5 }
add(); add();
is @GLOBAL::k.raku, '$[5, 5]', 'a push from a routine persists';

module M { our @list = 1, 2 }
is @M::list.raku, '[1, 2]', 'a declared our array is unaffected';
