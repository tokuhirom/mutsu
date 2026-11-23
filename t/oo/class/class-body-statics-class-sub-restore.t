# A class-scoped sub sees its class's `my` statics over a same-named caller
# lexical, and the caller's binding is put back after the call (#18274).
use Test;
class K {
    my $n = 10;
    my @list = 1, 2;
    my Int %typed;
    our sub peek { $n }
    our sub bump { $n++; @list.push($n); %typed<k> = $n; $n }
    our sub size { @list.elems }
    our proto multi-peek(|) {*}
    our multi multi-peek(Int $x) { $n + $x }
    our multi multi-peek(Str $s) { $s ~ $n }
    method via-sub { peek() }
}
my $n = 'caller';
my @list = 'caller-list';
is K::peek(), 10, 'a class-scoped sub reads its static, not the caller lexical';
is $n, 'caller', 'the caller lexical is restored';
is K::bump(), 11, 'a class-scoped sub writes its static';
is K::peek(), 11, 'and the write persists';
is K::size(), 3, 'an array static';
is @list.join, 'caller-list', 'the caller array is restored';
is K::multi-peek(1), 12, 'a multi candidate reads the static';
is K::multi-peek('s'), 's11', 'another candidate too';
is $n, 'caller', 'the caller lexical is still restored';
is K.via-sub, 11, 'a method calling the class sub';
throws-like { K::bump() for ^1; my Int %h = k => 'x' }, X::TypeCheck, 'typed hash checks still apply elsewhere';
done-testing;
