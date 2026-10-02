use Test;

# An element write to a package-qualified hash inside a routine persists in
# the package after the routine returns (#10901). Expected values are rakudo's.

plan 10;

sub bump() { %GLOBAL::h<a>++ }
bump();
bump();
is %GLOBAL::h.raku, '${:a(2)}',
    'an undeclared qualified hash persists as an itemized package scalar';

sub pre-bump() { ++%GLOBAL::c<k> }
pre-bump();
pre-bump();
is %GLOBAL::c<k>, 2, 'prefix increment accumulates across calls';

sub store() { %GLOBAL::s<z> = 3 }
store();
is %GLOBAL::s.raku, '${:z(3)}', 'an element assignment survives the routine';

sub concat() { %GLOBAL::t<k> ~= 'ab' }
concat();
concat();
is %GLOBAL::t<k>, 'abab', 'a compound element assignment accumulates';

sub nested() { %GLOBAL::n<a><b> = 1 }
nested();
is %GLOBAL::n.raku, '${:a(${:b(1)})}', 'a nested element assignment persists';

sub fill() { %GLOBAL::d<a> = 1; %GLOBAL::d<b> = 2 }
sub drop() { %GLOBAL::d<a>:delete }
fill();
drop();
is %GLOBAL::d.raku, '${:b(2)}', 'a :delete in another routine sees the stored hash';

sub slice() { %GLOBAL::sl<a b> = 1, 2 }
slice();
is %GLOBAL::sl.raku, '${:a(1), :b(2)}', 'a slice assignment persists';

%GLOBAL::m<a>++;
is %GLOBAL::m.raku, '${:a(1)}', 'an increment at file scope vivifies the hash';

our %o;
sub via-global() { %GLOBAL::o<q>++ }
via-global();
is %o.raku, '{:q(1)}', 'a declared our hash shares the qualified write';

package P { our %h; our sub show() { %h.raku } }
sub via-package() { %P::h<x> = 1 }
via-package();
is P::show(), '{:x(1)}', "a write through %P::h reaches the package's own %h";
