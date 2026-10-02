use Test;

# An undeclared package-qualified `@`/`%` slot is an auto-created Scalar, as
# in rakudo: `=` item-assigns into it and an element write vivifies an
# itemized container there (#11000). Expected strings are rakudo's.

plan 14;

@GLOBAL::u = 1, 2;
is @GLOBAL::u.raku, '$(1, 2)', 'a list assignment stores the itemized List';
is @GLOBAL::u.elems, 2, 'which still counts its elements';

%GLOBAL::v = a => 1;
is %GLOBAL::v.raku, ':a(1)', 'a single Pair is stored as is';

%GLOBAL::w = a => 1, b => 2;
is %GLOBAL::w.raku, '$(:a(1), :b(2))', 'a list of pairs stays a List';

@GLOBAL::five = 5;
is @GLOBAL::five.raku, '5', 'a scalar value is stored as is';

@GLOBAL::r = 1..3;
is @GLOBAL::r.raku, '1..3', 'so is a Range';

my @a = 1, 2;
@GLOBAL::alias = @a;
@a.push(3);
is @GLOBAL::alias.raku, '$[1, 2, 3]', 'an Array is stored itemized, not copied';

my %h = x => 1;
%GLOBAL::hh = %h;
is %GLOBAL::hh.raku, '${:x(1)}', 'a Hash is stored itemized';

@GLOBAL::again = 1, 2;
@GLOBAL::again = 3, 4;
is @GLOBAL::again.raku, '$(3, 4)', 'a second assignment replaces the item';

@P::q = 3, 4;
is @P::q.raku, '$(3, 4)', 'any undeclared package slot, not only GLOBAL';

@GLOBAL::x[0] = 1;
is @GLOBAL::x.raku, '$[1]', 'an element write vivifies an itemized Array';

sub elem() { @GLOBAL::y[1] = 5 }
elem();
is @GLOBAL::y.raku, '$[Any, 5]', 'an element write in a routine persists';

package Q { our @a; our sub show() { @a.raku } }
@Q::a[0] = 7;
is Q::show(), '[7]', "a declared our @a keeps its plain Array";
is @Q::a.raku, '[7]', 'read back through the qualified name';
