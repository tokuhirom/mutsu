use v6;
use Test;

# Regression (#7954, the `expected statement ...` parse-failure index):
# `let`/`temp` are statement prefixes over an assignment, and three spellings of
# one were missing. `Data::Record::Map` writes all three:
#
#     MapIterator.new(THIS, 'bounded', WRAP, 'push', let %!record .= push: @values)
#
# 1. The variable may be an ATTRIBUTE. `let $!x` was not recognized at all, so it
#    came back as the bareword `let` followed by an ordinary assignment -- the
#    save silently dropped -- and the term form `(let %!record)` was a hard parse
#    error. `temp $!x` was worse: the `$!` was read as the ERROR VARIABLE, so
#    `temp $!x = 2` assigned to a stray lexical `x` and the attribute was never
#    written.
# 2. The assignment may be a COMPOUND one (`.=`, `+=`). Only plain `=` had a
#    branch, so the `.= push` that followed became a *topic* dot-assign and the
#    container was left untouched.
# 3. The whole thing may stand as a TERM, including as an argument. Only the
#    parenthesized form `(let $x = 5)` was a term, and that path demands the `)`
#    immediately after.
#
# Every assertion below is rakudo's own answer.

plan 18;

# 1. An attribute is as temporizable as a lexical, and `temp` restores it.
class Scalar-attr {
    has $.x is rw = 1;
    method write-and-restore {
        my @seen;
        { temp $!x = 2; @seen.push: $!x }
        @seen.push: $!x;
        @seen.push: self.x;
        @seen;
    }
}
is Scalar-attr.new.write-and-restore, [2, 1, 1],
        'temp $!x writes the attribute and restores it at scope exit';

class Hash-attr {
    has %.r is rw;
    method push-in-scope {
        my @seen;
        %!r<z> = 0;
        { temp %!r .= push: (a => 1); @seen.push: %!r.keys.sort.join(',') }
        @seen.push: %!r.keys.sort.join(',');
        @seen;
    }
}
is Hash-attr.new.push-in-scope, ['a,z', 'z'],
        'temp %!r .= push temporizes the attribute container';

class Array-attr {
    has @.a is rw = [1];
    method swap {
        my @seen;
        { temp @!a = [9, 9]; @seen.push: @!a.elems }
        @seen.push: @!a.elems;
        @seen;
    }
}
is Array-attr.new.swap, [2, 1], 'temp @!a restores the array attribute';

# The term form of `let` on an attribute -- the shape `Data::Record::Map` uses.
class Term-form {
    has %.r is rw;
    method as-term { (let %!r).WHAT.gist }
    method push-as-term { (let %!r .= push: (a => 1)).gist }
}
is Term-form.new.as-term, '(Hash)', '(let %!r) is a term';
is Term-form.new.push-as-term, '{a => 1}', '(let %!r .= push: ...) is a term and pushes';

# 2. A compound assignment after `let`/`temp`, on an ordinary lexical.
my %r;
%r<z> = 0;
my @in-scope;
{ temp %r .= push: (a => 1); @in-scope.push: %r.keys.sort.join(',') }
is @in-scope[0], 'a,z', 'temp %h .= push runs the push';
is %r.keys.sort.join(','), 'z', '... and restores the hash at scope exit';

my $n = 1;
my @n-seen;
{ temp $n += 4; @n-seen.push: $n }
is @n-seen[0], 5, 'temp $x += 4 applies the compound assignment';
is $n, 1, '... and restores the scalar';

my $l = 1;
{ let $l += 4 }
is $l, 5, 'let keeps the value when the block exits successfully';

# 3. `let`/`temp` as a TERM in argument position.
sub capture(|c) { c.raku }
my $arg = 1;
is capture(0, let $arg = 5), '\\(0, 5)', 'let is a term in an argument list';
is $arg, 5, '... and the assignment really happened';

my %pushed;
is capture(0, let %pushed .= push: (a => 1)), '\\(0, {:a(1)})',
        'let with a compound assignment is a term in an argument list';

my $t = 1;
is capture(0, temp $t = 9), '\\(0, 9)', 'temp is a term in an argument list too';

# A `let`/`temp` term must not swallow the statement TERMINATOR: the statement
# parser consumes the `;`, so without putting it back the enclosing LISTOP keeps
# reading its argument list across it (`undefine temp $b; say 2` became
# `undefine(temp $b say 2)`).
my $term = 1;
my @log;
push @log, temp $term = 9; push @log, 2;
is @log.join(','), '9,2', 'a temp term does not swallow the statement terminator';
my $lterm = 1;
my @llog;
push @llog, let $lterm = 9; push @llog, 2;
is @llog.join(','), '9,2', '... nor does a let term';

# The plain forms are unchanged, including the error variable `$!` (which is
# what `temp $!x` was being mistaken for).
my $plain = 1;
{ temp $plain = 5 }
is $plain, 1, 'temp on a plain lexical still restores';
try { die 'boom' };
is $!.message, 'boom', '$! is still the error variable';
