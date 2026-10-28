use Test;

# `my %h does R = ...`: the declaration's own initializer is the container's
# STORE called with :INITIALIZE; a later plain assignment passes nothing.
# Found with WriteOnceHash (t/01-basic.rakutest).

plan 8;

my @seen;
role R {
    method STORE(\it, :$INITIALIZE) {
        @seen.push($INITIALIZE.raku);
        callwith(it, :$INITIALIZE);
    }
}

my %a does R = 1 => "a";
is-deeply @seen, ['Bool::True'], 'hash declaration initializer passes :INITIALIZE';
is %a<1>, 'a', 'the value is stored';

@seen = ();
my %b{Any} does R = 42 => "a";
is-deeply @seen, ['Bool::True'], 'object hash declaration initializer passes :INITIALIZE';

@seen = ();
my @c does R = 1, 2;
is-deeply @seen, ['Bool::True'], 'array declaration initializer passes :INITIALIZE';
is-deeply @c.List, (1, 2), 'the array is filled';

@seen = ();
%a = 2 => "b";
@c = 3, 4;
is-deeply @seen, ['Any', 'Any'], 'a later plain assignment passes no :INITIALIZE';

@seen = ();
my %d does R;
%d = 5 => "z";
is-deeply @seen, ['Any'], 'a declaration without initializer, then assignment, passes none';
is %d<5>, 'z', 'and stores the value';
