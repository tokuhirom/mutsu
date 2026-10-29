use Test;

# From WriteOnceHash 0.0.9 (`my %owoh{Any} does WriteOnce = 42 => "a"`): the
# initializer of a `does` declaration is the declaration's own, so the
# composed role's `STORE` receives `:INITIALIZE`; a later assignment does not.

plan 6;

my @seen;
role W {
    method STORE(\it, :$INITIALIZE) {
        @seen.push: $INITIALIZE.so;
        callsame;
    }
}

my %a does W = a => 1;
is @seen, [True], 'declaration initializer passes :INITIALIZE';
is %a<a>, 1, 'the initializer was stored';

my %b{Any} does W = 42 => "a";
is @seen, [True, True], 'object hash declaration passes :INITIALIZE too';
is %b{42}, "a", 'object hash initializer was stored';

%a = b => 2;
is @seen, [True, True, False], 'a later assignment does not pass :INITIALIZE';

my %c does W;
%c = c => 3;
is @seen, [True, True, False, False], 'no initializer, no :INITIALIZE';
