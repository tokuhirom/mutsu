use v6;
use Test;

# Regression (AttrX::Mooish): a private method called on `$proxy.VAR` runs with
# the Proxy container as `self`; when it returns `self`, a later assignment
# FETCHes (rakudo) instead of storing the Proxy object itself.

plan 5;

class B1 is Proxy {
    has $.instance;
    method !SET(%a) is raw { $!instance := %a<instance>; self }
    method new(:$FETCH, :$STORE, *%a) is raw {
        callwith(:$FETCH, :$STORE).VAR!SET(%a)
    }
}
my $a = B1.new(:instance(1), FETCH => -> $ { 7 }, STORE => -> $, $ { });
is $a.^name, 'Int', 'assignment fetched through the returned container';
is $a, 7, 'the fetched value';
my $b = $a;
is $b + 1, 8, 'copy is a plain value';

class B3 is Proxy {
    method new(:$FETCH, :$STORE) is raw { callwith(:$FETCH, :$STORE).VAR }
}
my $c = B3.new(FETCH => -> $ { 7 }, STORE => -> $, $ { });
is $c.^name, 'B3', 'a bare .VAR result is still the unfetched Proxy object';

class B4 is Proxy {
    method !SET is raw { self }
    method new(:$FETCH, :$STORE) is raw { callwith(:$FETCH, :$STORE).VAR!SET }
}
my $d = B4.new(FETCH => -> $ { 9 }, STORE => -> $, $ { });
is $d, 9, 'no attributes needed';
