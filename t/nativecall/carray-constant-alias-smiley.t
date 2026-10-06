use v6;
use Test;
use lib 't/lib';
use CArrayAliasMod;

# `CArray` here is an imported `my constant` alias of a namespaced class, as in
# the upstream NativeCall module. A definiteness smiley on the alias used to
# compare the instance's qualified class name against the bare spelling and
# reject it (#12121).

plan 8;

my $a = CArrayAliasMod::Types::CArray.new;

sub bare(CArray $x)   { 1 }
sub def(CArray:D $x)  { 1 }
sub undef(CArray:U $x) { 1 }
sub qualified(CArrayAliasMod::Types::CArray:D $x) { 1 }

ok $a ~~ CArray, 'instance smartmatches the alias';
ok $a ~~ CArray:D, 'and the alias with :D';
nok $a ~~ CArray:U, 'but not with :U';
is bare($a), 1, 'a bare alias parameter binds';
is def($a), 1, 'an alias:D parameter binds';
is qualified($a), 1, 'the qualified :D parameter binds';
nok (try undef($a)).defined, 'an alias:U parameter rejects the instance';
is undef(CArray), 1, 'an alias:U parameter binds the type object';

# vim: expandtab shiftwidth=4
