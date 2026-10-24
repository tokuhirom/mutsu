use v6;
use Test;

# A repository's `.candidates` smartmatches the matchers a
# `CompUnit::DependencySpecification` SUBCLASS answers, overrides included, and
# reads its short-name from the subclass instance (mutsu#14870, Pakku::Spec's
# `:ver(* > 0.2)` / `:auth(/^zef/)` selectors).
plan 8;

my $dir = $*TMPDIR.add( 'depspec-subclass-' ~ $*PID );
END { run 'rm', '-rf', ~$dir if $dir.e }
$dir.add('lib').mkdir;
$dir.add('META6.json').spurt: '{"name":"Sel::Dist","ver":"0.2","auth":"zef:abc","api":"1","provides":{"Sel::Dist":"lib/Sel/Dist.rakumod"}}';
$dir.add('lib/Sel').mkdir;
$dir.add('lib/Sel/Dist.rakumod').spurt: 'unit module Sel::Dist;';

class Spec is CompUnit::DependencySpecification {
  has $.ver-selector;
  has $.auth-selector;
  method version-matcher { $!ver-selector.defined ?? $!ver-selector !! callsame }
  method auth-matcher    { $!auth-selector.defined ?? $!auth-selector !! callsame }
}

my $repo = CompUnit::Repository::FileSystem.new: prefix => ~$dir;

ok $repo.candidates( Spec.new( short-name => 'Sel::Dist' ) ), 'a subclass instance names its dist';
nok $repo.candidates( Spec.new( short-name => 'Other::Dist' ) ), 'and only that dist';
ok $repo.candidates( Spec.new( short-name => 'Sel::Dist', ver-selector => * > v0.1 ) ), 'a WhateverCode version selector accepts';
nok $repo.candidates( Spec.new( short-name => 'Sel::Dist', ver-selector => * > v5 ) ), 'and rejects';
ok $repo.candidates( Spec.new( short-name => 'Sel::Dist', ver-selector => v0.1..v1 ) ), 'a Version range selector accepts';
ok $repo.candidates( Spec.new( short-name => 'Sel::Dist', auth-selector => /^ 'zef:' / ) ), 'a regex auth selector accepts';
nok $repo.candidates( Spec.new( short-name => 'Sel::Dist', auth-selector => /^ 'cpan:' / ) ), 'and rejects';
ok $repo.candidates( Spec.new( short-name => 'Sel::Dist', ver => '0.1+' ) ), 'the inherited matchers still work';
