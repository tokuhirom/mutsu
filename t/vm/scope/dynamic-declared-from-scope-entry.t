use Test;
use lib 't/lib';
use DynamicProtoNestedDecl;

# A scope's `my $*x` shadows the callers' binding from the scope's entry,
# unset until its declaration runs, and a dynamic declared in a nested block
# that has not been entered shadows nothing (#19174). `$*x` and
# `DYNAMIC::<$*x>` resolve alike. Checked against rakudo.
plan 12;

# Red's `red-do` shape: the proto's nested block declares the dynamic.
is-deeply (gather f { f { 1 } }).List, ('none none', '5 5'),
    'a proto whose nested block declares the dynamic sees the caller\'s';

sub r()  { DYNAMIC::<$*X> // 'none' }
sub r2() { $*X // 'none' }
sub outer(&c) { my $*X = 'outer'; c() }

my @got;
outer { @got.push: r() ~ ' ' ~ r2(); my $*X = 'inner'; @got.push: r() ~ ' ' ~ r2() };
is @got[0], 'none none', 'a block\'s own pending declaration shadows the caller\'s';
is @got[1], 'inner inner', 'the block\'s own declaration after it runs';

sub own()   { my @r = r() ~ ' ' ~ r2(); my $*X = 'own'; @r.push: r() ~ ' ' ~ r2(); @r }
sub calls() { my $*X = 'caller'; own() }
is-deeply calls(), ['none none', 'own own'], 'a routine\'s pending declaration';

my $*X = 'main';
sub dormant() { my $a = r() ~ ' ' ~ r2(); { my $*X = 'blk'; $a ~= ' / ' ~ r() ~ ' ' ~ r2() }; $a }
is dormant(), 'main main / blk blk', 'a nested block shadows only while it runs';
sub dormant-later() { my $a = r(); if False { my $*X = 'never' }; $a ~ ' ' ~ r() }
is dormant-later(), 'main main', 'a branch that does not run shadows nothing';
sub in-block() { my $a; { $a = r() ~ ' ' ~ r2(); my $*X = 'blk' }; $a }
is in-block(), 'none none', 'a running block\'s pending declaration';
sub in-if-value() { my $a; if True { $a = r() ~ ' ' ~ r2(); my $*X = 'if' }; $a }
is in-if-value(), 'none none', 'an if branch\'s pending declaration';
sub in-try() { my $a; try { $a = r() ~ ' ' ~ r2(); my $*X = 'try' }; $a }
is in-try(), 'none none', 'a try block\'s pending declaration';
sub in-loop() { my $a; for 1 { $a = r() ~ ' ' ~ r2(); my $*X = 'for' }; $a }
is in-loop(), 'none none', 'a loop body\'s pending declaration';
is r(), 'main', 'no scope leaks its declaration';

sub typed-r() { DYNAMIC::<$*T>.raku }
sub typed() { my $a = typed-r(); my Int $*T = 5; $a ~ ' ' ~ typed-r() }
is typed(), 'Int 5', 'a pending typed declaration holds its type object';
