use Test;
plan 27;

# List/Array.rotate, Any.splice, Array/Hash .name/.dynamic and List/Array.AT-POS
# are method-table rows (ADR-11276 §9.67); expectations checked against raku.

# rotate: not a mutator, answers a new Seq
my @a = 1, 2, 3;
is @a.rotate.raku, '(2, 3, 1).Seq', 'Array.rotate';
is @a.rotate(-1).raku, '(3, 1, 2).Seq', 'negative rotate';
is @a.rotate(5).raku, '(3, 1, 2).Seq', 'rotate wraps';
is @a.raku, '[1, 2, 3]', 'rotate leaves the array alone';
is (1, 2, 3).rotate(2).raku, '(3, 1, 2).Seq', 'List.rotate';
is ().rotate.raku, '().Seq', 'empty rotate';
is @a.rotate("2").raku, '(3, 1, 2).Seq', 'Str count';
throws-like { @a.rotate(1, 2) }, X::AdHoc, message => /'Too many positionals'/, 'two counts';
my @s[2;2];
throws-like { @s.rotate }, X::IllegalOnFixedDimensionArray, 'multi-dim shaped';

# Any.splice: the proto with no candidates, for everything but Array and Buf
sub no-cands(&c, $what) {
    try c();
    is $!.^name, 'X::Multi::NoMatch', "$what: X::Multi::NoMatch";
}
no-cands { 5.splice(0, 1) }, 'Int';
no-cands { "s".splice(0, 1) }, 'Str';
no-cands { (1..3).splice(0, 1) }, 'Range';
no-cands { {a => 1}.splice(0, 1) }, 'Hash';
no-cands { (1, 2).Seq.splice(0, 1) }, 'Seq';
no-cands { (1, 2).splice(0, 1) }, 'List';
no-cands { Any.splice(0, 1) }, 'type object';
try { 5.splice(0, 1) };
is $!.message, 'Cannot resolve caller splice(Int:D, Int:D, Int:D); Routine does not have any candidates.  Is only the proto defined?', 'message';
try { Int.splice(0, 1) };
is $!.message, 'Cannot resolve caller splice(Int:U, Int:D, Int:D); Routine does not have any candidates.  Is only the proto defined?', 'type object message';
class Mine { method splice(|) { 'mine' } }
is Mine.new.splice(1), 'mine', 'a user splice wins over the Any proto';
my @b = 1 .. 5;
is @b.splice(1, 2).raku, '[2, 3]', 'Array.splice still works';
my $buf = Buf.new(1, 2, 3);
is $buf.splice(1, 1).raku, 'Buf.new(2)', 'Buf.splice still works';

# name / dynamic read the container descriptor off the value
my %h = a => 1;
my $r = @a;
my @d is dynamic;
is @a.name, '@a', 'Array.name';
is %h.name, '%h', 'Hash.name';
is $r.name, '@a', 'the name travels with the value';
is [1, 2].name, 'element', 'an unnamed container is element';
is-deeply (@a.dynamic, @d.dynamic, %h.dynamic), (False, True, False), 'dynamic';

# AT-POS
my Int @i = 1;
is-deeply (@a.AT-POS(1), (1, 2, 3).AT-POS(2), @i.AT-POS(5)), (2, 3, Int), 'AT-POS';
