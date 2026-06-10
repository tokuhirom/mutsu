use Test;

# Native default construction for typed `@`/`%` attributes (`has Int @.nums`).
# Provided and defaulted typed containers must carry element-type metadata
# (`.of` / `.WHAT` / `~~ Array[Int]`) and type-check their elements, matching
# the variable path `my Int @a` and raku.

plan 32;

# --- typed array, provided ---
class IntArr { has Int @.nums; }
my $a = IntArr.new(nums => [1, 2, 3]);
is $a.nums, [1, 2, 3], 'typed array attr keeps values';
is $a.nums.^name, 'Array[Int]', 'typed array attr .WHAT is Array[Int]';
is $a.nums.of.^name, 'Int', 'typed array attr .of is Int';
ok $a.nums ~~ Array[Int], 'typed array attr smartmatches Array[Int]';

# --- typed hash, provided ---
class IntHash { has Int %.map; }
my $h = IntHash.new(map => { a => 1, b => 2 });
is $h.map<a>, 1, 'typed hash attr keeps values';
is $h.map.^name, 'Hash[Int]', 'typed hash attr .WHAT is Hash[Int]';
is $h.map.of.^name, 'Int', 'typed hash attr .of is Int';

# --- uninitialized typed containers ---
my $u = IntArr.new();
is $u.nums.^name, 'Array[Int]', 'uninit typed array is Array[Int]';
is $u.nums.of.^name, 'Int', 'uninit typed array .of is Int';
is $u.nums.elems, 0, 'uninit typed array is empty';
my $uh = IntHash.new();
is $uh.map.^name, 'Hash[Int]', 'uninit typed hash is Hash[Int]';
is $uh.map.of.^name, 'Int', 'uninit typed hash .of is Int';

# --- list / range provided values coerce ---
my $lr = IntArr.new(nums => 1..3);
is $lr.nums, [1, 2, 3], 'range provided coerces to array';
is $lr.nums.^name, 'Array[Int]', 'range provided is Array[Int]';

# --- mixed: typed $, typed @, typed % in one class ---
class Mixed { has Int $.n; has Str @.words; has Int %.counts; }
my $m = Mixed.new(n => 5, words => <a b c>, counts => { x => 1 });
is $m.n, 5, 'mixed: scalar attr';
is $m.words, [<a b c>], 'mixed: typed array attr';
is $m.words.of.^name, 'Str', 'mixed: typed array .of is Str';
is $m.counts.of.^name, 'Int', 'mixed: typed hash .of is Int';

# --- inheritance: typed container in parent and child ---
class Base { has Int @.a; }
class Derived is Base { has Str %.h; }
my $d = Derived.new(a => [10, 20], h => { k => 'v' });
is $d.a.of.^name, 'Int', 'inherited typed array .of is Int';
is $d.h.of.^name, 'Str', 'child typed hash .of is Str';

# --- subset element type ---
subset Pos of Int where * > 0;
class PosArr { has Pos @.p; }
my $pa = PosArr.new(p => [1, 2, 3]);
is $pa.p.^name, 'Array[Pos]', 'subset element type .WHAT';
is $pa.p.of.^name, 'Pos', 'subset element type .of';

# --- push after construction preserves element type ---
class Pushy { has Int @.q; }
my $p = Pushy.new(q => [1]);
$p.q.push(2);
is $p.q, [1, 2], 'push after construction works';
is $p.q.^name, 'Array[Int]', 'pushed array keeps Array[Int]';

# --- bad element type dies (matches raku + variable path) ---
dies-ok { IntArr.new(nums => ['x']) }, 'bad array element type dies';
dies-ok { IntHash.new(map => { a => 'x' }) }, 'bad hash element type dies';
my $err;
{
    IntArr.new(nums => [1, 'x', 3]);
    CATCH { default { $err = .message } }
}
ok $err && $err.contains('element of @!nums'), 'error names the attribute';
ok $err && $err.contains('expected Int'), 'error names the expected type';

# --- native int element falls through (no element type-check) ---
class NativeArr { has int @.x; }
my $na = NativeArr.new(x => [1, 2, 3]);
is $na.x, [1, 2, 3], 'native int element array keeps values';

# --- loop construction (perf path stays correct) ---
my @built;
for ^5 -> $i {
    @built.push(IntArr.new(nums => [$i]).nums.of.^name);
}
is @built, ['Int', 'Int', 'Int', 'Int', 'Int'], 'loop construction preserves element type';

# --- empty typed containers provided explicitly ---
my $empty = IntArr.new(nums => []);
is $empty.nums.elems, 0, 'explicit empty array';
is $empty.nums.^name, 'Array[Int]', 'explicit empty array is Array[Int]';

# done
