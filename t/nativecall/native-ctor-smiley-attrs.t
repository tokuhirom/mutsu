use Test;

# Native default construction for classes whose attributes carry a definedness
# smiley (`:D` / `:U` / `:_`). These used to fall through to the interpreter;
# the native default constructor now enforces the smiley as a post-assembly
# phase (the same `enforce_attribute_smiley_constraints` the full path uses).

plan 21;

# --- :D with a default (the default makes it defined) ---
class CDef { has Int:D $.x = 9 }
is CDef.new.x, 9, ':D attr with a default constructs natively';
is CDef.new(:x(5)).x, 5, ':D attr accepts a provided defined value';

# --- :D is required ---
class CReq { has Int:D $.x is required }
is CReq.new(:x(7)).x, 7, ':D required attr with a provided value';
dies-ok { CReq.new }, ':D required attr without a value dies';

# --- :D provided an undefined value dies ---
class CUndef { has Int:D $.x = 1 }
dies-ok { CUndef.new(:x(Int)) }, ':D attr rejects a provided type object';

# --- :U ---
class CU { has Int:U $.x }
nok CU.new.x.defined, ':U attr defaults to a type object (undefined)';
dies-ok { CU.new(:x(5)) }, ':U attr rejects a provided defined value';

# --- :_ (no definedness constraint) ---
class CAny { has Int:_ $.x }
nok CAny.new.x.defined, ':_ attr defaults to a type object';
is CAny.new(:x(3)).x, 3, ':_ attr accepts a defined value';

# --- mixed: a plain attr alongside a :D attr ---
class CMix { has $.name; has Int:D $.age = 0 }
my $c = CMix.new(:name<bob>, :age(42));
is $c.name, 'bob', 'mixed: plain attr set';
is $c.age, 42, 'mixed: :D attr set';

# --- inheritance: :D attr in a parent ---
class P { has Int:D $.id = 1 }
class Child is P { has $.tag }
my $q = Child.new(:id(10), :tag<t>);
is $q.id, 10, 'inherited :D attr set';
is $q.tag, 't', 'child attr set';
dies-ok { Child.new(:id(Int)) }, 'inherited :D attr rejects undefined';

# --- :D with BUILD/TWEAK (smiley enforced before and after the phasers) ---
class CTweak { has Int:D $.x = 1; submethod TWEAK { $!x = 100 } }
is CTweak.new.x, 100, ':D attr with a TWEAK that keeps it defined';

# A TWEAK that violates a :D attr is NOT re-checked (matches the interpreter
# baseline, which relies on an assignment-time check mutsu does not perform).
class CTweakBad { has Int:D $.x = 1; submethod TWEAK { $!x = Int } }
lives-ok { CTweakBad.new }, 'TWEAK violating a :D attr is not re-checked (baseline parity)';

class CBuild { has Int:D $.x = 1; submethod BUILD(:$y = 5) { $!x = $y } }
is CBuild.new(:y(8)).x, 8, ':D attr with a BUILD that keeps it defined';

# A BUILD that mutates a :D attribute into a violation is rejected (smileys are
# re-checked after BUILD, matching the full constructor's post-BUILD pass).
class CBuildBad { has Int:D $.x = 1; submethod BUILD { $!x = Int } }
dies-ok { CBuildBad.new }, 'BUILD violating a :D attr dies (post-BUILD smiley re-check)';

# --- :U constructs natively ---
class CU2 { has Int:U $.x }
isa-ok CU2.new, CU2, ':U attr constructs natively';

# --- str:D native-typed smiley ---
class CStr { has Str:D $.s = "hi" }
is CStr.new.s, "hi", 'Str:D attr with a default';
is CStr.new(:s<bye>).s, 'bye', 'Str:D attr provided';
