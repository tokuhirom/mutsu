use Test;

# Pin for the §D ③ ctor-fork slice native-izing the allomorph (`IntStr`/`NumStr`/
# `RatStr`/`ComplexStr`) and `ObjAt`/`ValueObjAt` `.new` constructors in the VM.
# These are pure data assembly (a numeric value mixed with a `Str` override, or a
# stringified `WHICH`), so the VM builds them directly via
# `try_native_builtin_construct` instead of bouncing to the interpreter. The
# behaviour is byte-identical to the interpreter's `dispatch_new_and_constructors`
# arm, which now delegates to the same `build_native_*` helpers.

plan 23;

# --- IntStr -----------------------------------------------------------------
my $is = IntStr.new(42, "forty-two");
isa-ok $is, IntStr, 'IntStr.new yields an IntStr';
is $is.Int, 42, 'IntStr numeric part';
is $is.Str, "forty-two", 'IntStr string part';
ok $is ~~ Int, 'IntStr does Int';
ok $is ~~ Str, 'IntStr does Str';

# --- NumStr -----------------------------------------------------------------
my $ns = NumStr.new(3.5e0, "three-and-a-half");
isa-ok $ns, NumStr, 'NumStr.new yields a NumStr';
is $ns.Num, 3.5e0, 'NumStr numeric part';
is $ns.Str, "three-and-a-half", 'NumStr string part';

# --- RatStr -----------------------------------------------------------------
my $rs = RatStr.new(1/2, "half");
isa-ok $rs, RatStr, 'RatStr.new yields a RatStr';
is $rs.Rat, 0.5, 'RatStr numeric part';
is $rs.Str, "half", 'RatStr string part';
ok $rs ~~ Rat, 'RatStr does Rat';

# --- ComplexStr -------------------------------------------------------------
my $cs = ComplexStr.new(1+2i, "one plus two i");
isa-ok $cs, ComplexStr, 'ComplexStr.new yields a ComplexStr';
is $cs.Complex, 1+2i, 'ComplexStr numeric part';
is $cs.Str, "one plus two i", 'ComplexStr string part';

# --- numeric value unwrapped from an allomorph argument ---------------------
my $nested = IntStr.new($is, "wrap");
is $nested.Int, 42, 'allomorph numeric argument is unwrapped to its inner value';
is $nested.Str, "wrap", 'allomorph string override is applied';

# --- allomorphs participate in arithmetic / string context ------------------
is $is + 8, 50, 'IntStr in numeric context';
is $rs * 4, 2, 'RatStr in numeric context';
is ~$is, "forty-two", 'IntStr stringifies to its Str part';

# --- ObjAt / ValueObjAt -----------------------------------------------------
my $o = ObjAt.new("some-which-value");
isa-ok $o, ObjAt, 'ObjAt.new yields an ObjAt';
my $vo = ValueObjAt.new("another-which");
isa-ok $vo, ValueObjAt, 'ValueObjAt.new yields a ValueObjAt';

# error: ObjAt.new with no positional argument dies
dies-ok { ObjAt.new }, 'ObjAt.new with no positional dies';
