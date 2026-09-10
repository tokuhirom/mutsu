use v6;
use Test;

# VM-native default construction now covers two more attribute shapes
# (Track A ③ constructor):
#   1. A class-typed `$` attribute left unprovided and undefaulted — stored as
#      `Nil`, with the declared *type object* synthesized at read time, exactly
#      as the interpreter does (and as raku does).
#   2. An `is required` attribute — the native builder raises
#      `X::Attribute::Required` itself: before defaults when there is no BUILD,
#      and after BUILD when a BUILD submethod has a chance to set it.

plan 21;

# --- class-typed `$` unset -> declared type object, undefined ---
class C1 { has Int $.x; }
is C1.new.x.^name, 'Int', 'unset Int attribute reads as its type object';
nok C1.new.x.defined, 'unset Int attribute is undefined';

class C2 { has Str $.s; has Int $.n; }
my $o = C2.new;
is $o.s.^name, 'Str', 'unset Str attribute type object';
is $o.n.^name, 'Int', 'unset Int attribute type object';
nok $o.s.defined, 'unset Str undefined';

# --- subtype/class type unset ---
class C3 { has Numeric $.v; }
is C3.new.v.^name, 'Numeric', 'unset Numeric attribute type object';

# --- mixed: one provided, one unset ---
class C4 { has Int $.a; has Int $.b; }
my $m = C4.new(a => 7);
is $m.a, 7, 'provided typed attribute';
is $m.b.^name, 'Int', 'sibling unset typed attribute is a type object';
nok $m.b.defined, 'sibling unset typed attribute undefined';

# --- inheritance: parent class-typed attr unset ---
class PBase { has Str $.name; }
class QDer is PBase { has Int $.age; }
my $q = QDer.new(age => 5);
is $q.name.^name, 'Str', 'inherited unset typed attribute type object';
is $q.age, 5, 'own provided typed attribute';

# --- introspect the stored value is the type object (read synthesis) ---
class C5 { has Int $.k; method peek() { $!k.^name } }
is C5.new.peek, 'Int', 'attribute read inside a method synthesizes the type object';

# --- required, no BUILD: not provided dies; provided lives ---
class R1 { has $.r is required; }
dies-ok { R1.new }, 'unprovided required attribute (no BUILD) dies';
is R1.new(r => 9).r, 9, 'provided required attribute constructs';

# --- required failure names the attribute and is the right exception type ---
# (mutsu does not currently thread the optional `is required(reason)` text into
# the message — a pre-existing parser limitation shared by the interpreter — so
# we assert the attribute name and exception type, which the native path does
# preserve byte-identically.)
class R2 { has $.x is required; }
my $err = (try { R2.new; "" }) // $!;
like $!.message, /'$!x'/, 'required failure names the attribute';
isa-ok $!, X::Attribute::Required, 'required failure is X::Attribute::Required';

# --- required + BUILD that sets it -> lives ---
class B1 { has $.x is required; submethod BUILD(:$x) { $!x = $x // 5 } }
is B1.new.x, 5, 'BUILD satisfies a required attribute (default)';
is B1.new(x => 8).x, 8, 'BUILD satisfies a required attribute (provided)';

# --- required + BUILD that does NOT set it -> dies after BUILD ---
class B2 { has $.x is required; submethod BUILD() { } }
dies-ok { B2.new }, 'required attribute unset by BUILD dies post-BUILD';

# --- required + TWEAK (no BUILD): required still enforced pre-TWEAK ---
class T1 { has $.x is required; submethod TWEAK() { } }
dies-ok { T1.new }, 'required attribute with only a TWEAK still dies';

# --- required typed attribute provided ---
class R3 { has Int $.n is required; }
is R3.new(n => 3).n, 3, 'required typed attribute provided constructs';
