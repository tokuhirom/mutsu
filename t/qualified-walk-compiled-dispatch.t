use Test;

# §B (#3658): qualified method calls ($obj.Class::meth), the .* / .+ method walk,
# and proto-method dispatch now route resolved candidates through the compiled
# bytecode runner (run_resolved_method_compiled_or_treewalk) instead of always
# tree-walking run_instance_method_resolved. These exercise such dispatch over
# methods that read/mutate attributes and captured-outer state.

plan 9;

# Qualified method call resolving to a role method that reads an attribute.
role Speaker { method greet { "hi from {self.name}" } }
class Person does Speaker { has $.name }
my $p = Person.new(name => 'Ann');
is $p.Speaker::greet, 'hi from Ann', 'qualified role-method call reads attribute (compiled)';

# Qualified call to a parent-class method that mutates an rw attribute.
class Base { has $.n is rw; method base-bump { $.n++ } }
class Derived is Base { method base-bump { $.n += 100 } }
my $d = Derived.new(n => 1);
$d.Base::base-bump;
is $d.n, 2, 'qualified parent-class method mutates the rw attribute (compiled)';

# A qualified call whose body writes a captured-outer lexical.
my $log = '';
role Logger { method record { $log ~= 'r' } }
class Worker does Logger { }
my $w = Worker.new;
$w.Logger::record;
$w.Logger::record;
is $log, 'rr', 'qualified role-method captured-outer write propagates (compiled)';

# .* method walk across the MRO calling every matching method.
my @seen;
class A1 { method tag { @seen.push('A1') } }
class B1 is A1 { method tag { @seen.push('B1') } }
my $b = B1.new;
my @r = $b.*tag;
is @seen.join(','), 'B1,A1', '.* walk invokes each MRO method (compiled, captured-outer write)';
is @r.elems, 2, '.* walk returns one result per matching method';

# .+ walk (at least one) similarly.
@seen = ();
$b.+tag;
is @seen.join(','), 'B1,A1', '.+ walk invokes each MRO method (compiled)';

# Qualified method call that increments a private attribute.
class Acct { has $!bal = 0; method Acct::deposit { $!bal += 5 }; method bal { $!bal } }
my $a = Acct.new;
$a.Acct::deposit; $a.Acct::deposit;
is $a.bal, 10, 'qualified call mutating a private attribute persists (compiled)';

# proto method dispatch still routes correctly.
class Calc {
    proto method op(|) {*}
    multi method op(Int $x) { $x * 2 }
    multi method op(Str $s) { $s ~ $s }
}
my $c = Calc.new;
is $c.op(21), 42, 'proto/multi-method Int candidate (compiled dispatch)';
is $c.op('ab'), 'abab', 'proto/multi-method Str candidate (compiled dispatch)';
