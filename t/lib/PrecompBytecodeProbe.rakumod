unit module PrecompBytecodeProbe;

# Each construct below once compiled differently in the process that wrote the
# compiled-bytecode cache and in a later one (ADR-11756 §2.3): a `state`
# variable's key, a chained comparison's temp, a BEGIN-time value slot and a
# role declaration's id. (A Signature literal is a constant the cache refuses,
# so a module holding one is compiled afresh every time; it is not here.)

my $begun = BEGIN { 6 * 7 };

role Greets { method greet { 'hi from ' ~ self.name } }
class Person does Greets { has $.name }

sub counter is export {
    state $n = 0;
    ++$n
}

sub in-range($x) is export { 1 < $x < 10 }

sub probe is export {
    (counter(), counter(), in-range(5), in-range(11), $begun, Person.new(:name<Ann>).greet).join('|')
}
