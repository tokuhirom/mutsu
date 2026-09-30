use Test;

# Reduced from Test::Describe's `define` storing a callable in an accessor hash.

plan 8;

class Store {
    has %.values;
}

my $store = Store.new;
$store.values<closure> := { 42 };
is $store.values<closure>.^name, 'Block', 'binding a closure through a hash accessor stores the closure';
is $store.values<closure>(), 42, 'the bound closure remains callable';
dies-ok { $store.values<closure> = { 99 } }, 'an expression bound through an accessor is read-only';

my $source = 1;
$store.values<alias> := $source;
$source = 2;
is $store.values<alias>, 2, 'the bound element sees a source write';
$store.values<alias> = 3;
is $source, 3, 'the source sees a bound element write';

sub install(Str $name, \value) {
    $store.values{$name} := value;
}
install('parameter', { 99 });
is $store.values<parameter>.^name, 'Block', 'binding a captured parameter through an accessor stores its value';
is $store.values<parameter>(), 99, 'the captured callable can be invoked';

my %params = counter => $store.values<closure>;
my &callback = -> :counter(&c) { c() };
is callback(|%params), 42, 'a bound callable retains its type through named alias binding';
