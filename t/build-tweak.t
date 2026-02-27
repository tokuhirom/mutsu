use Test;
plan 4;

class Counter {
    has $.count = 0;
    method BUILD { $!count = 1 }
    method TWEAK { $!count = $!count + 1 }
}

my $c = Counter.new();
is $c.count, 2, 'BUILD then TWEAK update attributes';

class OnlyBuild {
    has $.value = 0;
    method BUILD { $!value = 5 }
}

my $o = OnlyBuild.new();
is $o.value, 5, 'BUILD runs without TWEAK';

class NamedHooks {
    has $.value = '';
    has $.seen = '';
    submethod BUILD(Str :$value) { $!value = $value }
    submethod TWEAK(:$value) { $!seen = $value ~ '!' }
}

my $n = NamedHooks.new(value => 'bar');
is $n.value, 'bar', 'BUILD receives named constructor argument';
is $n.seen, 'bar!', 'TWEAK receives named constructor argument';
