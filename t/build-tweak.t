use Test;
plan 2;

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
