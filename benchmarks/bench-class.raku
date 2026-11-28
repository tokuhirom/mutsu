# OOP - class instantiation, method calls, inheritance
class Animal {
    has $.name;
    has $.legs;

    method describe() {
        return $.name ~ " has " ~ $.legs ~ " legs";
    }
}

class Dog is Animal {
    has $.breed;

    method bark() {
        return "Woof!";
    }

    method full-info() {
        return self.describe() ~ " (" ~ $.breed ~ ")";
    }
}

sub workload() {
    my $checksum = 0;

    # Object creation and method calls
    for 1..5000 -> $i {
        my $dog = Dog.new(name => "Rex", legs => 4, breed => "Labrador");
        my $desc = $dog.full-info();
        $checksum += $desc.chars;
        $checksum += $dog.bark().chars;
    }

    # Polymorphism
    my @animals;
    for 1..2000 -> $i {
        @animals.push(Animal.new(name => "Cat", legs => 4));
        @animals.push(Dog.new(name => "Rex", legs => 4, breed => "Lab"));
    }
    for @animals -> $a {
        $checksum += $a.describe().chars;
    }

    "checksum = $checksum";
}

# Warm timed section (convention: benchmarks/bench-regex-match.raku, docs/benchmarks.md):
# the workload runs twice untimed and a third time timed, and that third run is printed
# as `bench-section-seconds:`, so scripts/bench-ci.sh records a `@section` series free of
# interpreter startup. Under scripts/bench-det.sh (BENCH_DET=1) it runs once.
my $warm = %*ENV<BENCH_DET> ?? 0 !! 2;
workload() for ^$warm;
my $t0 = now;
my $result = workload();
say "bench-section-seconds: {now - $t0}";
say $result;
