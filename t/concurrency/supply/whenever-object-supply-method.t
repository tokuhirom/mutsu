use v6;
use Test;

plan 1;

# Manifesto 0.0.7 uses `whenever $manifesto`, relying on its user-defined
# `Supply` method rather than spelling `$manifesto.Supply`.
class ObjectSupply {
    has Supplier $!supplier;

    submethod TWEAK() {
        $!supplier = Supplier.new;
    }

    method Supply() {
        $!supplier.Supply;
    }

    method emit($value) {
        $!supplier.emit($value);
    }

    method done() {
        $!supplier.done;
    }
}

my $source = ObjectSupply.new;
my @got;

start {
    sleep 0.2;
    $source.emit(1);
    $source.emit(2);
    $source.done;
}

react {
    whenever $source -> $value {
        @got.push($value);
    }
}

is-deeply @got, [1, 2], 'whenever coerces a user object through its Supply method';
