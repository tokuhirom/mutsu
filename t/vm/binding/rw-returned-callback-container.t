use Test;

plan 1;

my &callback = { $ };

class Forward {
    method value() is rw {
        callback();
    }
}

my $forward = Forward.new;
$forward.value = 42;

is $forward.value, 42,
    'an is rw routine preserves the container returned by an indirect block';
