use Test;

plan 2;

# A supply emitter is compiled before its `whenever` body is dispatched. Each
# invocation must therefore retain the container parameter belonging to that
# invocation, rather than resolving the name through the latest caller frame.
sub wrap-array(Supply $source, @tag --> Supply) {
    supply whenever $source {
        emit @tag.join(",") ~ ":" ~ $_;
    }
}

my $source = Supplier.new;
my $first = wrap-array($source.Supply, ["A"]);
my $second = wrap-array($first, ["B"]);
my @array-got;
$second.tap({ @array-got.push($_) });
$source.emit(1);
is @array-got.join("|"), "B:A:1",
    'nested supply instances keep their array parameter bindings';

sub wrap-hash(Supply $source, %tag --> Supply) {
    supply whenever $source {
        emit %tag<value> ~ ":" ~ $_;
    }
}

my $hash-first = wrap-hash($source.Supply, { value => "A" });
my $hash-second = wrap-hash($hash-first, { value => "B" });
my @hash-got;
$hash-second.tap({ @hash-got.push($_) });
$source.emit(2);
is @hash-got.join("|"), "B:A:2",
    'nested supply instances keep their hash parameter bindings';
