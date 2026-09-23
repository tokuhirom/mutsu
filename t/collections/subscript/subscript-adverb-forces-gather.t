use Test;

plan 2;

# Text::Markov's read() method applies :v to a lazy gather returned by reader().
class LazyReaderFixture {
    method reader() returns Seq {
        return lazy gather { take 'foo' }
    }

    method read(Int:D $length = 1024) returns List {
        return self.reader[^$length]:v
    }
}

my $source = lazy gather { take 42 };
is-deeply $source[^1]:v, (42,), ':v forces an uncached lazy gather';
is-deeply LazyReaderFixture.new.read(1), ('foo',),
    ':v works through a lazy reader method return';
