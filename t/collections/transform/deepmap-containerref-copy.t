use v6;
use Test;

plan 6;

# A return-rw accessor promotes the selected aggregate to a ContainerRef.
# deepmap({ .clone }) must still descend into that aggregate when building a
# copy; otherwise the copy retains the original cell's aggregate.
class PathAccessor {
    method at($container, *@steps) is rw {
        my $root := $container;
        return-rw at($root, @steps);
    }
}

multi sub at(Associative:D $container, @steps where .elems == 1) is rw {
    my $root := $container;
    $root := $root{@steps[0]};
    return-rw $root;
}

my %original = a => { b => { d => 1 } };
my $ignored = PathAccessor.at(%original, 'a');
my %copy = %original.deepmap({ .clone });

isnt %copy<a>.WHICH, %original<a>.WHICH,
    'deepmap makes a fresh copy of a promoted aggregate';
isnt %copy<a><b>.WHICH, %original<a><b>.WHICH,
    'deepmap makes fresh copies below a promoted aggregate';

%copy<a><b><c> = 2;
is %original<a><b><c>:exists, False,
    'writing to the copy does not mutate the original';
is %copy<a><b><c>, 2, 'the copy remains writable';

# A nested sigilless call can pass the deepmap result through a static method
# whose parameter has the same name. Its writeback must follow the caller's
# existing alias chain instead of replacing the original container binding.
class StaticReplacer {
    method replace(\container) {
        container = { replacement => True };
        container
    }
}

sub copy-and-replace(\container) {
    my $copy = container.deepmap({ .clone });
    StaticReplacer.replace($copy);
    $copy
}

my %source = a => { b => { d => 1 } };
my $promoted = PathAccessor.at(%source, 'a');
my $replaced = copy-and-replace(%source);

is %source<a><replacement>:exists, False,
    'a nested static method does not mutate the original after deepmap';
is $replaced<replacement>, True,
    'the nested static method still updates the copied container';
