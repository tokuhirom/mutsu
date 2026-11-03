use Test;

plan 5;

class Col is IO::Handle {
    has @.got;
    method WRITE(Blob:D \data --> Bool:D) { @!got.push: data.decode; True }
}
sub col { my $c = Col.new(:!chomp); $c.encoding('utf8'); $c }

sub capture(&code) {
    my $outer = col();
    {
        my $*OUT = $outer;
        code();
    }
    $outer.got.join;
}

my $inner = col();

is capture({ for 1 { my $*OUT = $inner; say "in-for"; }; say "after-for"; }),
    "after-for\n", 'a for body ends its own my $*OUT: the later say is not captured by it';
is $inner.got.join, "in-for\n", 'the body wrote to its own handle';

is capture({ for 1, 2 { my $*OUT = $inner; print "x"; }; print "end"; }),
    "end", 'every iteration ends with the body';

is capture({ my $n = 0; while $n++ < 1 { my $*OUT = $inner; put "w"; }; put "after-while"; }),
    "after-while\n", 'a while body restores it too';

my $*FOO = 'outer';
for 1 { my $*FOO = 'inner'; }
is $*FOO, 'outer', 'a user dynamic is restored';
