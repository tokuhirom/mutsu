use Test;

class Dyn {
    has $.id;
    has Callable $.main;

    method run($sub?) {
        my $*dynself = self;
        my proto resource ($sc, %d2) {
            my $self = $*dynself;
            $sc.run('x');
            $self.id;
        }
        my proto process ($d1, %d2) {
            my $self = $*dynself;
            $self.id;
        }
        $.main.(&process, &resource);
    }
}

my $inner = Dyn.new(id => 1, main => -> &process, &resource {
    is process('a', 'x' => 'TEXT'), 1, 'inner proto reads the inner invocant';
});
my $outer = Dyn.new(id => 2, main => -> &process, &resource {
    is resource($inner, 'file' => 'TEXT'), 2,
        'outer proto keeps its own my lexical across a nested method call';
});
$outer.run;

done-testing;
