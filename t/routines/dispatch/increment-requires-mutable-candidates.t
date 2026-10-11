use Test;

# `++`/`--` on an operand with no container raise Rakudo's X::Multi::NoMatch
# message: the argument's type, the core candidates that match it but need a
# mutable argument, and (sorted) the ones that do not match (#9882).

plan 9;

my $int-pre = q:to/END/.chomp;
Cannot resolve caller prefix:<++>(Int:D); the following candidates
match the type but require mutable arguments:
    (Mu:D $a is rw)
    (Int:D $a is rw --> Int:D)
    (int $a is rw --> int)
    (uint $a is rw --> uint)
    (num $a is rw --> num)

The following do not match for other reasons:
    (Bool $a is rw)
    (Mu:U $a is rw)
    (Num:D $a is rw)
    (Num:U $a is rw)
END

my $int-post = q:to/END/.chomp;
Cannot resolve caller postfix:<++>(Int:D); the following candidates
match the type but require mutable arguments:
    (Mu:D $a is rw)
    (Int:D $a is rw --> Int:D)
    (int $a is rw --> int)
    (uint $a is rw --> uint)
    (num $a is rw --> num)

The following do not match for other reasons:
    (Bool:D $a is rw)
    (Bool:U $a is rw --> Bool::False)
    (Mu:U $a is rw)
    (Num:D $a is rw)
    (Num:U $a is rw --> 0e0)
END

my $x = 1;
throws-like { ++(++$x) }, X::Multi::NoMatch, message => $int-pre,
    'prefix ++ on a value lists the candidates';
is $x, 2, 'the operand is evaluated before the dispatch fails';

throws-like { my @foo = 1, 2; for @foo -> $_ { $_++ } }, X::Multi::NoMatch,
    message => $int-post, 'postfix ++ on a readonly loop variable';

sub f($n) { $n++ }
throws-like { f(1) }, X::Multi::NoMatch, message => $int-post,
    'postfix ++ on a readonly parameter';

throws-like { my $s = "a"; ($s ~ "b")++ }, X::Multi::NoMatch,
    message => q:to/END/.chomp, 'a Str operand';
    Cannot resolve caller postfix:<++>(Str:D); the following candidates
    match the type but require mutable arguments:
        (Mu:D $a is rw)
        (int $a is rw --> int)
        (uint $a is rw --> uint)
        (num $a is rw --> num)

    The following do not match for other reasons:
        (Bool:D $a is rw)
        (Bool:U $a is rw --> Bool::False)
        (Int:D $a is rw --> Int:D)
        (Mu:U $a is rw)
        (Num:D $a is rw)
        (Num:U $a is rw --> 0e0)
    END

my $called = 0;
sub g { $called++; 5 }
throws-like { ++g() }, X::Multi::NoMatch, message => $int-pre,
    'prefix ++ on a non-rw call result';
is $called, 1, 'the routine is called before the dispatch fails';

my $b = bag <a b>;
throws-like { $b>>++ }, X::Multi::NoMatch,
    message => $int-post, 'hyper ++ on an immutable Bag';

# A run-time binding failure is raised inside the callee.
sub h($x) {}
my @a = 1, 2;
try h(|@a);
is $!.backtrace.list.map(*.subname).grep(*.so).head, 'h',
    'an arity failure carries the callee frame';
