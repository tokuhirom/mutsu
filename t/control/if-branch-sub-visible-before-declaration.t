use Test;

# From the Linux::Fuser distribution: inside `if COND { ... }` a named `sub` is
# visible to the whole branch, so a call above its declaration works
# (t/01-basic.t calls `do_tests(...)` above `sub do_tests` in an `if` block).
plan 7;

my @log;

if True {
    mark('const-if');
    sub mark($s) { @log.push($s) }
}
is-deeply @log, ['const-if'], 'a call above the sub in a constant-condition if';

if @log.elems == 1 {
    mark('runtime-if');
    sub mark($s) { @log.push($s) }
}
is-deeply @log, ['const-if', 'runtime-if'], 'in a runtime-condition if';

if @log.elems == 99 {
    skip-me();
    sub skip-me() { @log.push('never') }
} else {
    mark('else');
    sub mark($s) { @log.push($s) }
}
is-deeply @log, ['const-if', 'runtime-if', 'else'], 'in an else branch';

unless @log.elems == 99 {
    mark('unless');
    sub mark($s) { @log.push($s) }
}
is @log.tail, 'unless', 'in an unless branch';

my $v = do if True {
    my $r = twice(4);
    sub twice($n) { $n * 2 }
    $r
};
is $v, 8, 'in a value-position if (do if)';

sub outer($flag) {
    if $flag {
        return inner(3);
        sub inner($n) { $n + 1 }
    }
    0
}
is outer(True), 4, 'inside a routine, the branch returns through the later sub';
is outer(False), 0, 'and the other path is untouched';
