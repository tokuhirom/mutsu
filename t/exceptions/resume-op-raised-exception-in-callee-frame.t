use Test;
plan 6;

my $name = 'freddy';
{
    my @log;
    CATCH { @log.push: 'caught'; .resume }
    sub q() { $name.substr-rw(7, 1) = 'x'; @log.push: 'q done' }
    q();
    @log.push: 'after';
    is @log.join(','), 'caught,q done,after', 'a sub resumes after its native op raised';
}

{
    my @log;
    CATCH { @log.push: 'caught'; .resume }
    my $c = -> $x { $name.substr-rw(7, 1) = $x; @log.push: 'closure done' };
    $c('y');
    @log.push: 'after';
    is @log.join(','), 'caught,closure done,after', 'a closure resumes';
}

{
    my @log;
    CATCH { @log.push: 'caught'; .resume }
    my $store := Proxy.new(FETCH => -> $ { 1 }, STORE => -> $, $v { $name.substr-rw(7, 1) = $v; @log.push: 'store done' });
    $store = 'gap';
    @log.push: 'after';
    is @log.join(','), 'caught,store done,after', 'a Proxy STORE resumes';
}

{
    my @log;
    CATCH { @log.push: "caught {.^name}"; .resume }
    sub deep() { sub inner() { $name.substr-rw(7, 1) = 'z'; @log.push: 'inner done' }; inner(); @log.push: 'deep done' }
    deep();
    @log.push: 'after';
    is @log.join(','), 'caught X::OutOfRange,inner done,deep done,after', 'two frames down';
}

{
    my @log;
    CATCH { default { @log.push: 'handled' } }
    sub p() { $name.substr-rw(7, 1) = 'x'; @log.push: 'p done' }
    p();
    @log.push: 'not reached';
}
is $name, 'freddy', 'a handler that does not resume still unwinds';

my $r = do {
    CATCH { .resume }
    sub t() { $name.substr-rw(7, 1) = 'x'; 42 }
    t()
};
is $r, 42, 'the resumed sub still returns its value';
