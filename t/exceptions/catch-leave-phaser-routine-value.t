use Test;

plan 8;

# A routine (or do block) holding both a CATCH and an ENTER/LEAVE/KEEP/UNDO
# phaser still returns its last statement's value. It came back as Any because
# the value was read from the enclosing topic. Red's `automatic-transaction`
# (`CATCH { ... }; KEEP $guard.commit; UNDO ...; block()`) lost its result.

my @log;

sub with-keep(&block) {
    CATCH { default { @log.push: "caught" } }
    KEEP @log.push: "keep";
    UNDO @log.push: "undo";
    block()
}
is with-keep({ 42 }), 42, 'CATCH + KEEP + UNDO: the tail call value is returned';
is @log.join(","), "keep", 'the KEEP phaser ran on success';

sub with-leave(&block) {
    CATCH { default { } }
    LEAVE @log.push: "leave";
    block()
}
is with-leave({ 44 }), 44, 'CATCH + LEAVE: the tail call value is returned';

sub literal-tail { CATCH { default { } }; LEAVE { }; 3 }
is literal-tail(), 3, 'a literal tail is returned';

sub enter-tail { CATCH { default { } }; ENTER { }; 8 }
is enter-tail(), 8, 'CATCH + ENTER: the tail is returned';

sub nested(@blocks) {
    with-keep({ my @r = @blocks.map({ $_() }).eager; @r.elems == 1 ?? @r.head !! @r })
}
is nested([{ 45 }]), 45, 'the value passes through a second routine';

my $x = do { CATCH { default { } }; LEAVE { }; 7 };
is $x, 7, 'a do block with CATCH + LEAVE yields its value';

sub dies { CATCH { default { } }; LEAVE { }; die "x"; 1 }
is dies().raku, 'Nil', 'a handled exception leaves Nil';
