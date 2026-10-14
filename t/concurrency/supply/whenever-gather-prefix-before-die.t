use Test;

plan 5;

# A gather source that dies still delivers what it took first; the QUIT phaser
# sees the failure afterwards (#13318).
my $s = supply {
    whenever Supply.from-list(gather { take 1; die "bang" }) {
        emit $_;
        QUIT { default { emit "q:" ~ .message } }
    }
}
is $s.list.join(","), "1,q:bang", 'prefix then quit inside a supply block';

my $t = supply {
    whenever Supply.from-list(gather { take 1; take 2; die "bang" }) {
        emit $_;
        QUIT { default { emit "q:" ~ .message } }
    }
}
is $t.list.join(","), "1,2,q:bang", 'two values then quit';

my @got;
my $quit;
Supply.from-list(gather { take 1; take 2; die "bang" }).tap(
    -> $v { @got.push: $v },
    quit => -> $e { $quit = $e.message });
is @got.join(","), "1,2", 'tap sees the prefix';
is $quit, "bang", 'tap sees the quit';

my $u = supply {
    whenever Supply.from-list(gather { take 1; take 2 }) {
        emit $_;
    }
}
is $u.list.join(","), "1,2", 'a gather that finishes is unchanged';
