use Test;

plan 4;

# A user subset declared inside a class may share its short name with a core
# type (Cro::HTTP::Request declares `subset Method of Str`); the attribute's
# declared type must resolve to the lexical subset, not the builtin `Method`.

class SSBT-Req {
    subset Method of Str where /^<[A..Z]>+$/;
    has Method $.m is rw;
}

my $r;
lives-ok { $r = SSBT-Req.new }, '.new with unset shadowing-subset attribute';
lives-ok { $r.m = "GET" }, 'assignment satisfying the subset';
is $r.m, "GET", 'value stored';
dies-ok { $r.m = "get" }, 'assignment violating the subset predicate dies';
