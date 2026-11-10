use Test;

# `try` fatalizes its body: a statement call whose closure hands back a
# variable holding an unhandled Failure throws (Test's `dies-ok { $failure }`),
# for both `$code()` and `&code()` call shapes. Outside `try`/`use fatal`, the
# same statement call stays quiet.
plan 8;

sub mk { Failure.new("boom") }

sub died-in-try(&code) {
    my $died = 1;
    try { code(); $died = 0 }
    $died
}

my $f1 = mk();
my $blk1 = { $f1 };
is died-in-try($blk1), 1, "block returning a Failure variable dies inside try";

my $f2 = mk();
my &blk2 = { $f2 };
is died-in-try(&blk2), 1, "&-bound block, same";

my $f3 = mk();
my $blk3 = { $f3 };
my $died3 = 1;
try { $blk3(); $died3 = 0 }
is $died3, 1, "direct \$blk() statement inside try";

my $f4 = mk();
my &blk4 = { $f4 };
my $died4 = 1;
try { blk4(); $died4 = 0 }
is $died4, 1, "direct blk() statement inside try";

dies-ok { my $x = mk(); $x }, "dies-ok on a block whose value is a Failure variable";

# Not fatal outside try: a plain statement call does not throw.
my $f5 = mk();
my &quiet = { $f5 };
my $survived = False;
{ quiet(); $survived = True }
ok $survived, "outside try/fatal the container's Failure is not sunk";

my $f6 = mk();
my $blk6 = { $f6 };
{ use fatal; my $died6 = 1; try { $blk6(); $died6 = 0 }; is $died6, 1, "use fatal + try" }

my $f7 = mk();
my $blk7 = { $f7 };
my $died7 = 1;
{ use fatal; try { $blk7(); $died7 = 0 } }
is $died7, 1, "use fatal around try";
