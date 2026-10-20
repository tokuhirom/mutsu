use v6.e.PREVIEW;
# The 6.e half of scripts/gen-rakudo-method-tables.raku, which runs it as a
# child process (a compilation unit raises its language revision with its
# first statement, and an EVAL cannot). Prints, per catalog owner that 6.e
# resolves to the class of that name, the `declared`, `methods` and `only`
# lists in the same format; the parent keeps the names they add.

# The owners: those the committed snapshot already lists, and any named on the
# command line (a class mutsu has gained rows for).
my $snapshot = $*PROGRAM.parent.parent.add('src/builtins/rakudo_method_tables.txt');
my @listed = $snapshot.e
    ?? $snapshot.lines.grep(!*.starts-with('#')).map({ .split("\t")[1] }).grep(*.defined)
    !! ();
my @owners = (|@listed, |@*ARGS).grep(!*.starts-with('RakuAST::')).unique.sort;

sub clean(@names) { @names.grep({ .defined && $_ ne '' && !/\s/ }).unique.sort }

for @owners -> $owner {
    next if $owner.starts-with('Metamodel::');
    # Not `try ::($owner)`: `try` turns its value into `Nil` when it is a
    # `Failure`, and `Failure` itself is one of the owners. A name the
    # revision does not know is a defined `Failure` instance.
    my $type := ::($owner);
    if $type.DEFINITE && $type ~~ Failure { $type.so; next }
    next without $type.HOW;
    next unless $type.HOW ~~ Metamodel::ClassHOW | Metamodel::EnumHOW;
    next unless $type.^name eq $owner;
    my @declared = try { clean($type.^method_table.keys) } // next;
    my @methods = try { clean($type.^methods.map(*.name)) } // next;
    my @only = try {
        clean($type.^method_table.pairs.grep({ !.value.?is_dispatcher }).map(*.key))
    } // next;
    say join "\t", 'declared', $owner, |@declared;
    say join "\t", 'methods', $owner, |@methods;
    say join "\t", 'only', $owner, |@only;
}
