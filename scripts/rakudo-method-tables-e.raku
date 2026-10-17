use v6.e.PREVIEW;
# The 6.e half of scripts/gen-rakudo-method-tables.raku, which runs it as a
# child process (a compilation unit raises its language revision with its
# first statement, and an EVAL cannot). Prints, per catalog owner that 6.e
# resolves to the class of that name, the `declared`, `methods` and `only`
# lists in the same format; the parent keeps the names they add.

my $table-file = $*PROGRAM.parent.parent.add('src/builtins/native_method_row_table.rs');
my @owners = $table-file.lines
    .map({ ~$0 if / ^ \s* '("' (<-["]>+) '",' / })
    .grep(*.defined)
    .grep(!*.starts-with('RakuAST::'))
    .unique
    .sort;

sub clean(@names) { @names.grep({ .defined && $_ ne '' && !/\s/ }).unique.sort }

for @owners -> $owner {
    next if $owner.starts-with('Metamodel::');
    my $type := try ::($owner);
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
