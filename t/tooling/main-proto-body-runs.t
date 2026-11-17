use v6;
use Test;

# The implicit MAIN dispatch calls `&MAIN` itself, as rakudo's RUN-MAIN does:
# a `proto MAIN` body runs before its `{*}` reaches the candidate (#17849).

plan 4;

my $exe = $*EXECUTABLE;
my $inline = q:to/END/;
    my $seen = 'unset';
    proto MAIN(Bool :i(:$stdin), |) { $seen = 'proto:' ~ ($stdin // 'nil'); {*} }
    multi MAIN('configs', Bool :i(:$stdin)) { say $seen }
    END

{
    my $r = run($exe, '-e', $inline, 'configs', :out, :err);
    is $r.out.slurp(:close).trim, 'proto:nil', 'the proto body runs before the candidate';
}

{
    my $r = run($exe, '-e', $inline, '-i', 'configs', :out, :err);
    is $r.out.slurp(:close).trim, 'proto:True', 'and binds the named argument the proto declares';
}

{
    my $r = run($exe, '-I', 't/lib', '-e', 'use ProtoBodyMain;', 'configs', :out, :err);
    is $r.out.slurp(:close).trim, 'proto:nil', 'an exported proto MAIN body runs too';
}

{
    my $r = run($exe, '-I', 't/lib', '-e', 'use ProtoBodyMain;', '-i', 'configs', :out, :err);
    is $r.out.slurp(:close).trim, 'proto:True', 'and writes the module lexical the candidate reads';
}
