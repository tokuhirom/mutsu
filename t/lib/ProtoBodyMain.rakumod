unit module ProtoBodyMain;

# A `proto MAIN` whose body sets module state before `{*}` (the shape of
# Sway::Config::CLI): the candidate reads what the proto body wrote.
my $seen = 'unset';

proto MAIN(Bool :i(:$stdin), |) is export {
    $seen = 'proto:' ~ ($stdin // 'nil');
    {*}
}

multi MAIN('configs', Bool :i(:$stdin)) { say $seen }
