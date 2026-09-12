use Test;

# An embedded regex code block runs in a scratch interpreter, which is built
# once per code block (1,609 of them on a 60-row YAMLish document). A scratch
# is handed the CALLER's env wholesale, so it no longer seeds the process
# magicals itself — `$*PID`, `$*TZ`, `$*INIT-INSTANT`, `%*ENV`, `@*ARGS` and
# `$*SCHEDULER` were built and dropped unread on every construction. They must
# still resolve inside the block, through the caller's env.

plan 7;

my @seen;
my $matched = 'abc' ~~ / a {
    @seen.push: $*PID ~~ Int && $*PID > 0;
    @seen.push: $*TZ ~~ Int;
    @seen.push: $*INIT-INSTANT ~~ Instant;
    @seen.push: %*ENV.elems > 0;
    @seen.push: @*ARGS ~~ Positional;
    @seen.push: $*SCHEDULER.defined;
} bc /;

ok $matched, 'the regex with the embedded code block matched';
ok @seen[0], '$*PID resolves inside an embedded regex code block';
ok @seen[1], '$*TZ resolves inside an embedded regex code block';
ok @seen[2], '$*INIT-INSTANT resolves inside an embedded regex code block';
ok @seen[3], '%*ENV resolves inside an embedded regex code block';
ok @seen[4], '@*ARGS resolves inside an embedded regex code block';
ok @seen[5], '$*SCHEDULER resolves inside an embedded regex code block';
