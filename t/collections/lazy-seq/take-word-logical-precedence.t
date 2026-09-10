use v6;
use Test;

plan 8;

{
    my @log;
    my @taken = gather {
        take (do { @log.push('X'); 2 }) and @log.push('Y');
    };
    is-deeply @taken, [2], 'take collects only its left operand';
    is-deeply @log, ['X', 'Y'], 'true taken value runs the and tail';
}

{
    my @log;
    my @taken = gather {
        take (do { @log.push('X'); 0 }) and @log.push('Y');
    };
    is-deeply @taken, [0], 'false left operand is still taken';
    is-deeply @log, ['X'], 'false taken value short-circuits the and tail';
}

{
    my @log;
    my @taken = gather {
        take (do { @log.push('X'); Nil }) orelse @log.push('Y');
    };
    is-deeply @taken, [Any], 'undefined left operand is still taken';
    is-deeply @log, ['X', 'Y'], 'undefined taken value runs the orelse tail';
}

{
    my @log;
    my @taken = gather {
        take (do { @log.push('X'); 3 }) andthen @log.push('Y');
    };
    is-deeply @taken, [3], 'andthen preserves the taken left operand';
    is-deeply @log, ['X', 'Y'], 'taken operand is evaluated exactly once';
}
