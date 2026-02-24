use Test;

plan 4;

lives-ok { die if 0 }, 'die if false does not throw';
dies-ok { die if 1 }, 'die if true throws';
lives-ok { fail if 0 }, 'fail if false does not throw';
dies-ok { fail if 1 }, 'fail if true throws';
