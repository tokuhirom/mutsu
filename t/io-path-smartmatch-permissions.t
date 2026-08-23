use Test;

plan 1;

is '/'.IO ~~ :w, False,
    'IO::Path smart-match uses effective write access';
