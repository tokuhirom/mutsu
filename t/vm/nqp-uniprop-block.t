use Test;
use nqp;

# `nqp::unipropcode('Block')` and the value codes of the blocks, as MoarVM
# numbers them: a block's place in the UCD's Blocks.txt (Red's regex compiler
# matches `<:Block(...)>` through them, t/121).

plan 13;

my $b = nqp::unipropcode('Block');
is $b, 6, 'Block has its property code';
is nqp::unipropcode('blk'), $b, 'and so does its short name';
is nqp::getuniprop_int(0x41, $b), 1, 'Basic Latin is the first block';
is nqp::getuniprop_int(0xE9, $b), 2, 'Latin-1 Supplement the second';
is nqp::getuniprop_int(0x378, $b), 0, 'an unassigned codepoint is No_Block';
is nqp::getuniprop_str(0xE9, $b), 'Latin-1 Supplement', 'the string value is the block name';
is nqp::getuniprop_str(0x378, $b), 'Greek and Coptic', 'also for an unassigned codepoint';
is nqp::unipvalcode($b, 'Basic_Latin'), 1, 'the long value name';
is nqp::unipvalcode($b, 'ASCII'), 1, 'the short value name';
is nqp::unipvalcode($b, 'greek_and_coptic'), 8, 'a lowercased value name';
is nqp::unipvalcode($b, 'Greek_and_Coptic'), 0, 'not any other spelling';
is nqp::unipvalcode($b, 'Latin1Sup'), 2, 'a name without its underscores';
ok nqp::matchuniprop(0xE9, $b, nqp::unipvalcode($b, 'Latin_1_Supplement')),
    'matchuniprop compares the block';
