use v6;
use Test;

# `need` takes the version, authority and API selectors written on the module
# name, as `use` does (Compress::Zlib's `need Compress::Zlib::Raw:ver<1.1.0+>`).
# Checked against rakudo.

plan 5;

use lib 't/lib';

need NeedSel::Raw:ver<1.1.0+>;
is NeedSel::Raw::raw-answer(), 42, ':ver<...> on the name';

need NeedSel::Raw:ver<2.0.0>:auth<zef:mutsu>;
is NeedSel::Raw::raw-answer(), 42, ':ver<...>:auth<...> on the name';

need NeedSel::Raw:ver(v2.0.0);
is NeedSel::Raw::raw-answer(), 42, 'the :ver(...) form';

need NeedSel;
is NeedSel::answer(), 42, 'a module that needs another with a selector loads';

nok ::('&raw-answer'), 'need still imports nothing';
