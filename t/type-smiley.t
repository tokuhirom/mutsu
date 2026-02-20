use Test;

plan 12;

# Type smileys: :U (undefined/type objects), :D (defined/instances), :_ (any)

# :U matches type objects (undefined)
ok HyperWhatever ~~ HyperWhatever:U, 'type object matches :U';
ok !(42 ~~ HyperWhatever:U), 'non-type does not match :U';

# :D matches defined values (instances)
ok ((**)) ~~ HyperWhatever:D, 'HyperWhatever value matches :D';
ok !(HyperWhatever ~~ HyperWhatever:D), 'type object does not match :D';

# :_ matches both
ok HyperWhatever ~~ HyperWhatever:_, 'type object matches :_';

# Int smileys
ok 42 ~~ Int:D, 'defined Int matches Int:D';
ok !(Int ~~ Int:D), 'type object Int does not match Int:D';
ok Int ~~ Int:U, 'type object Int matches Int:U';
ok !(42 ~~ Int:U), 'defined Int does not match Int:U';

# HyperWhatever literal **
ok (**).WHAT.raku eq 'HyperWhatever', '** creates HyperWhatever';

# X::Cannot::New for types that cannot be instantiated
dies-ok { HyperWhatever.new }, 'HyperWhatever.new throws';
dies-ok { Whatever.new }, 'Whatever.new throws';
