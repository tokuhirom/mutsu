# A dynamic variable bound to a missing hash element reads the Array a push autovivifies

`#13098` fixed `my $t := %h<a>; $t.push: {}` for lexicals; the same push through a dynamic variable (`$*tml := $*tml<a>; $*tml.push: {}`) still left the variable reading `Any`, because its slot is a shared cell around the binding or an env entry rather than a local. The push-family autovivification now peels that cell, installs the new Array at the entry and stores it as the variable's content. TOML's `[[array.of.tables]]` headers (`t/01-valid.t`, `t/04-nqp.t`) now match rakudo.
