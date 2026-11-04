proto sub pick(|) is export {*}
multi sub pick(Str:D $s is rw, *@p) { "rw" }
multi sub pick(Str:D $s, *@p) { "plain" }
