unit module UseInBlockNontrivialProto;

proto uibnp(|c) is export { 'proto:' ~ {*} }
multi uibnp(Int $x) { "int $x" }
multi uibnp(Str $x) { "str $x" }

proto uibnp-sig(Int $) is export {*}
multi uibnp-sig(Int $x) { "sig $x" }

proto uibnp-rec(|c) is export { '<' ~ {*} ~ '>' }
multi uibnp-rec(0) { 'z' }
multi uibnp-rec(Int $n) { $n ~ uibnp-rec($n - 1) }
