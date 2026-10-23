unit module WrapMultiFamily;
proto sub chat(|) is export {*}
multi sub chat(Int $n, *%args) { "int $n" }
multi sub chat($prompt = '', *%args) { chat(:$prompt, |%args) }
multi sub chat(:$prompt, *%args) { "named {$prompt // ''}" }
