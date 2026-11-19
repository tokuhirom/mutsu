# A qualified type with a smiley passes the module visibility gate

`Pkg::T:D` in a module routine died with "Could not find symbol 'Pkg::T:D'"
once another compunit provided the top-level `Pkg` module, while plain
`Pkg::T` resolved. The qualified-bareword visibility gate looked the whole
spelling, smiley included, up in the declaring-unit tables, which only hold
the type's own name. The gate now checks the name with its type smiley
stripped, since `T:D`, `T:U` and `T:_` name the same type as `T`.

LLM::Tooling's `@tools.all ~~ LLM::Tool:D` hit it under LLM::Functions, so
LLM::Functions' `t/07-LLM-chat-with-tools.rakutest` failed its tool-call
subtest (#18254).
