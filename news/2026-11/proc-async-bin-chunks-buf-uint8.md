# `Proc::Async` `:bin` chunks are `Buf[uint8]`

The chunks `stdout(:bin)` / `stderr(:bin)` / `Supply(:bin)` deliver were plain
`Buf`; rakudo's are `Buf[uint8]` (`.^name`, `.raku`) (#16150). The reader now
builds them with that type. `IO::Handle.read` and `.encode` already answered
`Buf[uint8]` / `utf8` like rakudo. Pinned by
`t/concurrency/proc-async-bin-chunks-are-buf-uint8.t`.
