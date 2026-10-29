# A method candidate's `where` clause names the routines of its own unit

`multi method eq ($ip where { $_ ~~ IP::Addr })` in a role declared by a module
(IP::Addr::Handler, with `sub term:<IP::Addr> () { once require IP::Addr }`)
died with `Unknown function: term:<IP::Addr>` as soon as the role was composed
into a class of another file: matching a method candidate evaluates its `where`
clause before the candidate has a frame, so the bare-name search used the
caller's package chain. A sub's candidates already ran in their declaring unit
(`with_candidate_scope`); the method candidate window now runs in the unit that
declared the method (`MethodDef::source_file`), and the bare-name package search
prefers that unit's lexical package while no frame of the candidate exists.
This takes IP::Addr's `t/040-ipv4-ops.rakutest` and `t/041-ipv6-ops.rakutest`
from 0 to 2 of 2.
