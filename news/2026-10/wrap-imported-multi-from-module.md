# `.wrap` on an imported `proto`/`multi` routine reaches calls made from another module

A routine of module A that called a `proto`/`multi` routine it had imported
from module B ran the routine directly when a third unit had `.wrap`ped it
(`&chat.wrap: -> |args { ... }`): the lexical-import call path handed the
bound code object to the dispatch without asking for its wrapper chain, so
only calls spelled in the wrapping unit itself saw the wrapper. A routine an
import binds as a code object now runs the chain recorded for it, as the
by-name call does. The `LLM::DWIM` distribution's `t/01-dwim.rakutest`
(wrapping `llm-chat` of `LLM::Functions`) now matches rakudo, 8/8.
