`fail` now leaves a called block for its lexically enclosing routine. A block
defined in the mainline throws its exception immediately, including when a
routine calls that block. A block defined inside a routine still returns a
Failure from that routine.
