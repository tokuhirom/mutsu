# An exported nested routine no longer writes the caller's same-named variable

An `is export` routine declared inside another routine's body
(`sub set { my @t; my sub test($d) is export { @t.push($d) } ... }`) is exported
at module load since #10050. Called *outside* the enclosing routine's dynamic
extent — before `set` ever ran, or after it returned — it still read its free
variable by name in the caller's environment, so an importer's own `my @t`
received the push (#10114).

The routine-nested sub's latest-activation cells (mutsu's emulation of Rakudo's
`capturelex`) now record which routine owns them, and an entry that belongs to
the running routine wins over the caller's same-named binding. Installing the
enclosing routine during a module load seeds those entries with a fresh
container per free variable, shared by every exported sub nested in the same
routine — Rakudo's static outer frame. A call before any activation therefore
reads the static frame, and a call after the routine returned reads its latest
activation; neither touches the caller.

Fixing the hash case turned up the same leak for a module's file-scope `my %h`:
`%h{$k}++` in a module routine incremented the importer's `%h`, because the
element increment wrote back through the env key instead of the cell the read
had found. It now writes through the module's own cell.
