use Test;

# A multi candidate's compiled routine used to be keyed by its POSITIONAL
# signature alone, so two candidates that differ only in their *named*
# parameters shared one key (`GLOBAL::f/0`) and the second body replaced the
# first. Dispatch survived that only because it re-checked the body fingerprint
# and fell back to compiling on the fly; once registration started handing each
# candidate the routine its declaration plan names, the collision installed one
# candidate's bytecode under the other and the wrong body ran.

plan 9;

multi sub f(Int :x($)) { "x" }
multi sub f(Int :y($)) { "y" }

is f(:x(1)), "x", 'named-only candidate :x runs its own body';
is f(:y(1)), "y", 'named-only candidate :y runs its own body';
# Once each candidate has its own compiled routine, the named-argument
# light-call path can reach one — and its cache is keyed by NAME alone, so a
# second call with different named arguments would reuse the first candidate.
is f(:x(1)), "x", 'a repeat call is not answered by the other candidate';

# The same shape with a `where` constraint, which is how zef's `MAIN` declares
# its `--version` candidate.
multi sub g(Bool :version($) where .so) { "version" }
multi sub g(Bool :h(:help($)))          { "help" }

is g(:version), "version", 'constrained named-only candidate wins on :version';
is g(:help),    "help",    'the other named-only candidate still wins on :help';
is g(:version), "version", 'and :version still reaches its own candidate after that';

# `callsame` needs the multi-dispatch frame the light-call path does not push.
multi sub n(:x($)) { "outer " ~ callsame() }
multi sub n(|)     { "inner" }

is n(:x(1)), "outer inner", 'a named-only candidate can redispatch with callsame';

# Candidates that differ in positional types keep their own bodies too, and a
# named-only candidate alongside them stays reachable.
multi sub h(Int $n)      { "int $n" }
multi sub h(Str $s)      { "str $s" }

is h(1),   "int 1", 'positional Int candidate';
is h("a"), "str a", 'positional Str candidate';
