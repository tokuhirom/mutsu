# Dependabot version updates with a cooldown

Routine dependency updates are now opened by Dependabot
(`.github/dependabot.yml`) instead of the monthly hand-run `cargo update`.
Cargo is checked weekly, with every in-range update grouped into one
`chore(deps):` PR and each major bump kept separate; the SHA-pinned GitHub
Actions are checked weekly as one grouped `ci(deps):` PR; the Dockerfile base
images are checked monthly, with the `rust` builder image limited to patch
updates because its minor version is the toolchain.

Every ecosystem has a `cooldown`: a release must be 3 to 30 days old, depending
on the ecosystem and update type, before it is proposed. A broken or compromised
release then has time to be yanked before mutsu picks it up. Security updates
skip the cooldown. `docs/maintenance.md` describes what remains manual.
