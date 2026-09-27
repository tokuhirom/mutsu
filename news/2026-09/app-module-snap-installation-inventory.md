# App::ModuleSnap exposes an installation-repository design gap

The ecosystem roulette run for App::ModuleSnap 0.0.14 re-measured its three
Rakudo-baseline files: two remain at parity, while `t/020-basic.t` remains
partial at 7/8 assertions because `App::ModuleSnap.get-dists` sees no installed
distributions under mutsu.

The reduced probe shows the repository-model difference directly. Rakudo's
default chain has four `CompUnit::Repository::Installation` links and three
installed distributions; mutsu has one empty site installation link. Mutsu's
bundled batteries are intentionally `FileSystem` repositories, and the
ecosystem dependency closure is supplied through `-I` paths, so manufacturing a
synthetic installed distribution would conflict with the package-manager
semantics of the bundled-repository design.

The required design decision is tracked in [#9759](https://github.com/tokuhirom/mutsu/issues/9759).
