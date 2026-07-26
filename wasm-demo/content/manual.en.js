/**
 * English text for the manual — mutsu's own user documentation: how to install
 * it, run it, find modules, install packages, and where it stands against
 * Rakudo. The Raku *language* is taught by the tutorial and documented
 * authoritatively at docs.raku.org; this page is about the tool.
 *
 * Section bodies are trusted HTML from this repository (never user input), and
 * are inserted with innerHTML the same way the tutorial's lesson bodies are.
 * `{roastPass}`, `{roastTotal}` and `{roastPct}` are substituted at render time
 * from content/stats.json, so the compatibility figure cannot go stale.
 */

export default {
  title: 'mutsu manual',
  intro: 'Everything about running mutsu itself: installing it, the command ' +
    'line, where it looks for modules, the bundled package manager, and how ' +
    'far its Raku support goes. For the language itself, take the ' +
    '<a href="tutorial.html">tutorial</a> or read the ' +
    '<a href="https://docs.raku.org/" rel="noopener">official Raku documentation</a>.',
  tocIntro: 'How to install and run mutsu itself. The Raku language is the ' +
    'tutorial’s subject, not this page’s.',
  tocTitle: 'Contents',

  sections: [
    {
      id: 'install',
      title: 'Installing mutsu',
      body: `
        <h3>With mise (recommended)</h3>
        <p>Prebuilt binaries are published to GitHub Releases for Linux and macOS,
        on both x86-64 and arm64. <a href="https://mise.jdx.dev/" rel="noopener">mise</a>
        installs one and puts <strong>both</strong> commands on your PATH — the
        <code>mutsu</code> interpreter and <code>mzef</code>, the bundled package
        manager.</p>
        <pre><code>mise use -g github:tokuhirom/mutsu        # latest release
mise use -g github:tokuhirom/mutsu@0.18.0 # or pin a version

mutsu -e 'say "Hello, World!"'
mzef --version</code></pre>
        <p>The archive is self-contained and needs no configuration:</p>
        <pre><code>bin/mutsu                  the interpreter
bin/mzef                   the package manager (a shim that runs zef on mutsu)
share/mutsu/zef            the vendored Zef, which mzef runs
share/mutsu/modules        the bundled libraries, found by a plain "use"</code></pre>

        <h3>With Docker</h3>
        <p>An image carrying both commands is published to GHCR. Tags follow the
        releases; <code>:latest</code> is the newest release and <code>:main</code>
        tracks the development branch.</p>
        <pre><code>docker run --rm -it ghcr.io/tokuhirom/mutsu              # a REPL
docker run --rm ghcr.io/tokuhirom/mutsu mutsu -e 'say (^10).sum'
docker run --rm -v "$PWD:/work:ro" ghcr.io/tokuhirom/mutsu mutsu hello.raku</code></pre>
        <p><code>mzef install</code> writes under <code>$HOME</code>, which is
        <code>/root</code> in the image. Mount a named volume there to keep
        installed modules between runs.</p>

        <h3>From source</h3>
        <p>You need Rust 1.94 or newer (edition 2024) and a C compiler, which
        <code>pcre2-sys</code> builds against.</p>
        <pre><code>git clone https://github.com/tokuhirom/mutsu.git
cd mutsu
cargo build --release
./target/release/mutsu --version</code></pre>
        <p>That builds both binaries: <code>target/release/mutsu</code> and
        <code>target/release/mzef</code>.</p>`,
    },

    {
      id: 'running',
      title: 'Running programs',
      body: `
        <p>Give mutsu a file, a string, or nothing at all:</p>
        <pre><code>mutsu script.raku              # run a file
mutsu -e 'say 42'              # run a one-liner
echo 'say 42' | mutsu          # run what arrives on stdin
mutsu -                        # the same, spelled explicitly
mutsu --repl                   # an interactive session</code></pre>
        <p>A shebang works the way you would expect, so a script can be executable
        on its own:</p>
        <pre><code>#!/usr/bin/env mutsu
say "hello from a script";</code></pre>

        <h3>Arguments</h3>
        <p>Everything after the script name belongs to the script and arrives in
        <code>@*ARGS</code> untouched — mutsu does not try to interpret it:</p>
        <pre><code>$ mutsu -e 'say @*ARGS' foo --bar=1
[foo --bar=1]</code></pre>
        <p>Declare a <code>MAIN</code> sub and the arguments are matched against its
        signature instead, with a usage message generated for a call that does not
        fit:</p>
        <pre><code>$ cat greet.raku
sub MAIN(Str $name) { say "Hello, $name!" }
$ mutsu greet.raku World
Hello, World!
$ mutsu greet.raku
Usage:
  greet.raku &lt;name&gt;</code></pre>

        <h3>Exit status and errors</h3>
        <p><code>exit 3</code> exits with 3. An uncaught exception prints its
        message and a backtrace to stderr and exits with 1:</p>
        <pre><code>$ mutsu boom.raku
boom
  in sub f at boom.raku line 1
  in sub g at boom.raku line 2
  in block &lt;unit&gt; at boom.raku line 3</code></pre>`,
    },

    {
      id: 'options',
      title: 'Command-line options',
      body: `
        <table class="opt-table">
          <tbody>
          <tr><td><code>-e CODE</code></td><td>Run <code>CODE</code> instead of a file.</td></tr>
          <tr><td><code>-n</code></td><td>Wrap the program in a loop over the input lines, with each line in <code>$_</code>.</td></tr>
          <tr><td><code>-p</code></td><td>Like <code>-n</code>, but print <code>$_</code> after each iteration.</td></tr>
          <tr><td><code>-ne CODE</code>, <code>-pe CODE</code></td><td>The combined forms, as in <code>mutsu -ne 'say .uc'</code>.</td></tr>
          <tr><td><code>-I PATH</code></td><td>Add <code>PATH</code> to the module search path. Repeatable; may be written <code>-IPATH</code>.</td></tr>
          <tr><td><code>-M MODULE</code></td><td><code>use MODULE</code> before running the program. Repeatable.</td></tr>
          <tr><td><code>--repl</code></td><td>Start the interactive REPL.</td></tr>
          <tr><td><code>--doc</code></td><td>Render the source's Pod documentation instead of running it.</td></tr>
          <tr><td><code>--dump-ast</code></td><td>Print the parsed AST instead of running it.</td></tr>
          <tr><td><code>--dump-bytecode</code></td><td>Print the compiled bytecode instead of running it.</td></tr>
          <tr><td><code>--no-precomp</code></td><td>Ignore and do not write the precompilation cache.</td></tr>
          <tr><td><code>-v</code>, <code>--version</code></td><td>Print the version.</td></tr>
          <tr><td><code>-h</code>, <code>--help</code></td><td>Print the option summary.</td></tr>
          </tbody>
        </table>
        <p>The line-loop options read standard input:</p>
        <pre><code>$ printf 'a\\nb\\n' | mutsu -ne 'say .uc'
A
B</code></pre>`,
    },

    {
      id: 'modules',
      title: 'Modules and the search path',
      body: `
        <p>A module name maps to a path: <code>use Foo::Bar</code> looks for
        <code>Foo/Bar.rakumod</code> (also <code>.pm6</code> and <code>.pm</code>)
        in each search directory, and in that directory's <code>lib/</code>
        subdirectory — so <code>-I .</code> finds <code>lib/Foo/Bar.rakumod</code>
        in a normal project layout.</p>
        <p>There are three ways to add a directory, and they are searched in this
        order together with the two sources mutsu provides on its own:</p>
        <ol>
          <li><code>use lib</code> paths, most recently added first</li>
          <li><code>-I</code> paths, in the order given</li>
          <li><code>MUTSULIB</code> paths, in the order given</li>
          <li>installed modules — the site repository <code>mzef</code> writes to</li>
          <li>the bundled libraries that ship with mutsu</li>
        </ol>
        <p>The first hit wins. So <code>-I</code> always shadows an installed module
        of the same name, <strong>whatever version</strong> that installed copy has —
        the flag pins a directory, not a version. Equally, the bundle is a floor
        rather than a ceiling: a newer copy you install with <code>mzef</code>
        shadows the bundled one.</p>
        <pre><code>mutsu -I lib script.raku
MUTSULIB=/opt/raku/lib:/srv/lib mutsu script.raku</code></pre>
        <pre><code>use lib 'lib';          # inside the program; also accepts a list
use Foo::Bar;</code></pre>

        <h3>Installed modules</h3>
        <p>Installed distributions live in the <em>site repository</em>, at
        <code>$XDG_DATA_HOME/mutsu/repo/site</code> (by default
        <code>~/.local/share/mutsu/repo/site</code>). Nothing needs to be
        configured for a plain <code>use</code> to find them.</p>
        <p>When several installed distributions provide the same module name, the
        highest version wins, and the <code>use</code> statement can narrow the
        choice:</p>
        <pre><code>use JSON::Class:auth&lt;zef:jonathanstowe&gt;;
use Cro::HTTP:ver&lt;0.8.7+&gt;;
use Some::Module:api&lt;2&gt;;</code></pre>`,
    },

    {
      id: 'packages',
      title: 'Installing packages with mzef',
      body: `
        <p><code>mzef</code> is the real <a href="https://github.com/ugexe/zef"
        rel="noopener">Zef</a> — the Raku ecosystem's package manager — vendored
        with mutsu and running <em>on</em> mutsu. It ships in the same archive as
        the interpreter, so there is nothing to bootstrap.</p>
        <pre><code>mzef install JSON::Fast      # install a distribution and its dependencies
mzef list --installed        # what is installed here
mzef info JSON::Fast         # metadata from the ecosystem index
mzef search json             # find something
mzef uninstall JSON::Fast
mzef update                  # refresh the ecosystem indexes</code></pre>
        <p>Installs go to the site repository described above, and a plain
        <code>use</code> picks them up immediately.</p>
        <p><strong>Expect rough edges here.</strong> The CLI and the install
        pipeline work, but the ecosystem is large and not every distribution
        builds and tests cleanly on mutsu yet. When one fails, that is a mutsu bug
        worth reporting — the project's policy is to fix the interpreter rather
        than patch the library.</p>`,
    },

    {
      id: 'bundled',
      title: 'Bundled libraries',
      body: `
        <p>mutsu ships a set of community libraries next to the binary, under
        <code>share/mutsu/modules</code>. They are genuine upstream modules,
        adopted unmodified, and they are on the search path from the moment you
        install mutsu — a plain <code>use</code> works with no
        <code>mzef install</code> and no network:</p>
        <pre><code>use HTTP::UserAgent;
my $ua = HTTP::UserAgent.new;
say $ua.get('https://example.com').content.chars;</code></pre>
        <p>TLS, HTTP, URI, MIME::Base64, templating and temp-file handling are all
        in the box. <a href="batteries.html">The bundled-libraries page</a> lists
        every one of them with its version, license, provided modules and upstream
        documentation.</p>
        <p>Because the bundle is the lowest-priority source, installing a newer
        version of a bundled library with <code>mzef</code> simply shadows it —
        that is how a security update reaches you without waiting for a mutsu
        release.</p>`,
    },

    {
      id: 'env',
      title: 'Environment variables',
      body: `
        <table class="opt-table">
          <tbody>
          <tr><td><code>MUTSULIB</code></td><td>Colon-separated module search paths, searched after <code>-I</code>.</td></tr>
          <tr><td><code>MUTSU_BUNDLE_DIR</code></td><td>Where the bundled libraries live. Defaults to <code>share/mutsu/modules</code> next to the binary.</td></tr>
          <tr><td><code>XDG_DATA_HOME</code></td><td>Parent of the site repository, at <code>$XDG_DATA_HOME/mutsu/repo/site</code>. Defaults to <code>~/.local/share</code>.</td></tr>
          <tr><td><code>XDG_CACHE_HOME</code></td><td>Parent of the precompilation cache, at <code>$XDG_CACHE_HOME/mutsu/precomp</code>. Defaults to <code>~/.cache</code>.</td></tr>
          <tr><td><code>MZEF_ZEF_HOME</code></td><td>The vendored Zef tree <code>mzef</code> should run. Normally discovered next to the binary.</td></tr>
          <tr><td><code>MZEF_MUTSU_BIN</code></td><td>The interpreter <code>mzef</code> should run Zef under. Normally the sibling <code>mutsu</code>.</td></tr>
          </tbody>
        </table>
        <p>The variables that change how the engine runs — the JIT, the garbage
        collector, tracing — are listed under
        <a href="#debugging">Looking inside</a>.</p>`,
    },

    {
      id: 'cache',
      title: 'The precompilation cache',
      body: `
        <p>Parsing a module is the expensive part of loading it, so mutsu caches
        the parsed form under <code>$XDG_CACHE_HOME/mutsu/precomp</code> (by
        default <code>~/.cache/mutsu/precomp</code>). An entry is keyed by the
        source path and validated against its modification time and a hash of its
        contents, so editing a module invalidates it — you never need to clear the
        cache by hand to see a change.</p>
        <p>Turn it off for a run with <code>--no-precomp</code>, or for one module
        by putting <code>no precompilation;</code> in its source. Deleting the
        directory is always safe; it is rebuilt on demand.</p>`,
    },

    {
      id: 'debugging',
      title: 'Looking inside',
      body: `
        <p>An uncaught exception already prints a backtrace with file and line
        numbers. When that is not enough:</p>
        <pre><code>mutsu --dump-ast -e 'say 1 + 2'        # what the parser made of it
mutsu --dump-bytecode -e 'say 1 + 2'   # what the compiler emitted</code></pre>
        <table class="opt-table">
          <tbody>
          <tr><td><code>MUTSU_TRACE=1</code></td><td>Trace everything to stderr. A comma-separated list narrows it: <code>MUTSU_TRACE=parse,vm</code>.</td></tr>
          <tr><td><code>MUTSU_VM_STATS=1</code></td><td>Print a one-line VM counter summary to stderr when the program ends.</td></tr>
          <tr><td><code>MUTSU_JIT=off</code></td><td>Run the bytecode interpreter without the JIT (on by default).</td></tr>
          <tr><td><code>MUTSU_GC=off</code></td><td>Disable the cycle collector (on by default). Cycles then leak, which is occasionally useful when bisecting.</td></tr>
          </tbody>
        </table>
        <p>If something misbehaves, the shortest useful bug report is the program,
        what mutsu printed, and what <code>raku</code> prints for the same program.
        Issues go to <a href="https://github.com/tokuhirom/mutsu/issues"
        rel="noopener">the tracker</a>.</p>`,
    },

    {
      id: 'compat',
      title: 'How compatible is it?',
      body: `
        <p>mutsu passes <strong>{roastPass} of the {roastTotal}</strong> files in
        <a href="https://github.com/Raku/roast" rel="noopener">roast</a>, the
        official Raku specification suite — {roastPct}% of the suite, counted per
        file, where a file counts only if every one of its assertions passes.
        Rakudo remains the reference implementation; mutsu is measured against it
        daily.</p>

        <h3>Things people assume are missing, and are not</h3>
        <ul>
          <li><strong>Real threads.</strong> <code>start</code>/<code>await</code>,
              <code>Thread</code>, <code>Lock</code> and <code>Promise</code> run on
              actual OS threads, not a cooperative fake.</li>
          <li><strong>Supplies and reactions.</strong>
              <code>supply</code>/<code>react</code>/<code>whenever</code> work.</li>
          <li><strong>NativeCall.</strong> Enough of it to run the bundled
              <code>OpenSSL</code> binding, which is how HTTPS works here.</li>
          <li><strong>Grammars</strong>, including action classes and
              <code>make</code>/<code>.made</code>.</li>
          <li><strong>Containers.</strong> <code>Proxy</code>, binding and
              <code>is rw</code> behave as specified.</li>
        </ul>

        <h3>Known gaps</h3>
        <ul>
          <li>Some compile-time diagnostics are missing — most visibly, an
              undeclared variable is not rejected at compile time the way strict
              mode requires.</li>
          <li>Not every <code>X::</code> exception type exists yet, so a
              <code>CATCH</code> that matches on a rare one may not fire.</li>
          <li>Feeds spanning multiple lines are not parsed yet (single-line feeds
              are).</li>
          <li><code>RakuAST</code> exists but is far from complete.</li>
          <li>Installing arbitrary ecosystem distributions is not yet dependable —
              see <a href="#packages">mzef</a> above.</li>
        </ul>

        <h3>Speed</h3>
        <p>Startup is roughly 25× faster than Rakudo's, which is what makes mutsu
        pleasant for one-liners and scripts. On the project's benchmarks the
        steady-state numbers land near Rakudo's — with the JIT on (the default),
        the recursion and class-method benchmarks are at or slightly ahead of
        <code>raku</code>. The
        <a href="bench-trend.html">benchmark trend</a> is measured on every push
        to the main branch and shows the current figures rather than these
        adjectives.</p>

        <p class="manual-note">mutsu is under active development and is not yet
        recommended for production. It is, however, entirely usable for scripts,
        CLI tools and learning Raku.</p>`,
    },
  ],
};
