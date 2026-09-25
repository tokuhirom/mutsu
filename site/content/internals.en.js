/**
 * English text for the Internals hub (internals.html): how mutsu works inside,
 * for contributors and the curious. The Raku language is the tutorial's
 * subject; running mutsu is the manual's. This page is about the machine.
 *
 * Everything here is prose that changes when a design changes, not when a
 * feature lands: the *lists* (every opcode, every value kind, every built-in
 * type) live on the generated pages (opcodes.html, types.html), which
 * scripts/gen-internals-manifest.py reads out of the source at deploy time.
 * Keep it that way — a list written into this file would go stale.
 *
 * Section bodies are trusted HTML from this repository (never user input),
 * inserted with innerHTML the same way the manual's are. Keep the section ids
 * identical to internals.ja.js.
 */

const SRC = 'https://github.com/tokuhirom/mutsu/blob/main';
const src = (path, text) => `<a href="${SRC}/${path}" rel="noopener"><code>${text ?? path}</code></a>`;
const adr = (file, text) => `<a href="${SRC}/docs/adr/${file}" rel="noopener">${text}</a>`;

export default {
  title: 'mutsu internals',
  intro: 'How mutsu works inside: the path from source text to a running ' +
    'program, how a Raku value is stored, the bytecode virtual machine, and the ' +
    'garbage collector. It is written for people who want to read or change the ' +
    'interpreter. The two reference pages it links to — ' +
    '<a href="types.html">values and types</a> and the ' +
    '<a href="opcodes.html">VM opcodes</a> — are generated from the source every ' +
    'time the site is deployed, so they never describe an older mutsu.',
  tocIntro: 'The machine behind the language: pipeline, values, VM, GC.',
  tocTitle: 'Contents',

  sections: [
    {
      id: 'pipeline',
      title: 'From source to execution',
      body: `
        <p>mutsu is a <strong>bytecode interpreter</strong> written in Rust. A program
        goes through three stages, and there is exactly one execution engine at the
        end of them — the old tree-walking interpreter was removed, and the struct
        called <code>Interpreter</code> <em>is</em> the VM.</p>
        <pre><code>source text
   │  parser        src/parser/     Raku grammar → AST (Expr / Stmt)
   ▼
  AST
   │  compiler      src/compiler/   AST → CompiledCode (opcodes + constant pool)
   ▼
CompiledCode
   │  VM            src/vm/         runs the opcodes on an operand stack
   ▼                                 ├─ hot chunks → Cranelift JIT (native code)
output                               └─ typed routines → TRIR (typed IR)</code></pre>
        <h3>Parser</h3>
        <p>${src('src/parser/')} turns source text into an AST: about fifty kinds
        of expression (${src('src/ast.rs', 'Expr')}) and thirty kinds of statement
        (<code>Stmt</code>). Raku switches sub-languages as it parses — regexes,
        quotes and Pod each have their own grammar inside the main one — and the
        parser follows those switches. Parse errors carry a structured code, line
        and column.</p>
        <h3>Compiler</h3>
        <p>${src('src/compiler/')} walks the AST once and emits a
        <code>CompiledCode</code> chunk per unit, routine and closure body: a vector
        of opcodes, a <strong>constant pool</strong> that literals and names are
        interned into (the same string used at ten sites takes one slot), and a
        parallel table mapping each instruction back to its source line. The
        compiler resolves what it can ahead of time — above all, which lexical
        variable a name refers to, so most variable accesses are a slot index
        rather than a lookup by name.</p>
        <h3>VM</h3>
        <p>${src('src/vm/')} executes the chunk. See
        <a href="#vm">the bytecode VM</a> below.</p>
        <h3>Two faster tiers</h3>
        <ul>
          <li><strong>JIT.</strong> A chunk that runs often enough is compiled to
          native code with <a href="https://cranelift.dev/" rel="noopener">Cranelift</a>.
          It is on by default (<code>MUTSU_JIT=off</code> turns it off).</li>
          <li><strong>TRIR.</strong> A routine whose variables, types and callees are
          all known statically is compiled a second time into a typed, resolved IR
          (${src('src/trir/')}), where a native <code>int</code> is a machine word in
          its own register bank instead of a boxed value. The VM calls it through
          the <a href="opcodes.html#op-CallTrir"><code>CallTrir</code></a> opcode.</li>
        </ul>`,
    },
    {
      id: 'values',
      title: 'How a value is stored',
      body: `
        <p>Every Raku value — an <code>Int</code>, a string, an array, an object, a
        closure — is a Rust ${src('src/value/mod.rs', 'Value')}, and a
        <code>Value</code> is <strong>8 bytes</strong>: one NaN-boxed 64-bit word. It
        plays the part the <code>SV</code> plays in Perl 5, and the different kinds of
        payload play the part of Perl 5's SV body types.</p>
        <pre><code>63            48 47                                           0
┌──────────────┬──────────────────────────────────────────────┐
│  page (16)   │                payload (48)                  │
└──────────────┴──────────────────────────────────────────────┘
page 0x0001          small Int, the payload is the number
page 0x0002..0xFFF2  a double, shifted up by 0x0002 pages
page 0xFFF3..0xFFFF  a kind tag + an inline value or a pointer</code></pre>
        <p>Small integers and doubles never allocate. Everything else is a
        <strong>kind</strong>, and a kind's payload lives in one of four places:</p>
        <ul>
          <li><strong>inline</strong> in the word — <code>Nil</code>, <code>Bool</code>,
          a type object;</li>
          <li>behind an <strong><code>Arc&lt;T&gt;</code></strong> — strings, big
          integers, rationals, ranges, <code>Seq</code>s: plain reference counting;</li>
          <li>behind a <strong><code>Gc&lt;T&gt;</code></strong> — arrays, hashes,
          objects, closures, containers: everything that can take part in a
          reference cycle, and so everything the <a href="#gc">cycle collector</a>
          looks at;</li>
          <li>behind a <strong><code>WeakGc&lt;T&gt;</code></strong> — the weak
          self-reference a block holds to itself.</li>
        </ul>
        <p>Only ${src('src/value/nanbox/mod.rs', 'src/value/nanbox/')} knows the bit
        layout. The rest of the interpreter reads a value through
        ${src('src/value/view.rs', 'ValueView')}, a borrowed enum decoded from the
        word, so the encoding can change without touching its callers.</p>
        <h3>Containers</h3>
        <p>Raku distinguishes a value from the container holding it. A bound
        variable (<code>$a := $b</code>), a captured closure variable and an
        <code>is rw</code> argument all share one
        <code>Gc&lt;ContainerCell&gt;</code>, so a write through one name is seen
        through every other. A container's type constraint belongs to the cell, not
        to the name. Array and hash elements are stored bare and promoted to their
        own cell only when something needs to alias them.</p>
        <p class="manual-note">The complete list of kinds with their payload types,
        and the built-in type tree (<code>Mu</code> → <code>Any</code> →
        <code>Cool</code> → …), are on <a href="types.html">Values and types</a>.</p>`,
    },
    {
      id: 'vm',
      title: 'The bytecode VM',
      body: `
        <p>The VM is a <strong>stack machine</strong>. An instruction pops its
        operands off an operand stack and pushes its result;
        <code>1 + $x</code> compiles to roughly <code>LoadConst</code>,
        <code>GetLocal</code>, <code>Add</code>. The dispatch loop is one large
        <code>match</code> over the current opcode in
        ${src('src/vm/vm_exec_dispatch.rs')}.</p>
        <h3>Frames and locals</h3>
        <p>A lexical variable the compiler can resolve lives in a numbered
        <strong>slot</strong>, and an opcode such as <code>GetLocal(3)</code> reads
        slot 3 directly. A call's slots are a window into one contiguous stack
        rather than a freshly allocated vector, so entering a routine costs no
        allocation. Names the compiler cannot resolve at compile time — dynamic
        variables, <code>our</code> variables, anything reached through
        <code>EVAL</code> — go through a by-name environment instead, which is the
        slower path.</p>
        <h3>The instruction set</h3>
        <ul>
          <li>There are several hundred opcodes. Most are small (load, store,
          compare, call), but some <strong>compound opcodes</strong> run a whole
          construct: <code>ForLoop</code>, <code>WhileLoop</code> and friends run
          their loop inside a single instruction, with no jump back through the
          dispatcher per iteration.</li>
          <li>An opcode is kept to <strong>48 bytes</strong> or less (a unit test
          pins it). A variant that needs more boxes its payload, so the instruction
          stream stays compact.</li>
          <li>Every opcode's dispatch arm carries a <code>// Cost:</code> comment
          stating its complexity and what it scales with. When that is worse than
          Rakudo's, the comment says so and cites a tracking issue.</li>
        </ul>
        <h3>Looking at it</h3>
        <ul>
          <li><code>mutsu --dump-bytecode file.raku</code> prints the chunks a program
          compiles to.</li>
          <li><code>mutsu --dump-ast file.raku</code> prints the AST.</li>
        </ul>
        <h3>The JIT</h3>
        <p>The JIT compiles a whole chunk once it is hot: after it has been called
        100 times by default (<code>MUTSU_JIT_THRESHOLD</code> changes that). It
        works in two tiers:</p>
        <ul>
          <li><strong>Tier A</strong> turns the opcode sequence into a native
          function that calls the interpreter's own helpers one after another, with
          branches as native control flow. That removes the dispatch loop.</li>
          <li><strong>Tier B</strong> expands the hottest opcodes — integer and float
          arithmetic, comparisons, local access — into inline machine code. It checks
          the NaN-box tag and falls back to a helper when the check fails.</li>
        </ul>
        <p>A chunk containing an opcode the JIT does not support simply stays
        interpreted. There are no guards, no deoptimization and no on-stack
        replacement, which keeps the JIT's behaviour identical to the interpreter's
        by construction.</p>
        <p class="manual-note">Every opcode, its operands, its description and its
        cost: <a href="opcodes.html">VM opcodes</a>.</p>`,
    },
    {
      id: 'gc',
      title: 'Memory and garbage collection',
      body: `
        <p>mutsu manages memory with <strong>reference counting plus a cycle
        collector</strong>. Reference counting frees almost everything the moment
        its last reference goes away. What it cannot free is a cycle — a closure
        that captures itself, an object whose child points back at it, a hash that
        contains itself — and ordinary Raku makes those easily. The cycle collector
        exists for them.</p>
        <h3>Gc&lt;T&gt;</h3>
        <p>A cycle-capable payload is a ${src('src/gc/gc_ptr.rs', 'Gc<T>')}, an
        <code>Arc</code> around a <code>GcBox</code> whose header holds a
        GC-visible strong count, a colour and a "buffered" flag. Types that cannot
        form a cycle (numbers, strings, <code>Seq</code>s) stay plain
        <code>Arc</code>s and cost the collector nothing: a numeric benchmark
        registers no GC work at all.</p>
        <h3>The algorithm: Bacon–Rajan trial deletion</h3>
        <ol>
          <li><strong>Candidates.</strong> When a <code>Gc</code> reference is dropped
          but others remain, the node <em>might</em> now be kept alive only by a
          cycle. It is coloured purple and recorded in a candidate buffer. The buffer
          holds weak handles, so being a candidate never keeps anything alive, and it
          is sharded 64 ways so threads do not contend on it.</li>
          <li><strong>Mark gray.</strong> From each candidate, walk the subgraph and
          subtract every internal edge from the counts.</li>
          <li><strong>Scan.</strong> A node whose count is still above zero is
          referenced from outside the subgraph. It and everything it reaches are
          restored (black). The rest are white.</li>
          <li><strong>Collect white.</strong> The white nodes are garbage that only
          referenced each other. Their finalizers run first, so a Raku
          <code>DESTROY</code> still sees the object's attributes. Then their edges
          are cut and ordinary reference counting frees them.</li>
        </ol>
        <p>The collector never needs to find the program's roots. It works purely
        from reference counts, so values held in Rust locals and on the VM stack are
        safe without being scanned. Nothing ever moves, which is also what lets the
        JIT hold raw pointers without stack maps or write barriers.</p>
        <h3>When it runs</h3>
        <p>A collection is triggered by the <strong>size of the candidate
        buffer</strong>: 16,384 candidates to start with, adjusted after every
        collection to twice the number of survivors (up to about a million). It
        runs synchronously, and only at a <strong>safepoint</strong>: a loop
        backedge, a call or return, an <code>await</code> and similar moments when
        the interpreter holds no borrow a collection could invalidate.</p>
        <h3>Threads</h3>
        <p>With several threads running, the collector requests a cooperative
        stop-the-world. Every other thread either parks at its next safepoint or
        is already inside a blocking wait. If the world does not stop within
        50 ms, the collector puts its candidates back and tries later; it never
        scans unsafely.</p>
        <h3>Knobs</h3>
        <table class="opt-table"><tbody>
          <tr><td><code>MUTSU_GC=off</code></td><td>turn the cycle collector off
          (reference counting still frees acyclic data)</td></tr>
          <tr><td><code>MUTSU_GC_THRESHOLD=N</code></td><td>the starting size of the
          candidate buffer that triggers a collection</td></tr>
          <tr><td><code>MUTSU_GC_LOG=summary</code></td><td>log each collection
          (<code>detail</code> and <code>trace</code> say more)</td></tr>
          <tr><td><code>MUTSU_GC_VERIFY=1</code></td><td>check the collector's
          invariants around every collection</td></tr>
          <tr><td><code>MUTSU_GC_EVERY_CANDIDATE=N</code></td><td>stress mode: collect
          every N candidates</td></tr>
        </tbody></table>
        <p>CI runs the whole test suite and the whole spec suite a second time with
        the collector forced to run far more often than usual and every invariant
        verified. A collector bug therefore shows up as a failing test, not as
        corrupted data in someone's program.</p>
        <h3>What was not chosen</h3>
        <p>A precise, moving, generational collector in the style of MoarVM was
        rejected: it would mean turning every <code>Value</code> into a handle the
        GC can update, which amounts to rewriting the VM. The reasoning, and what is
        still open, is recorded in
        ${adr('0001-gc-strategy-and-phasing.md', 'ADR-0001')} and
        ${adr('0003-default-on-gc-trigger.md', 'ADR-0003')}.</p>`,
    },
    {
      id: 'reading',
      title: 'Further reading',
      body: `
        <p>Large design decisions are recorded as ADRs in
        <a href="https://github.com/tokuhirom/mutsu/tree/main/docs/adr" rel="noopener"><code>docs/adr/</code></a>.
        The ones behind this page:</p>
        <ul>
          <li>${adr('0001-gc-strategy-and-phasing.md', 'ADR-0001')} — the GC strategy and
          the order of the GC → NaN-boxing → JIT work</li>
          <li>${adr('0003-default-on-gc-trigger.md', 'ADR-0003')} — when a collection is
          triggered</li>
          <li>${adr('0004-jit-strategy.md', 'ADR-0004')} — the Cranelift JIT and its
          tiers</li>
          <li>${adr('0005-nanbox-representation-encoding.md', 'ADR-0005')} — the NaN-box
          encoding</li>
          <li>${adr('0013-container-interior-mutability-cellvalue.md', 'ADR-0013')} — how
          shared containers are mutated in place</li>
          <li>${adr('0077-locals-are-a-window-into-a-contiguous-stack.md', 'ADR-0077')} —
          a call's locals as a window into one stack</li>
          <li>${adr('0110-typed-resolved-ir-for-statically-typed-routines.md', 'ADR-0110')}
          — the typed IR (TRIR)</li>
        </ul>
        <p>The contributor guide is the repository's
        <a href="https://github.com/tokuhirom/mutsu/blob/main/CLAUDE.md" rel="noopener"><code>CLAUDE.md</code></a>.</p>`,
    },
  ],
};
