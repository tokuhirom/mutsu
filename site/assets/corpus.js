/**
 * Parser for the plain-text snippet corpora (content/lessons.txt,
 * content/highlights.txt).  Shared by the site and by
 * scripts/check-site-snippets.mjs, so the browser and the verifier can never
 * disagree about what a snippet is.
 *
 * Format:
 *
 *     #== <group>/<id> [flag ...]
 *     <code lines>
 *     #-- expect
 *     <expected output lines>
 *
 * Anything before the first "#==" is a file comment and is ignored.
 *
 * The only flag so far is `no-browser`: the snippet runs natively but not in
 * the WebAssembly build (`start` needs real threads, which wasm32 has none of),
 * so the site shows its recorded output instead of offering to run it.
 */

const START = /^#== +(\S+)((?: +[\w-]+)*)\s*$/;
const EXPECT = /^#-- expect\s*$/;

/** Drop leading/trailing blank lines, keeping interior ones. */
function trimBlank(lines) {
  let a = 0;
  let b = lines.length;
  while (a < b && lines[a].trim() === '') a++;
  while (b > a && lines[b - 1].trim() === '') b--;
  return lines.slice(a, b);
}

/**
 * @returns {Array<{group: string, id: string, key: string, flags: string[],
 *                  code: string, expect: string}>}
 */
export function parseCorpus(text) {
  const entries = [];
  let current = null;
  let target = null;   // 'code' | 'expect'

  for (const line of text.split('\n')) {
    const start = START.exec(line);
    if (start) {
      const key = start[1];
      const slash = key.indexOf('/');
      current = {
        group: slash < 0 ? key : key.slice(0, slash),
        id: slash < 0 ? key : key.slice(slash + 1),
        key,
        flags: start[2].split(/\s+/).filter(Boolean),
        code: [],
        expect: [],
      };
      entries.push(current);
      target = 'code';
      continue;
    }
    if (!current) continue;             // file header
    if (EXPECT.test(line)) { target = 'expect'; continue; }
    current[target].push(line);
  }

  return entries.map(e => ({
    group: e.group,
    id: e.id,
    key: e.key,
    flags: e.flags,
    code: trimBlank(e.code).join('\n'),
    expect: trimBlank(e.expect).join('\n'),
  }));
}

/** Group parsed entries in file order: [{group, items: [...]}, ...]. */
export function groupCorpus(entries) {
  const groups = [];
  for (const e of entries) {
    let g = groups.find(g => g.group === e.group);
    if (!g) groups.push(g = { group: e.group, items: [] });
    g.items.push(e);
  }
  return groups;
}

/** Re-emit a corpus, replacing every expectation.  Used by --update. */
export function renderCorpus(header, entries, expectations) {
  const out = [header.replace(/\n+$/, ''), ''];
  for (const e of entries) {
    out.push(`#== ${[e.key, ...(e.flags || [])].join(' ')}`);
    out.push(e.code);
    const expect = expectations.get(e.key);
    if (expect !== undefined && expect !== '') {
      out.push('#-- expect');
      out.push(expect);
    }
    out.push('');
  }
  return out.join('\n');
}
