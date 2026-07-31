/**
 * A deliberately small Raku-flavoured tokenizer: it is a highlighter, not a
 * parser, so it aims to be right on ordinary code and never to throw.
 */

const KEYWORDS = new Set([
  'my', 'our', 'has', 'state', 'let', 'temp', 'constant', 'sub', 'method', 'multi', 'only', 'proto',
  'submethod', 'class', 'role', 'grammar', 'token', 'rule', 'regex', 'module', 'package', 'enum',
  'subset', 'unit', 'if', 'elsif', 'else', 'unless', 'with', 'without', 'while', 'until', 'for', 'loop',
  'repeat', 'given', 'when', 'default', 'return', 'return-rw', 'last', 'next', 'redo', 'do', 'try',
  'CATCH', 'CONTROL', 'BEGIN', 'END', 'INIT', 'ENTER', 'LEAVE', 'FIRST', 'LAST', 'use', 'need', 'require',
  'import', 'is', 'does', 'where', 'returns', 'of', 'handles', 'start', 'await', 'react', 'whenever',
  'supply', 'gather', 'take', 'say', 'print', 'put', 'note', 'die', 'fail', 'so', 'not', 'and', 'or',
  'andthen', 'orelse', 'xor', 'eq', 'ne', 'lt', 'gt', 'le', 'ge', 'cmp', 'leg', 'x', 'xx', 'div', 'mod',
  'gcd', 'lcm', 'elem', 'cont', 'self', 'sort', 'map', 'grep', 'first', 'reduce', 'new',
]);

const TOKEN_RE = new RegExp([
  /(?<comment>#.*)/,
  /(?<pod>^=\w+[\s\S]*?^=end\s+\w+$|^=\w+.*$)/,
  /(?<qstring>\b(?:q|qq|qw|rx|m|s|tr)\s*(?:\:\w+\s*)*[/{|(<[][\s\S]*?[/}|)>\]])/,
  /(?<string>"(?:[^"\\]|\\.)*"|'(?:[^'\\]|\\.)*'|<(?:[^<>\n])*>)/,
  /(?<number>\b\d[\d_]*(?:\.\d+)?(?:e[-+]?\d+)?\b|\bpi\b|\bInf\b|\bNaN\b)/,
  /(?<variable>[$@%&][.!^*?:=~]?[\w'-]+|\$[/!_0-9]|\$\^\w+)/,
  /(?<word>[A-Za-z_][\w-]*)/,
  /(?<op>[-+*/~%^?!<>=|&.,;:\\()[\]{}]+)/,
].map(r => r.source).join('|'), 'gm');

export function escapeHtml(s) {
  return s.replace(/[&<>]/g, c => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;' }[c]));
}

export function highlight(src) {
  let out = '';
  let last = 0;
  TOKEN_RE.lastIndex = 0;
  let m;
  while ((m = TOKEN_RE.exec(src)) !== null) {
    if (m.index > last) out += escapeHtml(src.slice(last, m.index));
    const g = m.groups;
    const text = escapeHtml(m[0]);
    if (g.comment || g.pod) out += `<span class="tok-comment">${text}</span>`;
    else if (g.qstring || g.string) out += `<span class="tok-string">${text}</span>`;
    else if (g.number) out += `<span class="tok-number">${text}</span>`;
    else if (g.variable) out += `<span class="tok-var">${text}</span>`;
    else if (g.word) {
      if (KEYWORDS.has(m[0])) out += `<span class="tok-keyword">${text}</span>`;
      else if (/^[A-Z]/.test(m[0])) out += `<span class="tok-type">${text}</span>`;
      else out += text;
    } else if (g.op) out += `<span class="tok-op">${text}</span>`;
    else out += text;
    last = m.index + m[0].length;
    if (m[0].length === 0) TOKEN_RE.lastIndex++;   // never loop forever
  }
  out += escapeHtml(src.slice(last));
  return out;
}
