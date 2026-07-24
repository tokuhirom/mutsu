/**
 * A tiny, dependency-free Markdown renderer for the batteries page.
 *
 * It renders the vendored upstream READMEs (content/batteries.json), which are
 * trusted repository content, not user input. Even so it builds DOM nodes with
 * textContent rather than assembling an HTML string, so no markup in a README
 * can ever become live nodes, and it sanitizes every link href to an allowed
 * scheme. The supported subset is what those READMEs actually use:
 *
 *   - fenced code blocks (``` … ```), with an optional language label
 *   - ATX headers (`#`..`######`) and setext headers (a line underlined with
 *     `===` → h2, `---` → h3 — h1 is reserved for the library name the page
 *     draws itself)
 *   - unordered lists (`*`, `-`, `+`), one level
 *   - blockquotes (`>`)
 *   - paragraphs
 *   - inline: `code`, **bold**, *italic*, [text](url); standalone badge/images
 *     (`![alt](url)`) are dropped as chrome
 *
 * It returns a DocumentFragment ready to append.
 */

const ALLOWED_SCHEME = /^(https?:|mailto:)/i;

/** Return a safe href, or null if the scheme is not allow-listed. */
function safeHref(raw) {
  const url = raw.trim();
  if (/^[a-z][a-z0-9+.-]*:/i.test(url)) {
    return ALLOWED_SCHEME.test(url) ? url : null;   // has a scheme: must be allowed
  }
  return url;                                        // relative / fragment: fine
}

/**
 * Render inline markup into `parent`. Order matters: code spans are extracted
 * first so their contents are never re-parsed, then images, links, bold, italic.
 */
function renderInline(parent, text) {
  // Split on inline code first; odd indices are the code contents.
  const parts = text.split(/`([^`]+)`/g);
  parts.forEach((chunk, i) => {
    if (i % 2 === 1) {
      const code = document.createElement('code');
      code.textContent = chunk;
      parent.appendChild(code);
    } else {
      renderInlineText(parent, chunk);
    }
  });
}

// Combined matcher, tried left to right: linked image (a badge: dropped),
// image (dropped), link, bold, italic. The two image forms carry no capture
// group, so the link/bold/italic groups keep indices 1..4.
const INLINE_RE =
  /\[!\[[^\]]*\]\([^)]*\)\]\([^)]*\)|!\[[^\]]*\]\([^)]*\)|\[([^\]]*)\]\(([^)]*)\)|\*\*([^*]+)\*\*|(?<![\w*])\*([^*\n]+)\*(?![\w*])/;

function renderInlineText(parent, text) {
  let rest = text;
  while (rest) {
    const m = INLINE_RE.exec(rest);
    if (!m) {
      parent.appendChild(document.createTextNode(rest));
      return;
    }
    if (m.index > 0) parent.appendChild(document.createTextNode(rest.slice(0, m.index)));
    const tok = m[0];
    if (tok.startsWith('[![') || tok.startsWith('![')) {
      // A badge/screenshot (or a linked badge): drop it — this is a doc reader,
      // not a mirror.
    } else if (tok.startsWith('[')) {
      const href = safeHref(m[2] || '');
      if (href) {
        const a = document.createElement('a');
        a.href = href;
        a.textContent = m[1] || href;
        if (/^https?:/i.test(href)) a.rel = 'noopener';
        parent.appendChild(a);
      } else {
        parent.appendChild(document.createTextNode(m[1] || ''));
      }
    } else if (tok.startsWith('**')) {
      const b = document.createElement('strong');
      renderInlineText(b, m[3]);
      parent.appendChild(b);
    } else {
      const em = document.createElement('em');
      renderInlineText(em, m[4]);
      parent.appendChild(em);
    }
    rest = rest.slice(m.index + tok.length);
  }
}

function heading(level, text) {
  const h = document.createElement('h' + Math.min(level, 6));
  renderInline(h, text);
  return h;
}

/** Render `src` (a Markdown string) into a DocumentFragment. */
export function renderMarkdown(src) {
  const frag = document.createDocumentFragment();
  const lines = String(src ?? '').replace(/\r\n?/g, '\n').split('\n');
  let i = 0;

  const flushList = (items) => {
    const ul = document.createElement('ul');
    for (const item of items) {
      const li = document.createElement('li');
      renderInline(li, item);
      ul.appendChild(li);
    }
    frag.appendChild(ul);
  };

  while (i < lines.length) {
    const line = lines[i];

    // blank
    if (/^\s*$/.test(line)) { i++; continue; }

    // fenced code
    const fence = /^\s*```+\s*([^\s`]*)/.exec(line);
    if (fence) {
      const buf = [];
      i++;
      while (i < lines.length && !/^\s*```+\s*$/.test(lines[i])) buf.push(lines[i++]);
      i++;                                            // consume the closing fence
      const pre = document.createElement('pre');
      const code = document.createElement('code');
      if (fence[1]) code.dataset.lang = fence[1];
      code.textContent = buf.join('\n');
      pre.appendChild(code);
      frag.appendChild(pre);
      continue;
    }

    // ATX header
    const atx = /^(#{1,6})\s+(.*?)\s*#*\s*$/.exec(line);
    if (atx) { frag.appendChild(heading(atx[1].length, atx[2])); i++; continue; }

    // setext header: text underlined by === (h2) or --- (h3)
    const next = lines[i + 1];
    if (next !== undefined && line.trim() && /^\s*(=+|-+)\s*$/.test(next) && !/^\s*[*+-]\s/.test(line)) {
      frag.appendChild(heading(next.trim()[0] === '=' ? 2 : 3, line.trim()));
      i += 2;
      continue;
    }

    // unordered list (one level; sub-bullets are flattened)
    if (/^\s*[*+-]\s+/.test(line)) {
      const items = [];
      while (i < lines.length && /^\s*[*+-]\s+/.test(lines[i])) {
        items.push(lines[i].replace(/^\s*[*+-]\s+/, ''));
        i++;
      }
      flushList(items);
      continue;
    }

    // blockquote
    if (/^\s*>\s?/.test(line)) {
      const buf = [];
      while (i < lines.length && /^\s*>\s?/.test(lines[i])) {
        buf.push(lines[i].replace(/^\s*>\s?/, ''));
        i++;
      }
      const bq = document.createElement('blockquote');
      renderInline(bq, buf.join(' '));
      frag.appendChild(bq);
      continue;
    }

    // paragraph: gather until a blank line or a block starter
    const buf = [];
    while (
      i < lines.length &&
      !/^\s*$/.test(lines[i]) &&
      !/^\s*```+/.test(lines[i]) &&
      !/^#{1,6}\s/.test(lines[i]) &&
      !/^\s*[*+-]\s+/.test(lines[i]) &&
      !/^\s*>\s?/.test(lines[i]) &&
      !(lines[i + 1] !== undefined && /^\s*(=+|-+)\s*$/.test(lines[i + 1]) && lines[i].trim())
    ) {
      buf.push(lines[i]);
      i++;
    }
    if (buf.length) {
      const p = document.createElement('p');
      renderInline(p, buf.join(' '));
      // A paragraph made only of badges renders to nothing — drop it so the
      // reader does not get a blank gap where a row of shields used to be.
      if (p.textContent.trim()) frag.appendChild(p);
    } else {
      i++;                                            // safety: never spin
    }
  }

  return frag;
}
