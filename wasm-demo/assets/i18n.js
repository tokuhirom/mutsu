/**
 * Language selection and the shared page chrome (nav + footer).
 *
 * The picked language lives in localStorage and in the `?lang=` query
 * parameter, so a shared URL keeps the language it was read in.  Pages listen
 * for the `langchange` event and re-render; nothing reloads.
 */

export const LANGS = ['en', 'ja'];

const STRINGS = {
  en: {
    'nav.home': 'Home',
    'nav.tutorial': 'Tutorial',
    'nav.playground': 'Playground',
    'nav.repl': 'REPL',
    'nav.bench': 'Benchmarks',
    'nav.github': 'GitHub',

    'footer.tagline': 'mutsu — a Raku interpreter written in Rust, running here as WebAssembly.',
    'footer.credit': 'The tutorial and its examples follow the official Raku documentation ' +
      '(Raku/doc), which this repository vendors as <code>raku-doc/</code> and uses under the ' +
      'Artistic License 2.0.',
    'footer.docs': 'Official Raku docs',
    'footer.doc-repo': 'Raku/doc repository',
    'footer.repo': 'mutsu on GitHub',

    'run': 'Run ▶',
    'run.running': 'Running…',
    'run.loading': 'Loading the interpreter…',
    'run.hint': 'Ctrl+Enter',
    'reset': 'Reset code',
    'editor': 'Editor',
    'output': 'Output',
    'output.empty': 'Press Run to execute this snippet.',
    'output.none': '(no output)',
    'expected.summary': 'Expected output',
    'verdict.ok': '✓ matches the expected output',
    'verdict.differs': '≠ differs from the expected output — that is fine while you experiment',
    'no-browser.note': 'This example needs something the WebAssembly build ' +
      'does not have — so it cannot run here. The output below is what it prints when ' +
      'you run it with mutsu (or Rakudo) on your machine.',
    'no-browser.output': 'Output (recorded from a native run)',

    'lesson.editor': 'Editor',
    'lesson.prev': '← Previous',
    'lesson.next': 'Next →',
    'lesson.of': 'Lesson {n} of {total}',

    'toc.title': 'Contents',
    'toc.reset-progress': 'Clear progress',

    'pg.title': 'Playground',
    'pg.intro': 'Write a program, run it, read what it printed. Every run starts from a ' +
      'clean interpreter, exactly like running a file.',
    'pg.pick': 'Pick an example, edit it, then press Run.',
    'pg.loaded': 'Loaded from a shared link.',
    'pg.share': 'Share link',
    'pg.share.copied': 'Link copied to clipboard',
    'pg.share.hash': 'The link is in the address bar',
    'pg.repl-cta': 'Want a session that remembers what you declared? Try the REPL →',

    'repl.title': 'REPL',
    'repl.intro': 'One line at a time, in a session that keeps everything you declare. ' +
      'An unfinished line waits for the rest.',
    'repl.pane': 'Session',
    'repl.loading': 'Loading the mutsu WASM module…',
    'repl.ready': 'mutsu — a Raku interpreter in Rust, compiled to WebAssembly.',
    'repl.ready2': 'Type an expression and press Enter. Everything you declare stays in scope.',
    'repl.hint': 'Enter runs · ↑ / ↓ recalls · Ctrl+L clears the screen',
    'repl.reset': 'Reset session',
    'repl.clear': 'Clear screen',
    'repl.was-reset': 'Session reset — every variable and declaration is gone.',
    'repl.load-failed': 'Failed to load the WASM module: {msg}',
    'repl.fatal': 'WASM error: {msg}',
    'repl.fatal-hint': 'This session cannot continue — press "Reset session" for a fresh one.',
    'repl.try': 'Try',
    'repl.pg-cta': 'Writing something longer than a line? Open the playground →',
  },
  ja: {
    'nav.home': 'ホーム',
    'nav.tutorial': 'チュートリアル',
    'nav.playground': 'プレイグラウンド',
    'nav.repl': 'REPL',
    'nav.bench': 'ベンチマーク',
    'nav.github': 'GitHub',

    'footer.tagline': 'mutsu — Rust で書かれた Raku インタプリタ。このページでは WebAssembly として動いています。',
    'footer.credit': 'チュートリアルと例文は公式 Raku ドキュメント（Raku/doc）を参考にしています。' +
      '本リポジトリは <code>raku-doc/</code> としてそれを同梱し、Artistic License 2.0 のもとで利用しています。',
    'footer.docs': '公式 Raku ドキュメント',
    'footer.doc-repo': 'Raku/doc リポジトリ',
    'footer.repo': 'mutsu (GitHub)',

    'run': '実行 ▶',
    'run.running': '実行中…',
    'run.loading': 'インタプリタを読み込み中…',
    'run.hint': 'Ctrl+Enter',
    'reset': 'コードを戻す',
    'editor': 'エディタ',
    'output': '出力',
    'output.empty': '「実行」を押すとこのコードが動きます。',
    'output.none': '（出力なし）',
    'expected.summary': '期待される出力',
    'verdict.ok': '✓ 期待どおりの出力です',
    'verdict.differs': '≠ 期待される出力とは違います（自由に書き換えて試している場合はこれで OK）',
    'no-browser.note': 'この例は WebAssembly 版にない機能を必要とするため、' +
      'ここでは実行できません。下の出力は、手元の環境で mutsu（または Rakudo）で実行したときのものです。',
    'no-browser.output': '出力（ネイティブ実行の記録）',

    'lesson.editor': 'エディタ',
    'lesson.prev': '← 前へ',
    'lesson.next': '次へ →',
    'lesson.of': 'レッスン {n} / {total}',

    'toc.title': '目次',
    'toc.reset-progress': '進捗を消す',

    'pg.title': 'プレイグラウンド',
    'pg.intro': 'プログラムを書いて、実行して、出力を読む。実行のたびにインタプリタは' +
      'まっさらな状態から始まります（ファイルを実行するのと同じです）。',
    'pg.pick': '例を選んで書き換え、「実行」を押してください。',
    'pg.loaded': '共有リンクから読み込みました。',
    'pg.share': 'リンクを共有',
    'pg.share.copied': 'リンクをクリップボードにコピーしました',
    'pg.share.hash': 'リンクをアドレスバーに入れました',
    'pg.repl-cta': '宣言したものが残るセッションが欲しいなら REPL へ →',

    'repl.title': 'REPL',
    'repl.intro': '1 行ずつ試すためのセッション。宣言したものはそのまま残り、' +
      '途中の行は閉じるまで待ってくれます。',
    'repl.pane': 'セッション',
    'repl.loading': 'mutsu の WASM モジュールを読み込み中…',
    'repl.ready': 'mutsu — Rust で書かれた Raku インタプリタ（WebAssembly 版）。',
    'repl.ready2': '式を入力して Enter。宣言したものはこのセッションに残ります。',
    'repl.hint': 'Enter で実行 · ↑ / ↓ で履歴 · Ctrl+L で画面を消去',
    'repl.reset': 'セッションを初期化',
    'repl.clear': '画面を消去',
    'repl.was-reset': 'セッションを初期化しました。変数も宣言もすべて消えています。',
    'repl.load-failed': 'WASM モジュールの読み込みに失敗しました: {msg}',
    'repl.fatal': 'WASM エラー: {msg}',
    'repl.fatal-hint': 'このセッションは継続できません。「セッションを初期化」を押してください。',
    'repl.try': '例',
    'repl.pg-cta': '1 行に収まらないものを書くならプレイグラウンドへ →',
  },
};

const STORAGE_KEY = 'mutsu-site-lang';

function detect() {
  const fromUrl = new URLSearchParams(location.search).get('lang');
  if (LANGS.includes(fromUrl)) return fromUrl;
  try {
    const saved = localStorage.getItem(STORAGE_KEY);
    if (LANGS.includes(saved)) return saved;
  } catch { /* private mode */ }
  return (navigator.language || 'en').toLowerCase().startsWith('ja') ? 'ja' : 'en';
}

let lang = detect();

export function currentLang() {
  return lang;
}

export function setLang(next) {
  if (!LANGS.includes(next) || next === lang) return;
  lang = next;
  try { localStorage.setItem(STORAGE_KEY, next); } catch { /* private mode */ }
  const url = new URL(location.href);
  url.searchParams.set('lang', next);
  history.replaceState(null, '', url);
  document.documentElement.lang = next;
  window.dispatchEvent(new CustomEvent('langchange', { detail: next }));
}

/** Look up a UI string, with `{name}` placeholders. */
export function t(key, vars) {
  let s = (STRINGS[lang] && STRINGS[lang][key]) ?? STRINGS.en[key] ?? key;
  if (vars) for (const [k, v] of Object.entries(vars)) s = s.replaceAll(`{${k}}`, v);
  return s;
}

/** Keep in-page links on the current language. */
export function langHref(path) {
  return `${path}?lang=${lang}`;
}

const NAV = [
  { key: 'nav.home', href: 'index.html', page: 'home' },
  { key: 'nav.tutorial', href: 'tutorial.html', page: 'tutorial' },
  { key: 'nav.playground', href: 'playground.html', page: 'playground' },
  { key: 'nav.repl', href: 'repl.html', page: 'repl' },
  { key: 'nav.bench', href: 'bench-trend.html', page: 'bench' },
];

/**
 * Render the shared header and footer into `<nav class="site-nav">` and
 * `<footer class="site-footer">`, and keep them in sync with the language.
 */
export function renderChrome(activePage) {
  const nav = document.querySelector('.site-nav');
  const footer = document.querySelector('.site-footer');

  function paint() {
    document.documentElement.lang = lang;
    if (nav) {
      nav.innerHTML = '';
      const brand = document.createElement('a');
      brand.className = 'brand';
      brand.href = langHref('index.html');
      brand.textContent = 'mutsu';
      nav.appendChild(brand);

      const links = document.createElement('div');
      links.className = 'nav-links';
      for (const item of NAV) {
        const a = document.createElement('a');
        a.href = item.page === 'bench' ? item.href : langHref(item.href);
        a.textContent = t(item.key);
        if (item.page === activePage) a.setAttribute('aria-current', 'page');
        links.appendChild(a);
      }
      const gh = document.createElement('a');
      gh.href = 'https://github.com/tokuhirom/mutsu';
      gh.textContent = t('nav.github');
      gh.rel = 'noopener';
      links.appendChild(gh);
      nav.appendChild(links);

      const spacer = document.createElement('span');
      spacer.className = 'spacer';
      nav.appendChild(spacer);

      const sw = document.createElement('div');
      sw.className = 'lang-switch';
      sw.setAttribute('role', 'group');
      sw.setAttribute('aria-label', 'Language');
      for (const l of LANGS) {
        const b = document.createElement('button');
        b.type = 'button';
        b.dataset.lang = l;
        b.textContent = l === 'ja' ? '日本語' : 'English';
        b.setAttribute('aria-pressed', String(l === lang));
        b.addEventListener('click', () => setLang(l));
        sw.appendChild(b);
      }
      nav.appendChild(sw);
    }

    if (footer) {
      footer.innerHTML = '';
      const tagline = document.createElement('p');
      tagline.textContent = t('footer.tagline');
      footer.appendChild(tagline);

      // The credit carries a <code> element, so it is the one string built
      // from markup rather than textContent.  It is a literal from STRINGS,
      // never user input.
      const credit = document.createElement('p');
      credit.innerHTML = t('footer.credit');
      footer.appendChild(credit);

      const links = document.createElement('p');
      const parts = [
        ['https://docs.raku.org/', t('footer.docs')],
        ['https://github.com/Raku/doc', t('footer.doc-repo')],
        ['https://github.com/tokuhirom/mutsu', t('footer.repo')],
      ];
      parts.forEach(([href, text], i) => {
        if (i) links.appendChild(document.createTextNode(' · '));
        const a = document.createElement('a');
        a.href = href;
        a.textContent = text;
        a.rel = 'noopener';
        links.appendChild(a);
      });
      footer.appendChild(links);
    }
  }

  paint();
  window.addEventListener('langchange', paint);
}
