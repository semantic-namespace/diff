(async () => {
  const cfg = JSON.parse(document.getElementById('sdiff-config').textContent);
  const remote = await fetch('/state?ref=' + encodeURIComponent(cfg.ref)).then(r => r.json()).catch(() => ({}));
  const settings = Object.assign({ 'show-inferred': true, 'fold-viewed-forms': true, 'fold-viewed-files': true, 'mark-file-on-github': false, 'fold-cosmetic-files': false, 'skip-deps': false }, remote.settings);
  const state = Object.assign({ forms: {}, notes: [], verdict: 'comment', summary: '' }, remote.review);
  const legacyKey = 'sdiff:' + cfg.ref + ':' + cfg.head;
  try {
    const old = JSON.parse(localStorage.getItem(legacyKey) || 'null');
    if (old && old.notes && old.notes.length && !state.notes.length) Object.assign(state, { notes: old.notes, verdict: old.verdict || state.verdict, summary: old.summary || state.summary });
    localStorage.removeItem(legacyKey);
  } catch (e) {}
  let previewed = null;
  let saveTimer = null;
  const post = (path, body) => fetch(path, { method: 'POST', headers: { 'Content-Type': 'application/json', 'X-Sdiff-Token': cfg.token }, body: JSON.stringify(body) });
  const save = () => { clearTimeout(saveTimer); saveTimer = setTimeout(() => post('/state', { pr: cfg.ref, review: { forms: state.forms, notes: state.notes, verdict: state.verdict, summary: state.summary } }).catch(() => {}), 300); };
  const el = (tag, attrs = {}, ...kids) => {
    const e = document.createElement(tag);
    for (const [k, v] of Object.entries(attrs)) k === 'class' ? e.className = v : k.startsWith('on') ? e.addEventListener(k.slice(2), v) : e.setAttribute(k, v);
    for (const k of kids) if (k != null) e.append(k);
    return e;
  };
  const where = n => n.form + (n.at ? ' › ' + n.at : '');

  function openEditor(anchor, target) {
    if (anchor.nextElementSibling && anchor.nextElementSibling.classList.contains('sd-editor')) return;
    const ta = el('textarea', { rows: 3, placeholder: 'Note on ' + where(target) });
    const box = el('div', { class: 'sd-editor' }, ta,
      el('div', { class: 'sd-row' },
        el('button', { type: 'button', onclick: () => {
          if (!ta.value.trim()) return;
          state.notes.push(Object.assign({}, target, { body: ta.value.trim() }));
          box.remove(); changed();
        } }, 'Add note'),
        el('button', { type: 'button', class: 'sd-quiet', onclick: () => box.remove() }, 'Cancel')));
    anchor.after(box);
    ta.focus();
  }

  document.querySelectorAll('section.form[data-form]').forEach(sec => {
    const base = { file: sec.dataset.file, form: sec.dataset.form };
    const h3 = sec.querySelector('h3');
    h3.append(el('button', { type: 'button', class: 'sd-add', title: 'Comment on ' + base.form, onclick: () => openEditor(h3, base) }, '💬 comment'));
    sec.querySelectorAll(':scope > ul.changes > li[data-at]').forEach(li => {
      const at = Object.assign({}, base, { at: li.dataset.at });
      (li.querySelector(':scope > .p') || li).append(el('button', { type: 'button', class: 'sd-add', title: 'Comment on ' + where(at),
        onclick: () => openEditor(li, at) }, '💬'));
    });
  });

  const levels = [
    { sel: 'article.file', head: ':scope > h2' },
    { sel: 'section.form', head: ':scope > h3' }];
  const allOf = sel => [...document.querySelectorAll(sel)].filter(x => !x.closest('.sd-panel'));
  levels.forEach(({ sel, head }) => allOf(sel).forEach(box => {
    const h = box.querySelector(head);
    if (!h) return;
    h.classList.add('sd-fold');
    h.title = 'Click to fold; Alt+click folds or unfolds all';
    h.addEventListener('click', e => {
      if (e.target.closest('button, a, .sd-editor')) return;
      const fold = !box.classList.contains('sd-folded');
      (e.altKey ? allOf(sel) : [box]).forEach(b => b.classList.toggle('sd-folded', fold));
      e.preventDefault();
    });
  }));
  allOf('details > summary').forEach(sm => sm.addEventListener('click', e => {
    if (!e.altKey) return;
    e.preventDefault();
    const open = !sm.parentElement.open;
    allOf('details').forEach(d => { d.open = open; });
  }));

  const viewedBoxes = {};
  async function setGithubViewed(path, want) {
    const v = viewedBoxes[path]; if (!v) return;
    v.label.classList.add('sd-busy');
    try {
      const res = await post('/viewed', { pr: cfg.ref, path, viewed: want });
      const data = await res.json();
      if (!res.ok) throw new Error(data.error || res.statusText);
      setViewed(path, data.state);
      if (want && v.onView && settings['fold-viewed-files']) v.onView();
    } catch (e) { v.cb.checked = !want; alert('GitHub did not accept the change: ' + e.message); }
    v.label.classList.remove('sd-busy');
  }
  function viewedBox(path, onView) {
    const cb = el('input', { type: 'checkbox' });
    const label = el('label', { class: 'sd-viewed', title: 'Viewed on GitHub' }, cb, ' Viewed');
    label.addEventListener('click', e => e.stopPropagation());
    cb.addEventListener('change', () => setGithubViewed(path, cb.checked));
    viewedBoxes[path] = { cb, label, onView };
    return label;
  }
  function setViewed(path, state) {
    const v = viewedBoxes[path]; if (!v) return;
    v.cb.checked = state === 'VIEWED';
    v.label.classList.toggle('sd-dismissed', state === 'DISMISSED');
    v.label.title = state === 'DISMISSED' ? 'Changed since you marked it viewed' : 'Viewed on GitHub';
  }
  allOf('article.file[data-file]').forEach(a => {
    const h2 = a.querySelector(':scope > h2');
    if (h2) h2.append(viewedBox(a.dataset.file, () => a.classList.add('sd-folded')));
  });
  allOf('details.other li[data-file]').forEach(li => li.append(viewedBox(li.dataset.file)));
  fetch('/viewed?ref=' + encodeURIComponent(cfg.ref)).then(r => r.json()).then(states => {
    for (const [path, state] of Object.entries(states)) {
      setViewed(path, state);
      if (state === 'VIEWED' && settings['fold-viewed-files']) { const a = document.querySelector('article.file[data-file="' + CSS.escape(path) + '"]'); if (a) a.classList.add('sd-folded'); }
    }
  }).catch(() => {});

  const formBoxes = {};
  const formStatus = k => {
    const info = cfg.forms[k]; if (!info) return 'none';
    const stored = k in state.forms ? state.forms[k] : (info.was ? state.forms[info.was] : undefined);
    return stored === undefined ? 'unviewed' : stored === info.fp ? 'viewed' : 'changed';
  };
  function renderForm(k) {
    const b = formBoxes[k], st = formStatus(k);
    b.cb.checked = st === 'viewed';
    b.label.classList.toggle('sd-dismissed', st === 'changed');
    b.label.title = st === 'changed' ? 'The code changed since you marked this form viewed' : 'Viewed (kept on this machine)';
  }
  function fileComplete(file) {
    const ks = Object.keys(formBoxes).filter(k => formBoxes[k].file === file);
    if (!settings['mark-file-on-github'] || !ks.length || !ks.every(k => formStatus(k) === 'viewed')) return;
    const v = viewedBoxes[file];
    if (v && !v.cb.checked) { v.cb.checked = true; setGithubViewed(file, true); }
  }
  allOf('section.form[data-form]').forEach(sec => {
    const k = sec.dataset.file + '|' + sec.dataset.form;
    if (!cfg.forms[k]) return;
    const cb = el('input', { type: 'checkbox' });
    const label = el('label', { class: 'sd-viewed sd-form-viewed' }, cb, ' Viewed');
    label.addEventListener('click', e => e.stopPropagation());
    cb.addEventListener('change', () => {
      const info = cfg.forms[k];
      if (cb.checked) { state.forms[k] = info.fp; if (info.was) delete state.forms[info.was]; }
      else { delete state.forms[k]; if (info.was) delete state.forms[info.was]; }
      renderForm(k); save(); render();
      if (cb.checked && settings['fold-viewed-forms']) sec.classList.add('sd-folded');
      if (cb.checked) fileComplete(sec.dataset.file);
    });
    sec.querySelector(':scope > h3').append(label);
    formBoxes[k] = { cb, label, sec, file: sec.dataset.file };
    renderForm(k);
  });
  function applyFolds() {
    document.body.classList.toggle('sd-hide-inferred', !settings['show-inferred']);
    if (settings['fold-viewed-forms']) Object.entries(formBoxes).forEach(([k, b]) => { if (formStatus(k) === 'viewed') b.sec.classList.add('sd-folded'); });
    if (settings['fold-cosmetic-files']) allOf('article.file.comments-only, article.file.whitespace-only, article.file.rename-only').forEach(a => a.classList.add('sd-folded'));
  }
  applyFolds();

  const status = cfg.status || {};
  const own = status.author && status.author === status.viewer;
  const approved = status.mine && status.mine.state === 'APPROVED';
  const locked = status.state === 'merged' || status.state === 'closed';
  const verdictLabel = v => v === 'approve' && approved ? 'approve (already approved ' + status.mine.at + ')'
                         : (v === 'approve' || v === 'request-changes') && own ? v.replace('-', ' ') + ' (not on your own PR)'
                         : v.replace('-', ' ');
  const verdictAllowed = v => !((v === 'approve' && (approved || own)) || (v === 'request-changes' && own));
  const verdict = el('select', { onchange: e => { state.verdict = e.target.value; changed(); } },
    ...['comment', 'approve', 'request-changes'].map(v => el('option', Object.assign({ value: v }, verdictAllowed(v) ? {} : { disabled: '' }), verdictLabel(v))));
  if (!verdictAllowed(state.verdict)) state.verdict = 'comment';
  verdict.value = state.verdict;
  const standing = [status.state ? 'This PR is ' + status.state + (status['merged-at'] ? ' (' + status['merged-at'] + ')' : '') + '.' : '',
    own ? 'It is your own, so GitHub only allows a comment review.' : '',
    status.mine ? 'You ' + status.mine.state.toLowerCase().replace('_', ' ') + ' it on ' + status.mine.at + '.' : '',
    locked ? 'Nothing more can be posted here.' : ''].filter(Boolean).join(' ');
  const summary = el('textarea', { rows: 3, placeholder: 'Review summary (markdown)', oninput: e => { state.summary = e.target.value; changed(); } });
  summary.value = state.summary;
  const list = el('ol', { class: 'sd-notes' });
  const out = el('div', { class: 'sd-out' });
  const postBtn = el('button', { type: 'button', class: 'sd-post', disabled: '' }, 'Post review');
  const count = el('span', { class: 'sd-count' });
  const panel = el('aside', { class: 'sd-panel' },
    el('div', { class: 'sd-head', onclick: () => panel.classList.toggle('sd-open') }, el('strong', {}, 'Review ' + cfg.ref), count),
    el('div', { class: 'sd-body' },
      standing ? el('p', { class: 'sd-hint sd-standing' }, standing) : null,
      el('p', { class: 'sd-hint' }, 'Click a file or form heading to fold it. Alt+click folds or unfolds every one at that level, and Alt+click on a source toggle opens or closes them all.'),
      el('label', {}, 'Verdict ', verdict), summary, list,
      el('details', { class: 'sd-settings' }, el('summary', {}, 'Settings'),
        ...[['show-inferred', 'Show inferred notes'],
            ['fold-viewed-forms', 'Fold forms I mark viewed'],
            ['fold-viewed-files', 'Fold files viewed on GitHub'],
            ['mark-file-on-github', 'Mark a file viewed on GitHub when all its forms are viewed'],
            ['fold-cosmetic-files', 'Fold files that only change formatting, comments or names'],
            ['skip-deps', 'Skip dependency analysis (it fetches the whole repository at base and head)']].map(([k, label]) => {
          const cb = el('input', { type: 'checkbox' });
          cb.checked = !!settings[k];
          cb.addEventListener('change', () => {
            settings[k] = cb.checked;
            post('/settings', { settings }).catch(() => {});
            applyFolds();
          });
          return el('label', {}, cb, ' ' + label);
        })),
      el('div', { class: 'sd-row' },
        el('button', { type: 'button', onclick: preview }, 'Preview'), postBtn),
      out));
  document.body.append(panel);
  postBtn.addEventListener('click', postReview);

  function changed() { previewed = null; postBtn.disabled = true; out.textContent = ''; save(); render(); }

  function render() {
    const ks = Object.keys(formBoxes);
    count.textContent = state.notes.length + (state.notes.length === 1 ? ' note' : ' notes') +
      (ks.length ? ' · ' + ks.filter(k => formStatus(k) === 'viewed').length + '/' + ks.length + ' forms viewed' : '');
    list.replaceChildren(...state.notes.map((n, i) => el('li', {},
      el('code', {}, n.file.split('/').pop() + ' · ' + where(n)), el('div', {}, n.body),
      el('button', { type: 'button', class: 'sd-quiet', onclick: () => { state.notes.splice(i, 1); changed(); } }, 'remove'))));
  }

  async function call(path, body) {
    const res = await post(path, body);
    const data = await res.json();
    if (!res.ok) throw new Error(data.error || res.statusText);
    return data;
  }
  const request = () => ({ pr: cfg.ref, verdict: state.verdict, summary: state.summary, notes: state.notes });

  async function preview() {
    out.textContent = 'Drafting…';
    try {
      const req = request();
      const { payload, placed } = await call('/draft', req);
      previewed = JSON.stringify(req);
      out.replaceChildren(
        el('ul', { class: 'sd-placed' }, ...placed.map(p => el('li', {},
          p.inline ? el('span', { class: 'sd-inline' }, 'inline ' + p.inline.path.split('/').pop() + ':' + (p.inline.start_line ? p.inline.start_line + '–' : '') + p.inline.line)
                   : el('span', { class: 'sd-inbody' }, 'in summary: ' + p.body),
          ' ← ' + where(p.note)))),
        el('details', {}, el('summary', {}, payload.event + ' payload'), el('pre', {}, JSON.stringify(payload, null, 2))));
      postBtn.disabled = locked;
      if (locked) out.append(el('p', { class: 'sd-hint' }, 'Preview only: the PR is ' + status.state + '.'));
    } catch (e) { out.textContent = 'Draft failed: ' + e.message; }
  }

  async function postReview() {
    if (previewed !== JSON.stringify(request())) { out.textContent = 'The review changed since the preview. Preview again.'; return; }
    if (!confirm('Post a ' + state.verdict.replace('-', ' ') + ' review with ' + state.notes.length + ' notes to ' + cfg.ref + ' as your GitHub user?')) return;
    postBtn.disabled = true;
    out.textContent = 'Posting…';
    try {
      const res = await call('/post', request());
      out.replaceChildren('Posted (' + res.state + '): ', el('a', { href: res.url, target: '_blank' }, res.url));
      state.notes = []; state.summary = ''; summary.value = ''; save(); render();
    } catch (e) { out.textContent = 'Post failed: ' + e.message; postBtn.disabled = false; }
  }

  render();
  if (state.notes.length) panel.classList.add('sd-open');
})();
