(() => {
  const cfg = JSON.parse(document.getElementById('sdiff-config').textContent);
  const key = 'sdiff:' + cfg.ref + ':' + cfg.head;
  const load = () => { try { return JSON.parse(localStorage.getItem(key)) || {}; } catch (e) { return {}; } };
  const state = Object.assign({ notes: [], verdict: 'comment', summary: '' }, load());
  let previewed = null;
  const save = () => { try { localStorage.setItem(key, JSON.stringify(state)); } catch (e) {} };
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

  const verdict = el('select', { onchange: e => { state.verdict = e.target.value; changed(); } },
    ...['comment', 'approve', 'request-changes'].map(v => el('option', { value: v }, v.replace('-', ' '))));
  verdict.value = state.verdict;
  const summary = el('textarea', { rows: 3, placeholder: 'Review summary (markdown)', oninput: e => { state.summary = e.target.value; changed(); } });
  summary.value = state.summary;
  const list = el('ol', { class: 'sd-notes' });
  const out = el('div', { class: 'sd-out' });
  const postBtn = el('button', { type: 'button', class: 'sd-post', disabled: '' }, 'Post review');
  const count = el('span', { class: 'sd-count' });
  const panel = el('aside', { class: 'sd-panel' },
    el('div', { class: 'sd-head', onclick: () => panel.classList.toggle('sd-open') }, el('strong', {}, 'Review ' + cfg.ref), count),
    el('div', { class: 'sd-body' },
      cfg.author ? el('p', { class: 'sd-hint' }, 'Author: ' + cfg.author + '. GitHub only allows a comment review on your own pull request.') : null,
      el('label', {}, 'Verdict ', verdict), summary, list,
      el('div', { class: 'sd-row' },
        el('button', { type: 'button', onclick: preview }, 'Preview'), postBtn),
      out));
  document.body.append(panel);
  postBtn.addEventListener('click', post);

  function changed() { previewed = null; postBtn.disabled = true; out.textContent = ''; save(); render(); }

  function render() {
    count.textContent = state.notes.length + (state.notes.length === 1 ? ' note' : ' notes');
    list.replaceChildren(...state.notes.map((n, i) => el('li', {},
      el('code', {}, n.file.split('/').pop() + ' · ' + where(n)), el('div', {}, n.body),
      el('button', { type: 'button', class: 'sd-quiet', onclick: () => { state.notes.splice(i, 1); changed(); } }, 'remove'))));
  }

  async function call(path, body) {
    const res = await fetch(path, { method: 'POST', headers: { 'Content-Type': 'application/json', 'X-Sdiff-Token': cfg.token }, body: JSON.stringify(body) });
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
      postBtn.disabled = false;
    } catch (e) { out.textContent = 'Draft failed: ' + e.message; }
  }

  async function post() {
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
