/*
 * LPdoc UI basic elements
 *
 * (c) 2018-2022 The Ciao Development Team
 * 
 * This is a JavaScript support library for LPdoc HTML backend and
 * Ciao playgrounds.
 *
 * NOTE: Keep this library as simple as possible so that documents
 * loads quickly.
 */

/* =========================================================================== */
/* Dynamic imports */

function importScript(src, is_async = false) {
  let el = document.createElement('script');
  el.src = src;
  el.async = is_async;
  document.head.appendChild(el);
}
/* (as a promise) */
function tryImportScript(src, is_async = false) {
  return new Promise((resolve, reject) => {
    let el = document.createElement('script');
    el.onload = () => resolve(el);
    el.onerror = () => reject(new Error(`could not load ${src}`));
    el.async = is_async;
    el.src = src; // (make sure onload is set before, it may be cached)
    document.head.append(el);
  });
}

function importCSS(src) {
  let el = document.createElement('link');
  el.rel = 'stylesheet';
  el.type = "text/css";
  el.href = src;
  document.head.appendChild(el);
}

/* =========================================================================== */
/* DOM helper */

/* element from str */
function elem_from_str(str) {
  const el = document.createElement("div");
  el.innerHTML = str;
  return el.firstElementChild;
}
/* element with class */
function elem_cn(kind, cn) {
  const el = document.createElement(kind);
  el.className = cn;
  return el;
}

/* create a button */
function btn(style, title, text, onclick) {
  const e = elem_cn('button', style);
  e.title = title;
  e.innerHTML = text;
  e.onclick = onclick;
  return e;
}

/* =========================================================================== */
/* UI - Dropdown menus */

// TODO: avoid globals?
var clicked_dropdown = false;
var active_dropdown = null;

/* close dropdown menu */
window.addEventListener('click', e => {
  if (active_dropdown === null) return;
  if (clicked_dropdown === true) { // skip first click
    clicked_dropdown = false;
    return;
  }
  if (active_dropdown.classList.contains('show')) {
    active_dropdown.classList.remove('show');
    active_dropdown = null;
  }
});

class DropdownButton {
  constructor(menu, title, btn_text_el, items, onchange) {
    var c = elem_cn('div', 'dropdown');
    var dropdown = elem_cn('div', 'dropdown-content');
    var b = elem_cn('button', 'dropdown-btn');
    this.btn_el = b;
    b.title = title;
    b.onclick = e => { 
      if (active_dropdown !== null) { // close other
        active_dropdown.classList.toggle("show");
      }
      if (active_dropdown !== dropdown) { // open new
        dropdown.classList.toggle("show");
        active_dropdown = dropdown;
      } else { // keep closed
        active_dropdown = null;
      }
      clicked_dropdown = true;
    };
    b.appendChild(btn_text_el);
    c.appendChild(b);
    // Add items and create links
    this.values = [];
    for (const i of items) {
      let item = { k:i.k, n:i.n };
      var li = document.createElement('a');
      li.innerHTML = item.n;
      li.onclick = () => {
        onchange(item.k);
      };
      item.el = li;
      this.values.push(item);
      dropdown.appendChild(item.el);
    }
    c.appendChild(dropdown);
    menu.appendChild(c);
  }

  get_item(value) {
    for (const item of this.values) {
      if (item.k === value) {
        return item;
      }
    }
    return null;
  }

  highlight(value) {
    for (const item of this.values) {
      item.el.style.fontWeight = 'normal';
    }
    this.get_item(value).el.style.fontWeight = 'bold';
  }

  update_marks(filter) {
    for (const item of this.values) {
      const mark = filter(item.k) ? "&#10004;" : "&nbsp;";
      item.el.innerHTML = "<span class='dropdown-mark'>" + mark + "</span> " + item.n;
    }
  }
}

/* =========================================================================== */
/* SVGs (Note: clone with .cloneNode(true) if needed) */

const theme_svg = elem_from_str(`<svg class="header-icon-img" viewBox="0 0 32 32" xmlns="http://www.w3.org/2000/svg">
<rect height="3" width="2" x="15" y="2" fill="currentColor"/>
<rect height="2" width="3" x="27" y="15" fill="currentColor"/>
<rect height="3" width="2" x="15" y="27" fill="currentColor"/>
<rect height="2" width="3" x="2" y="15" fill="currentColor"/>
<rect height="3" transform="translate(-3 7.23) rotate(-45)" width="2" x="6.22" y="5.73" fill="currentColor"/>
<rect height="2" transform="translate(2.14 19.63) rotate(-45)" width="3" x="23.27" y="6.23" fill="currentColor"/>
<rect height="3" transform="translate(-10.26 24.77) rotate(-45)" width="2" x="23.77" y="23.27" fill="currentColor"/>
<polygon points="5.47 25.13 7.59 23 9 24.42 6.88 26.54 5.47 25.13" fill="currentColor"/>
<path d="M16,8a8,8,0,1,0,8,8A8,8,0,0,0,16,8Zm0,14a6,6,0,0,1,0-12Z" fill="currentColor"/>
</svg>`);
const search_svg = elem_from_str(`<svg class="header-icon-img" version="1.1" viewBox="0 0 512 512" xml:space="preserve" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">
<path d="M344.5,298c15-23.6,23.8-51.6,23.8-81.7c0-84.1-68.1-152.3-152.1-152.3C132.1,64,64,132.2,64,216.3  c0,84.1,68.1,152.3,152.1,152.3c30.5,0,58.9-9,82.7-24.4l6.9-4.8L414.3,448l33.7-34.3L339.5,305.1L344.5,298z M301.4,131.2  c22.7,22.7,35.2,52.9,35.2,85c0,32.1-12.5,62.3-35.2,85c-22.7,22.7-52.9,35.2-85,35.2c-32.1,0-62.3-12.5-85-35.2  c-22.7-22.7-35.2-52.9-35.2-85c0-32.1,12.5-62.3,35.2-85c22.7-22.7,52.9-35.2,85-35.2C248.5,96,278.7,108.5,301.4,131.2z" fill="currentColor"/>
</svg>`);
search_svg.style.marginRight = '0px';

/* =========================================================================== */
/* UI - Themes */

/* CSS UI themes (see lpdoc.css) */
var theme_list = [
  { k:'system', n:'System' },
  { k:'light', n:'Light' },
  { k:'dark', n:'Dark' }
];

/* Hook function to be called when theme needs to be updated */
var update_theme_hook = null;

/* Update theme when on dark/light events */
window.matchMedia('(prefers-color-scheme: dark)').addEventListener('change', e => {
  update_theme_hook();
});
/* Update as soon as DOM is there */
window.addEventListener('DOMContentLoaded', (event) => {
  update_theme_hook(); // TODO: this happens too late and causes flickering with ciao_playground.js
});

/* Get theme from local storage */
function theme_get_value() {
  try {
    var v = window.localStorage.getItem('theme');
    if (v === null) return 'system';
    return v;
  } catch (e) {
    return 'system';
  }
}

/* Update theme in local storage */
function theme_set_value(value) {
  try {
    if (value === 'system') {
      window.localStorage.removeItem('theme');
    } else {
      window.localStorage.setItem('theme', value);
    }
  } catch (e) {
    return;
  }
}

/* Get actual theme (based on selection and user UI preferences) */
function get_actual_theme() {
  var theme = theme_get_value();
  if (theme === 'system') {
    if (window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches) {
      theme = 'dark';
    } else {
      theme = 'light';
    }
  }
  return theme;
}

function update_css_theme() {
  document.documentElement.setAttribute('data-theme', get_actual_theme());
}

/* (default theme update hook) */
update_theme_hook = () => {
  update_css_theme();
  if (typeof github_stars_el !== 'undefined') {
    update_github_stars_theme();
  }
};

function new_theme_button(base_el) {
  const theme_button =
        new DropdownButton(base_el,
                           "Change theme",
                           theme_svg.cloneNode(true),
                           theme_list,
                           value => {
                             theme_button.highlight(value);
                             theme_set_value(value);
                             update_theme_hook();
                           });
  theme_button.highlight(theme_get_value());
  return theme_button;
}

/* =========================================================================== */
/* (LPdoc specific) */

// TODO: Generate better HTML, avoid patching
(function() {
  /* --------------------------------------------------------------------------- */
  /* LPdoc - Toogle sidebar, adjust lpdoc-nav, add theme button, patch search button */

  /* Toogle sidebar (for mobile-friendly) */
  /* NOTE: need sidebar and sidebar-toogle-button */
  function setup_nav() {
    var trigger = document.getElementById('sidebar-toggle-button');
    if (trigger === null) {
      return; /* no trigger */
    }
    /* Detect element to be toggled and its class */
    var toggle_el;
    var toggle_class;
    var nav_el;
    /* LPdoc sidebar? */
    toggle_el = document.getElementById('sidebar');
    if (toggle_el !== null) { /* ok */
      toggle_class = 'sidebar-toggled';
      /* find nav */
      var nav_el = document.getElementsByClassName('lpdoc-nav')[0];
      if (nav_el !== null) {
        /* ensure that it appears in the main div */
        if (nav_el.parentNode === toggle_el) {
          /* move nav to main */
          var main_el = document.getElementsByClassName('lpdoc-main')[0];
          main_el.prepend(nav_el);
          /* remove leftover <hr> from sidebar */
          if (toggle_el.firstChild.tagName === 'HR') {
            toggle_el.removeChild(toggle_el.firstChild);
          }
        }
        nav_el.classList.add('lpdoc-sidebar');
        nav_el.style.padding="10px 0px 0px 0px"; /* amend padding */

        /* Insert theme selection button */
        let dummy = document.createElement('div');
        let thm_btn_el = new_theme_button(dummy);
        nav_el.prepend(dummy.firstChild);
      }
    } else {
      /* Horiz menu (e.g., website layout)? */
      toggle_el = document.getElementsByClassName('lpdoc-horiz-menu')[0];
      if (toggle_el !== null) { /* ok */
        toggle_class = 'lpdoc-horiz-menu-toggled';
        /* Insert theme selection button */
        let li = document.createElement('li');
        let thm_btn_el = new_theme_button(li);
        toggle_el.prepend(li);
        setup_github_stars(toggle_el);
      } else {
	return; /* Nothing to be toggled */
      }
    }
    trigger.addEventListener('click', function(e) {
      e.preventDefault();
      toggle_el.classList.toggle(toggle_class); 
      return false;
    });
    /* Patch search button */
    function patch_search(e) { if (e.innerHTML === "🔍") { e.innerHTML = ""; e.appendChild(search_svg.cloneNode(true)); } }
    for (const e of document.getElementsByClassName('lpdoc-navbutton')) { patch_search(e); }
    for (const e of document.getElementsByClassName('lpdoc-searchmenu')) { patch_search(e); }
  }

  window.addEventListener('DOMContentLoaded', function(){
    setup_nav();
  });
})();

/* =========================================================================== */
/* LPdoc - interactive index search */

(function() {
  /* Index search */
  var timeout=null;
  var results_div=null;
  var text_input=null;

  function do_search(text) {
    var ntext = text.toLowerCase();
    results_div.innerHTML = ""; // clear results (so that query selector works)
    if (ntext == "") return;

    var time0 = new Date();
    var elems = document.querySelectorAll('a[search_kwd]');
    var what;
    var res = [];
    if (ntext == "_") { /* all */
      elems.forEach(function(elem) {
	res.push(elem.parentNode);
      });
      what = "showing all entries";
    } else if (ntext.match(/^[a-z0-9]$/i) !== null) { /* single alphanum character, only prefix */
      elems.forEach(function(elem) {
	var kwd = elem.getAttribute('search_kwd');
	if (kwd.startsWith(ntext)) {
	  res.push(elem.parentNode);
	}
      });
      what = "only prefixes";
    } else { /* first prefix, then substring */
      elems.forEach(function(elem) {
	var kwd = elem.getAttribute('search_kwd');
	if (kwd.startsWith(ntext)) {
	  res.push(elem.parentNode);
	}
      });
      elems.forEach(function(elem) {
	var kwd = elem.getAttribute('search_kwd');
	if (!kwd.startsWith(ntext) && kwd.includes(ntext)) {
	  res.push(elem.parentNode);
	}
      });
      what = "prefixes and substrings";
    }
    var time1 = new Date();
    var time_diff = time1-time0; // in ms

    if (res.length === 0) {
      results_div.innerHTML = "<br><b>No results found</b>";
    } else {
      var rbody = "";
      res.forEach(function(elem) {
	rbody += "<li>" + elem.innerHTML + "</li>";
      });
      results_div.innerHTML =
	"<br>" + 
	"<b>" + res.length + " results (" + time_diff + " ms)</b> " +
	"for " + what + ":" +
	"<ul>"+rbody+"</ul>";
    }
  }

  function setup_search() {
    /* Find search divs */
    text_input = document.getElementById('search-input');
    results_div = document.getElementById("search-results");
    idx_div = document.getElementById("search-index");

    if (text_input == null) return; /* No search div */

    for(var i=0,els=idx_div.querySelectorAll('a'); i<els.length; i++) {
      var elid = els[i].id;
      if (elid != "") {
	els[i].setAttribute('search_kwd',elid.toLowerCase());
      }
    }

    /* Search from hash (if any) */
    var hash=decodeURIComponent(window.location.hash.substring(1));
    text_input.value = hash;
    do_search(hash);

    text_input.select(); /* select input automatically */

    /* Search as we type */
    text_input.onkeydown = function (e) {
      clearTimeout(timeout);
      timeout = setTimeout(function() {
	do_search(text_input.value); }, 100); 
    };
  }

  window.addEventListener('DOMContentLoaded', function(){
    setup_search();
  });
})();

/* =========================================================================== */

/* Official github buttons (See https://buttons.github.io/) */
/* Hack to support light/dark themes: we create two buttons and set visibility based on theme */
function setup_github_stars(base_el) {
  if (typeof github_stars_el !== 'undefined') return; /* already setup */
  importScript('https://buttons.github.io/buttons.js', true);
  function b(m) { // Append an initially hidden gh star to base_el with style 'm' and
    const elb = elem_from_str(`<a class='github-button' style='display: none;' href='https://github.com/ciao-lang/ciao' data-color-scheme='no-preference: ${m}; light: ${m}; dark: ${m};' data-size='large' data-show-count='true' aria-label='Star ciao-lang/ciao on GitHub'>Star</a>`);
    const el = document.createElement('div');
    el.style.padding = '6px 0 0 0';
    el.style.marginLeft = '16px';
    el.style.display = 'none';
    el.appendChild(elb);
    base_el.appendChild(el);
    return el;
  }
  window.github_stars_el = {};
  window.github_stars_el['dark'] = b('dark');
  window.github_stars_el['light'] = b('light');
  update_github_stars_theme();
}

function update_github_stars_theme() {
  const dark = get_actual_theme() === 'dark';
  window.github_stars_el['dark'].style.display = dark ? "inline-block" : "none";
  window.github_stars_el['light'].style.display = (!dark) ? "inline-block" : "none";
}

/* =========================================================================== */
/* Load playground functionality if needed */

var urlPREFIX = null; /* global */

function load_playground() {
  (async() => {
    const pg = '/playground/js/ciao_playground.js';
    const prefixes = [];
    // Default prefix (assume playground is fetched from the same location)
    // Due to CORS restrictions this only works properly when accessing from HTTP
    // (not file:///).
    prefixes.push('');
    // // Site symlink from local docs // TODO: due to CORS restrictions loading wasm from file:/// may not work
    // prefixes.push('site');
    // Local server (for development)
    if (window.location.protocol === 'file:') {
      prefixes.push('http://localhost:8001'); // (see ciao-serve-mt)
    }
    // ciao-lang site
    prefixes.push('https://ciao-lang.org'); // TODO: missing version // TODO: add some CDN
    for (const p of prefixes) {
      urlPREFIX = p;
      try {
        await tryImportScript(urlPREFIX+pg);
        console.log(`{loaded playground from '${urlPREFIX+pg}'}`);
        break;
      } catch(e) {
      }
    }
  })();
}

if (typeof lpdocPG !== 'undefined') {
  // Enable playground now
  load_playground();
} else {
  // Wait for document to be loaded and load playground only if some
  // some runnable codeblock is detected
  document.addEventListener("DOMContentLoaded", function() {
    if (typeof lpdocPG === 'undefined') {
      const runnables = document.getElementsByClassName("lpdoc-codeblock-runnable");
      if (runnables.length > 0) {
        lpdocPG = 'runnable';
        load_playground();
      }
    }
  });
}
