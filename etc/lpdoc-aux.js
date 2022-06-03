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

/* clean div contents */
function clean_div(elm) {
  while (elm.hasChildNodes()) {
    elm.removeChild(elm.firstChild);
  }
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
var clicked_dropdown;
var active_dropdown;

/* close dropdown menu */
window.addEventListener('click', e => {
  if (active_dropdown === undefined) return;
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
  constructor(menu, btn_el, items, onchange) {
    this.values = Array.from(items);
    var c = this.#setup(btn_el, onchange);
    menu.appendChild(c);
  }
  #setup(btn_el, onchange) {
    var c = elem_cn('div', 'dropdown');
    var dropdown = elem_cn('div', 'dropdown-content');
    var b;
    b = elem_cn('button', 'dropdown-btn');
    b.onclick = e => { 
      active_dropdown = dropdown;
      clicked_dropdown = true;
      dropdown.classList.toggle("show");
    };
    b.appendChild(btn_el);
    c.appendChild(b);
    //
    for (const item of this.values) {
      var li = document.createElement('a');
      li.innerHTML = item.n;
      li.onclick = () => {
        onchange(item.k);
      };
      item.el = li;
      dropdown.appendChild(item.el);
    }
    c.appendChild(dropdown);
    return c;
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
  update_theme_hook();
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
};

/* =========================================================================== */
/* (LPdoc specific) */

(function() {
  /* --------------------------------------------------------------------------- */
  /* LPdoc - Toogle sidebar, adjust lpdoc-nav, add theme button */

  /* Toogle sidebar (for mobile-friendly) */
  /* NOTE: need sidebar and sidebar-toogle-button */
  function setup_toggle() {
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
        var dummy = document.createElement('div');
        const theme_button =
              new DropdownButton(dummy,
                                 elem_from_str('<span style="margin-right: 10px;">&#9788;</span>'),
                                 theme_list,
                                 value => {
                                   theme_button.highlight(value);
                                   theme_set_value(value);
                                   update_theme_hook();
                                 });
        nav_el.prepend(dummy.firstChild);
        theme_button.highlight(theme_get_value());
      }
    } else {
      /* Horiz menu (e.g., website layout)? */
      toggle_el = document.getElementsByClassName('lpdoc-horiz-menu')[0];
      if (toggle_el !== null) { /* ok */
        toggle_class = 'lpdoc-horiz-menu-toggled';
      } else {
	return; /* Nothing to be toggled */
      }
    }
    trigger.addEventListener('click', function(e) {
      e.preventDefault();
      toggle_el.classList.toggle(toggle_class); 
      return false;
    });
  }

  window.addEventListener('DOMContentLoaded', function(){
    setup_toggle();
  });
})();

/* =========================================================================== */
/* LPdoc - seachbox */

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
