\title Tricks for ALD

@comment{Internal undocumented tricks}

# Generate from Prolog

TODO: write a self-contained example, keep code in a runnable

~~~
```ciao_runnable
%! \begin{dynpreview}
{
  render_pred: 'bundles_dyn:render',
  state_hash: true,
  depends: ['ciaowasm', 'website', 'lpdoc'],
  on_init: ['use_module(catalog_ui(bundles_dyn))']
}
%! \end{dynpreview}
```
~~~

# Inline HTML

~~~
```ciao_runnable
%! \begin{jseval}
async (pg) => {
  let e = pg.preview_el;
  e.innerHTML = `
<table style="border: 1px solid red">
  <tr style="background-color: #444;">
    <td style="color: red;">Red</td>
    <td style="color: green;">Green</td>
    <td style="color: blue;">Blue</td>
  </tr>
  <tr>
    <td style="font-size: 20px;">16</td>
    <td style="font-size: 20px;">14</td>
    <td style="font-size: 20px;">10</td>
  </tr>
</table>
  `;
  pg.show_preview(true);
  pg.update_inner_layout();
}
%! \end{jseval}
```
~~~

# Change slide fonts with custom CSS

~~~
```ciao_runnable
%! \begin{jseval}
async(pgcell) => {
const cssString = `
@import url(https://fonts.googleapis.com/css?family=PT+Sans);
@import url(https://fonts.googleapis.com/css?family=Yanone+Kaffeesatz);

.lpdoc-slide-mode body {
  /* font-family: 'Nunito Sans'; */
  font-family: 'PT Sans';
}
.lpdoc-slide-mode h1,
.lpdoc-slide-mode h2,
.lpdoc-slide-mode h3 {
  font-family: 'Yanone Kaffeesatz';
  font-weight: 400;
  margin-bottom: 0;
}
`;
  const style_el = document.createElement("style");
  style_el.textContent = cssString;
  document.head.appendChild(style_el);
}
%! \end{jseval}
```
~~~

# Effects with custom CSS

Make the Ciao logo swing.
~~~
```ciao_runnable
%! \begin{jseval}
async(pgcell) => {
const cssString = `
@keyframes rotate {
  from {
    transform: rotate(-10deg);
  }
  to {
    transform: rotate(10deg);
  }
}
#logo {
  animation: rotate 1s ease infinite alternate;
}
`;
  const style_el = document.createElement("style");
  style_el.textContent = cssString;
  document.head.appendChild(style_el);
}
%! \end{jseval}
```
~~~

# Change width in pixels (before scaling)

~~~
```ciao_runnable
%! \begin{jseval}
async(pgcell) => {
const cssString = `
.lpdoc-slide-mode .lpdoc-main {
  width: 1000px;
}
`;
  const style_el = document.createElement("style");
  style_el.textContent = cssString;
  document.head.appendChild(style_el);
  pgcell.pgset.main_doc.update_dimensions();
}
%! \end{jseval}
```
~~~

# Automatic presentation mode (even from playground)

~~~
```ciao_runnable
%! \begin{jseval}
async(pgcell) => {
  if (main_doc !== pgcell.pgset.main_doc) toggle_presentation(main_doc.pgset.cells[0]);
}
%! \end{jseval}
```
~~~

# Automatic slide mode 

~~~
```ciao_runnable
%! \begin{jseval}
async(pgcell) => {
  pgcell.pgset.main_doc.set_slide_mode(true);
}
%! \end{jseval}
```
~~~

# TODO: Interact with runnables

TODO: Load some runnable

TODO: Load some query

TODO: Fill runnable data

## Generate PDF for slides from the command line

You can use Chrome headless to do that:
```
chrome --headless --no-pdf-header-footer --print-to-pdf=example.pdf foo.html/index.html 
```

NOTE: On macOS the Chrome binary can be accessed from `/Applications/Google\ Chrome.app/Contents/MacOS/Google\ Chrome`

