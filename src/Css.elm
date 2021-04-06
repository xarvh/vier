module Css exposing (..)


css =
    """
.row { display: flex; }
.col { display: flex; flex-direction: column; }

.mt { margin-top: 1em; }
.mb { margin-bottom: 1em; }
.ml { margin-left: 1em; }
.mr { margin-right: 1em; }

.p { padding: 1em; }

.header {
  font-family: sans;
  font-weight: bold;
}

.border { border: 1px solid lightgray; }

.thirds > * {
  max-width: 90vw;
}


body {
  background: #003;
  color: lightgray;
}

*, *::before, *::after {
  box-sizing: border-box;
}



.editor {
  border: 1px solid gray;
}


.editor-content {
  position: relative;
  background: #002;

  margin-left: 8px;
}

.editor-overlay, .editor-textarea {
  position: absolute;
  top: 0;
  left: 0;
  bottom: 0;
  right: 0;
  width: 100%;
}


.editor-overlay, .editor-textarea, .editor-line-numbers {
  height: 100%;

  margin: 0;
  resize: none;
  border: none;
  outline: none;
  background: none;

  padding: 2px;
  font-size: 14px;
  font-family: monospace;
  white-space: pre;

  caret-color: #ddd;
}

.editor-overlay {
  pointer-events: none;
}

.editor-line-numbers {
  text-align: right;
}

.mutable  { color: red; }
.comment  { color: #00d; }
.literal  { color: green; }
.valueUp  { color: #aff; }
.valueLo  { color: #ddd; }
.globalUp { color: #499; }
.globalLo { color: #999; }
.op       { color: red; }
.keyword  { color: #ffdd07; }
.paren    { color: red; }

    """
