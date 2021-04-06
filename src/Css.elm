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
  background: #151515;
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
  background: #151515;

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

  caret-color: #c6b6ee;
}

.editor-overlay {
  pointer-events: none;
}

.editor-line-numbers {
  text-align: right;
}

/* from https://github.com/metalelf0/jellybeans-nvim/blob/main/lua/lush_theme/jellybeans-nvim.lua */
.valueLo  { color: #81c0df; }
.globalLo { color: #81c0df; text-decoration: underline; }

.valueUp  { color: #fad07a; }
.globalUp { color: #fad07a; text-decoration: underline; }

.comment  { color: #888888; }

.literal  { color: #de6240; }

.mutable  { color: #fe99c0; }
.op       { color: #ffcd6; }
.keyword  { color: #ee8eff; }
.paren    { color: #4f9631; }


    """
