/*
* Reads and transpiles gulpfile.ts into an in-memory gulpfile.js
*/

var fs = require('fs');
var tsc = require('typescript');

var typescript = fs.readFileSync("./gulpfile.ts").toString();
var javascript = tsc.transpile(typescript);

eval(javascript);
