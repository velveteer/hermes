const csvToMd = require('csvtomd-lib');
const {promises: {readFile, writeFile}} = require("fs");
const magic = require('markdown-magic');
const path = require('path');

const markdownPath = path.join(__dirname, '../README.md');

Promise.all([
  readFile('bench.csv'),
]).then(([bench]) => {
  const benchTable = csvToMd.fromString(bench.toString());
  const config = {
    transforms: {
      BENCHES(content, options) {
        return benchTable;
      }
    }
  };
  magic(markdownPath, config)
});
