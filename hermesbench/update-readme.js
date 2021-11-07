#! /usr/bin/node
const csvToMd = require('csvtomd-lib');
const {promises: {readFile, writeFile}} = require("fs");
const magic = require('markdown-magic');
const path = require('path');

const markdownPath = path.join(__dirname, '../README.md');

Promise.all([
  readFile('bench.csv'),
  readFile('bench_threaded.csv')
]).then(([bench, benchT]) => {
  const benchTable = csvToMd.fromString(bench.toString());
  const benchTTable = csvToMd.fromString(benchT.toString());
  const config = {
    transforms: {
      BENCHES(content, options) {
        return benchTable;
      },
      BENCHES_THREADED(content, options) {
        return benchTTable;
      }
    }
  };
  magic(markdownPath, config)
});
