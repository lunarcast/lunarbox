"use strict"

const marked = require("marked")

/**
 * Parse a string of markdown to html
 *
 * @param {String} input The string to parse.
 */
exports.parseMarkdown = (input) =>
  marked(input, {
    sanitize: true
  })
