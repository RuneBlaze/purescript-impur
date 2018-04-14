exports.highlight = function(lang) {
    return function(code) {
        var hljs = require('highlight.js');
        return hljs.highlight(lang, code, true).value;
    };
};