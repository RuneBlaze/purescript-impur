exports.renderToStringNullable = function(str) {
    var katex = require('katex');
    try {
        return katex.renderToString(str);
    } catch(error) {
        return null;
    }
}