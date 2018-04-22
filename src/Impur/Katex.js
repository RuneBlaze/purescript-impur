exports.renderToStringNullable = function(str) {
    return function(displayMode) {
        var katex = require('katex');
        try {
            return katex.renderToString(str, {displayMode: displayMode});
        } catch(error) {
            return null;
        }
    }
}