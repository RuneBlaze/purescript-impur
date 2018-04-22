var fontawesome = require('@fortawesome/fontawesome');
var solid       = require('@fortawesome/fontawesome-free-solid').default;
fontawesome.library.add(solid);

exports.faIconRaw = function(icon) {
    return fontawesome.icon({prefix: 'fas', iconName: icon}).html;
};

exports.faCss = fontawesome.dom.css();