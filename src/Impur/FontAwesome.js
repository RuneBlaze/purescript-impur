var fontawesome = require('@fortawesome/fontawesome');
// var solid       = require('@fortawesome/fontawesome-free-solid').default;
var brands      = require('@fortawesome/fontawesome-free-brands').default;

// fontawesome.library.add(solid);
fontawesome.library.add(brands);

exports.faIconRaw = function(icon) {
    return fontawesome.icon({prefix: 'fab', iconName: icon}).html;
};

exports.faCss = fontawesome.dom.css();