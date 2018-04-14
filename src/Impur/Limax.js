"use strict"

exports.limax = function(string) {
    var limax = require('limax');
    return limax(string, {tone: false});
};