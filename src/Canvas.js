"use strict";

// module Canvas

exports.get2DContext = function(canvasId) {
  return function() {
    return document.getElementById(canvasId).getContext('2d');
  };
};

exports.initContext = function(color) {
  return function initContext(context) {
    return function() {
      context.beginPath();
      context.lineWidth = 2;
      context.strokeStyle = color;
      return context;
    };
  };
};

exports.beginStroke = function(context) {
  return function() {
    context.beginPath();
    return context;
  };
};

exports.endStroke = function(context) {
  return function() {
    context.stroke();
    return context;
  };
};

exports.lineTo = function(x) {
  return function(y) {
    return function (context) {
      return function() {
        context.lineTo(x,y);
        return context;
      };
    };
  };
};

exports["drawFilledArc'"] = function(fillStyle) {
  return function (x) {
    return function(y) {
      return function(r) {
        return function(angleStart) {
          return function(angleEnd) {
            return function(context) {
              return function() {
                context.arc(x, y, r, angleStart, angleEnd);
                context.fillStyle = fillStyle;
                context.fill();
                return context;
              };
            };
          };
        };
      };
    };
  };
};

exports.moveTo = function(x) {
  return function(y) {
    return function (context) {
      return function() {
        context.moveTo(x,y);
        return context;
      };
    };
  };
};

exports.setStrokeStyle = function(style) {
  return function (context) {
    return function() {
      context.strokeStyle = style;
      return context;
    };
  };
};
