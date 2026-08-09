"use strict";

// module Canvas

export const get2DContext = function(canvasId) {
  return function() {
    return document.getElementById(canvasId).getContext('2d');
  };
};

export const initContext = function(color) {
  return function initContext(context) {
    return function() {
      context.lineWidth = 2;
      context.strokeStyle = color;
      return context;
    };
  };
};

export const beginPath = function(context) {
  return function() {
    context.beginPath();
    return context;
  };
};

export const closePath = function(context) {
  return function() {
    context.closePath();
    return context;
  };
};

export const stroke = function(context) {
  return function() {
    context.stroke();
    return context;
  };
};

export const lineTo = function(x) {
  return function(y) {
    return function (context) {
      return function() {
        context.lineTo(x,y);
        return context;
      };
    };
  };
};

export const drawFilledArcForeign = function(fillStyle) {
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

export const moveTo = function(x) {
  return function(y) {
    return function (context) {
      return function() {
        context.moveTo(x,y);
        return context;
      };
    };
  };
};

export const setStrokeStyle = function(style) {
  return function (context) {
    return function() {
      context.strokeStyle = style;
      return context;
    };
  };
};
