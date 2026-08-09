"use strict";

// module Canvas

export const get2DContext = (canvasId) => () => {
  return document.getElementById(canvasId).getContext('2d');
};

export const initContext = (color) => (context) => () => {
  context.lineWidth = 2;
  context.strokeStyle = color;
  return context;
};

export const beginPath = (context) => () => {
  context.beginPath();
  return context;
};

export const closePath = (context) => () => {
  context.closePath();
  return context;
};

export const stroke = (context) => () => {
  context.stroke();
  return context;
};

export const lineTo = (x) => (y) => (context) => () => {
  context.lineTo(x,y);
  return context;
};

export const drawFilledArcForeign = (fillStyle) => (x) => (y) => (r) => (angleStart) => (angleEnd) => (context) => () => {
  context.arc(x, y, r, angleStart, angleEnd);
  context.fillStyle = fillStyle;
  context.fill();
  return context;
};

export const moveTo = (x) => (y) => (context) => () => {
  context.moveTo(x,y);
  return context;
};

export const setStrokeStyle = (style) => (context) => () => {
  context.strokeStyle = style;
  return context;
};
