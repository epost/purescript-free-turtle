module Canvas where

import Control.Monad.Eff
import Language (Distance (), Angle ())

foreign import data Context2D :: *
foreign import data DOM :: !

type Context2DEff = Eff (dom :: DOM) Context2D

type ColorString = String

foreign import get2DContext
  """
  function get2DContext(canvasId) {
    return function() {
      return document.getElementById(canvasId).getContext('2d');
    };
  }
  """ :: forall eff. String -> Context2DEff

foreign import initContext
  """
  function initContext(color) {
    return function initContext(context) {
      return function() {
        context.beginPath();
        context.lineWidth = 2;
        context.strokeStyle = color;
        return context;
      };
    }
  }
  """ :: forall eff. ColorString -> Context2D -> Context2DEff

foreign import beginStroke
  """
  function beginStroke(context) {
    return function() {
      context.beginPath();
      return context;
    };
  }
  """ :: forall eff. Context2D -> Context2DEff

foreign import endStroke
  """
  function endStroke(context) {
    return function() {
      context.stroke();
      return context;
    };
  }
  """ :: forall eff. Context2D -> Context2DEff

foreign import lineTo
  """
  function lineTo(x) {
    return function(y) {
      return function (context) {
        return function() {
          //console.log('executing lineTo(', x, ',', y, ')');
          context.lineTo(x,y);
          return context;
        };
      };
    };
  }
  """ :: Distance -> Distance -> Context2D -> Context2DEff

foreign import drawArc
  """
  function drawArc(x) {
    return function(y) {
      return function(r) {
        return function(angleStart) {
          return function(angleEnd) {
            return function(context) {
              return function() {
                context.arc(x, y, r, angleStart, angleEnd);
                return context;
              };
            };
          };
        };
      };
    };
  }
  """ :: Distance -> Distance -> Distance -> Angle -> Angle -> Context2D -> Context2DEff

foreign import moveTo
  """
  function moveTo(x) {
    return function(y) {
      return function (context) {
        return function() {
          //console.log('executing moveTo(', x, ',', y, ')');
          context.moveTo(x,y);
          return context;
        };
      };
    };
  }
  """ :: Distance -> Distance -> Context2D -> Context2DEff

foreign import setStrokeStyle
  """
  function setStrokeStyle(style) {
    return function (context) {
      return function() {
        context.strokeStyle = style;
        return context;
      };
    };
  }
  """ :: String -> Context2D -> Context2DEff
