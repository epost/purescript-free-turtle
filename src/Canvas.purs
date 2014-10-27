module Canvas where

import Control.Monad.Eff

foreign import data Context2D :: *
foreign import data DOM :: !

type Context2DEff = Eff (dom :: DOM) Context2D


foreign import get2DContext
  """
  function get2DContext(canvasId) {
    return function() {
      return document.getElementById(canvasId).getContext('2d');
    };
  }
  """ :: forall eff. String -> Context2DEff


foreign import beginStroke
  """
  function beginStroke(context) {
    return function() {
      context.beginPath();
      context.lineWidth = 2;
      context.strokeStyle = 'purple';
      context.moveTo(0, 0);
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
  """ :: Number -> Number -> Context2D -> Context2DEff
