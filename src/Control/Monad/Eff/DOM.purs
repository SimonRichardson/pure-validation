module Control.Monad.Eff.DOM where

import Data.Maybe
import Data.Foreign
import Data.Function

import Control.Monad.Eff

foreign import data DOM :: !

foreign import data Node :: *

foreign import body
  """
  function body() {
    return document.body;
  }
  """ :: forall eff. Eff (dom :: DOM | eff) Node

foreign import createElement
  """
  function createElement(name) {
    return function() {
      return document.createElement(name);
    };
  }
  """ :: forall eff. String -> Eff (dom :: DOM | eff) Node

foreign import querySelectorImpl
  """
  function querySelectorImpl(r, f, s) {
    return function() {
      var result = document.querySelector(s);
      return result ? f(result) : r;
    };
  }
  """ :: forall err r. Fn3 r (Node -> r) String (Eff (dom :: DOM | err) r)

querySelector :: forall eff. String -> Eff (dom :: DOM | eff) (Maybe Node)
querySelector s = runFn3 querySelectorImpl Nothing Just s

foreign import querySelectorAll
  """
  function querySelectorAll(s) {
    return function() {
      return document.querySelectorAll(s);
    };
  }
  """ :: forall eff. String -> Eff (dom :: DOM | eff) [Node]

foreign import appendChild
  """
  function appendChild(child) {
    return function(node) {
      return function() {
        node.appendChild(child);
        return node;
      };
    };
  }
  """ :: forall eff. Node -> Node -> Eff (dom :: DOM | eff) Node

foreign import parentNodeImpl
  """
  function parentNodeImpl(r, f, n) {
    return function() {
      var result = n.parentNode;
      return result ? f(result) : r;
    };
  }
  """ :: forall err r. Fn3 r (Node -> r) Node (Eff (dom :: DOM | err) r)

parentNode :: forall eff. Node -> Eff (dom :: DOM | eff) (Maybe Node)
parentNode n = runFn3 parentNodeImpl Nothing Just n

foreign import removeChild
  """
  function removeChild(child) {
    return function(node) {
      return function() {
        node.removeChild(child);
        return node;
      };
    };
  }
  """ :: forall eff. Node -> Node -> Eff (dom :: DOM | eff) Node

foreign import addClass
  """
  function addClass(name) {
    return function(node) {
      return function() {
        node.classList.add(name);
        return node;
      };
    };
  }
  """ :: forall eff. String -> Node -> Eff (dom :: DOM | eff) Node

foreign import setText
  """
  function setText(text) {
    return function(node) {
      return function() {
        node.textContent = text;
        return node;
      };
    };
  }
  """ :: forall eff. String -> Node -> Eff (dom :: DOM | eff) Node

foreign import getValue
  """
  function getValue(node) {
    return function() {
      return node.value;
    };
  }
  """ :: forall eff. Node -> Eff (dom :: DOM | eff) Foreign

foreign import setValue
  """
  function setValue(value) {
    return function(node) {
      return function() {
        node.value = value;
        return node;
      };
    };
  }
  """ :: forall a eff. a -> Node -> Eff (dom :: DOM | eff) Node

foreign import setInnerHTML
  """
  function setInnerHTML(html) {
    return function(node) {
      return function() {
        node.innerHTML = html;
        return node;
      };
    };
  }
  """ :: forall eff. String -> Node -> Eff (dom :: DOM | eff) Node

foreign import addEventListener
  """
  function addEventListener(name) {
    return function(handler) {
      return function(node) {
        return function() {
          node.addEventListener(name, function(e) {
            handler();
            e.preventDefault();
          });
          return {};
        };
      };
    };
  }
  """ :: forall eff. String -> Eff (dom :: DOM | eff) Unit -> Node -> Eff (dom :: DOM | eff) Unit 
