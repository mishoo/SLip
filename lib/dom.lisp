(defpackage :dom
  (:use :sl :ffi)
  (:shadow #:remove)
  (:export #:document
           #:element #:elementp #:node #:nodep
           #:from-html #:append-to #:remove #:insert-at
           #:create-element
           #:add-class #:remove-class #:toggle-class #:has-class
           #:dataset
           #:query #:query-all #:do-query #:matches
           #:on-event #:off-event
           #:event-prevent-default
           #:make-dialog))

(in-package :dom)

(defconstant document ((lambda-js () "return document")))

(defun-js elementp (thing) "
  return thing instanceof Element;
")

(defun-js nodep (thing) "
  return thing instanceof Node;
")

(deftype element ()
  '(satisfies elementp))

(deftype node ()
  '(satisfies nodep))

(defun-js from-html (html) "
  let div = document.createElement('div');
  div.innerHTML = html.trim();
  if (div.childNodes.length > 1) {
    let range = document.createRange();
    range.selectNodeContents(div);
    return range.extractContents();
  }
  return div.firstChild;
")

(defun-js append-to (parent child) "
  return parent.appendChild(child);
")

(defun-js remove (element) "
  return element.remove();
")

(defun-js insert-at (anchor sibling) "
  return anchor.parentNode.insertBefore(sibling, anchor);
")

(defun-js query (selector &optional container) "
  return (container || document).querySelector(selector);
")

(defun-js query-all (selector &optional container) "
  return [ ...(container || document).querySelectorAll(selector) ];
")

(defmacro do-query ((element selector &optional container) &body body)
  `(loop for ,element across (query-all ,selector ,container)
         do (progn ,@body)))

(defun-js matches (element selector) "
  return element.matches(selector);
")

(defun-js create-element (type) "
  return document.createElement(type);
")

(defun-js add-class (element cls) "
  element.classList.add(cls);
")

(defun-js remove-class (element cls) "
  element.classList.remove(cls);
")

(defun-js has-class (element cls) "
  return element.classList.contains(cls);
")

(defun-js toggle-class (element cls &optional (condition nil condition-passed-p)) "
  if (condition_passed_p) {
    return element.classList.toggle(cls, condition);
  } else {
    return element.classList.toggle(cls);
  }
")

(let ((dataset (lambda-js (element key value) "
                  if (arguments.length === 2) return element.dataset[key];
                  return element.dataset[key] = value;")))
  (defun dataset (element key)
    (when (symbolp key)
      (setf key (%:%js-camelcase-name key)))
    (funcall dataset element key))

  (defun (setf dataset) (value element key)
    (when (symbolp key)
      (setf key (%:%js-camelcase-name key)))
    (funcall dataset element key value)))

(defun-js on-event (element event-name
                            &key
                            selector
                            process
                            (signal event-name)) "
  let handler = ev => {
    let target = ev.target;
    let send_signal = () =>
      LispProcess.sendmsg(process || this.process, signal, LispCons.fromArray([ target, ev ]));
    if (!selector)
      return send_signal();
    while (target) {
      if (target instanceof Element && target.matches(selector))
        return send_signal();
      target = target.parentElement;
    }
  };
  element.addEventListener(event_name, handler);
  return handler;
")

(defun-js off-event (element event-name handler) "
  element.removeEventListener(event_name, handler);
")

(defun-js event-prevent-default (event) "
  return event.preventDefault();
")

;;; this is for ide/Ymacs
(defun-js make-dialog (width height &key content class-name) "
  let dlg = YMACS.makeDialog({
    width, height, content,
    closable: true,
    draggable: content?.querySelector('._drag-dialog'),
    resizable: true,
  });
  if (class_name) dlg.addClass(class_name);
  return dlg.getElement();
")