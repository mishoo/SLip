(defpackage :dom
  (:use :sl :ffi)
  (:shadow #:remove)
  (:export #:document
           #:element #:elementp #:node #:nodep
           #:from-html #:append-to #:remove #:insert-at
           #:has-animations
           #:next-sibling #:next-element-sibling
           #:previous-sibling #:previous-element-sibling
           #:parent-element
           #:inner-html #:checked #:value
           #:create-element
           #:add-class #:remove-class #:toggle-class #:has-class
           #:dataset
           #:query #:query-all #:do-query #:matches
           #:on-event #:off-event
           #:prevent-default
           #:scroll-into-view
           #:trigger-reflow
           #:offset-width
           #:offset-height
           #:focus
           #:key
           #:make-dialog
           #:close-dialog
           #:load-css
           #:style))

(in-package :dom)

(defconstant document ((lambda-js () "return document")))

(defmacro define-simple-getter (name prop)
  `(defun-js ,name (element)
     ,(format nil "return element.~A" prop)))

(defmacro define-simple-accessor (name prop)
  `(let ((accessor (lambda-js (element value) ,(format nil "
  if (arguments.length === 1) return element.~A;
  return element.~A = value;
" prop prop))))
     (defun ,name (element)
       (funcall accessor element))
     (defun (setf ,name) (value element)
       (funcall accessor element value))))

(defun-js elementp (thing) "return thing instanceof Element")

(defun-js nodep (thing) "return thing instanceof Node")

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

(define-simple-getter next-sibling "nextSibling")
(define-simple-getter next-element-sibling "nextElementSibling")
(define-simple-getter previous-sibling "previousSibling")
(define-simple-getter previous-element-sibling "previousElementSibling")
(define-simple-getter parent-element "parentElement")

(defun-js query (container selector) "
  return (container || document).querySelector(selector);
")

(defun-js query-all (container selector) "
  return LispCons.fromArray((container || document).querySelectorAll(selector));
")

(defmacro do-query ((element container selector) &body body)
  `(loop for ,element in (query-all ,container ,selector)
         do (progn ,@body)))

(defun-js matches (element selector) "
  return element.matches(selector);
")

(defun-js create-element (nodename) "
  return document.createElement(nodename);
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

(defun-js has-animations (element) "return element.getAnimations({ subtree: true }).length > 0")

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

(define-simple-accessor checked "checked")
(define-simple-accessor value "value")
(define-simple-accessor inner-html "innerHTML")
(define-simple-accessor trigger-reflow "offsetWidth")
(define-simple-accessor offset-width "offsetWidth")
(define-simple-accessor offset-height "offsetHeight")

(let ((style (lambda-js (element prop value) "
  if (arguments.length === 2)
    return window.getComputedStyle(element).getPropertyValue(prop);
  element.style.setProperty(prop, value);
  return value;
")))
  (defun style (element prop)
    (when (symbolp prop)
      (setf prop (string-downcase (symbol-name prop))))
    (funcall style element prop))
  (defun (setf style) (value element prop)
    (when (symbolp prop)
      (setf prop (string-downcase (symbol-name prop))))
    (funcall style element prop value)))

(defun-js scroll-into-view (element &key (block "nearest") (inline "nearest")) "
  element.scrollIntoView({ block, inline });
")

(defun-js focus (element) "element.focus()")

(defun-js on-event (element event-name
                            &key
                            selector
                            process
                            (signal event-name)) "
  let handler = ev => {
    let target = ev.target;
    let send_signal = () =>
      LispProcess.sendmsg(process || this.process, signal,
                          LispCons.fromArray([ target, ev ]));
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

(defun-js prevent-default (event) "return event.preventDefault()")

(define-simple-getter key "key")

(defun-js load-css (url) "
  let link = document.querySelector('link[data-url=\"' + url + '\"]');
  if (!link) {
    link = document.createElement('link');
    link.dataset.url = url;
    link.rel = 'stylesheet';
    link.type = 'text/css';
    document.querySelector('head').appendChild(link);
  }
  link.href = url + '?kc=' + Date.now();
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
  let el = dlg.getElement();
  el.tabIndex = 1;
  return el;
")

(defun-js close-dialog (dlg) "dlg._ymacs_object.close()")
