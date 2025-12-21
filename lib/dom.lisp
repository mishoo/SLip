(defpackage :dom
  (:use :sl :ffi)
  (:export #:document #:document-element
           #:create-document-fragment
           #:element #:elementp #:node #:nodep
           #:from-html #:append-to #:remove-element #:insert-before #:insert-after
           #:replace-children #:replace-with
           #:has-animations
           #:first-child #:first-element-child #:last-child #:last-element-child
           #:next-sibling #:next-element-sibling
           #:previous-sibling #:previous-element-sibling
           #:parent-element #:closest
           #:inner-html #:inner-text #:checked #:value
           #:create-element
           #:add-class #:remove-class #:toggle-class #:has-class #:class-name
           #:dataset
           #:query #:query-all #:do-query #:matches
           #:on-event #:off-event #:with-events #:done-events
           #:prevent-default #:stop-propagation #:stop-immediate-propagation
           #:shift-key #:ctrl-key #:alt-key #:meta-key
           #:scroll-into-view
           #:bounding-client-rect
           #:trigger-reflow
           #:clipboard-write-text
           #:offset-width #:offset-height #:offset-left #:offset-top #:offset-parent
           #:client-width #:client-height #:client-left #:client-top
           #:scroll-left #:scroll-top #:scroll-width #:scroll-height
           #:focus
           #:key #:client-x #:client-y
           #:trigger
           #:make-dialog
           #:close-dialog
           #:load-css
           #:style))

(in-package :dom)

(defconstant document ((lambda-js () "return document")))
(defconstant document-element ((lambda-js () "return document.documentElement")))

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

(defun-js append-to (parent child)
  "return parent.appendChild(child)")

(defun-js remove-element (element)
  "return element.remove()")

(defun-js insert-before (anchor sibling)
  "return anchor.before(sibling)")

(defun-js insert-after (anchor sibling)
  "return anchor.after(sibling)")

(defun-js replace-with (element replacement)
  "return element.replaceWith(replacement)")

(defun-js replace-children (element &rest replacement)
  "return element.replaceChildren(...LispCons.toArray(replacement))")

(define-simple-getter first-child "firstChild")
(define-simple-getter first-element-child "firstElementChild")
(define-simple-getter last-child "lastChild")
(define-simple-getter last-element-child "lastElementChild")
(define-simple-getter next-sibling "nextSibling")
(define-simple-getter next-element-sibling "nextElementSibling")
(define-simple-getter previous-sibling "previousSibling")
(define-simple-getter previous-element-sibling "previousElementSibling")
(define-simple-getter parent-element "parentElement")

(defun-js closest (element selector)
  "return element.closest(selector)")

(defun-js query (container selector)
  "return (container || document).querySelector(selector)")

(defun-js query-all (container selector)
  "return LispCons.fromArray((container || document).querySelectorAll(selector))")

(defmacro do-query ((element container selector) &body body)
  `(loop for ,element in (query-all ,container ,selector)
         do (progn ,@body)))

(defun-js matches (element selector)
  "return element.matches(selector)")

(defun-js create-element (nodename)
  "return document.createElement(nodename)")

(defun-js add-class (element cls)
  "element.classList.add(cls)")

(defun-js remove-class (element cls)
  "element.classList.remove(cls)")

(defun-js has-class (element cls)
  "return element.classList.contains(cls)")

(defun-js toggle-class (element cls &optional (condition nil condition-passed-p)) "
  if (condition_passed_p) {
    return element.classList.toggle(cls, condition);
  } else {
    return element.classList.toggle(cls);
  }
")

(define-simple-accessor class-name "className")

(defun-js has-animations (element)
  "return element.getAnimations({ subtree: true }).length > 0")

(let ((dataset (lambda-js (element key value) "
                  if (arguments.length === 2) return element.dataset[key];
                  if (value === false) return delete element.dataset[key], false;
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
(define-simple-accessor inner-text "innerText")
(define-simple-accessor outer-html "outerHTML")
(define-simple-getter trigger-reflow "offsetWidth")
(define-simple-getter offset-width "offsetWidth")
(define-simple-getter offset-height "offsetHeight")
(define-simple-getter offset-left "offsetLeft")
(define-simple-getter offset-top "offsetTop")
(define-simple-getter offset-parent "offsetParent")
(define-simple-getter client-width "clientWidth")
(define-simple-getter client-height "clientHeight")
(define-simple-getter client-left "clientLeft")
(define-simple-getter client-top "clientTop")
(define-simple-accessor scroll-left "scrollLeft")
(define-simple-accessor scroll-top "scrollTop")
(define-simple-getter scroll-width "scrollWidth")
(define-simple-getter scroll-height "scrollHeight")

(defun-js bounding-client-rect (el &optional relative-to) "
  let box = el.getBoundingClientRect();
  box = {
    left: box.left,
    top: box.top,
    right: box.right,
    bottom: box.bottom,
    width: box.width,
    height: box.height,
  };
  if (relative_to) {
    let rel = relative_to.getBoundingClientRect();
    box.left -= rel.left;
    box.right -= rel.left;
    box.top -= rel.top;
    box.bottom -= rel.top;
  }
  return new Values([ box.left, box.top, box.width, box.height, box.right, box.bottom ]);
")

(let ((style (lambda-js (element prop value) "
  if (arguments.length === 2)
    return window.getComputedStyle(element).getPropertyValue(prop);
  if (value === false) {
    element.style.removeProperty(prop);
  } else {
    element.style.setProperty(prop, value);
  }
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

(defun-js scroll-into-view (element &key (block "nearest") (inline "nearest"))
  "element.scrollIntoView({ block, inline })")

(defun-js focus (element)
  "return element.focus()")

(defun-js on-event (element event-name
                            &key
                            capture
                            once
                            passive
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
    while (target instanceof Element) {
      if (target.matches(selector))
        return send_signal();
      target = target.parentElement;
    }
  };
  element.addEventListener(event_name, handler, { capture, once, passive });
  return handler;
")

(defun-js off-event (element event-name handler &key capture)
  "element.removeEventListener(event_name, handler, { capture })")

(defmacro with-events ((element &rest events) &body body)
  (let ((el (gensym "element"))
        (handlers (mapcar (lambda (ev) (gensym (car ev))) events)))
    `(let* ((,el ,element)
            ,@(mapcar (lambda (handler ev)
                        `(,handler (on-event ,el ,@ev)))
                      handlers events))
       (flet ((done-events ()
                ,@(mapcar (lambda (handler ev)
                            `(off-event ,el ,(car ev) ,handler
                                        ,@(aif (getf (cdr ev) :capture)
                                               `(:capture ,it))))
                          handlers events)))
         ,@body
         (done-events)))))

(defun-js prevent-default (event) "return event.preventDefault()")
(defun-js stop-propagation (event) "return event.stopPropagation()")
(defun-js stop-immediate-propagation (event) "return event.stopImmediatePropagation()")
(define-simple-getter shift-key "shiftKey")
(define-simple-getter alt-key "altKey")
(define-simple-getter ctrl-key "ctrlKey")
(define-simple-getter meta-key "metaKey")

(defun-js clipboard-write-text (text) "
  try {
    navigator.clipboard.writeText(text);
    return true;
  }
  catch(ex) {
    return false;
  }
")

(define-simple-getter client-x "clientX")
(define-simple-getter client-y "clientY")

(defun-js key (event) "return event.key === ' ' ? 'Space' : event.key")

(defun-js trigger (element event-name &key data bubbles cancelable composed) "
  let event = new Event(event_name, { bubbles, cancelable, composed });
  if (data !== false) event.data = data;
  element.dispatchEvent(event);
")

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

(defun-js close-dialog (dlg)
  "dlg._ymacs_object.close()")
