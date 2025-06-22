(in-package :sl-user)

(destructuring-bind (&key ((:a (b c)) '(1 2) a-p)) '(:a (3 4))
  (values a-p c b))
