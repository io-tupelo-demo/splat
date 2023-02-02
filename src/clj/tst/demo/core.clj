(ns tst.demo.core
  (:use demo.core tupelo.core tupelo.test))

(verify
  (is (safe-even? 2))
  (isnt (safe-even? 3))
  (isnt (safe-even? nil))
  (isnt (safe-even? :c))
  )

(verify
  (isnt (safe-odd? 2))
  (is (safe-odd? 3))
  (isnt (safe-odd? nil))
  (isnt (safe-odd? :c))
  )

(verify
  (is= :a (safe-inc :a))
  (is= 4 (safe-inc 3))
  (is= nil (safe-inc nil))
  )


