(ns tst.x12-forest-examples
  (:use demo.core
        tupelo.core
        tupelo.test)
  (:require
    [clojure.walk :as walk]
    [tupelo.core :as t]
    [tupelo.forest :as tf]
    [tupelo.parse.xml :as xml]
    [tupelo.splat :as splat]
    ))

(def t0-hiccup
  [:item
   [:item 1]
   [:item
    [:item :a]
    [:item :b]]
   [:item 2]
   [:item 3]
   [:item
    [:item 40]
    [:item 50]
    [:item 60]]])

; Find all items with a keyword value
(verify
  ; Use the `splat/stack-spy` function to display the frame (node+stack) when
  ; we encounter a valid target like `4`
  (let [intc-spy {:leave (fn [stack node]
                           (when (= :a (:data node))
                             (splat/stack-spy stack node)))}]
    (when false ; enable to print result
      (splat/splatter-walk-noop intc-spy t0-hiccup)
      (comment
        ; node =>
        {:branch :list/val :data :a :type :prim}
        ; stack =>
        [{:idx 1 :type :list/entry :val {:data :a :type :prim}}
         {:branch :list/val :type :coll/list}
         {:idx 1 :type :list/entry :val {:type :coll/list}}
         {:branch :list/val :type :coll/list}
         {:idx 2 :type :list/entry :val {:type :coll/list}}
         {:type :coll/list}]
        ))
    (let [stack-tmpl [{:idx 1}
                      {:branch :list/val :type :coll/list}]
          intc       {:leave (fn [stack node]
                               (let [node-data (:data node)]
                                 (when (and (= (:branch node) :list/val)
                                         (keyword? node-data)
                                         (submatch? stack-tmpl stack))
                                   (cum-vector-append! node-data))))}
          result     (with-cum-vector (splat/splatter-walk-noop intc t0-hiccup))]
      (is= result [:a :b])))

  ; Increment all integer children at 1 level of nesting
  (let [intc-spy {:leave (fn [stack node]
                           (when (= 1 (:data node))
                             (splat/stack-spy stack node)))}]
    (when false ; enable to print result
      (splat/splatter-walk-noop intc-spy t0-hiccup)
      (comment
        ; node =>
        {:branch :list/val :data 1 :type :prim}
        ; stack =>
        [{:idx 1 :type :list/entry :val {:data 1 :type :prim}}
         {:branch :list/val :type :coll/list}
         {:idx 1 :type :list/entry :val {:type :coll/list}}
         {:type :coll/list}]
        ))
    (let [stack-tmpl [{:type :list/entry :idx 1}
                      {:type :coll/list}
                      {:type :list/entry}
                      {:type :coll/list}]
          intc       {:leave (fn [stack node]
                               (let [node-data (:data node)]
                                 (cond-it-> node
                                   (and (= :list/val (:branch node))
                                     (number? node-data)
                                     (submatch? stack-tmpl stack)
                                     (= 4 (count stack)))
                                   (update-in node [:data] inc))))}
          result     (splat/splatter-walk intc t0-hiccup)]
      (is= result
        [:item
         [:item 2]
         [:item [:item :a] [:item :b]]
         [:item 3]
         [:item 4]
         [:item [:item 40] [:item 50] [:item 60]]]))))

;-----------------------------------------------------------------------------
; Discard any xml nodes of Type="B" or Type="C" (plus blank string nodes)
(verify
  (let [xml-str     " <ROOT>
                        <Items>
                          <Item><Type>A</Type><Note>AA1</Note></Item>
                          <Item><Type>B</Type><Note>BB1</Note></Item>
                          <Item><Type>C</Type><Note>CC1</Note></Item>
                          <Item><Type>A</Type><Note>AA2</Note></Item>
                        </Items>
                      </ROOT>"
        data-enlive (xml/parse xml-str)
        data-hiccup (-> data-enlive tf/enlive->hiccup)
        ]
    (is= data-hiccup
      [:ROOT
       [:Items
        [:Item [:Type "A"] [:Note "AA1"]]
        [:Item [:Type "B"] [:Note "BB1"]]
        [:Item [:Type "C"] [:Note "CC1"]]
        [:Item [:Type "A"] [:Note "AA2"]]]])

    (let [result (->> data-hiccup
                   (walk/postwalk (fn [arg]
                                    (cond-it-> arg
                                      (submatch? [:Item [:Type "B"]] it) nil
                                      (submatch? [:Item [:Type "C"]] it) nil
                                      (submatch? [:Items] arg) (keep-if not-nil? it)))))]
      (is= result
        [:ROOT
         [:Items
          [:Item [:Type "A"] [:Note "AA1"]]
          [:Item [:Type "A"] [:Note "AA2"]]]]))))

;-----------------------------------------------------------------------------
; xml searching example
(verify
  (let [xml-str     "<data>
                    <products>
                      <product>
                        <section>Red Section</section>
                        <images>
                          <image>img.jpg</image>
                          <image>img2.jpg</image>
                        </images>
                      </product>
                      <product>
                        <section>Blue Section</section>
                        <images>
                          <image>img.jpg</image>
                          <image>img3.jpg</image>
                        </images>
                      </product>
                      <product>
                        <section>Green Section</section>
                        <images>
                          <image>img.jpg</image>
                          <image>img2.jpg</image>
                        </images>
                      </product>
                    </products>
                  </data> "
        data-enlive (xml/parse xml-str)
        data-hiccup (-> data-enlive tf/enlive->hiccup)
        data-red    (first
                      (with-cum-vector
                        (->> data-hiccup
                          (walk/postwalk (fn [arg]
                                           (cond-it-> arg
                                             (submatch? [:product [:section "Red Section"]] it)
                                             (cum-vector-append! it)))))))
        ]
    (is= data-red
      [:product
       [:section "Red Section"]
       [:images [:image "img.jpg"] [:image "img2.jpg"]]])))

;-----------------------------------------------------------------------------

#_(verify
  (let [
        data1   {:type :fn
                :name :times
                :op   :*
                :args [{:type :prim :val 2}
                       {:type :prim :val 3}]}
        data2   {:type :fn
                :name :plus
                :op   :+
                :args [{:type :prim :val 2}
                       {:type :prim :val 3}]}
        op->impl {:* *
                  :+ +}
        doeval (fn doeval-fn [node]
                 (with-map-vals node [type]
                   (cond
                     (= :prim type) (grab :val node)
                     (= :fn type) (let [args    (mapv doeval-fn (grab :args node))
                                        impl-fn (op->impl (:op node))
                                        result  (apply impl-fn args)]
                                    result)
                     :else (throw (ex-info "bad type" node)))))
        ]
    (is= 6 (doeval data1))
    (is= 5 (doeval data2))

    ))



