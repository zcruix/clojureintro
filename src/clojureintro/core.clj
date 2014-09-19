(ns clojureintro.core
  (:use [bakery.core]))

(defn error [& args]
  (apply println args)
  :error
  )

(defn add-egg []
  (grab :egg)
  (squeeze)
  (add-to-bowl))

(defn add-sugar []
  (grab :cup)
  (scoop :sugar)
  (add-to-bowl)
  (release))

(defn add-flour []
  (grab :cup)
  (scoop :flour)
  (add-to-bowl)
  (release))

(defn add-milk []
  (grab :cup)
  (scoop :milk)
  (add-to-bowl)
  (release))

(defn add-butter []
  (grab :butter)
  (add-to-bowl))

(defn add-eggs [n]
  (dotimes [e n]
    (add-egg)))

(defn add-flour-cups [n]
  (dotimes [e n]
    (add-flour)))

(defn add-milk-cups [n]
  (dotimes [e n]
    (add-milk)))

(defn add-sugar-cups [n]
  (dotimes [e n]
    (add-sugar)))

(defn add-butters [n]
  (dotimes [e n]
    (add-butter)))

(def scooped-ingredients #{:milk :sugar :flour})
(defn scooped? [ingredient]
  (contains? scooped-ingredients ingredient))

(def squeezed-ingredients #{:egg})
(defn squeezed? [ingredient]
  (contains? squeezed-ingredients ingredient))

(def simple-ingredients #{:butter})
(defn simple? [ingredient]
  (contains? simple-ingredients ingredient))

(def pantry-ingredients #{:flour :sugar})
(defn from-pantry? [ingredient]
  (contains? pantry-ingredients ingredient))

(def fridge-ingredients #{:milk :egg :butter})
(defn from-fridge? [ingredient]
  (contains? fridge-ingredients ingredient))

(defn add-squeezed
  ([ingredient]
     (add-squeezed ingredient 1))
  ([ingredient amount]
     (if (squeezed? ingredient)
       (dotimes [i amount]
         (grab ingredient)
         (squeeze)
         (add-to-bowl) :ok)
       (error "This function only works on squeezed ingredients. You asked me to squeeze" ingredient))))

(defn add-scooped
  ([ingredient]
     (add-scooped ingredient 1))
  ([ingredient amount]
     (if (scooped? ingredient)
       (do
         (grab :cup)
         (dotimes [i amount]
          (scoop ingredient)
          (add-to-bowl))
          (release) :ok)
       (error "This function only works on scooped ingredients. You asked me to scoop" ingredient))))

(defn add-simple
  ([ingredient]
     (add-simple ingredient 1))
  ([ingredient amount]
     (if (simple? ingredient)
       (dotimes [i amount]
         (grab ingredient)
         (add-to-bowl) :ok)
       (error "This function only works on simple ingredients. You asked me to add" ingredient))))

(defn add
  ([ingredient]
     (add ingredient 1))
  ([ingredient amount]
     (cond
      (squeezed? ingredient)
      (add-squeezed ingredient amount)

      (simple? ingredient)
      (add-simple ingredient amount)

      (scooped? ingredient)
      (add-scooped ingredient amount)

      :else

      (error "I do not have the ingredient" ingredient))))

(defn bake-cookies []
  (add :egg 1)
  (add :flour 1)
  (add :butter 1)
  (add :sugar 1)

  (mix)

  (pour-into-pan)
  (bake-pan 30)
  (cool-pan))

(defn bake-cake []
  (add :egg 2)
  (add :flour 2)
  (add :milk 1)
  (add :sugar 1)

  (mix)

  (pour-into-pan)
  (bake-pan 25)
  (cool-pan))

(defn fetch-from-pantry ( [ingredient]
                          fetch-from-pantry ingredient 1 )
  ([ingredient amount]
   (if (from-pantry? ingredient)
     (do
       (go-to :pantry)
       (dotimes [i amount]
         (load-up ingredient))
       (go-to :prep-area)
       (dotimes [i amount]
         (unload ingredient)))
     (error "This function only works on ingredients in the pantry. You asked me to fetch" ingredient))))

(defn fetch-from-fridge ( [ingredient]
                          fetch-from-fridge ingredient 1 )
  ([ingredient amount]
   (if (from-fridge? ingredient)
     (do
       (go-to :fridge)
       (dotimes [i amount]
         (load-up ingredient))
       (go-to :prep-area)
       (dotimes [i amount]
         (unload ingredient)))
     (error "This function only works on ingredients in the fridge. You asked me to fetch" ingredient))))

(defn fetch-ingredient ([ingredient]
                          fetch-ingredient ingredient 1)
  ([ingredient amount]
      (cond
       (from-fridge? ingredient)
       (fetch-from-fridge ingredient amount)
       (from-pantry? ingredient)
       (fetch-from-pantry ingredient amount)
       :else
       (error "I don't know where to get" ingredient))))

(defn load-up-amount [ingredient amount]
  (dotimes [i amount]
    (load-up ingredient)))

(defn unload-amount [ingredient amount]
  (dotimes [i amount]
    (unload ingredient)))

(defn fetch-list [shopping-list]
  (doseq [[location ingredients]
           {:pantry pantry-ingredients
            :fridge fridge-ingredients}]
    (go-to location)
    (doseq  [ingredient ingredients]
      (load-up-amount ingredient (ingredient shopping-list 0)))

    (go-to :prep-area)
    (doseq [ingredient ingredients]
      (unload-amount ingredient (ingredient shopping-list 0)))))

 (def cake-list {:egg 2
                 :flour 2
                 :milk 1
                 :sugar 1})

 (def cookies-list {:egg 1
                    :flour 1
                    :butter 1
                    :sugar 1 })

(defn add-ingredients [a b]
  (merge-with + a b))

(defn multiply-ingredients [n ingredients]
  (for [[ingredient amount] ingredients] [ingredient (* n amount)]))

(defn order->ingredient [order]
  (add-ingredients (multiply-ingredients (:cake (:items order 0)) cake-list)
                   (multiply-ingredients (:cookies (:items order 0) cookies-list))))

(defn orders->ingredients [orders]
  (reduce add-ingredients (map orders->ingredients orders)))

(defn bake [item]
  (cond
   (= :cake item)
   (bake-cake)
   (= :cookies item)
   (bake-cookies)
   :else
   (error "I don't know how to bake" item))
  )

(defn day-at-the-bakery []
  (let [orders (get-morning-orders)
        ingredient-list (orders->ingredients orders)]
    (fetch-list ingredient-list)
    (doseq [order orders]
      (let [items (:items order)
            racks (for [[item amount] items i (range amount)]
                    (bake item))
            receipt {:orderid (:orderid order)
                     :address (:address order)
                     :rackids racks}]
        (delivery receipt)))))

(defn -main []
  (start-over)
  (day-at-the-bakery)
 )
