(ns discourje.Buyer2
  (:require [discourje.core.core :refer :all]))

(defn contribute?
  "returns true when the received quote 50% or greater"
  [quote div]
  (println (format "received quote: %d and div: %d, contribute = %s" quote div (>= (* 100 (float (/ div quote))) 50)))
  (>= (* 100 (float (/ div quote))) 50))

(defn generateAddress
  "generates the address"
  []
  "Open University, Valkenburgerweg 177, 6419 AT, Heerlen")

(defn orderBook
  "Order a book from buyer2's perspective"
  [this protocol]
  (recvDelayed! "quote" "seller" this protocol
                (fn [receivedQuote]
                  (println "buyer2 received quote! " receivedQuote)
                  (recvDelayed! "quoteDiv" "buyer1" this protocol
                                (fn [receivedQuoteDiv]
                                  (if (contribute? receivedQuote receivedQuoteDiv)
                                    (do (send! "ok" "ok" this "seller" protocol)
                                        (send! "address" (generateAddress) this "seller" protocol)
                                        (recvDelayed! "date" "seller" this protocol (fn [x] (println "Received date!" x)))
                                        (recvDelayed! "repeat" "seller" this protocol
                                                      (fn [x]
                                                        (println "repeat received on buyer2 from seller!")
                                                        (orderBook this protocol)))
                                        )
                                    (send! "quit" "quit" this "seller" protocol))
                                  )))))

;wait for quote
;wait for quote div
;branch on data
;true
;send ok to seller
;send address to seller
;wait for date
;false
;quit