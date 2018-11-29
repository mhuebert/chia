(ns chia.graphql.root)

(defmacro invoke*
  ([root variables]
   `(~'chia.graphql.root/invoke* ~root ~variables (:root/xkeys ~root)))
  ([root variables xkeys]
   `(~'chia.graphql.root/invoke
     (-> ~root
         (assoc :root/variables ~variables
                :root/xkeys ~xkeys
                :root/view ~'chia.view/*current-view*)))))