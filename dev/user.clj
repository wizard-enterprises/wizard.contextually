(ns user
  (:use wizard.toolbelt)
  (:require [wizard.contextually :as ctx]
            [kaocha [repl :as k] [watch :as w]]))

(comment
  (foo/add 2 3) ; => 5

  ;; run tests once
  (k/run :unit)
  ;; run tests in watch mode
  (w/run (k/config))

  (server/make-org-server org)
  (dev-server/setup! "3014")
  (dev-server/restart!))
