(load "base.scm")
(use-modules (home-base))

(base-home-environment
    #:repos
    (list
        '("repos/pywalfox" ,"git@github.com:frewacom/pywalfox.git")
        '("repos/pywalfox-native" ,"git@github.com:frewacom/pywalfox-native.git")))
