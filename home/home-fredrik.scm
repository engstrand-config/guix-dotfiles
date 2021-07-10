(use-modules
    (home home-base)
    (users user-fredrik))

(base-home-environment fredrik
    #:repos
    (list
        '("repos/pywalfox" ,"git@github.com:frewacom/pywalfox.git")
        '("repos/pywalfox-native" ,"git@github.com:frewacom/pywalfox-native.git")))
