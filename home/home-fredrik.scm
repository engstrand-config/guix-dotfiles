(define-module (home home-fredrik)
               #:use-module (home home-base)
               #:use-module (users user-fredrik))

(base-home-environment
  #:user fredrik
  #:repos
  (list
    '("repos/pywalfox" ,"git@github.com:frewacom/pywalfox.git")
    '("repos/pywalfox-native" ,"git@github.com:frewacom/pywalfox-native.git")))
