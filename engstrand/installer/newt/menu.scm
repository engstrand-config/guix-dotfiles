(define-module (engstrand installer newt menu)
  #:use-module (gnu installer steps)
  #:use-module (gnu installer utils)
  #:use-module (gnu installer newt page)
  #:use-module (gnu installer newt utils)
  #:use-module (guix i18n)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 receive)
  #:use-module (newt)
  #:export (run-menu-page))

(define info-textbox-width (make-parameter 40))
(define options-listbox-height (make-parameter 15))

(define* (run-menu-page title info-text
                        #:key
                        listbox-items
                        listbox-item->text)
  "Run a page with the given TITLE, to ask the user to choose between
LISTBOX-ITEMS displayed in a listbox. The listbox items are converted to text
using LISTBOX-ITEM->TEXT procedure. Contrary to other pages, we cannot resort
to grid layouts, because we want this page to occupy all the screen space
available."
  (define (fill-listbox listbox items)
    (map (lambda (item)
           (let* ((text (listbox-item->text item))
                  (key (append-entry-to-listbox listbox text)))
             (cons key item)))
         items))

  (let* ((info-textbox
          (make-reflowed-textbox -1 -1
                                 info-text
                                 (info-textbox-width)))
         (options-listbox
          (make-listbox -1 -1
                        (options-listbox-height)
                        (logior FLAG-BORDER FLAG-RETURNEXIT)))
         (keys (fill-listbox options-listbox listbox-items))
         (grid (vertically-stacked-grid
                GRID-ELEMENT-COMPONENT info-textbox
                GRID-ELEMENT-COMPONENT options-listbox))
         (form (make-form)))

    (define (choice->item str)
      ;; Return the item that corresponds to STR.
      (match (find (match-lambda
                     ((key . item)
                      (string=? str (listbox-item->text item))))
                   keys)
        ((key . item) item)
        (#f (abort-to-prompt 'installer-step 'abort))))

    (add-form-to-grid grid form #t)
    (make-wrapped-grid-window grid title)

    (receive (exit-reason argument)
        (run-form-with-clients form
                               `(menu (title ,title)
                                      (text ,info-text)
                                      (items
                                       ,(map listbox-item->text
                                             listbox-items))))
      (dynamic-wind
        (const #t)
        (lambda ()
          (match exit-reason
            ('exit-component
             (let* ((entry (current-listbox-entry options-listbox))
                    (item (assoc-ref keys entry)))
               (match item
                 ((text . proc)
                  (proc)))))
            ('exit-fd-ready
             (let* ((choice argument)
                    (item   (choice->item choice)))
               (match item
                 ((text . proc)
                  (proc)))))))
        (lambda ()
          (destroy-form-and-pop form))))))
