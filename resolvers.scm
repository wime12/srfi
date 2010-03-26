;;;  --------------------------------------------------------------  ;;;
;;;                                                                  ;;;
;;;                 Module, import and export resolvers              ;;;
;;;                                                                  ;;;
;;;  --------------------------------------------------------------  ;;;


;;;; ---------- Module resolvers ----------

;; A module resolver is a function that takes a list (like (/srfi/1)
;; or (spork /core)), the current loader and the current module path
;; (or #f) and returns a module identifier.

;; This is the 'here module resolver function
(define (current-module-resolver loader path . ids)
  (map (lambda (id)
         (make-module
          loader
          ((loader-path-absolutize loader) id path)))
       ids))

(define (package-module-resolver path)
  (let ((path
         (string-append
          (path-strip-trailing-directory-separator path) "/")))
    (lambda (_ __ . ids)
      (map (lambda (id)
             (make-module
              local-loader
              ((loader-path-absolutize local-loader) id path)))
           ids))))

;; This is a helper function for singleton loaders, for instance 'module
(define (make-singleton-module-resolver pkg)
  (lambda (_ __)
    (list (make-module pkg #f))))

(define *module-resolvers* '())

(define (module-resolver-add! name res)
  (set! *module-resolvers*
        (cons (cons name res)
              *module-resolvers*)))

(define (resolve-module name #!optional cm)
  (if (module? name)
      (list name)
      (call-with-values
          (lambda ()
            (cond
             ((symbol? name)
              (values 'here (list name)))
             ((string? name)
              (values 'lib (list name)))
             ((pair? name)
              (values (car name) (cdr name)))
             (else
              (error "Invalid module identifier:" name))))
        (lambda (resolver-id resolver-args)
          (let ((resolver (let ((pair (assq resolver-id
                                            *module-resolvers*)))
                            (and pair (cdr pair)))))
            (if (not resolver)
                (error "Module resolver not found:" resolver-id)
                (apply resolver
                       `(,(if cm
                              (module-loader cm)
                              (current-loader))
                         ,(if cm
                              (module-path cm)
                              (let ((current-mod (current-module)))
                                (and current-mod (module-path current-mod))))
                         ,@resolver-args))))))))

(define (resolve-modules names #!optional cm)
  (apply append
         (map (lambda (x)
                (resolve-module x cm))
              names)))

(define (resolve-one-module name #!optional cm)
  (let ((res (resolve-module name cm)))
    (if (or (null? res)
            (not (pair? res))
            (not (null? (cdr res))))
        (error "Module identifier must refer to one module only:" name)
        (car res))))



;;;; ---------- Import resolvers ----------

;; Import resolvers are the implementation of (only: [mod] names),
;; add-prefix:, and similar features. They are functions that take the
;; current module, and their arguments ((only: [mod] names ...) would
;; give the arguments '([mod] names ...) to the only: resolver) and
;; return two values: the imported symbols (in the same format as
;; module-info-exports) and a list of the modules that this import
;; depends on.

(define *import-resolvers* '())

(define (resolve-import val #!optional cm)
  (with-module-cache
   (lambda ()
     (cond
      ((and (pair? val)
            (keyword? (car val)))
       (let* ((resolver-id (car val))
              (resolver (let ((pair (assq resolver-id
                                          *import-resolvers*)))
                          (and pair (cdr pair)))))
         (if (not resolver)
             (error "Import resolver not found:" resolver-id)
             (apply resolver
                    (cons (or cm (current-module))
                          (cdr val))))))
      
      (else
       (let ((mods (resolve-module val cm)))
         (values (apply
                  append
                  (map (lambda (mod)
                         (module-info-exports
                          (module-info mod)))
                       mods))
                 mods)))))))

(define (resolve-imports vals #!optional cm)
  (with-module-cache
   (lambda ()
     (let ((defs '())
           (mods '()))
       (for-each (lambda (val)
                   (call-with-values
                       (lambda ()
                         (resolve-import val cm))
                     (lambda (def mod)
                       (set! defs (cons def defs))
                       (set! mods (cons mod mods)))))
                 vals)
       (values (flatten1 defs)
               (flatten1 mods))))))

(define (only-resolver cm mod . names)
  (call-with-values
      (lambda () (resolve-import mod cm))
    (lambda (defs modules)
      (values
       (map (lambda (name)
              (or (assq name defs)
                  (error "only: Symbol not defined" name mod)))
            names)
       modules))))

(define (except-resolver cm mod . names)
  (call-with-values
      (lambda () (resolve-import mod cm))
    (lambda (defs modules)
      (let ((def-clone (map (lambda (x) x) defs)))
        (for-each
         (lambda (name)
           (let ((found? #f))
             (set! def-clone
                   (remove!
                    (lambda (x)
                      (and (eq? (car x) name)
                           (begin
                             (set! found? #t)
                             #t)))
                    def-clone))
             (if (not found?)
                 (error "except: Symbol not defined" name mod))))
         names)
        (values def-clone modules)))))

(define (prefix-resolver cm mod prefix)
  (let ((prefix-str
         (if (symbol? prefix)
             (symbol->string prefix)
             (error "prefix: prefix must be a symbol" prefix))))
    (call-with-values
        (lambda () (resolve-import mod cm))
      (lambda (defs modules)
        (values
         (map (lambda (def)
                (cons (string->symbol
                       (string-append
                        prefix-str
                        (symbol->string (car def))))
                      (cdr def)))
              defs)
         modules)))))

(define (rename-resolver cm mod . renames)
  (call-with-values
      (lambda () (resolve-import mod cm))
    (lambda (defs modules)
      (let ((def-clone (map (lambda (x) x) defs)))
        (for-each
         (lambda (rename)
           (if (not (and (list? rename)
                         (eq? 2 (length rename))
                         (symbol? (car rename))
                         (symbol? (cadr rename))))
               (error "rename: Invalid rename form" rename))

           (let ((pair (assq (car rename)
                             def-clone)))
             (if pair
                 (if (assq (cadr rename) def-clone)
                     (error "rename: Symbol already in set"
                            (cadr rename))
                     (set-car! pair (cadr rename)))
                 (error "rename: Symbol not found" (car rename)))))
         renames)
        (values def-clone modules)))))

(set! *import-resolvers*
      `((only: . ,only-resolver)
        (except: . ,except-resolver)
        (prefix: . ,prefix-resolver)
        (rename: . ,rename-resolver)))



;;;; ---------- Export resolvers ----------

;; Export resolvers are the export equivalent of import
;; resolvers. They parse forms like (rename: (from to)), (re-export:
;; srfi/1). Similar to import resolvers, they take the current
;; environment and a list as arguments. Similar to import resolvers,
;; they return two values: the symbols to be exported, and modules
;; that need to loaded to use this export. (This is to make it
;; possible to implement re-export:)

(define *export-resolvers* '())

(define (export-helper env name as)
  (if (not (and (symbol? name)
                (symbol? as)))
      (error "Invalid exports declaration" name as))
  (cond
   ((environment-get env name) =>
    (lambda (val)
      (if (eq? 'mac (car val))
          (list as
                'mac
                (cadr val) ;; Macro procedure
                (caddr val)) ;; Macro environment
          (list as
                'def
                (cadr val)))))
   
   (else
    (error "Name can't be exported because it isn't defined"
           name))))

;; TODO This code is very similar to resolve-import, which is bad.
(define (resolve-export val env)
  (with-module-cache
   (lambda ()
     (cond
      ((and (pair? val)
            (keyword? (car val)))
       (let* ((resolver-id (car val))
              (resolver (let ((pair (assq resolver-id
                                          *export-resolvers*)))
                          (and pair (cdr pair)))))
         (if (not resolver)
             (error "Export resolver not found:" resolver-id)
             (apply resolver
                    (cons env (cdr val))))))
      
      ((symbol? val)
       (values (list (export-helper env val val))
               '()))

      (else
       (error "Invalid exports declaration" val))))))

;; TODO This code is pretty much a copy/paste of resolve-imports, which
;; is bad.
(define (resolve-exports vals env)
  (with-module-cache
   (lambda ()
     (let ((defs '())
           (mods '()))
       (for-each (lambda (val)
                   (call-with-values
                       (lambda ()
                         (resolve-export val env))
                     (lambda (def mod)
                       (set! defs (cons def defs))
                       (set! mods (cons mod mods)))))
                 vals)
       (values (flatten1 defs)
               (flatten1 mods))))))

(define (rename-export-resolver env . renames)
  (values (map (lambda (rename)
                 (if (not (and (list? rename)
                               (eq? 2 (length rename))))
                     (error "Invalid exports declaration"
                            rename))
                 (export-helper env
                                (car rename)
                                (cadr rename)))
               renames)
          '()))

(define (re-export-export-resolver env . import-decls)
  (resolve-imports import-decls (environment-module env)))

(set! *export-resolvers*
      `((rename: . ,rename-export-resolver)
        (re-export: . ,re-export-export-resolver)))