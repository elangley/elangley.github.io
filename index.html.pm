#lang pollen
◊(require pollen/pagetree pollen/template sugar/coerce)

◊headline{Table Of Contents}
◊(let () (current-pagetree (load-pagetree "index.ptree")) "")

◊(define (node->link node #:capitalize [caps? #f])
    (define node-string (->string node))
    (define node-titles "Lambda Calc Demo")
    (define link-name
       (let* ([name (if (dev-mode?) 
                       node-titles
                       node-titles)]
             [name (if caps? (capitalize-first-letter name) name)])
         name))
   ◊link[node-string]{◊link-name})

◊(define (make-toc-subsection pagenode)
  (define node-children (children pagenode))
  ◊div{
    ◊h3{◊(node->link pagenode #:capitalize #t)}
    ◊(if node-children
      (apply ul (map (compose1 li node->link) node-children))
      "")})
◊(apply div #:class "toc" 
  (map make-toc-subsection '(test1.html)))
