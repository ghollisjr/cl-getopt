(require 'getopt)

(defun test ()
  (let* ((args (list "-f hello.txt output.txt"))
         (options
          (list (list "verbose"
                      :none
                      nil)
                (list "file"
                      :required
                      nil)
                (list "f"
                      :required
                      nil))))
    (getopt:getopt args options)))
