;;; CPSC 453 assignment 1

(in-package "TL-USER")

(unless (find-package "GL-TEST-BOX")
  (make-package "GL-TEST-BOX" :use '("TL")))

(defparameter tl-user::*TL-CURRENT-PROJECT-COPYRIGHT-STRING*
  "Copyleft 2004 Vladimir Sedach. All warranties disclaimed.")

(declare-system (gl-test-box :main-function gl-test-box::main
		             ;;:extra-h-files ("GL/glut")
			     )
  boot
  test-box
  )