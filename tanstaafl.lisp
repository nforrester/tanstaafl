; Tanstaafl - A free space flight simulator
; Copyright (C) 2009  Neil Forrester
; 
; This program is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.
; 
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
; 
; You should have received a copy of the GNU General Public License
; along with this program.  If not, see <http://www.gnu.org/licenses/>.

; TANSTAAFL = There Ain't No Such Thing As A Free Lunch
; By Neil Forrester

(setf custom:*floating-point-contagion-ansi* t)

(load "util.lisp")

(load "math.lisp")
(load "physics.lisp")
(load "elements.lisp")

(load "char-set.lisp")
(load "gl-bindings.lisp")

(load "gui.lisp")

(load "menu.lisp")
(load "button.lisp")
(load "mfd.lisp")
(load "hud-layer.lisp")

(load "test-mfd.lisp")
(load "orbit-mfd.lisp")
(load "relative-hud.lisp")

(load "thruster.lisp")
(load "vessel.lisp")
(load "autopilot.lisp")

(load "vsop87.lisp")
(load "astronomical-bodies.lisp")

(defvar *state-output-stream* t)
(defvar *command-input-stream* t)
(defvar *time-acceleration* 1)
(defvar *wait-for-command* nil)

(defun process-command-line-args ()
  (let (active-arg)
    (dolist (a *args*)
      (cond
        ((or (string= a "-i") (string= a "--input")) (setf active-arg :input))
        ((eq active-arg :input) (setf *command-input-stream* (open a)) (setf active-arg nil))

        ((or (string= a "-o") (string= a "--output")) (setf active-arg :output))
        ((eq active-arg :output) (setf *state-output-stream* (open a :direction :output)) (setf active-arg nil))

        ((or (string= a "-t") (string= a "--time-acceleration")) (setf active-arg :time-acceleration))
        ((eq active-arg :time-acceleration) (setf *time-acceleration* (read-from-string a)) (setf active-arg nil))

        ((or (string= a "-w") (string= a "--wait-for-command")) (setf active-arg :wait-for-command))
        ((eq active-arg :wait-for-command) (setf *wait-for-command* a) (setf active-arg nil))

        (t (format *error-output* "Unrecognized command line option: ~a" a))))))

(defun wait-for-command-if-needed ()
  (if (not (eq nil *wait-for-command*))
    (loop
      (if (equal *wait-for-command* (get-command))
        (return)
        (sleep .01)))))

; and here... we... go!

(process-command-line-args)

(wait-for-command-if-needed)

(defvar *epoch-time* 0d0)

(setf *sun* (make-instance 'vsop-star
                           :name "Sun"
                           :mass 1.9891d30
                           :radius 695500000d0
                           :vsop-base-interval 1049
                           :x-series-set *vsop-series-set-sun-x*
                           :y-series-set *vsop-series-set-sun-y*
                           :z-series-set *vsop-series-set-sun-z*))

(setf *mercury* (make-instance 'vsop-planet
                               :name "Mercury"
                               :mass 3.3022d23
                               :radius 2439700d0
                               :vsop-base-interval 1000
                               :x-series-set *vsop-series-set-mercury-x*
                               :y-series-set *vsop-series-set-mercury-y*
                               :z-series-set *vsop-series-set-mercury-z*))

(setf *venus* (make-instance 'vsop-planet
                             :name "Venus"
                             :mass 4.8685d24
                             :radius 6051800d0
                             :vsop-base-interval 1001
                             :x-series-set *vsop-series-set-venus-x*
                             :y-series-set *vsop-series-set-venus-y*
                             :z-series-set *vsop-series-set-venus-z*))

(setf *earth* (make-instance 'vsop-planet
                             :name "Earth"
                             :mass 5.9742d24
                             :radius 6.3781d6
                             :vsop-base-interval 1002
                             :x-series-set *vsop-series-set-earth-x*
                             :y-series-set *vsop-series-set-earth-y*
                             :z-series-set *vsop-series-set-earth-z*))

(setf *mars* (make-instance 'vsop-planet
                            :name "Mars"
                            :mass 6.4185d23
                            :radius 3386200d0
                            :vsop-base-interval 1003
                            :x-series-set *vsop-series-set-mars-x*
                            :y-series-set *vsop-series-set-mars-y*
                            :z-series-set *vsop-series-set-mars-z*))

(setf *jupiter* (make-instance 'vsop-planet
                               :name "Jupiter"
                               :mass 1.8986d27
                               :radius 69000000d0
                               :vsop-base-interval 1004
                               :x-series-set *vsop-series-set-jupiter-x*
                               :y-series-set *vsop-series-set-jupiter-y*
                               :z-series-set *vsop-series-set-jupiter-z*))

(setf *saturn* (make-instance 'vsop-planet
                              :name "Saturn"
                              :mass 5.6846d26
                              :radius 58000000d0
                              :vsop-base-interval 1005
                              :x-series-set *vsop-series-set-saturn-x*
                              :y-series-set *vsop-series-set-saturn-y*
                              :z-series-set *vsop-series-set-saturn-z*))

(setf *uranus* (make-instance 'vsop-planet
                              :name "Uranus"
                              :mass 8.6810d25
                              :radius 25200000d0
                              :vsop-base-interval 1006
                              :x-series-set *vsop-series-set-uranus-x*
                              :y-series-set *vsop-series-set-uranus-y*
                              :z-series-set *vsop-series-set-uranus-z*))

(setf *neptune* (make-instance 'vsop-planet
                               :name "Neptune"
                               :mass 1.0243d26
                               :radius 24500000d0
                               :vsop-base-interval 1007
                               :x-series-set *vsop-series-set-neptune-x*
                               :y-series-set *vsop-series-set-neptune-y*
                               :z-series-set *vsop-series-set-neptune-z*))

(setf *tp1* (make-instance 'vessel
                           :name "Teapot"
                           :mass 1d0
                           :pos (make-vector-3 (+ 64000000d0 1.4572625236939914d11) -3.5401098170742485d10 -2.221160603393515d9)
                           :inertia-tensor (compute-inertia-tensor 1d0 1d0 1d0)
                           :radius 2d0
                           :max-torque (make-vector-3 1d0 1d0 1d0)
                           :vel (make-vector-3 4000.0d0 29783.0d0 0d0)))
(setf *tp2* (make-instance 'vessel
                           :name "Teapot-2"
                           :mass 1d0
                           :pos (make-vector-3 (+ 64000000d0 1.4572625236939914d11) (+ 10d0 -3.5401098170742485d10) -2.221160603393515d9)
                           :inertia-tensor (compute-inertia-tensor 1d0 1d0 1d0)
                           :radius 2d0
                           :max-torque (make-vector-3 1d0 1d0 1d0)
                           :vel (make-vector-3 4000.0d0 29783.0d0 0d0)))

(defvar *all-objs* (list *tp1* *tp2* *sun* *mercury* *venus* *earth* *mars* *jupiter* *saturn* *uranus* *neptune*))
;(defvar *all-objs* (list *tp1* *tp2* *earth*))

(make-instance 'menu
               :anchor-point (make-vector-2 1 1)
               :pos (make-vector-2 0.5 0.5)
               :items (list (list 1 "f") (list 2 "s") (list 3 "t"))
               :selection-function #'(lambda (selection)
                                       (print selection)))

(make-instance 'text-bg-button
               :anchor-point (make-vector-2 0 1)
               :text (format nil "Hello World!")
               :pos (make-vector-2 .2 .9)
               :background-color (make-color 0 1 0 0.5)
               :text-color       (make-color 1 0 0 1)
               :click-function #'(lambda () (print "click")))

(make-instance 'test-mfd :color (make-color .4 0 .4 .5) :pos (make-vector-2 .4 .4) :anchor-point (make-vector-2 0 1) :max-size (make-vector-2 .3 .3))
(make-instance 'orbit-mfd
               :anchor-point (make-vector-2 .5 .5)
               :pos (make-vector-2 .5 .5)
               :max-size (make-vector-2 .5 .5)
            ;       :major-body *sun*
            ;       :minor-bodies (list *mercury* *venus* *earth* *mars* *jupiter* *saturn* *uranus* *neptune*))))
            ;       :major-body *earth*
            ;       :minor-bodies (list *tp1*))))
               :major-body *earth*
               :minor-bodies (list *tp1* *tp2*))

(defvar *all-hud-layers* ())

(make-instance 'relative-hud
               :color (make-color 0 0 1 0.8)
               :origin *tp1*
               :target *tp2*)
(make-instance 'relative-hud
               :color (make-color 1 0.5 1 0.8)
               :origin *tp1*
               :target *sun*)
(make-instance 'relative-hud
               :color (make-color 1 0.5 0 0.8)
               :origin *tp1*
               :target *mercury*)
(make-instance 'relative-hud
               :color (make-color 1 0.5 0 0.8)
               :origin *tp1*
               :target *venus*)
(make-instance 'relative-hud
               :color (make-color 1 0.5 0 0.8)
               :origin *tp1*
               :target *earth*)
(make-instance 'relative-hud
               :color (make-color 1 0.5 0 0.8)
               :origin *tp1*
               :target *mars*)
(make-instance 'relative-hud
               :color (make-color 1 0.5 0 0.8)
               :origin *tp1*
               :target *jupiter*)
(make-instance 'relative-hud
               :color (make-color 1 0.5 0 0.8)
               :origin *tp1*
               :target *saturn*)
(make-instance 'relative-hud
               :color (make-color 1 0.5 0 0.8)
               :origin *tp1*
               :target *uranus*)
(make-instance 'relative-hud
               :color (make-color 1 0.5 0 0.8)
               :origin *tp1*
               :target *neptune*)

(setf *focused-object* (first *all-objs*))

(make-simple-thruster-setup (first *all-objs*))

(main-loop)
