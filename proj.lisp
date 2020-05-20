(in-package :user)

;;https://github.com/srps/SameGame/blob/master/g012.lisp

(defvar *array_size_col* )
(defvar *array_size_lin* )
(defvar *board_aux* )

(defun transpose-board (board)
	(setq new_list nil)
	(setq rest_list nil)
	(setq form_list nil)
	(setq limit (list-length (car board) ))
	(dotimes (y limit)

		(dolist (li board)
			(setq new_list (append new_list (list (car li))))
			(setq rest_list (append rest_list (list (cdr li))))

		)
				
		(setq form_list (append  form_list (list new_list) ))
		(setq board rest_list )
		(setq new_list nil)
		(setq rest_list nil)
	)
	(format t " ~a ~%" form_list)
	(return-from transpose-board form_list)
)


(defun compactar-horizontal (form_list)
	(setq zero_list nil)
	(setq non_zero_list nil)
	(setq total_list nil)
	(dolist (lista form_list)
		(dolist (el lista)
			(if (= el 0)
   				(setq zero_list (append zero_list (list el)))
				(setq non_zero_list (append non_zero_list (list el)))

			)
		)
		(setq total_list (append total_list (list  (append zero_list non_zero_list )) ))
		(setq zero_list nil)
		(setq non_zero_list nil)

	)
	; Era o nome das funções que estava trocada ou a string embaixo?
	(format t " Compacta vertical ~a ~%" total_list )	
	(return-from compactar-horizontal total_list)
)


(defun compactar-vertical (total_list)
	(dolist (lista total_list)
			(if (= (reduce '+ lista) 0)
				(setq zero_list  (append zero_list (list lista)))
				(setq non_zero_list (append non_zero_list (list lista)))
			)
	)
	(setq total_list (append non_zero_list zero_list  )) 
	(format t " compacta horizontal : ~a ~%" total_list )
	(return-from compactar-vertical total_list)
)


(defun compactar-tabuleiro (board)
	
	(setq form_list (transpose-board board))  
	(setq total_list (compactar-horizontal form_list))
	(setq total_list2 (compactar-vertical total_list))
	(setq transpose_list (transpose-board total_list2))
	;;(format t " ~a ~%" (transpose-board total_list) )
	(return-from compactar-tabuleiro transpose_list)
)

;(setq new ( transpose-board '((1 0 4 0) (0 0 4 0) (3 0 4 2))))
;(setq new2 (compactar-horizontal new))
;(setq new3 (compactar-vertical new2))
;(setq new2 (transpose-board new3))

(defun lista-convert-to-array (lista)
	(setf state (make-array '(4 4) 
   		:initial-contents lista)
	)    
    state
)


(defun list_set_limits_size (board)
	(setq *array_size_col* (list-length (car board) ))
	(setq  *array_size_lin* (list-length board ))
	(format t " ~a ~%"  *array_size_col* )
	(format t " ~a ~%"  *array_size_lin* )
)


(defun create-auxiliar-board ()
	(setq *board_aux* (make-array (list *array_size_lin* *array_size_col*))) 
	(dotimes (i *array_size_lin*)
			(dotimes (j *array_size_col*)
  				(setf (aref *board_aux* i j) 0 )
			)
	)
	(return-from create-auxiliar-board *board_aux*)    	
)


(defun get_value (posx posy array_board)
	(setq value (aref array_board posx posy))
	value
)


(defun fill_value  (posx posy array_board value) 
	(setf (aref array_board posx posy) value )
	array_board
)


(defun list_is_member (lista linha coluna)
	(return-from  list_is_member (find (list linha coluna) lista :test #'equal))
)


(defun find_color_cluster_positions (posx posy array_board color list-neighbors ) 
	(if (and (>= posx 0) (and (>= posy 0) (and (< posx *array_size_lin*) (< posy *array_size_col*))))
		(progn 
			(setq t_side (- posx 1))
			(setq b_side (+ posx 1))
			(setq l_side (- posy 1))
			(setq r_side (+ posy 1))
			;(format t " estou aqui ~%"  )
			(if (and (> posx 0) (and ( = (get_value t_side posy array_board) color ) (not (list_is_member list-neighbors t_side posy ))))
				(progn
					(setq list-neighbors (append  list-neighbors (list (list t_side posy) )))
					(format t " ~a ~%"  list-neighbors )
					(if (= (get_value t_side posy *board_aux*) 0)
						(progn 
							(fill_value t_side posy *board_aux* 1)
							(setq list-neighbors (find_color_cluster_positions t_side posy array_board color list-neighbors  ) )
						)
					) 
				)
			)
			;(format t " estou aqui2 ~%"  )
			(if (and (> posy 0)  (and ( = (get_value posx l_side array_board) color ) (not (list_is_member list-neighbors posx  l_side ))))
				(progn
					(setq list-neighbors (append  list-neighbors (list (list posx  l_side) )))
					(format t " ~a ~%"  list-neighbors )
					(if (= (get_value posx  l_side *board_aux*) 0)
						(progn 
							(fill_value posx  l_side  *board_aux* 1)
							(setq list-neighbors  (find_color_cluster_positions posx  l_side array_board color list-neighbors  ) )
						)
					) 
				)
			)

			(if (and (< posx (- *array_size_lin* 1))  (and ( = (get_value b_side posy array_board) color ) (not (list_is_member list-neighbors b_side posy ))))
				(progn
					(setq list-neighbors (append  list-neighbors  (list (list b_side posy) )))
					(format t " ~a ~%"  list-neighbors )
					(if (= (get_value b_side posy *board_aux*) 0)
						(progn 
							(fill_value b_side posy  *board_aux* 1)
							(setq list-neighbors  (find_color_cluster_positions b_side posy array_board color list-neighbors  ) )
						)
					) 
				)
			)

			(if (and (< posy (- *array_size_col* 1))  (and ( = (get_value posx r_side array_board) color ) (not (list_is_member list-neighbors posx r_sidey ))))
				(progn
					(setq list-neighbors (append  list-neighbors  (list (list posx r_side ) )))
					(format t " ~a ~%"  list-neighbors )
					(if (= (get_value posx r_side *board_aux*) 0)
						(progn 
							(fill_value posx rside  *board_aux* 1)
							(setq list-neighbors  (find_color_cluster_positions posx r_side array_board color list-neighbors  ) )
						)
					) 
				)
			)
			(if (= (list-length list-neighbors) 0 )
				(setq list-neighbors (append  list-neighbors (list posx posy ) ))
			)
			;(format t " estou aqui3 ~%"  )
		)
	)
	list-neighbors
)


(defun find_color_blocks (board)
	(setq empList (make-hash-table)) 
	(setq counter  0) 
	(setq array_main (lista-convert-to-array board))
	;;(setq *board_aux* (create-auxiliar-board)  )

	(dotimes (i *array_size_lin*)
		(dotimes (j *array_size_col*)
			(if ( = (get_value i j *board_aux*) 0)
				(progn
					(fill_value i j  *board_aux* 1)
					(if (/= (get_value i j array_main ) 0)
						(progn 
							(setq list-neighbors '())
							(setq color (get_value i j array_main) )
							(setq lista (find_color_cluster_positions i j array_main color list-neighbors) )
							(setf (gethash counter empList) lista )
							;(format t " estou aqui4 ~%"  )
							(setq counter (+ counter 1) )
						)
					) 
				)
			)
		)
	)

	empList	
)


(defun board_remove_group (board group)
	(setq newBoard nil)
	(loop for line in board do
		(setq copy_list (copy-tree line))		; function copy-list if we intend to preserve the elements
		(setq newBoard (append newBoard (list copy_list)))
	)
	; alternatively (setq newBoard (copy-tree board))

	(loop for el in group do
		(setq l (car el))
		(setq c (cadr el))
		(setf (nth c (nth l newBoard)) 0)
	)
	(setq newBoard (compactar-tabuleiro newBoard))
	newBoard
)


(defclass SGState()
	((_board
		:initarg :board
		:accessor SGState-board)
	)
)

(defmethod lessThan ((o1 SGState) (o2 SGState))
	(< (list-length (SGState-board o1)) (list-length (SGState-board o2)) )
)


(defclass SameGame()	; FIXME inherits from Problem
	((_board :initarg :board :accessor SameGame-board)
	 (_initial :accessor SameGame-initial))
)

(defmethod initialize-instance :after ((o SameGame) &key)
	(setf (SameGame-initial o) (make-instance 'SGState :board (SameGame-board o)))
)
; Este constructor todo só para ter:
; class same_game(Problem):
; 	def __init__(self, b):
; 		self.board = b
;		self.initial = SGState(b)

(defmethod actions ((s  SGState))
	

	(setf possible_actions (find_color_blocks (SGState-board s)))
	(format t " Actions ~a ~%" possible_actions)
	(setq actions NIL )
	(loop for v being the hash-value in possible_actions
      do 
      	(progn 
      		(if (> (list-length v) 1)
      			(setq actions (append actions (list v)) )
      		)

      	)
      	
      )
       
	(format t " Actions ~a ~%" actions)
	actions
)



(defmethod goal_test ((s SGState))
	(setq idx_last_line (- (list-length (SGState-board s)) 1))
	(setq line_board (nth idx_last_line SGState-board s))
	(loop for pos in line_board do
		(if (/= pos 0)
			(return-from goal_test nil)
		)
	)
	t
)

(defmethod path_cost ((c integer) (s1 SGState) (s2 SGState) (action list))
	(+ c 1)
)

(defmethod result ((s SGState) (action list))
	(setq b (board_remove_group (SGState-board s) action))
	(make-instance 'SGState :board b)
)

(defparameter s1 (make-instance 'SGState :board '((1 0 4 0) (0 0 4 0) (3 0 4 0) (0 0 1 0)) ))
( list_set_limits_size '((1 0 4 0) (0 0 4 0) (3 0 4 0) (0 0 1 0)))
(create-auxiliar-board)
(actions s1)

(defparameter sg1 (make-instance 'SameGame :board '((1 1 4 0) (1 0 4 0) (3 0 4 0) (0 0 1 0)) ))
(create-auxiliar-board)
(setq remove_aux '((0 0) (0 1) (1 0)))
(write (SGState-board (result (SameGame-initial sg1) remove_aux))) (terpri)


;; ----------------------------------------------------------------------------------------------------------


(defun 2d-array-to-list (array)
  (setf lisa (loop for i below (array-dimension array 0)
        collect (loop for j below (array-dimension array 1)
                      collect (aref array i j))))
  
  (return-from 2d-array-to-list  lisa)
)

;;def board_find_groups(board):
;;    i =0
;;    j =0
;;    index_list = 0
;;    board_aux = []
;;    board_aux = board_auxiliar(board)
;;    groups_total = []
;;    d =[]
;    for i in range(len(board)):
;        for j in range(len(board[0])):
;            neighbors=[]
;            if(board_aux[i][j]!=1):
;                board_aux[i][j] = 1
;                if(board[i][j]!=0):
;                    groups_total.insert(index_list, find_neighbors(i, j, neighbors, board, board_aux))
;;                    index_list +=1
;
;    #print(groups_total)
;    return groups_total


;(format t " ~a ~%"  :(- 3 1))

;(setq board_comp (compactar-tabuleiro board))
;(list_set_limits_size board)
;(create-auxiliar-board)

;(format t " ~a ~%"  (find_color_blocks board))

;(setq board_comp (compactar-tabuleiro (lista-convert-to-array  board_comp )))

;;(list_set_limits_size '( )

;(setq array (lista-convert-to-array board))

;(format t " ~a ~%"  (2d-array-to-list array))

;;(compactar-horizontal '((1 2 2 3 3) (2 2 2 1 3) (1 2 2 2 2) (1 1 1 1 1)))
