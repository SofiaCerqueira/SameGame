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
	;;(format t " ~a ~%" total_list )	
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
	(format t " total list : ~a ~%" total_list )
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


(defun lista-convert-to-array (lista)
	(setf state (make-array '(4 4) 
   		:initial-contents lista)
	)
    (write state)
    state
    )


(defun list_set_limits_size (board)
	(setq array_size_col (list-length (car board) ))
	(setq  array_size_lin (list-length board ))
	(format t " ~a ~%"  array_size_col )
	(format t " ~a ~%"  array_size_lin )
	)

(defun create-auxiliar-board ()
	(setq *board_aux* (make-array (list array_size_lin array_size_col))) 
	(dotimes (i array_size_lin)
			(dotimes (j array_size_col)
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
	)

(defun list_is_member (lista linha coluna)
	(find (list linha coluna) lista :test #'equal)

	)

(defun find_color_cluster_positions (posx posy board color list-neighbors ) 
	(if (and (>= posx 0) (and (>= posy 0) (and (< posx array_size_lin) (< posy array_size_col))))
		(progn 
			(setq left_side (- posx 1))
			(setq rigth_side (+ posx 1))
			(setq top_side (- posy 1))
			(setq bottom_side (+ posy 1))

			(if (and (> posx 0) (and ( = (get_value left_side posy array_board) color ) (list_is_member list-neighbors left_side posy )))
				(progn
					(setq list-neighbors (append  list-neighbors (list left_side posy) ))
					(format t " ~a ~%"  list-neighbors )
					(if (= (get_value left_side posy *board_aux*) 0)
						(progn 
							(fill_value left_side posy *board_aux* 1)
							(find_color_cluster_positions left_side posy board color list-neighbors  ) 
						)
					) 
				)
			)

			(if (and (> posy 0)  (and ( = (get_value posx top_side array_board) color ) (list_is_member list-neighbors posx  top_side )))
				(progn
					(setq list-neighbors (append  list-neighbors (list posx  top_side) ))
					(format t " ~a ~%"  list-neighbors )
					(if (= (get_value posx  top_side *board_aux*) 0)
						(progn 
							(fill_value posx  top_side  *board_aux* 1)
							(find_color_cluster_positions posx  top_side board color list-neighbors  ) 
						)
					) 
				)
			)

			(if (and (> posx (- *array_size_lin* 1))  (and ( = (get_value rigth_side posy array_board) color ) (list_is_member list-neighbors rigth_side posy )))
				(progn
					(setq list-neighbors (append  list-neighbors (list rigth_side posy) ))
					(format t " ~a ~%"  list-neighbors )
					(if (= (get_value rigth_side posy *board_aux*) 0)
						(progn 
							(fill_value rigth_side posy  *board_aux* 1)
							(find_color_cluster_positions rigth_side posy board color list-neighbors  ) 
						)
					) 
				)
			)

			(if (and (> posy (- *array_size_col 1))  (and ( = (get_value posx bottom_side array_board) color ) (list_is_member list-neighbors posx bottom_sidey )))
				(progn
					(setq list-neighbors (append  list-neighbors (list posx bottom_side ) ))
					(format t " ~a ~%"  list-neighbors )
					(if (= (get_value posx bottom_side *board_aux*) 0)
						(progn 
							(fill_value posx bottom_side  *board_aux* 1)
							(find_color_cluster_positions posx bottom_side board color list-neighbors  ) 
						)
					) 
				)
			)



			(if (= (list-length list-neighbors) 0 )
				(setq list-neighbors (append  list-neighbors (list posx posy ) ))
			)
			

		)
	)
	list-neighbors
)





(defun find_color_blocks (board)
	(setq empList (make-hash-table)) 
	(setq counter (+ counter 0) )
	(setq array_main (lista-convert-to-array board))
	;;(setq *board_aux* (create-auxiliar-board)  )
	(dotimes (i array_size_lin)
		(dotimes (j array_size_col)
			(if ( = (get_value i j *board_aux*) 0)
				(progn
					(fill_value i j  *board_aux* 1)
					
					(if (/= (get_value i j array_main ) 0)
						(progn 
							(setf (gethash 'counter empList) (find_color_cluster_positions i j array_main color list-neighbors))
							(setq counter (+ counter 1) )
						)
					) 
				)
				
			)
			;(format t " ~a ~%"  )
				; 
		)
	)


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

(format t " ~a ~%"  :(- 3 1))

(setq board '((1 0 4 0) (0 0 4 0) (3 0 4 0) (0 0 1 0)) )
(setq board_comp (compactar-tabuleiro board))
(list_set_limits_size board )
( create-auxiliar-board)
(find_color_blocks board)

;;(setq board_comp (compactar-tabuleiro (lista-convert-to-array  board_comp )))




;;(list_set_limits_size '((1 0 4 0) (0 0 4 0) (3 0 4 0)) )





;;
;;def board_remove_group(board, group):
;;
;;    copy_board = []
;;    i = 0
;;    j = 0
;;    for i in range(len(board)):
 ;;       copy_list = board[i][:]
;;        copy_board.append(copy_list)
;;    for j in range(len(group)):
;;        pos = group[j]
;;        
;;        line = pos_l(pos)
;;        column = pos_c(pos)
;;        copy_board[line][column] = 0
;;
;;    
;;    copy_board = vertical_compact(copy_board)
;;    copy_board = horizontal_compact(copy_board)
;;    return copy_board



;;(compactar-horizontal '((1 2 2 3 3) (2 2 2 1 3) (1 2 2 2 2) (1 1 1 1 1)))

;;
