(in-package :user)
(compile-file "procura.lisp")
(load "procura")


;Grupo 12
;; André Filipe Leitão nº 87629
;; Sofia Cerqueira nº81200

;###########################################################################################
;################################### VARIVAVEIS GLOBAIS#####################################

(defvar *array_size_col* 0 )
(defvar *array_size_lin* 0  )
(defvar *board_aux* )
(defvar *start-clock* )



(defvar *lista_ramos* '())
(defvar *estado_terminal* )
(defconstant MAX_TIME 300)
(defvar *nos_gerados* 0 )
(defvar *nos_expandidos* 0)

(defvar *best_result* )

;###########################################################################################
;######################### FUNCOES DE MOVIMENTACAO DO TABULERIO ############################


;Tabuleiro transposto: auxilia a compactacao do tabileiro verticalmente e horizontalmente
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
	(return-from transpose-board form_list)
)

;compacta valores para a esquerda
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
	(return-from compactar-horizontal total_list)
)

;Acao da gravidade- > compacta para baixo
(defun compactar-vertical (total_list)
	(dolist (lista total_list)
			(if (= (reduce '+ lista) 0)
				(setq zero_list  (append zero_list (list lista)))
				(setq non_zero_list (append non_zero_list (list lista)))
			)
	)
	(setq total_list (append non_zero_list zero_list  )) 
	(return-from compactar-vertical total_list)
)

; funcao chamada para compactar tabuleiro de jogo e retorna o
(defun compactar-tabuleiro (board)
	
	(setq form_list (transpose-board board))  
	(setq total_list (compactar-horizontal form_list))
	(setq total_list2 (compactar-vertical total_list))
	(setq transpose_list (transpose-board total_list2))
	(return-from compactar-tabuleiro transpose_list)
)


; funcao auxiliar para converter lista para array
(defun lista-convert-to-array (lista)
	(setf state (make-array (list *array_size_lin* *array_size_col*)
   		:initial-contents lista)
	)    
    state
)

;funcao que define tamanhos do tabuleiro por exemplo 4*5
(defun list_set_limits_size (board)
	(setq *array_size_col* (list-length (car board) ))
	(setq  *array_size_lin* (list-length board ))
)

;retorna tabuleiro auxiliar (inciado a zeros) para ser indicar que celulas/posicoes foram visitadas
(defun create-auxiliar-board ()
	(setq *board_aux* (make-array (list *array_size_lin* *array_size_col*))) 
	(dotimes (i *array_size_lin*)
			(dotimes (j *array_size_col*)
  				(setf (aref *board_aux* i j) 0 )
			)
	)
	(return-from create-auxiliar-board *board_aux*)    	
)

;funcao paa obter valor da posicao x y ou seja linha x  coluna y
(defun get_value (posx posy array_board)
	(setq value (aref array_board posx posy))
	value
)

;funcao para prencher valor da posicao x y ou seja linha x  coluna y
(defun fill_value  (posx posy array_board value) 
	(setf (aref array_board posx posy) value )
	array_board
)

;funcao que verifica se tuplo (x,y) esta presente na lista
(defun list_is_member (lista linha coluna)
	(return-from  list_is_member (find (list linha coluna) lista :test #'equal))
)

;funcao recursiva que identica todos os grupos/blocos no tabuleiro -> retorna bloco com bolas da mesma cor vizinhas
; olha para bolas adjacentes nas quatro direcçoes topo, baixo, esquerda e direita
(defun find_color_cluster_positions (posx posy array_board color list-neighbors ) 
	
	(if (and (>= posx 0) (and (>= posy 0) (and (< posx *array_size_lin*) (< posy *array_size_col*))))

		(progn 
			(setq t_side (- posx 1))
			(setq b_side (+ posx 1))
			(setq l_side (- posy 1))
			(setq r_side (+ posy 1))

			(setq bound_rigth (- *array_size_col* 1))
			(setq bound_bottom (- *array_size_lin* 1))
			
			(if (>= t_side 0) 
				(progn 
					(if (and ( = (get_value t_side posy array_board) color ) (not (list_is_member list-neighbors t_side posy )))
						(progn
							(setq list-neighbors (append  list-neighbors (list (list t_side posy) )))							
							(if (= (get_value t_side posy *board_aux*) 0)
								(progn 									
									(fill_value t_side posy *board_aux* 1)
									(setq list-neighbors (find_color_cluster_positions t_side posy array_board color list-neighbors  ) )
								)
							) 
						)
					)
				)
			)
			
			(if  (>= l_side 0)  
				(progn 					
					(if (and ( = (get_value posx l_side array_board) color ) (not (list_is_member list-neighbors posx  l_side )))
						(progn
							(setq list-neighbors (append  list-neighbors (list (list posx  l_side) )))							
							(if (= (get_value posx  l_side *board_aux*) 0)
								(progn 									
									(fill_value posx  l_side  *board_aux* 1)
									(setq list-neighbors  (find_color_cluster_positions posx  l_side array_board color list-neighbors  ) )
								)
							) 
						)
					)
				)
			)

			(if  (<= b_side bound_bottom)  
				(progn 					
					(if (and ( = (get_value b_side posy array_board) color ) (not (list_is_member list-neighbors b_side posy )))
						(progn
							(setq list-neighbors (append  list-neighbors  (list (list b_side posy) )))							
							(if (= (get_value b_side posy *board_aux*) 0)
								(progn 									
									(fill_value b_side posy  *board_aux* 1)
									(setq list-neighbors  (find_color_cluster_positions b_side posy array_board color list-neighbors  ) )
								)
							) 
						)
					)
				)
			)

			(if (<= r_side bound_rigth)  
				(progn 
					(if (and ( = (get_value posx r_side array_board) color ) (not (list_is_member list-neighbors posx r_side )))
						(progn
							(setq list-neighbors (append  list-neighbors  (list (list posx r_side ) )))
							(if (= (get_value posx r_side *board_aux*) 0)
								(progn 
									(fill_value posx r_side  *board_aux* 1)
									(setq list-neighbors  (find_color_cluster_positions posx r_side array_board color list-neighbors  ) )
								)
							) 
						)
					)
				)
			)
			(if (= (list-length list-neighbors) 0 )
				(setq list-neighbors (list (append  list-neighbors (list posx posy ) )))
			)
			
		)
	)
	list-neighbors
)

;funcao que itera tabuleiro ara encontrar todos os blocos de cores iguais -> retorna hash em que cada key tem um bloco (lista de posicoes)
(defun find_color_blocks (board)
	(setq empList (make-hash-table)) 
	(setq counter  0) 
	(setq array_main (lista-convert-to-array board))
	(setq *board_aux* (create-auxiliar-board)  )

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
							(setq counter (+ counter 1) )
						)
					) 
				)
			)
		)
	)

	empList	
)

;funcao que remove grupo/bloco do tabuleiro
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

;###########################################################################################
;######################### ESTRUTURA DE DADOS PARA REPRESENTAR NO ##########################

;estrutura no
;board -> tabuleior (type lista)
;points -> pontuacao do estado currente
;depth -> profundidade do estado atual
;n_groups -> grupos/blocos possiveis no estado atual (variavek utilizada na heuristica 1 e 3)
;n-balls -> bolas que ainda estão no tabuliro (variavel utlizada na heuristica 2 e 3)
;possible_actions -> bolleano para indicar se existem açoes possiveis (pouco importante)
;position_first_ball -> bola que idendica um grupo
;return_list_path -> lista que contem resultado do resolve-same-game 

(defstruct node 
   board 
   (points 0 :type integer)
   (depth 0 :type integer) 
   (n_groups 0 :type integer)
   n-balls
   (possible_actions nil :type boolean)
   position_first_ball 
   return_list_path
)

;###########################################################################################
;######################### METRICAS PARA AVALIAR ALGORITMO #################################

;acumula nos gerados
(defun incr_nos_gerados ()
	(setq *nos_gerados* (+ *nos_gerados* 1))
)
;acumula nos expandidos
(defun incr_nos_expandidos ()
	(setq *nos_expandidos* (+ *nos_expandidos* 1))
)

; acumula lista com numero de ramos gerados
(defun guarda_numero_ramos_gerados (nr_ramos)
	(setq *lista_ramos* (append  *lista_ramos* (list nr_ramos) ))
)

;calcula fator mdedio de ramificacao
(defun mean_lista_ramos ()
	( / (reduce '+ *lista_ramos*) ( list-length  *lista_ramos*) )
)

;muda pontuacao de um estado
(defun pontuacao (state ant-points n_balls)
	(setf (node-points state) (+ ant-points (expt (- n_balls 2) 2)))	
)

;muda profundidade de um estado
(defun profundidade (state ant-depth)
	(setf (node-depth state) (+ ant-depth 1))
)
;muda return_list_path para agregar novo tuplo (x, y) que indica posicçaõ de uma das bolas do bloco removido
(defun append_path_new_pos (board_state path  new_pos  )
	(setf (node-return_list_path board_state) (append  path (list new_pos )))
)
;###########################################################################################
;######################### FUNCOES AULIARES AO ALGORITMOS DE PROCURA########################


;funcao que lista os operadores, retona todos os estados possiveis (lista de elementos do tipo node (estrutura de dados acima referida))
(defun lista-operadores (estado)
	(incr_nos_expandidos ) ; quando chamamos esta função lista-operadores estamos a expandir o estado/operador
	(setf possible_actions (find_color_blocks (node-board estado)))

	(setq nr_ramos 0)
	(setq actions NIL )
	(loop for v being the hash-value in possible_actions
      do 
      	(progn 
      		(setq n_pecas (list-length v))
      		

      		(if (> n_pecas 1)
      			(progn 
      				
      				(setf node-possible_actions t)
      				(incr_nos_gerados ) ; quando criamos novas instacias de estados estamos a gerar operadores/nos/estados
	      			(setq copy_state (copy-seq (node-board estado) ))
	      			(setq suc-state (board_remove_group copy_state  v))

	      			(setq n_balls (- (node-n-balls estado)  n_pecas  ))
	      			(setq board-suc (make-node :board  suc-state  :n-balls n_balls))

	      			(setq antecessor-points (node-points estado))
      				(pontuacao board-suc antecessor-points n_pecas)
      				(profundidade board-suc (node-depth estado))

	      			(setq actions (append actions (list board-suc)) )
	      			(incf nr_ramos)
	      			(setq path (node-return_list_path estado) )
	      			(setq pos (car v) )
	      			(append_path_new_pos board-suc path pos  )


	      		)
      		)
      	)     	
      )
	(guarda_numero_ramos_gerados nr_ramos)
	(return-from lista-operadores actions)
)

;funcao que verifica se estado é obejctivo
;retorna true quando atinge um estado terminal ou atinge limite de tempo
; em ambas opcoes  o estado final escolhido, sera aquele que tem maior pontuacao
(defun objectivo? (state)
	(setq flag1 t)

	(setq flag2 (<= (* MAX_TIME INTERNAL-TIME-UNITS-PER-SECOND) (- (get-internal-run-time) *start-clock*)))
	
	(setq idx_last_line (- (list-length (node-board state)) 1))
	(setq line_board (nth idx_last_line (node-board state)))
	(loop for pos in line_board do
		(if (/= pos 0)
			(setq flag1 nil)
		)
	)

	(if ( < (node-points *best_result* ) (node-points state ))
		(progn 
			(setq *best_result* state )
			(setq *estado_terminal* *best_result*)

		)
	)
	(if  ( or flag2 flag1)
		;(format t " result ~a ~%" (node-points state))
		(progn (setq *estado_terminal* *best_result*)
			   (return-from objectivo? t))
		)
	(return-from objectivo? nil)



)

;###########################################################################################
;################################ HEURIRTICAS #######################################

;escolhe o camnho com menos grupos, vai escolher aquele que fica com mais clusters
(defun heuristica1 (state)
	(setf groups-ht (find_color_blocks (node-board state)))
	(setf (node-n_groups state) (hash-table-count groups-ht) )
	(node-n_groups state)	
)
;numero de bolas no tabuleiro restantes
(defun heuristica2 (state)	
	(node-n-balls state)
)

;conbinação das duas heuristicas anteriores 
(defun heuristica3 (state)
(setf groups-ht (find_color_blocks (node-board state)))
	(setf (node-n_groups state) (hash-table-count groups-ht) )

	(if (<= (node-depth state) 3)
		(return-from heuristica3 (node-n_groups state))
		(return-from heuristica3 (node-n-balls state))

	)
)


;###########################################################################################
;################################ SONDAGEM ITERATIVA #######################################

;escolhe no aleatorio entre os sucessores do no currente
(defun choose_random (successors)
 	(setq num (random (list-length successors)))
    (nth num successors)
 )

;funcao recursiva da sondagem iterativa
(defun sondagem_iterativa_recursao ( no )
	(if (objectivo? no)
		(return-from sondagem_iterativa_recursao no)
	)
	(setq successors (lista-operadores no))
	(if ( =  (list-length successors) 0)
		(return-from  sondagem_iterativa_recursao nil)
		(progn 
			(setq random_no (choose_random successors))
			(return-from sondagem_iterativa_recursao (sondagem_iterativa_recursao random_no))
		)
	)
)

;fucao principal da sondagem iterativa
(defun sondagem_iterativa (estado)
	(loop 
		(setq result (sondagem_iterativa_recursao estado) )
		(if  (not (null  result))
			(return result)
		)
	)
)


;###########################################################################################
;##################### ABORDAGEM ALTERNAYIVA NAO USADA DDL-BBS #############################
;apropriada para arvores nao binarias no entanto performance fica pouco a desejar

(defun probe (no k l)
	
	(if (objectivo? no)
		(return-from probe (list no 0))
	)
	
	(setq successors (lista-operadores no))
	(if (> k 0)
		(setq successors (reverse successors ))
	)
	(setq i 0)
	(setq counter 0)
	(setq maxheight 0)
	
	(dolist (child successors)
		(if (and (= k 0) (>= counter 1) )
			(break)
		)

		(if (and (> k 0) (= i 0) )
			(setq new_k (- k 1))
			(setq new_k k)	
		)
		(setq result_goal (probe child new_k l) )
		(setq goal (car result_goal ))
		(setq height (nth 1 result_goal ))
		
		(setq maxheight (max maxheight (+ 1 height)))
		(if  (not (null goal))
			(return-from probe (list goal 0))
		)
		(setq i (+ i 1))
		(if ( >= height l )
			(setq counter (+ counter 1))
		)
	)

	(return-from probe (list nil maxheight))
)


(defun LDS_BBS (root l max_depth)
	(dotimes (n max_depth)
		(setq result_goal (probe root n l))
		(if  (not (null  (car result_goal)))
			(return-from LDS_BBS (car result_goal))
		)
	)
	(return-from LDS_BBS nil)
	)


;###########################################################################################
;################################ ABORDAGEM ALTERNATIVA ILDS  ##############################

;; ILDS- Improved Limited Discrepancy Seach, (slightly) modified
(defun ILDS (node n)
	(dotimes (k (+ n 1))

		(defparameter adt nil) ; boolean for all discrepancies taken
		(setf result (ILDSProbe node k n))
		(if (or (not (null result)) (not adt))
			(return-from ILDS result)
		)
	)
	nil
)

(defun ILDSProbe (node k rDepth)
	;; rDepth- Remainder Depth over which discrepancies can be taken

	(if (objectivo? node)	; isGoal
		(return-from ILDSProbe node)
	)

	(setq successors (lista-operadores node))
	;; if failed
	(if (= (list-length successors) 0)
		(return-from ILDSProbe nil)
	)
	
	
	
	(if (= k 0)
		(setq adt t)
	)

	;; if node has only one child
	(if (= (list-length successors) 1)
			(return-from ILDSProbe (ILDSProbe (car successors) (- k 1) (- rDepth 1)) )	
	)
	(setq result nil)
	(setf children (extract_rigth_left_node successors))
	
	;(terpri) (terpri) (write children)
	
	;; extract left and right children
	(if (> k 0)
		(setf result (ILDSProbe (car children) (- k 1) (- rDepth 1)))
	)
	(if (and (> rDepth k) (not result))
		(setf result (ILDSProbe (cadr children) k (- rDepth 1)))
	)
	result
)

;esta funcao escolhera dois nós dos sucessores de acordo com a heuristica
;adaptaçao feia para arvore não binaria
(defun extract_rigth_left_node (successors)
	(setq new_value 0)
	(setq right_child nil)
	(setq left_child nil)
	(setq rnode nil)
	(setq lnode nil)
	
	(dolist (n successors)
		(setf new_value (heuristica1 n))
		(cond
			((not right_child)
				(setq right_child new_value)
				(setq rnode n)
			)
			((not left_child)
				(if (> right_child new_value)
					(progn
						(setq left_child new_value)
						(setq lnode n)
					)
					(progn
						(setq left_child right_child)
						(setq lnode rnode)
						(setq right_child new_value)
						(setq rnode n)
					)
				)
			)
			(t (cond
					((> new_value right_child)
						(setq left_child right_child)
						(setq right_child new_value)
					)
					((> new_value left_child)
						(setq left_child new_value)
					)
				)
			)
		)
	)
	(return-from extract_rigth_left_node (list rnode lnode))
)
;(trace extract_rigth_left_node)
;(trace ILDSProbe )

;###########################################################################################
;################################ OUTRAS FUNCOES AUXILIARES  ##############################


(defun 2d-array-to-list (array)
  (setf lisa (loop for i below (array-dimension array 0)
        collect (loop for j below (array-dimension array 1)
                      collect (aref array i j))))
  
  (return-from 2d-array-to-list  lisa)
)

(defun start-clock ()
	(setq *start-clock* (get-internal-run-time)))

;###########################################################################################
;######################## FUNCAO PRINCIPAL RESOLVE-SAME-GAME  ##############################
(defun resolve-same-game (problema algoritmo)
  	(start-clock)
  	(setq n_balls (* (list-length (car problema)) (list-length  problema) ))
	(setq board-init (make-node :board  problema :n-balls n_balls ))
	(setq *best_result* board-init )
	(list_set_limits_size problema)
	(setf (node-return_list_path board-init) '())
	
    (cond        
    			((string-equal algoritmo "melhor.abordagem")
               	(time (procura (cria-problema board-init  (list #'lista-operadores) :objectivo? #'objectivo? :custo (always 1) 
               		:heuristica #'heuristica1) "a*" :espaco-em-arvore? T)))


    			((string-equal algoritmo "a*.melhor.heuristica")
               	(time (procura (cria-problema board-init  (list #'lista-operadores) :objectivo? #'objectivo? :custo (always 1) 
               		:heuristica #'heuristica1) "a*" :espaco-em-arvore? T)))

    			((string-equal algoritmo "a*.melhor.heuristica.alternativa")
               	(time (procura (cria-problema board-init  (list #'lista-operadores) :objectivo? #'objectivo? :custo (always 1) 
               		:heuristica #'heuristica2) "a*" :espaco-em-arvore? T)))

    			((string-equal algoritmo "sondagem.iterativa")
                 (time (sondagem_iterativa board-init)))

    			 ((string-equal algoritmo "abordagem.alternativa")
                	(time (ilds board-init 3)))





    			;IGNORAR condicoes seguintes, realizadas para testes      
                ((string-equal algoritmo "profundidade")
                (time (procura (cria-problema board-init (list #'lista-operadores) :objectivo? #'objectivo? :estado= #'equal) 
									"profundidade" :espaco-em-arvore? T)))
                
                
                ((string-equal algoritmo "largura")
                (time (procura (cria-problema board-init (list #'lista-operadores) :objectivo? #'objectivo? :estado= #'equal) 
									"largura" :espaco-em-arvore? T)))

                 ((string-equal algoritmo "a1*")
               	(time (procura (cria-problema board-init  (list #'lista-operadores) :objectivo? #'objectivo? :custo (always 1) 
               		:heuristica #'heuristica1) "a*" :espaco-em-arvore? T)))

                 (
                 	(string-equal algoritmo "a2*")
               	(time (procura (cria-problema board-init  (list #'lista-operadores) :objectivo? #'objectivo? :custo (always 1) 
               		:heuristica #'heuristica2) "a*" :espaco-em-arvore? T)))

                 ((string-equal algoritmo "a3*")
               	(time (procura (cria-problema board-init  (list #'lista-operadores) :objectivo? #'objectivo? :custo (always 1) 
               		:heuristica #'heuristica3) "a*" :espaco-em-arvore? T)))


                 ((string-equal algoritmo "ida1*")
               	(time (procura (cria-problema board-init  (list #'lista-operadores) :objectivo? #'objectivo? :custo (always 1) 
               		:heuristica #'heuristica1) "ida*" :espaco-em-arvore? T)))

                 ((string-equal algoritmo "ida2*")
               	(time (procura (cria-problema board-init  (list #'lista-operadores) :objectivo? #'objectivo? :custo (always 1) 
               		:heuristica #'heuristica2) "ida*" :espaco-em-arvore? T)))

                 ((string-equal algoritmo "ida3*")
               	(time (procura (cria-problema board-init  (list #'lista-operadores) :objectivo? #'objectivo? :custo (always 1) 
               		:heuristica #'heuristica3) "ida*" :espaco-em-arvore? T)))


                 ((string-equal algoritmo "profundidade-iterativa")
               	(time (procura (cria-problema board-init  (list #'lista-operadores) :objectivo? #'objectivo? :estado= #'equal)
               			 "profundidade-iterativa" :espaco-em-arvore? T)))

                ((string-equal algoritmo "si")
                 (time (sondagem_iterativa board-init)))


                ((string-equal algoritmo "lds_bbs")
                 (time (LDS_BBS board-init 5 13)))
				             

                ((string-equal algoritmo "ilds")
                	(time (ilds board-init 3)))

    )
    ;#############################RELATORIO ##############################
	;(format t "Resultados ~%")
	;(format t "Nos gerados: ~a ~%"   *nos_gerados*  )
	;(format t "Nos expandidos: ~a ~%" *nos_expandidos*)
	;(format t "Factor medio de ramificacao: ~a ~%" (mean_lista_ramos ))
	;(format t "Profundidade maxima: ~a ~%" (node-depth *estado_terminal*))
	;(format t "Pontuacao: ~a ~%" (node-points *estado_terminal*))
	;(format t "Tabuleiro: ~a ~%" (node-board *estado_terminal*))
	;(format t "Caminho: ~a ~%" (node-return_list_path *estado_terminal*))
	;######################################################################

    

    
	(return-from resolve-same-game (node-return_list_path *estado_terminal*))

 )

;###########################################################################################
;##################################### PARA TESTES  ########################################

;(trace lista-operadores)
;(trace objectivo?)
;(trace heuristica1)
;(trace heuristica2)
;(trace heuristica3)
;(setq board0 '((1 2 2 3 3) (2 2 2 1 3) (1 2 2 2 2) (1 1 1 1 1)))

; Tabuleiros do enunciado:
;(setq board1 '((2 1 3 2 3 3 2 3 3 3) (1 3 2 2 1 3 3 2 2 2) (1 3 1 3 2 2 2 1 2 1) (1 3 3 3 1 3 1 1 1 3)) )

;(setq board2 '((4 3 3 1 2 5 1 2 1 5) (2 4 4 4 1 5 2 4 1 2) (5 2 4 1 4 5 1 2 5 4) (1 3 1 4 2 5 2 5 4 5)) )

;(setq board3 '((3 3 3 2 1 2 3 1 3 1) (1 1 2 3 3 1 1 1 3 1) (3 3 1 2 1 1 3 2 1 1) (3 3 2 3 3 1 3 3 2 2) 
;			   (3 2 2 2 3 3 2 1 2 2) (3 1 2 2 2 2 1 2 1 3) (2 3 2 1 2 1 1 2 2 1) (2 2 3 1 1 1 3 2 1 3) 
;			   (1 3 3 1 1 2 3 1 3 1) (2 1 2 2 1 3 1 1 2 3) (2 1 1 3 3 3 1 2 3 1) (1 2 1 1 3 2 2 1 2 2) 
;			   (2 1 3 2 1 2 1 3 2 3) (1 2 1 3 1 2 2 3 2 3) (3 3 1 2 3 1 1 2 3 1)) )

;(setq board4 '((5 1 1 1 2 1 4 2 1 2) (5 5 5 4 1 2 2 1 4 5) (5 5 3 5 5 3 1 5 4 3) (3 3 3 2 4 3 1 3 5 1)
;			   (5 3 4 2 2 2 2 1 3 1) (1 1 5 3 1 1 2 5 5 5) (4 2 5 1 4 5 4 1 1 1) (5 3 5 3 3 3 3 4 2 2)
;			   (2 3 3 2 5 4 3 4 4 4) (3 5 5 2 2 5 2 2 4 2) (1 4 2 3 2 4 5 5 4 2) (4 1 3 2 4 3 4 4 3 1)
;			   (3 1 3 4 4 1 5 1 5 4) (1 3 1 5 2 4 4 3 3 2) (4 2 4 2 2 5 3 1 2 1)) )

;(setq boards (list  board1 board2 board3 ))

;(terpri)
;(write-line "Valores das soluções para diferentes estratégias:")

;(defparameter counter_x 1)
;(loop for board in boards do
;	(terpri) (format t "Board ~a: ~%" counter_x)
;	(write board) (terpri)(terpri)
;
;	(write-line "Profundidade:")
;	(resolve-same-game (copy-tree board) "profundidade")
;	(terpri)
;	(write-line "Profundidade Iterativa:")
;	;(resolve-same-game (copy-tree board) "profundidade-iterativa")
;	(terpri)
;	(write-line "Largura:")
;	;(resolve-same-game (copy-tree board) "largura")
;	(terpri)
;	(write-line "IDA*:")
;	;(resolve-same-game (copy-tree board) "ida*")
;	(terpri)
;	(write-line "A:")
;	;(resolve-same-game (copy-tree board) "a*")
;	(terpri)
;	(write-line "LDS BBS:")
;	;(resolve-same-game (copy-tree board) "lds_bbs")
;	(terpri)
;	(write-line "ILDS:")
;	;(resolve-same-game (copy-tree board) "ilds")
;	(terpri)
;
;	(incf counter_x)
;)

;(write-line "melhor.abordagem Board1:")
;(resolve-same-game (copy-tree board1) "melhor.abordagem")

;(write-line "a*.melhor.heuristica Board1:")
;(resolve-same-game (copy-tree board1) "a*.melhor.heuristica")

;(write-line "a*.melhor.heuristica.alternativa Board1:")
;(resolve-same-game (copy-tree board1) "a*.melhor.heuristica.alternativa")

;(write-line "sondagem.iterativa Board1:")
;(resolve-same-game (copy-tree board1) "sondagem.iterativa")

;(write-line "abordagem.alternativa Board4:")
;(resolve-same-game (copy-tree board4) "abordagem.alternativa")
