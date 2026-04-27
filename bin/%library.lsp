(vl-load-com)
;; (setq activeDocument(vla-get-ActiveDocument (vlax-get-acad-object))
;;       modelSpace (vla-get-ModelSpace activeDocument))

((lambda( / spath tcad )
   (setq spath(getvar "ACADPREFIX")  )
   (if(vl-string-search "ARES" spath)(setq command_for_alter "ARES")
     (if(vl-string-search "IJ" spath)(setq command_for_alter "IJCAD")
       ))
   ))

(defun dprint(msg)
  (if bool_codecheck (princ (strcat "\nDEBUG: " (vl-princ-to-string msg))))
  (progn)
  )

;; (setq activeDocument(vla-get-ActiveDocument (vlax-get-acad-object))
;;       modelSpace (vla-get-ModelSpace activeDocument))
;; (setq modelSpace (vla-get-ModelSpace(vla-get-ActiveDocument (vlax-get-acad-object))))

(defun C:cbascii_text ( / str strout str1 int1 bool_str )
  (setq strout "" str(getstring t) )
  (while(/= str "")
    (setq str1(substr str 1 1)int1(ascii str1))
    (if(and(> int1 32)(< int1 127))
        (setq str1(if bool_str str1(strcat " \"" str1))bool_str T)
      (setq str1(strcat(if bool_str "\" " " ")(itoa int1))bool_str nil))
    (setq strout(strcat strout str1)str(substr str 2))
    )
  (copy_to_clip (strcat "" strout(if bool_str "\") " " ")))
  (princ)
  )

(defun c:get_guidedrawing_line( / set_ent num entna vnam p1 p2 c str)
  (if(setq set_ent(ssget(list(cons 0 "LINE"))))
      (progn
        (setq num(sslength set_ent)str "")
        (while(>(setq num(1- num))-1)
          (setq entna(ssname set_ent num)
                vnam(vlax-ename->vla-object entna)
                p1(vlax-curve-getstartpoint vnam)
                p2(vlax-curve-getendpoint vnam)
                c(vla-get-color vnam)
                str(strcat str "(list(list "(substr(vl-princ-to-string p1)2)
                           "(list "(substr(vl-princ-to-string p2)2)(itoa c)")\n")
                )
          )
        (copy_to_clip str)
        
        ))
  )

;;<<<<library

(defun bg-dark-p( / key col rgb r g b brightness)
  (setq key(strcat "HKEY_CURRENT_USER\\" (vlax-product-key)
                   "\\Profiles\\" (getvar "CPROFILE")
                   "\\Drawing Window" ))
  
  (if(if vl-registry-read
         (vl-catch-all-error-p
          (setq col(vl-catch-all-apply 'vl-registry-read(list key "Background"))))
       T)
      nil
    (if col
        (progn
          (setq r(logand col 255)g(logand (lsh col -8) 255)b(logand (lsh col -16) 255)
                brightness(+ (* 0.299 r) (* 0.587 g) (* 0.114 b)) )
          (> brightness 128);;nilが暗い
          ))
    )
  )

(defun error_standard(msg)(gc)(princ msg));;普通のエラー

(defun set_laptimes( lst / int_type time_0 str time_1)
  (setq time_1(if(= command_for_alter "IJCAD")
                  (rem(* 100000(getvar "date"))1000.)
                (getvar "MILLISECS")))
  (cond
   ((or(=(car lst)0)(null lst))time_1)
   ((=(car lst)1)
    (if(setq time_0(cadr lst))T(setq time_0 0))
    (if(setq str(caddr lst))T(setq str ""))
    (princ(strcat str(rtos(*(- time_1 time_0)0.001)2 2)"sec" "\n"))
    )
   )
  )

(defun redraw_startcommand
    (real_texheight
     /  vs point_view_center vec_x_onview 
     ls_screen_size_yx real_delta_x real_delta_y th point_base real_dist_plane )

  (setq vec_view(unit_vector(getvar "VIEWDIR"))
        angle_viewhorizon(if(<(abs(1-(abs(caddr vec_view))))1e-6)(-(getvar "VIEWTWIST"))0)
        point_view_center(getvar "VIEWCTR")
        point_view_center(mapcar '(lambda(a b)(+ a(* 1000000.0 b)))
                                 point_view_center vec_view)
        bool_center(equal point_view_center point_view_center_p)
        )

  (if bool_center T
    (progn
      (setq real_delta_y(*(getvar "VIEWSIZE")0.5)
            real_texheight(* 0.1 real_delta_y real_texheight))
      (if(= angle_viewhorizon 0)
          (setq vec_x_onview(trans-x(list 1 0 0)vec_view(list 0 0 1))
                vec_y_onview(trans-x(list 0 1 0)vec_view(list 0 0 1)))
        (setq vec_x_onview(list(cos angle_viewhorizon)(sin angle_viewhorizon)0)
              vec_y_onview(list(-(sin angle_viewhorizon))(cos angle_viewhorizon)0))
        )
      (setq ls_screen_size_yx(getvar "SCREENSIZE")
            real_delta_x(/(* real_delta_y(car ls_screen_size_yx))
                          (cadr ls_screen_size_yx))
            point_base
            (mapcar '(lambda(a b c)
                       (+ a(*(- real_texheight real_delta_x)b)
                          (*(- real_delta_y(* 2 real_texheight))c)))
                    point_view_center vec_x_onview vec_y_onview)
            real_dist_plane(apply '+(mapcar '* point_base vec_view))
            )
      (setq array_vla_normal(vlax-make-safearray vlax-vbDouble '(0 . 2)))
      (vlax-safearray-fill array_vla_normal vec_view)
      
      (setq array_vla_point(vlax-make-safearray vlax-vbDouble '(0 . 2)))
      (vlax-safearray-fill array_vla_point point_base)
      (vla-put-normal vnam_back array_vla_normal)
      (vla-put-InsertionPoint vnam_back array_vla_point)
      (vla-put-Height vnam_back (* real_texheight 40))
      (vla-put-rotation vnam_back angle_viewhorizon)

      (setq point_base(mapcar '(lambda(a b)(+ a(* real_texheight b)))point_base vec_view))
      (setq y -0.5 x 0 y_delta 2.5 x_delta 20.0)
      (setq ls_bound_prev
            (mapcar '(lambda(vnam vnam_icon ls_columnrow
                                  / p1 p2 str_type vnam real_imageheight real_scale xcoord ycoord
                                  pb1 pb2 x0 x1 y0 y1)
                       (setq xcoord(+ x(car ls_columnrow))
                             ycoord(- y(cadr  ls_columnrow))
                             p2(mapcar '(lambda(a b c)
                                          (+ a (* xcoord real_texheight b)
                                             (* ycoord real_texheight c)))
                                       point_base vec_x_onview vec_y_onview)
                             p1(mapcar '(lambda(a b c)
                                          (+ a (* 3.0 real_texheight b)
                                             (* 0.5 real_texheight c)))
                                       p2 vec_x_onview vec_y_onview)
                             array_vla_point(vlax-make-safearray vlax-vbDouble '(0 . 2)))
                       (vlax-safearray-fill array_vla_point p1)
                       (mapcar '(lambda(a b)(a vnam b))
                               (list vla-put-normal vla-put-InsertionPoint
                                     vla-put-Height vla-put-rotation)
                               (list(vlax-3d-point 0 0 1)array_vla_point
                                    real_texheight angle_viewhorizon)
                               )
                       (vla-getBoundingBox vnam 'pb1 'pb2)
                       (mapcar '(lambda(a b)(a vnam b))
                               (list vla-put-normal vla-put-InsertionPoint vla-put-rotation)
                               (list array_vla_normal array_vla_point angle_viewhorizon)
                               )
                       (setq pb1(vlax-safearray->list pb1)
                             pb2(vlax-safearray->list pb2)
                             x1(-(car pb2)(car pb1))y1(-(cadr pb2)(cadr pb1)))
                       ;; (vla-put-AttachmentPoint vnam 7)
                       
                       (setq pb1(trans-x p1(list 0 0 1)vec_view)
                             x0(car pb1)y0(cadr pb1)x1(+ x0 x1)y1(+ y0 y1)
                             )
                       ;; (if(< x1 x0)((lambda (a b)(setq x0 b x1 a))x0 x1))
                       ;; (if(< y1 y0)((lambda (a b)(setq y0 b y1 a))y0 y1))

                       (vlax-safearray-fill array_vla_point p2)
                       (if vnam_icon
                           (progn
                             (setq real_scale(* 4 real_texheight real_height_icon 0.0001))
                             (mapcar '(lambda(a b) (a vnam_icon b))
                                     (list vla-put-normal vla-put-InsertionPoint vla-put-Rotation
                                           vla-put-XEffectiveScaleFactor vla-put-XScaleFactor
                                           vla-put-YEffectiveScaleFactor vla-put-YScaleFactor
                                           vla-put-ZEffectiveScaleFactor vla-put-ZScaleFactor
                                           )
                                     (list array_vla_normal array_vla_point angle_viewhorizon
                                           real_scale real_scale
                                           real_scale real_scale
                                           real_scale real_scale
                                           )
                                     )
                             ;; (vla-GetBoundingBox vnam_icon 'pb1 'pb2)
                             ;; (setq pb1(trans-x pb1(list 0 0 1)vec_view)
                             ;;       pb2(trans-x pb2(list 0 0 1)vec_view)
                             ;;       x0(car pb1)y0(cadr pb1)x1(car pb2)y1(cadr pb2)
                             ;;       )
                             
                             
                             ))

                       (list x0 x1 y0 y1)
                       )
                    ls_vnam_button ls_vnam_icon
                    ls_columnrow_prev
                    )
            )

      (mapcar '(lambda(vnam ls_columnrow
                            / p1 p2 str_type vnam real_imageheight real_scale xcoord ycoord)
                 (setq xcoord(+ x(car ls_columnrow))
                       ycoord(- y(cadr  ls_columnrow))
                       p1(mapcar '(lambda(a b c)
                                    (+ a (* xcoord real_texheight b)
                                       (* ycoord real_texheight c)))
                                 point_base vec_x_onview vec_y_onview)
                       array_vla_point(vlax-make-safearray vlax-vbDouble '(0 . 2)))
                 (vlax-safearray-fill array_vla_point p1)
                 (mapcar '(lambda(a b)(a vnam b))
                         (list vla-put-normal vla-put-InsertionPoint
                               vla-put-Height vla-put-rotation)
                         (list array_vla_normal array_vla_point
                               real_texheight angle_viewhorizon)
                         )
                 )
              ls_vnam_category ls_columnrow_category
              )
      ))


  
  point_view_center
  )

;; 行入れ替え関数
(defun swap-rows (lst i j / ri rj result idx)
  (setq ri (nth i lst) rj (nth j lst) result '() idx 0)
  (foreach
   row lst
   (setq result(cons(cond
                     ((= idx i) rj)
                     ((= idx j) ri)
                     (t row)
                     )
                    result)
         idx (1+ idx)))
  (reverse result)
  )

(defun gauss-jordan-solve (mat / n i j k pivot factor tmp zero-row sol newrow idx bool_solve)
  (setq n (- (length (car mat)) 1)) ; 変数の数
  
  ;; 前進消去＋完全消去
  (setq i -1)
  (while (<(setq i (1+ i))n)
    ;; ピボット選択（絶対値最大）
    (setq pivot i k i )
    (while (<(setq k (1+ k))n)
      (if (> (abs (nth i (nth k mat))) (abs (nth i (nth pivot mat))))
          (setq pivot k))
      )
    ;; 行入れ替え
    (if (/= pivot i) (setq mat (swap-rows mat i pivot)))
    ;; ピボットゼロ判定
    (if (equal (nth i (nth i mat)) 0.0 1e-12)
        (progn
          ;; 下に非ゼロピボットがあれば交換、なければ次列に進む
          (setq k i  zero-row t)
          (while (and (<(setq k (1+ k))n) zero-row)
            (if (not (equal (nth i (nth k mat)) 0.0 1e-12))
                (setq mat (swap-rows mat i k) zero-row nil))
            )
          ;; 列全体ゼロなら無限解か解なし判定
          (if(if zero-row (equal (nth i (nth i mat)) 0.0 1e-12))
              (progn (alert "Infinite or No Solutions") (quit) ))
          ))
    
    ;; 他行をゼロ化
    (setq j -1)
    (while (<(setq j (1+ j))n)
      (if (/= j i)
          (progn
            (setq factor (/ (nth i (nth j mat)) (nth i (nth i mat)))
                  tmp((lambda( / tmp idx)
                        (setq idx 0)
                        (foreach val (nth j mat)
                                 (setq tmp (cons(- val (* factor (nth idx (nth i mat))))tmp)
                                       idx (1+ idx)) )
                        (reverse tmp)))
                  )
            ;; mat の j 行更新
            (setq mat(append(mapcar '(lambda (r) (if (equal r (nth j mat)) tmp r)) mat)) )
            ))
      )
    ;; ピボットを1に正規化
    (setq tmp((lambda( / tmp idx)
                (setq idx 0)
                (foreach val (nth i mat)
                         (setq tmp (cons(/ val (nth i (nth i mat)))tmp)
                               idx (1+ idx)))
                (reverse tmp)))
          ;; mat の i 行更新
          mat (append (mapcar '(lambda (r)(if (equal r (nth i mat)) tmp r)) mat)))
    )

  ;; 解を取得
  (setq sol '() i -1)
  (while (<(setq i (1+ i))n)
    (setq sol (cons(nth n (nth i mat))sol))
    )
  (reverse sol)
  )

(defun xmemory_dict(str / dict_obj dicts)
  (setq dicts (vla-get-Dictionaries(vla-get-ActiveDocument (vlax-get-acad-object))))
  (if (vl-catch-all-error-p(vl-catch-all-apply 'vla-Item (list dicts str)))
      (vla-Add dicts str) (vla-Item dicts str))
  )

(defun error-preview_command(msg)
  (if ls_entna_killobj
      (mapcar '(lambda(a)
                 (if a(if(=(type a)'ENAME)
                          (vla-delete(vlax-ename->vla-object a))
                        (vla-delete a))))
              ls_entna_killobj) )
  (setq ls_entna_killobj nil);;load_dcl nil open_file nil ls_dcl_delete nil)
  
  (defun *error*(msg)(princ msg))
  (princ msg)
  )

(defun input_style_drawing
    ( icon_dat ls_stylenamed ls_dimstyle ls_mleadstyle ls_insertdwg ls_str_command_prev
               ls_str_buttonwrite_prev ls_text_category_prev ls_columnrow_prev
               ls_str_help real_textheight
               / vec_view vs point_view_center vec_x_onview vec_y_onview
               ls_screen_size_yx real_delta_x real_delta_y th point_base real_dist_plane
               ls_bound_prev vec_view)
  
  (setq *error* error-preview_command ls_entna_killobj nil)

  ;;アイコン,スタイル
  (setq ls_iconexist(list))
  (if(setq open_file(open(strcat sp_path icon_dat)"r"))
      (progn
        (while(setq str(read-line open_file))
          (setq ls_iconexist(cons str ls_iconexist)))
        (close open_file)
        )
    (setq ls_iconexist(list ""))
    )

  (mapcar '(lambda(str bool_ins / inspt vnam str_path)
             (setq str_path(strcat sp_path str ".dwg"))
             (if bool_ins T
               (if(null(findfile str_path))T
                 (progn
                   (setq inspt (vlax-3d-point 0 0 0)
                         vnam(vla-InsertBlock(vla-get-ModelSpace(vla-get-ActiveDocument (vlax-get-acad-object)))
                                             inspt str_path 1.0 1.0 1.0 0.0))

                   (vla-delete vnam)
                   (vlax-release-object vnam)
                   (setq blkcollection(vla-get-Blocks (vla-get-ActiveDocument(vlax-get-acad-object))))
                   (vla-Delete (vla-Item blkcollection str))
                   (vlax-release-object blkcollection)
                   ))
               )
             )
          ls_insertdwg
          (list(apply 'and(mapcar '(lambda(a)(tblsearch "block" a))ls_iconexist) )
               (and
                (apply 'and(mapcar '(lambda(a)(tblsearch "style" a))ls_stylenamed ))
                (apply 'and(mapcar '(lambda(a)(tblsearch "dimstyle" a))ls_dimstyle ))
                ((lambda( / dict)
                   (setq dict(dictsearch (namedobjdict) "ACAD_MLEADERSTYLE"))
                   (apply 'and(mapcar '(lambda(a)(vl-position(cons 3 a)dict))ls_mleadstyle))))
                )
               )
          )
  (gc)
  )

;;マスク付きテキストブロック
(defun makeguidetext(str_textstyle)
  
  (setq str_bname "G1T2A2E3P3TEXT")
  (if(vl-catch-all-error-p
      (setq vnam_blkdef
            (vl-catch-all-apply
             'vla-Item(list(vla-get-Blocks(vla-get-ActiveDocument (vlax-get-acad-object)))
                           str_bname))))
      (progn
        (setq vnam_blkdef
              (vla-Add(vla-get-blocks(vla-get-ActiveDocument (vlax-get-acad-object)))
                      (vlax-3d-point 0 0 0)str_bname))

        (setq ls_vnam
              (mapcar '(lambda(i / entna)
                         (setq entna
                               (if(= i 0)
                                   (entmakex(list(cons 0 "WIPEOUT")
                                                 (cons 100 "AcDbEntity")(cons 100 "AcDbWipeout")
                                                 (list 10 0 0 0)(list 11 1. 0. 0.)(list 12 0. -1. 0.)
                                                 ))
                                 (entmakex
                                  (list(cons 0 "MTEXT")(cons 100 "AcDbEntity")(cons 100 "AcDbMText")
                                       (cons 7 str_textstyle)(list 10 0 0 0)(cons 40 1)
                                       (cons 1 ".")
                                       (cons 71 1)
                                       ))
                                 )
                               )
                         (vlax-ename->vla-object entna)
                         )
                      (list 0 1))
              )
        
        (vla-copyobjects(vla-get-ActiveDocument(vlax-get-acad-object))
                        (vlax-make-variant
                         (vlax-safearray-fill
                          (vlax-make-safearray
                           vlax-vbObject
                           (cons 0 (1-(length ls_vnam)))
                           )
                          ls_vnam
                          )
                         )
                        block)
        
        (vlax-release-object blocktable)
        (vlax-release-object block)


        
        )
    
    ;;(vlax-for obj vnam_blkdef(vla-delete obj));;あるなら消さない

    )
  
  
  
  
  )



(defun preview_command_button
    ( icon_dat ls_stylenamed ls_dimstyle ls_mleadstyle ls_insertdwg ls_str_command_prev
               ls_str_buttonwrite_prev ls_text_category_prev ls_columnrow_prev
               ls_str_help real_textheight
               / vec_view vs point_view_center vec_x_onview vec_y_onview
               ls_screen_size_yx real_delta_x real_delta_y th point_base real_dist_plane
               ls_bound_prev vec_view)
  
  (setq *error* error-preview_command ls_entna_killobj nil)

  ;;アイコン,スタイル
  (setq ls_iconexist(list))
  (if(setq open_file(open(strcat sp_path icon_dat)"r"))
      (progn
        (while(setq str(read-line open_file))
          (setq ls_iconexist(cons str ls_iconexist)))
        (close open_file)
        )
    (setq ls_iconexist(list ""))
    )

  (mapcar '(lambda(str bool_ins / inspt vnam str_path)
             (setq str_path(strcat sp_path str ".dwg"))
             (if bool_ins T
               (if(null(findfile str_path))T
                 (progn
                   (setq inspt (vlax-3d-point 0 0 0)
                         vnam(vla-InsertBlock(vla-get-ModelSpace(vla-get-ActiveDocument (vlax-get-acad-object)))
                                             inspt str_path 1.0 1.0 1.0 0.0))

                   (vla-delete vnam)
                   (vlax-release-object vnam)
                   (setq blkcollection(vla-get-Blocks (vla-get-ActiveDocument(vlax-get-acad-object))))
                   (vla-Delete (vla-Item blkcollection str))
                   (vlax-release-object blkcollection)
                   ))
               )
             )
          ls_insertdwg
          (list(apply 'and(mapcar '(lambda(a)(tblsearch "block" a))ls_iconexist) )
               (and
                (apply 'and(mapcar '(lambda(a)(tblsearch "style" a))ls_stylenamed ))
                (apply 'and(mapcar '(lambda(a)(tblsearch "dimstyle" a))ls_dimstyle ))
                ((lambda( / dict)
                   (setq dict(dictsearch (namedobjdict) "ACAD_MLEADERSTYLE"))
                   (apply 'and(mapcar '(lambda(a)(vl-position(cons 3 a)dict))ls_mleadstyle))))
                )
               )
          )
  (gc)
  ;; (princ(list
  ;;        (apply 'and(mapcar '(lambda(a)(tblsearch "style" a))ls_stylenamed ))
  ;;        (apply 'and(mapcar '(lambda(a)(tblsearch "dimstyle" a))ls_dimstyle ))
  ;;        ((lambda( / dict)
  ;;           (setq dict(dictsearch (namedobjdict) "ACAD_MLEADERSTYLE"))
  ;;           (apply 'and(mapcar '(lambda(a)(vl-position(cons 3 a)dict))ls_mleadstyle))))
  ;;        ))
  

  (mapcar '(lambda(a b / str)
             (set b(if(tblsearch "style" a)a(getvar "TEXTSTYLE"))) )
          ls_stylenamed '(str_textstyle-button)
          )

  
  (setq entna_back(entmakex
                   (list(cons 0 "MTEXT")(cons 100 "AcDbEntity")(cons 100 "AcDbMText")
                        (cons 7 str_textstyle-button)
                        (cons 40 100)(list 10 0 0 0)(cons 40 1)
                        (cons 1 "                    .")
                        (cons 71 1)
                        (cons 90 1)(cons 63 177)(cons 421 986966)(cons 45 2))
                   )
        vnam_back(vlax-ename->vla-object entna_back)
        ls_entna_killobj(cons vnam_back ls_entna_killobj)
        )

  (setq ii -1 real_height_icon nil 
        ls_vnam_icon
        (mapcar
         '(lambda
            ( str_command str_button
                          / str_type real_ImageHeight str_path entna_icon vnam_icon str_icon)
            (setq str_icon(strcat "icon_" str_command)
                  ii(1+ ii))
            
            (if(tblsearch "block" str_icon)
                (progn
                  (setq entna_icon(entmakex(list(cons 0 "INSERT")(cons 2 str_icon)(list 10 0 0 0)))
                        vnam_icon(vlax-ename->vla-object entna_icon)
                        ls_entna_killobj(cons vnam_icon ls_entna_killobj)
                        )
                  (if real_height_icon T
                    (setq real_height_icon
                          ((lambda( / p_min  p_max )
                             (vla-getboundingbox vnam_icon 'p_min 'p_max)
                             (setq p_min(vlax-safearray->list p_min)
                                   p_max(vlax-safearray->list p_max))
                             (-(cadr p_max)(cadr p_min))
                             ))
                          )
                    )
                  ))
            
            (setq entna(entmakex
                        (list(cons 0 "MTEXT")(cons 100 "AcDbEntity")(cons 100 "AcDbMText")
                             (cons 7 str_textstyle-button)(cons 62 255)
                             (cons 40 100)(list 10 0 0 0)(cons 40 1)
                             (cons 1(strcat (itoa ii)"." str_button))
                             (cons 71 7)
                             (cons 90 16)(cons 63 256)(cons 45 1.2)
                             )
                        )
                  vnam(vlax-ename->vla-object entna)
                  ls_entna_killobj(cons vnam ls_entna_killobj)
                  )

            (list entna vnam vnam_icon)
            )
         ls_str_command_prev
         ls_str_buttonwrite_prev
         )
        ls_entna_button(mapcar 'car ls_vnam_icon)
        ls_vnam_button(mapcar 'cadr ls_vnam_icon)
        ls_vnam_icon(mapcar 'caddr ls_vnam_icon)
        )

  (setq ls_vnam_category
        (mapcar
         '(lambda
            ( str_text
              / str_type real_ImageHeight str_path entna_icon vnam_icon str_icon)
            (setq entna(entmakex
                        (list(cons 0 "MTEXT")(cons 100 "AcDbEntity")(cons 100 "AcDbMText")
                             (cons 7 str_textstyle-button)(cons 62 255)
                             (cons 40 100)(list 10 0 0 0)(cons 40 1)
                             (cons 1 str_text)
                             (cons 71 7)
                             ;;(cons 90 16)(cons 63 256)(cons 45 1.2)
                             )
                        )
                  vnam(vlax-ename->vla-object entna)
                  ls_entna_killobj(cons vnam ls_entna_killobj)
                  )
            vnam
            )
         (mapcar 'cadr ls_text_category_prev)

         )
        )
  (setq ls_columnrow_category(mapcar 'car ls_text_category_prev))
  (setq point_view_center_p nil)

  (setq point_view_center_p(redraw_startcommand real_textheight))

  (setq str_input ""
        ls_chrint(list 46 48 49 50 51 52 53 54 55 56 57)
        str_message(mix_strasc(list 91 38917 30446 12434 12463 12522 12483 12463 12414 12383 12399 25968 20516 "->Enter" 93 12289 91 25968 20516 28961 12375 "->Enter" 101 114 12391 30452 21069 12395 20351 29992 12375 12383 12467 12510 12531 12489 12434 20877 23455 34892 93)))
  ;;[項目をクリックまたは数値->Enter]、[数値無し->Enterで直前に使用したコマンドを再実行]
  
    
  ;; (princ str_message)
  ;; (setq entna_button(button_search_grread "MTEXT,INSERT"(list)vec_y_onview nil ))
  ;;選択対象、番号対応リスト、yベクトル、回転する
  
  (setq entna_button nil str_input "" str_command nil)
  (while(null str_command)
    (progn
      (setq ls_grread(grread t 15 0) int_grread(car  ls_grread) )
      (cond
       ((if(= int_grread 2)(/=(setq ascii_grread(cadr ls_grread))13))
        (cond
         ((vl-position ascii_grread ls_chrint)
          (princ(chr ascii_grread))
          (setq str_input(strcat str_input(chr ascii_grread)))
          )
         ((= ascii_grread 8);;BS
          (princ(strcat "\n"(setq str_input(substr str_input 1(1-(strlen str_input))))))
          )
         )
        )
       ((= int_grread 3)
        (princ "\n")
        (setq str_input "")
        (setq p_grread(cadr ls_grread)
              p_grread(trans-x p_grread(list 0 0 1)vec_view)
              ls_p_grread(list(car p_grread)(car p_grread)(cadr p_grread)(cadr p_grread))
              ls_bool_bound(mapcar '(lambda(a / )
                                      (apply 'and(mapcar '(lambda(f b c)(f b c))
                                                         (list > < > <)ls_p_grread a)))
                                   ls_bound_prev)
              
              int_num(vl-position T ls_bool_bound)
              )
        
        (if int_num
            (setq str_command(nth int_num ls_str_command_prev)
                  int_preselectnumber(if str_command int_num))
          )
        
        )
       ((and(= int_grread 2)(=(cadr ls_grread) 13))
        (if(and(/= str_input "0")(=(atoi str_input)0))
            (if int_preselectnumber
                (setq str_command(nth int_preselectnumber ls_str_command_prev)))
          (setq str_command(nth(atoi str_input)ls_str_command_prev)
                int_preselectnumber(if str_command(atoi str_input))
                )
          )
        (princ "\n")
        (setq str_input "")
        )
       )
      
      (cond
       ((null str_command) )
       (help_command_x
        (help_command_open str_command(if int_num(nth int_num ls_str_help)))
        (setq str_command nil)
        )
       ((= str_command "help")
        (help_command_set)
        (setq str_command nil)
        )
       )
      )
    (setq point_view_center_p(redraw_startcommand real_textheight))
    )
  (*error* "")
  str_command

  )

;;スタイル検索ブロックも
(defun search_drawstyle( lst / sym )
  (mapcar
   '(lambda(str / sym)
      (setq sym(car lst)lst(cdr lst))
      (if sym
          (set sym((lambda( / ls_sty ls_entry)
                     (setq ls_entry (tblnext str T))
                     (while ls_entry
                       (setq ls_sty(cons (cdr (assoc 2 ls_entry)) ls_sty)
                             ls_entry (tblnext str))
                       )
                     ls_sty ))))
      ) (list "STYLE" "DIMSTYLE"))
  
  (if(setq sym(car lst))
      (set sym(vl-remove nil(mapcar '(lambda(a)(if(=(car a)3)(cdr a)))
                                    (dictsearch(namedobjdict)"ACAD_MLEADERSTYLE")))) )

  (if(setq sym(cadr lst))
      (set sym((lambda( / lst blk str)
                 (vlax-for blk(vla-get-blocks (vla-get-activedocument (vlax-get-acad-object)))
                           (setq lst(cons(vla-get-name blk)lst)) )
                 lst))) )
  )


(defun search_sty-lay-blo( lst / sym )
  
  (mapcar
   '(lambda(str / sym)
      (setq sym(cdr(assoc str lst)))
      (if sym
          (set sym((lambda( / ls_sty ls_entry)
                     (setq ls_entry (tblnext str T))
                     (while ls_entry
                       (setq ls_sty(cons (cdr (assoc 2 ls_entry)) ls_sty)
                             ls_entry (tblnext str))
                       )
                     (acad_strlsort ls_sty) ))))
      ) (list "STYLE" "DIMSTYLE" "LAYER"))
  
  (if(setq sym(cdr(assoc "LEADER" lst)))
      (set sym(acad_strlsort
               (vl-remove nil(mapcar '(lambda(a)(if(=(car a)3)(cdr a)))
                                     (dictsearch(namedobjdict)"ACAD_MLEADERSTYLE"))))) )
  
  (if(setq sym(cdr(assoc "BLOCK" lst)))
      (set sym((lambda( / lst blk str)
                 (vlax-for blk(vla-get-blocks (vla-get-activedocument (vlax-get-acad-object)))
                           (setq str(vla-get-name blk))
                           (if(=(ascii str)42)T(setq lst(cons str lst)))
                           )
                 
                 (acad_strlsort lst)))) )
  )



(defun get_visual_color(vnam / i)
  (if(=(setq i(vla-get-color vnam)) 256)
      (setq i(vla-get-color
              (vla-item(vla-get-layers(vla-get-activedocument(vlax-get-acad-object)))
                       (vla-get-layer vnam)))))
  i)

(defun xvla-lwpoly( ls_p ls_val / array_p vnam)
  (setq array_p(vlax-make-safearray vlax-vbDouble(cons 0 (1-(length ls_p)))))
  (vlax-safearray-fill array_p ls_p)
  (setq vnam(vla-addlightweightpolyline
             (vla-get-modelspace(vla-get-activedocument(vlax-get-acad-object)))array_p))
  (mapcar '(lambda(func val)(func vnam val))
          (list vla-put-layer vla-put-color vla-put-Closed)
          ls_val)
  vnam
  )


(defun make_2pdimension
    ( entna_dim
      lst / p1 p2 angle_dire height_ext vec_normal dist_normal vec_y
      str_lay str_dimstyle str_dim int_incol
      dist_plane p_height vla_obj vmat_DataType vmat_Data str_type vec_dim)
  (mapcar 'set
          '(p1 p2 vec_normal dist_normal vec_y angle_dire height_ext str_dim str_dimstyle str_lay)
          lst)
  ;;height_ext : real 高さ list world座標
  (if(=(type height_ext)'LIST) (setq p_height height_ext)
    (setq p_height(if(= height_ext 0)p1(mapcar '(lambda(a b)(+ a(* b height_ext)))p1 vec_y)) ) )
  (if str_dimstyle T(setq str_dimstyle(getvar "DIMSTYLE")))
  (if str_lay T(setq str_lay(getvar "CLAYER")))
  (if dist_normal T(setq dist_normal(apply '+(mapcar '* vec_normal p1))))

  (if(if entna_dim(entget entna_dim))
      (progn
        (entmod(mapcar '(lambda( dls_code / int_code )
                          (setq int_code(car dls_code))
                          (cond
                           ((= int_code 8)(cons 8 str_lay))
                           ((= int_code 10)(cons 10 p_height))
                           ((= int_code 11)(list 11 0 0 dist_normal))
                           ((= int_code 210)(cons 210 vec_normal))
                           ((= int_code 13)(cons 13 p1))
                           ((= int_code 14)(cons 14 p2))
                           ((= int_code 50)(cons 50 angle_dire))
                           ((= int_code 1)(cons 1 str_dim))
                           (T dls_code)
                           ))
                       (entget entna_dim)))
        ;;(vla-put-StyleName(vlax-ename->vla-object(car(entsel)))(getvar "DIMSTYLE"))
        ;;(vla-put-color(vlax-ename->vla-object(car(entsel)))(getvar "CECOLOR"));;RGB不可
        ;;(vla-put-Layer(vlax-ename->vla-object(car(entsel)))(getvar "CLAYER"))
        )
    (setq entna_dim
          (entmakex(list(cons 0 "DIMENSION")(cons 100 "AcDbEntity")
                        (cons 100 "AcDbDimension")(cons 8 str_lay)
                        (cons 10 p_height)(list 11 0 0 dist_normal)
                        (list 12 0 0 0)(cons 70 32)(cons 1 str_dim)(cons 71 5)
                        (cons 72 1)(cons 210 vec_normal)(cons 3 str_dimstyle)
                        (cons 100 "AcDbAlignedDimension")
                        (cons 13 p1)(cons 14 p2)(cons 50 angle_dire)
                        (cons 100 "AcDbRotatedDimension")
                        ))
          )
    )
  entna_dim
  )


(defun make_arcdimension
    (entna_dim
     lst / p_cent p_sta p_end p_way r vec_normal dist_normal str_lay str_dimstyle str_dim int_incol
     dist_plane )

  (mapcar 'set '(p_cent p_sta p_end p_way r vec_normal dist_normal str_dim str_dimstyle)lst)
  (if dist_normal T(setq dist_normal(apply '+(mapcar '* vec_normal p_cent))))

  (if(if entna_dim(entget entna_dim))
      ;; (entmod(list(cons -1 tm)(cons 10 p_way)(list 11 0 0 dist_plane)(cons 210 vec_normal)
      ;;   (cons 13 p_sta)(cons 14 p_end)(cons 15 p_cent)(cons 16 p_way)(cons 1 str_dim)))
      (entmod(mapcar '(lambda(dls_code / int_code)
                        (setq int_code(car dls_code))
                        (cond
                         ((= int_code  10)(cons  10 p_way))
                         ((= int_code  11)(list  11 0 0 dist_normal))
                         ((= int_code 210)(cons 210 vec_normal))
                         ((= int_code  13)(cons  13 p_sta))
                         ((= int_code  14)(cons  14 p_end))
                         ((= int_code  15)(cons  15 p_cent))
                         ((= int_code  16)(cons  16 p_way))
                         ((= int_code   1)(cons   1 str_dim))
                         (T dls_code)
                         ))
                     (entget entna_dim)))
    
    (setq entna_dim
          (entmakex(list(cons 0 "DIMENSION")(cons 100 "AcDbEntity")
                        (cons 100 "AcDbDimension");;(cons 8 str_lay)
                        (cons 3 str_dimstyle)
                        ;;(cons 62 int_incol)
                        (cons 10 p_way)(list 11 0 0 dist_normal)
                        (cons 70 37)(cons 1 str_dim)(cons 210 vec_normal)
                        (cons 100 "AcDb3PointAngularDimension")
                        (cons 13 p_sta)(cons 14 p_end)(cons 15 p_cent)
                        (cons 16 p_way)))
          )
    )
  entna_dim
  )


;; (defun make_OrdinateDimension
;;     (entna_dim
;;      lst / p1 p2 p_end p_way r vec_normal dist_normal str_lay str_dimstyle str_dim int_incol
;;      dist_plane )

;;   (mapcar 'set '(p1 p2 vec_normal dist_normal str_dim str_dimstyle str_lay)lst)

;;   (if(if entna_dim(entget entna_dim))
;;       (entmod(mapcar '(lambda(dls_code / int_code)
;;                         (setq int_code(car dls_code))
;;                         (cond
;;                          ((= int_code 8)(cons 8 str_lay))
;;                          ((= int_code 11)(list 11 0 0 dist_normal))
;;                          ((= int_code 210)(cons 210 vec_normal))
;;                          ((= int_code 13)(cons 13 p1))
;;                          ((= int_code 14)(cons 14 p2))
;;                          ;;((= int_code 50)(cons 50 angle_dire))
;;                          ((= int_code 1)(cons 1 str_dim))
;;                          (T dls_code)
;;                          ))
;;                      (entget entna_dim)))
;;     (setq entna_dim
;;           (entmakex(list(cons 0 "DIMENSION")(cons 100 "AcDbEntity")
;;                         (cons 100 "AcDbDimension")(cons 8 str_lay)
;;                         (list 11 0 0 dist_normal)
;;                         (cons 70 38) (cons 1 str_dim) (cons 210 vec_normal)
;;                         (cons 3 str_dimstyle)(cons 100 "AcDbOrdinateDimension")
;;                         (cons 13 p1)(cons 14 p2)
;;                         (cons 50 0.)
;;                         ))
;;           )
;;     )
;;   entna_dim
;;   )



(defun dimheight_transvers
    ( / entna ls_gcode bool_loop str_type ls_p10 p10 p13 vec_0 vec_1 vec_2 height_dim)
  (setq bool_loop T)
  (princ(mix_strasc(list 10 23544 27861 12434 36984 25246 40 69 115 99 12391 32066 20102 41)))
  ;;寸法を選択(Escで終了)
  (while bool_loop
    (if(progn
         (if(setq entna(car(entsel)))
             (progn
               (setq ls_gcode(entget entna)
                     str_type(cdr(assoc 0 ls_gcode)))
               (= str_type "DIMENSION")
               )))
        (progn
          (setq ls_p10(assoc 10 ls_gcode) p10(cdr ls_p10)p13(cdr(assoc 13 ls_gcode))
                vec_1(unit_vector(mapcar '-(cdr(assoc 14 ls_gcode))p13))
                vec_2(cross_product(cdr(assoc 210 ls_gcode))vec_1)
                height_dim(apply '+(mapcar '(lambda(a b c)(*(- a b)c))p10 p13 vec_2))
                p10(mapcar '(lambda(a b)(+ a(* -2. height_dim b)))p10 vec_2)
                )
          (entmod(subst(cons 10 p10)ls_p10 ls_gcode))
          ))
    )
  (progn)
  )


(defun error-preview_command_dcl(msg)
  (setq ls_entna_killobj nil);;load_dcl nil open_file nil ls_dcl_delete nil)
  
  (defun *error*(msg)(princ msg))
  (princ msg)
  )


(defun preview_command_dclbutton
    ( icon_dat ls_stylenamed ls_dimstyle ls_mleadstyle ls_insertdwg ls_str_command_prev
               ls_str_buttonwrite_prev ls_text_category_prev ls_columnrow_prev
               ls_str_help real_textheight
               / vec_view vs point_view_center vec_x_onview vec_y_onview
               ls_screen_size_yx real_delta_x real_delta_y th point_base real_dist_plane
               ls_bound_prev vec_view)
  
  (setq *error* error-preview_command_dcl ls_entna_killobj nil)

  ;;アイコン,スタイル
  ;; (setq ls_iconexist(list))
  ;; (if(setq open_file(open(strcat sp_path icon_dat)"r"))
  ;;     (progn
  ;;       (while(setq str(read-line open_file))
  ;;         (setq ls_iconexist(cons str ls_iconexist)))
  ;;       (close open_file)
  ;;       )
  ;;   (setq ls_iconexist(list ""))
  ;;   )

  (setq ls_iconexist nil)
  
  (mapcar '(lambda(str bool_ins / inspt vnam str_path)
             (setq str_path(strcat sp_path str ".dwg"))
             (if bool_ins T
               (if(null(findfile str_path))T
                 (progn
                   (setq inspt (vlax-3d-point 0 0 0)
                         vnam(vla-InsertBlock(vla-get-ModelSpace(vla-get-ActiveDocument (vlax-get-acad-object)))
                                             inspt str_path 1.0 1.0 1.0 0.0))

                   (vla-delete vnam)
                   (vlax-release-object vnam)
                   (setq blkcollection(vla-get-Blocks (vla-get-ActiveDocument(vlax-get-acad-object))))
                   (vla-Delete (vla-Item blkcollection str))
                   (vlax-release-object blkcollection)
                   ))
               )
             )
          ls_insertdwg
          (list(apply 'and(mapcar '(lambda(a)(tblsearch "block" a))ls_iconexist) )
               (and
                (apply 'and(mapcar '(lambda(a)(tblsearch "style" a))ls_stylenamed ))
                (apply 'and(mapcar '(lambda(a)(tblsearch "dimstyle" a))ls_dimstyle ))
                ((lambda( / dict)
                   (setq dict(dictsearch (namedobjdict) "ACAD_MLEADERSTYLE"))
                   (apply 'and(mapcar '(lambda(a)(vl-position(cons 3 a)dict))ls_mleadstyle))))
                )
               )
          )
  (gc)
  ;; (princ(list
  ;;        (apply 'and(mapcar '(lambda(a)(tblsearch "style" a))ls_stylenamed ))
  ;;        (apply 'and(mapcar '(lambda(a)(tblsearch "dimstyle" a))ls_dimstyle ))
  ;;        ((lambda( / dict)
  ;;           (setq dict(dictsearch (namedobjdict) "ACAD_MLEADERSTYLE"))
  ;;           (apply 'and(mapcar '(lambda(a)(vl-position(cons 3 a)dict))ls_mleadstyle))))
  ;;        ))
  

  (mapcar '(lambda(a b / str)
             (set b(if(tblsearch "style" a)a(getvar "TEXTSTYLE"))) )
          ls_stylenamed '(str_textstyle-button)
          )

  (setq ls_columnrow_category(mapcar '(lambda(a)(list(car a)0(cadr a)))ls_text_category_prev)
        ls_columnrow_prevdcl(mapcar 'list ls_columnrow_prev ls_str_command_prev ls_str_buttonwrite_prev)
        ls_columnrow_prevdcl(cdr(reverse(cdr(reverse ls_columnrow_prevdcl))))
        ls_dclbutton(vl-sort
                     (append ls_columnrow_prevdcl ls_columnrow_category )
                     '(lambda (a b)
                        (cond
                         ((< (caar a) (caar b)) T)
                         ((> (caar a) (caar b)) nil)
                         (T (< (cadar a) (cadar b)))
                         )
                        )
                     )
        
        )
  
  
  (setq path_dcl(strcat sp_path "commandbutton.dcl") 
        open_file (open path_dcl "w")
        help_command_x nil
        x_column nil int_numbutton -1
        )
  (mapcar
   '(lambda(str)(write-line str open_file))
   (append
    (list
     "Sedit :dialog"
     "{"
     ;; (strcat " label = \""(chr 12504)(chr 12523)(chr 12503)"\";")
     " :boxed_row"
     " {"
     )
    (mapcar
     '(lambda(a / str_button str_icon str_command bool_row)
        (setq int_numbutton(1+ int_numbutton) str_icon(cadr a)str_command(caddr a)
              bool_row nil;;(if(=(type str_icon)'STR)(findfile(strcat sp_path "icon_" str_icon ".sld")))
              )
        
        (if(= x_column(caar a))(setq str_button "")
          (setq str_button(strcat(if x_column "}\n" "")
                                 ":column \n { \n alignment = top;\n fixed_height = true;\n")
                x_column(caar a)))
        (if bool_row
            (setq str_button
                  (strcat str_button ":row \n { \n"
                          "  :icon_image\n"
                          "  {\n"
                          "   label = \"\";\n"
                          "   key = \"image"(itoa int_numbutton)"\";\n"
                          "   width = 4;\n"
                          "   height = 2;\n"
                          "   }\n"
                          )))
        
        (setq str_button
              (strcat str_button
                      (if(= str_icon 0)
                          (strcat
                           " :text\n"
                           " {\n"
                           "  value = \"" str_command "\";\n"
                           "  width = 30;\n"
                           "  fixed_width = true;\n"
                           "  }\n"
                           )
                        (strcat
                         " :button\n"
                         " {\n"
                         "  label = \"" str_command "\";\n"
                         "  key = \"button"(itoa int_numbutton) "\";\n"
                         "  width = 30;\n"
                         "  fixed_width = true;\n"
                         "  }\n"
                         )
                        ))
              )

        (if bool_row(setq str_button (strcat str_button "} \n")))
        str_button
        )
     ls_dclbutton
     )
    (list
     " }";;column
     "}";;boxed_row

     " :row"
     " {"
     "  "

     " :toggle"
     " {"
     (strcat "  label = \"" (car ls_str_buttonwrite_prev) "\";")
     "  key = \"button_help\";\n"
     "  width = 10;\n"
     "  fixed_width = true;\n"
     "  }\n"
     
     " :button"
     " {"
     (strcat "  label = \"" (last ls_str_buttonwrite_prev) "\";")
     "  key = \"button_licence\";\n"
     "  width = 30;\n"
     "  fixed_width = true;\n"
     "  }\n"
     
     " :ok_button"
     " {"
     (mix_strasc(list "  label = \"" ;;直近の操作繰り返し
                      30452 36817 12398 25805 20316 32368 12426 36820 12375
                      "\";"))
     "  width = 30;\n"
     "  fixed_width = true;\n"
     "  }\n"
     
     " :cancel_button"
     " {"
     "  width = 30;\n"
     "  fixed_width = true;\n"
     "  }\n"

     "  }"
     " }";//end
     )
    ))
  (close open_file)
  
  (setq isw 0)
  (setq load_dcl (load_dialog path_dcl))
  (new_dialog "Sedit" load_dcl)

  (setq int_numbutton -1)
  (mapcar
   '(lambda(a / str_button str_icon str_command bool_row)
      (setq int_numbutton(1+ int_numbutton) str_icon(cadr a)str_command(caddr a)
            bool_row(if(=(type str_icon)'STR)(findfile(strcat sp_path "icon_" str_icon ".sld"))))

      (if bool_row
          (progn
            (start_image(strcat "image"(itoa int_numbutton)))
            (slide_image -10 0 50 25 (strcat sp_path "icon_" str_icon ".sld"))
            (end_image)
            ))
      (if(= str_icon 0)T
        (action_tile(strcat "button"(itoa int_numbutton))
                    (strcat "(setq int_num "(itoa int_numbutton)")(done_dialog 1)")))
      )
   ls_dclbutton
   )
  
  (action_tile "accept" "(setq int_num nil)(done_dialog 1)")
  (action_tile "button_help" "(setq help_command_x(=(get_tile \"button_help\")\"1\"))")
  (action_tile "button_licence" "(setq int_num -1)(done_dialog 1)")
  
  (setq dialog-box (start_dialog)) ; ユーザー入力を受け付ける
  (unload_dialog load_dcl) ; ダイアログをメモリーから解放する
  (if int_num
      (setq str_command(if(= int_num -1)(last ls_str_command_prev)
                         (cadr(nth int_num ls_dclbutton)) )
            int_num(vl-position str_command ls_str_command_prev)
            int_preselectnumber(if str_command int_num)
            )
    (setq str_command(nth int_preselectnumber ls_str_command_prev))
    )

  (if help_command_x
      (progn
        (help_command_open str_command(if int_num(nth int_num ls_str_help)))
        (setq str_command nil)
        (quit)
        ))

  (*error* "")
  str_command
  )


(defun release_licence
    (str_command_mainmenue
     symls_function str_key_licence str_key_trial / str_formurl str_passpath)
  (if(if(setq str_cdate(getenv str_key_trial))(<=(getvar "CDATE")(atoi str_cdate)))
      (alert(mix_strasc(list 20307 39443 29256 20351 29992 20013 12391 12377 36820 21364 12399 24517 35201 12354 12426 12414 12379 12435)))
    (progn
      (eval (read (strcat "(defun C:" str_command_mainmenue " ( / ) (progn))")))
      (mapcar '(lambda(a)(set a nil)) symls_function)
      ;; (setq str_passpath(strcat sp_path "licence.dat"))
      ;; (if(findfile str_passpath)(vl-file-delete str_passpath))
      (setq str_passpath nil str_username "-")
      (if(setq open_file(open (strcat sp_path "idmemory.dat")"r"))
          (progn
            (read-line open_file)
            (setq str_username(read-line open_file))
            (close open_file)))
      (if str_username T (setq str_username "-"))
      
      (setq str_foemsent(strcat "%"(getenv str_key_licence))
            str_formurl "1FAIpQLSdBPZxBYuA16fzAuDrzwet-juqI86zs-ELrXy7DKxFW0OQHOQ"
            str_scriptpath (strcat sp_path "download.ps1")
            open_file(open str_scriptpath "w") )
      (mapcar '(lambda(str)(write-line str open_file))
              (list
               (strcat "$uri = \"https://docs.google.com/forms/d/e/" str_formurl "/formResponse\"")
               (strcat "$body = @{"
                       "\"entry.1800155201\" = \"" str_formsent "\" ; "
                       "\"entry.1628079341\" = \"" str_username "\" ; "
                       "\"entry.1260333573\" = \"" "release" "\" "
                       "}")
               "try { Invoke-WebRequest -Uri $uri -Method POST -Body $body -TimeoutSec 5 | Out-Null }"
               "catch {  }"
               )
              )
      (close open_file)

      (setq str_formsent nil)
      (setq shell (vlax-create-object "WScript.Shell"))
      (setq str_psexe (strcat "powershell -ExecutionPolicy Bypass -File \"" str_scriptpath "\""))
      (vlax-invoke shell 'Run str_psexe 0)
      (vlax-release-object shell)
      
      (setq ii 5)
      (setvar "CMDECHO" 0)
      (princ "\n\n")
      (while(>(setq ii(1- ii))-1)
        (vl-cmdf "._delay" 2000)
        (princ "\n")
        (princ(mix_strasc(list 35299 38500 20013 12539 12539 12539)))
        )
      (setvar "CMDECHO" 1)
      (vl-file-delete str_scriptpath)
      (setq release_licence nil)
      
      (setenv str_key_licence "")
      (alert(mix_strasc(list 12521 12452 12475 12531 12473 12434 36820 21364 12375 12289 12467 12510 12531 12489 12399 20351 29992 19981 21487 12395 12394 12426 12414 12375 12383
                             "\n" 27425 22238 20351 29992 26178 12399 12497 12473 12527 12540 12489 12398 20837 21147 12364 24517 35201 12391 12377)))
      ;;ライセンスを返却し、コマンドは使用不可になりました
      ;;次回使用時はパスワードの入力が必要です
      ))
  (progn)
  )

(defun dcl_key_check(str / val xval nval);;client_data_tileでできないときのため
  (setq val(get_tile str))
  (if(and val(/= val ""))T
    (progn
      (setq xval "99999")
      (set_tile str xval)
      (setq nval(get_tile str))
      (set_tile str val)
      (= nval xval)))
  )


(defun axd_make_dcl_attribute (str_path_tempdirectory / path_dcl)
  (setq path_dcl(strcat str_path_tempdirectory "addattribute.dcl") 
        open_file (open path_dcl "w")
        )
  
  (write_strlist
   open_file
   (list
    "Sedit :dialog"
    "{"
    (mix_strasc;;付与項目と値
     (list " label = \"" 20184 19982 38917 30446 12392 20516 "\";"))

    " :row"
    " {"
    "  :column"
    "  {"
    (mapcar
     '(lambda(str / str0 str1 bool)
        (list
         " :row"
         " {"
         "  alignment = left;"
         "  fixed_width = true;"

         (list_to_dcltext
          (list "text"(cons "width" "4")(cons "fixed_width" "true")
                (cons "key"(strcat "number_" str))))

         (list_to_dcltext
          (list "edit_box"(cons "width" "40")(cons "fixed_width" "true")
                (cons "key"(strcat "item_" str))))
         (list_to_dcltext
          (list "edit_box"(cons "width" "40")(cons "fixed_width" "true")
                (cons "key"(strcat "value_" str))))
         
         "  }"
         )
        )
     (mapcar 'itoa(inclist 0 20))
     )
    "   }"
    
    "  :column"
    "  {"
    "  alignment = centered;"
    "  fixed_height = true;"

    (list_to_dcltext
     (list "button"(cons "width" "10")(cons "fixed_width" "true")
           (cons "key" "up10")(cons "label"(chr 9650))))
    " spacer;"
    (list_to_dcltext
     (list "button"(cons "width" "10")(cons "fixed_width" "true")
           (cons "key" "up1")(cons "label"(chr 9652))))
    " spacer;"
    (list_to_dcltext
     (list "button"(cons "width" "10")(cons "fixed_width" "true")
           (cons "key" "down1")(cons "label"(chr 9662))))
    " spacer;"
    (list_to_dcltext
     (list "button"(cons "width" "10")(cons "fixed_width" "true")
           (cons "key" "down10")(cons "label"(chr 9660))))

    (list_to_dcltext
     (list "text"(cons "width" "8")(cons "fixed_width" "true")
           (cons "value" " 1 or 10")))
    
    " spacer;"
    " spacer;"

    "  }"
    " }"

    " spacer;"
    " :row"
    " {"
    
    (list_to_dcltext
     (list "text"(cons "width" "8")(cons "fixed_width" "true")
           (cons "value"(mix_strasc(list 34892 30058 21495)))))
    (list_to_dcltext
     (list "text"(cons "width" "4")(cons "fixed_width" "true")
           (cons "key" "selectfrom")))
    (list_to_dcltext
     (list "text"(cons "width" "4")(cons "fixed_width" "true")
           (cons "value"(mix_strasc(list 12363 12425)))))
    (list_to_dcltext
     (list "text"(cons "width" "4")(cons "fixed_width" "true")
           (cons "key" "selectto")))
    (list_to_dcltext
     (list "text"(cons "width" "4")(cons "fixed_width" "true")
           (cons "value"(mix_strasc(list 12414 12391)))))

    " spacer;"
    (list_to_dcltext
     (list "button"(cons "width" "10")(cons "fixed_width" "true")
           (cons "key" "copyval")(cons "label"(mix_strasc(list 12467 12500 12540)))))
    (list_to_dcltext
     (list "text"(cons "width" "2")(cons "fixed_width" "true")
           (cons "value" "or")))
    (list_to_dcltext
     (list "button"(cons "width" "10")(cons "fixed_width" "true")
           (cons "key" "pasteval")(cons "label"(mix_strasc(list 36028 20184)))))
    " spacer;"
    
    (list_to_dcltext
     (list "button"(cons "width" "10")(cons "fixed_width" "true")
           (cons "key" "selectnil")
           (cons "label"(mix_strasc(list 31354 30333 " = " 20840 31684 22258 )))))
    
    " }"
    (list_to_dcltext
     (list "text"(cons "width" "40")(cons "fixed_width" "true")
           (cons "value"(mix_strasc(list 8251 27396 12391 "Enter" 25276 19979 12391 31684 22258 12434 25351 23450)))))
    (list_to_dcltext
     (list "text"(cons "width" "40")(cons "fixed_width" "true")
           (cons "value"(mix_strasc
                         (list 8251 31354 34892 12395 12375 12383 31623 25152 12399 27425 22238 12363 12425 35440 12417 12390 34920 31034 12375 12414 12377)
                         ))))
    ;;※空行にした箇所は次回から詰めて表示します
    
    " ok_cancel;"
    " }";//end
    )
   )
  (close open_file)
  path_dcl
  )

(defun axd_function_read_session
    (str_initialsession str_session / );;dclを使わない;;多分外に出す
  (setq func_name "home")
  ((lambda(str)
     (mapcar '(lambda(a / sym)(if(setq sym(cdr(assoc str a)))(set sym nil)))ls_initialparameters))
   "TEMP")
  
  (cond
   ;;今回選んだセッションと前回選んだセッションが違っていたら呼び出す
   ((=(strcat str_initialsession str_session)prev_select_sessionname)
    )
   (T
    ((lambda(str)
       (mapcar '(lambda(a / sym)(if(setq sym(cdr(assoc str a)))(set sym nil)))ls_initialparameters))
     "SYMBOL")
    
    (if(vl-catch-all-error-p
        (setq asis_session
              (vl-catch-all-apply
               'vla-Item (list dicts_all(strcat str_initialsession str_session)))))
        (setq asis_session(vla-Add dicts_all(strcat str_initialsession str_session))
              xrec(vla-AddXRecord asis_session "PARAMETER") )
      (if(vl-catch-all-error-p
          (setq xrec(vl-catch-all-apply 'vla-Item (list asis_session "PARAMETER"))))
          T
        (progn
          (vla-GetXRecordData xrec 'array_type 'array_data )
          (setq lst(if array_data
                       (mapcar 'vlax-variant-value
                               (vlax-safearray->list array_data)))
                ;; lst(split_list 0 lst)
                )

          ((lambda(ls_dummy)
             
             (while lst
               (setq str(car lst)lst(cdr lst)val(car lst)lst(cdr lst))
               ((lambda(lst / sym a)
                  (while lst
                    
                    (setq a(car lst)sym(cdr(assoc "SYMBOL" a))lst(cdr lst))
                    
                    (if(if sym(=(alphabet_low(vl-symbol-name sym))
                                (alphabet_low str)))
                        (progn
                          (setq ls_dummy(vl-remove a ls_dummy)lst nil)
                          (set sym val)
                          ))
                    )
                  )
                ls_dummy)
               )

             
             ;;いるんだったらつかう
             ;; (setq path_csvlsp(strcat sp_path "inputcsv.lsp") 
             ;;       open_file_lsp(open path_csvlsp "w"))
             ;; (setq open_file_csv(open csvnm "r")ls_params(list))
             ;; (mapcar
             ;;  '(lambda(str / ii num str1 str_mark str_val bool_once)
             ;;     (write-line(strcat "(setq " str_mark str_val ")")open_file_lsp)
             ;;     )
             ;;  ls_params)
             ;; (close open_file_lsp)

             ;; (setq orig-secureload (getvar 'SECURELOAD))
             ;; (setvar 'SECURELOAD 0)
             ;; (if(vl-catch-all-error-p(vl-catch-all-apply 'load (list path_csvlsp)))
             ;;     (progn(alert "inputcsv(fordev)" )
             ;;           (quit)
             ;;           ))
             ;; (setvar 'SECURELOAD orig-secureload)
             ;; (setq orig-secureload nil)

             
             
             )
           ls_initialparameters)

          )
        )
      )
    
    (setq str_layname nil)
    (mapcar
     '(lambda(lst / sym str func)
        (setq sym(cdr(assoc "SYMBOL" lst))
              str(cdr(assoc "INITIALTYPE" lst))
              func(cdr(assoc "INITIALFUNC" lst)))
        (setq val(eval sym))
        (cond
         
         ((= str "LAYNAME")(setq str_layname val))
         ((and(= str "LAYCOL")str_layname)
          (if(vl-position str_layname ls_layername)T
            (entdel(entmakex(list(cons 0 "POINT")(list 10 0 0 0)(cons 8 str_layname)))))
          (setq vnam(vla-item
                     (vla-get-layers(vla-get-activedocument(vlax-get-acad-object)))
                     str_layname))
          (vla-put-color vnam val)
          (setq str_layname nil)
          )

         ((null val)
          (if func(set sym(func val)))
          )
         
         
         )

        )
     ls_initialparameters)

    (setq prev_select_sessionname(strcat str_initialsession str_session))
    
    )
   )
  
  )


(defun axd_command_initial(grfunc) ;;初期値initial common
  (setq bool_loop T bool_select nil p_selerac nil 
        bool_snap(<(getvar 'OSMODE)16384)
        ;;int_snap(if(<(getvar 'OSMODE)16384)0 1)
        
        ls_intchr(list 45 46 48 49 50 51 52 53 54 55 56 57)
        str_input "" ;;(if(= str_edit "solid")(rtos dist_unitsolid 2 3)"")
        str_edit_loop nil p_selerac nil
        p_gr5loop nil vec_view_gr5loop nil num_gudeentesc 2
        bool_backbright(bg-dark-p)
        )

  (mapcar 'set
          '(str_gcol_w str_gcol_r str_gcol_y str_gcol_c str_gcol_g str_gcol_p)
          (if bool_backbright
              (list "255" "10" "42" "140" "55" "220")
            (list "255" "10" "50" "130" "71" "241"))
          )
  
  (setq str_guidebackspace
        (mix_strasc
         (list "\n {\\C" str_gcol_p ";BackSpace} : "
               "{\\C" str_gcol_c ";" "1" 25991 23383 28040 12377 "}"
               " , {\\C" str_gcol_p ";BackSlash} : "
               "{\\C" str_gcol_c ";" 12377 12409 12390 28040 12377"}"
               " , {\\C" str_gcol_p ";Slash"  "} : "
               "{\\C" str_gcol_c ";" 25147 12377 "}"
               " , {\\C" str_gcol_p ";" "Enter," 21491 12463 12522 12483 12463 "} : "
               "{\\C" str_gcol_c ";" 27770 23450 "}"
               ))
        
        str_guidebackspace_mouse
        (mix_strasc
         (list "\n {\\C" str_gcol_y ";  "
               12510 12454 12473
               (if(= int_controleinputval 1)
                   (list 24038 21491)(list 19978 19979))
               12391 25968 20516 12434 21046 24481 "}"
               " , {\\C" str_gcol_p ";Slash"  "} : "
               "{\\C" str_gcol_c ";" 20024 12417 26689 19978 12370 "}"
               " , {\\C" str_gcol_p ";BackSlash} : "
               "{\\C" str_gcol_c ";" 20024 12417 26689 19979 12370 "}"
               " , {\\C" str_gcol_p ";BackSpace," 25968 20516 20837 21147 "} : "
               "{\\C" str_gcol_c ";" 12510 12454 12473 12514 12540 12489 32066 20102 "}"
               ))
        
        
        str_guidepointcancel
        (mix_strasc
         (list "\n {\\C" str_gcol_p ";" "Enter , " 21491 12463 12522 12483 12463 "} : "
               "{\\C" str_gcol_c ";"  29694 22312 12398 36984 25246 12398 12414 12414 "}"
               ))
        str_guideinitial1_1
        (mix_strasc
         (list "{\\C" str_gcol_c ";#} : "  12459 12540 12477 12523 12395 36861 24467
               " {\\C" str_gcol_c ";%} : " 12463 12522 12483 12463 21487 21542
               ))
        str_guideinitial1_2
        (mix_strasc
         (list "\n"
               "{\\C" str_gcol_c ";$} : " 30067 12416 ;;12383 12383 12416
               "  {\\C" str_gcol_c ";<} : " 23567 12373 12367
               "  {\\C" str_gcol_c ";>} : " 22823 12365 12367
               "  {\\C" str_gcol_c ";^} : " 19978 12408 
               "  {\\C" str_gcol_c ";v} : " 19979 12408
               "  {\\C" str_gcol_c ";~} : " 12513 12483 12475 12540 12472
               
               ;;メニューをカーソル付近に置く、放す ;;<:小さく、>:大きく ^:上へv:下へ
               ))
        
        ;; str_guidemeasureorinput
        ;; (mix_strasc(list "  {\\C" str_gcol_c ";!} : " 19968 26178 28204 36317))
        
        str_guideinitial2_c
        (mix_strasc
         (list "\n{\\C" str_gcol_c ";" 12304 20808 38957 12398 12461 12540 20837 21147
               "} (" 22823 25991 23383 23567 25991 23383 20840 35282 21322 35282 19981 21839 ") "
               "{\\C" str_gcol_c ";" 12305 "}  "  
               ;;先頭のキー入力(大文字小文字全角半角不問)   または
               ))
        str_guideinitial2_y
        (mix_strasc
         (list 12414 12383 12399
               "\n{\\C" str_gcol_y ";" 12304 "}"
               "{\\C" str_gcol_y ";" 9733 "}" 32 "{\\C30;" 9734"}" 32 12398 12392 12365
               "{\\C" str_gcol_y ";"
               " [" 24038 12463 12522 12483 12463 " :"
               "{\\C30;" 9734 12434 20778 20808 "}] , "
               "[ Enter , " 21491 12463 12522 12483 12463 "] " 12305 "}"
               ;;星のとき もう一度クリック,Enter,右クリック実行
               ))

        str_guide_inputval
        (mix_strasc(list "\n\n" 25968 23383 12461 12540 12392 12300 "." 12301 12300 "-" 12301 12398 12415 20837 21147 12434 21463 12369 20184 12369 12414 12377
                         ((lambda(str / n str_out)
                            (setq str_out "")
                            (while(vl-string-search "{" str)
                              (setq str(vl-string-subst "" "{" str)))
                            (while(vl-string-search "}" str)
                              (setq str(vl-string-subst "" "}" str)))
                            (while(setq n(vl-string-search "\\C" str))
                              (setq str_out(strcat str_out(substr str 1 n))
                                    n(vl-string-search ";" str)
                                    str(substr str(+ 2 n))
                                    )
                              )
                            (strcat str_out str))
                          str_guidebackspace)
                         ))
        ;;\n数字キーと「.」「-」のみ入力を受け付けます
        
        str_guide_selectval
        (mix_strasc(list "\n\n" 38917 30446 12434 36984 25246 12377 12427 12392 20516 12434 22793 26356 12391 12365 12414 12377 ))
        ;;項目を選択すると値を変更できます
        str_guide_selectmode
        (mix_strasc(list 12458 12502 12472 12455 12463 12488 12434 36984 25246 12375 12383 12392 12365 36861 21152 12373 12428 12427 12363 38500 22806 12373 12428 12427 12363 12434 20999 12426 26367 12360 12414 12377 "\n" 36984 25246 20013 12398 12458 12502 12472 12455 12463 12488 12399 12495 12452 12521 12452 12488 12373 12428 12414 12377 "\n" 36984 25246 12399 36890 24120 12398 "CAD" 25805 20316 12392 21516 27096 12395 21491 20596 22258 12415 12364 31684 22258 36984 25246 12289 24038 20596 22258 12415 12364 20132 24046 36984 25246 12392 12394 12387 12390 12356 12414 12377 "\n" 22522 26412 30340 12395 23550 35937 12392 12394 12427 12458 12502 12472 12455 12463 12488 12398 12415 12364 36984 25246 12373 12428 12427 20351 29992 12392 12394 12387 12390 12362 12426 23550 35937 12392 12394 12425 12394 12356 12418 12398 12434 22258 12435 12391 12418 12495 12452 12521 12452 12488 12373 12428 12394 12356 12424 12358 12395 12394 12387 12390 12356 12414 12377 ))
        ;;オブジェクトを選択したとき追加されるか除外されるかを切り替えます\n選択中のオブジェクトはハイライトされます\n選択は通常のCAD操作と同様に右側囲みが範囲選択、左側囲みが交差選択となっています\n基本的に対象となるオブジェクトのみが選択される使用となっており対象とならないものを囲んでもハイライトされないようになっています


        str_guide_point
        (mix_strasc(list "\n" 23550 35937 12392 12377 12427 20301 32622 12434 12463 12522 12483 12463 12375 12390 12367 12384 12373 12356))
        ;;対象とする位置をクリックしてください
        
        str_noguidedrawing(mix_strasc(list "\n\n" 22259 35299 28310 20633 20013 ));;図解準備中
        str_startguidedrawing(mix_strasc(list "\n\n" 22259 35299 12364 22987 12414 12426 12414 12377 ));;図解が始まります
        
        ;;項目を選択すると値を変更できます
        num_initialexplane 4

        cal_viewtopleft
        (lambda(ratio_height
                deltax deltay deltaz bool_3d 
                /  height_view ls_screen_size_yx width_view bool_plane)
          (setq p_viewcenter(getvar "VIEWCTR")
                height_view(getvar "VIEWSIZE")
                height_text(* height_view ratio_height)
                vec_view  (if bool_3d(unit_vector(getvar "VIEWDIR"))(list 0 0 1))
                bool_plane(and(<=(abs(car vec_view))0.015625)(<=(abs(cadr vec_view))0.015625))
                
                ang_viewtwist(if bool_plane(-(getvar "VIEWTWIST"))0.)
                vec_x_onview(list 1 0 0)vec_y_onview(list 0 1 0)
                )

          (if bool_plane
              (setq vec_x_onview(list(cos ang_viewtwist)(sin ang_viewtwist)0.)
                    vec_y_onview(list(-(sin ang_viewtwist))(cos ang_viewtwist)0.) )
            (if bool_3d
                (setq vec_x_onview(trans-x vec_x_onview vec_view(list 0 0 1))
                      vec_y_onview(trans-x vec_y_onview vec_view(list 0 0 1)) )
              ))
          
          (setq ls_screen_size_yx(getvar "SCREENSIZE")
                width_view(/(* height_view(car ls_screen_size_yx))
                            (cadr ls_screen_size_yx))
                deltaz((lambda( / p e v)
                         (if point_guide_center
                             (apply '+(mapcar '(lambda(p1 p2 v)(* v(- p1 p2)))
                                              point_guide_center p_viewcenter vec_view))
                           0.)
                         ))
                
                point_base
                (mapcar '(lambda(p x y z)
                           (+ p
                              (*(+ height_text(* -0.5 deltax width_view))x)
                              (*(+(* deltay height_text)(* 0.5 height_view)) y)
                              (* deltaz z)
                              ))
                        p_viewcenter vec_x_onview vec_y_onview vec_view)
                )
          
          ;; (setq limit_val 10000000.0)
          ;; (princ point_base)
          ;; (princ "\n")
          ;; 10000000.0,10000000.0,10000000.0
          ;; -10000000.0,-10000000.0,-10000000.0
          
          )
        
        )

  (progn ;;strinput
    (setq path_strinputdcl(strcat str_path_tempdirectory "strinput.dcl") 
          open_file (open path_strinputdcl "w")
          )
    (write_strlist
     open_file
     (list
      "Sedit :dialog"
      "{"
      (mix_strasc;;入力
       (list " label = \"" 20837 21147 "\";"))
      (list_to_dcltext
       (list "text"(cons "width" "40")(cons "fixed_width" "true")(cons "key" "guide")))
      (list_to_dcltext
       (list "edit_box"(cons "width" "40")(cons "fixed_width" "true")(cons "key" "strinput")))
      " ok_cancel;"
      " }";//end
      )
     )
    (close open_file)
    (setq settile_strinput
          (lambda(sym func str str_alert / func_las bool_loop accept_input);;funcエラー条件
            (setq load_dcl (load_dialog path_strinputdcl))
            (new_dialog "Sedit" load_dcl)
            (if str(set_tile "guide" str))
            (set_tile "strinput"(eval sym))
            (setq accept_input
                  (lambda( / str)
                    (setq str(get_tile "strinput"))
                    (if(if func(func str)T)
                        (progn (set sym str)(done_dialog 1))
                      (alert(if str_alert str_alert
                              (mix_strasc
                               (list 20837 21147 20869 23481 12364 19981 36969 20999 12391 12377))) ))
                    )
                  )
            (action_tile "accept" "(accept_input)")
            (setq dialog-box (start_dialog))
            (unload_dialog load_dcl)
            )
          )
    )
  
  (setq ls_grfunc(grfunc))
  (mapcar '(lambda(a / func str)
             
             (setq str(car a)a(cdr a)
                   func(cdr(assoc "INITIAL" a)))
             (mapcar '(lambda(a / sym)
                        (setq sym(cadr a))
                        (if a(if(if(car a)(boundp sym))T
                               (if sym(set sym(caddr a))
                                 ((caddr a)))
                               )))
                     (if func(func(list(= str str_edit) T))))
             )
          ls_grfunc)
  )

;;ls_initialparametersが必要,変数にしない理由が何かあった
(defun axd_make_dcl_basesetting
    (asis_session / num_dclwrite num_column num_row num_row_max num_page_max ii);;basicsetting
  ;;データを作った回数を記録
  (setq num_row_max 15 num_column_max 3)
  ((lambda( / ii num_column)
     (setq ii -1 num_column 1)
     (mapcar
      '(lambda(lst)
         (if(assoc "ITEM" lst) (setq ii(1+ ii)))
         (if(assoc "EXPLANE" lst)(setq ii(1+ ii)))
         (if(if(= ii 0)nil(or(>= ii num_row_max)(assoc "BREAK" lst)))
             (setq ii -1 num_column(1+ num_column)))
         )
      ls_initialparameters)
     (setq num_page_max(fix(/ num_column num_column_max)))
     (if(=(rem num_column num_column_max)0)T(setq num_page_max(1+ num_page_max)))
     ))
  
  (setq num_dclwrite -1 num_row 0)

  (while(nth num_row ls_initialparameters)
    (setq num_dclwrite(1+ num_dclwrite) num_column num_column_max 
          path_basicsettingdcl
          (strcat str_path_tempdirectory "basicsetting"(itoa num_dclwrite)".dcl") 
          open_file (open path_basicsettingdcl "w")
          )
    
    (write_strlist
     open_file
     (list
      "Sedit :dialog"
      "{"
      (mix_strasc;;基本設定
       (list " label = \""  22522 26412 35373 23450 "\";"))
      (list_to_dcltext
       (list "text"(cons "width" "100")(cons "fixed_width" "true")
             (cons "value"(mix_strasc
                           (list 8251 " " 12467 12510 12531 12489 20877 38283 26178 12399 12371 12398 30011 38754 12391 35373 23450 12375 12383 20516 12434 20351 29992 12375 12414 12377 12290
                                 20491 21029 12398 "DWG" 12395 35373 23450 12364 35352 25014 12373 12428 12414 12377 12290 
                                 )))))
      (list_to_dcltext
       (list "text"(cons "width" "100")(cons "fixed_width" "true")
             (cons "value"(mix_strasc
                           (list "    " 12371 12398 30011 38754 20197 22806 12391 20516 12434 35373 23450 12375 12383 12392 12365 12399 12467 12510 12531 12489 32066 20102 12414 12391 26377 21177 12391 12377 12290)))))
      ;;※コマンド再開時はこの画面で設定した値を使用します。個別のDWGに設定が記憶されます。
      ;;この画面以外で値を設定したときはコマンド終了まで有効です。
      " spacer;"
      " spacer;"
      
      " :row"
      " {"
      "  alignment = left;"
      "  fixed_width = true;"
      
      ((lambda( / ii ls_out lst
                  str0 str1 bool ls_out str_item str_explane str_sym sym str_break)
         (setq ii -1 )

         (while(and(setq lst(nth num_row ls_initialparameters))(> num_column 0))
           
           (setq str_item(cdr(assoc "ITEM" lst))
                 str_explane(cdr(assoc "EXPLANE" lst))
                 ls_out
                 (cons
                  
                  (if str_item
                      (progn
                        
                        (setq ii(1+ ii))
                        (if(setq sym(cdr(assoc "SYMBOL" lst)))
                            (setq str_sym(vl-symbol-name sym)))
                        
                        (list
                         (if(= ii 0)
                             (list " :column"
                                   " {"
                                   "  alignment = top;"
                                   "  fixed_height = true;"))
                         " :row"
                         " {"
                         "  alignment = left;"
                         "  fixed_width = true;"
                         
                         (list_to_dcltext
                          (list "text"(cons "width" "20")(cons "fixed_width" "true")
                                (cons "value"(cdr(assoc "TEXT" lst)))))

                         (cond
                          ((= str_item "TEXT"))
                          ((= str_item "BUTTON")
                           (list
                            (list_to_dcltext
                             (list "button"(cons "width" "10")(cons "fixed_width" "true")
                                   (cons "label"(cdr(assoc "BUTTONLABEL" lst)))
                                   (cons "key" str_sym) ))
                            (list_to_dcltext
                             (list "toggle"(cons "key"(strcat "toggle" str_sym))))
                            )
                           )
                          ((= str_item "EDIT_BOX")
                           (list_to_dcltext
                            (list "edit_box"(cons "width" "16")(cons "fixed_width" "true")
                                  (cons "key" str_sym)))
                           )
                          ((= str_item "POPUP_LIST")
                           (list_to_dcltext
                            (list "popup_list"(cons "width" "16")(cons "fixed_width" "true")
                                  (cons "key" str_sym)))
                           )
                          ((= str_item "COLOR")
                           (list_to_dcltext
                            (list "icon_image"(cons "width" "2")(cons "height" "1")
                                  (cons "key" str_sym)))
                           )
                          
                          )
                         
                         "  }"
                         
                         (if str_explane
                             (progn
                               (setq ii(1+ ii))
                               (list_to_dcltext
                                (list "text"(cons "width" "45")(cons "fixed_width" "true")
                                      (cons "value" str_explane)))
                               ))
                         " spacer;"
                         
                         (if(if(= ii 0)nil(or(>= ii num_row_max)(assoc "BREAK" lst)))
                             (progn
                               (setq ii -1 num_column(1- num_column))
                               
                               (list
                                " }"
                                (if(= num_column 0)nil
                                  (list
                                   " :column"
                                   " {"
                                   (mapcar
                                    '(lambda(i)
                                       (list_to_dcltext
                                        (list "text"(cons "width" "4");;(cons "fixed_width" "true")
                                              (cons "value"(chr 9474))));;9475
                                       )
                                    (inclist 0 num_row_max))
                                   "  }"
                                   ))
                                )
                               
                               ))
                         )
                        )
                    )
                  ls_out)
                 )
           (setq num_row(1+ num_row))
           )

         (if(and(> ii -1)(< ii num_row_max))
             (setq ls_out(cons
                          " }"
                          (cons
                           (list_to_dcltext
                            (list "text"(cons "width" "40")(cons "fixed_width" "true")))
                           ls_out ))))
         (reverse ls_out)
         ))
      
      "  }"
      
      " :row"
      " {"
      
      " :row"
      " {"
      "  alignment = left;"
      "  fixed_width = true;"

      (list_to_dcltext
       (list "button"(cons "width" "20")(cons "fixed_width" "true")
             (cons "key" "export_basicdata")
             (cons "label"(mix_strasc(list 35373 23450 12398 12456 12463 12473 12509 12540 12488)))))
      
      (list_to_dcltext
       (list "button"(cons "width" "20")(cons "fixed_width" "true")
             (cons "key" "inport_basicdata")
             (cons "label"(mix_strasc(list 35373 23450 12398 12452 12531 12509 12540 12488)))))
      
      
      "  }"
      
      " :row"
      " {"
      "  alignment = right;"
      "  fixed_width = true;"

      (list_to_dcltext
       (list "text"(cons "width" "16")(cons "fixed_width" "true")
             (cons "value"(strcat "(  "(itoa(1+ num_dclwrite))"  /  "(itoa num_page_max)"  )"))))
      
      (if(/= num_dclwrite 0)
          (list_to_dcltext
           (list "button"(cons "width" "20")(cons "fixed_width" "true")
                 (cons "key" "prev_setting")
                 (cons "label"(mix_strasc(list 21069 12398 35373 23450 12408)))))
        )
      
      (if(nth num_row ls_initialparameters)
          (list_to_dcltext
           (list "button"(cons "width" "20")(cons "fixed_width" "true")
                 (cons "key" "next_setting")
                 (cons "label"(mix_strasc(list 27425 12398 35373 23450 12408)))))
        )

      "  spacer;"
      "  spacer;"
      "  ok_cancel;"

      "  }"
      
      "  }"
      
      " }";//end
      )
     )
    (close open_file)
    
    ;;ループここまで
    )
  
  
  (setq
   settile_basicsetting
   (lambda( / bool_loop n_dia func_next num_dclwrite int_next
              settile_inport)
     (setq bool_loop T num_dclwrite 0)

     (setq settile_inport
           (lambda()
             (mapcar
              '(lambda(lst / str_item sym val str_sym sym_int i str_c str_l str_s
                           func_action)
                 (setq str_item(cdr(assoc "ITEM" lst)))
                 (if(setq sym(cdr(assoc "SYMBOL" lst)))
                     (setq val(eval sym) str_sym(vl-symbol-name sym)))
                 
                 (cond
                  
                  ((= str_item "EDIT_BOX")
                   (set_tile str_sym(as-numstr val))
                   )
                  
                  ((= str_item "COLOR")
                   (start_image str_sym)
                   (fill_image 0 0 20 20 val)
                   (end_image)
                   
                   (action_tile
                    str_sym
                    (strcat
                     "((lambda( / i )"
                     "  (if(setq i(acad_colordlg  " str_sym "))"
                     "      (progn"
                     "        (start_image \"" str_sym "\" )"
                     "        (fill_image 0 0 20 20 i)"
                     "        (end_image)"
                     "        (setq " str_sym " i)"
                     "        ))"
                     "  ))"
                     ))
                   
                   )
                  
                  ((= str_item "BUTTON")
                   (action_tile str_sym (strcat "(" (cdr(assoc "ACTION" lst))")"
                                                "(done_dialog 2)"))
                   (set_tile(strcat "toggle" str_sym)(if val "1" "0"))
                   (action_tile(strcat "toggle" str_sym)
                               (strcat "(set_tile \""(strcat "toggle" str_sym )"\" "
                                       "(if " str_sym " \"1\" \"0\"))"))
                   )
                  ((= str_item "POPUP_LIST")
                   
                   (start_list str_sym)
                   (mapcar 'add_list(cdr(assoc "VALLIST" lst)))
                   (end_list)
                   (set_tile str_sym(as-numstr val))
                   (if(setq func_action(cdr(assoc "ACTIONLIST" lst)))
                       (action_tile str_sym(strcat "(" func_action " \"" str_sym "\")")))
                   )
                  
                  
                  )
                 
                 )
              ls_initialparameters)
             )

           ls_symbolcode(list 53 29 41 61 37 31 23 47 43 59)
           export_basicdata
           (lambda( / ls_in lst str sym str_type str_key ls_str_out bool_x
                      str1 str2 ii)
             (setq ls_in ls_initialparameters)
             
             (while ls_in
               (setq lst(car ls_in)ls_in(cdr ls_in)
                     str_type(cdr(assoc "TYPE" lst)))
               
               (cond
                ((assoc "NOEXPORT" lst))
                ((setq sym(cdr(assoc "SYMBOL" lst)))
                 (setq str_key(vl-symbol-name sym)
                       str_val(get_tile str_key)
                       )
                 (if(if(or(null str_val)(= str_val ""))
                        (setq str_val(eval sym)))
                     (setq str_val(as-numstr str_val))
                   )

                 (if(or(null str_val)(= str_val ""))T
                   (setq str1(cdr(assoc "TEXT" lst))
                         str2(cdr(assoc "EXPLANE" lst))
                         ii -1
                         ls_str_out
                         (cons
                          (strcat
                           "T"
                           (apply 'strcat
                                  (mapcar '(lambda(a / b)
                                             (setq ii(1+ ii)
                                                   b(nth(rem ii(length ls_symbolcode))
                                                        ls_symbolcode))
                                             (substr(itoa(+ a(- b)100))2))
                                          (vl-string->list str_key) ) )
                           "," str_val ","
                           (mix_strasc
                            (if(=(cdr(assoc "ITEM" lst))"POPUP_LIST")
                                (list 12522 12473 12488 12398 30058 21495) ;;リストの番号
                              (if(= str_type "INT")(list 25972 25968 )
                                (if(= str_type "REAL")(list 23455 25968 )
                                  (if(=(cdr(assoc "ITEM" lst))"COLOR")
                                      (list 12452 12531 12487 12483 12463 12473 12459 12521 12540 30058 21495 )
                                    (list 12486 12461 12473 12488 )
                                    ))))
                            )
                           ","(if str1 str1 "") ","(if str2 str2 "")
                           ((lambda( / ls_str)
                              (if(setq ls_str(cdr(assoc "VALLIST" lst)))
                                  (apply 'strcat(mapcar '(lambda(a)(strcat "," a))ls_str))
                                "")))
                           )
                          ls_str_out)
                         )
                   )
                 
                 )
                )
               )

             (setq ls_str_out
                   (cons
                    (mix_strasc
                     (list "MARK,VALUE," 20837 21147 26041 27861 "," 35500 26126  ","
                           35036 36275 "," 12522 12473 12488 12398 20869 23481
                           "(" 12371 12398 21015 12364 "0" 21015 30446 12392 12375 12390
                           12289 21491 12395 "1" 12378 12388 22679 21152 "),"
                           8251 35501 36796 12399 12300 "MARK" 12301 12289
                           12300 "VALUE" 12301 21015 12398 12415 24517 35201 12391 12289
                           21015 12398 38918 30058 12362 12424 12403
                           12381 12398 20182 12398 21015 12398 20869 23481 12399
                           24433 38911 12375 12414 12379 12435
                           ))
                    (reverse ls_str_out))
                   )
             ;;記号,値,入力方法,補足,リストの内容(この列を0列目として、右に1ずつ増加),
             ;;※読込は「MARK」、「VALUE」列のみ必要で、列の順番およびその他の列の内容は影響しません
             
             (if(setq path_bsexport
                      (getfiled(mix_strasc(list 20445 23384 20808 12398 36984 25246 ))
                               (strcat(getvar "DWGPREFIX")"default-setting")"csv" 1))
                 (progn
                   (if command_for_alter
                       (setq open_file (open path_bsexport "w" ) );;"utf8"
                     (progn
                       (setq open_file (open path_bsexport "w" "utf8") )
                       (write-char 65279 open_file) ;; BOM
                       ))
                   (if open_file
                       (progn
                         (mapcar '(lambda(str)(write-line str open_file))ls_str_out)
                         (close open_file)
                         (alert(mix_strasc(list 20986 21147 12375 12414 12375 12383)))
                         )
                     (alert(mix_strasc(list 12501 12449 12452 12523 12364 38283 12363 12428 12390 12356 12427 12383 12417 26360 36796 12415 12391 12365 12414 12379 12435 12391 12375 12383 )))
                     )
                   )

               )
             
             nil
             )

           inport_basicdata
           (lambda( / ls_in lst str sym str_type str_key ls_str_out bool_x
                      ls_row0 func ii)
             (setq ls_in ls_initialparameters)

             (if(setq path_bsinport
                      (getfiled(mix_strasc(list 12487 12540 12479 12398 36984 25246 ));;
                               (getvar "DWGPREFIX")"csv" 0))
                 (progn
                   (setq open_file (open path_bsexport "r" ) )
                   
                   (setq str(read-line open_file)
                         lst(getlist_str_split str ",")
                         int_mark(vl-position "MARK" lst)
                         int_val(vl-position "VALUE" lst)
                         )
                   
                   (if(if(if int_mark T (progn(setq str "MARK") nil))
                          (if int_val T (progn(setq str "VALUE") nil)))
                       (while(setq str(read-line open_file))
                         (setq lst(getlist_str_split str ",")
                               ls_str_out(cons(list(nth int_mark lst)(nth int_val lst))
                                              ls_str_out)
                               )
                         )
                     (alert(mix_strasc(list 12371 12398 12487 12540 12479 12395 12399 str 12364 12394 12356 12383 12417 35501 12415 36796 12417 12414 12379 12435 )))
                     )
                   (close open_file)
                   )

               )
             
             (while ls_in
               (setq lst(car ls_in)ls_in(cdr ls_in)
                     str_type(cdr(assoc "TYPE" lst))
                     str_key(if(setq sym(cdr(assoc "SYMBOL" lst)))
                                (vl-symbol-name sym))
                     )
               
               (if(if(and ls_str_out str_key)
                      (setq ii -1
                            str_key
                            (mapcar '(lambda(a / b)
                                       (setq ii(1+ ii)
                                             b(nth(rem ii(length ls_symbolcode))
                                                  ls_symbolcode))
                                       (substr(itoa(+ a(- b)100))2))
                                    (vl-string->list str_key) )

                            str_key(strcat "T"(apply 'strcat str_key))
                            val(assoc str_key ls_str_out)
                            ls_str_out(vl-remove val ls_str_out)
                            val(cadr val)
                            ) )
                   (if(setq func(cdr(assoc "NOEXPORT" lst)))
                       (if(=(func)T)T(set sym(func)))
                     (cond
                      ((if(= str_type "INT")T(=(cdr(assoc "ITEM" lst))"COLOR"))
                       (set sym(as-atoi val)))
                      ((= str_type "REAL")(set sym(as-atof val)))
                      
                      (T(set sym val))
                      )
                     )
                 )
               )
             
             (settile_inport)
             (alert(mix_strasc(list 35501 12415 36796 12415 12414 12375 12383 )))
             nil
             )
           
           
           accept_setting
           (lambda(bool / bool_lay lst str_val)
             
             (mapcar
              '(lambda(lst / str sym str_type str_key)
                 (setq str_type(cdr(assoc "TYPE" lst)))
                 
                 (if(setq sym(cdr(assoc "SYMBOL" lst)))
                     (setq str_key(vl-symbol-name sym)
                           str_val(get_tile str_key)
                           ))
                 
                 (cond
                  ((or(null str_val)(= str_val "")))
                  ((= str_type "INT") (set sym(as-atoi str_val)) )
                  ((= str_type "REAL")(set sym(as-atof str_val)) )
                  ((= str_type "STR") (set sym str_val) )
                  )
                 )
              ls_initialparameters )
             
             ;;(setq xrec(vla-AddXRecord asis_session "PARAMETER") )
             (if(vl-catch-all-error-p
                 (setq xrec(vl-catch-all-apply 'vla-Item (list asis_session "PARAMETER"))))
                 T ;;見つからないことはないはず
               (progn
                 (setq lst(mapcar
                           '(lambda(a / sym str int_type str_type)
                              (if(setq sym(cdr(assoc "SYMBOL" a)))
                                  (setq str(vl-symbol-name sym)
                                        val(eval sym)str_type(type val)))
                              (if val
                                  (list(cons 1000 str)
                                       (cons(if(= str_type 'REAL)1040
                                              (if(= str_type 'INT)1071
                                                (if(= str_type 'STR)1000
                                                  )))
                                            val)))
                              )
                           ls_initialparameters)
                       lst(apply 'append lst)
                       array_data(vlax-make-safearray
                                  vlax-vbVariant(cons 0(1-(length lst))))
                       array_type(vlax-make-safearray
                                  vlax-vbInteger(cons 0(1-(length lst))))
                       )
                 (vlax-safearray-fill array_type(mapcar 'car lst))
                 (vlax-safearray-fill array_data(mapcar 'cdr lst))
                 (vla-SetXRecordData xrec array_type array_data )
                 ))
             
             (if bool(progn(setq int_next nil)(done_dialog 1)))
             
             )
           
           )
     
     (while bool_loop
       (setq path_basicsettingdcl
             (strcat str_path_tempdirectory
                     "basicsetting"(itoa num_dclwrite)".dcl")
             load_dcl(load_dialog path_basicsettingdcl))
       (new_dialog "Sedit" load_dcl)
       
       (settile_inport)

       (action_tile "export_basicdata" "(export_basicdata)")
       (action_tile "inport_basicdata" "(inport_basicdata)")
       
       (action_tile
        "prev_setting" "(accept_setting nil)(setq int_next -1)(done_dialog 3)")
       (action_tile
        "next_setting" "(accept_setting nil)(setq int_next  1)(done_dialog 3)")
       (action_tile "accept" "(accept_setting T)")
       (setq dialog-box (start_dialog))
       (unload_dialog load_dcl)
       
       (cond
        (func_next (func_next)(setq func_next nil))
        (int_next(setq num_dclwrite(+ int_next num_dclwrite)))
        (T (setq bool_loop nil) )
        )
       )
     
     )
   )
  )

(defun axd_settile_att
    (path_addattributedcl elem_grread ls_ssget / )
  (if(setq set_ent(ssget elem_grread ls_ssget))
      (progn
        (setq vnam(vlax-ename->vla-object(ssname set_ent 0)))
        
        (setq load_dcl (load_dialog path_addattributedcl))
        (new_dialog "Sedit" load_dcl)
        
        (setq bool_rangecopy nil int_att_initialnumber 1)
        (vla-getXData vnam "attributedata" 'array_Type 'array_Data )
        (setq ls_xdata
              (if array_data
                  (split_list 2(cdr(mapcar 'vlax-variant-value
                                           (vlax-safearray->list array_data)))) )
              ii 0
              ls_xdata(mapcar '(lambda(a)(cons(setq ii(1+ ii))a))ls_xdata)
              )
        
        (setq set_tilexdata
              (lambda()
                (mapcar
                 '(lambda(i / lst str num str1 tr2)
                    (setq str(itoa i)num(+ i int_att_initialnumber))
                    (set_tile(strcat "number_" str)(itoa num))
                    (if(setq lst(cdr(assoc num ls_xdata)))
                        (setq str1(car lst)str2(cadr lst))
                      (setq str1 "" str2 ""))
                    (set_tile(strcat "item_" str)str1)
                    (set_tile(strcat "value_" str)str2)
                    )
                 (inclist 0 20)
                 )
                (set_tile "selectfrom" "")
                (set_tile "selectto" "")
                )
              
              set_rangetile
              (lambda($reason int)
                (if(= $reason 1)
                    (progn
                      (setq bool_rangecopy(null bool_rangecopy)
                            str_row(itoa(+ int int_att_initialnumber)))
                      (set_tile(if bool_rangecopy "selectfrom" "selectto" )str_row)
                      ))
                )
              
              save_tile
              (lambda()
                (mapcar
                 '(lambda(i / str str1 str2 lst ls_c num)
                    (setq str(itoa i)num(+ int_att_initialnumber i)
                          str1(get_tile(strcat "item_" str))
                          str2(get_tile(strcat "value_" str))
                          ls_c(list num str1 str2)
                          
                          ls_xdata
                          (if(setq lst(assoc num ls_xdata))
                              (subst ls_c lst ls_xdata)
                            (cons ls_c ls_xdata))
                          )
                    )
                 (inclist 0 20)
                 )
                )

              scroll_tile
              (lambda(ii)
                (save_tile)
                (if(<= int_att_initialnumber (- ii))
                    (setq int_att_initialnumber 1)
                  (setq int_att_initialnumber(+ int_att_initialnumber ii)))
                (set_tilexdata)
                )

              accept_att
              (lambda( / bool_lay lst)
                (save_tile)
                (setq ls_xdata(vl-sort ls_xdata '(lambda(a b)(<(car a)(car b))))
                      ls_xdata(mapcar '(lambda(lst)
                                         (if(and(=(cadr lst)"")(=(caddr lst)""))nil
                                           (cdr lst)))
                                      ls_xdata)
                      ls_xdata(mapcar '(lambda(a)(cons 1000 a))
                                      (apply 'append(vl-remove nil ls_xdata)))
                      )
                (set_xda vnam ls_xdata "attributedata")
                (done_dialog 1)
                )

              att_copy
              (lambda( / str_out ii1 ii2)
                (setq str_out ""
                      ii1(atoi(get_tile "selectfrom"))
                      ii2(atoi(get_tile "selectto"))
                      )
                (if(= ii1 0)T(setq ii1(- ii1 int_att_initialnumber)))
                (if(= ii2 0)(setq ii2 20)(setq ii2(- ii2 int_att_initialnumber -1)))
                (mapcar '(lambda(i / str str1 str2)
                           (setq str(itoa i)
                                 str1(get_tile(strcat "item_" str))
                                 str2(get_tile(strcat "value_" str))
                                 str_out(strcat str_out str1 (chr 9) str2 (chr 10))
                                 )
                           )
                        (inclist(min ii1 ii2)(max ii1 ii2)))
                (copy_to_clip str_out)
                (alert(mix_strasc(list 12463 12522 12483 12503 12508 12540 12489 12395 12467 12500 12540 12375 12414 12375 12383)))
                )
              
              att_paste
              (lambda( / lst )
                (setq lst(get_clipboard_text)
                      ii1(atoi(get_tile "selectfrom"))
                      ii2(atoi(get_tile "selectto"))
                      )
                (if(= ii1 0)T(setq ii1(- ii1 int_att_initialnumber)))
                (if(= ii2 0)(setq ii2 20)(setq ii2(- ii2 int_att_initialnumber -1)))
                
                (mapcar '(lambda(i str_in / str str1 str2 num)
                           (setq str(itoa i))
                           (if(setq num(vl-string-search(chr 9)str_in))
                               (setq str1(substr str_in 1 num)
                                     str2(substr str_in(+ num 2)))
                             (setq str1 str_in str2 ""))
                           (set_tile(strcat "item_" str)str1)
                           (set_tile(strcat "value_" str)str2)
                           )
                        (inclist(min ii1 ii2)(max ii1 ii2))lst)
                
                (alert(mix_strasc(list 36028 20184 12375 12414 12375 12383)))
                )
              
              )

        (action_tile "up1" "(scroll_tile -1)")
        (action_tile "up10" "(scroll_tile -10)")
        (action_tile "down1" "(scroll_tile 1)")
        (action_tile "down10" "(scroll_tile 10)")
        (action_tile "selectnil" "(set_tile \"selectfrom\" \"\")(set_tile \"selectto\" \"\")")
        
        (mapcar
         '(lambda(int / str str_row str_range)
            (setq str(itoa int))
            (action_tile(strcat "item_" str)
                        (strcat "(set_rangetile $reason " str ")" ))
            (action_tile(strcat "value_" str)
                        (strcat "(set_rangetile $reason " str ")" ))
            )
         (inclist 0 20)
         )

        (action_tile "copyval" "(att_copy)")
        (action_tile "pasteval" "(att_paste)")
        (action_tile "accept" "(accept_att)")

        (set_tilexdata)


        (setq dialog-box (start_dialog))
        (unload_dialog load_dcl)

        
        
        ))
  
  )

(defun axd_command_loop();; common
  (setq textsize_guide_bo_temp textsize_guide_bo
        y_guidebase_temp y_guidebase
        bool_textclose nil bool_textfold nil
        str_guide_prev "" bool_guideclick T
        str_editreturn nil int_row_guideprev nil
        ls_str_guide_prev(list)p_controleinputval nil
        )

  (setq ls_pstar
        (list(list 0. 1.0)
             (list 0.224514 0.309017) (list 0.951057 0.309017)
             (list 0.363271 -0.118034) (list 0.587785 -0.809017)
             (list 0.0 -0.381966)
             (list -0.587785 -0.809017) (list -0.363271 -0.118034)
             (list -0.951057 0.309017) (list -0.224514 0.309017)
             (list 0. 1.0)))
  
  (while bool_loop
    (progn
      (if bool_snap(setq str_osnaps(_getosmode (getvar 'OSMODE))))
      ;;(if(= int_snap 0)(setq str_osnaps(_getosmode (getvar 'OSMODE))))
      (if(= str_edit_loop str_edit) T
        (progn
          (if(setq lst(assoc str_edit ls_grfunc))(setq ls_currentgrfunc(cdr lst))
            (progn
              (x-alert(list 26410 23455 35013 12391 12377))
              (setq str_edit str_edit_loop)
              ))
          (mapcar '(lambda(sym str)(set sym(cdr(assoc str ls_currentgrfunc))))
                  '(func_grinitial func_grdisp func_gr5 func_gr3 func_gr2)
                  (list "INITIAL" "GUIDE" "MOVE" "CLICK" "KEYBOAD"))
          (setq int_selectmenu nil ls_vnam_select nil initial_selectmenu nil 
                bool_input nil str_input "" ls_vnam_select nil bool_getpoint nil
                int_exchangeguide_gr5 nil ls_ssgetinsert nil
                str_addsnap ""
                )
          (setq str_funcmarker "INITIAL")
          (func_grinitial(list T))
          ))
      (setq str_edit_loop str_edit )

      (if((lambda( / str nn count start)
            (setq
             str
             (mix_strasc
              (if bool_textfold
                  (list "{\\C" str_gcol_c ";$} : " 38283 12367
                        ((lambda(lst / a str)
                           (while lst
                             (setq a(car lst)lst(cdr lst))
                             (if(vl-position(cons "NEXTMODE" str_edit)a)
                                 (setq lst nil str(cdr(assoc "ITEM" a))))
                             )
                           (if str(list "  " 12304 29694 22312 " : " str 12305))
                           )
                         ls_home_guide)
                        )
                
                (list
                 str_guideinitial1_1
                 " {\\C" str_gcol_c ";?} : "
                 
                 (if(or int_selectmenu int_starselectmenu)
                     (list 38917 30446 12460 12452 12489);;項目ガイド
                   (list 27425 12395 12420 12427 12371 12392) ;;次にやること
                   )
                 
                 (if(if(= str_edit "tempdistlength")(null bool_inputmeasure))nil
                   (list(if bool_inputmeasure nil(list " {\\C" str_gcol_c ";!} : "))
                        (if(or bool_input bool_inputmeasure)
                            (list "  {\\C" str_gcol_y ";"
                                  "2" 28857 12463 12522 12483 12463 12391 20837 21147 "}")
                          (list 19968 26178 30340 12394 28204 36317))
                        )
                   )
                 
                 ;; (if p_controleinputval
                 ;;     (vl-string-subst
                 ;;      "8;>"(strcat str_gcol_c ";>")
                 ;;      (vl-string-subst "8;<"(strcat str_gcol_c ";<")str_guideinitial1_2))
                 str_guideinitial1_2
                 
                 (if func_grdisp nil
                   (list str_guideinitial2_c
                         (if(= int_guideclick 1)
                             (list "\n{\\C" "53" ";"
                                   12463 12522 12483 12463 25805 20316 28961 21177 "}")
                           str_guideinitial2_y)
                         ))
                 
                 "{\\C" str_gcol_g ";"
                 (apply 'strcat (mapcar '(lambda(a)(strcat "\n" a))ls_guideexplane))
                 ;;(mapcar '(lambda(a)(mix_strasc(cons "\n" a)))ls_guideexplane)
                 "}"
                 
                 ((lambda(func lst / str ii func_input lst)
                    (setq ii -1)

                    (if ls_guidemenu
                        
                        
                        (list
                         
                         (mapcar
                          '(lambda(lst / num str str_bool str_key str_item str_val func_con sym_input
                                       bool_selectmenu bool_starselectmenu
                                       int_col int_key sym_bool ls_item)
                             
                             (setq ii(1+ ii)
                                   bool_selectmenu(= int_selectmenu ii)
                                   bool_starselectmenu(= int_starselectmenu ii))
                             
                             (if(setq int_key(caar lst))
                                 (setq str_key(if(= int_key 8)"BackSpace "
                                                (if(= int_key 32)"Space "
                                                  (strcat(chr int_key)" ")))))
                             
                             (setq str_item(cdr(assoc "ITEM" lst)))
                             
                             (cond
                              ((setq func_input(cdr(assoc "INPUT" lst)))
                               (setq sym_input(func_input)
                                     str_val(if(and bool_selectmenu bool_input)str_input
                                              (as-numstr(eval sym_input))))
                               )
                              ((setq func_input(cdr(assoc "INPUTSTR" lst))) 
                               (setq sym_input(func_input) str_val(eval sym_input))
                               )
                              ((setq func_input(cdr(assoc "INPUTCOLOR" lst)))
                               (setq sym_input(func_input) int_col(eval sym_input))
                               )
                              ((setq func_input(cdr(assoc "INPUTSWITCH" lst)))
                               (setq ls_item(func_input)str_val(nth(eval(car ls_item))(cadr ls_item)))
                               )
                              
                              ((setq func_input(cdr(assoc "GETPOINT" lst)))
                               (setq lst(func_input)
                                     str_val
                                     (list "{\\C"(if(car lst)
                                                     (list(cadr lst)";" 36984 25246 28168 12415 )
                                                   (list str_gcol_r ";" 26410 36984 25246)
                                                   )
                                           "}"
                                           )
                                     lst nil
                                     )
                               )
                              ((setq func_con(cdr(assoc "STATUS" lst)))
                               (setq str_val(func_con))
                               )
                              
                              )
                             
                             (cond
                              ((setq func_input(cdr(assoc "BOOL" lst)))
                               (setq int_displaybool 0)
                               (if(if func_input(func_input))
                                   (progn
                                     (setq int_displaybool(length str_val))
                                     (mapcar '(lambda(lst)
                                                (mix_strasc(cons "\n" lst)))
                                             str_val)))
                               )
                              (T(list "\n{\\C" str_gcol_c ";  " str_key "}"
                                      (if(or bool_selectmenu bool_starselectmenu)
                                          (list
                                           (if(or bool_selectmenu(null int_selectmenu))
                                               (list "{\\C" str_gcol_y ";" 9733)
                                             (list "{\\C30;" 9734))
                                           9 ": " str_item "}")
                                        
                                        (list 9 ": " str_item))
                                      (if str_val(list "  " 12304 32 str_val 32 12305))
                                      (if int_col(list "  {\\C"(itoa int_col)";" 9608 9608 9608"}"))
                                      (if(and bool_selectmenu bool_input)
                                          (if p_controleinputval
                                              str_guidebackspace_mouse str_guidebackspace)
                                        )
                                      (if(and bool_selectmenu bool_getpoint)str_guidepointcancel)
                                      )
                                )
                              )
                             )
                          lst)
                         
                         (if bool_point
                             (list "\n{\\C" str_gcol_c ";  S} "
                                   (if bool_selectsnap(list "{\\C" str_gcol_y ";" 9733 "}"))
                                   9 ": "
                                   (if bool_selectsnap
                                       (list "{\\C" str_gcol_y ";"
                                             12473 12490 12483 12503 20999 26367 "}")
                                     (list 12473 12490 12483 12503 20999 26367))
                                   
                                   "  " 12304 32 (if bool_snap "ON" "OFF") 32 12305)
                           )
                         
                         
                         "\n{\\C" str_gcol_y ";" 9733 "}" 32 "{\\C30;" 9734 "} "
                         "{\\C" str_gcol_g ";" 12364 12394 12356 12392 12365 " Enter , "
                         21491 12463 12522 12483 12463 " : "
                         (cdr(assoc "ITEM"(assoc(list "ENTER")ls_guidemenu)))"}"
                         )
                      (if func(func)) )
                    )
                  func_grdisp
                  (vl-remove-if '(lambda(a)(assoc "ENTER" a))ls_guidemenu)
                  )
                 

                 
                 "\n{\\C" str_gcol_p ";Esc : " 12467 12510 12531 12489 12434 32066 20102 "}" ;;終了
                 ))
              )
             )
            (setq str_funcmarker "DISPLAY")

            (setq nn 0 start 1)
            (while(setq count(vl-string-search "\n" str start))
              (setq nn(1+ nn)start(+ count 1))
              )
            (setq int_row_guide nn)
            
            (if(= str str_guide_prev)T
              (setq str_guide_prev str nn nil))
            ))
          T ;;IJのgrdrawのために必要
        (progn
          (vla-put-textstring
           vnam_guide
           str_guide_prev )
          
          (if(= int_row_guide int_row_guideprev)T
            (setq height_guidewhole
                  ((lambda(vnam / p0 p1 v r h)
                     (setq v(vla-get-normal vnam)
                           r(vla-get-rotation vnam))
                     (vla-put-normal vnam(vlax-3d-point 0 0 1))
                     (vla-put-rotation vnam 0)
                     (vla-getboundingbox vnam 'p0 'p1)
                     (setq h(-(cadr(vlax-safearray->list p0))
                              (cadr(vlax-safearray->list p1))))
                     (vla-put-normal vnam v)
                     (vla-put-rotation vnam r)
                     ;; (vla-put-attachmentpoint vnam_guide 7)
                     ;; (setq point_starbottom
                     ;;       (vlax-safearray->list(vlax-variant-value(vla-get-insertionpoint vnam_guide))))
                     ;; (vla-put-attachmentpoint vnam_guide 1)
                     h)
                   vnam_guide))
            )
          )
        )
      
      (if func_loopunique(func_loopunique))
      (if bool_replacegrread(setq bool_replacegrread nil)
        (setq ls_grread(grread t 15(if bool_point 0 2))
              int_grread(car ls_grread) elem_grread(cadr ls_grread) )
        )

      (cond
       (ls_guide_drawing
        ;;図解中;Esc以外の任意キー,左右クリックで戻る
        (vla-put-textstring vnam_guide(mix_strasc(list 22259 35299 20013 ";Esc" 20197 22806 12398 20219 24847 12461 12540 "," 24038 21491 12463 12522 12483 12463 12391 25147 12427 )))

        (setq p_gr5 nil)
        (while(progn
                (setq ls_grread(grread t 15(if bool_point 0 2))
                      int_grread(car ls_grread) elem_grread(cadr ls_grread) )
                (if(or(= int_grread 2)(= int_grread 3)(= int_grread 25))
                    (setq str_guide_prev "" vec_view_gr5loop nil
                          ls_guide_drawing nil)
                  T))
          (if(= int_grread 5)
              (progn
                (cal_viewtopleft textsize_guide_bo_temp x_guidebase y_guidebase_temp 10. T )
                (if(equal p_gr5 p_viewcenter)T
                  (progn
                    (redraw)
                    (vla-put-normal vnam_guide(vlax-3d-point vec_view))
                    (vla-put-InsertionPoint vnam_guide(vlax-3d-point point_base))
                    (vla-put-Height vnam_guide(* 2. height_text))
                    (vla-put-rotation vnam_guide ang_viewtwist)
                    
                    ((lambda(lst / d pc)
                       (setq d(* height_text(car lst)))
                       (mapcar
                        '(lambda(lst / p1 p2 c p01 p02 cc)
                           (setq p1(car lst)p2(cadr lst))
                           (if(setq c(caddr lst))
                               (if(setq cc(cdr(assoc c(if bool_backbright
                                                          ls_index_color_extw ls_index_color_extb))))
                                   (setq c cc))
                             (setq c 7))
                           (if(and p1 p2)
                               (progn
                                 (setq p01(mapcar '(lambda(a x y z);;zいらない
                                                     (+ a(*(apply '+(mapcar '*(list x y)p1))d)))
                                                  p_viewcenter vec_x_onview vec_y_onview vec_view)
                                       p02(mapcar '(lambda(a x y z)
                                                     (+ a(*(apply '+(mapcar '*(list x y)p2))d)))
                                                  p_viewcenter vec_x_onview vec_y_onview vec_view)
                                       )
                                 (grdraw p01 p02 c)
                                 ))
                           )
                        (cdr lst))
                       )
                     ls_guide_drawing)
                    (setq p_gr5 p_viewcenter)
                    ))
                
                ))
          )
        )
       
       ((vl-position int_grread(list 11 12))
        (vla-put-Height vnam_guide(* 1.6 height_text))
        (vla-put-textstring ;;画面回転有効：右クリック,Enterで戻る
         vnam_guide 
         (mix_strasc(list "{\\C" str_gcol_y ";" 30011 38754 22238 36578 26377 21177
                          65306 21491 12463 12522 12483 12463 ",Enter" 12391 25147 12427 "}")))
        
        (if bool_viewrotationfirsttime
            (progn
              (x-alert(list  30011 38754 22238 36578 26377 21177
                             "\n" 21491 12463 12522 12483 12463 ",Enter" 12391 25147 12427))
              (setq bool_viewrotationfirsttime nil)))
        ;; (vla-put-Visible vnam_guide 0)
        (getint(mix_strasc(list 21491 12463 12522 12483 12463 ",Enter" 12391 25147 12427)))
        (setq str_guide_prev "" vec_view_gr5loop nil)
        
        ;; (vla-put-Visible vnam_guide -1)
        
        )
       ((or(= int_grread 5)(= int_grread 3))

        (cal_viewtopleft textsize_guide_bo_temp x_guidebase y_guidebase_temp 10. T )

        ;;(and(equal point_base p_gr5loop)(equal vec_view vec_view_gr5loop))
        
        (if(if(and vec_view vec_view_gr5loop)
               (if((lambda(p1 p2 / x y z)
                     (setq x(-(car p1)(car p2))
                           y(-(cadr p1)(cadr p2))
                           z(-(caddr p1)(caddr p2)))
                     (<(+(* x x)(* y y)(* z z))1e-8))
                   vec_view vec_view_gr5loop)
                   ((lambda(p1 p2 / x y z)
                      (setq x(-(car p1)(car p2))
                            y(-(cadr p1)(cadr p2))
                            z(-(caddr p1)(caddr p2)))
                      (<(+(* x x)(* y y)(* z z))1e-8))
                    point_base p_gr5loop)
                 ))
            (if bool_textclose
                (progn
                  (setq point_starbase elem_grread)
                  (vla-put-InsertionPoint vnam_guide(vlax-3d-point elem_grread))

                  (setq height_guidewhole
                        ((lambda(vnam / p0 p1 v r h)
                           (setq v(vla-get-normal vnam)
                                 r(vla-get-rotation vnam))
                           (vla-put-normal vnam(vlax-3d-point 0 0 1))
                           (vla-put-rotation vnam 0)
                           (vla-getboundingbox vnam 'p0 'p1)
                           (setq h(-(cadr(vlax-safearray->list p0))
                                    (cadr(vlax-safearray->list p1))))
                           (vla-put-normal vnam v)
                           (vla-put-rotation vnam r)
                           ;; (vla-put-attachmentpoint vnam_guide 7)
                           ;; (setq point_starbottom
                           ;;       (vlax-safearray->list(vlax-variant-value(vla-get-insertionpoint vnam_guide))))
                           ;; (vla-put-attachmentpoint vnam_guide 1)
                           h)
                         vnam_guide))
                  
                  ))
          (progn
            (vla-put-normal vnam_guide(vlax-3d-point vec_view))
            (vla-put-InsertionPoint vnam_guide(vlax-3d-point point_base))
            (vla-put-Height vnam_guide height_text)
            (vla-put-rotation vnam_guide ang_viewtwist)
            (setq point_starbase point_base)

            
            (setq height_guidewhole
                  ((lambda(vnam / p0 p1 v r h)
                     (setq v(vla-get-normal vnam)
                           r(vla-get-rotation vnam))
                     (vla-put-normal vnam(vlax-3d-point 0 0 1))
                     (vla-put-rotation vnam 0)
                     (vla-getboundingbox vnam 'p0 'p1)
                     (setq h(-(cadr(vlax-safearray->list p0))
                              (cadr(vlax-safearray->list p1))))
                     (vla-put-normal vnam v)
                     (vla-put-rotation vnam r)
                     ;; (vla-put-attachmentpoint vnam_guide 7)
                     ;; (setq point_starbottom
                     ;;       (vlax-safearray->list(vlax-variant-value(vla-get-insertionpoint vnam_guide))))
                     ;; (vla-put-attachmentpoint vnam_guide 1)
                     h)
                   vnam_guide))
            
            ))
        
        (setq p_gr5loop point_base vec_view_gr5loop vec_view)
        (setq p_snap nil)
        
        (redraw)

        ((lambda(num_e num_g ls_bool / p1 p2 ny y nx lst)
           (if(or(= num_g 0)(= int_guideclick 1))T
             (if((lambda( / d)
                   (setq p1 point_starbase
                         d(apply '+(mapcar '(lambda(a b c)(*(- b a)c))
                                           p1 elem_grread vec_x_onview)))
                   (if(and(> d 0)(< d(* width_guideclick height_text)))
                       (progn
                         (setq p2 point_starbottom
                               ny(*(/(apply '+(mapcar '(lambda(a b c)(*(- b a)c))
                                                      p1 elem_grread vec_y_onview))
                                     height_guidewhole
                                     ;; (apply '+(mapcar '(lambda(a b c)(*(- b a)c))
                                     ;;                  p1 p2 vec_y_onview))
                                     )
                                   (+ num_e num_g num_gudeentesc
                                      (if bool_point 1 0)
                                      (if bool_input 1 0)
                                      (if bool_getpoint 1 0)
                                      (apply '+(mapcar '(lambda(lst / func)
                                                          (if(assoc "BOOL" lst)int_displaybool) )
                                                       ls_bool))
                                      ))
                               ny(-(fix ny)num_e)
                               )
                         
                         (if(if bool_input(> ny int_selectmenu))(setq ny(1- ny)))
                         (if(if bool_getpoint(> ny int_selectmenu))(setq ny(1- ny)))
                         
                         (setq bool_selectsnap(if bool_point(= ny num_g))
                               int_starselectmenu
                               (if(or(< ny 0)(> ny num_g))nil ny)
                               )
                         int_starselectmenu
                         )
                     (setq int_starselectmenu nil)
                     )
                   ))
                 (if bool_textfold T
                   ((lambda(ls_p)

                      (mapcar '(lambda(p0 p1)(grdraw p0 p1(atoi str_gcol_r)))
                              ls_p(cdr ls_p)))
                    (mapcar '(lambda(v / xx yy)
                               (setq xx(* height_text(car v))
                                     yy(* height_text(cadr v)))
                               (mapcar '(lambda(a x y)(+ a(* xx x)(* yy y)))
                                       elem_grread vec_x_onview vec_y_onview))
                            ls_pstar))
                   )
               ) )
           )
         (+ num_initialexplane(length ls_guideexplane))
         (length(vl-remove-if '(lambda(a)(or(assoc "ENTER" a)(assoc "BOOL" a)))
                              ls_guidemenu))
         (vl-remove-if '(lambda(a)(null(assoc "BOOL" a)))ls_guidemenu)
         )
        
        (if(if(and(zerop (logand 16384 (getvar 'OSMODE)))bool_snap bool_point)
               (setq p_snap (osnap elem_grread(strcat str_osnaps str_addsnap))))
            ((lambda(p c / s ls_p)
               (setq s(* height_text 0.8)
                     ls_p(mapcar
                          '(lambda(xx yy)
                             (mapcar '(lambda(a x y)(+ a(* xx x)(* yy y)))
                                     p vec_x_onview vec_y_onview)
                             )
                          (list s(- s)(- s)s) (list s(- s)s(- s)))
                     )
               (mapcar '(lambda(p0 p1)(grdraw p0 p1 c))
                       (list(car ls_p)(caddr ls_p))
                       (list(cadr ls_p)(cadddr ls_p)))
               )
             (setq elem_grread p_snap)int_colsnappoint)
          )
        
        (if guidegr5_unique(guidegr5_unique))
        (if func_gr5(func_gr5))
        (setq str_funcmarker "MOVE")
        
        (if(if bool_selectent p_selerac)
            (progn
              (setq vec_x(trans-x(list 1 0 0)vec_view(list 0 0 1))
                    vec_y(trans-x(list 0 1 0)vec_view(list 0 0 1))
                    vec_delta(mapcar '- elem_grread p_selerac)
                    x_delta(apply '+(mapcar '* vec_x vec_delta))
                    y_delta(apply '+(mapcar '* vec_y vec_delta))
                    ls_prac(list p_selerac(mapcar '(lambda(a b)(+ a(* y_delta b)))p_selerac vec_y)
                                 elem_grread(mapcar '(lambda(a b)(+ a(* x_delta b)))p_selerac vec_x)
                                 p_selerac)
                    int_color(if(< x_delta 0) 141 81)
                    )
              (mapcar 'grdraw ls_prac(cdr ls_prac)
                      (list int_color int_color int_color int_color))
              
              )
          )
        
        (if(if p_controleinputval(>(distance p_controleinputval elem_grread)1e-8))
            (setq str_input
                  ((lambda(val i)
                     (if(< i 0)
                         (progn
                           (setq val(rtos(* val(expt 10. i))2 0))
                           (while(<(setq i(1+ i))1)
                             (setq val(strcat val "0")))
                           val
                           )
                       (rtos val 2 i))
                     )
                   (apply '+(mapcar '*(mapcar '- elem_grread p_controleinputval)
                                    (if(= int_controleinputval 1)vec_x_onview vec_y_onview)))
                   int_roundcontroleval )))
        
        (cond
         ((= int_grread 5))
         (bool_selectsnap (setq bool_snap(null bool_snap)) )
         (int_starselectmenu
          (setq int_selectmenu int_starselectmenu
                lst(nth int_selectmenu ls_guidemenu))
          ;; (if(= ny int_selectmenu)
          (if(setq str(cdr(assoc "NEXTMODE" lst)))
              (if(setq func(cdr(assoc "CLICKFUNCTION" lst)))(func)
                (setq str_edit str int_selectmenu nil)
                )
            (if(or(cdr(assoc "INPUT" lst))
                  (cdr(assoc "INPUTSTR" lst))
                  (cdr(assoc "INPUTCOLOR" lst))
                  (cdr(assoc "INPUTSWITCH" lst))
                  (cdr(assoc "LOADFUNCTION" lst))
                  (cdr(assoc "GETPOINT" lst))
                  )
                (setq bool_replacegrread T bool_input nil
                      int_grread 2 elem_grread(caar lst))
              )
            )
          
          ;;(setq str_edit str_edit_loop))
          ;; (progn
          ;;   (setq int_selectmenu ny)
          ;;   )
          
          )
         
         ((and bool_input(/= ny int_selectmenu))
          (setq int_selectmenu nil bool_input nil p_controleinputval nil)
          ;;(setq bool_replacegrread T int_grread 2 elem_grread 13)
          )
         (bool_getpoint (func_input T) )
         (T

          (setq int_selectmenu nil ls_vnam_temp(list))
          
          (cond
           (bool_point)
           ;;((null bool_selectent))
           (p_selerac
            ;; (if ls_vnam_select T
            ;;   (progn
            (setq set_ent(ssget(if(< x_delta 0)"C" "W")
                               p_selerac elem_grread
                               ls_ssget )
                  
                  num(if set_ent(sslength set_ent)0))
            (while(>(setq num(1- num))-1)
              (setq ls_vnam_temp(cons(vlax-ename->vla-object(ssname set_ent num))
                                     ls_vnam_temp))
              )
            ;; ))
            
            (setq bool_select nil p_selerac nil )
            )
           
           ((setq vnam(if(setq set_ent(ssget elem_grread ls_ssget ))
                          (vlax-ename->vla-object(setq entna(ssname set_ent 0)))))
            (setq ls_vnam_temp(list vnam) bool_select nil)
            ;; num 1 bool_replacegrread T int_grread 3 elem_grread nil)
            
            )
           
           ((if ls_ssgetinsert
                (setq vnam(if(setq set_ent(ssget elem_grread ls_ssgetinsert))
                              (vlax-ename->vla-object(setq entna(ssname set_ent 0))))))
            
            (setq p_insert(vlax-safearray->list(vlax-variant-value(vla-get-insertionpoint vnam)))
                  rotation_insert(vla-get-rotation vnam)
                  vec_insert(vlax-safearray->list(vlax-variant-value(vla-get-normal vnam)))
                  x_scale(vla-get-XScaleFactor vnam)
                  y_scale(vla-get-YScaleFactor vnam)
                  z_scale(vla-get-ZScaleFactor vnam)
                  transMat(vlax-tmatrix (list(list 0 -1 0 0)
                                             (list 1 0 0 0)
                                             (list 0 0 1 0)
                                             (list 0 0 0 1)))
                  )
            
            ;;normal非対応
            ;; (if(and(<(abs(car vec_insert))1e-8)(<(abs(cadr vec_insert))1e-8))
            ;;     (progn
            (setq ls_nent(nentselp elem_grread)
                  entna(car ls_nent)
                  ls_gcode(entget entna)
                  )
            
            (if(=(cdr(assoc 0 ls_gcode))"VERTEX")
                (progn
                  (while (/= "SEQEND" (cdr(assoc 0(entget entna))))
                    (setq entna(entnext entna))
                    )
                  (setq entna(cdr(assoc -2 (entget entna)))
                        ls_gcode(entget entna))
                  ))
            

            (setq vnam(vlax-ename->vla-object entna)
                  int_col(get_visual_color vnam)

                  vnam
                  (vla-CopyObjects
                   (vla-get-ActiveDocument(vlax-get-acad-object))
                   (vlax-make-variant
                    (vlax-safearray-fill
                     (vlax-make-safearray vlax-vbObject (cons 0 0))
                     (list vnam)))
                   (vla-get-ModelSpace(vla-get-ActiveDocument(vlax-get-acad-object)))
                   )
                  vnam(car(vlax-safearray->list(vlax-variant-value vnam)))
                  )

            (if(ssget "L" ls_ssget)
                (progn
                  
                  ;; (if(apply 'and(mapcar
                  ;;                '(lambda(a / ii dd xx)
                  ;;                   (setq ii(car a)dd(cdr a)
                  ;;                         xx(cdr(assoc ii ls_gcode)))
                  ;;                   (if(=(type dd)'STR)
                  ;;                       (vl-string-search xx dd)
                  ;;                     (equal xx dd))
                  ;;                   )
                  ;;                ls_ssget))
                  ;;     (progn

                  
                  (setq ls_vnam_temp(cons vnam ls_vnam_temp) )
                  (vla-put-color vnam int_col)
                  (vla-scaleentity vnam(vlax-3d-point 0 0 0)x_scale)
                  (vla-Rotate3D
                   vnam(vlax-3d-point 0 0 0)(vlax-3d-point 0 0 1) rotation_insert)
                  (vla-Move vnam(vlax-3d-point 0 0 0)(vlax-3d-point p_insert))
                  ;; (vla-TransformBy vnam transMat)
                  
                  (addkillobj vnam)
                  
                  )
              (vla-delete vnam)
              )
            
            
            )
           
           ((= int_selectmode -1) );;単体選択
           ((and(null ls_vnam_temp)(null p_selerac))
            (setq p_selerac elem_grread bool_select T)
            )
           
           )

          
          (if(and xtype_ssget xdata_ssget)
              (setq ls_vnam_temp
                    (vl-remove-if
                     '(lambda(vnam / ls_xdata str_type)
                        (vla-getXData vnam xdata_ssget 'array_Type 'array_Data )
                        
                        (if array_data
                            (setq ls_xdata
                                  (split_list 0(mapcar 'vlax-variant-value
                                                       (vlax-safearray->list array_data)))
                                  str_type(cdr(assoc xdata_ssget ls_xdata)))
                          )
                        (if str_type
                            (null(vl-string-search str_type xtype_ssget))
                          T)
                        
                        )
                     ls_vnam_temp)))

          
          
          (if(= int_selectmode -1) (setq ls_vnam_select ls_vnam_temp)
            (progn
              (if(= int_selectmode 0)
                  (mapcar '(lambda(v)
                             (setq ls_vnam_highlight(cons v ls_vnam_highlight)
                                   ls_vnam_select(cons v ls_vnam_select)))
                          ls_vnam_temp)
                (mapcar '(lambda(v)
                           (setq ls_vnam_highlight(vl-remove v ls_vnam_highlight)
                                 ls_vnam_select(vl-remove v ls_vnam_select))
                           (vla-Highlight v :vlax-false)
                           )
                        ls_vnam_temp)
                )
              
              (mapcar '(lambda(a)(vla-Highlight a :vlax-true))ls_vnam_highlight)
              ))

          (if func_gr3(func_gr3))
          (setq str_funcmarker "CLICK")
          )
         )
        (if(= int_grread 3)(setq bool_getpoint nil))
        )

       ((or(= int_grread 2)(= int_grread 25))
        (if(and bool_getpoint(or(= elem_grread 13)(= int_grread 25)))(func_input nil) )
        (setq bool_getpoint nil)
        (cond
         ((and(vl-position elem_grread(list 33 65281 -255))(/= str_edit "tempdistlength"))
          (if bool_input(setq bool_inputmeasure T str_inputmeasure nil))
          (setq str_editreturn str_edit str_edit "tempdistlength"))
         ((vl-position elem_grread(list 35 65283 -253))
          (setq bool_textclose(null bool_textclose)))
         ((vl-position elem_grread(list 36 65284 -252))
          (setq bool_textfold(null bool_textfold)))
         ((vl-position elem_grread(list 37 65285 -251))
          (setq int_selectmenu nil int_starselectmenu nil
                int_guideclick(rem(1+ int_guideclick)2)))
         
         ((vl-position elem_grread(list 94 65342 -194))
          (setq y_guidebase_temp(1+ y_guidebase_temp)) )
         ((vl-position elem_grread(list 86 118 65334 65366 -202 -170))
          (setq y_guidebase_temp(1- y_guidebase_temp)) )

         ((vl-position elem_grread(list 63 65311 -225))
          (if(if(setq func
                      (cond
                       ((if(and ls_guidemenu int_selectmenu)
                            (cdr(assoc "HELP"(nth int_selectmenu ls_guidemenu)))))
                       ((if(and ls_guidemenu int_starselectmenu)
                            (cdr(assoc "HELP"(nth int_starselectmenu ls_guidemenu)))))
                       (func_guidemenu func_guidemenu)
                       )
                      )
                 (setq str(func)))
              (alert str))
          
          )
         
         ((vl-position elem_grread(list 126 65374 -162))
          (alert(mix_strasc
                 (list((lambda(str / n str_out)
                         (setq str_out "")
                         (while(vl-string-search "{" str)
                           (setq str(vl-string-subst "" "{" str)))
                         (while(vl-string-search "}" str)
                           (setq str(vl-string-subst "" "}" str)))
                         (while(setq n(vl-string-search "\\C" str))
                           (setq str_out(strcat str_out(substr str 1 n))
                                 n(vl-string-search ";" str)
                                 str(substr str(+ 2 n))
                                 )
                           )
                         (strcat str_out str))
                       str_guide_prev)
                      )))

          
          ;; (setq int_guidemask(rem(1+ int_guidemask)2))
          ;; (setq ls_vnam_killobj(vl-remove vnam_guide ls_vnam_killobj))
          ;; (vla-delete vnam_guide)
          ;; (setq vnam_guide
          ;;       ((lambda( / e v)
          ;;          (setq e(entmakex
          ;;                  (append
          ;;                   (list(cons 0 "MTEXT")(cons 100 "AcDbEntity")(cons 100 "AcDbMText")
          ;;                        (cons 7 str_textstyle_gbo)(cons 62 255)(cons 40 0.01 )(list 10 0 0 0)
          ;;                        (cons 1 "AAAAAAAAA")(cons 71 1)
          ;;                        )
          ;;                   (if(= int_guidemask 1)
          ;;                       ;;(list(cons 90 1)(cons 63 177)(cons 421 986966)(cons 45 2.))
          ;;                       (list(cons 90 3)(cons 63 256)(cons 45 2.))
          ;;                     )
          ;;                   )))
          ;;          (vlax-ename->vla-object e)))
          ;;       )
          ;; (addkillobj vnam_guide)
          ;; (setq str_guide_prev "" vec_view_gr5loop nil
          ;;       bool_replacegrread T int_grread 5 elem_grread(list 0 0 0))
          
          )
         
         ((and(vl-position elem_grread(list 83 115))bool_point)(setq bool_snap(null bool_snap)))

         ((and bool_input(vl-position elem_grread ls_intchr))
          (setq str_input(strcat str_input(chr elem_grread)))
          (setq p_controleinputval nil)
          )
         ((and bool_input(= elem_grread 8))
          (setq str_input(substr str_input 1(1-(strlen str_input))))
          (setq p_controleinputval nil)
          )
         
         ((and bool_input(vl-position elem_grread(list 92 165 65340 65509 -196 -27)))
          (if p_controleinputval
              (setq int_roundcontroleval(1+ int_roundcontroleval))
            (setq str_input "") ))
         ((and bool_input(vl-position elem_grread(list 47 65295 -241)))
          (if p_controleinputval
              (setq int_roundcontroleval(1- int_roundcontroleval))
            (setq str_input(as-numstr(eval sym_input))) ))
         ((and(or(and bool_input(/= int_grread 3))
                 (if(and bool_inputmeasure str_inputmeasure)
                     (setq bool_inputmeasure nil str_input str_inputmeasure)))
              (or(= elem_grread 13)(= int_grread 25)))
          (if(or(if(vl-string-search "-"(substr str_input 2))
                    (progn;;先頭以外に「-」があります
                      (x-alert(list 20808 38957 20197 22806 12395 12300 "-" 12301 12364 12354 12426 12414 12377 )) T))
                (if(if(setq num(vl-string-search "." str_input))
                       (vl-string-search "."(substr str_input(+ 2 num))))
                    (progn;;「.」が2か所以上あります
                      (x-alert(list 12300 "." 12301 12364 "2" 12363 25152 20197 19978 12354 12426 12414 12377 )) T))
                )
              T
            (progn
              (set sym_input(cond((= type_input 'REAL)(as-atof str_input))
                                 ((= type_input 'INT) (as-atoi str_input))
                                 (T str_input)))
              (if func_input(func_input))
              (setq bool_input nil int_selectmenu nil str_input "")
              (if(= int_controleinputval 0)T(setq p_controleinputval nil))
              ))
          
          )
         
         ((setq ii(vl-position elem_grread(list 60 62 65308 65310 -228 -226)))
          ((lambda( / a)
             (setq a(+(nth(rem ii 2)(list -0.001 0.001))textsize_guide_bo_temp))
             (if(< a 0.)T(setq textsize_guide_bo_temp a))
             ))
          )
         ;; ((setq ii(vl-position elem_grread(list 60 62)))
         ;;  (setq x_guidebase(+(nth ii(list 0.05 -0.05))x_guidebase)))
         
         (((lambda(lst / a str_next func num)
             (if int_selectmenu T
               (if int_starselectmenu(setq int_selectmenu int_starselectmenu)))
             (while lst
               (setq a(car lst) lst(cdr lst))
               (if(if(equal(car a)(list "ENTER"))
                      (if(or(= elem_grread 13)(= int_grread 25))
                          (cond
                           (int_selectmenu;;selectmenuがあるとき一覧にないキーを押すと反応してしまう
                            (if(< int_selectmenu 0)(setq lst nil)
                              (setq a(nth int_selectmenu ls_guidemenu))))
                           (ls_vnam_select (setq lst nil));;func_gr2へ
                           (T T);;(or(= elem_grread 13)(= int_grread 25)))
                           ))
                    (progn
                      
                      (cond
                       ((or(and(< 65295 elem_grread)(< elem_grread 65306));;全角数字
                           (and(< 65312 elem_grread)(< elem_grread 65371)));;全角アルファベット
                        (setq elem_grread(- elem_grread 65248)))
                       ((or(and(< -241 elem_grread)(< elem_grread -230))
                           (and(< -224 elem_grread)(< elem_grread -165)))
                        (setq elem_grread(+ elem_grread 288)))
                       )
                      (cond
                       ((and(< 96 elem_grread)(< elem_grread 123))
                        (setq elem_grread(- elem_grread 32)))
                       )
                      (vl-position elem_grread(car a))
                      )
                    )
                   (if(cond
                       ((if(setq func_input(cdr(assoc "NO-INPUT" a)))
                            (func_input))
                        (setq int_selectmenu nil)
                        (setq str_next T)
                        )
                       ((setq str_next(cdr(assoc "NEXTMODE" a)))
                        (if(setq func(cdr(assoc "LOADFUNCTION" a)))(func))
                        T)
                       ((setq func_input(cdr(assoc "INPUT" a)))
                        
                        (setq sym_input(func_input)
                              val_input(eval sym_input)
                              str_input(as-numstr val_input)
                              type_input(type val_input)
                              int_selectmenu(vl-position a ls_guidemenu)
                              func_input(cdr(assoc "LOADFUNCTION" a))
                              bool_input T
                              ;;str_edit str_edit_loop
                              )
                        (if(= int_controleinputval 0)T(setq p_controleinputval(cadr(grread t 15 0))))
                        T)
                       
                       ((setq func_input(cdr(assoc "INPUTSTR" a)))
                        (setq sym_input(func_input)
                              func_input(cdr(assoc "LOADFUNCTION" a))
                              int_selectmenu nil)
                        
                        (settile_strinput
                         sym_input func_input
                         (mix_strasc(cdr(assoc "ITEM" a)))
                         (cdr(assoc "STRINPUTALERT" a))
                         )
                        
                        (setq str_next T)
                        )

                       ((setq func_input(cdr(assoc "INPUTCOLOR" a)))
                        (setq sym_input(func_input)
                              func_input(cdr(assoc "LOADFUNCTION" a))
                              int_selectmenu nil)
                        (if(setq i(acad_colordlg(eval sym_input)))
                            (set sym_input i))
                        (if func_input(func_input))
                        (setq str_next T)
                        )
                       ((setq func_input(cdr(assoc "INPUTSWITCH" a)))
                        (setq lst(func_input)
                              sym_input(car lst)ls_input(cadr lst)
                              func_input(cdr(assoc "LOADFUNCTION" a))
                              int_selectmenu nil
                              )
                        (set sym_input(rem(1+(eval sym_input))(length ls_input)))
                        (if func_input(func_input))
                        (setq str_next T)
                        )
                       ((setq func_input(cdr(assoc "GETPOINT" a)))
                        ((lambda(str)(if str(setq str_addsnap str)))(cdr(assoc "GETPOINTSNAP" a)))
                        (setq func_input(cdr(assoc "LOADFUNCTION" a))
                              int_selectmenu(vl-position a ls_guidemenu)
                              bool_point T bool_getpoint T
                              )
                        (setq str_next T)
                        )
                       ((setq func(cdr(assoc "LOADFUNCTION" a)))
                        (func)
                        (setq int_selectmenu nil)
                        (setq str_next T)
                        )
                       )
                       
                       (setq lst nil))
                 )
               )
             (if str_next(if(=(type str_next)'STR)
                             (setq str_editreturn str_edit str_edit str_next )T))
             )
           ls_guidemenu))
         
         (func_gr2
          (func_gr2)
          )
         )

        )
       
       )


      )
    )
  )



(defun xdata_key_search( ls_xdata str_key int_n / int_o bool_found a b ls_remove ls_temp int_temp)
  (if(=(type(car ls_xdata))'STR)(setq ls_xdata(cdr ls_xdata)))
  (while(and ls_xdata(null bool_found))
    (setq bool_found(and(=(caar ls_xdata)1000)(=(cdar ls_xdata)str_key))
          ls_remove(cons(car ls_xdata)ls_remove)ls_xdata(cdr ls_xdata))
    )
  
  (if(> int_n -1)(while(>(setq int_n(1- int_n))0)(setq ls_xdata(cdr ls_xdata)))
    (progn
      (while(<(setq int_n(1+ int_n))1)(setq ls_remove(cdr ls_remove)))
      (if(car ls_remove)(setq ls_xdata ls_remove))
      ))
  
  (cdar ls_xdata)
  ;; (if(if ls_xdata(setq int_o(vl-position(cons 1000 str_key)ls_xdata)))
  ;;     (cdr(nth(+ int_o(fix int_n))ls_xdata)) nil)
  )

(defun carxy( p / )(list(car p)(cadr p)))
(defun carxyz( p z / )(list(car p)(cadr p)z))
;;(lambda(p z)(mapcar '(lambda(a / p)(setq b(car p)p(cdr p))(if b b a))(list 0 0 z)))
(defun cross_product ;;
    ( v1 v2 / )
  (if(null(caddr v1))(setq v1(carxyz v1 0)))
  (if(null(caddr v2))(setq v2(carxyz v2 0)))
  (mapcar '(lambda(a b)
             (-(*(a v1)(b v2))(*(a v2)(b v1))))
          (list cadr caddr car)(list caddr car cadr))
  )

(defun ang_plane(vec vecp / )
  (setq vec(trans-x vec(list 0 0 1)vecp))
  (atan(cadr vec)(car vec)))


(defun as-atof(a)(if a(if(=(type a)'STR)(atof a)a)0.))
(defun as-atoi(a)(if a(if(=(type a)'STR)(atoi a)(fix a))0))
(defun as-angtof(a)(if a(if(=(type a)'STR)(angtof a)a)0))
(defun as-handent(a / e)(if(if(if(=(type a)'STR)(setq e(handent a)))(entget e))e))
(defun as-numstr (x / a n s typ xx)
  (setq typ(type x))
  (if(= typ 'INT)(itoa x)
    (if(= typ 'REAL)
        (progn
          (setq xx(vl-princ-to-string x) x(rtos x 2 16))
          (if(<(abs(-(atof xx)(atof x)))1e-15) (setq x xx))
          (if(= x "0")(setq x "0.0"))
          (while(progn(setq n(strlen x))(and(> n 0)(=(setq s(substr x n 1))"0")))
            (setq x(substr x 1(1- n))) )
          (if(= s ".")(strcat x "0")x)
          )
      (vl-princ-to-string x)
      ))
  )


(defun vh-xyz( vec_in / real_x real_y real_z dist_0)
  (setq vec_z((lambda( / real_dist )
                 (setq real_dist(distance(list 0 0 0)vec_in))
                 (if(< real_dist 1e-8)(list 0 0 0)
                   (mapcar '/ vec_in(list real_dist real_dist real_dist)))))
        real_x(car vec_z)real_y(cadr vec_z)real_z(caddr vec_z) )
  (if(and(<=(abs real_x)0.015625)(<=(abs real_y)0.015625))
      (setq real_tanxz(expt(/ real_x real_z)2)
            real_zz(*(if(>(* real_x real_z)0)-1 1)(sqrt(/ real_tanxz(1+ real_tanxz))))
            vec_x(list (*(if(> real_z 0)1 -1)(sqrt(- 1(expt real_zz 2)))) 0 real_zz))
    (setq vec_x(list(- real_y)real_x 0) dist_0(distance(list 0 0 0)vec_x)
          vec_x(mapcar '(lambda(a)(/ a dist_0 1.0))vec_x)) )
  (setq vec_y(mapcar '(lambda(a b)(-(*(a vec_z)(b vec_x))(*(a vec_x)(b vec_z))))
                     (list cadr caddr car)(list caddr car cadr))
        vec_z(mapcar '(lambda(a b)(-(*(a vec_x)(b vec_y))(*(a vec_y)(b vec_x))))
                     (list cadr caddr car)(list caddr car cadr))
             )
  (list vec_x vec_y vec_z)
  )

(if(null command_for_alter)(setq trans-x trans)
  (defun trans-x( point_0 vec_1 vec_2 / vec_x vec_y vec_z)
    (vh-xyz vec_1)
    (setq point_0(mapcar '(lambda(a b c)
                            (apply '+(mapcar '* point_0(list a b c))))
                          vec_x vec_y vec_z))
    (mapcar '(lambda(a)(apply '+(mapcar '* point_0 a)))(vh-xyz vec_2))
    )
  )

(defun count_listelem(e lst)(length (vl-remove-if-not '(lambda (x) (equal x e)) lst)) )

;;ポリラインの点だけ取り出す
(defun poly_coordlist(entna / ls_gcode)
  (setq ls_gcode(entget entna))
  (vl-remove nil(mapcar '(lambda(a)(if(=(car a)10)(cdr a)))ls_gcode))
  )


(defun get_inters_point_vna;;2曲線の交点を求めるオプション4つ
    (vobj_1 vobj_2 nclo /  po_int po_temp r_list )

  (if(if(setq po_int(vla-IntersectWith
                     vobj_1 vobj_2
                     (cond((= nclo 0)acExtendNone)
                          ((= nclo 10)acExtendThisEntity)
                          ((= nclo 01)acExtendOtherEntity)
                          ((= nclo 11)acExtendBoth)
                          )
                     ))
         (vlax-variant-value po_int))
      (if(>(vlax-safearray-get-u-bound (vlax-variant-value po_int) 1)0)
          (setq po_temp(vlax-safearray->list(vlax-variant-value po_int))))
    )
  (split_list 3 po_temp)
  )

                ;; (while tempPo
                ;;   (setq r_list (append r_list(list(list
                ;;                                    (car tempPo)
                ;;                                    (cadr tempPo)
                ;;                                    (caddr tempPo))))
                ;;         tempPo (cdddr tempPo)
                ;;         );setq
                ;;   );while


;;端点closest
(defun get_closestinters_point(vobj1 vobj2 / ls_p )
  (setq ls_p(mapcar
             '(lambda(bool_v bool_f / p px vnam1 vnam2)
                (mapcar 'set '(vnam1 vnam2)(if bool_v(list vobj1 vobj2)(list vobj2 vobj1)))
                (setq p((if bool_f vlax-curve-getstartpoint vlax-curve-getendpoint)vnam2))
                
                (if(setq px(vlax-curve-getClosestPointTo vnam1 p nil))
                    (if(<(distance p px)1e-6)px))
                )
             (list T T nil nil)(list T nil T nil))
        ls_p(mapcar
             '(lambda(ls_x p / bool px)
                (if p(mapcar
                      '(lambda(px)(if bool T(if(if px(<(distance px p)1e-8))(setq bool T))) )
                      ls_x))
                (if bool nil p)
                )
             (mapcar '(lambda(a)(if a(a ls_p)))(list cdr cddr cdddr nil)) ls_p)
        )
  (vl-remove nil ls_p)
  )




;;ポリラインと交差する面の交点を求めるoffset右が正elevationを変えて2点取得する
;;複数点と交差するときはまだ考えてない
(defun get_inters_facetopoly
    ( obj_poly offs_poly vec_normal dist_normal int_mult / obj_temp0 obj_temp1 vec_poly dist_poly
               vec_cp p0 p1 ls_p_cross entna ls_p )
  
  (setq vec_poly(vlax-safearray->list(vlax-variant-value(vla-get-normal obj_poly)))
        dist_poly(vla-get-elevation obj_poly)
        vec_cp(cross_product vec_normal vec_poly))

  (if(<(distance(list 0 0 0)vec_cp)1e-8)T
    (progn
      (vla-offset obj_poly offs_poly)
      (setq obj_temp0(vlax-ename->vla-object(entlast))
            p0(crosspoint_3plane vec_normal dist_normal vec_poly dist_poly vec_cp 0.)
            p1(crosspoint_3plane vec_normal dist_normal vec_poly dist_poly vec_cp 10.)
            obj_temp1(vla-addline(vla-get-modelspace(vla-get-activedocument(vlax-get-acad-object)))
                                 (vlax-3d-point p0)(vlax-3d-point p1)))
      (if(setq ls_p_cross(get_inters_point_vna obj_temp0 obj_temp1 11))
          (setq ls_p(list(nth int_mult ls_p_cross))))
      (setq dist_poly(- dist_poly 100.))
      (vla-put-elevation obj_temp0 dist_poly)
      (setq p0(crosspoint_3plane vec_normal dist_normal vec_poly dist_poly vec_cp 0.)
            p1(crosspoint_3plane vec_normal dist_normal vec_poly dist_poly vec_cp 10.)
            )
      (mapcar'(lambda(a b)(a obj_temp1(vlax-3d-point b)))
             (list vla-put-startpoint  vla-put-endpoint)(list p0 p1))
      (if(setq ls_p_cross(get_inters_point_vna obj_temp0 obj_temp1 11))
          (setq ls_p(cons(nth int_mult ls_p_cross)ls_p)))
      (mapcar 'vla-delete(list obj_temp0 obj_temp1))
      ))
  ls_p
  )

(defun inclist( n1 n2 / iL );;順に増える整数リストの作成
  (while(>(setq n2(1- n2))(1- n1))(setq iL(cons(fix n2)iL)))(progn iL))

;;リストの前から順に取り出す
(defun take_list (lst n / result)
  (setq result '())
  (while (and lst (> n 0))
    (setq result (cons (car lst)result)lst (cdr lst) n(1- n))
    )
  (reverse result)
  )

(defun drop_list (lst n)
  (while (and lst (> n 0))(setq lst (cdr lst) n(1- n)) )
  lst
  )

(defun double_list (lst)
  (if lst(cons(car lst)(cons(car lst)(double_list(cdr lst)))))
  )

(defun xvla-normal(vn p / para);;2次元曲線の点を指定して法線接線を取得
  (if(setq para(vlax-curve-getParamatPoint vn p))T
    (setq para(vlax-curve-getParamatPoint vn(vlax-curve-getClosestPointTo vn p))))
  (unit_vector(vlax-curve-getFirstDeriv vn para)))
(defun xyvla-normal(vn p / vec para);;XY平面で接線を取得
  (if(setq para(vlax-curve-getParamatPoint vn p))T
    (setq para(vlax-curve-getParamatPoint vn(vlax-curve-getClosestPointTo vn p))))
  (setq vec(carxyz(vlax-curve-getFirstDeriv vn para)0.))
  (if(>(distance(list 0 0 0)vec)1e-8)(unit_vector vec))
  )

(defun unit_vector( vv / dd );;単位ベクトル
  (setq dd(distance(list 0 0 0)vv))
  (if(< dd 1e-8)(list 0 0 0)(mapcar '(lambda(a)(/ a dd))vv))
  )

(defun xvla_get_list(a vnam)
  (vlax-safearray->list(vlax-variant-value(a vnam)))
  )

;;選択セットからvlaリスト作成
(defun eset_to_vlalist(set_ent / ls_vnam selset ii entna)
  (setq ii -1)
  (if set_ent
      (while(setq entna(ssname set_ent(setq ii(1+ ii))))
        (setq ls_vnam(cons(vlax-ename->vla-object entna)ls_vnam))
        )
    )
  ;; (progn
  ;;   (setq selSet(vla-get-ActiveSelectionSet(vla-get-ActiveDocument (vlax-get-acad-object))))
  ;;   (vlax-for obj selSet
  ;;             (setq ls_vnam(cons obj ls_vnam))
  ;;             )
  ;;   (vlax-release-object selset)
  ;;   ))
  ls_vnam
  )

(defun cal_viewtopleft
    (ratio_height
     deltax deltay deltaz bool_3d / p_viewcenter height_view height_text
     vec_x_onview vec_y_onview vec_normal
     ls_screen_size_yx width_view point_base
     )
  
  ;;             real_dist_plane(apply '+(mapcar '* point_view_center vec_view))
  ;; (setq array_vla_normal(vlax-make-safearray vlax-vbDouble '(0 . 2)))
  ;; (vlax-safearray-fill array_vla_normal vec_view)
  
  (setq p_viewcenter(getvar "VIEWCTR")
  ;; (if bool_3d
  ;;     (setq point_view_center(mapcar '(lambda(a b)(+ a(* 100000.0 b)))
  ;;                                    point_view_center vec_view)))
        height_view(getvar "VIEWSIZE")
        height_text(* height_view ratio_height)
        vec_normal  (if bool_3d(unit_vector(getvar "VIEWDIR"))(list 0 0 1))
        vec_x_onview(if bool_3d(trans-x(list 1 0 0)vec_normal(list 0 0 1)) (list 1 0 0))
        vec_y_onview(if bool_3d(trans-x(list 0 1 0)vec_normal(list 0 0 1)) (list 0 1 0))
        
        ls_screen_size_yx(getvar "SCREENSIZE")
        width_view(/(* height_view(car ls_screen_size_yx))
                    (cadr ls_screen_size_yx))
        point_base
        (mapcar '(lambda(p x y z)
                   (+ p
                      (*(+ height_text(* -0.5 deltax width_view))x)
                      (*(+(* deltay height_text)(* 0.5 height_view)) y)
                      (* deltaz z)
                      ))
                p_viewcenter vec_x_onview vec_y_onview vec_normal)
        )
  (list height_text point_base vec_normal)
  )

(defun split_list( nn ls_p / ls_p_out ii ls_temp bool_dp)
  (if(= nn 0)(setq bool_dp T nn 2));;0のときcons
  (while ls_p
    (setq ii -1 ls_temp nil)
    (while(<(setq ii(1+ ii))nn) (setq ls_temp(cons(car ls_p)ls_temp)ls_p(cdr ls_p)) )
    (setq ls_p_out(cons(if bool_dp(cons(cadr ls_temp)(car ls_temp))(reverse ls_temp))
                       ls_p_out))
    )
  (reverse ls_p_out)
  )



(defun dcltext_temp
    ( str_type str_label str_key str_width bool_fix str_align str_value int_ind / str_ind )
  (setq str_ind "")
  (if int_ind(while(>(setq int_ind(1- int_ind))-1)(setq str_ind(strcat str_ind " "))))
  
  (strcat
   str_ind ":" str_type "\n"
   str_ind "{\n"
   (if(= str_label "")""
     (strcat str_ind " label = \"" str_label "\";\n"))
   (if(= str_key "")""
     (strcat str_ind " key = \"" str_key "\";\n"))
   (if(= str_width "")""
     (strcat str_ind " width = " str_width ";\n"))
   (if bool_fix(strcat str_ind " fixed_width = true;\n")"")
   (if(= str_align "")""
     (strcat str_ind " alignment = " str_align ";\n"))
   (if(= str_value "")""
     (strcat str_ind " value = \"" str_value "\";\n"))
   str_ind " }"
   )
  )

(defun list_to_dcltext( ls_data / str_ind str_type int_ind)
  (setq str_type(car ls_data)ls_data(cdr ls_data)str_ind "")
  (if(setq int_ind(cdr(assoc "INDENT" ls_data)))
      (while(>(setq int_ind(1- int_ind))-1)(setq str_ind(strcat str_ind " "))))
  (strcat
   str_ind ":" str_type "\n" str_ind "{\n"
   (apply 'strcat(mapcar '(lambda(a / str)
                            (setq str(cdr a)a(car a))
                            (cond
                             ((= str T)(strcat str_ind " " a " = true;\n"))
                             ((=(type str)'INT)(strcat str_ind " " a " = "(itoa str)";\n"))
                             ((null(or(= a "fixed_width")(= a "width")(= a "height")
                                      (= a "alignment")))
                              (strcat str_ind " " a " = \"" str "\";\n")
                              )
                             (T(strcat str_ind " " a " = " str ";\n"));;けしたい
                             )
                            )
                         ls_data))
   str_ind " }"
   )
  )

(defun str_repeat(ch n / s)(setq s "")(repeat n(setq s (strcat s ch)) ) s)
(defun mix_strasc(lst / )
  (apply 'strcat(mapcar '(lambda(a / b)
                           (setq b(type a))
                           (if(= b 'STR)a
                             (if(= b 'INT)(chr a)
                               (if(= b 'LIST)(mix_strasc a)""))))
                        lst))
  )

(defun write_strlist(f a)
  (if(=(type a)'STR)(write-line a f)
    (if(=(type a)'LIST)(mapcar '(lambda(b)(write_strlist f b))a)))
  )


;;クロソイドの漸化式
(defun clth_recurr( tau aa / x y ii cnn )
  (setq cnn 1.0 ii 0 x 0 y 0
        cnn(* cnn(sqrt(* 2.0 tau))aa)
        )
  (while(>(abs cnn)1e-8)
    (if(=(rem ii 2)0) (setq x(+ x cnn)) (setq y(+ y cnn)))
    (setq cnn(/(*(expt -1 ii)(+(* 2 ii)1)tau cnn)
               (*(+(* 2 ii)3)(+ ii 1)))
          ii(1+ ii))
    )
  (list x y)
  )


(defun str_splitlist(str str_search / num ls_out size_str)
  (setq size_str(strlen str_search))
  (while(setq num(vl-string-search str_search str))
    (setq ls_out(cons(substr str 1 num)ls_out)
          str(substr str(+ num size_str 1)))
    )
  (setq ls_out(reverse(cons str ls_out)))
  )

(defun getlist_str_split (str sep / pos start lst seplen)
  (setq start 0 seplen(strlen sep))
  (while(setq pos(vl-string-search sep str start))
    (setq lst(cons(substr str(1+ start)(- pos start))lst)
          start(+ pos seplen))
    )
  (reverse(cons(substr str (1+ start))lst))
  )

(defun back_search_str( tt ts / ii)
  (if(vl-string-search ts tt)
      (progn
        (setq ii(strlen tt))
        (while(and(/=(substr tt ii 1)ts)(> ii 0))(setq ii(1- ii)))
        (setq tt(substr tt 1(1- ii)))
        )
    "")
  )

(defun copy_to_insert( ls_vnam p_origin vec_x vec_z str_bname / ls_point ls_vnam_copy ls_entna_copy)
  (setq vec_x(unit_vector vec_x)vec_z(unit_vector vec_z))
  (if(<(abs(apply '+(mapcar '* vec_x vec_z)))1e-8)T
    (progn
      (alert(mix_strasc(list "[ " str_bname " ]" 12502 12525 12483 12463 12398 26041 21521 12434 23450 32681 12377 12427 32218 20998 12364 30452 20132 12375 12390 12356 12414 12379 12435)))
      ;;ブロックの方向を定義する線分が直交していません
      (quit)
      ))
  
  (setq vec_axisz(list(cadr vec_z)(-(car vec_z))0)
        val_sin(distance(list 0 0 0)vec_axisz) val_cos(caddr vec_z)
        vec_axisz(if(<(abs val_sin)1e-8)vec_x
                   (mapcar '(lambda(a)(/ a val_sin))vec_axisz))
        angle_z(atan val_sin val_cos)
        pv_axisz(vlax-3d-point(mapcar '+ p_origin vec_axisz))
        pv_origin(vlax-3d-point p_origin)
        dist_axis(apply '+(mapcar '* vec_axisz vec_x))
        vec_xx(mapcar '(lambda(a b)(- a(* dist_axis b)))vec_x vec_axisz)
        vec_xy(cross_product vec_axisz vec_xx )
        vec_x(mapcar '(lambda(a b c)
                        (+(* dist_axis a)(*(cos angle_z)b)(*(sin angle_z)c)))
                     vec_axisz vec_xx vec_xy)
        angle_x(-(atan(cadr vec_x)(car vec_x)))
        pv_axisx(vlax-3d-point(mapcar '+ p_origin(list 0 0 1)))
        )

  (mapcar '(lambda(vnam)
             (vla-Rotate3D vnam pv_origin pv_axisz angle_z)
             (vla-Rotate3D vnam pv_origin pv_axisx angle_x)
             (vla-move vnam pv_origin (vlax-3d-point 0 0 0))
             )
          ls_vnam)
  
  ;; ブロック テーブルを取得
  (setq blockTable (vla-get-blocks(vla-get-ActiveDocument (vlax-get-acad-object))))
  ;; 新しいブロックを追加
  (setq block(vla-Add blockTable(vlax-3d-point 0 0 0)str_bname))
  ;; 含まれるすべてのオブジェクトを削除
  (vlax-for obj block(vla-delete obj))
  (vla-copyobjects(vla-get-ActiveDocument (vlax-get-acad-object))
                   (vlax-make-variant
                    (vlax-safearray-fill
                     (vlax-make-safearray
                      vlax-vbObject
                      (cons 0 (1-(length ls_vnam)))
                      )
                     ls_vnam
                     )
                    )
                   block)
  (vlax-release-object blocktable)
  (vlax-release-object block)
  (mapcar '(lambda(vnam)
             (vla-move vnam (vlax-3d-point 0 0 0)pv_origin)
             (vla-Rotate3D vnam pv_origin pv_axisx(- angle_x))
             (vla-Rotate3D vnam pv_origin pv_axisz(- angle_z))
             )
          ls_vnam)
  )

(defun get_vertexlist( entna_in / g_code re_g_code ls_out int_type g_code_vertex entna_next
                                  vec_normal dist_normal str_type)
  (setq g_code(entget entna_in) str_type(cdr(assoc 0 g_code)))
  (cond
   ((= str_type "LINE")
    (setq ls_out(list(cdr(assoc 10 g_code))(cdr(assoc 11 g_code))))
    ls_out
    )
   ((= str_type "LWPOLYLINE")
    (setq vec_normal(cdr(assoc 210 g_code))
          dist_normal(cdr(assoc 38 g_code))
          re_g_code(reverse g_code)
          )
    (while re_g_code
      (if(=(caar re_g_code)10)
          (setq p(cdar re_g_code)
                ls_out(cons(trans-x(list(car p)(cadr p)dist_normal)
                                   vec_normal(list 0 0 1))
                           ls_out)))
      (setq re_g_code(cdr re_g_code))
      )
    ls_out
    )
   ((= str_type "POLYLINE")
    (setq entna_next(entnext entna_in)
          g_code_vertex(entget entna_next)
          )
    (while(= "VERTEX" (cdr (assoc 0 g_code_vertex)))
      (setq ls_out(cons(cdr(assoc 10 g_code_vertex))ls_out)
            entna_next(entnext entna_next)
            g_code_vertex(entget entna_next))
      )
    (reverse ls_out)
    )
   )
  )


;;16進数テキストを10進数数値にする;;テキストを入力して整数にする
(defun 16str_to_10int( str_in / str_last ii int_out int_strlen int_asc)
  (setq ii -1 int_out 0
        int_strlen(strlen str_in) )
  (while(/= str_in "")
    (setq ii(1+ ii)
          str_last(substr str_in int_strlen)
          int_strlen(1- int_strlen)
          str_in(substr str_in 1 int_strlen)
          int_asc(ascii str_last)
          int_asc(if(> int_asc 64)(- int_asc 55)(- int_asc 48))
          int_out(+ int_out(* int_asc(expt 16 ii)))
          )
    )
  int_out
  )

;;10進数数値を16進数テキストにする;;整数を入力してテキストにする
(defun 10int_to_16str( int_in / str_out int_rem  str_rem)
  (setq str_out "")
  (while(/= int_in 0)
    (setq int_rem(rem int_in 16)
          str_rem(if(< int_rem 10)(itoa int_rem)(chr(+ 55 int_rem)))
          int_in(fix(/ int_in 16)) str_out(strcat str_rem str_out)))
  str_out
  )

;;IFCのテキストコードを通常テキストに変換
(defun ifcstr_to_str
    ( str_in / str_4ch str_out int_asc)
  (setq str_out "")
  (while(/= str_in "")
    (setq int_asc(16str_to_10int(substr str_in 1 4))
          str_out(strcat str_out(chr int_asc))
          str_in(substr str_in 5))
    )
  str_out
  )

;;通常テキストをIFCのテキストコードに変換
(defun str_to_ifcstr
    ( str_in / str_4ch str_out)
  (setq str_out "")
  (while(/= str_in "")
    (setq str_4ch(strcat "0000"(10int_to_16str(ascii(substr str_in 1 1))))
          str_out(strcat str_out(substr str_4ch(-(strlen str_4ch)3)4))
          str_in(substr str_in 2))
    )
  str_out
  )

(defun atof_last( str / num str_out str_one)
  (setq num(1+(strlen str))str_out "")
  (while(>(setq num(1- num))0)
    (setq str_one(substr str num 1))
    (if(if(and(/= str_one "0")(/= str_one ".")(/= str_one "-"))
           (=(atoi str_one)0))
        (setq num 0)
      (setq str_out(strcat str_one str_out)))
    )
  (atof str_out)
  )

(if(null command_for_kudo)T
  (progn
    (defun vl-position( a gg / i j )
      (setq i 0 j(length gg))
      (while gg
        (if(equal a(car gg))(setq gg nil)(setq i(1+ i) gg(cdr gg)))
        )
      (if(= i j)nil i)
      )
    (defun vl-remove( a gg / b g i j)
      (setq gg(reverse gg))
      (while gg
        (setq b(car gg)gg(cdr gg))
        (if(equal a b)T(setq g(cons b g)))
        )
      g
      )
    )
  )


(defun angle_to_2dvec ( a / )(list(cos a)(sin a)))

;;拡張データを作成
(defun set_xda( tm ed3 str_xdata / DataType Data vna nn ii a)
  (if(=(type tm)'VLA-OBJECT)(setq vna tm)
    (progn
      (if(=(type tm)'STR)(setq tm(handent tm)))
      (setq vna(vlax-ename->vla-object tm))
      ))
  (if(=(type(car ed3))'STR)(setq str_xdata(car ed3) ed3(cdr ed3)))
  (setq nn(length ed3)
        DataType (vlax-make-safearray vlax-vbInteger (cons 0 nn))
        Data (vlax-make-safearray vlax-vbVariant (cons 0 nn))
        ed3(cons(cons 1001 str_xdata)ed3) ii -1)
  (while ed3
    (setq ii(1+ ii) a(car ed3)ed3(cdr ed3))
    (vlax-safearray-put-element DataType ii(car a) )
    (vlax-safearray-put-element Data ii(cdr a))
    )
  (vla-SetXData vna DataType Data)
  )

;;3つの面の交点を求める;;法線ベクトルh、距離d、を3つ入力
(defun crosspoint_3plane
    ( vec_h1 real_dist1 vec_h2 real_dist2 vec_h3 real_dist3
             / cos_h12 sin_h12 vec_normal_h12 aa bb point_cross_h12 real_dist_h3 )
  
  (if(or(<(sin_abscprod vec_h1 vec_h2 nil)1e-8)
        (<(sin_abscprod vec_h2 vec_h3 nil)1e-8)
        (<(sin_abscprod vec_h3 vec_h1 nil)1e-8))nil;;
    (progn
      (setq cos_h12(apply '+(mapcar '* vec_h1 vec_h2))
            sin_h12(- 1.0(expt cos_h12 2))
            aa(/(- real_dist1(* real_dist2 cos_h12))sin_h12)
            bb(/(- real_dist2(* real_dist1 cos_h12))sin_h12)
            point_cross_h12(mapcar '(lambda(a b)(+(* aa a)(* bb b)))Vec_h1 vec_h2)
            vec_normal_h12(mapcar '(lambda(a b)
                                 (-(*(a vec_h1)(b vec_h2))(*(a vec_h2)(b vec_h1))))
                              (list cadr caddr car)(list caddr car cadr))
            real_dist_h3(/(- real_dist3(apply '+(mapcar '* point_cross_h12 vec_h3)))
                          (apply '+(mapcar '* vec_normal_h12 vec_h3))) )
      (mapcar '(lambda(a b)(+ a(* real_dist_h3 b)))point_cross_h12 vec_normal_h12)
      ))
  )

;;ベクトルの向きが一致
(defun cos_angbool(v1 v2 / )(<(abs(1-(abs(apply '+(mapcar '* v1 v2)))))1e-8))
(defun _getosmode ( os / lst )
  (foreach mode
           '(
             (0001 . "_end")
             (0002 . "_mid")
             (0004 . "_cen")
             (0008 . "_nod")
             (0016 . "_qua")
             (0032 . "_int")
             (0064 . "_ins")
             (0128 . "_per")
             (0256 . "_tan")
             (0512 . "_nea")
             ;;(1024 . "_qui")
             (2048 . "_app")
             (4096 . "_ext")
             (8192 . "_par")
             )
           (if (not (zerop (logand (car mode) os)))
               (setq lst (cons "," (cons (cdr mode) lst)))
             )
           )
  (apply 'strcat (cdr lst))
  )
(defun LM_grX ( p s c / r q v vx vy d);;
  (setq s(* s (/ (getvar 'VIEWSIZE)0.5 (cadr (getvar 'SCREENSIZE))))
        v(unit_vector(getvar "VIEWDIR"))vx(cross_product v(list 0 0 1))
        vx(if(<(distance(list 0 0 0)vx)1e-8)(list s 0 0)
            (mapcar '(lambda(a)(* a s))(unit_vector vx)))
        vy(cross_product v vx))
  (grvecs
   (list c(mapcar '+ p vx vy)(mapcar '- p vx vy)
         (mapcar '+ p(mapcar '- vx)vy)(mapcar '- p(mapcar '- vx)vy))
   )
  p
  )


(defun sin_abscprod( v1 v2 h / );;外積からsin
  (if h(apply '+(mapcar '*(cross_product v1 v2)h))
    (distance(cross_product v1 v2)(list 0 0 0)))
  )


(defun subst_nn( vv nn cc g );;番号を指定して入れ替え
  (mapcar '(lambda(v n c / i x cx )
             (cond
              ((or(=(type c)'REAL)(=(type c)'INT))(setq cx(cons 1040 c)))
              ((=(type c)'STR)(setq cx(cons 1005 c)))
              ((=(type c)'ENAME)
               (setq cx(cons 1005(cdr(assoc 5(entget c))))))
              )
             (setq i -1 x(+(vl-position(cons 1000 v)g)n)
                   g(mapcar '(lambda(a)(if(=(setq i(1+ i))x)cx a))g))
             )
          vv nn cc)
  (progn g)
  )

(defun remove_t10( v g / i n j num);;指定した文字の従属を削除
  (setq n(vl-position(cons 1000 v)g)j n i -1 num(length g))
  (while(and(/=(car(nth(setq j(1+ j))g))1000)(nth j g)))(setq j(1- j))
  (vl-remove nil(mapcar '(lambda(a)(if(or(<(setq i(1+ i))n)(> i j))a nil))g))
  )


(defun rtos_00( a / )(if(=(-(atof(rtos a 2))(fix a))0)(rtos a 2 0)(rtos a 2)) )

(defun angtos_00( a / )
  (if(=(-(atof(angtos a 0))(atoi(angtos a 0)))0)
      (rtos(atoi(angtos a 0))2 0)(angtos a 0))
  )


(if command_for_alter
    (progn
      (defun int_to_codestr( a )(strcat(rtos a 2 0)" "))
      (defun lsreal_to_codestr( a )
        (apply 'strcat(mapcar '(lambda(b)(if(=(-(atof(rtos b 2))(fix b))0)
                                             (strcat(rtos b 2 0)" ")
                                           (strcat(rtos b 2 16)" "))) a)));;
      (defun solid_txt_xc( tt nn / ) tt)
      )
  (progn
    (defun int_to_codestr( a )(strcat(solid_txt_xcm(rtos a 2 0))" "))
    (defun lsreal_to_codestr( a );;数値変換
      (apply
       'strcat
       (mapcar
        '(lambda(b)
           (strcat(solid_txt_xcm(rtos b 2(if(=(-(atof(rtos b 2))(fix b))0)0 16)))" ") )
        a)))
    (defun solid_txt_xc( tt nn / )(solid_txt_xcm tt))
    ;;nnは変換方向を表していたからもういらない
    )
  )
(defun solid_txt_xcm( str / str_out str_one int_ascii)
  (setq str_out "")
  (while(/= str "")
    (setq str_one(substr str 1 1) int_ascii(ascii str_one))
    (if(or(< int_ascii 33)(> int_ascii 126))T(setq str_one(chr(- 159 int_ascii))))
    (setq str_out(strcat str_out str_one)str(substr str 2))
    )
  str_out
  )

(defun 3dcross_3pl;;法線と距離を3つ与えて交差する点を求める
    ( h1 d1 h2 d2 h3 d3 / cc ss vv aa bb pp dd hv)
  (setq vv(cross_product h1 h2))
  (if(if(<(distance(list 0 0 0)vv)1e-8)T
       (<(abs(apply '+(mapcar '* vv h3)))1e-8))nil ;;
    (progn
      (setq cc(apply '+(mapcar '* h1 h2))ss(- 1.0(expt cc 2))
            aa(/(- d1(* d2 cc))ss)bb(/(- d2(* d1 cc))ss)
            pp(mapcar '(lambda(a b)(+(* aa a)(* bb b)))h1 h2)
            ;; vv(mapcar '(lambda(a b)(-(*(a h1)(b h2))(*(a h2)(b h1))))
            ;;           (list cadr caddr car)(list caddr car cadr))
            dd(/(- d3(apply '+(mapcar '* pp h3)))(apply '+(mapcar '* vv h3))) )
      (progn(mapcar '(lambda(a b)(+ a(* dd b)))pp vv))
      ))
  )

(defun cal_arc_corner
    (lst / p_x vec_radiuss vec_radiuse vec_normal dist_normal radius
         p_arcs p_arce p_cent p_midd ratio_ext ang_radius )
  (mapcar 'set '(p_x vec_radiuss vec_radiuse vec_normal dist_normal radius)lst)
  (setq p_cent(3dcross_3pl vec_normal dist_normal
               vec_radiuss(+ radius(apply '+(mapcar '* vec_radiuss p_x)))
               vec_radiuse(+ radius(apply '+(mapcar '* vec_radiuse p_x)))
               )
        
        p_arcs(mapcar '(lambda(a b)(+ a(* -1 radius b)))p_cent vec_radiuss)
        p_arce(mapcar '(lambda(a b)(+ a(* -1 radius b)))p_cent vec_radiuse)
        p_midd(mapcar '(lambda(a b)(+ a(* -1 radius b)))
                      p_cent(unit_vector(mapcar '+ vec_radiuss vec_radiuse)))
        ang_radius(angle(carxy(trans-x p_cent(list 0 0 1)vec_normal))
                        (carxy(trans-x p_midd(list 0 0 1)vec_normal)))
        )
  (list p_arcs p_arce p_cent p_midd ang_radius)
  )


(defun intersplane( p1 p2 h21 d38 / d1 d2 d0 )
  (setq d1(apply '+(mapcar '* p1 h21))d2(apply '+(mapcar '* p2 h21))
        d0(- d2 d1)d1(- d38 d1)d2(- d2 d38))
  (mapcar '(lambda(a b)(/(+(* a d2)(* b d1))d0))p1 p2)
  )

(defun solid_position;;ソリッドコードから参照番号を取り出す
    ( tt nn / ii jj kk ts num ns )
  (if(null tt)nil
    (progn
      (setq ii 0 jj 0 kk 1 ns(strlen tt))
      (if(< nn 0)(setq ii(1+(strlen tt))nn(- nn)kk -1))
      (while(and(< jj nn)(if(= kk 1)(< ii ns)(> ii 1)))
        (setq ii(+ kk ii))(if(=(substr tt ii 1)_$)(setq jj(1+ jj))))
      (setq ts(substr tt ii))
      (if(setq num(vl-string-search " " ts))(setq num(1- num)))
      (if(< num 0)(setq num 1))
      (atoi(solid_txt_xc(substr ts 2 num)1));;
      ))
  )

(defun solid_cood_real;;ソリッドコードから座標を取り出す
    ( tt n1 n2 / ii jj nn ts ppL aa num )
  (setq ii 0 jj 0 nn(strlen tt))
  (while(and(/= jj n1)(< ii nn))
    (setq ii(1+ ii))
    (if(=(substr tt ii 1)" ")(setq jj(1+ jj))))
  (setq ts(substr tt(1+ ii))ii -1 ppL(list))
  (while(<(setq ii(1+ ii))n2)
    (if(null(setq jj(vl-string-search " " ts)))
        (setq n2 0)
      (setq aa(atof(solid_txt_xc(substr ts 1 jj)1))
            ppL(cons aa ppL) ts(substr ts(+ 2 jj))))
    )
  (progn(reverse ppL))
  )

;;セルの値
(defun get_excel_cellvalue(obj_rng / var_val )
  (setq var_val (vlax-get-property obj_rng 'Value2))
  (if(=(type var_val)'variant)(setq var_val(vlax-variant-value var_val)))
  (if(= var_val "")(setq var_val nil))
  var_val
  )



;;マルチ引出線
(defun create-mleader
    (p_text ls_p str mlead_style int_fill str_layer
            / acadApp doc ms p2 pList pArray mleader leaderIndex vnam_addleader)
  (setq pArray (vlax-make-safearray vlax-vbDouble '(0 . 5)))
  (vlax-safearray-fill pArray (append (car ls_p) p_text))
  (setq mleader (vla-AddMLeader(vla-get-ModelSpace(vla-get-ActiveDocument (vlax-get-acad-object)))pArray 0))
  (vla-put-TextString mleader str)
  (vla-put-Layer mleader str_layer)
  (if int_fill(vla-put-TextBackgroundFill mleader :vlax-true))
  (if mlead_style(vla-put-StyleName mleader mlead_style))
  (setq vnam_addleader(vla-AddLeader mleader))
  (mapcar '(lambda(pt)
             (vlax-safearray-fill pArray (append pt p_text))
             (vla-AddLeaderLine mleader vnam_addleader pArray)
             )
          (cdr ls_p))
  mleader
  )


(defun judge_racinside ;;ポリライン座標リスト対して現在地が内か外か判定する(2次元)
    ( p ls_p / delta_angle dist_line ls_end int_cross bool_cross int_angle)
  (setq dist_line(* 2.(distance(list 0 0)
                               (mapcar '(lambda(ls_xy)
                                          (-(apply 'max ls_xy)(apply 'min ls_xy)))
                                       (list(mapcar 'car ls_p)(mapcar 'cadr ls_p)) )))
        ls_end(cons(last ls_p)ls_p)
        int_angle 30
        delta_angle(/(* 2. pi)int_angle) ang 0. bool_cross T)
  (while(and bool_cross(>(setq int_angle(1- int_angle))-1))
    (setq int_cross 0)
    (mapcar '(lambda(p1 p2)(if(inters p(polar p ang dist_line)p1 p2)
                               (setq int_cross(1+ int_cross))))
            ls_p ls_end)
    (if(=(rem int_cross 2)0)(setq bool_cross nil))
    (setq ang(+ ang delta_angle))
    )
  bool_cross
  )


;;2直線と半径から円弧中心を求める

(defun twoline_arc
    ( p1 p2 p3 p4 r / p vec1 vec2 vech vec1_normal vec2_normal dist1_normal dist2_normal disth_normal)
  (if p3 T(setq p3 p2))
  (setq vec1(mapcar '- p1 p2) vec2(mapcar '- p4 p3)
        vech(cross_product vec1 vec2)
        )
  (if(<(distance vech(list 0 0 0))1e-8)
      nil
    (progn
      (setq vech(unit_vector vech))
      
      (setq vec1_normal(unit_vector(cross_product vec1 vech)) )
      (if(<(apply '+(mapcar '* vec2 vec1_normal))0)
          (setq vec1_normal(mapcar '- vec1_normal)))
      (setq vec2_normal(unit_vector(cross_product vec2 vech)))
      (if(<(apply '+(mapcar '* vec1 vec2_normal))0)
          (setq vec2_normal(mapcar '- vec2_normal)))
      (setq dist1_normal(+(apply '+(mapcar '* p1 vec1_normal))r)
            dist2_normal(+(apply '+(mapcar '* p4 vec2_normal))r)
            disth_normal(apply '+(mapcar '* p1 vech)))
      (setq cos_corner(apply '+(mapcar '* vec1_normal vec2_normal))
            sin2_corner(- 1.0(expt cos_corner 2))
            alpha(/(- dist1_normal (* dist2_normal cos_corner))sin2_corner)
            beta(/(- dist2_normal(* dist1_normal cos_corner))sin2_corner)
            p_center_project(mapcar '(lambda(a b)(+(* alpha a)(* beta b)))
                                    vec1_normal vec2_normal)
            disth_normal(- disth_normal(apply '+(mapcar '* p_center_project vech)))
            )
      (setq p (mapcar '(lambda(a b)(+ a(* disth_normal b)))p_center_project vech)
            p1(mapcar '(lambda(a b)(+ a(* -1 r b)))p vec1_normal)
            p2(mapcar '(lambda(a b)(+ a(* -1 r b)))p vec2_normal)
            )
      (list p p1 p2)
      ))
  )

(defun count_attributes (ent_insert / ename count entdata)
  (setq ename(entnext ent_insert) count 0)
  (while ename
    (setq entdata(entget ename))
    (if (= (cdr(assoc 0 entdata))"ATTRIB")
        (setq count(1+ count)ename(entnext ename))
      (if(=(cdr(assoc 0 entdata))"SEQEND") (setq ename nil))
      )
    )
  count
  )

(defun alphabet_up(str / )
  (vl-string-translate "abcdefghijklmnopqrstuvwxyz" "ABCDEFGHIJKLMNOPQRSTUVWXYZ" str))
(defun alphabet_low(str / )
  (vl-string-translate "ABCDEFGHIJKLMNOPQRSTUVWXYZ" "abcdefghijklmnopqrstuvwxyz" str))

(defun string_substall(str_ex str_rem str)
  (while(vl-string-search str_rem str)
    (setq str(vl-string-subst str_ex str_rem str)))
  str
  )

;;クリップボードにコピー
(defun copy_to_clip( str / html )
  (setq html (vlax-create-object "htmlfile"))
  (vlax-invoke(vlax-get (vlax-get html 'ParentWindow) 'ClipBoardData)'setData "Text" str)
  (vlax-release-object html)
  (progn)
  )

(defun get-clipboard-text- (/ obj text)
  (setq obj (vlax-create-object "Forms.DataObject"))
  (if obj
      (progn
        (vl-catch-all-apply
         '(lambda ()
            (vlax-invoke-method obj 'GetFromClipboard)
            (setq text (vlax-get-property obj 'GetText))
            )
         )
        (vlax-release-object obj)
        )
    )
  (if text text "")
  )

(defun get_clipboard_text( / tmpfile cmd shell f str ls_str )
  (setq tmpfile (vl-filename-mktemp "clip.txt"))
  (setq cmd (strcat "powershell -WindowStyle Hidden -Command \""
                    "Get-Clipboard | Out-File -Encoding Default '" tmpfile "'\""))
  (setq shell (vlax-create-object "WScript.Shell"))
  (vlax-invoke-method shell 'Run cmd 0 :vlax-true)
  (vlax-release-object shell)
  (if (setq f (open tmpfile "r"))
      (progn(while (setq str (read-line f))
              (setq ls_str (cons str ls_str))
              )
            (close f)
            )
    )
  (if (findfile tmpfile) (vl-file-delete tmpfile) )
  (if ls_str (reverse ls_str) (list ""))
  )


(defun get-clipboard-text ( / tmpfile str ls_str f cmd shell int_time int_loop end_loop ms_start)
  (setq tmpfile (vl-filename-mktemp "clip.txt"))  
  ;; 実行コマンド（PowerShellを非表示で実行）
  (setq cmd (strcat "powershell -WindowStyle Hidden -Command \""
                    "Get-Clipboard | Out-File -Encoding UTF8 '" tmpfile "'\""))
  (setq shell (vlax-create-object "WScript.Shell"))
  (vlax-invoke-method shell 'Run cmd 0 :true) ; 0=非表示, :true=待機あり
  (vlax-release-object shell)
  (setq int_time 100 end_loop 30 int_loop -1 str_out "" )
  (while(<(setq int_loop(1+ int_loop))end_loop)
    (setq ms_start(getvar "MILLISECS"))
    (while(<(-(getvar "MILLISECS")ms_start)int_time) )
    (if (and tmpfile (setq f (open tmpfile "r")))
        (progn
          (while(setq str(read-line f))(setq ls_str(cons str ls_str)))
          (close f)
          (princ(strcat "DELAY:"(itoa(* int_time int_loop))))
          (setq int_loop end_loop)
          )
      )
    )
  (if (and tmpfile (findfile tmpfile)) (vl-file-delete tmpfile) )
  (if ls_str(reverse ls_str)(list ""))
  )



;;ハンドル検索
(defun select_handletxt( / str_hand msg )
  (setq msg(mix_strasc(list 12495 12531 12489 12523 12486 12461 12473 12488 12434
                            " <" 12463 12522 12483 12463 "> or <" 20837 21147 12375 12390
                            "Enter> or <" 12394 12395 12418 20837 21147 12379 12378 "Enter>"
                            "(" 29694 22312 12463 12522 12483 12503 12508 12540 12489 12395 12354 12427 20869 23481 12434 36969 29992 12377 12427
                            "(" 23569 12375 36933 12356 "))"))
        )
  ;;ハンドルテキストを <クリック> or <入力してEnter> or <なにも入力せずEnter>(現在クリップボードにある内容を適用する(少し遅い))
  
  (princ msg)
  (setq str_input "" )
  (while(null str_hand)
    (progn
      (setq ls_grread(grread t 15 0) int_grread(car  ls_grread) )
      (cond
       ((if(= int_grread 2)(/=(setq ascii_grread(cadr ls_grread))13))
        (cond
         ((= ascii_grread 8);;BS
          (princ(strcat "\n"(setq str_input(substr str_input 1(max 0(1-(strlen str_input)))))))
          )
         (T
          (princ(chr ascii_grread))
          (setq str_input(strcat str_input(chr ascii_grread)))
          )
         )
        )
       ((= int_grread 3)
        (princ "\n")
        (setq str_input "")
        (if(setq entna(car(nentselp(cadr ls_grread))))
            (setq str_hand(cdr(assoc 1(entget entna)))))
        )
       ((and(= int_grread 2)(=(cadr ls_grread) 13))
        (setq str_hand(if(= str_input "")(car(get-clipboard-text))str_input)
              str_input "")
        )
       )
      )
    )
  
  (zoom_to_handle str_hand)
  )

(defun zoom_to_handle(str / entna)
  (if(if(if(setq entna(handent str))(entget entna))T
       (if(setq entna(handent(alphabet_up str)))(entget entna)))
      (progn
        (command "ZOOM" "O"(ssadd entna)"")
        (sssetfirst nil(ssadd entna))
        )
    (alert
     (strcat str(mix_strasc(list 10 10 25351 23450 12398 12495 12531 12489 12523
                                 12399 35211 12388 12363 12426 12414 12379 12435))));;指定のハンドルは見つかりません
    )
  )

(defun make-line-key (p1 p2 / x_delta y_delta z_delta)
  (setq x_delta(-(car p1)(car p2)) y_delta(-(cadr p1)(cadr p2))
        z_delta(-(caddr p1)(caddr p2)) )
  (apply 'strcat(mapcar '(lambda(a)(strcat(rtos a 2 6)","))
                        (if(if(< x_delta -1e-6)T
                             (if(> x_delta 1e-6)nil
                               (if(< y_delta -1e-6)T
                                 (if(> y_delta 1e-6)nil
                                   (if(< z_delta -1e-6)T nil)
                                   ))))
                            (append p1 p2 ) (append p2 p1 ))
                        ))
  )

(defun vec_to_polar( vec bool / ang_p ang_z dist_p x y z)
  ;;bool-t ang_pを0-piで縛る(負にならない)
  (setq x(car vec)y(cadr vec)z(caddr vec)
        dist_p(sqrt(+(* x x)(* y y)))
        ang_z(atan z dist_p)
        ang_p(if(< dist_p 1e-8)0(atan y x))
        )
  (if(and(<(abs(sin ang_p))1e-6)(>(cos ang_p)0))(setq ang_p 0)
    (if(and bool(<(sin ang_p)1e-6))
        (setq ang_p(atan(- y)(- x))ang_z(- ang_z))))
  (list ang_p ang_z)
  )


;;ソリッドのエッジ
(defun solid_edgeline
    ( entna / eset ed tsL cx cy cz ppL vvL vxL p1L p2L p3L p4L ii num
            pcL vaxL vcxL rgL pa1L pa2L pa3L pcvL vaxvL vcxvL pccL raxsL
            pcvL pa1vL pa2Lv pa3vL pa4vL pccvL obL obvL rg1L rg2L
            key-mode lst save_tile isw ls_p_line)

  (if command_for_alter
      (setq scd_F0F "F 0 F" scd_II "I I" _$ "$" 
            scd_transf "transform" scd_0313 " 0 3 1 3 " scd_coedge "coedge" _sps "{" _spe "}"
            )
    (setq scd_F0F "Y o Y" scd_II "V V" _$ "{" 
          scd_transf "+->1,90-2" scd_0313 " o l n l " scd_coedge "<0:;8:" _sps "$" _spe "\""
          )
    )

  (if command_for_alter
      (setq scd_reversedsingle "reversed single #"         scd_forwardsingle "forward single #"
            scd_history "persubent-acadSolidHistory-attrib $-1 -1 $-1 $-1 $"
            scd_face "face $"  scd_loop "loop $-1 -1 $-1 $"
            scd_conesurface "cone-surface $-1 -1 $-1 "  scd_torussurface "torus-surface $-1 -1 $-1 "
            scd_coedge "coedge $-1 -1 $-1 $"       scd_edge "edge $"
            scd_ellipsecurve "ellipse-curve $-1 -1 $-1 " scd_point "point $-1 -1 $-1 "
            scd_straightcurve "straight-curve $-1 -1 $-1 "
            scd_vertex "vertex $-1 -1 $-1 $"       scd_planesurface "plane-surface $-1 -1 $-1 "
            scd_forwardvi "forward_v I I I I #"       scd_forwardi "forward I I I I #"
            scd_forwardunknown "forward @7 unknown #"      scd_target "forward @7 tangent #"
            _$ "$" _# "#" _-1 "-1 " _0 "0 "
            )
    (setq scd_reversedsingle "-:):-,:; ,6183: |"          scd_forwardsingle "90-(>-; ,6183: |"
          scd_history "/:-,*=:1+r><>;L036;W6,+0-&r>++-6= {rn rn {rn {rn {"
          scd_face "9><: {" scd_loop "300/ {rn rn {rn {"
          scd_conesurface "<01:r,*-9><: {rn rn {rn "   scd_torussurface "+0-*,r,*-9><: {rn rn {rn "
          scd_coedge "<0:;8: {rn rn {rn {"        scd_edge ":;8: {"
          scd_ellipsecurve ":336/,:r<*-): {rn rn {rn "  scd_point "/061+ {rn rn {rn "
          scd_straightcurve ",+->687+r<*-): {rn rn {rn "
          scd_vertex "):-+:' {rn rn {rn {"        scd_planesurface "/3>1:r,*-9><: {rn rn {rn "
          scd_forwardvi "90-(>-;@) V V V V |"        scd_forwardi "90-(>-; V V V V |"
          scd_forwardunknown "90-(>-; _h *1410(1 |"       scd_target "90-(>-; _h +>18:1+ |"
          _$ "{" _# "|" _-1 "rn " _0 "o "
          )
    )

  (setq ed(reverse(entget entna)) ed(member(assoc 1 ed)ed)
        edtL(list) ii -4 isw T)
  (while(=(caar ed)1)
    (setq tt(cdar ed)ed(cdr ed))
    (while(=(caar ed)3) (setq tt(strcat(cdar ed)tt)ed(cdr ed)));;またいつか
    (setq edtL(cons tt edtL))
    )
  (setq edtL(mapcar '(lambda(a)
                       (if isw(setq ii(1+ ii) isw(null(vl-string-search _sps a)))
                         (setq isw(vl-string-search _spe a)))
                       (cons ii a))
                    edtL))
  (setq ii -1 edgL(list)ixL(list))
  (setq ls_ellipse(list)ls_straight(list)ls_point(list)ls_vertex(list)ls_edge(list))

  (while(setq tt(nth (setq ii(1+ ii)) edtL))
    (setq ix(car tt)tt(cdr tt))
    (if(vl-string-search scd_ellipsecurve tt)
        (setq ls_ellipse(cons(cons ix(solid_cood_real tt 4 10))ls_ellipse))
      );;中心点 ;;法線ベクトル ;;中心からのベクトル ;;扁平率(1以下)
    (if(vl-string-search scd_straightcurve tt)
        (setq ls_straight(cons(cons ix(solid_cood_real tt 4 6))ls_straight))
      );;点 ;;方向ベクトル

    (if(vl-string-search scd_point tt)
        (setq ls_point(cons(cons ix(solid_cood_real tt 4 3))ls_point)) )

    (if(and(vl-string-search scd_edge tt)(null(vl-string-search scd_coedge tt)))
        (progn
          (setq n1(solid_position tt -1)
                nn(solid_position tt 3)
                n2(solid_position(cdr(assoc nn edtL))-1)
                nn(solid_position tt 4)
                n3(solid_position(cdr(assoc nn edtL))-1)
                )
          (if(and n1 n2 n3)
              (setq ls_edge(cons(list n1 n2 n3)ls_edge)))
          )
      )
    
    (if(vl-string-search scd_transf tt)
        (setq cx(solid_cood_real tt 3 3)cy(solid_cood_real tt 6 3)
              cz(solid_cood_real tt 9 3)mv(solid_cood_real tt 12 3)))
    )

  (setq excod nil)
  (cond
   (cx ;;座標変換あるとき
    (setq vL(mapcar '(lambda(a)(mapcar 'a(list cx cy cz)))(list car cadr caddr)))
    (defun excod(p)(mapcar '+ mv(mapcar '(lambda(b)(apply '+(mapcar '* p b)))vL)))
    (setq ls_point(mapcar '(lambda(p)(cons(car p)(excod(cdr p))))ls_point))
    )
   );;

  (while ls_edge
    (setq a(car ls_edge)ls_edge(cdr ls_edge)
          n1(car a)n2(cadr a)n3(caddr a)
          p0(cdr(assoc n2 ls_point))
          p1(cdr(assoc n3 ls_point)))
    (cond
     ((assoc n1 ls_straight)
      (setq ls_p_line(cons(list p0 p1)ls_p_line))
      )
     ((setq a(assoc n1 ls_ellipse))
      (setq a(cdr a)
            p_center(if excod(excod(take_list a 3))(take_list a 3))a(cdddr a)
            vec_normal(take_list a 3)a(cdddr a)
            vec_x(take_list a 3)a(cdddr a)
            ratio_Oblate(car a)
            vec_y(cross_product vec_normal vec_x)
            ls_ang(if(and p0 p1)
                      ((lambda( / vec0 vec1 sin_ang cos_ang ang_0 ang_r)
                         (setq vec0(mapcar '- p0 p_center)vec1(mapcar '- p1 p_center)
                               sin_ang(sin_abscprod vec0 vec1 vec_normal)
                               cos_ang(apply '+(mapcar '* vec0 vec1)))
                         (list(atan sin_ang cos_ang)
                              (atan(apply '+(mapcar '* vec_y vec0))
                                   (apply '+(mapcar '* vec_x vec0))))
                         ))
                    (list(* 2 pi)0.)
                    )
            )
      (if(<(abs(sin(car ls_ang)))1e-8)(setq ls_ang(cons(* 2 pi)(cdr ls_ang))))
      ;; (if(<(abs(-(abs(car ls_ang))pi))1e-8)(setq ls_ang(cons(* 2 pi)(cdr ls_ang))))
      
      (setq num(fix(/(car ls_ang)0.15)))
      (if(= num 0)T
        (progn
          (setq ang_delta(/(car ls_ang)num)
                ang0(cadr ls_ang)
                p0(mapcar '(lambda(p x y)(+ p(* x(cos ang0))(* y(sin ang0)ratio_oblate)))
                          p_center vec_x vec_y)
                )
      
          (while(>(setq num(1- num))-1)
            (setq ang0(+ ang0 ang_delta)
                  p1(mapcar '(lambda(p x y)(+ p(* x(cos ang0))(* y(sin ang0)ratio_oblate)))
                            p_center vec_x vec_y)) 
            (setq ls_p_line(cons(list p0 p1)ls_p_line))
            (setq p0 p1)
            )
          ))
      
      )
     )
    )
  
  ls_p_line
  )

(defun merge-intervals (lst / sorted result curr)
  (setq sorted (vl-sort lst '(lambda (a b) (< (car a) (car b))))
        curr (car sorted) )
  (mapcar '(lambda(interval)
             (if(<= (car interval) (cadr curr)) ; overlap
                 (setq curr (list (car curr) (max (cadr curr) (cadr interval))))
               (setq result (cons curr result) curr interval)
               ) )
          (cdr sorted))
  (reverse(cons curr result))
  )

(defun merge-flat-intervals (flatlist / sorted result curr x1 x2)
  ;; まず、2要素ずつのリストを抽出して、開始点でソート
  (setq sorted
        (vl-sort
         (mapcar '(lambda (i)
                    (setq x1 (nth i flatlist)
                          x2 (nth (+ i 1) flatlist))
                    (if (< x1 x2)
                        (list x1 x2)
                      (list x2 x1)))
                 (vl-list* 0 2 (apply 'append (mapcar '(lambda (n) (list (+ n 2))) (vl-list* 0 2 flatlist)))))
         '(lambda (a b) (< (car a) (car b)))
         )
        )
  ;; マージ処理
  (if sorted
      (progn
        (setq curr (car sorted) result '())
        (foreach interval (cdr sorted)
                 (if (<= (car interval) (cadr curr)) ; 重なってる
                     (setq curr (list (car curr) (max (cadr curr) (cadr interval))))
                   (progn
                     (setq result (cons curr result))
                     (setq curr interval))))
        (setq result (cons curr result))
        (apply 'append (reverse result))
        )
    '() ; 入力が空の場合
    )
  )


(defun pair_wise (lst / res)
  (while(and lst (cdr lst))
    (setq res(cons(list(car lst)(cadr lst))res)lst(cddr lst))
    )
  (reverse res)
  )


(defun set_dummy_assoc( val_key ls_origin sym_origin sym_dummy sym_val / )
  (setq ls_dummy nil)
  (while(and(/=(caar ls_origin)val_key)ls_origin)
    (setq ls_dummy(cons(car ls_origin)ls_dummy)ls_origin(cdr ls_origin))
    )
  (set sym_val(car ls_origin))
  (setq ls_origin(cdr ls_origin))
  (set sym_origin ls_origin)
  (set sym_dummy ls_dummy)
  )

(defun reset_dummy_assoc( bool_type ls_origin ls_dummy val_new / )
  (if bool_type(setq ls_origin(cons val_new ls_origin)))
  ;; (setq ls_origin(append(reverse ls_dummy)ls_origin))
  (while ls_dummy
    (setq ls_origin(cons(car ls_dummy)ls_origin)ls_dummy(cdr ls_dummy))
    )
  (if bool_type T(setq ls_origin(cons val_new ls_origin)))
  ls_origin
  )



;;>>>>library


;;<<<<<<<<<<<<<<<help

;;ヘルプ呼び出しコマンド
(defun help_command_set( / save_tile )
  (if(setq file_r(open(strcat sp_path "help_app.dat")"r"))
      (progn
        (setq helpapp(read-line file_r)
              helpapp_path(read-line file_r))
        (close file_r)
        )
    (setq helpapp "msedge"
          helpapp_path "C:\\Program Files (x86)\\Microsoft\\Edge\\Application\\")
    )
  (setq path_dcl(strcat sp_path "help.dcl") 
        open_file (open path_dcl "w")
        help_command_x nil
        )
  (foreach
   str(list
       "Sedit :dialog"
       "{"
       (mix_strasc(list " label = \"" 12504 12523 12503 "\";"))

       " :row"
       " {"
       "  ok_cancel;"

       (dcltext_temp "text" "" "" "45" T ""
                     (mix_strasc(list "  OK" 12508 12479 12531 12434 25276 12375 12383 27425 12395 12467 12510 12531 12489 12375 12383 12418 12398 12398 12504 12523 12503 12434 38283 12365 12414 12377))
                     nil )
       "  }"
       " spacer;"
       " spacer;"

       " :boxed_row"
       " {"
       (mix_strasc(list "  label = \"PDF"  12434 38283 12367 12450 12503 12522 12465 12540 12471 12519 12531 12398 35373 23450 "\";"))
       ;;を開くアプリケーションの設定
       "  :column"
       "  {"
       (dcltext_temp "text" "" "" "10" T "" ;;フォルダパス
                     (mix_strasc(list 12501 12457 12523 12480 12497 12473))nil )
       (dcltext_temp "text" "" "" "10" T "" ;;実行ファイル
                     (mix_strasc(list 23455 34892 12501 12449 12452 12523))nil )
       "   }"
       "  :column"
       "  {"
       (dcltext_temp "edit_box" "" "path" "60" T "" ""nil )
       (dcltext_temp "edit_box" "" "app" "60" T "" ""nil )
       "   }"
       "  }"

       ;; " :button"
       ;; " {"
       ;; "  label = \"ヘルプのヘルプを開く\";"
       ;; "  key = \"help\";"
       ;; "  width = 15;"
       ;; "  fixed_width = true;"
       ;; "  }"
       
       " }";//end
       )
   (write-line str open_file))
  (close open_file)

  (defun save_tile( / filew)
    (setq helpapp(get_tile "app")
          helpapp_path(get_tile "path"))
    (if(=(substr helpapp_path(strlen helpapp_path)1)"\\")T
      (setq helpapp_path(strcat helpapp_path "\\")))
    (setq filew(open(strcat sp_path "help_app.dat")"w"))
    (write-line helpapp filew )
    (write-line helpapp_path filew )
    (close filew)
    )
  
  (setq isw 0)
  (setq load_dcl (load_dialog path_dcl))
  (new_dialog "Sedit" load_dcl)

  (set_tile "app" helpapp)
  (set_tile "path" helpapp_path)

  (action_tile "accept" "(save_tile)(setq isw 1)(done_dialog 1)")
  ;; (action_tile "help" "(save_tile)(setq isw 2)(done_dialog 1)")
  
  (setq dialog-box (start_dialog)) ; ユーザー入力を受け付ける
  (unload_dialog load_dcl) ; ダイアログをメモリーから解放する

  (cond
   ((= isw 1)
    (setq help_command_x T)
    
    )
   ((= isw 2)
    ;; (if(startapp(strcat helpapp_path helpapp)
    ;;             (strcat sp_path "help_command_set.pdf"))T
    ;;   (alert(apply 'strcat(mapcar 'chr(list 12450 12503 12522 12465 12540 12471 12519 12531 12497 12473 12434 35211 30452 12375 12390 12367 12384 12373 12356))))
    ;;   )
    )
   )
  (princ)
  )

(defun help_command_open(str_command_name str_comment / filep)
  (setq helpapr(strcat helpapp_path helpapp)
        filep(strcat sp_path str_command_name ".pdf")
        help_command_x nil )
  
  (if(findfile filep)
      (if(startapp helpapr filep)T
        (alert(mix_strasc(list 12450 12503 12522 12465 12540 12471 12519 12531 12497 12473 12434 35211 30452 12375 12390 12367 12384 12373 12356))))
    ;;アプリケーションパスを見直してください
    (if(or(= str_comment "")(null str_comment))
        (alert(mix_strasc(list  30003 12375 35379 12354 12426 12414 12379 12435 10 12371 12398 38917 30446 12398 12504 12523 12503 12399 12414 12384 12354 12426 12414 12379 12435)))
      ;;"申し訳ありません\nこの項目のヘルプはまだありません"))
      (alert(mix_strasc(list str_comment "\n(PDF" 12398 12504 12523 12503 12399 12354 12426 12414 12379 12435 41) ));;PDFのヘルプはありません
      )
    )
  (progn)
  )

;;help>>>>>>>>>>>>>>

;;solid<<<<<<<<<<<<<

;;柱状ソリッドコード作成
(defun ext_sld
    (ls_shape p_origin vec_zdr vec_xdr height_sld str_layer int_color
              /
              ii jj num tt rrt psL pp1 pp2 pp3 pp4 pd rr mg ft
              scd_reversedsingle scd_forwardsingle scd_history scd_face scd_loop
              scd_conesurface scd_torussurface scd_coedge scd_edge scd_ellipsecurve
              scd_point scd_vertex scd_planesurface ngf scd_forwardvi scd_forwardi
              scd_forwardunknown scd_target ls_codehead ls_codefin at11 gyou 
              peb1 peb2 pc ph phr pac xda nt1 nt2 nt3 nt4 at21 at22 at23 ds
              ls_codehead ls_codefin ls_codetrans ls_code
              ls_x ls_y ls_r)
  
  (setq vec_ydr(cross_product vec_zdr vec_xdr)
        vec_zdr(mapcar '(lambda(a)(* height_sld a))vec_zdr)
        )

  (setq num_vertex 0)
  (while ls_shape
    (setq ls_x(cons(car ls_shape)ls_x)ls_shape(cdr ls_shape)
          ls_y(cons(car ls_shape)ls_y)ls_shape(cdr ls_shape)
          ls_r(cons(car ls_shape)ls_r)ls_shape(cdr ls_shape)
          num_vertex(1+ num_vertex)
          )
    )
  (setq ls_x(reverse ls_x)ls_y(reverse ls_y)ls_r(reverse ls_r)
        ls_p(mapcar 'list ls_x ls_y))

  (setq num_face(+ 2 num_vertex)
        num_edge(* 3 num_vertex)
        ls_mark(list num_face num_face num_face num_face
                     (* 2 num_edge)num_edge num_edge
                     (* 2 num_vertex)(* 2 num_vertex)
                     )
        )
  (setq ii -2
        ls_marksum
        (mapcar '(lambda ( n / i a )
                   (setq a 5 i -1 n(1+ n))
                   (while(<(setq i(1+ i))n)(setq a(+ a(nth i ls_mark))))
                   a )
                (mapcar '(lambda(a)(setq ii(1+ ii)))(cons 0 ls_mark)))
        )

  (setq ls_dist(mapcar 'distance ls_p(cons(last ls_p)ls_p))
        ls_ang(mapcar 'angle ls_p(cons(last ls_p)ls_p))
        ls_center(mapcar '(lambda(ang0 ang1 p0 p1 r)
                            (cond;;180°はできない
                             ((= r 0)(list 0 0 0))
                             ((<(sin(- ang0 ang1))0)
                              (carxyz(polar p0(- ang0(* 0.5 pi))r)-1))
                             (T(carxyz(polar p0(+ ang0(* 0.5 pi))r)1))
                             )  )
                         (cons(last ls_ang)ls_ang)ls_ang(cons(last ls_p)ls_p)ls_p ls_r)
        ls_x_center(mapcar 'car ls_center)
        ls_y_center(mapcar 'cadr ls_center)
        ls_arcdirection(mapcar 'caddr ls_center)
        ls_cos(mapcar 'cos ls_ang)ls_sin(mapcar 'sin ls_ang)
        
        ls_angfan(mapcar '(lambda(a b c r)
                        (if(= r 0)0(rem(abs(-(angle a b)(angle a c)))pi)))
                     ls_center(cons(last ls_p)ls_p)ls_p ls_r)
        ls_vecx(mapcar '(lambda(a b r)(if(= r 0)0(- a b))) ls_x ls_x_center ls_r)
        ls_vecy(mapcar '(lambda(a b r)(if(= r 0)0(- a b))) ls_y ls_y_center ls_r)
        )


  (setq str_numcode(int_to_codestr(+(* 20 num_vertex)13)))
  (if(tblsearch "layer" str_layer)T(setq str_layer(getvar "CLAYER")))
  (if command_for_alter
      (setq scd_reversed "reversed " scd_forward "forward " scd_single "single #"
            scd_history "persubent-acadSolidHistory-attrib $-1 -1 $-1 $-1 $"
            scd_face "face $"  scd_loop "loop $-1 -1 $-1 $"
            scd_conesurface "cone-surface $-1 -1 $-1 "  scd_torussurface "torus-surface $-1 -1 $-1 "
            scd_coedge "coedge $-1 -1 $-1 $"       scd_edge "edge $"
            scd_ellipsecurve "ellipse-curve $-1 -1 $-1 " scd_point "point $-1 -1 $-1 "
            scd_vertex "vertex $-1 -1 $-1 $"       scd_planesurface "plane-surface $-1 -1 $-1 "
            scd_forwardvi "forward_v I I I I #"       scd_forwardi "forward I I I I #"
            scd_forwardunknown "forward @7 unknown #"      scd_target "forward @7 tangent #"
            scd_straightcurve "straight-curve $-1 -1 $-1 "
            _$ "$" _# "#" _-1 "-1 " _0 "0 " _1 "1 " _1 "2 " _I "I " _F "F "
            ls_codehead(list
                        (cons 0 "3DSOLID")(cons 8 str_layer)(cons 100 "AcDbEntity")
                        (cons 100 "AcDbModelerGeometry")(cons 70 1)(cons 62 int_color)
                        (cons 1 (strcat "22300 " str_numcode "2 1         "))
                        (cons 1 "16 Autodesk AutoCAD 20 ASM 229.8.0.65535 NT 0")
                        (cons 1 "1 9.9999999999999995e-007 1e-010 ") 
                        (cons 1 "asmheader $-1 -1 @13 229.8.0.65535 #") 
                        (cons 1 "body $-1 -1 $-1 $2 $-1 $4 #") 
                        (cons 1 "lump $-1 -1 $-1 $-1 $3 $1 #") 
                        (cons 1 "shell $-1 -1 $-1 $-1 $-1 $5 $-1 $2 #") 
                        ))
    (setq scd_reversed "-:):-,:; " scd_forward "90-(>-; " scd_single ",6183: |"
          scd_history "/:-,*=:1+r><>;L036;W6,+0-&r>++-6= {rn rn {rn {rn {"
          scd_face "9><: {" scd_loop "300/ {rn rn {rn {"
          scd_conesurface "<01:r,*-9><: {rn rn {rn "   scd_torussurface "+0-*,r,*-9><: {rn rn {rn "
          scd_coedge "<0:;8: {rn rn {rn {"        scd_edge ":;8: {"
          scd_ellipsecurve ":336/,:r<*-): {rn rn {rn "  scd_point "/061+ {rn rn {rn "
          scd_vertex "):-+:' {rn rn {rn {"        scd_planesurface "/3>1:r,*-9><: {rn rn {rn "
          scd_forwardvi "90-(>-;@) V V V V |"        scd_forwardi "90-(>-; V V V V |"
          scd_forwardunknown "90-(>-; _h *1410(1 |"       scd_target "90-(>-; _h +>18:1+ |"
          scd_straightcurve ",+->687+r<*-): {rn rn {rn "
          _$ "{" _# "|" _-1 "rn " _0 "o " _1 "n " _2 "l " _I "V " _F "Y "
          ls_codehead(list
                      (cons 0 "3DSOLID")(cons 8 str_layer)(cons 100 "AcDbEntity")
                      (cons 100 "AcDbModelerGeometry")(cons 70 1)(cons 62 int_color)
                      (cons 1 (strcat "mmloo " str_numcode "m n         "))
                      (cons 1 "ni ^*+0;:,4 ^*+0\\^[ mo ^LR mmjqjqoqijjlj QK o  ")
                      (cons 1 "n fqfffffffffffffffjkh:roh nqooooooooooooooooli:rno ")
                      (cons 1 ">,27:>;:- {rn rn _nl mmjqjqoqijjlj |")
                      (cons 1 "=0;& {rn rn {rn {m {rn {k |")
                      (cons 1 "3*2/ {rn rn {rn {rn {l {n |")
                      (cons 1 ",7:33 {rn rn {rn {rn {rn {j {rn {m |")
                      )
          )
    )

  (setq ls_codefin(append(if command_for_alter(list(cons 1 "End-of-ASM-data ")))
                         (list(cons 290 1)
                              (cons 2 "{00000000-0000-0000-0000-000000000000}")
                              (cons 100 "AcDb3dSolid"))
                         (if command_for_alter nil(list(cons 350(entlast))))
                         ))

  
  (setq str_trans(lsreal_to_codestr(append vec_xdr vec_ydr vec_zdr p_origin))
        ls_codetrans
        (list(if command_for_alter
                 (cons 1(strcat "transform $-1 -1 " str_trans "1 no_rotate no_reflect no_shear #"))
               (cons 1(strcat "+->1,90-2 {rn rn " str_trans "n 10@-0+>+: 10@-:93:<+ 10@,7:>- |"))
               )
             )
        )

  (setq ls_code(list) ls_incr(inclist 0 num_face))
  (setq ls_code
        (append
         ls_code
         (mapcar '(lambda(i);;面1
                    (if(= i(1- num_face))
                        (cons 1(strcat
                                scd_face(int_to_codestr(+(nth 1 ls_marksum)i))
                                _-1 _$ _-1 _$ _-1 _$
                                (int_to_codestr(+(nth 2 ls_marksum)i))
                                _$ _2 _$ _-1 _$
                                (int_to_codestr(+(nth 3 ls_marksum)i))
                                scd_reversed scd_single))
                      (cons 1(strcat
                              scd_face(int_to_codestr(+(nth 1 ls_marksum)i))
                              _-1 _$ _-1 _$ (int_to_codestr(+(nth 0 ls_marksum)i 1)) _$
                              (int_to_codestr(+(nth 2 ls_marksum)i))
                              _$ _2 _$ _-1 _$
                              (int_to_codestr(+(nth 3 ls_marksum)i))
                              scd_forward scd_single))
                      )
                    )
                 ls_incr)
         (mapcar '(lambda(i);;面2
                    (cons 1(strcat scd_history(int_to_codestr(+(nth 0 ls_marksum)i))
                                   _1 _1 _1 _1 _#
                                   )))
                 ls_incr)
         (mapcar '(lambda(i / s);;面3
                    (setq str(strcat scd_loop _-1 _$))
                    (cond
                     ((< i(- num_face 2))
                      (setq str(strcat str(int_to_codestr(+(nth 4 ls_marksum)1(* 2 i)))_$)))
                     ((= i(- num_face 2))
                      (setq str(strcat str(int_to_codestr(+(nth 4 ls_marksum)-1(* 4 num_vertex)))_$)))
                     ((= i(- num_face 1))
                      (setq str(strcat str(int_to_codestr(+(nth 4 ls_marksum)-2(* 2 num_vertex)))_$)))
                     )
                    (cons 1(strcat str(int_to_codestr(+(nth 0 ls_marksum)i))_#)))
                 ls_incr)
         ))

  (setq ls_code
        (append
         ls_code
         (mapcar '(lambda(i r rd x y xc yc cos0 sin0 vecx vecy);;面ベクトル
                    (if(= r 0)
                        (cons 1(strcat scd_planesurface
                                       (lsreal_to_codestr(list x y 0 sin0(- cos0)0 cos0 sin0 0))
                                       scd_forwardvi))
                      (cons 1(strcat
                              scd_conesurface(lsreal_to_codestr(list xc yc 0 0 0 1 vecx vecy 0 1))
                              _I _I _0(lsreal_to_codestr(list rd r))scd_forwardi))
                      )
                    )
                 ls_incr ls_r ls_arcdirection
                 ls_x ls_y ls_x_center ls_y_center
                 ls_cos ls_sin ls_vecx ls_vecy)
         (list(cons 1(strcat scd_planesurface _0 _0 _1 _0 _0 _1 _1 _0 _0 scd_forwardvi))
              (cons 1(strcat scd_planesurface _0 _0 _0 _0 _0 _1 _1 _0 _0 scd_forwardvi)))
         
         ))
  
  (setq ls_incr(inclist 0 (* num_vertex 2))
        ls_code
        (append
         ls_code
         (mapcar '(lambda(i / jn j2 k1 k2 k3 k4 k5 te);;辺と隣の面-下
                    (setq jn(rem i(* 2 num_vertex))j2(rem i 2))
                    (cond
                     ((= jn(* 2(1- num_vertex))) (setq k1(nth 4 ls_marksum)))
                     ((= j2 0)(setq k1(+(nth 4 ls_marksum)i 2)))
                     ((= i 1)(setq k1(1-(nth 5 ls_marksum))))
                     ((= j2 1)(setq k1(+(nth 4 ls_marksum)i(* 4 num_vertex)-2)))
                     )
                    (cond
                     ((= i 0)(setq k2(+(nth 4 ls_marksum)(* 2 num_vertex)-2)))
                     ((= j2 0)(setq k2(+(nth 4 ls_marksum)i -2)))
                     ((= j2 1)(setq k2(+(nth 4 ls_marksum)i(* 4 num_vertex)-1)))
                     )
                    (cond
                     ((= j2 0)(setq k3(+(nth 4 ls_marksum)i 1)
                                    k5(1-(nth 3 ls_marksum))
                                    te(strcat scd_reversed _$)))
                     ((= j2 1)(setq k3(+(nth 4 ls_marksum)i -1)
                                    k5(+(nth 2 ls_marksum)(/ i 2))
                                    te(strcat scd_forward _$)))
                     )
                    (setq k4(+(nth 5 ls_marksum)(/ i 2)))
                    (cons 1(strcat scd_coedge(int_to_codestr k1)
                                   _$(int_to_codestr k2)_$(int_to_codestr k3)
                                   _$(int_to_codestr k4)te(int_to_codestr k5)
                                   _0 _$ _-1 _#)))
                 ls_incr)

         (mapcar '(lambda(i / jn j2 k1 k2 k3 k4 k5 te);;辺と隣の面-上
                    (setq jn(rem i(* 2 num_vertex))j2(rem i 2))
                    (cond
                     ((= j2 0)(setq k1(+(nth 4 ls_marksum)i(* 4 num_vertex))))
                     ((= i 1)(setq k1(+(nth 4 ls_marksum)(* 4 num_vertex)-1)))
                     ((= j2 1)(setq k1(+(nth 4 ls_marksum)i(* 2 num_vertex)-2)))
                     )
                    (cond
                     ((= i 0)(setq k2(1-(nth 5 ls_marksum))))
                     ((= j2 0)(setq k2(+(nth 4 ls_marksum)i(* 4 num_vertex)-1)))
                     ((= jn(1-(* 2 num_vertex))) (setq k2(+(nth 4 ls_marksum)(* 2 num_vertex)1)))
                     ((= j2 1)(setq k2(+(nth 4 ls_marksum)i(* 2 num_vertex)2)))
                     )
                    (cond
                     ((= j2 0)(setq k3(+(nth 4 ls_marksum)i(* 2 num_vertex)1)
                                    k5(+(nth 2 ls_marksum)(/ i 2))
                                    te(strcat scd_reversed _$)))
                     ((= j2 1)(setq k3(+(nth 4 ls_marksum)i(* 2 num_vertex)-1)
                                    k5(-(nth 3 ls_marksum)2)
                                    te(strcat scd_forward _$)))
                     )
                    (setq k4(+(nth 5 ls_marksum)(+(/ i 2)num_vertex)))
                    (cons 1(strcat scd_coedge(int_to_codestr k1)
                                   _$(int_to_codestr k2)_$(int_to_codestr k3)
                                   _$(int_to_codestr k4)te(int_to_codestr k5)
                                   _0 _$ _-1 _#)))
                 ls_incr)

         (mapcar '(lambda(i / jn j2 k1 k2 k3 k4 k5 te);;辺と隣の面-縦
                    (setq jn(rem i(* 2 num_vertex))j2(rem i 2))
                    (cond
                     ((= j2 0)
                      (setq k1(+(nth 4 ls_marksum)i 1)
                            k2(+(nth 4 ls_marksum)i(* 2 num_vertex))
                            k5(+(nth 2 ls_marksum)(/ i 2)) ))
                     ((= jn(1-(* 2 num_vertex)))
                      (setq k1(+(nth 4 ls_marksum)(* 2 num_vertex))
                            k2(+(nth 4 ls_marksum)1)
                            k5(nth 2 ls_marksum) ))
                     ((= j2 1)
                      (setq k1(+(nth 4 ls_marksum)i(* 2 num_vertex)1)
                            k2(+(nth 4 ls_marksum)i 2)
                            k5(+(nth 2 ls_marksum)(/ i 2)1) ))
                     )
                    (cond
                     ((= j2 0)
                      (setq k3(+(nth 4 ls_marksum)i(* 4 num_vertex)1)
                            te(strcat scd_reversed _$))
                      )
                     ((= j2 1)
                      (setq k3(+(nth 4 ls_marksum)i(* 4 num_vertex)-1)
                            te(strcat scd_forward _$))
                      )
                     )
                    (setq k4(+(nth 5 ls_marksum)(+(/ i 2)(* 2 num_vertex))))
                    (cons 1(strcat scd_coedge(int_to_codestr k1)
                                   _$(int_to_codestr k2)_$(int_to_codestr k3)
                                   _$(int_to_codestr k4)te(int_to_codestr k5)
                                   _0 _$ _-1 _#)))
                 ls_incr)
         ))
  
  (setq ls_code
        (append
         ls_code
         (mapcar '(lambda(i r d angfan / s jt ds);;辺の長さ-下
                    (if(= i 0)(setq jt(int_to_codestr(+(nth 7 ls_marksum)(1- num_vertex))))
                      (setq jt(int_to_codestr(+(nth 7 ls_marksum)(1- i)))))
                    (if(= r 0)(setq ds(lsreal_to_codestr(list d)))
                      (setq ds(lsreal_to_codestr(list angfan))))
                    (cons 1(strcat scd_edge _-1 _-1 _$ _-1
                                   _$(int_to_codestr(+(nth 7 ls_marksum)i))_0 _$ jt ds
                                   _$(int_to_codestr(+(nth 4 ls_marksum)1(* 2 i)))
                                   _$(int_to_codestr(+(nth 6 ls_marksum)i))scd_forwardunknown)))
                 ls_incr ls_r ls_dist ls_angfan)

         (mapcar '(lambda(i r d angfan / s jt ds);;辺の長さ-上
                    (if(= i 0)(setq jt(int_to_codestr(+(nth 7 ls_marksum)(1- num_vertex)num_vertex)))
                      (setq jt(int_to_codestr(+(nth 7 ls_marksum)(1- i)num_vertex))))
                    (if(= r 0)(setq ds(lsreal_to_codestr(list d)))
                      (setq ds(lsreal_to_codestr(list angfan))))
                    (cons 1(strcat scd_edge _-1 _-1 _$ _-1
                                   _$(int_to_codestr(+(nth 7 ls_marksum)i num_vertex))_0 _$ jt ds
                                   _$(int_to_codestr(+(nth 4 ls_marksum)(* 2(+ i num_vertex))))
                                   _$(int_to_codestr(+(nth 6 ls_marksum)i num_vertex))scd_forwardunknown)))
                 ls_incr ls_r ls_dist ls_angfan)

         (mapcar '(lambda(i r1 r2 / tt);;辺の長さ-縦;;18:1
                    (setq tt(if(and(= r1 0)(= r2 0))scd_forwardunknown scd_target))
                    (cons 1(strcat scd_edge _-1 _-1 _$ _-1
                                   _$(int_to_codestr(+(nth 7 ls_marksum)i))_0
                                   _$(int_to_codestr(+(nth 7 ls_marksum)i num_vertex))_1
                                   _$(int_to_codestr(+(nth 4 ls_marksum)1(* 2(+ num_vertex num_vertex i))))
                                   _$(int_to_codestr(+(nth 6 ls_marksum)i(* 2 num_vertex)))tt)))
                 ls_incr ls_r(append(cdr ls_r)(list(car ls_r))))
         ))

  (setq ls_code
        (append;;線の向き下
         ls_code(mapcar
                 '(lambda(x y cos0 sin0 xc yc r rd vecx vecy d angfan)
                    (if(= r 0)(cons 1(strcat scd_straightcurve(lsreal_to_codestr(list x y 0 cos0 sin0 0))
                                             _F _0 _F(lsreal_to_codestr(list d))_#))
                      (cons 1(strcat scd_ellipsecurve(lsreal_to_codestr(list xc yc 0 0 0 rd vecx vecy 0 1))
                                     _F _0 _F(lsreal_to_codestr(list angfan))_#))
                      )
                    )
                 ls_x ls_y ls_cos ls_sin ls_x_center ls_y_center
                 ls_r ls_arcdirection ls_vecx ls_vecy ls_dist ls_angfan)))
  
  (setq ls_code
        (append
         ls_code
         (mapcar '(lambda(x y cos0 sin0 xc yc r rd vecx vecy);;線の向き上
                    (if(= r 0)
                        (cons 1(strcat scd_straightcurve(lsreal_to_codestr(list x y 1 cos0 sin0 0))
                                       _I _I _#))
                      (cons 1(strcat scd_ellipsecurve(lsreal_to_codestr(list xc yc 1 0 0 rd vecx vecy 0 1))
                                     _I _I _#))
                      )
                     ;; ((= rd -1)
                     ;;  (cons 1(strcat scd_ellipsecurve(lsreal_to_codestr(list xc yc 1 0 0 -1 0(- r)0 1))
                     ;;                 "V V |")))
                    )
                 ls_x ls_y ls_cos ls_sin ls_x_center ls_y_center ls_r ls_arcdirection ls_vecx ls_vecy)

         (mapcar '(lambda(x y);;線の向き縦
                    (cons 1(strcat scd_straightcurve(lsreal_to_codestr(list x y))
                                   _0 _0 _0 _1 _I _I _#)))
                 ls_x ls_y)
         ))
  
  (setq ls_code
        (append
         ls_code
         (mapcar '(lambda(i);;点と辺-下
                    (cons 1(strcat scd_vertex(int_to_codestr(+(nth 5 ls_marksum)i))_0  ;;" o "
                                   _$(int_to_codestr(+(nth 8 ls_marksum)i))_#)))
                 (take_list ls_incr(length ls_x)))
         (mapcar '(lambda(i);;点と辺-上
                    (cons 1(strcat scd_vertex(int_to_codestr(+(nth 5 ls_marksum)i num_vertex))_1
                                   _$(int_to_codestr(+(nth 8 ls_marksum)i num_vertex))_#)))
                 (take_list ls_incr(length ls_x)))
         (mapcar '(lambda(x y);;点-下
                    (cons 1(strcat scd_point(lsreal_to_codestr(list x y))_0 _#)))
                 ls_x ls_y)
         (mapcar '(lambda(x y);;点-上
                    (cons 1(strcat scd_point(lsreal_to_codestr(list x y))_1 _#)))
                 ls_x ls_y)
         ))

  (entmakex(append ls_codehead ls_codetrans ls_code ls_codefin))
  )


(if command_for_alter
    (defun pole_sld( r h xyz dxyz na ls_x / v1 v2 v3 p v);;円柱
      (setq p(lsreal_to_codestr xyz)v(unit_vector dxyz)
            v3(lsreal_to_codestr(mapcar '(lambda(a)(* a h))v))
            v1(lsreal_to_codestr(mapcar '(lambda(a)(* a r))
                             (trans-x(list 1 0 0)v(list 0 0 1))))
            v2(lsreal_to_codestr(mapcar '(lambda(a)(* a r))
                             (trans-x(list 0 1 0)v(list 0 0 1))))
            )
      
      (if(null na)(setq na(getvar "CLAYER")))
      (entmakex
       (append
        (list
         (cons 0 "3DSOLID")(cons 8 na)
         (cons 100 "AcDbEntity")(cons 100 "AcDbModelerGeometry")(cons 70 1)

         (cons 1 "22300 30 2 1 ");;4 - 1
         (cons 1 "16 Autodesk AutoCAD 20 ASM 229.8.0.65535 NT 0")
         (cons 1 "1 9.9999999999999995e-007 1e-010 ")
         (cons 1 "asmheader $-1 -1 @13 229.8.0.65535 #")
         (cons 1 "body $-1 -1 $-1 $2 $-1 $3 #")
         (cons 1 "lump $-1 -1 $-1 $-1 $4 $1 #")
         (cons 1(strcat "transform $-1 -1 " v1 v2 v3 p
                        "1 rotate no_reflect no_shear #"))
         (cons 1 "shell $-1 -1 $-1 $-1 $-1 $5 $-1 $2 #")
         
         (cons 1 "face $8 -1 $-1 $6 $11 $4 $-1 $16 forward single #")
         (cons 1 "face $9 -1 $-1 $7 $12 $4 $-1 $15 forward single #")
         (cons 1 "face $10 -1 $-1 $-1 $14 $4 $-1 $17 reversed single #")
         (cons 1 "persubent-acadSolidHistory-attrib $-1 -1 $-1 $-1 $5 1 1 1 1 #")
         (cons 1 "persubent-acadSolidHistory-attrib $-1 -1 $-1 $-1 $6 1 1 1 1 #")
         (cons 1 "persubent-acadSolidHistory-attrib $-1 -1 $-1 $-1 $7 1 1 1 1 #")
         (cons 1 "loop $-1 -1 $-1 $13 $18 $5 #")
         (cons 1 "loop $-1 -1 $-1 $-1 $19 $6 #")
         (cons 1 "loop $-1 -1 $-1 $-1 $20 $5 #")
         (cons 1 "loop $-1 -1 $-1 $-1 $21 $7 #")
         (cons
          1 "plane-surface $-1 -1 $-1 0 0 1 0 0 1 1 0 0 forward_v I I I I #")
         (cons 1 "cone-surface $-1 -1 $-1 0 0 0 0 0 1 -1 0 0 1 I I 0 1 1 forward I I I I #")
         (cons
          1 "plane-surface $-1 -1 $-1 0 0 0 0 0 1 1 0 0 forward_v I I I I #")

         (cons 1 "coedge $-1 -1 $-1 $18 $18 $19 $22 reversed $11 0 $-1 #")
         (cons 1 "coedge $-1 -1 $-1 $19 $19 $18 $22 forward $12 0 $-1 #")
         (cons 1 "coedge $-1 -1 $-1 $20 $20 $21 $23 forward $13 0 $-1 #")
         (cons 1 "coedge $-1 -1 $-1 $21 $21 $20 $23 reversed $14 0 $-1 #")
         (cons 1 "edge $-1 -1 $-1 $26 -3.141592653589793116 $26 3.141592653589793116 $18 $24 forward @7 unknown #")
         (cons 1 "edge $-1 -1 $-1 $27 -3.141592653589793116 $27 3.141592653589793116 $20 $25 forward @7 unknown #")
         (cons 1 "ellipse-curve $-1 -1 $-1 0 0 1 0 0 1 -1 0 0 1 I I #")
         (cons 1 "ellipse-curve $-1 -1 $-1 0 0 0 0 0 1 -1 0 0 1 I I #")
         (cons 1 "vertex $-1 -1 $-1 $22 2 $28 #")
         (cons 1 "vertex $-1 -1 $-1 $23 2 $29 #")
         (cons 1 "point $-1 -1 $-1 1 0 1 #")
         (cons 1 "point $-1 -1 $-1 1 0 0 #")
         
         (cons 1 "End-of-ASM-data ")
         (cons 290 1)(cons 2 "{00000000-0000-0000-0000-000000000000}")
         (cons 100 "AcDb3dSolid")
         )
        (if ls_x(list(list -3(list(car ls_x)(cons 1000(cadr ls_x))(cons 1005(caddr ls_x))))))
        ))
      )

  (defun pole_sld( r h xyz dxyz na ls_x / v1 v2 v3 p v);;円柱
    (setq p(lsreal_to_codestr xyz)v(unit_vector dxyz)
          v3(lsreal_to_codestr(mapcar '(lambda(a)(* a h))v))
          v1(lsreal_to_codestr(mapcar '(lambda(a)(* a r))
                           (trans-x(list 1 0 0)v(list 0 0 1))))
          v2(lsreal_to_codestr(mapcar '(lambda(a)(* a r))
                           (trans-x(list 0 1 0)v(list 0 0 1))))
          )
    (if(null na)(setq na(getvar "CLAYER")))
    (entmakex
     (append
      (list
       (cons 0 "3DSOLID")(cons 8 na)
       (cons 100 "AcDbEntity")(cons 100 "AcDbModelerGeometry")(cons 70 1)
       (cons 1 "mmloo lo m n         ")
       (cons 1 "ni ^*+0;:,4 ^*+0\\^[ mo ^LR mmjqjqoqijjlj QK o  ")
       (cons 1 "n fqfffffffffffffffjkh:roh nqooooooooooooooooli:rno ")
       (cons 1 ">,27:>;:- {rn rn _nl mmjqjqoqijjlj |")
       (cons 1 "=0;& {rn rn {rn {m {rn {l |")
       (cons 1 "3*2/ {rn rn {rn {rn {k {n |")
       (cons 1(strcat "+->1,90-2 {rn rn " v1 v2 v3 p
                      "n 10@-0+>+: 10@-:93:<+ 10@,7:>- |"))
       (cons 1 ",7:33 {rn rn {rn {rn {rn {j {rn {m |")

       (cons 1 "9><: {g rn {rn {i {nn {k {rn {ni 90-(>-; ,6183: |")
       (cons 1 "9><: {f rn {rn {h {nm {k {rn {nj 90-(>-; ,6183: |")
       (cons 1 "9><: {no rn {rn {rn {nk {k {rn {nh -:):-,:; ,6183: |")
       (cons 1 "/:-,*=:1+r><>;L036;W6,+0-&r>++-6= {rn rn {rn {rn {j n n n n |")
       (cons 1 "/:-,*=:1+r><>;L036;W6,+0-&r>++-6= {rn rn {rn {rn {i n n n n |")
       (cons 1 "/:-,*=:1+r><>;L036;W6,+0-&r>++-6= {rn rn {rn {rn {h n n n n |")
       (cons 1 "300/ {rn rn {rn {nl {ng {j |")
       (cons 1 "300/ {rn rn {rn {rn {nf {i |")
       (cons 1 "300/ {rn rn {rn {rn {mo {j |")
       (cons 1 "300/ {rn rn {rn {rn {mn {h |")
       (cons 1 "/3>1:r,*-9><: {rn rn {rn o o n o o n n o o 90-(>-;@) V V V V |")
       (cons 1(strcat "<01:r,*-9><: {rn rn {rn o o o o o n rn o o "
                      "n V V o n n 90-(>-; V V V V |"))
       (cons 1 "/3>1:r,*-9><: {rn rn {rn o o o o o n n o o 90-(>-;@) V V V V |")
       (cons 1 "<0:;8: {rn rn {rn {ng {ng {nf {mm -:):-,:; {nn o {rn |")
       (cons 1 "<0:;8: {rn rn {rn {nf {nf {ng {mm 90-(>-; {nm o {rn |")
       (cons 1 "<0:;8: {rn rn {rn {mo {mo {mn {ml 90-(>-; {nl o {rn |")
       (cons 1 "<0:;8: {rn rn {rn {mn {mn {mo {ml -:):-,:; {nk o {rn |")
       (cons 1(strcat ":;8: {rn rn {rn {mi rlqnknjfmijljgfhflnni "
                      "{mi lqnknjfmijljgfhflnni {ng {mk 90-(>-; _h *1410(1 |"))
       (cons 1(strcat ":;8: {rn rn {rn {mh rlqnknjfmijljgfhflnni "
                      "{mh lqnknjfmijljgfhflnni {mo {mj 90-(>-; _h *1410(1 |"))
       (cons 1 ":336/,:r<*-): {rn rn {rn o o n o o n rn o o n V V |")
       (cons 1 ":336/,:r<*-): {rn rn {rn o o o o o n rn o o n V V |")
       (cons 1 "):-+:' {rn rn {rn {mm m {mg |")
       (cons 1 "):-+:' {rn rn {rn {ml m {mf |")
       (cons 1 "/061+ {rn rn {rn n o n |")
       (cons 1 "/061+ {rn rn {rn n o o |")

       (cons 290 1)(cons 2 "")(cons 100 "AcDb3dSolid")
       (cons 350(entlast))
       )
      (if ls_x(list(list -3(list(car ls_x)(cons 1000(cadr ls_x))(cons 1005(caddr ls_x))))))
      ))
    )
  )


(if command_for_alter
    (defun sph_sld( xyz r na ls_x / );;球
      (if(null na)(setq na(getvar "CLAYER")))
      (entmakex
       (append
        (list
         (cons 0 "3DSOLID")(cons 8 na)
         (cons 100 "AcDbEntity")(cons 100 "AcDbModelerGeometry")(cons 70 1)

         (cons 1 "22300 8 2 1 ")
         (cons 1 "16 Autodesk AutoCAD 20 ASM 229.8.0.65535 NT 0")
         (cons 1 "1 9.9999999999999995e-007 1e-010 ")
         (cons 1 "asmheader $-1 -1 @13 229.8.0.65535 #")
         (cons 1 "body $-1 -1 $-1 $2 $-1 $3 #")
         (cons 1 "lump $-1 -1 $-1 $-1 $4 $1 #")
         (cons 1(strcat "transform $-1 -1 1 0 0 0 1 0 0 0 1 "(lsreal_to_codestr xyz)
                        "1 no_rotate no_reflect no_shear #"))
         (cons 1 "shell $-1 -1 $-1 $-1 $-1 $5 $-1 $2 #")
         (cons 1 "face $6 -1 $-1 $-1 $-1 $4 $-1 $7 forward single #")
         (cons 1 "persubent-acadSolidHistory-attrib $-1 -1 $-1 $-1 $5 1 1 1 0 #")
         (cons 1(strcat "sphere-surface $-1 -1 $-1 0 0 0 "(lsreal_to_codestr(list r))
                        " 1 0 0 0 0 1 forward_v I I I I #"))
         (cons 1 "End-of-ASM-data ")
         
         (cons 290 1)(cons 2 "{00000000-0000-0000-0000-000000000000}")
         (cons 100 "AcDb3dSolid")
         )
        (if ls_x(list(list -3(list(car ls_x)(cons 1000(cadr ls_x))(cons 1005(caddr ls_x))))))
        
        ;; (list -3(list rebar_ss_app(cons 1000 "O")(cons 1005 ha)))
        ))
      )
  
  (defun sph_sld( xyz r na ls_x / );;球
    (if(null na)(setq na(getvar "CLAYER")))
    (entmakex
     (append
      (list
       (cons 0 "3DSOLID")(cons 8 na)
       (cons 100 "AcDbEntity")(cons 100 "AcDbModelerGeometry")(cons 70 1)
       (cons 1 "mmloo g m n         ")
       (cons 1 "ni ^*+0;:,4 ^*+0\\^[ mo ^LR mmjqjqoqijjlj QK o  ")
       (cons 1 "n fqfffffffffffffffjkh:roh nqooooooooooooooooli:rno ")
       (cons 1 ">,27:>;:- {rn rn _nl mmjqjqoqijjlj |")
       (cons 1 "=0;& {rn rn {rn {m {rn {l |")
       (cons 1 "3*2/ {rn rn {rn {rn {k {n |")
       (cons 1(strcat "+->1,90-2 {rn rn n o o o n o o o n "(lsreal_to_codestr xyz)
                      "n 10@-0+>+: 10@-:93:<+ 10@,7:>- |"))
       (cons 1 ",7:33 {rn rn {rn {rn {rn {j {rn {m |")
       (cons 1 "9><: {i rn {rn {rn {rn {k {rn {h 90-(>-; ,6183: |")
       (cons 1 "/:-,*=:1+r><>;L036;W6,+0-&r>++-6= {rn rn {rn {rn {j n n n o |")
       (cons 1(strcat ",/7:-:r,*-9><: {rn rn {rn o o o "(lsreal_to_codestr(list r))
                      " n o o o o n 90-(>-;@) V V V V |"))
       (cons 290 1)(cons 2 "")(cons 100 "AcDb3dSolid")
       (cons 350(entlast))
       )
      (if ls_x(list(list -3(list(car ls_x)(cons 1000(cadr ls_x))(cons 1005(caddr ls_x))))))
      ;; (list -3(list rebar_ss_app(cons 1000 "O")(cons 1005 ha)))
      ))
    )
  )


(if command_for_alter
    (defun rac_sld( b h s xyz c vz vx na ls_x / p rx ry rz);;角柱
      (setq rx(unit_vector vx) rz(unit_vector vz) ry(cross_product rz rx)
            p(lsreal_to_codestr(mapcar '- xyz(mapcar '* ry(list c c c))))
            rx(lsreal_to_codestr(mapcar '* rx(list b b b)))
            ry(lsreal_to_codestr(mapcar '* ry(list h h h)))
            rz(lsreal_to_codestr(mapcar '* rz(list s s s))))
      (if(null na)(setq na(getvar "CLAYER")))
      (entmakex
       (append
        (list
         (cons 0 "3DSOLID")(cons 8 na)
         (cons 100 "AcDbEntity")(cons 100 "AcDbModelerGeometry")(cons 70 1)

         (cons 1 "22300 93 2 1 ")
         (cons 1 "16 Autodesk AutoCAD 20 ASM 229.8.0.65535 NT 0")
         (cons 1 "1 9.9999999999999995e-007 1e-010 ")
         (cons 1 "asmheader $-1 -1 @13 229.8.0.65535 #")
         (cons 1 "body $-1 -1 $-1 $2 $-1 $3 #")
         (cons 1 "lump $-1 -1 $-1 $-1 $4 $1 #")
         (cons 1(strcat "transform $-1 -1 " rx ry rz p
                        "1 no_rotate no_reflect shear #"))
         (cons 1 "shell $-1 -1 $-1 $-1 $-1 $5 $-1 $2 #")

         (cons 1 "face $6 -1 $-1 $7 $8 $4 $-1 $9 forward single #")
         (cons 1 "persubent-acadSolidHistory-attrib $-1 -1 $-1 $-1 $5 1 1 1 1 #")
         (cons 1 "face $10 -1 $-1 $11 $12 $4 $-1 $13 forward single #")
         (cons 1 "loop $-1 -1 $-1 $-1 $14 $5 #")
         (cons 1 "plane-surface $-1 -1 $-1 1 -1 0 1 0 0 0 1 0 forward_v I I I I #")
         (cons 1 "persubent-acadSolidHistory-attrib $-1 -1 $-1 $-1 $7 1 1 1 1 #")
         (cons 1 "face $15 -1 $-1 $16 $17 $4 $-1 $18 forward single #")
         (cons 1 "loop $-1 -1 $-1 $-1 $19 $7 #")
         (cons 1 "plane-surface $-1 -1 $-1 -1 -1 0 0 -1 0 1 0 0 forward_v I I I I #")
         (cons 1 "coedge $-1 -1 $-1 $20 $21 $22 $23 reversed $8 0 $-1 #")
         (cons 1 "persubent-acadSolidHistory-attrib $-1 -1 $-1 $-1 $11 1 1 1 1 #")
         (cons 1 "face $24 -1 $-1 $25 $26 $4 $-1 $27 forward single #")
         (cons 1 "loop $-1 -1 $-1 $-1 $28 $11 #")
         (cons 1 "plane-surface $-1 -1 $-1 -1 1 0 -1 0 0 0 -1 0 forward_v I I I I #")
         (cons 1 "coedge $-1 -1 $-1 $29 $30 $31 $32 reversed $12 0 $-1 #")
         (cons 1 "coedge $-1 -1 $-1 $33 $14 $34 $35 forward $8 0 $-1 #")
         (cons 1 "coedge $-1 -1 $-1 $14 $33 $29 $36 reversed $8 0 $-1 #")
         (cons 1 "coedge $-1 -1 $-1 $31 $37 $14 $23 forward $38 0 $-1 #")
         (cons 1 "edge $-1 -1 $-1 $39 0 $40 2 $22 $41 forward @7 unknown #")
         (cons 1 "persubent-acadSolidHistory-attrib $-1 -1 $-1 $-1 $16 1 1 1 1 #")
         (cons 1 "face $42 -1 $-1 $43 $38 $4 $-1 $44 reversed single #")
         (cons 1 "loop $-1 -1 $-1 $-1 $45 $16 #")
         (cons 1 "plane-surface $-1 -1 $-1 1 1 0 0 1 0 -1 0 0 forward_v I I I I #")
         (cons 1 "coedge $-1 -1 $-1 $46 $47 $48 $49 reversed $17 0 $-1 #")
         (cons 1 "coedge $-1 -1 $-1 $50 $19 $21 $36 forward $12 0 $-1 #")
         (cons 1 "coedge $-1 -1 $-1 $19 $50 $46 $51 reversed $12 0 $-1 #")
         (cons 1 "coedge $-1 -1 $-1 $48 $22 $19 $32 forward $38 0 $-1 #")
         (cons 1 "edge $-1 -1 $-1 $40 0 $52 2 $31 $53 forward @7 unknown #")
         (cons 1 "coedge $-1 -1 $-1 $21 $20 $54 $55 reversed $8 0 $-1 #")
         (cons 1 "coedge $-1 -1 $-1 $45 $56 $20 $35 reversed $26 0 $-1 #")
         (cons 1 "edge $-1 -1 $-1 $39 0 $57 1 $34 $58 forward @7 unknown #")
         (cons 1 "edge $-1 -1 $-1 $40 0 $59 1 $29 $60 forward @7 unknown #")
         (cons 1 "coedge $-1 -1 $-1 $22 $48 $45 $61 forward $38 0 $-1 #")
         (cons 1 "loop $-1 -1 $-1 $-1 $37 $25 #")
         (cons 1 "vertex $-1 -1 $-1 $61 1 $62 #")
         (cons 1 "vertex $-1 -1 $-1 $23 1 $63 #")
         (cons 1 "straight-curve $-1 -1 $-1 -1 -1 0 0 1 0 F 0 F 2 #")
         (cons 1 "persubent-acadSolidHistory-attrib $-1 -1 $-1 $-1 $25 1 1 1 1 #")
         (cons 1 "face $64 -1 $-1 $-1 $65 $4 $-1 $66 forward single #")
         (cons 1 "plane-surface $-1 -1 $-1 0 0 0 0 0 1 1 0 0 forward_v I I I I #")
         (cons 1 "coedge $-1 -1 $-1 $67 $34 $37 $61 reversed $26 0 $-1 #")
         (cons 1 "coedge $-1 -1 $-1 $68 $28 $30 $51 forward $17 0 $-1 #")
         (cons 1 "coedge $-1 -1 $-1 $28 $68 $67 $69 reversed $17 0 $-1 #")
         (cons 1 "coedge $-1 -1 $-1 $37 $31 $28 $49 forward $38 0 $-1 #")
         (cons 1 "edge $-1 -1 $-1 $52 0 $70 2 $48 $41 forward @7 unknown #")
         (cons 1 "coedge $-1 -1 $-1 $30 $29 $71 $72 reversed $12 0 $-1 #")
         (cons 1 "edge $-1 -1 $-1 $52 0 $73 1 $46 $74 forward @7 unknown #")
         (cons 1 "vertex $-1 -1 $-1 $32 1 $75 #")
         (cons 1 "straight-curve $-1 -1 $-1 1 -1 0 -1 0 0 F 0 F 2 #")
         (cons 1 "coedge $-1 -1 $-1 $76 $71 $33 $55 forward $65 0 $-1 #")
         (cons 1 "edge $-1 -1 $-1 $59 -2 $57 0 $33 $77 forward @7 unknown #")
         (cons 1 "coedge $-1 -1 $-1 $34 $67 $76 $78 reversed $26 0 $-1 #")
         (cons 1 "vertex $-1 -1 $-1 $35 1 $79 #")
         (cons 1 "straight-curve $-1 -1 $-1 1 1 0 0 0 1 I I #")
         (cons 1 "vertex $-1 -1 $-1 $36 1 $80 #")
         (cons 1 "straight-curve $-1 -1 $-1 1 -1 0 0 0 1 I I #")
         (cons 1 "edge $-1 -1 $-1 $70 0 $39 2 $37 $81 forward @7 unknown #")
         (cons 1 "point $-1 -1 $-1 1 1 0 #")
         (cons 1 "point $-1 -1 $-1 1 -1 0 #")
         (cons 1 "persubent-acadSolidHistory-attrib $-1 -1 $-1 $-1 $43 1 1 1 1 #")
         (cons 1 "loop $-1 -1 $-1 $-1 $76 $43 #")
         (cons 1 "plane-surface $-1 -1 $-1 0 0 1 0 0 1 1 0 0 forward_v I I I I #")
         (cons 1 "coedge $-1 -1 $-1 $56 $45 $47 $69 forward $26 0 $-1 #")
         (cons 1 "coedge $-1 -1 $-1 $47 $46 $82 $83 reversed $17 0 $-1 #")
         (cons 1 "edge $-1 -1 $-1 $70 0 $84 1 $67 $85 forward @7 unknown #")
         (cons 1 "vertex $-1 -1 $-1 $61 0 $86 #")
         (cons 1 "coedge $-1 -1 $-1 $54 $82 $50 $72 forward $65 0 $-1 #")
         (cons 1 "edge $-1 -1 $-1 $73 -2 $59 0 $50 $87 forward @7 unknown #")
         (cons 1 "vertex $-1 -1 $-1 $51 1 $88 #")
         (cons 1 "straight-curve $-1 -1 $-1 -1 -1 0 0 0 1 I I #")
         (cons 1 "point $-1 -1 $-1 -1 -1 0 #")
         (cons 1 "coedge $-1 -1 $-1 $82 $54 $56 $78 forward $65 0 $-1 #")
         (cons 1 "straight-curve $-1 -1 $-1 1 1 1 0 1 0 I I #")
         (cons 1 "edge $-1 -1 $-1 $57 -2 $84 0 $56 $89 forward @7 unknown #")
         (cons 1 "point $-1 -1 $-1 1 1 1 #")
         (cons 1 "point $-1 -1 $-1 1 -1 1 #")
         (cons 1 "straight-curve $-1 -1 $-1 -1 1 0 1 0 0 F 0 F 2 #")
         (cons 1 "coedge $-1 -1 $-1 $71 $76 $68 $83 forward $65 0 $-1 #")
         (cons 1 "edge $-1 -1 $-1 $84 -2 $73 0 $68 $90 forward @7 unknown #")
         (cons 1 "vertex $-1 -1 $-1 $69 1 $91 #")
         (cons 1 "straight-curve $-1 -1 $-1 -1 1 0 0 0 1 I I #")
         (cons 1 "point $-1 -1 $-1 -1 1 0 #")
         (cons 1 "straight-curve $-1 -1 $-1 1 -1 1 1 0 0 I I #")
         (cons 1 "point $-1 -1 $-1 -1 -1 1 #")
         (cons 1 "straight-curve $-1 -1 $-1 -1 1 1 -1 0 0 I I #")
         (cons 1 "straight-curve $-1 -1 $-1 -1 -1 1 0 -1 0 I I #")
         (cons 1 "point $-1 -1 $-1 -1 1 1 #")
         (cons 1 "End-of-ASM-data ")
         
         (cons 290 1)(cons 2 "{00000000-0000-0000-0000-000000000000}")
         (cons 100 "AcDb3dSolid")
         )
        (if ls_x(list(list -3(list(car ls_x)(cons 1000(cadr ls_x))(cons 1005(caddr ls_x))))))
        ;; (list -3(list rebar_ss_app(cons 1000 "O")(cons 1005 ha)))
        ))
      )
  (defun rac_sld( b h s xyz c vz vx na ls_x / p rx ry rz);;角柱
    (setq rx(unit_vector vx) rz(unit_vector vz) ry(cross_product rz rx)
          p(lsreal_to_codestr(mapcar '- xyz(mapcar '* ry(list c c c))))
          rx(lsreal_to_codestr(mapcar '* rx(list b b b)))
          ry(lsreal_to_codestr(mapcar '* ry(list h h h)))
          rz(lsreal_to_codestr(mapcar '* rz(list s s s))))
    (if(null na)(setq na(getvar "CLAYER")))
    (entmakex
     (append
      (list
       (cons 0 "3DSOLID")(cons 8 na)
       (cons 100 "AcDbEntity")(cons 100 "AcDbModelerGeometry")(cons 70 1)
       (cons 1 "mmloo fl m n         ")
       (cons 1 "ni ^*+0;:,4 ^*+0\\^[ mo ^LR mmjqjqoqijjlj QK o  ")
       (cons 1 "n fqfffffffffffffffjkh:roh nqooooooooooooooooli:rno ")
       (cons 1 ">,27:>;:- {rn rn _nl mmjqjqoqijjlj |")
       (cons 1 "=0;& {rn rn {rn {m {rn {l |")
       (cons 1 "3*2/ {rn rn {rn {rn {k {n |")
       (cons 1(strcat "+->1,90-2 {rn rn " rx ry rz p
                      "n 10@-0+>+: 10@-:93:<+ 10@,7:>- |"))
       (cons 1 ",7:33 {rn rn {rn {rn {rn {j {rn {m |")

       (cons 1 "9><: {nn rn {rn {i {nh {k {rn {ml 90-(>-; ,6183: |")
       (cons 1 "9><: {nm rn {rn {h {ng {k {rn {mk 90-(>-; ,6183: |")
       (cons 1 "9><: {nl rn {rn {g {nf {k {rn {mj 90-(>-; ,6183: |")
       (cons 1 "9><: {nk rn {rn {f {mo {k {rn {mi 90-(>-; ,6183: |")
       (cons 1 "9><: {nj rn {rn {no {mn {k {rn {mh -:):-,:; ,6183: |")
       (cons 1 "9><: {ni rn {rn {rn {mm {k {rn {mg 90-(>-; ,6183: |")
       (cons 1 "/:-,*=:1+r><>;L036;W6,+0-&r>++-6= {rn rn {rn {rn {j n n n n |")
       (cons 1 "/:-,*=:1+r><>;L036;W6,+0-&r>++-6= {rn rn {rn {rn {i n n n n |")
       (cons 1 "/:-,*=:1+r><>;L036;W6,+0-&r>++-6= {rn rn {rn {rn {h n n n n |")
       (cons 1 "/:-,*=:1+r><>;L036;W6,+0-&r>++-6= {rn rn {rn {rn {g n n n n |")
       (cons 1 "/:-,*=:1+r><>;L036;W6,+0-&r>++-6= {rn rn {rn {rn {f n n n n |")
       (cons 1 "/:-,*=:1+r><>;L036;W6,+0-&r>++-6= {rn rn {rn {rn {no n n n n |")
       (cons 1 "300/ {rn rn {rn {rn {mf {j |")
       (cons 1 "300/ {rn rn {rn {rn {ln {i |")
       (cons 1 "300/ {rn rn {rn {rn {ll {h |")
       (cons 1 "300/ {rn rn {rn {rn {lj {g |")
       (cons 1 "300/ {rn rn {rn {rn {li {f |")
       (cons 1 "300/ {rn rn {rn {rn {lg {no |")
       (cons 1 "/3>1:r,*-9><: {rn rn {rn n rn o n o o o n o 90-(>-;@) V V V V |")
       (cons 1 "/3>1:r,*-9><: {rn rn {rn rn rn o o rn o n o o 90-(>-;@) V V V V |")
       (cons 1 "/3>1:r,*-9><: {rn rn {rn rn n o rn o o o rn o 90-(>-;@) V V V V |")
       (cons 1 "/3>1:r,*-9><: {rn rn {rn n n o o n o rn o o 90-(>-;@) V V V V |")
       (cons 1 "/3>1:r,*-9><: {rn rn {rn o o o o o n n o o 90-(>-;@) V V V V |")
       (cons 1 "/3>1:r,*-9><: {rn rn {rn o o n o o n n o o 90-(>-;@) V V V V |")
       (cons 1 "<0:;8: {rn rn {rn {ki {kh {lo {jl -:):-,:; {nh o {rn |")
       (cons 1 "<0:;8: {rn rn {rn {lm {li {mf {jl 90-(>-; {mn o {rn |")
       (cons 1 "<0:;8: {rn rn {rn {kg {kf {lm {jk -:):-,:; {ng o {rn |")
       (cons 1 "<0:;8: {rn rn {rn {lk {lo {ln {jk 90-(>-; {mn o {rn |")
       (cons 1 "<0:;8: {rn rn {rn {jo {jn {lk {jj -:):-,:; {nf o {rn |")
       (cons 1 "<0:;8: {rn rn {rn {li {lm {ll {jj 90-(>-; {mn o {rn |")
       (cons 1 "<0:;8: {rn rn {rn {jm {kj {li {ji -:):-,:; {mo o {rn |")
       (cons 1 "<0:;8: {rn rn {rn {lo {lk {lj {ji 90-(>-; {mn o {rn |")
       (cons 1 "<0:;8: {rn rn {rn {kj {jm {lg {jh -:):-,:; {mo o {rn |")
       (cons 1 "<0:;8: {rn rn {rn {ko {kk {lh {jh 90-(>-; {mm o {rn |")
       (cons 1 "<0:;8: {rn rn {rn {jn {jo {ko {jg -:):-,:; {nf o {rn |")
       (cons 1 "<0:;8: {rn rn {rn {km {lg {lf {jg 90-(>-; {mm o {rn |")
       (cons 1 "<0:;8: {rn rn {rn {kf {kg {km {jf -:):-,:; {ng o {rn |")
       (cons 1 "<0:;8: {rn rn {rn {kk {ko {kn {jf 90-(>-; {mm o {rn |")
       (cons 1 "<0:;8: {rn rn {rn {kh {ki {kk {io -:):-,:; {nh o {rn |")
       (cons 1 "<0:;8: {rn rn {rn {lg {km {kl {io 90-(>-; {mm o {rn |")
       (cons 1 "<0:;8: {rn rn {rn {lj {lh {ki {in -:):-,:; {mo o {rn |")
       (cons 1 "<0:;8: {rn rn {rn {kl {mf {kj {in 90-(>-; {nh o {rn |")
       (cons 1 "<0:;8: {rn rn {rn {mf {kl {kg {im -:):-,:; {nh o {rn |")
       (cons 1 "<0:;8: {rn rn {rn {kn {ln {kh {im 90-(>-; {ng o {rn |")
       (cons 1 "<0:;8: {rn rn {rn {ln {kn {jo {il -:):-,:; {ng o {rn |")
       (cons 1 "<0:;8: {rn rn {rn {lf {ll {kf {il 90-(>-; {nf o {rn |")
       (cons 1 "<0:;8: {rn rn {rn {ll {lf {jm {ik -:):-,:; {nf o {rn |")
       (cons 1 "<0:;8: {rn rn {rn {lh {lj {jn {ik 90-(>-; {mo o {rn |")
       (cons 1 ":;8: {rn rn {rn {hh o {hg m {lo {ih 90-(>-; _h *1410(1 |")
       (cons 1 ":;8: {rn rn {rn {hg o {hf m {lm {ii 90-(>-; _h *1410(1 |")
       (cons 1 ":;8: {rn rn {rn {hf o {go m {lk {ih 90-(>-; _h *1410(1 |")
       (cons 1 ":;8: {rn rn {rn {go o {hh m {li {ig 90-(>-; _h *1410(1 |")
       (cons 1 ":;8: {rn rn {rn {gn rm {gk o {lh {if 90-(>-; _h *1410(1 |")
       (cons 1 ":;8: {rn rn {rn {gk rm {gl o {lf {ho 90-(>-; _h *1410(1 |")
       (cons 1 ":;8: {rn rn {rn {gl rm {gm o {kn {hn 90-(>-; _h *1410(1 |")
       (cons 1 ":;8: {rn rn {rn {gm rm {gn o {kl {hm 90-(>-; _h *1410(1 |")
       (cons 1 ":;8: {rn rn {rn {hh o {gn n {kj {hl 90-(>-; _h *1410(1 |")
       (cons 1 ":;8: {rn rn {rn {hg o {gm n {kg {hk 90-(>-; _h *1410(1 |")
       (cons 1 ":;8: {rn rn {rn {hf o {gl n {jo {hj 90-(>-; _h *1410(1 |")
       (cons 1 ":;8: {rn rn {rn {go o {gk n {jm {hi 90-(>-; _h *1410(1 |")
       (cons 1 ",+->687+r<*-): {rn rn {rn n n o o rn o Y o Y m |")
       (cons 1 ",+->687+r<*-): {rn rn {rn n rn o rn o o Y o Y m |")
       (cons 1 ",+->687+r<*-): {rn rn {rn rn rn o o n o Y o Y m |")
       (cons 1 ",+->687+r<*-): {rn rn {rn rn n o n o o Y o Y m |")
       (cons 1 ",+->687+r<*-): {rn rn {rn rn n n rn o o V V |")
       (cons 1 ",+->687+r<*-): {rn rn {rn rn rn n o rn o V V |")
       (cons 1 ",+->687+r<*-): {rn rn {rn n rn n n o o V V |")
       (cons 1 ",+->687+r<*-): {rn rn {rn n n n o n o V V |")
       (cons 1 ",+->687+r<*-): {rn rn {rn n n o o o n V V |")
       (cons 1 ",+->687+r<*-): {rn rn {rn n rn o o o n V V |")
       (cons 1 ",+->687+r<*-): {rn rn {rn rn rn o o o n V V |")
       (cons 1 ",+->687+r<*-): {rn rn {rn rn n o o o n V V |")
       (cons 1 "):-+:' {rn rn {rn {ji n {gj |")
       (cons 1 "):-+:' {rn rn {rn {jl n {gi |")
       (cons 1 "):-+:' {rn rn {rn {jk n {gh |")
       (cons 1 "):-+:' {rn rn {rn {ji n {gg |")
       (cons 1 "):-+:' {rn rn {rn {in n {gf |")
       (cons 1 "):-+:' {rn rn {rn {im n {fo |")
       (cons 1 "):-+:' {rn rn {rn {il n {fn |")
       (cons 1 "):-+:' {rn rn {rn {ik n {fm |")
       (cons 1 "/061+ {rn rn {rn n n o |")
       (cons 1 "/061+ {rn rn {rn n rn o |")
       (cons 1 "/061+ {rn rn {rn rn rn o |")
       (cons 1 "/061+ {rn rn {rn rn n o |")
       (cons 1 "/061+ {rn rn {rn n n n |")
       (cons 1 "/061+ {rn rn {rn n rn n |")
       (cons 1 "/061+ {rn rn {rn rn rn n |")
       (cons 1 "/061+ {rn rn {rn rn n n |")
      
       (cons 290 1)(cons 2 "")(cons 100 "AcDb3dSolid")
       (cons 350(entlast))
       )
      (if ls_x(list(list -3(list(car ls_x)(cons 1000(cadr ls_x))(cons 1005(caddr ls_x))))))
      ;;(list -3(list rebar_ss_app(cons 1000 "O")(cons 1005 ha)))
      ))
    )
  )




(defun edge_of_dstt(d)
  
  (if command_for_ares
      (progn
        (defun rt1_xc( a )
          (apply 'strcat(mapcar '(lambda(b)(if(=(-(atof(rtos b 2))(fix b))0)
                                               (strcat(rtos b 2 0)" ")
                                             (strcat(rtos b 2 16)" "))) a)));;
        )
    (defun rt1_xc( a );;数値変換
      (apply
       'strcat
       (mapcar
        '(lambda(b)
           (if(=(-(atof(rtos b 2))(fix b))0)
               (strcat(vl-string-translate sotnum sotkey(rtos b 2 0))" ")
             (strcat(vl-string-translate sotnum sotkey(rtos b 2 16))" ")))
        a)))
    )
  
  (mapcar '(lambda(a)(cons 1 a))
          (if command_for_ares
              (list "pcurve $-1 -1 $ -1 0 forward { exp_par_cur nubs 1 open 2 "
                    (strcat " 0 1 "(rt1_xc(list d)) "1 ")
                    " 1 1 " " 0 1 " " 0 " " spline forward { ref 0 } I I I I "
                    " } 0 0 #")
            (list "/<*-): {rn rn {rn o 90-(>-; $ :'/@/>-@<*- 1*=, n 0/:1 m "
                  (strcat " o n "(rt1_xc(list d)) "n ")
                  " n n " " o n " " o " " ,/361: 90-(>-; $ -:9 o \" V V V V "
                  " \" o o |")
            ))
  )

;;柱状ソリッドコード作成と面作成
;;lsls_point:2重の座標リスト:( (p01 p02 p03 p04 p05) (p11 p12 p13 p14 p15) )
;;p01-p11、p02-p12がそれぞれ前面と背面の同じ点を表す
;;p01が手前、p11が奥になるように見たとき、p01→p02→p03→ は時計回りにならんでいる
;;ls_surf_name:面に付ける名前のリスト 必要ないとき「""」で入力
;;順番は 前面 → 背面 → p01,p02,p11,p12で構成される面 → p02,p03,p12,p13で構成される面 → 
;;ls_surf_entna:既存のポリラインを編集したい場合は図形名をリストにして入力
;;順番はmenLと同じ
(defun code_loft_solid 
    ( lsls_point
      ls_surf_name ls_surf_entna  / ii jj num tt rrt psL pp1 pp2 pp3 pp4 pd rr mg ft
      scd_reversedsingle scd_forwardsingle  scd_history scd_face scd_loop scd_conesurface
      scd_torussurface scd_coedge scd_edge scd_ellipsecurve scd_point scd_vertex scd_planesurface
      scd_forwardvi scd_forwardi t-31 t314 scd_forwardunknown scd_target scd_straightcurve
      scd_reversed scd_forward
      at11 gyou ftL pef1 pef2
      peb1 peb2 pc ph phr pac xda nt1 nt2 nt3 nt4 at21 at22 at23 ds aa )

  (setq ls_p_fro(car lsls_point) ls_p_rea(cadr lsls_point))

  ;;辺の長さを求める
  (setq ls_distp_fro(mapcar 'distance ls_p_fro(append(cdr ls_p_fro)(list(car ls_p_fro))))
        ls_distp_rea(mapcar 'distance ls_p_rea(append(cdr ls_p_rea)(list(car ls_p_rea))))
        ls_depth_p(mapcar 'distance ls_p_fro ls_p_rea)
        )

  (setq ii 0 num_point(length ls_p_fro)
        vec_norm_fro
        (unit_vector(cross_product(mapcar '-(cadr ls_p_fro)(car ls_p_fro)) ;底面法線ベクトル
                                  (mapcar '-(caddr ls_p_fro)(car ls_p_fro))))
        p_cent_fro(mapcar '(lambda(a)(/(apply '+(mapcar 'a ls_p_fro))num_point));中心点
                          (list car cadr caddr))
        vec_norm_rea
        (unit_vector(cross_product(mapcar '-(caddr ls_p_rea)(car ls_p_rea))
                                  (mapcar '-(cadr ls_p_rea)(car ls_p_rea))));底面法線ベクトル
        vec_norm_rea(mapcar '- vec_norm_rea)
        p_cent_rea(mapcar '(lambda(a)(/(apply '+(mapcar 'a ls_p_rea))num_point));中心点
                          (list car cadr caddr))
        ls_vec_norm
        (mapcar
         '(lambda(p_fro0 p_fro1 p_rea0 p_rea1
                         / pL ls_point h21 vec_norm dist_plane) 
            (setq ls_point(list p_fro0 p_fro1 p_rea0 p_rea1)
                  vec_norm(unit_vector(cross_product
                                       (mapcar '- p_rea0 p_fro0);側面法線ベクトル
                                       (mapcar '- p_fro1 p_fro0)))
                  dist_plane(apply '+(mapcar '* p_fro0 vec_norm)));面高さ

            (cond
             ((<(abs(-(apply '+(mapcar '* p_rea1 vec_norm))dist_plane))1e-8);;ねじれがないとき
              (list(mapcar '(lambda(a)(* 0.25(apply '+(mapcar 'a ls_point))))
                           (list car cadr caddr))
                   vec_norm  (trans-x(list 1 0 0)vec_norm(list 0 0 1)))
              );;中点 法線 面内ベクトルのリスト
             
             (T;;ねじれがあるとき(使わないようにしている)
              (setq aa(list p_fro0 p_fro1 p_rea0 p_rea1)
                    xL(mapcar 'car aa)yL(mapcar 'cadr aa)zL(mapcar 'caddr aa)
                    x(apply '+ xL) y(apply '+ yL) z(apply '+ zL)
                    xx(apply '+(mapcar '* xL xL)) yy(apply '+(mapcar '* yL yL))
                    zz(apply '+(mapcar '* zL zL)) xy(apply '+(mapcar '* xL yL))
                    yz(apply '+(mapcar '* yL zL)) zx(apply '+(mapcar '* xL zL))
                    xs(list x xx xy zx)ys(list y xy yy yz)zs(list z zx yz zz)
                    )
              (setq xs(mapcar '(lambda(a b)(-(* a zz)(* zx b)))xs zs)
                    ys(mapcar '(lambda(a b)(-(* a zz)(* yz b)))ys zs)
                    xs(mapcar '(lambda(a b)(-(* a(caddr ys))(*(caddr xs)b))) xs ys))
              (if(>(abs(cadr xs))1e-8)
                  (setq xx(/(car xs)(cadr xs)-1.0)
                        yy(/(+(car ys)(* xx(cadr ys)))(caddr ys)-1.0)
                        zz(/(+(car zs)(* xx(cadr zs))(* yy(caddr zs))) (cadddr zs)-1.0))
                (setq ys(mapcar '(lambda(a b)(-(* a(cadr xs))(*(cadr ys)b))) ys xs)
                      yy(/(car ys)(cadr ys)-1.0)
                      xx(/(+(car xs)(* yy(caddr xs)))(cadr xs)-1.0)
                      zz(/(+(car zs)(* xx(cadr zs))(* yy(caddr zs))) (cadddr zs)-1.0))
                )
              (setq dd(distance(list xx yy zz)(list 0 0 0))
                    h21(list(/ xx dd)(/ yy dd)(/ zz dd)) ;; dd(/ 1.0 dd)
                    n(length aa)
                    h210(if(>(apply '+(mapcar '* h210 h21))0) h21  (mapcar '- h21)))
              (setq ddL(mapcar '(lambda(a)(apply '+(mapcar '* h210 a)))aa)
                    dv(/(apply '+ ddL)n)
                    sv(/(apply '+(mapcar '(lambda(a)(expt(- a dv)2))ddL))n))
              ;;(princ (list sv dv))
              (setq
               p01(trans-x p_fro0(list 0 0 1)h210) z01(caddr p01) p01(carxy p01)
               p04(trans-x p_fro1(list 0 0 1)h210) z04(caddr p04) p04(carxy p04)
               p13(trans-x p_rea0(list 0 0 1)h210) z13(caddr p13) p13(carxy p13)
               p16(trans-x p_rea1(list 0 0 1)h210) z16(caddr p16) p16(carxy p16)
               p02(mapcar '(lambda(a b)(/(+(* 2.0 a)(* 1.0 b))3.0))p01 p04)
               z02(/(+(* 2.0 z01)(* 1.0 z04))3.0)
               p03(mapcar '(lambda(a b)(/(+(* 1.0 a)(* 2.0 b))3.0))p01 p04)
               z03(/(+(* 1.0 z01)(* 2.0 z04))3.0)
               p05(mapcar '(lambda(a b)(/(+(* 2.0 a)(* 1.0 b))3.0))p01 p13)
               z05(/(+(* 2.0 z01)(* 1.0 z13))3.0)
               p08(mapcar '(lambda(a b)(/(+(* 2.0 a)(* 1.0 b))3.0))p04 p16)
               z08(/(+(* 2.0 z04)(* 1.0 z16))3.0)
               p09(mapcar '(lambda(a b)(/(+(* 1.0 a)(* 2.0 b))3.0))p01 p13)
               z09(/(+(* 1.0 z01)(* 2.0 z13))3.0)
               p12(mapcar '(lambda(a b)(/(+(* 1.0 a)(* 2.0 b))3.0))p04 p16)
               z12(/(+(* 1.0 z04)(* 2.0 z16))3.0)
               p14(mapcar '(lambda(a b)(/(+(* 2.0 a)(* 1.0 b))3.0))p13 p16)
               z14(/(+(* 2.0 z13)(* 1.0 z16))3.0)
               p15(mapcar '(lambda(a b)(/(+(* 1.0 a)(* 2.0 b))3.0))p13 p16)
               z15(/(+(* 1.0 z13)(* 2.0 z16))3.0)
               p06(inters p02 p14 p05 p08 nil)
               z06(*(+(/(+(*(distance p06 p14)z02)(*(distance p06 p02)z14))(distance p02 p14));
                      (/(+(*(distance p06 p08)z05)(*(distance p06 p05)z08))(distance p05 p08));
                      )0.5);;
               p07(inters p03 p15 p05 p08 nil)
               z07(*(+(/(+(*(distance p07 p15)z03)(*(distance p07 p03)z15))(distance p03 p15));
                      (/(+(*(distance p07 p08)z05)(*(distance p07 p05)z08))(distance p05 p08));
                      )0.5);;
               p10(inters p02 p14 p09 p12 nil)
               z10(*(+(/(+(*(distance p10 p14)z02)(*(distance p10 p02)z14))(distance p02 p14));
                      (/(+(*(distance p10 p12)z09)(*(distance p10 p09)z12))(distance p09 p12));
                      )0.5);;
               p11(inters p03 p15 p09 p12 nil)
               z11(*(+(/(+(*(distance p11 p15)z03)(*(distance p11 p03)z15))(distance p03 p15));
                      (/(+(*(distance p11 p12)z09)(*(distance p11 p09)z12))(distance p09 p12));
                      )0.5);;
               )
              (setq pL(mapcar
                       '(lambda(a b)(trans-x(carxyz a b)h210(list 0 0 1)))
                       (list p01 p02 p03 p04 p05 p06 p07 p08 p09 p10 p11 p12 p13 p14 p15 p16)
                       (list z01 z02 z03 z04 z05 z06 z07 z08 z09 z10 z11 z12 z13 z14 z15 z16) ))
              (cons h210 pL)
              );;ねじれがあるときおわり
             );;cond
            )
         ls_p_fro(append(cdr ls_p_fro)(list(car ls_p_fro)))
         ls_p_rea(append(cdr ls_p_rea)(list(car ls_p_rea)))
         )
        )

  ;;行番号リストの作成
  (setq ls_code(list) ;;ls_code
        num_line(* 3 num_point)num_surf(+ 2 num_point)
        ls_num_code(list num_surf num_surf num_surf num_surf(* 2 num_line)
                           num_line num_line(* 2 num_point)(* 2 num_point)))
  (setq ii -2
        ls_sum_code
        (mapcar '(lambda ( n / i a  )
                   (setq a 5 i -1 n(1+ n))
                   (while(<(setq i(1+ i))n)(setq a(+ a(nth i ls_num_code))))
                   (progn a)   )
                (mapcar '(lambda(a)(setq ii(1+ ii)))(cons 0 ls_num_code))))

  (setq ii num_surf ls_repeat(list))
  (while(>(setq ii(1- ii))-1)(setq ls_repeat(cons ii ls_repeat)))
  (setq p1(car ls_p_fro)p2(car ls_p_rea))
  (setq ls_entna_surf
        (mapcar
         '(lambda(vec_norm ls_surfname ls_entna_switch i
                           / ls_polyp entna_poly vec_norm_poly str_name
                           vec_x vec_y p_cent)

            (setq str_name(car ls_surfname)
                  entna_poly(car ls_entna_switch)
                  ls_entna_switch(cdr ls_entna_switch))
            ;;(if(wcmatch(car ls_surfname)"*ハンチ*")(princ(car ls_surfname)))
            (cond
             ((=(car ls_surfname)"")(setq entna_poly nil tmL nil))
             ((=(length vec_norm)3)
              (setq vec_norm_poly(if(= i 0)vec_norm
                                   (if(= i 1)(mapcar '- vec_norm)(cadr vec_norm))))

              (setq p_cent(if(= i 0)p_cent_fro (if(= i 1)p_cent_rea (car vec_norm)))
                    vec_x(trans-x(list 1 0 0)vec_norm_poly(list 0 0 1))
                    vec_y(trans-x(list 0 1 0)vec_norm_poly(list 0 0 1))
                    ls_polyp(mapcar '(lambda(a)
                                       (mapcar '(lambda(b c)(+ b(* ratio_boxlen 500 c)))
                                               p_cent a))
                                    (list vec_x(mapcar '- vec_y)(mapcar '- vec_x)vec_y)) )
              (if entna_poly(poly_edit ls_polyp entna_poly)
                (setq entna_poly(poly_entm ls_polyp 1(list str_name(* ratio_boxlen 0.25 500)))))
              (mapcar
               '(lambda(a / e hand_)
                  (if a(if(setq e(entgetxr a))
                           (progn
                             (setq hand_(handent(xdata_key_search(cadr(assoc -3 e)) "FN" 2)))
                             (mapcar 'entdel(list a hand_))
                             )))) ls_entna_switch);;
              (list(cdr(assoc 5(entget entna_poly))) )
              )
             (T;;ねじれがあるとき(使わないようにする)
              (setq h210(car vec_norm))
              (setq pp(mapcar '(lambda(a)(*(apply '+(mapcar 'a(cdr vec_norm)))0.0625))
                              (list car cadr caddr))
                    
                    vx(trans-x(list 1 0 0)h210(list 0 0 1))
                    vy(trans-x(list 0 1 0)h210(list 0 0 1))
                    ls_polyp(mapcar '(lambda(a)
                                  (mapcar '(lambda(b c)(+ b(* ratio_boxlen 500 c)))
                                          pp a))
                               (list vx(mapcar '- vy)(mapcar '- vx)vy)) )

              (if tm(poly_edit ls_polyp tm)
                (setq tm(poly_entm ls_polyp 1(list str_name(* ratio_boxlen 0.25 500)))))
              (setq ed(entget tm) ha(cdr(assoc 5 ed))
                    p1(nth(- i 2)ls_p_fro)p2(nth(rem(- i 1)num_point)ls_p_fro)
                    p3(nth(rem(- i 1)num_point)ls_p_rea)p4(nth(- i 2)ls_p_rea))

              (setq str_name(strcat str_name "-"(chr 32257)) ;;縁
                    ii 0
                    tmL(mapcar
                        '(lambda(a b  / p v tm vx vy pL tt)
                           (setq p(mapcar '(lambda(a b)(* 0.5(+ a b)))a b)
                                 v(mapcar '- a b)
                                 v(unit_vector(cross_product v(cross_product h210 v)))
                                 vx(trans-x(list 1 0 0)v(list 0 0 1))
                                 vy(trans-x(list 0 1 0)v(list 0 0 1))
                                 tt(strcat str_name(itoa(setq ii(1+ ii))))
                                 pL(mapcar
                                    '(lambda(a)
                                       (mapcar '(lambda(b c)
                                                  (+ b(* ratio_boxlen 500 c)))p a));;
                                    (list vx(mapcar '- vy)(mapcar '- vx)vy)) )
                           (if(setq tm(car ls_entna_switch))(poly_edit pL tm)
                             (setq tm(poly_entm pL 1
                                                (list tt(* ratio_boxlen 0.25 500)))))
                           (setq ls_entna_switch(cdr ls_entna_switch))
                           (cdr(assoc 5(entget tm)))  )
                        (list p1 p2 p3 p4)(list p2 p3 p4 p1))
                    
                    tmL(cons ha tmL)
                    )
              )
             )
            
            )
         (append(list vec_norm_fro vec_norm_rea)ls_vec_norm)
         ls_surf_name ls_surf_entna(inclist 0(+ num_point 2))))
  

  ;;((lambda(tt / t1 )(while(/= tt "")(setq t1(substr tt 1 1)tt(substr tt 2))(princ(chr(- 159(ascii t1))))))"")

  (if command_for_alter
      (setq scd_reversedsingle "reversed single #"
            scd_forwardsingle "forward single #"
            scd_history "persubent-acadSolidHistory-attrib $-1 -1 $-1 $-1 $"
            scd_face "face $"                  scd_loop "loop $-1 -1 $-1 $"
            scd_conesurface "cone-surface $-1 -1 $-1 "
            scd_torussurface "torus-surface $-1 -1 $-1 "
            scd_coedge "coedge $-1 -1 $-1 $"   scd_edge "edge $"
            scd_ellipsecurve "ellipse-curve $-1 -1 $-1 "
            scd_point "point $-1 -1 $-1 "      scd_vertex "vertex $-1 -1 $-1 $"
            scd_planesurface "plane-surface $-1 -1 $-1 "
            scd_forwardvi "forward_v I I I I #"
            scd_forwardi "forward I I I I #"
            scd_forwardunknown "forward @7 unknown #"
            scd_target "forward @7 tangent #"
            scd_straightcurve "straight-curve $-1 -1 $-1 "
            scd_reversed "reversed $"          scd_forward "forward $"
            _$ "$" _# "#" _-1 "-1 " _0 "0 ")
    (setq scd_reversedsingle "-:):-,:; ,6183: |"
          scd_forwardsingle "90-(>-; ,6183: |"
          scd_history "/:-,*=:1+r><>;L036;W6,+0-&r>++-6= {rn rn {rn {rn {"
          scd_face "9><: {"                    scd_loop "300/ {rn rn {rn {"
          scd_conesurface "<01:r,*-9><: {rn rn {rn "
          scd_torussurface "+0-*,r,*-9><: {rn rn {rn "
          scd_coedge "<0:;8: {rn rn {rn {"     scd_edge ":;8: {"
          scd_ellipsecurve ":336/,:r<*-): {rn rn {rn "
          scd_point "/061+ {rn rn {rn "        scd_vertex "):-+:' {rn rn {rn {"
          scd_planesurface "/3>1:r,*-9><: {rn rn {rn "
          scd_forwardvi "90-(>-;@) V V V V |"
          scd_forwardi "90-(>-; V V V V |"
          scd_forwardunknown "90-(>-; _h *1410(1 |"
          scd_target "90-(>-; _h +>18:1+ |"
          scd_straightcurve ",+->687+r<*-): {rn rn {rn "
          scd_reversed "-:):-,:; {"            scd_forward "90-(>-; {"
          _$ "{" _# "|" _-1 "rn " _0 "o " )
    )
  
  (setq ls_codefin(append(if command_for_alter(list(cons 1 "End-of-ASM-data ")))
                        (list(cons 290 1)
                             (cons 2 "{00000000-0000-0000-0000-000000000000}")
                             (cons 100 "AcDb3dSolid"))
                        (if command_for_alter(list)(list(cons 350(entlast))))
                        ))
  
  (setq ls_codetrans
        (if command_for_alter
            "transform $-1 -1 1 0 0 0 1 0 0 0 1 0 0 0 1 no_rotate no_reflect no_shear #"
          "+->1,90-2 {rn rn n o o o n o o o n o o o n 10@-0+>+: 10@-:93:<+ 10@,7:>- |")
        ls_codetrans(list(cons 1 ls_codetrans)))

  (setq ls_code
        (append;;面1
         ls_code
         (mapcar
          '(lambda(i)
             (cons 1(strcat scd_face(int_to_codestr(+(nth 1 ls_sum_code)i))
                            _-1 _$ _-1 _$
                            (if(= i(1- num_surf)) _-1
                              (int_to_codestr(+(nth 0 ls_sum_code)i 1)))
                            _$(int_to_codestr(+(nth 2 ls_sum_code)i))
                            _$(int_to_codestr 3)
                            _$ _-1
                            _$(int_to_codestr(+(nth 3 ls_sum_code)i))
                            (if(= i 1)scd_forwardsingle scd_reversedsingle)) )  )
          ls_repeat)))
  
  (setq ls_code
        (append;;面2
         ls_code
         (mapcar
          '(lambda(i)
             (cons 1(strcat  scd_history
                             (int_to_codestr(+(nth 0 ls_sum_code)i))
                             (if command_for_alter "1 1 1 1 #" "n n n n |"))))
          ls_repeat)))
  
  (setq ls_code
        (append;;面3
         ls_code
         (mapcar
          '(lambda(i)
             (cons 1(strcat scd_loop _-1 _$
                            (int_to_codestr
                             (+(nth 4 ls_sum_code)
                               (if(= i 0)0
                                 (if(= i 1)num_point
                                   (+(* 2 num_point)1(* 4(- i 2)))))))
                            _$(int_to_codestr(+(nth 0 ls_sum_code)i))_#)) )
          ls_repeat)))
  
  (setq ls_1(append p_cent_fro
                    vec_norm_fro
                    (unit_vector(mapcar '-(cadr ls_p_fro)(car ls_p_fro))))
        ls_2(append p_cent_rea
                    vec_norm_rea
                    (unit_vector(mapcar '-(cadr ls_p_rea)(car ls_p_rea))))
        ls_code
        (append ls_code
                (list(cons 1(strcat scd_planesurface
                                    (lsreal_to_codestr ls_1)
                                    scd_forwardvi))
                     (cons 1(strcat scd_planesurface
                                    (lsreal_to_codestr ls_2)
                                    scd_forwardvi)))))
  
  (if command_for_alter
      (setq str_1 "spline-surface $-1 -1 $-1 forward { loft_spl_sur 22800 2 "
            str_2 " 1 1 straight 0 0 0 1 0 0 F 0 F 0 null_surface nullbs F 213 1 1 0 1 0 0 0 0 F null_curve 0 -1 "
            str_3 " F 1 F 0 F 1 F 0 F F F F 0 0 full nubs 3 3 open open none none 2 2 " )
    (setq str_1 ",/361:r,*-9><: {rn rn {rn 90-(>-; $ 309+@,/3@,*- mmgoo m "
          str_2 " n n ,+->687+ o o o n o o Y o Y o 1*33@,*-9><: 1*33=, Y mnl n n o n o o o o Y 1*33@<*-): o rn "
          str_3 " Y n Y o Y n Y o Y Y Y Y o o 9*33 1*=, l l 0/:1 0/:1 101: 101: m m " ))
  
  (setq ls_int_distortion(list)ii(+(nth 2 ls_sum_code)1))
  (mapcar
   '(lambda( vec_norm )
      (setq ii(1+ ii))
      (cond
       ((=(length vec_norm)3)
        (setq ls_int_distortion(append ls_int_distortion(list 0))
              ls_code(append ls_code
                             (list(cons 1(strcat scd_planesurface
                                                 (lsreal_to_codestr(apply 'append vec_norm))
                                                 scd_forwardvi))))))
       (T;;ねじれ
        (setq vec_norm(cdr vec_norm)
              ls_int_distortion(append ls_int_distortion(list ii))
              ls_code (append ls_code
                              (mapcar '(lambda(a)(cons 1 a))
                                      (if command_for_alter
                                          (list str_1 " 0 " str_2 " 1 " str_2 " 0 " str_3
                                                " 0 3 1 3 " " 0 3 1 3 ") 
                                        (list str_1 " o " str_2 " n " str_2 " o " str_3
                                              " o l n l " " o l n l ") ))
                              (mapcar '(lambda(a)(cons 1(strcat " "(lsreal_to_codestr a))))vec_norm)
                              (mapcar '(lambda(a)(cons 1 a))
                                      (if command_for_alter
                                          (list " 0 " " " " 0 " " 0 " " 0 " " 0 "
                                                " 0 " " 0 " " F } I I I I #")
                                        (list " o " " " " o " " o " " o " " o "
                                              " o " " o " " Y \" V V V V |")
                                        ))
                              
                              )
              )
        
        )
       )
      )ls_vec_norm);;

  (setq fee(list)int_count_rowcode(1-(last ls_sum_code)))
  (setq ii num_point ls_repeat(list))
  (while(>(setq ii(1- ii))-1)(setq ls_repeat(cons ii ls_repeat)))
  (setq str_1(strcat scd_reversed(int_to_codestr(nth 2 ls_sum_code))))
  (setq ls_code(append;;辺と隣の面-上
                ls_code(mapcar
                        '(lambda(i / jn j2 k1 k2 k3 k4 k5 te)
                           (setq k1(+(nth 4 ls_sum_code)(if(= i 0)num_point i)-1)
                                 k2(+(nth 4 ls_sum_code)(if(= i(1- num_point))0(1+ i)))
                                 k3(+(nth 4 ls_sum_code)(* 2 num_point)(* i 4))
                                 k4(+(nth 5 ls_sum_code)i) )
                           (cons 1(strcat scd_coedge(int_to_codestr k1)
                                          _$(int_to_codestr k2)
                                          _$(int_to_codestr k3)
                                          _$(int_to_codestr k4)
                                          str_1 _0 _$ _-1 _#))
                           ) ls_repeat)));;

  (setq str_1(strcat scd_forward(int_to_codestr(1+(nth 2 ls_sum_code)))))
  (setq ls_code
        (append;;辺と隣の面-上
         ls_code(mapcar
                 '(lambda(i / jn j2 k1 k2 k3 k4 k5 te)
                    (setq k1(+(nth 4 ls_sum_code)(if(= i(1- num_point))0(1+ i))num_point)
                          k2(+(nth 4 ls_sum_code)(if(= i 0)num_point i)num_point -1)
                          k3(+(nth 4 ls_sum_code)(* 2 num_point)1(* i 4))
                          k4(+(nth 5 ls_sum_code)i num_point))
                    (cons 1(strcat scd_coedge(int_to_codestr k1)
                                   _$(int_to_codestr k2)
                                   _$(int_to_codestr k3)
                                   _$(int_to_codestr k4)
                                   str_1 _0 _$ _-1 _#))
                    ) ls_repeat)));;
  
  (setq ls_code
        (append
         ls_code
         (apply 'append
                (mapcar
                 '(lambda(i / k0 k1 k20 k21 k3 k4 k5 tdL)
                    (setq k5(+(nth 2 ls_sum_code)2 i)
                          k0(+(nth 4 ls_sum_code)(* 2 num_point)(* i 4))
                          k1(+(nth 4 ls_sum_code)i)
                          k20(if(= i 0)(1-(nth 5 ls_sum_code))(1- k0))
                          k21(if(= i(1- num_point))
                                 (+(nth 4 ls_sum_code)(* 2 num_point)2)
                               (+ k0 6))
                          k3(+(nth 5 ls_sum_code)i)
                          k4(+(nth 5 ls_sum_code)num_point
                              i 1(if(= i(1- num_point))0 num_point))
                          tdL(if(null(vl-position k5 ls_int_distortion))
                                 (list _-1 _-1 _-1 _-1)
                               (mapcar
                                '(lambda(a)
                                   (setq fee(append fee(list a))
                                         int_count_rowcode(1+ int_count_rowcode) )
                                   (int_to_codestr int_count_rowcode)
                                   )
                                (list k3(+ k3 num_point)(+ k3 num_point num_point)k4)))
                          )
                    (list(cons 1(strcat
                                 scd_coedge(int_to_codestr(+ k0 3))
                                 _$(int_to_codestr(+ k0 2))
                                 _$(int_to_codestr k1 )
                                 _$(int_to_codestr(+ k3 0))
                                 scd_forward(int_to_codestr k5)_0
                                 _$(nth 0 tdL)_#))
                         (cons 1(strcat
                                 scd_coedge(int_to_codestr(+ k0 2))
                                 _$(int_to_codestr(+ k0 3))
                                 _$(int_to_codestr(+ k1 num_point))
                                 _$(int_to_codestr(+ k3 num_point))
                                 scd_reversed(int_to_codestr k5)_0
                                 _$(nth 1 tdL)_#))
                         (cons 1(strcat
                                 scd_coedge(int_to_codestr(+ k0 0))
                                 _$(int_to_codestr(+ k0 1))
                                 _$(int_to_codestr k20)
                                 _$(int_to_codestr(+ k3 num_point num_point))
                                 scd_forward(int_to_codestr k5)_0
                                 _$(nth 2 tdL)_#))
                         (cons 1(strcat
                                 scd_coedge(int_to_codestr(+ k0 1))
                                 _$(int_to_codestr(+ k0 0))
                                 _$(int_to_codestr k21)
                                 _$(int_to_codestr k4)
                                 scd_reversed(int_to_codestr k5)_0
                                 _$(nth 3 tdL)_#))
                         )
                    )
                 ls_repeat))))

  (setq fee(mapcar '(lambda(a)(fix(- a(nth 5 ls_sum_code))))fee))
  
  (setq ls_code
        (append;;辺の長さ-下
         ls_code
         (mapcar
          '(lambda(i d / s jt ds)
             (if(null(vl-position i fee))T
               (setq fee(subst(edge_of_dstt d)i fee)))
             
             (setq jt(if(= i(1- num_point))(int_to_codestr(nth 7 ls_sum_code))
                       (int_to_codestr(+(nth 7 ls_sum_code)1 i)))
                   ds(lsreal_to_codestr(list d)))
             (cons 1(strcat scd_edge _-1 _-1 _$ _-1 _$
                            (int_to_codestr(+(nth 7 ls_sum_code)i))_0
                            _$ jt ds
                            _$(int_to_codestr
                               (+(nth 4 ls_sum_code)(* 2 num_point)(* 4 i)))
                            _$(int_to_codestr
                               (+(nth 6 ls_sum_code)i))
                            scd_forwardunknown)))
          ls_repeat ls_distp_fro)))
  
  (setq ls_code
        (append;;辺の長さ-上
         ls_code
         (mapcar
          '(lambda(i d / s jt ds)
             (if(null(vl-position(+ i num_point)fee))T
               (setq fee(subst(edge_of_dstt d)(+ i num_point)fee)))
             (setq jt(if(= i(1- num_point))(int_to_codestr(+(nth 7 ls_sum_code)num_point))
                       (int_to_codestr(+(nth 7 ls_sum_code)num_point 1 i)))
                   ds(lsreal_to_codestr(list d)))
             (cons 1(strcat scd_edge _-1 _-1 _$ _-1
                            _$(int_to_codestr
                               (+(nth 7 ls_sum_code)i num_point))
                            _0 _$ jt ds
                            _$(int_to_codestr
                               (+(nth 4 ls_sum_code)1(* 2 num_point)(* 4 i)))
                            _$(int_to_codestr
                               (+(nth 6 ls_sum_code)i num_point))
                            scd_forwardunknown)))
          ls_repeat ls_distp_rea )))

  (setq ls_code
        (append;;辺の長さ-縦
         ls_code
         (mapcar
          '(lambda(i d  / tt ds k3);;18:1
             (if(null(vl-position(+ i num_point num_point)fee))T
               (setq fee(subst(edge_of_dstt d)(+ i num_point num_point)fee)))
             ;; (setq tt scd_forwardunknown)
             (setq ds(lsreal_to_codestr(list d))
                   k3(int_to_codestr
                      (if(= i 0)(+(nth 5 ls_sum_code)-1)
                        (+(nth 4 ls_sum_code)(* 2 num_point)2(* 4 i))) ))
             (cons 1(strcat scd_edge _-1 _-1 _$ _-1
                            _$(int_to_codestr(+(nth 7 ls_sum_code)i num_point))_0
                            _$(int_to_codestr(+(nth 7 ls_sum_code)i)) ds;;長さ
                            _$ k3
                            _$(int_to_codestr
                               (+(nth 6 ls_sum_code)i num_point num_point))
                            scd_forwardunknown)))
          ls_repeat ls_depth_p )))
  
  (setq ls_code
        (append;;線の向き下
         ls_code
         (mapcar
          '(lambda(p1 p2 d / tt)
             (setq tt(lsreal_to_codestr(append p1(unit_vector(mapcar '- p2 p1)))))
             (cons 1(strcat scd_straightcurve tt
                            (if command_for_alter "F 0 F " "Y o Y ")
                            (lsreal_to_codestr(list d))_#)))
          ls_p_fro(append(cdr ls_p_fro)(list(car ls_p_fro)))ls_distp_fro)))

  (setq ls_code
        (append;;線の向き上
         ls_code
         (mapcar
          '(lambda(p1 p2 d / tt)
             (setq tt(lsreal_to_codestr(append p1(unit_vector(mapcar '- p2 p1)))))
             (cons 1(strcat scd_straightcurve tt
                            (if command_for_alter "F 0 F " "Y o Y ")
                            (lsreal_to_codestr(list d))_#)))
          ls_p_rea(append(cdr ls_p_rea)(list(car ls_p_rea)))ls_distp_rea)))

  (setq ls_code
        (append;;線の向き縦
         ls_code
         (mapcar
          '(lambda(p1 p2 / tt)
             (setq tt(lsreal_to_codestr(append p1(unit_vector(mapcar '- p2 p1)))))
             (cons 1(strcat scd_straightcurve tt
                            (if command_for_alter " I I #" " V V |"))))
          ls_p_fro ls_p_rea)))
  
  (setq ls_code
        (append;;点と辺-下
         ls_code
         (mapcar
          '(lambda(i x)
             (cond
              ((= i 0)
               (cons 1(strcat scd_vertex
                              (int_to_codestr(+(nth 5 ls_sum_code)num_point -1))
                              (if command_for_alter " 1 $" " n {")
                              (int_to_codestr(+(nth 8 ls_sum_code)i))" " _#)))
              (T
               (cons 1(strcat scd_vertex
                              (int_to_codestr(+(nth 5 ls_sum_code)i))" " _0
                              _$(int_to_codestr(+(nth 8 ls_sum_code)i))" " _#)))
              ))
          ls_repeat ls_p_fro)))
  
  (setq ls_code
        (append;;点と辺-上
         ls_code
         (mapcar
          '(lambda(i x)
             (cond
              ((= i 0)
               (cons 1(strcat scd_vertex
                              (int_to_codestr(+(nth 5 ls_sum_code)num_point num_point -1))
                              (if command_for_alter " 1 $" " n {")
                              (int_to_codestr(+(nth 8 ls_sum_code)i num_point))" " _#)))
              (T
               (cons 1(strcat scd_vertex(int_to_codestr(+(nth 5 ls_sum_code)i num_point))
                              (if command_for_alter " 0 $" " o {")
                              (int_to_codestr(+(nth 8 ls_sum_code)i num_point))" "_#)))
              ))
          ls_repeat ls_p_fro)))

  (setq ls_code(append ls_code
                       (mapcar ;;点-下
                        '(lambda(p)(cons 1(strcat scd_point(lsreal_to_codestr p)_#)))ls_p_fro)
                       (mapcar;;点-上
                        '(lambda(p)(cons 1(strcat scd_point(lsreal_to_codestr p)_#)))ls_p_rea)
                       ))
  
  (setq str_count(int_to_codestr(1+ int_count_rowcode)))
  
  (setq ls_codehead
        (if command_for_alter
            (list
             (cons 0 "3DSOLID")
             (cons 100 "AcDbEntity")(cons 100 "AcDbModelerGeometry")(cons 70 1)
             (cons 1 (strcat "22300 " str_count "2 1         "))
             (cons 1 "16 Autodesk AutoCAD 20 ASM 229.8.0.65535 NT 0")
             (cons 1 "1 9.9999999999999995e-007 1e-010 ") 
             (cons 1 "asmheader $-1 -1 @13 229.8.0.65535 #") 
             (cons 1 "body $-1 -1 $-1 $2 $-1 $4 #") 
             (cons 1 "lump $-1 -1 $-1 $-1 $3 $1 #") 
             (cons 1 "shell $-1 -1 $-1 $-1 $-1 $5 $-1 $2 #") 
             )
          (list
           (cons 0 "3DSOLID")
           (cons 100 "AcDbEntity")(cons 100 "AcDbModelerGeometry")(cons 70 1)
           (cons 1 (strcat "mmloo " str_count "m k         "))
           (cons 1 "ni ^*+0;:,4 ^*+0\\^[ mo ^LR mmjqnqoqijjlj QK o  ")
           (cons 1 "n fqfffffffffffffffjkh:roh nqooooooooooooooooli:rno ")
           (cons 1 ">,27:>;:- {rn rn _nl mmjqnqoqijjlj |")
           (cons 1 "=0;& {rn rn {rn {m {rn {k |")
           (cons 1 "3*2/ {rn rn {rn {rn {l {n |")
           (cons 1 ",7:33 {rn rn {rn {rn {rn {j {rn {m |")
           )
          )
        )

  
  (setq entna_sld(entmakex(append ls_codehead
                                  ls_codetrans
                                  ls_code(apply 'append fee)
                                  ls_codefin)));;faafbbfcc
  (cons entna_sld ls_entna_surf)
  )



(defun code_circleloft_solid 
    ( ls_point
      read_radius
      / ii jj num tt rrt psL pp1 pp2 pp3 pp4 pd rr mg ft
      scd_reversedsingle scd_forwardsingle  scd_history scd_face scd_loop scd_conesurface
      scd_torussurface scd_coedge scd_edge scd_ellipsecurve scd_point scd_vertex scd_planesurface
      scd_forwardvi scd_forwardi t-31 t314 scd_forwardunknown scd_target scd_straightcurve
      scd_reversed scd_forward
      at11 gyou ftL pef1 pef2
      peb1 peb2 pc ph phr pac xda nt1 nt2 nt3 nt4 at21 at22 at23 ds aa )

  (setq ls_p_fro(car lsls_point) ls_p_rea(cadr lsls_point))

  ;;辺の長さを求める
  (setq ls_distp_fro(mapcar 'distance ls_p_fro(append(cdr ls_p_fro)(list(car ls_p_fro))))
        ls_distp_rea(mapcar 'distance ls_p_rea(append(cdr ls_p_rea)(list(car ls_p_rea))))
        ls_depth_p(mapcar 'distance ls_p_fro ls_p_rea)
        )

  (setq ii 0 num_point(length ls_p_fro)
        vec_norm_fro
        (unit_vector(cross_product(mapcar '-(cadr ls_p_fro)(car ls_p_fro)) ;底面法線ベクトル
                                  (mapcar '-(caddr ls_p_fro)(car ls_p_fro))))
        p_cent_fro(mapcar '(lambda(a)(/(apply '+(mapcar 'a ls_p_fro))num_point));中心点
                          (list car cadr caddr))
        vec_norm_rea
        (unit_vector(cross_product(mapcar '-(caddr ls_p_rea)(car ls_p_rea))
                                  (mapcar '-(cadr ls_p_rea)(car ls_p_rea))));底面法線ベクトル
        vec_norm_rea(mapcar '- vec_norm_rea)
        p_cent_rea(mapcar '(lambda(a)(/(apply '+(mapcar 'a ls_p_rea))num_point));中心点
                          (list car cadr caddr))
        ls_vec_norm
        (mapcar
         '(lambda(p_fro0 p_fro1 p_rea0 p_rea1
                         / pL ls_point h21 vec_norm dist_plane) 
            (setq ls_point(list p_fro0 p_fro1 p_rea0 p_rea1)
                  vec_norm(unit_vector(cross_product
                                       (mapcar '- p_rea0 p_fro0);側面法線ベクトル
                                       (mapcar '- p_fro1 p_fro0)))
                  dist_plane(apply '+(mapcar '* p_fro0 vec_norm)));面高さ

            (cond
             ((<(abs(-(apply '+(mapcar '* p_rea1 vec_norm))dist_plane))1e-8);;ねじれがないとき
              (list(mapcar '(lambda(a)(* 0.25(apply '+(mapcar 'a ls_point))))
                           (list car cadr caddr))
                   vec_norm  (trans-x(list 1 0 0)vec_norm(list 0 0 1)))
              );;中点 法線 面内ベクトルのリスト
             
             );;cond
            )
         ls_p_fro(append(cdr ls_p_fro)(list(car ls_p_fro)))
         ls_p_rea(append(cdr ls_p_rea)(list(car ls_p_rea)))
         )
        )

  ;;行番号リストの作成
  (setq ls_code(list) ;;ls_code
        num_line(* 3 num_point)num_surf(+ 2 num_point)
        ls_num_code(list num_surf num_surf num_surf num_surf(* 2 num_line)
                           num_line num_line(* 2 num_point)(* 2 num_point)))
  (setq ii -2
        ls_sum_code
        (mapcar '(lambda ( n / i a  )
                   (setq a 5 i -1 n(1+ n))
                   (while(<(setq i(1+ i))n)(setq a(+ a(nth i ls_num_code))))
                   (progn a)   )
                (mapcar '(lambda(a)(setq ii(1+ ii)))(cons 0 ls_num_code))))

  (setq ii num_surf ls_repeat(list))
  (while(>(setq ii(1- ii))-1)(setq ls_repeat(cons ii ls_repeat)))
  (setq p1(car ls_p_fro)p2(car ls_p_rea))
  (setq ls_entna_surf
        (mapcar
         '(lambda(vec_norm ls_surfname ls_entna_switch i
                           / ls_polyp entna_poly vec_norm_poly str_name
                           vec_x vec_y p_cent)

            (setq str_name(car ls_surfname)
                  entna_poly(car ls_entna_switch)
                  ls_entna_switch(cdr ls_entna_switch))
            ;;(if(wcmatch(car ls_surfname)"*ハンチ*")(princ(car ls_surfname)))
            (cond
             ((=(car ls_surfname)"")(setq entna_poly nil tmL nil))
             ((=(length vec_norm)3)
              (setq vec_norm_poly(if(= i 0)vec_norm
                                   (if(= i 1)(mapcar '- vec_norm)(cadr vec_norm))))

              (setq p_cent(if(= i 0)p_cent_fro (if(= i 1)p_cent_rea (car vec_norm)))
                    vec_x(trans-x(list 1 0 0)vec_norm_poly(list 0 0 1))
                    vec_y(trans-x(list 0 1 0)vec_norm_poly(list 0 0 1))
                    ls_polyp(mapcar '(lambda(a)
                                       (mapcar '(lambda(b c)(+ b(* ratio_boxlen 500 c)))
                                               p_cent a))
                                    (list vec_x(mapcar '- vec_y)(mapcar '- vec_x)vec_y)) )
              (if entna_poly(poly_edit ls_polyp entna_poly)
                (setq entna_poly(poly_entm ls_polyp 1(list str_name(* ratio_boxlen 0.25 500)))))
              (mapcar
               '(lambda(a / e hand_)
                  (if a(if(setq e(entgetxr a))
                           (progn
                             (setq hand_(handent(xdata_key_search(cadr(assoc -3 e)) "FN" 2)))
                             (mapcar 'entdel(list a hand_))
                             )))) ls_entna_switch);;
              (list(cdr(assoc 5(entget entna_poly))) )
              )
             (T;;ねじれがあるとき(使わないようにする)
              (setq h210(car vec_norm))
              (setq pp(mapcar '(lambda(a)(*(apply '+(mapcar 'a(cdr vec_norm)))0.0625))
                              (list car cadr caddr))
                    
                    vx(trans-x(list 1 0 0)h210(list 0 0 1))
                    vy(trans-x(list 0 1 0)h210(list 0 0 1))
                    ls_polyp(mapcar '(lambda(a)
                                  (mapcar '(lambda(b c)(+ b(* ratio_boxlen 500 c)))
                                          pp a))
                               (list vx(mapcar '- vy)(mapcar '- vx)vy)) )

              (if tm(poly_edit ls_polyp tm)
                (setq tm(poly_entm ls_polyp 1(list str_name(* ratio_boxlen 0.25 500)))))
              (setq ed(entget tm) ha(cdr(assoc 5 ed))
                    p1(nth(- i 2)ls_p_fro)p2(nth(rem(- i 1)num_point)ls_p_fro)
                    p3(nth(rem(- i 1)num_point)ls_p_rea)p4(nth(- i 2)ls_p_rea))

              (setq str_name(strcat str_name "-"(chr 32257)) ;;縁
                    ii 0
                    tmL(mapcar
                        '(lambda(a b  / p v tm vx vy pL tt)
                           (setq p(mapcar '(lambda(a b)(* 0.5(+ a b)))a b)
                                 v(mapcar '- a b)
                                 v(unit_vector(cross_product v(cross_product h210 v)))
                                 vx(trans-x(list 1 0 0)v(list 0 0 1))
                                 vy(trans-x(list 0 1 0)v(list 0 0 1))
                                 tt(strcat str_name(itoa(setq ii(1+ ii))))
                                 pL(mapcar
                                    '(lambda(a)
                                       (mapcar '(lambda(b c)
                                                  (+ b(* ratio_boxlen 500 c)))p a));;
                                    (list vx(mapcar '- vy)(mapcar '- vx)vy)) )
                           (if(setq tm(car ls_entna_switch))(poly_edit pL tm)
                             (setq tm(poly_entm pL 1
                                                (list tt(* ratio_boxlen 0.25 500)))))
                           (setq ls_entna_switch(cdr ls_entna_switch))
                           (cdr(assoc 5(entget tm)))  )
                        (list p1 p2 p3 p4)(list p2 p3 p4 p1))
                    
                    tmL(cons ha tmL)
                    )
              )
             )
            
            )
         (append(list vec_norm_fro vec_norm_rea)ls_vec_norm)
         ls_surf_name ls_surf_entna(inclist 0(+ num_point 2))))
  

  ;;((lambda(tt / t1 )(while(/= tt "")(setq t1(substr tt 1 1)tt(substr tt 2))(princ(chr(- 159(ascii t1))))))"")

  (if command_for_alter
      (setq scd_reversedsingle "reversed single #"
            scd_forwardsingle "forward single #"
            scd_history "persubent-acadSolidHistory-attrib $-1 -1 $-1 $-1 $"
            scd_face "face $"                  scd_loop "loop $-1 -1 $-1 $"
            scd_conesurface "cone-surface $-1 -1 $-1 "
            scd_torussurface "torus-surface $-1 -1 $-1 "
            scd_coedge "coedge $-1 -1 $-1 $"   scd_edge "edge $"
            scd_ellipsecurve "ellipse-curve $-1 -1 $-1 "
            scd_point "point $-1 -1 $-1 "      scd_vertex "vertex $-1 -1 $-1 $"
            scd_planesurface "plane-surface $-1 -1 $-1 "
            scd_forwardvi "forward_v I I I I #"
            scd_forwardi "forward I I I I #"
            scd_forwardunknown "forward @7 unknown #"
            scd_target "forward @7 tangent #"
            scd_straightcurve "straight-curve $-1 -1 $-1 "
            scd_reversed "reversed $"          scd_forward "forward $"
            _$ "$" _# "#" _-1 "-1 " _0 "0 ")
    (setq scd_reversedsingle "-:):-,:; ,6183: |"
          scd_forwardsingle "90-(>-; ,6183: |"
          scd_history "/:-,*=:1+r><>;L036;W6,+0-&r>++-6= {rn rn {rn {rn {"
          scd_face "9><: {"                    scd_loop "300/ {rn rn {rn {"
          scd_conesurface "<01:r,*-9><: {rn rn {rn "
          scd_torussurface "+0-*,r,*-9><: {rn rn {rn "
          scd_coedge "<0:;8: {rn rn {rn {"     scd_edge ":;8: {"
          scd_ellipsecurve ":336/,:r<*-): {rn rn {rn "
          scd_point "/061+ {rn rn {rn "        scd_vertex "):-+:' {rn rn {rn {"
          scd_planesurface "/3>1:r,*-9><: {rn rn {rn "
          scd_forwardvi "90-(>-;@) V V V V |"
          scd_forwardi "90-(>-; V V V V |"
          scd_forwardunknown "90-(>-; _h *1410(1 |"
          scd_target "90-(>-; _h +>18:1+ |"
          scd_straightcurve ",+->687+r<*-): {rn rn {rn "
          scd_reversed "-:):-,:; {"            scd_forward "90-(>-; {"
          _$ "{" _# "|" _-1 "rn " _0 "o " )
    )
  
  (setq ls_codefin(append(if command_for_alter(list(cons 1 "End-of-ASM-data ")))
                        (list(cons 290 1)
                             (cons 2 "{00000000-0000-0000-0000-000000000000}")
                             (cons 100 "AcDb3dSolid"))
                        (if command_for_alter(list)(list(cons 350(entlast))))
                        ))
  
  (setq ls_codetrans
        (if command_for_alter
            "transform $-1 -1 1 0 0 0 1 0 0 0 1 0 0 0 1 no_rotate no_reflect no_shear #"
          "+->1,90-2 {rn rn n o o o n o o o n o o o n 10@-0+>+: 10@-:93:<+ 10@,7:>- |")
        ls_codetrans(list(cons 1 ls_codetrans)))

  (setq ls_code
        (append;;面1
         ls_code
         (mapcar
          '(lambda(i)
             (cons 1(strcat scd_face(int_to_codestr(+(nth 1 ls_sum_code)i))
                            _-1 _$ _-1 _$
                            (if(= i(1- num_surf)) _-1
                              (int_to_codestr(+(nth 0 ls_sum_code)i 1)))
                            _$(int_to_codestr(+(nth 2 ls_sum_code)i))
                            _$(int_to_codestr 3)
                            _$ _-1
                            _$(int_to_codestr(+(nth 3 ls_sum_code)i))
                            (if(= i 1)scd_forwardsingle scd_reversedsingle)) )  )
          ls_repeat)))
  
  (setq ls_code
        (append;;面2
         ls_code
         (mapcar
          '(lambda(i)
             (cons 1(strcat  scd_history
                             (int_to_codestr(+(nth 0 ls_sum_code)i))
                             (if command_for_alter "1 1 1 1 #" "n n n n |"))))
          ls_repeat)))
  
  (setq ls_code
        (append;;面3
         ls_code
         (mapcar
          '(lambda(i)
             (cons 1(strcat scd_loop _-1 _$
                            (int_to_codestr
                             (+(nth 4 ls_sum_code)
                               (if(= i 0)0
                                 (if(= i 1)num_point
                                   (+(* 2 num_point)1(* 4(- i 2)))))))
                            _$(int_to_codestr(+(nth 0 ls_sum_code)i))_#)) )
          ls_repeat)))
  
  (setq ls_1(append p_cent_fro
                    vec_norm_fro
                    (unit_vector(mapcar '-(cadr ls_p_fro)(car ls_p_fro))))
        ls_2(append p_cent_rea
                    vec_norm_rea
                    (unit_vector(mapcar '-(cadr ls_p_rea)(car ls_p_rea))))
        ls_code
        (append ls_code
                (list(cons 1(strcat scd_planesurface
                                    (lsreal_to_codestr ls_1)
                                    scd_forwardvi))
                     (cons 1(strcat scd_planesurface
                                    (lsreal_to_codestr ls_2)
                                    scd_forwardvi)))))
  
  (if command_for_alter
      (setq str_1 "spline-surface $-1 -1 $-1 forward { loft_spl_sur 22800 2 "
            str_2 " 1 1 straight 0 0 0 1 0 0 F 0 F 0 null_surface nullbs F 213 1 1 0 1 0 0 0 0 F null_curve 0 -1 "
            str_3 " F 1 F 0 F 1 F 0 F F F F 0 0 full nubs 3 3 open open none none 2 2 " )
    (setq str_1 ",/361:r,*-9><: {rn rn {rn 90-(>-; $ 309+@,/3@,*- mmgoo m "
          str_2 " n n ,+->687+ o o o n o o Y o Y o 1*33@,*-9><: 1*33=, Y mnl n n o n o o o o Y 1*33@<*-): o rn "
          str_3 " Y n Y o Y n Y o Y Y Y Y o o 9*33 1*=, l l 0/:1 0/:1 101: 101: m m " ))
  
  (setq ls_int_distortion(list)ii(+(nth 2 ls_sum_code)1))
  (mapcar
   '(lambda( vec_norm )
      (setq ii(1+ ii))
      (cond
       ((=(length vec_norm)3)
        (setq ls_int_distortion(append ls_int_distortion(list 0))
              ls_code(append ls_code
                             (list(cons 1(strcat scd_planesurface
                                                 (lsreal_to_codestr(apply 'append vec_norm))
                                                 scd_forwardvi))))))
       (T;;ねじれ
        (setq vec_norm(cdr vec_norm)
              ls_int_distortion(append ls_int_distortion(list ii))
              ls_code (append ls_code
                              (mapcar '(lambda(a)(cons 1 a))
                                      (if command_for_alter
                                          (list str_1 " 0 " str_2 " 1 " str_2 " 0 " str_3
                                                " 0 3 1 3 " " 0 3 1 3 ") 
                                        (list str_1 " o " str_2 " n " str_2 " o " str_3
                                              " o l n l " " o l n l ") ))
                              (mapcar '(lambda(a)(cons 1(strcat " "(lsreal_to_codestr a))))vec_norm)
                              (mapcar '(lambda(a)(cons 1 a))
                                      (if command_for_alter
                                          (list " 0 " " " " 0 " " 0 " " 0 " " 0 "
                                                " 0 " " 0 " " F } I I I I #")
                                        (list " o " " " " o " " o " " o " " o "
                                              " o " " o " " Y \" V V V V |")
                                        ))
                              
                              )
              )
        
        )
       )
      )ls_vec_norm);;

  (setq fee(list)int_count_rowcode(1-(last ls_sum_code)))
  (setq ii num_point ls_repeat(list))
  (while(>(setq ii(1- ii))-1)(setq ls_repeat(cons ii ls_repeat)))
  (setq str_1(strcat scd_reversed(int_to_codestr(nth 2 ls_sum_code))))
  (setq ls_code(append;;辺と隣の面-上
                ls_code(mapcar
                        '(lambda(i / jn j2 k1 k2 k3 k4 k5 te)
                           (setq k1(+(nth 4 ls_sum_code)(if(= i 0)num_point i)-1)
                                 k2(+(nth 4 ls_sum_code)(if(= i(1- num_point))0(1+ i)))
                                 k3(+(nth 4 ls_sum_code)(* 2 num_point)(* i 4))
                                 k4(+(nth 5 ls_sum_code)i) )
                           (cons 1(strcat scd_coedge(int_to_codestr k1)
                                          _$(int_to_codestr k2)
                                          _$(int_to_codestr k3)
                                          _$(int_to_codestr k4)
                                          str_1 _0 _$ _-1 _#))
                           ) ls_repeat)));;

  (setq str_1(strcat scd_forward(int_to_codestr(1+(nth 2 ls_sum_code)))))
  (setq ls_code
        (append;;辺と隣の面-上
         ls_code(mapcar
                 '(lambda(i / jn j2 k1 k2 k3 k4 k5 te)
                    (setq k1(+(nth 4 ls_sum_code)(if(= i(1- num_point))0(1+ i))num_point)
                          k2(+(nth 4 ls_sum_code)(if(= i 0)num_point i)num_point -1)
                          k3(+(nth 4 ls_sum_code)(* 2 num_point)1(* i 4))
                          k4(+(nth 5 ls_sum_code)i num_point))
                    (cons 1(strcat scd_coedge(int_to_codestr k1)
                                   _$(int_to_codestr k2)
                                   _$(int_to_codestr k3)
                                   _$(int_to_codestr k4)
                                   str_1 _0 _$ _-1 _#))
                    ) ls_repeat)));;
  
  (setq ls_code
        (append
         ls_code
         (apply 'append
                (mapcar
                 '(lambda(i / k0 k1 k20 k21 k3 k4 k5 tdL)
                    (setq k5(+(nth 2 ls_sum_code)2 i)
                          k0(+(nth 4 ls_sum_code)(* 2 num_point)(* i 4))
                          k1(+(nth 4 ls_sum_code)i)
                          k20(if(= i 0)(1-(nth 5 ls_sum_code))(1- k0))
                          k21(if(= i(1- num_point))
                                 (+(nth 4 ls_sum_code)(* 2 num_point)2)
                               (+ k0 6))
                          k3(+(nth 5 ls_sum_code)i)
                          k4(+(nth 5 ls_sum_code)num_point
                              i 1(if(= i(1- num_point))0 num_point))
                          tdL(if(null(vl-position k5 ls_int_distortion))
                                 (list _-1 _-1 _-1 _-1)
                               (mapcar
                                '(lambda(a)
                                   (setq fee(append fee(list a))
                                         int_count_rowcode(1+ int_count_rowcode) )
                                   (int_to_codestr int_count_rowcode)
                                   )
                                (list k3(+ k3 num_point)(+ k3 num_point num_point)k4)))
                          )
                    (list(cons 1(strcat
                                 scd_coedge(int_to_codestr(+ k0 3))
                                 _$(int_to_codestr(+ k0 2))
                                 _$(int_to_codestr k1 )
                                 _$(int_to_codestr(+ k3 0))
                                 scd_forward(int_to_codestr k5)_0
                                 _$(nth 0 tdL)_#))
                         (cons 1(strcat
                                 scd_coedge(int_to_codestr(+ k0 2))
                                 _$(int_to_codestr(+ k0 3))
                                 _$(int_to_codestr(+ k1 num_point))
                                 _$(int_to_codestr(+ k3 num_point))
                                 scd_reversed(int_to_codestr k5)_0
                                 _$(nth 1 tdL)_#))
                         (cons 1(strcat
                                 scd_coedge(int_to_codestr(+ k0 0))
                                 _$(int_to_codestr(+ k0 1))
                                 _$(int_to_codestr k20)
                                 _$(int_to_codestr(+ k3 num_point num_point))
                                 scd_forward(int_to_codestr k5)_0
                                 _$(nth 2 tdL)_#))
                         (cons 1(strcat
                                 scd_coedge(int_to_codestr(+ k0 1))
                                 _$(int_to_codestr(+ k0 0))
                                 _$(int_to_codestr k21)
                                 _$(int_to_codestr k4)
                                 scd_reversed(int_to_codestr k5)_0
                                 _$(nth 3 tdL)_#))
                         )
                    )
                 ls_repeat))))

  (setq fee(mapcar '(lambda(a)(fix(- a(nth 5 ls_sum_code))))fee))
  
  (setq ls_code
        (append;;辺の長さ-下
         ls_code
         (mapcar
          '(lambda(i d / s jt ds)
             (if(null(vl-position i fee))T
               (setq fee(subst(edge_of_dstt d)i fee)))
             
             (setq jt(if(= i(1- num_point))(int_to_codestr(nth 7 ls_sum_code))
                       (int_to_codestr(+(nth 7 ls_sum_code)1 i)))
                   ds(lsreal_to_codestr(list d)))
             (cons 1(strcat scd_edge _-1 _-1 _$ _-1 _$
                            (int_to_codestr(+(nth 7 ls_sum_code)i))_0
                            _$ jt ds
                            _$(int_to_codestr
                               (+(nth 4 ls_sum_code)(* 2 num_point)(* 4 i)))
                            _$(int_to_codestr
                               (+(nth 6 ls_sum_code)i))
                            scd_forwardunknown)))
          ls_repeat ls_distp_fro)))
  
  (setq ls_code
        (append;;辺の長さ-上
         ls_code
         (mapcar
          '(lambda(i d / s jt ds)
             (if(null(vl-position(+ i num_point)fee))T
               (setq fee(subst(edge_of_dstt d)(+ i num_point)fee)))
             (setq jt(if(= i(1- num_point))(int_to_codestr(+(nth 7 ls_sum_code)num_point))
                       (int_to_codestr(+(nth 7 ls_sum_code)num_point 1 i)))
                   ds(lsreal_to_codestr(list d)))
             (cons 1(strcat scd_edge _-1 _-1 _$ _-1
                            _$(int_to_codestr
                               (+(nth 7 ls_sum_code)i num_point))
                            _0 _$ jt ds
                            _$(int_to_codestr
                               (+(nth 4 ls_sum_code)1(* 2 num_point)(* 4 i)))
                            _$(int_to_codestr
                               (+(nth 6 ls_sum_code)i num_point))
                            scd_forwardunknown)))
          ls_repeat ls_distp_rea )))

  (setq ls_code
        (append;;辺の長さ-縦
         ls_code
         (mapcar
          '(lambda(i d  / tt ds k3);;18:1
             (if(null(vl-position(+ i num_point num_point)fee))T
               (setq fee(subst(edge_of_dstt d)(+ i num_point num_point)fee)))
             ;; (setq tt scd_forwardunknown)
             (setq ds(lsreal_to_codestr(list d))
                   k3(int_to_codestr
                      (if(= i 0)(+(nth 5 ls_sum_code)-1)
                        (+(nth 4 ls_sum_code)(* 2 num_point)2(* 4 i))) ))
             (cons 1(strcat scd_edge _-1 _-1 _$ _-1
                            _$(int_to_codestr(+(nth 7 ls_sum_code)i num_point))_0
                            _$(int_to_codestr(+(nth 7 ls_sum_code)i)) ds;;長さ
                            _$ k3
                            _$(int_to_codestr
                               (+(nth 6 ls_sum_code)i num_point num_point))
                            scd_forwardunknown)))
          ls_repeat ls_depth_p )))
  
  (setq ls_code
        (append;;線の向き下
         ls_code
         (mapcar
          '(lambda(p1 p2 d / tt)
             (setq tt(lsreal_to_codestr(append p1(unit_vector(mapcar '- p2 p1)))))
             (cons 1(strcat scd_straightcurve tt
                            (if command_for_alter "F 0 F " "Y o Y ")
                            (lsreal_to_codestr(list d))_#)))
          ls_p_fro(append(cdr ls_p_fro)(list(car ls_p_fro)))ls_distp_fro)))

  (setq ls_code
        (append;;線の向き上
         ls_code
         (mapcar
          '(lambda(p1 p2 d / tt)
             (setq tt(lsreal_to_codestr(append p1(unit_vector(mapcar '- p2 p1)))))
             (cons 1(strcat scd_straightcurve tt
                            (if command_for_alter "F 0 F " "Y o Y ")
                            (lsreal_to_codestr(list d))_#)))
          ls_p_rea(append(cdr ls_p_rea)(list(car ls_p_rea)))ls_distp_rea)))

  (setq ls_code
        (append;;線の向き縦
         ls_code
         (mapcar
          '(lambda(p1 p2 / tt)
             (setq tt(lsreal_to_codestr(append p1(unit_vector(mapcar '- p2 p1)))))
             (cons 1(strcat scd_straightcurve tt
                            (if command_for_alter " I I #" " V V |"))))
          ls_p_fro ls_p_rea)))
  
  (setq ls_code
        (append;;点と辺-下
         ls_code
         (mapcar
          '(lambda(i x)
             (cond
              ((= i 0)
               (cons 1(strcat scd_vertex
                              (int_to_codestr(+(nth 5 ls_sum_code)num_point -1))
                              (if command_for_alter " 1 $" " n {")
                              (int_to_codestr(+(nth 8 ls_sum_code)i))" " _#)))
              (T
               (cons 1(strcat scd_vertex
                              (int_to_codestr(+(nth 5 ls_sum_code)i))" " _0
                              _$(int_to_codestr(+(nth 8 ls_sum_code)i))" " _#)))
              ))
          ls_repeat ls_p_fro)))
  
  (setq ls_code
        (append;;点と辺-上
         ls_code
         (mapcar
          '(lambda(i x)
             (cond
              ((= i 0)
               (cons 1(strcat scd_vertex
                              (int_to_codestr(+(nth 5 ls_sum_code)num_point num_point -1))
                              (if command_for_alter " 1 $" " n {")
                              (int_to_codestr(+(nth 8 ls_sum_code)i num_point))" " _#)))
              (T
               (cons 1(strcat scd_vertex(int_to_codestr(+(nth 5 ls_sum_code)i num_point))
                              (if command_for_alter " 0 $" " o {")
                              (int_to_codestr(+(nth 8 ls_sum_code)i num_point))" "_#)))
              ))
          ls_repeat ls_p_fro)))

  (setq ls_code(append ls_code
                       (mapcar ;;点-下
                        '(lambda(p)(cons 1(strcat scd_point(lsreal_to_codestr p)_#)))ls_p_fro)
                       (mapcar;;点-上
                        '(lambda(p)(cons 1(strcat scd_point(lsreal_to_codestr p)_#)))ls_p_rea)
                       ))
  
  (setq str_count(int_to_codestr(1+ int_count_rowcode)))
  
  (setq ls_codehead
        (if command_for_alter
            (list
             (cons 0 "3DSOLID")
             (cons 100 "AcDbEntity")(cons 100 "AcDbModelerGeometry")(cons 70 1)
             (cons 1 (strcat "22300 " str_count "2 1         "))
             (cons 1 "16 Autodesk AutoCAD 20 ASM 229.8.0.65535 NT 0")
             (cons 1 "1 9.9999999999999995e-007 1e-010 ") 
             (cons 1 "asmheader $-1 -1 @13 229.8.0.65535 #") 
             (cons 1 "body $-1 -1 $-1 $2 $-1 $4 #") 
             (cons 1 "lump $-1 -1 $-1 $-1 $3 $1 #") 
             (cons 1 "shell $-1 -1 $-1 $-1 $-1 $5 $-1 $2 #") 
             )
          (list
           (cons 0 "3DSOLID")
           (cons 100 "AcDbEntity")(cons 100 "AcDbModelerGeometry")(cons 70 1)
           (cons 1 (strcat "mmloo " str_count "m k         "))
           (cons 1 "ni ^*+0;:,4 ^*+0\\^[ mo ^LR mmjqnqoqijjlj QK o  ")
           (cons 1 "n fqfffffffffffffffjkh:roh nqooooooooooooooooli:rno ")
           (cons 1 ">,27:>;:- {rn rn _nl mmjqnqoqijjlj |")
           (cons 1 "=0;& {rn rn {rn {m {rn {k |")
           (cons 1 "3*2/ {rn rn {rn {rn {l {n |")
           (cons 1 ",7:33 {rn rn {rn {rn {rn {j {rn {m |")
           )
          )
        )

  
  (setq entna_sld(entmakex(append ls_codehead
                                  ls_codetrans
                                  ls_code(apply 'append fee)
                                  ls_codefin)));;faafbbfcc
  (cons entna_sld ls_entna_surf)
  )



(defun bendpipe_sld;;鉄筋ソリッドコード作成
    ( rr ppL cmL vdL na ls_x / ii jj num tt rt psL p1 p2 p3 p4 pd mg ft tee txls nis
         scd_reversedsingle scd_forwardsingle scd_history scd_face scd_loop
         scd_conesurface scd_torussurface scd_coedge scd_edge scd_ellipsecurve
         scd_point scd_vertex scd_planesurface ngf scd_forwardvi scd_forwardi
         t-31 t314 scd_forwardunknown scd_target ls_codehead ls_codefin
         at11 gyou ftL ef1 ef2 n t1 t2 bo
         eb1 eb2 pc ph phr pac xda n1 n2 n3 n4 at21 at22 at23 ds aa bb cc i ve
         h210 )
  (setq vdL(mapcar '(lambda(a)(mapcar '- a))vdL))

  (setq num(length ppL)ls_code(list));ii -1 psL(list))
  (setq snL(list (1+ num)(1+ num)(* 2 num)(1+ num)(* 2 num)num num num num)
        ii -2 sunL(mapcar '(lambda ( n / i a )
                             (setq a 4 i -1 n(1+ n))
                             (while(<(setq i(1+ i))n)(setq a(+ a(nth i snL))))
                             (progn a)   )
                          (mapcar '(lambda(a)(setq ii(1+ ii)))snL)))

  (setq ft(int_to_codestr(+ 7(* 11 num))) i -2 stL(list))

  (setq rt(lsreal_to_codestr(list rr)))

  (if(null na)(setq na(getvar "CLAYER")))
  (if command_for_alter
      (setq scd_reversedsingle "reversed single #"         scd_forwardsingle "forward single #"
            scd_history "persubent-acadSolidHistory-attrib $-1 -1 $-1 $-1 $"
            scd_face "face $"  scd_loop "loop $-1 -1 $-1 $"
            scd_conesurface "cone-surface $-1 -1 $-1 "  scd_torussurface "torus-surface $-1 -1 $-1 "
            scd_coedge "coedge $-1 -1 $-1 $"       scd_edge "edge $"
            scd_ellipsecurve "ellipse-curve $-1 -1 $-1 " scd_point "point $-1 -1 $-1 "
            scd_vertex "vertex $-1 -1 $-1 $"       scd_planesurface "plane-surface $-1 -1 $-1 "
            scd_forwardvi "forward_v I I I I #"       scd_forwardi "forward I I I I #"
            scd_forwardunknown "forward @7 unknown #"      scd_target "forward @7 tangent #"
            _$ "$" _# "#" _-1 "-1 " _0 "0 "
            ls_codehead(list
                (cons 0 "3DSOLID")(cons 8 na)(cons 100 "AcDbEntity")
                (cons 100 "AcDbModelerGeometry")(cons 70 1)
                (cons 1 (strcat "22300 " ft "2 1         "))
                (cons 1 "16 Autodesk AutoCAD 20 ASM 229.8.0.65535 NT 0")
                (cons 1 "1 9.9999999999999995e-007 1e-010 ") 
                (cons 1 "asmheader $-1 -1 @13 229.8.0.65535 #") 
                (cons 1 "body $-1 -1 $-1 $2 $-1 $-1 #") 
                (cons 1 "lump $-1 -1 $-1 $-1 $3 $1 #") 
                (cons 1 "shell $-1 -1 $-1 $-1 $-1 $4 $-1 $2 #") 
                ))
    (setq scd_reversedsingle "-:):-,:; ,6183: |"          scd_forwardsingle "90-(>-; ,6183: |"
          scd_history "/:-,*=:1+r><>;L036;W6,+0-&r>++-6= {rn rn {rn {rn {"
          scd_face "9><: {" scd_loop "300/ {rn rn {rn {"
          scd_conesurface "<01:r,*-9><: {rn rn {rn "   scd_torussurface "+0-*,r,*-9><: {rn rn {rn "
          scd_coedge "<0:;8: {rn rn {rn {"        scd_edge ":;8: {"
          scd_ellipsecurve ":336/,:r<*-): {rn rn {rn "  scd_point "/061+ {rn rn {rn "
          scd_vertex "):-+:' {rn rn {rn {"        scd_planesurface "/3>1:r,*-9><: {rn rn {rn "
          scd_forwardvi "90-(>-;@) V V V V |"        scd_forwardi "90-(>-; V V V V |"
          scd_forwardunknown "90-(>-; _h *1410(1 |"       scd_target "90-(>-; _h +>18:1+ |"
          _$ "{" _# "|" _-1 "rn " _0 "o "
          ls_codehead(list
              (cons 0 "3DSOLID")(cons 8 na)(cons 100 "AcDbEntity")
              (cons 100 "AcDbModelerGeometry")(cons 70 1)
              (cons 1 (strcat "mmloo " ft "m n         "))
              (cons 1 "ni ^*+0;:,4 ^*+0\\^[ mo ^LR mmjqjqoqijjlj QK o  ")
              (cons 1 "n fqfffffffffffffffjkh:roh nqooooooooooooooooli:rno ")
              (cons 1 ">,27:>;:- {rn rn _nl mmjqjqoqijjlj |")
              (cons 1 "=0;& {rn rn {rn {m {rn {rn |")
              (cons 1 "3*2/ {rn rn {rn {rn {l {n |")
              (cons 1 ",7:33 {rn rn {rn {rn {rn {k {rn {m |")
              )
          )
    )

  (setq ls_codefin(append(if command_for_alter(list(cons 1 "End-of-ASM-data ")))
                  (list(cons 290 1)
                       (cons 2 "{00000000-0000-0000-0000-000000000000}")
                       (cons 100 "AcDb3dSolid"))
                  (if command_for_alter(list)(list(cons 350(entlast))))
                  (if ls_x(list(list -3(list(car ls_x)(cons 1000(cadr ls_x))(cons 1005(caddr ls_x))))))
                  ))
  
  (setq ii(* 2 num)iiL(list))(while(>(setq ii(1- ii))-1)(setq iiL(cons ii iiL)))
  (setq
   ls_code(append
           ls_code
           (mapcar;;面1
            '(lambda(i a / n1 n2 tt)
               (if(= i num)(setq n1 _-1 tt scd_reversedsingle)
                 (setq n1(int_to_codestr(+(nth 0 sunL)i 1))tt scd_forwardsingle))
               (if(< i(1- num))(setq n2(int_to_codestr(+(nth 2 sunL)num i 1)))
                 (setq n2(int_to_codestr(+(nth 2 sunL)i))))
               
               (cons 1(strcat scd_face(int_to_codestr(+(nth 1 sunL)i)) _-1 _$ _-1 _$  n1 _$
                              n2 _$(int_to_codestr 3)_$ _-1 _$(int_to_codestr(+(nth 3 sunL)i))tt)) )
            iiL(cons 0 ppL))
           (mapcar;;面2
            '(lambda(i a)
               (cons 1(strcat scd_history(int_to_codestr(+(nth 0 sunL)i))
                              (if command_for_alter
                                  "1 1 1 1 #" "n n n n |"))))
            iiL(cons 0 ppL))
           (mapcar;;面3
            '(lambda(i / n1 n2)
               (if(<= i num)(setq n1 _-1 n2(int_to_codestr(+(nth 0 sunL)i)))
                 (setq n1(int_to_codestr(+(nth 2 sunL)i(- num)-1))
                       n2(int_to_codestr(+(nth 0 sunL)i(- num)-1)) ))
               
               (cons 1(strcat
                       scd_loop n1 _$(int_to_codestr(+(nth 4 sunL)i))_$ n2 _#)))
            iiL)))

  (setq xvL(mapcar '(lambda(a)(trans-x(list 1 0 0)a(list 0 0 1)))vdL)
        zvL(mapcar '(lambda(a)(trans-x(list 0 1 0)a(list 0 0 1)))vdL))

  
  (setq xzvL(mapcar
             '(lambda(a b c d i / xv)
                (cond
                 (b(list(setq xv(unit_vector(mapcar '- a b)))(cross_product c xv)))
                 (d(list(setq xv(unit_vector(mapcar '- a d)))(cross_product c xv)))
                 (T nil)
                 )
                )ppL cmL vdL(cons nil cmL)iiL);;
        xaL(mapcar 'car xzvL) zaL(mapcar 'cadr xzvL));;za-torasの軸

  (if(=(length ppL)2)(setq bo nil)
    (setq h210(unit_vector(cross_product(mapcar '-(cadr ppL)(car ppL))
                                (mapcar '-(caddr ppL)(car ppL))))
          d38(apply '+(mapcar '* h210(car ppL)))
          bo(apply 'and(mapcar
                        '(lambda(a)
                           (<(abs(-(apply '+(mapcar '* a h210))d38))1e-8))
                        ppL))))


  (if bo(setq ve(mapcar '(lambda(a)(mapcar '(lambda(a)(* a rr))h210))ppL)vc ve)
    (setq
     ve(mapcar
        '(lambda(i a b x z xa za / d p dr vx vy)
           (if(= i 0)
               (setq p(mapcar '(lambda(b c)(+ b(* c rr)))a z))
             (if pb(setq p(mapcar '(lambda(a b c)(+ a(* cx b)(* cz c)))a xa za))
               (setq p(mapcar '(lambda(a b c)(+ a(* cx b)(* cz c)))a x z)) ) )
           (setq dr(mapcar '- p a)pb b)
           (if b(setq cz(apply '+(mapcar '* za dr))
                      cx(apply '+(mapcar '* xa dr)))
             ;; dr(unit_vector
             ;; (mapcar '(lambda(a b c)(+ a b(* -2.0 c)))a pn b)))
             (setq cz(apply '+(mapcar '* z dr))cx(apply '+(mapcar '* x dr))))
           (progn dr)
           )
        iiL ppL cmL xvL zvL xaL zaL);;(append(cdr ppL)(list 0)))
     vc(mapcar '(lambda(a b)(if b(cross_product a b)))vdL ve)
     );;
    );;
  
  (setq xrL(mapcar '(lambda(a)(trans-x(list rr 0 0)a(list 0 0 1)))vdL)
        vc xrL
        agedL(mapcar '(lambda(a b c / cc ss ag)
                        (setq cc(apply '+(mapcar '* a b)) ss(sin_abscprod b a c)
                              ag(atan ss cc))
                        (while(and(> ag (* -1 pi))(>(abs(+ ag pi))1e-6))
                          (setq ag(- ag(* 2 pi))))
                        ag
                        )
                     (mapcar '(lambda(a)(mapcar '- a))ve) xrL vdL)
        )

  (setq ii 0)
  (setq
   ls_code(append
           ls_code(mapcar;;サーフェス
                   '(lambda(i a b c x z e cc / dh mg x )
                      (if b
                          (cons 1(strcat scd_torussurface(lsreal_to_codestr(append b z(list(distance a b)rr)x))scd_forwardvi))
                        (cons 1(strcat scd_conesurface(lsreal_to_codestr(append a c e))
                                       (if command_for_alter "1 I I 0 1 " "n V V o n ")
                                       rt scd_forwardi))))
                   iiL ppL cmL vdL xaL zaL xrL(cdr ppL))));;
  
  (setq xL1(lsreal_to_codestr(append(car ppL)(car vdL)(car zvL))) ;;蓋
        xL2(lsreal_to_codestr(append(last ppL)(last vdL)(last zvL)))
        ls_code(append ls_code(list(cons 1(strcat scd_planesurface xL1 scd_forwardvi))
                           (cons 1(strcat scd_planesurface xL2 scd_forwardvi)))))

  (setq
   ls_code(append
           ls_code
           (mapcar;;辺と面;;エッジ
            '(lambda(i / n1 n2 n3 n4 tt)
               (setq n1(int_to_codestr(+(nth 4 sunL)i))n4(int_to_codestr(+(nth 2 sunL)i)))
               (if(< i num)
                   (setq tt(if command_for_alter "forward $" "90-(>-; {")
                         n2(int_to_codestr(+(nth 4 sunL)(rem(+ i 2)num)num))
                         n3(int_to_codestr(+(nth 5 sunL)(rem(+ i 1)num))) )
                 (setq tt(if command_for_alter "reversed $" "-:):-,:; {")
                       n2(int_to_codestr(+(nth 4 sunL)(rem(- i 2)num)))
                       n3(int_to_codestr(+(nth 5 sunL)(rem(- i 1)num))) )
                 )
               (cons 1(strcat scd_coedge n1 _$ n1 _$ n2 _$ n3 tt n4 _0 _$ _-1 _#))
               )
            iiL)
           (mapcar;;エッジ
            '(lambda(i a ag / tt n1 n2 n3 n4 ag1 ag0)
               (setq ag1(+ ag(* 2 pi))ag0(lsreal_to_codestr(list ag))ag1(lsreal_to_codestr(list ag1)))
               (setq n2(int_to_codestr(+(nth 7 sunL)i))n4(int_to_codestr(+(nth 6 sunL)i)))
               (if(= i(1- num))(setq n3(int_to_codestr(+(nth 4 sunL)num -2)))
                 (setq n3(int_to_codestr(+(nth 4 sunL)i num 1))))
               (if(or(= i 0)(= i(1- num)))(setq tt scd_forwardunknown)(setq tt scd_target))
               (cons 1(strcat scd_edge _-1 _-1 _$ _-1
                              _$ n2 ag0 _$ n2 ag1
                              _$ n3 _$ n4 tt)))
            iiL ppL agedL)
           (mapcar;;辺の向き
            '(lambda(a b e )
               ;;(setq b(mapcar '- b))
               (cons 1(strcat scd_ellipsecurve(lsreal_to_codestr(append a b e))
                              (if command_for_alter "1 I I #" "n V V |"))))
            ppL vdL vc)
           (mapcar;;点1
            '(lambda(i a)(cons 1(strcat scd_vertex(int_to_codestr(+(nth 5 sunL)i))
                                        (if command_for_alter "2 $" "m {")
                                        (int_to_codestr(+(nth 8 sunL)i))_#)))
            iiL ppL)
           (mapcar
            '(lambda(a e)(cons 1(strcat scd_point(lsreal_to_codestr(mapcar '- a e))_#)))
            ppL ve)
           ));;
  
  (entmakex(append ls_codehead ls_code ls_codefin))
  )



;;solid>>>>>>>>>>>>

(setq
 ls_index_color_rgb
 (list
  (list 0 0 0 0)
  (list 1 255 0 0)(list 2 255 255 0)(list 3 0 255 0)(list 4 0 255 255)
  (list 5 0 0 255)(list 6 255 0 255)(list 7 255 255 255)(list 8 128 128 128)
  (list 9 192 192 192)(list 10 255 0 0)(list 11 255 127 127)(list 12 204 0 0)
  (list 13 204 102 102)(list 14 153 0 0)(list 15 153 76 76)(list 16 127 0 0)
  (list 17 127 63 63)(list 18 76 0 0)(list 19 76 38 38)(list 20 255 63 0)
  (list 21 255 159 127)(list 22 204 51 0)(list 23 204 127 102)(list 24 153 38 0)
  (list 25 153 95 76)(list 26 127 31 0)(list 27 127 79 63)(list 28 76 19 0)
  (list 29 76 47 38)(list 30 255 127 0)(list 31 255 191 127)(list 32 204 102 0)
  (list 33 204 153 102)(list 34 153 76 0)(list 35 153 114 76)(list 36 127 63 0)
  (list 37 127 95 63)(list 38 76 38 0)(list 39 76 57 38)(list 40 255 191 0)
  (list 41 255 223 127)(list 42 204 153 0)(list 43 204 178 102)(list 44 153 114 0)
  (list 45 153 133 76)(list 46 127 95 0)(list 47 127 111 63)(list 48 76 57 0)
  (list 49 76 66 38)(list 50 255 255 0)(list 51 255 255 127)(list 52 204 204 0)
  (list 53 204 204 102)(list 54 153 153 0)(list 55 153 153 76)(list 56 127 127 0)
  (list 57 127 127 63)(list 58 76 76 0)(list 59 76 76 38)(list 60 191 255 0)
  (list 61 223 255 127)(list 62 153 204 0)(list 63 178 204 102)(list 64 114 153 0)
  (list 65 133 153 76)(list 66 95 127 0)(list 67 111 127 63)(list 68 57 76 0)
  (list 69 66 76 38)(list 70 127 255 0)(list 71 191 255 127)(list 72 102 204 0)
  (list 73 153 204 102)(list 74 76 153 0)(list 75 114 153 76)(list 76 63 127 0)
  (list 77 95 127 63)(list 78 38 76 0)(list 79 57 76 38)(list 80 63 255 0)
  (list 81 159 255 127)(list 82 51 204 0)(list 83 127 204 102)(list 84 38 153 0)
  (list 85 95 153 76)(list 86 31 127 0)(list 87 79 127 63)(list 88 19 76 0)
  (list 89 47 76 38)(list 90 0 255 0)(list 91 127 255 127)(list 92 0 204 0)
  (list 93 102 204 102)(list 94 0 153 0)(list 95 76 153 76)(list 96 0 127 0)
  (list 97 63 127 63)(list 98 0 76 0)(list 99 38 76 38)(list 100 0 255 63)
  (list 101 127 255 159)(list 102 0 204 51)(list 103 102 204 127)(list 104 0 153 38)
  (list 105 76 153 95)(list 106 0 127 31)(list 107 63 127 79)(list 108 0 76 19)
  (list 109 38 76 47)(list 110 0 255 127)(list 111 127 255 191)(list 112 0 204 102)
  (list 113 102 204 153)(list 114 0 153 76)(list 115 76 153 114)(list 116 0 127 63)
  (list 117 63 127 95)(list 118 0 76 38)(list 119 38 76 57)(list 120 0 255 191)
  (list 121 127 255 223)(list 122 0 204 153)(list 123 102 204 178)(list 124 0 153 114)
  (list 125 76 153 133)(list 126 0 127 95)(list 127 63 127 111)(list 128 0 76 57)
  (list 129 38 76 66)(list 130 0 255 255)(list 131 127 255 255)(list 132 0 204 204)
  (list 133 102 204 204)(list 134 0 153 153)(list 135 76 153 153)(list 136 0 127 127)
  (list 137 63 127 127)(list 138 0 76 76)(list 139 38 76 76)(list 140 0 191 255)
  (list 141 127 223 255)(list 142 0 153 204)(list 143 102 178 204)(list 144 0 114 153)
  (list 145 76 133 153)(list 146 0 95 127)(list 147 63 111 127)(list 148 0 57 76)
  (list 149 38 66 76)(list 150 0 127 255)(list 151 127 191 255)(list 152 0 102 204)
  (list 153 102 153 204)(list 154 0 76 153)(list 155 76 114 153)(list 156 0 63 127)
  (list 157 63 95 127)(list 158 0 38 76)(list 159 38 57 76)(list 160 0 63 255)
  (list 161 127 159 255)(list 162 0 51 204)(list 163 102 127 204)(list 164 0 38 153)
  (list 165 76 95 153)(list 166 0 31 127)(list 167 63 79 127)(list 168 0 19 76)
  (list 169 38 47 76)(list 170 0 0 255)(list 171 127 127 255)(list 172 0 0 204)
  (list 173 102 102 204)(list 174 0 0 153)(list 175 76 76 153)(list 176 0 0 127)
  (list 177 63 63 127)(list 178 0 0 76)(list 179 38 38 76)(list 180 63 0 255)
  (list 181 159 127 255)(list 182 51 0 204)(list 183 127 102 204)(list 184 38 0 153)
  (list 185 95 76 153)(list 186 31 0 127)(list 187 79 63 127)(list 188 19 0 76)
  (list 189 47 38 76)(list 190 127 0 255)(list 191 191 127 255)(list 192 102 0 204)
  (list 193 153 102 204)(list 194 76 0 153)(list 195 114 76 153)(list 196 63 0 127)
  (list 197 95 63 127)(list 198 38 0 76)(list 199 57 38 76)(list 200 191 0 255)
  (list 201 223 127 255)(list 202 153 0 204)(list 203 178 102 204)(list 204 114 0 153)
  (list 205 133 76 153)(list 206 95 0 127)(list 207 111 63 127)(list 208 57 0 76)
  (list 209 66 38 76)(list 210 255 0 255)(list 211 255 127 255)(list 212 204 0 204)
  (list 213 204 102 204)(list 214 153 0 153)(list 215 153 76 153)(list 216 127 0 127)
  (list 217 127 63 127)(list 218 76 0 76)(list 219 76 38 76)(list 220 255 0 191)
  (list 221 255 127 223)(list 222 204 0 153)(list 223 204 102 178)(list 224 153 0 114)
  (list 225 153 76 133)(list 226 127 0 95)(list 227 127 63 111)(list 228 76 0 57)
  (list 229 76 38 66)(list 230 255 0 127)(list 231 255 127 191)(list 232 204 0 102)
  (list 233 204 102 153)(list 234 153 0 76)(list 235 153 76 114)(list 236 127 0 63)
  (list 237 127 63 95)(list 238 76 0 38)(list 239 76 38 57)(list 240 255 0 63)
  (list 241 255 127 159)(list 242 204 0 51)(list 243 204 102 127)(list 244 153 0 38)
  (list 245 153 76 95)(list 246 127 0 31)(list 247 127 63 79)(list 248 76 0 19)
  (list 249 76 38 47)(list 250 51 51 51)(list 251 91 91 91)(list 252 132 132 132)
  (list 253 173 173 173)(list 254 214 214 214)(list 255 255 255 255))

 ls_index_color_extb
 (list
  (cons 1 20)(cons 5 152)(cons 6 213)(cons 10 20)
  (cons 12 20)(cons 14 34)(cons 16 34)(cons 17 27)(cons 18 46)(cons 19 27)
  (cons 22 32)(cons 24 34)(cons 26 34)(cons 28 46)(cons 29 27)
  (cons 36 34)(cons 38 46)(cons 39 27)(cons 48 46)(cons 49 27)
  (cons 58 76)(cons 59 27)(cons 68 76)(cons 69 97)
  (cons 78 86)(cons 79 97)(cons 88 86)(cons 89 97)
  (cons 98 96)(cons 99 97)(cons 108 106)(cons 109 97)
  (cons 118 106)(cons 119 97)(cons 128 116)(cons 129 97)
  (cons 138 116)(cons 139 107)(cons 146 136)(cons 148 116)(cons 149 251)
  (cons 154 144)(cons 156 144)(cons 158 116)(cons 159 251)(cons 160 150)
  (cons 162 152)(cons 164 144)(cons 166 144)(cons 167 157)(cons 168 116)(cons 169 251)(cons 170 152)
  (cons 172 152)(cons 174 175)(cons 176 175)(cons 177 175)(cons 178 157)(cons 179 251)(cons 180 173)
  (cons 182 175)(cons 184 175)(cons 186 175)(cons 187 175)(cons 188 157)(cons 189 251)(cons 190 183)
  (cons 192 185)(cons 194 175)(cons 196 175)(cons 197 185)(cons 198 251)(cons 199 251)(cons 200 203)
  (cons 202 215)(cons 204 217)(cons 206 217)(cons 207 217)(cons 208 251)(cons 209 251)(cons 210 213)
  (cons 212 213)(cons 214 217)(cons 216 217)(cons 218 227)(cons 219 251)(cons 220 213)
  (cons 222 215)(cons 224 227)(cons 226 227)(cons 228 27)(cons 229 251)(cons 230 243)
  (cons 232 245)(cons 234 15)(cons 236 27)(cons 237 227)(cons 238 27)(cons 239 251)(cons 240 20)
  (cons 242 15)(cons 244 15)(cons 246 27)(cons 247 27)(cons 248 27)(cons 249 27)(cons 250 251)
  )

 ls_index_color_extw
 (list
  (cons 2 42)(cons 3 92)(cons 4 140)(cons 9 191)
  (cons 21 11)(cons 31 11)(cons 33 23)(cons 40 42)
  (cons 41 11)(cons 43 23)(cons 50 42)(cons 51 11)(cons 52 42)(cons 53 55)(cons 60 42)
  (cons 61 23)(cons 62 54)(cons 63 55)(cons 70 82)(cons 71 55)(cons 72 82)(cons 73 55)(cons 80 82)
  (cons 81 55)(cons 83 65)(cons 90 92)(cons 91 65)(cons 93 85)(cons 100 102)
  (cons 101 135)(cons 103 125)(cons 110 112)(cons 111 153)(cons 113 135)(cons 120 122)
  (cons 121 161)(cons 123 153)(cons 130 140)(cons 131 161)(cons 132 122)(cons 133 153)
  (cons 141 161)(cons 143 153)(cons 151 161)
  (cons 211 201)(cons 221 231)(cons 253 252)(cons 254 201)(cons 255 201)
  )
 )


(princ)
