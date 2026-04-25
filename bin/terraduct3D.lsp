(vl-load-com)
;;cpath_terraduct3d
(regapp "attributedata")
(regapp "terraduct3d")
(defun entgettd3(e)(entget e(list "terraduct3d")))
(defun *error*(msg)(princ msg));;保険
(setq bool_viewrotationfirsttime T
      prev_select_sessionname ""
      )

(defun load_las_to_grid(str_lasxml)
  ;; (setq cpath_terraduct3d(strcat (getenv "APPDATA")"\\" "terraduct3d-ac" "\\app"))
  
  (setq str_data(strcat cpath_terraduct3d "\\pyexe\\grid_points.csv")
        str_path(strcat cpath_terraduct3d "\\pyexe\\main.exe")
        bool_loop T size_file nil
        int_time 100 ms_loop nil ms_max 20000 
        size_grid 0.5
        )

  (setq str_laspath(getfiled(strcat "Select " str_lasxml " file")(getvar "DWGPREFIX")str_lasxml 0))
  (if(null str_laspath)
      (alert(mix_strasc(list 12501 12449 12452 12523 12364 36984 25246 12373 12428 12414 12379 12435 12391 12375 12383)))
    
    (progn
      (setq str_lasfile str_laspath )
      
      (while(setq num(vl-string-search "\\" str_lasfile))
        (setq str_lasfile(substr str_lasfile(+ num 2))))
      
      (if(findfile str_data)(vl-file-delete str_data))
      ;; (startapp str_path "")
      
      (setq wsh (vlax-create-object "WScript.Shell"))
      (setq cmd(strcat "\"" str_path "\" "
                       "\"" str_lasxml  "\" "
                       "\"" str_laspath "\""))
      (vlax-invoke-method wsh 'Run cmd 0 :vlax-true)
      (vlax-release-object wsh)

      (setq ms_loop(getvar "MILLISECS"))
      (while bool_loop
        (setq ms_start(getvar "MILLISECS"))
        (if(>(- ms_start ms_loop)ms_max)(setq bool_loop nil))
        (while(<(-(getvar "MILLISECS")ms_start)int_time) )
        (if(findfile str_data)
            (if(= size_file(vl-file-size str_data))(setq bool_loop nil)
              (setq size_file(vl-file-size str_data)) ))
        )


      (if(null(findfile str_data))
          (progn
            (setq dict_name nil)
            (alert(mix_strasc(list 35501 12415 36796 12415 12395 22833 25943 12375 12414 12375 12383)))
            )
        (progn
          ;;(vla-startUndoMark (vla-get-ActiveDocument (vlax-get-Acad-Object)))
          ;; (setq *error*
          ;;       (lambda(msg)
          ;;         (if(= msg "")T
          ;;           (vla-EndUndoMark (vla-get-ActiveDocument (vlax-get-Acad-Object))) )
          ;;         (mapcar '(lambda(a / v)
          ;;                    (if a(if(if(=(type a)'ENAME)
          ;;                                (if(entget a)(setq v(vlax-ename->vla-object a))(progn nil))
          ;;                              (if(setq v a a(vlax-vla-object->ename a))(entget a)(progn nil)) )
          ;;                             (vla-delete v))))
          ;;                 ls_vnam_killobj)
          ;;         (setq ls_vnam_killobj nil vnam_guide nil)
          ;;         (if (= 'FILE (type file_r)) (close file_r))
          ;;         (gc)
          ;;         (setq *error*(lambda(msg)(princ msg)))
          ;;         (princ msg)
          ;;         )
          ;;       ls_vnam_killobj nil
          ;;       )
          
          
          (setq num_limit 100000 num_start 0 num_last nil num_elem 1000 str_grid "1"
                delta_grid(atof str_grid))
          
          (setq spantime0(getvar "MILLISECS")
                ls_grid(list)
                file_r (open str_data "r")
                )

          
          (if(= int_inputlasinsert 0)T
            (progn
              (setq str_bname(strcat "input-lasgrid-" str_lasfile ))
              (if(vl-catch-all-error-p
                  (setq block(vl-catch-all-apply 'vla-Item(list vnam_blocktable str_bname))))
                  (setq block(vla-Add vnam_blockTable(vlax-3d-point 0 0 0)str_bname) )
                (progn
                  (if(setq set_ent(ssget "X"(list(cons 2 str_bname))))
                      (progn
                        (setq num(sslength set_ent))
                        (while(>(setq num(1- num))-1)
                          (setq vnam(vlax-ename->vla-object(ssname set_ent num)))
                          (vla-delete vnam)
                          )
                        ))
                  (vla-delete block)
                  (mapcar '(lambda(v)
                             (vlax-release-object v)
                             (setq ls_vla-release(vl-remove v ls_vla-release))
                             )
                          (list block))
                  (setq block(vla-Add vnam_blockTable(vlax-3d-point 0 0 0)str_bname))
                  )
                )
              (setq ls_vla-release(cons block ls_vla-release) )
              ))

          (setq z_ave 0. z_min nil z_max nil)
          (while(setq str(read-line file_r))
            (setq x(atof str)str(substr str(+(vl-string-search "," str)2))
                  y(atof str)str(substr str(+(vl-string-search "," str)2))
                  z(atof str)z_ave(+ z_ave z)
                  )
            (if z_min(setq z_min(min z z_min)z_max(max z z_max))
              (setq z_min z z_max z))

            (if(= int_inputlasinsert 0)T(vla-addpoint block(vlax-3d-point x y z)))
            
            (setq nx(/ x size_grid) nx(fix(- nx(abs(rem nx 1.))))
                  ny(/ y size_grid) ny(fix(- ny(abs(rem ny 1.))))
                  
                  ls_grid(cons(list(strcat(itoa nx)"$"(itoa ny))z)ls_grid)
                  )
            )
          (close file_r)

          
          (if(= int_inputlasinsert 0)T
            (mapcar '(lambda(v)
                       (vlax-release-object v)
                       (setq ls_vla-release(vl-remove v ls_vla-release))
                       )
                    (list block))
            )
          
          (if(= int_inputlasinsert 1)
              (vla-InsertBlock
               (vla-get-ModelSpace(vla-get-ActiveDocument(vlax-get-acad-object)))
               (vlax-3d-point 0 0 0)str_bname 1 1 1 0)
            )
          

          ;; (setq dicts_all(vla-get-Dictionaries(vla-get-ActiveDocument (vlax-get-acad-object)))
          ;;       vnam(vlax-ename->vla-object entna)
          ;;       vec_insert(vlax-safearray->list(vlax-variant-value(vla-get-normal vnam)))
          ;;       p_insert(vlax-safearray->list(vlax-variant-value(vla-get-insertionpoint vnam)))
          ;;       str_nameblock(vla-get-name vnam)
          ;;       )
          
          ;; (while(vl-string-search "*" str_nameblock)
          ;;   (setq str_nameblock(vl-string-subst "^^^^" "*" str_nameblock))
          ;;   )
          ;; (if spantime0 T(setq spantime0(getvar "MILLISECS")))

          ;; (defun dict_las_input(str_lasfile bool)

          (setq ls_xdata(list))
          (setq dict_name(strcat "lasgrid" "-" str_lasfile ))
          ;; (setq dict_grid(vl-catch-all-apply 'vla-Item (list dicts_all(strcat dict_name))))
          (if(if nil(assoc dict_name ls_lasgrid);;あれば作る必要がない
               (progn ;;あったら消して作る
                 (if(setq dict_grid(cdr(assoc dict_name ls_lasgrid)))(vla-delete dict_grid))
                 nil
                 ) )
              (progn  nil ) ;;すでにあるものを使うとき 通ることはない
            (progn ;;新しく作るとき
              
              (setq z_ave(/ z_ave(length ls_grid)))
              (setq dict_grid(vla-Add dicts_all dict_name))
              (setq xrec(vla-AddXRecord dict_grid "GRID-SIZE_CENTER"))
              (setq array_data(vlax-make-safearray vlax-vbVariant(cons 0 3))
                    array_type(vlax-make-safearray vlax-vbInteger(cons 0 3)))
              (vlax-safearray-fill array_type(list 1040 1040 1040 1040))
              (vlax-safearray-fill array_data(list size_grid z_ave z_min z_max))
              (vla-SetXRecordData xrec array_type array_data )

              (setq array_data(vlax-make-safearray vlax-vbVariant(cons 0 0))
                    array_type(vlax-make-safearray vlax-vbInteger(cons 0 0)))
              (vlax-safearray-fill array_type (list 1040))
              (mapcar
               '(lambda(lst)
                  (setq xrec(vla-AddXRecord dict_grid(strcat(car lst))))
                  (vlax-safearray-fill array_data(cdr lst))
                  (vla-SetXRecordData xrec array_type array_data )
                  )
               ls_grid)
              
              (if(setq lst(assoc dict_name ls_lasgrid))(vl-remove lst ls_lasgrid))
              (setq ls_lasgrid(cons(cons dict_name dict_grid)ls_lasgrid))
              )
            )
          
          ;;(vla-startUndoMark (vla-get-ActiveDocument (vlax-get-Acad-Object)))
          ;; (princ "\nFinish")

          (alert(mix_strasc(list 35501 36796 23436 20102)))
          ))
      ))
  
  ;; (*error* "")
  dict_name
  )


(defun c:devtd3d( / e v array_Type array_Data)
  (if(setq e(car(entsel)))
      (progn
        (setq v(vlax-ename->vla-object e))
        (vla-getXData v "terraduct3d" 'array_Type 'array_Data )
        (if array_data(split_list 0(cdr(mapcar 'vlax-variant-value
                                               (vlax-safearray->list array_data)))))))
  )


(defun x-alert(lst / str)
  (setq str(mix_strasc lst))
  (if(= int_alert 0)(alert str)(princ(strcat "\n" str))))

;;メインコマンド
(defun c:terraduct3d( / func_name dict_dokos bool_guidedisplay array_type array_data)
  (setq x_guidebase 1.)
  (vla-StartUndoMark (vla-get-ActiveDocument (vlax-get-Acad-Object)))
  
  (setq dicts_all(vla-get-Dictionaries(vla-get-ActiveDocument (vlax-get-acad-object))))
  
  (setq str_initialsession "terraduct3d_session_" ls_session(list))
  (vlax-for
   xrec dicts_all
   (if(vl-catch-all-error-p(setq str(vl-catch-all-apply 'vla-get-name (list xrec)))) T
     (if(vl-string-search str_initialsession str)
         (setq ls_session(cons(cons(vl-string-subst "" str_initialsession str)xrec)ls_session))))
   )
  
  (setq ls_session (reverse ls_session))
  
  ;; (mapcar '(lambda(a)(set(read(car a))(cadr a)))ls_dictdata)
  
  (search_sty-lay-blo(list(cons "STYLE" 'ls_textstyle)
                          (cons "DIMSTYLE" 'ls_dimstyle)
                          (cons "LAYER" 'ls_layername)
                          ;; (cons "BLOCK" 'ls_blockname)
                          ))

  ;; (setq ls_blockname
  ;;       ((lambda(lst / ls0 ls_out ii jj str)
  ;;          (setq ii(ascii(car lst)))
  ;;          (while lst
  ;;            (setq str(car lst)lst(cdr lst)jj(ascii str))
  ;;            (if(= ii jj)(setq ls0(cons str ls0))
  ;;              (setq ls_out(cons(cons ii ls0)ls_out)
  ;;                    ls0(list str)ii jj jj nil)
  ;;              )
  ;;            )
  ;;          (if jj ls_out(cons(cons ii ls0)ls_out)))
  ;;        ls_blockname)
  ;;       ls_blockname(reverse ls_blockname)
  ;;       )
  
  (setq

   cal_viewtopleft
   (lambda(ratio_height
           deltax deltay deltaz bool_3d 
           / height_view ls_screen_size_yx width_view bool_plane)
     (setq p_viewcenter(getvar "VIEWCTR")
           height_view(getvar "VIEWSIZE")
           height_text(* height_view ratio_height)
           vec_view  (if bool_3d(unit_vector(getvar "VIEWDIR"))(list 0 0 1))
           bool_plane(and(<(abs(car vec_view))1e-8)(<(abs(cadr vec_view))1e-8))
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
     )

   ;;TYPEがいるのはDCLのとき
   ls_ductparameters;;全角14文字まで
   (list(list(cons "TEXT"(mix_strasc(list 12304 12460 12452 12489 12486 12461 12473 12488 12398 35373 23450 12305)))
             (cons "ITEM" "TEXT"));;ガイドテキストの設定

        (list(cons "SYMBOL" 'str_textstyle_gbo) (cons "INITIALFUNC" (lambda(a)nil))
             (cons "EXPLANE"(mix_strasc(list 25991 23383 12473 12479 12452 12523)))
             )

        (list(cons "TEXT"(mix_strasc(list 25991 23383 12473 12479 12452 12523)))
             (cons "ITEM" "POPUP_LIST")(cons "VALLIST" ls_textstyle);;ガイドテキストの文字スタイル
             (cons "SYMBOL" 'int_textstyle_gbo)(cons "TYPE" "INT")
             (cons "INITIALFUNC" (lambda(a / i)
                                   (if(if str_textstyle_gbo
                                          (setq i(vl-position str_textstyle_gbo ls_textstyle)))T
                                     (setq i(vl-position(getvar "TEXTSTYLE")ls_textstyle)))
                                   i))
             (cons "ACTIONLIST"((lambda( / )
                                  (setq action_guidetext
                                        (lambda(str / i)
                                          (setq i(atoi(get_tile str))
                                                int_textstyle_gbo i
                                                str_textstyle_gbo(nth i ls_textstyle))
                                          (vla-put-StyleName vnam_guide str_textstyle_gbo)
                                          )
                                        )
                                  "action_guidetext"
                                  )))
             (cons "NOEXPORT"(lambda()nil));;出力せず読み込み時にnilにする
             )
        
        (list(cons "SYMBOL" 'str_textstyle_gbo);;スタイルは2回やる
             (cons "INITIALFUNC" (lambda(a)(nth int_textstyle_gbo ls_textstyle)))
             (cons "NOEXPORT"(lambda()T));;出力せず読み込み時になにもしない
             )
        

        (list(cons "TEXT"(mix_strasc(list 12463 12522 12483 12463 25805 20316)) )
             (cons "ITEM" "POPUP_LIST");;ガイドテキストのクリック操作
             (cons "VALLIST"(mapcar 'mix_strasc(list(list 26377 21177)(list 28961 21177))))
             (cons "SYMBOL" 'int_guideclick)(cons "TYPE" "INT")
             (cons "INITIALFUNC" (lambda(a)0))
             )
        (list(cons "TEXT"(mix_strasc(list 12463 12522 12483 12463 12398 27178 26041 21521 26377 21177 31684 22258) ) )
             (cons "ITEM" "EDIT_BOX");;ガイドテキストクリックの横方向有効範囲
             (cons "EXPLANE"(mix_strasc(list 8251 9734 12398 31684 22258
                                             " ( " 12473 12479 12452 12523 12395 20381 23384 12377 12427 
                                             "  2 " 12391 20808 38957 25991 23383 31243 24230 " )") ))
             
             ;;星の範囲スタイル依存:2で先頭文字程度
             (cons "SYMBOL" 'width_guideclick)(cons "TYPE" "REAL")
             (cons "INITIALFUNC" (lambda(a)10.))
             )
        (list(cons "TEXT"(mix_strasc(list 12486 12461 12473 12488 12469 12452 12474)))
             (cons "EXPLANE"(mix_strasc(list 8251 30011 38754 27604 ) ))
             (cons "ITEM" "EDIT_BOX")
             (cons "SYMBOL" 'textsize_guide_bo)(cons "TEMP" 'textsize_guide_bo_temp)
             (cons "TYPE" "REAL")(cons "INITIALFUNC"(lambda(a)0.015)) )
        
        (list(cons "TEXT"(mix_strasc(list  32294 26041 21521 20301 32622)))
             (cons "EXPLANE"(mix_strasc(list 8251 24038 19978 12364 "0" )))
             (cons "ITEM" "EDIT_BOX")
             (cons "SYMBOL" 'y_guidebase)(cons "TEMP" 'y_guidebase_temp)
             (cons "TYPE" "REAL") (cons "INITIALFUNC"(lambda(a)-2.))
             )
        
        ;; (list(cons "TEXT"(mix_strasc(list 12463 12522 12483 12463 25805 20316)) );;背景マスク
        ;;      (cons "ITEM" "POPUP_LIST")
        ;;      (cons "VALLIST"(mapcar 'mix_strasc(list(list 12394 12375)(list 12354 12426 ))))
        ;;      (cons "SYMBOL" 'int_guidemask)(cons "TYPE" "INT")
        ;;      (cons "INITIALFUNC" (lambda(a)0))
        ;;      )


        (list(cons "TEXT"(mix_strasc(list 12304 12381 12398 20182 "UI" 35373 23450 12305)))
             (cons "ITEM" "TEXT"))

        (list(cons "TEXT"(mix_strasc(list 35686 21578 25991 12398 20986 12375 26041 )) )
             (cons "ITEM" "POPUP_LIST");;警告文の出し方
             (cons "EXPLANE"(mix_strasc(list 8251 12480 12452 12450 12525 12464 25805 20316 26178 12398 35686 21578 12399 12509 12483 12503 12450 12483 12503 12398 12415 )));;ダイアログ操作時の警告はポップアップのみ
             (cons "VALLIST"(mapcar 'mix_strasc(list(list 12509 12483 12503 12450 12483 12503)
                                                    (list 12467 12510 12531 12489 12521 12452 12531 ))))
             (cons "SYMBOL" 'int_alert)(cons "TYPE" "INT")
             (cons "INITIALFUNC" (lambda(a)0))
             (cons "BREAK" 0)
             )

        (list(cons "TEXT"(mix_strasc(list 12304 31649 36335 20316 25104  12305)))
             (cons "ITEM" "TEXT"))
        
        (list(cons "TEXT"(mix_strasc(list 22320 34920 38754 27161 39640)))
             (cons "ITEM" "BUTTON");;地表面標高
             (cons "BUTTONLABEL"(mix_strasc(list 36984 25246)))
             (cons "SYMBOL" 'str_lasground);;(cons "TYPE" "STR");;BUTTONのときいらない
             (cons "ACTION"
                   ((lambda()
                      (setq action_buttonground
                            (lambda() (setq func_next settile_selectground)))
                      "action_buttonground")))
             (cons "NOEXPORT"(lambda()T))
             ;;INITIALTYPE;;INITIALFUNC
             )
        (list(cons "SYMBOL" 'height_ground)
             (cons "NOEXPORT"(lambda()T)))
        
        (list(cons "TEXT"(mix_strasc(list 32218 24418 25237 24433 26178 12398 26368 22823 12500 12483 12481)))
             (cons "ITEM" "EDIT_BOX");;線形投影時の最大ピッチ
             (cons "EXPLANE"(mix_strasc(list 8251 "0" 12398 12392 12365 36984 25246 12375 12383 32218 12398 31680 28857 12398 12415 12434 25237 24433 12377 12427)))
             ;;0のとき選択した線の節点のみを投影する
             (cons "SYMBOL" 'pitch_project)(cons "TEMP" 'pitch_project_temp)
             (cons "TYPE" "REAL") (cons "INITIALFUNC"(lambda(a)0.5))
             )

        (list(cons "TEXT"(mix_strasc(list 28145 24230 20837 21147 26041 27861 )))
             (cons "ITEM" "POPUP_LIST");;高度入力方法;;地表面標高差分,正負逆で差分入力,標高入力
             (cons "VALLIST"(setq ls_type_inputdepth
                                  (mapcar 'mix_strasc
                                          (list(list 27161 39640 24046 20998)
                                               (list 27161 39640 24046 20998
                                                     "(" 27491 36000 36870 ")")
                                               (list 27161 39640 20837 21147)
                                               ))))
             (cons "SYMBOL" 'int_inputdepth)(cons "TEMP" 'int_inputdepth_temp)
             (cons "TYPE" "INT") (cons "INITIALFUNC"(lambda(a)1)))
        
        (list(cons "TEXT"(mix_strasc(list 28145 24230 20837 21147 25163 38918   )))
             (cons "EXPLANE"(mix_strasc(list 8251 32076 36335 27770 23450 12398 12463 12522 12483 12463 24460 12398 25805 20316 12434 36984 25246)));;※経路決定のクリック後の操作を選択
             (cons "ITEM" "POPUP_LIST");;深度入力手順;;即時適用,深度の入力に移行,深度→オフセットの入力に移行
             (cons "VALLIST"(setq ls_type_editdepth
                                  (mapcar
                                   'mix_strasc
                                   (list(list 28145 24230 12398 20837 21147 12395 31227 34892)
                                        (list 28145 24230 8594 12458 12501 12475 12483 12488 12398 20837 21147)
                                        (list 21363 26178 36969 29992)
                                        ))))
             (cons "SYMBOL" 'int_editdepth);;(cons "TEMP" 'int_inputdepth_temp)
             (cons "TYPE" "INT") (cons "INITIALFUNC"(lambda(a)0)))
        
        (list(cons "TEXT"(mix_strasc(list  26032 35215 20870 24359 12398 35373 23450 20301 32622 )))
             (cons "EXPLANE"(mix_strasc(list 8251 "2" 22238 30446 20197 38477 12398 32232 38598 26178 12399 20219 24847 12395 35373 23450 12391 12365 12414 12377  )));;※2回目以降の編集時は任意に設定できます
             (cons "ITEM" "POPUP_LIST");;新規円弧の設定位置
             (cons "VALLIST"(setq ls_type_editarcposition
                                  (mapcar
                                   'mix_strasc;;手動,起点側終点を通過,終点側起点を通過
                                   (list(list 25163 21205)
                                        (list 36215 28857 20596 32066 28857 12434 36890 36942)
                                        (list 32066 28857 20596 36215 28857 12434 36890 36942)
                                        ))))
             (cons "SYMBOL" 'int_editarcposition);;(cons "TEMP" 'int_inputdepth_temp)
             (cons "TYPE" "INT") (cons "INITIALFUNC"(lambda(a)1)))
        
        (list(cons "TEXT"(mix_strasc(list 31649 12398 12514 12487 12522 12531 12464 )))
             (cons "ITEM" "POPUP_LIST");;管のモデリング
             (cons "VALLIST"(mapcar 'mix_strasc
                                    (list(list 12513 12483 12471 12517 )
                                         (list 12477 12522 12483 12489 ) )))
             (cons "SYMBOL" 'int_ductsolidmesh);;(cons "TEMP" 'int_protectcon_temp)
             (cons "TYPE" "INT") (cons "INITIALFUNC"(lambda(a)0)))
        
        (list(cons "TEXT"(mix_strasc(list 20445 35703 12467 12531 12463 12522 12540 12488 26377 28961)))
             (cons "ITEM" "POPUP_LIST");;保護コンクリート有無
             (cons "VALLIST"(setq ls_type_protect
                                  (mapcar 'mix_strasc
                                          (list(list 12354 12426 " ( " 20840 21608 " )")
                                               (list 12354 12426 " ( " 19979 21322 " )")
                                               (list 12394 12375)))))
             (cons "SYMBOL" 'int_protectcon)(cons "TEMP" 'int_protectcon_temp)
             (cons "TYPE" "INT") (cons "INITIALFUNC"(lambda(a)2))
             (cons "BREAK" 0)
             )
        
        (list(cons "TEXT"(mix_strasc(list "")))
             (cons "ITEM" "TEXT"))
        
        (list(cons "TEXT"(mix_strasc(list 31649 30452 24452))) 
             (cons "ITEM" "EDIT_BOX");;管直径
             (cons "SYMBOL" 'diam_duct)(cons "TEMP" 'diam_duct_temp)
             (cons "TYPE" "REAL")(cons "INITIALFUNC"(lambda(a)0.35)))
        (list(cons "TEXT"(mix_strasc(list 20445 35703 12467 12531 12463 12522 12540 12488 24133))) 
             (cons "ITEM" "EDIT_BOX");;保護コンクリート幅
             (cons "SYMBOL" 'width_protect)(cons "TEMP" 'width_protect_temp)
             (cons "TYPE" "REAL")(cons "INITIALFUNC"(lambda(a)0.5)))
        (list(cons "TEXT"(mix_strasc(list 20445 35703 12467 12531 12463 12522 12540 12488 39640 12373)))
             (cons "ITEM" "EDIT_BOX");;保護コンクリート高さ
             (cons "EXPLANE"(mix_strasc(list 8251 19979 21322 12398 12392 12365 12399 20837 21147 20516 12398 "0.5" 20493 12392 12394 12427 )))
             (cons "SYMBOL" 'height_protect)(cons "TEMP" 'height_protect_temp)
             (cons "TYPE" "REAL") (cons "INITIALFUNC"(lambda(a)0.5))
             )
        (list(cons "TEXT"(mix_strasc(list 20445 35703 12467 12531 12463 12522 12540 12488 38754 21462 12426)))
             (cons "ITEM" "EDIT_BOX");;保護コンクリート面取り
             (cons "SYMBOL" 'filet_protect)(cons "TEMP" 'filet_protect_temp)
             (cons "TYPE" "REAL") (cons "INITIALFUNC"(lambda(a)0.1)))
        (list(cons "TEXT"(mix_strasc(list 26354 32218 37096 12398 21322 24452)))
             (cons "ITEM" "EDIT_BOX");;曲線部の半径
             (cons "SYMBOL" 'radius_bend)(cons "TEMP" 'radius_bend_temp)
             (cons "TYPE" "REAL") (cons "INITIALFUNC"(lambda(a)5.))
             )

        (list(cons "TEXT"(mix_strasc(list 32076 36335 12364 12394 12356 12392 12365 12398 28145 24230 20837 21147 )))
             (cons "ITEM" "POPUP_LIST");;経路がないときの深度入力
             (cons "VALLIST"(mapcar 'mix_strasc;;許可しない
                                    (list(list 35377 21487 12375 12394 12356 )
                                         (list 35377 21487 12377 12427) )))
             (cons "SYMBOL" 'int_allow_noroad)(cons "TEMP" 'int_allow_noroad_temp)
             (cons "TYPE" "INT") (cons "INITIALFUNC"(lambda(a)0))
             )

        (list(cons "TEXT"(mix_strasc(list 12304 31649 36335 29305 27530 37096 20849 36890 12305)))
             (cons "ITEM" "TEXT"))
        
        (list(cons "SYMBOL" 'str_dimstyle_ductlevel)(cons "INITIALFUNC"(lambda(a)nil))
             (cons "EXPLANE"(mix_strasc(list 27161 39640 23544 27861 12473 12479 12452 12523)))
             )
        
        (list(cons "TEXT"(mix_strasc(list 27161 39640 23544 27861 12473 12479 12452 12523)))
             (cons "ITEM" "POPUP_LIST")(cons "VALLIST" ls_dimstyle);;標高寸法スタイル
             (cons "EXPLANE"(mix_strasc(list 8251 12371 12428 12363 12425 20316 25104 12373 12428 12427 12418 12398 12395 23550 12375 12390 26377 21177)))
             (cons "SYMBOL" 'int_dimstyle_ductlevel)(cons "TYPE" "INT")
             (cons "INITIALFUNC" (lambda(a)
                                   (if(if str_dimstyle_ductlevel
                                          (setq i(vl-position str_dimstyle_ductlevel ls_dimstyle)))T
                                     (setq i(vl-position(getvar "DIMSTYLE")ls_dimstyle)))
                                   i))
             
             (cons "ACTIONLIST"((lambda( / )
                                  (setq action_dimlevel
                                        (lambda(str / i)
                                          (setq i(atoi(get_tile str))
                                                int_dimstyle_ductlevel i
                                                str_dimstyle_ductlevel(nth i ls_dimstyle))
                                          )
                                        )
                                  "action_dimlevel"
                                  )))
             (cons "NOEXPORT"(lambda()nil))
             )
        
        (list(cons "SYMBOL" 'str_dimstyle_ductlevel)
             (cons "INITIALFUNC"(lambda(a)(nth int_dimstyle_ductlevel ls_dimstyle)))
             (cons "NOEXPORT"(lambda()T))
             )

        (list(cons "TEXT"(mix_strasc(list 27161 39640 20024 12417 26689 25968 ))) 
             (cons "ITEM" "EDIT_BOX");;標高丸め桁数
             (cons "SYMBOL" 'int_unitelevation);;
             (cons "TYPE" "INT") (cons "INITIALFUNC"(lambda(a)1)))

        
        (list(cons "TEXT"(mix_strasc(list 21517 31216 12398 19978 26360 12365 )));;
             (cons "ITEM" "POPUP_LIST")
             (cons "VALLIST"(mapcar 'mix_strasc(list(list 35377 21487 12375 12394 12356)
                                                    (list 35377 21487 12377 12427 ))))
             (cons "SYMBOL" 'int_allow_overwrite)(cons "TYPE" "INT")
             (cons "INITIALFUNC" (lambda(a)0))
             (cons "BREAK" 0)
             )
        
        ;; (list(cons "TEXT" "");;(mix_strasc(list 26354 32218 37096 38291 12398 30452 32218 37096 38263 12373)))
        ;;      ;; ;;曲線部間の直線部長さ
        ;;      ;; (cons "EXPLANE"(mix_strasc(list "0" 12424 12426 22823 12365 12356 12392 12365 30452 32218 37096 12434 12388 12394 12368 26354 32218 37096 12398 38291 12395 30452 32218 37096 12434 35373 12369 12427))) ;;0より大きいとき直線部をつなぐ曲線部の間に直線部を設ける
        ;;      (cons "ITEM" "TEXT");;"EDIT_BOX")
        ;;      (cons "SYMBOL" 'length_arccenter)(cons "TEMP" 'length_arccenter_temp)
        ;;      (cons "TYPE" "REAL") (cons "INITIALFUNC"(lambda(a)0.))
        ;;      )

        (list(cons "TEXT"(mix_strasc(list 12304 29305 27530 37096 20316 25104 12305)))
             (cons "ITEM" "TEXT"));;特殊部
        
        (list(cons "TEXT"(mix_strasc(list 22825 31471 12363 12425 22320 34920 38754 12414 12391)))
             (cons "ITEM" "EDIT_BOX");;特殊部天端から地表面まで
             (cons "EXPLANE"(mix_strasc(list 8251 65288 "=" 12510 12531 12507 12540 12523 31361 20986 38263 12373 65289))) ;;（=マンホール突出長さ）
             (cons "SYMBOL" 'height_ccboxtop)(cons "TEMP" 'height_ccboxtop_temp)
             (cons "TYPE" "REAL") (cons "INITIALFUNC"(lambda(a) 0.3))
             )
        (list(cons "TEXT"(mix_strasc(list 39640 12373)));;特殊部高さ
             (cons "ITEM" "EDIT_BOX")
             (cons "SYMBOL" 'height_ccbox)(cons "TEMP" 'height_ccbox_temp)
             (cons "TYPE" "REAL") (cons "INITIALFUNC"(lambda(a)2.)))
        
        (list(cons "TEXT"(mix_strasc(list 12510 12531 12507 12540 12523 30452 24452)))
             (cons "ITEM" "EDIT_BOX");; マンホール直径
             (cons "SYMBOL" 'diam_manhole)(cons "TEMP" 'diam_manhole_temp)
             (cons "TYPE" "REAL") (cons "INITIALFUNC"(lambda(a)0.9)))

        (list(cons "TEXT"(mix_strasc(list 22343 12375 12467 12531 12458 12501 12475 12483 12488)))
             (cons "ITEM" "EDIT_BOX");; 均しコンオフセット
             (cons "SYMBOL" 'offset_levelingcon)(cons "TEMP" 'offset_levelingcon_temp)
             (cons "TYPE" "REAL") (cons "INITIALFUNC"(lambda(a)0.05)))
        (list(cons "TEXT"(mix_strasc(list 22343 12375 12467 12531 39640 12373)))
             (cons "ITEM" "EDIT_BOX");; 均しコン高さ
             (cons "EXPLANE"(mix_strasc(list 8251 22343 12375 12467 12531 39640 12373 12364 "0" 12398 12392 12365 20316 25104 12373 12428 12394 12356 )));;0のとき作成されない
             (cons "SYMBOL" 'height_levelingcon)(cons "TEMP" 'height_levelingcon_temp)
             (cons "TYPE" "REAL") (cons "INITIALFUNC"(lambda(a)0.1))
             (cons "BREAK" 0)
             )

        (list(cons "TEXT"(mix_strasc(list 12304 35377 23481 20516 12305)))
             (cons "ITEM" "TEXT"))

        (list(cons "TEXT"(mix_strasc(list 24178 28169 35377 23481 20516)))
             (cons "ITEM" "EDIT_BOX");; 干渉許容値
             (cons "EXPLANE"(mix_strasc(list 8251 12431 12378 12363 12394 24178 28169 12434 35377 12377 12392 12365 36000 12398 20516 12434 20837 21147 )));;※わずかな干渉を許すとき負の値を入力
             (cons "SYMBOL" 'allow_influence)(cons "TEMP" 'allow_influence_temp)
             (cons "TYPE" "REAL") (cons "INITIALFUNC"(lambda(a)0.01)))
        
        (list(cons "TEXT"(mix_strasc(list 22303 34987 12426 35377 23481 20516)))
             (cons "ITEM" "EDIT_BOX");; 土被り許容値
             (cons "EXPLANE"(mix_strasc(list 8251 22303 34987 12426 35377 23481 20516 20197 19978 12398 12418 12398 12399 34920 31034 12373 12428 12394 12356))) ;; 土被り許容値以上のものは表示されない
             (cons "SYMBOL" 'allow_cover)(cons "TEMP" 'allow_cover_temp)
             (cons "TYPE" "REAL") (cons "INITIALFUNC"(lambda(a)0.5))
             (cons "BREAK" 0)
             )
        
        ;; (list(cons "TEXT"(mix_strasc(list 22303 34987 12426 34920 31034 12500 12483 12481)))
        ;;      (cons "ITEM" "EDIT_BOX");; 土被り表示ピッチ
        ;;      (cons "EXPLANE"(mix_strasc(list 8251 22303 34987 12426 35377 23481 20516 "0" 12398 12392 12365 25351 23450 12375 12383 12500 12483 12481 12391 22303 34987 12426 12434 34920 31034 12377 12427))) ;;土被り許容値0のとき指定したピッチで土被りを表示する
        ;;      (cons "SYMBOL" 'pitch_cover)(cons "TEMP" 'pitch_cover_temp)
        ;;      (cons "TYPE" "REAL") (cons "INITIALFUNC"(lambda(a)2.))
        ;;      )

        (list(cons "TEXT"(mix_strasc(list 12304 33394 12392 30011 23652 35373 23450 12305)))
             (cons "ITEM" "TEXT"));;色と画層設定

        (list(cons "TEXT"(mix_strasc(list 12473 12490 12483 12503 12398 33394)))
             (cons "ITEM" "COLOR");; スナップの色
             (cons "SYMBOL" 'int_colsnappoint)(cons "TEMP" 'int_colsnappoint_temp)
             (cons "INITIALFUNC"(lambda(a)11)))

        
        (list(cons "TEXT"(mix_strasc(list 31649 12398 33394)))
             (cons "ITEM" "COLOR");; 管の色
             (cons "SYMBOL" 'int_colduct)(cons "TEMP" 'int_colduct_temp)
             (cons "INITIALFUNC"(lambda(a)1)))
        (list(cons "TEXT"(mix_strasc(list 32076 36335 12434 36984 25246 12375 12383 12392 12365 )))
             (cons "ITEM" "POPUP_LIST");;経路を選択したとき;;色を取得するしない
             (cons "VALLIST"(mapcar 'mix_strasc(list(list 33394 12434 21462 24471 12377 12427  )
                                                    (list 33394 12434 21462 24471 12375 12394 12356 ))))
             (cons "SYMBOL" 'int_getcolor_ductroad)(cons "TYPE" "INT")
             (cons "INITIALFUNC" (lambda(a)0))
             )

        (list(cons "TEXT"(mix_strasc(list 31649 12398 23544 27861 12398 33394)))
             (cons "ITEM" "COLOR");; 管の寸法の色
             (cons "SYMBOL" 'int_colductdim)(cons "TEMP" 'int_colductdim_temp)
             (cons "INITIALFUNC"(lambda(a)7)))

        (list(cons "TEXT"(mix_strasc(list 32232 38598 20013 12398 31649 12398 23544 27861 12398 33394)))
             (cons "ITEM" "COLOR");; 編集中の管の寸法の色
             (cons "SYMBOL" 'int_colductdimedit)(cons "TEMP" 'int_colductdimedit_temp)
             (cons "INITIALFUNC"(lambda(a)3)))
        
        (list(cons "TEXT"(mix_strasc(list 20445 35703 12467 12531 12398 33394)))
             (cons "ITEM" "COLOR");; 保護コンの色
             (cons "SYMBOL" 'int_colprotect)(cons "TEMP" 'int_colprotect_temp)
             (cons "INITIALFUNC"(lambda(a)9)))

        ;; (list(cons "TEXT"(mix_strasc(list 20445 35703 12467 12531 12398 30011 23652 )))
        ;;      (cons "ITEM" "POPUP_LIST");;保護コンの画層
        ;;      (cons "VALLIST" ls_layername)
        ;;      (cons "SYMBOL" 'int_getcolor_ductroad)(cons "TYPE" "INT")
        ;;      (cons "INITIALFUNC" (lambda(a)0))
        ;;      )
        
        
        (list(cons "TEXT"(mix_strasc(list 29305 27530 37096 12398 33394)))
             (cons "ITEM" "COLOR");; 特殊部の色
             (cons "SYMBOL" 'int_colccbox)(cons "TEMP" 'int_colccbox_temp)
             (cons "INITIALFUNC"(lambda(a)8)))

        ;; (list(cons "TEXT"(mix_strasc(list 22303 34987 12426 "OK" 33394)))
        ;;      (cons "ITEM" "COLOR");; 土被りOK色
        ;;      (cons "SYMBOL" 'int_colcoverok)(cons "TEMP" 'int_colcoverok_temp)
        ;;      (cons "INITIALFUNC"(lambda(a)3)))
        
        (list(cons "TEXT"(mix_strasc(list 22303 34987 12426 "NG" 33394)))
             (cons "ITEM" "COLOR");; 土被りNG色
             (cons "SYMBOL" 'int_colcoverng)(cons "TEMP" 'int_colcoverng_temp)
             (cons "INITIALFUNC"(lambda(a)6)))

        (list(cons "TEXT"(mix_strasc(list 24178 28169 "NG" 33394)))
             (cons "ITEM" "COLOR");; 干渉NG色
             (cons "SYMBOL" 'int_colinfluence)(cons "TEMP" 'int_colinfluence_temp)
             (cons "INITIALFUNC"(lambda(a)11))
             )

        
        )

   ls_initialparameters ls_ductparameters
   
   
   function_read_session
   (lambda( / );;dclを使わない;;多分外に出す
     (setq str_terraductsession "onlysession")
     (setq func_name "home")
     ((lambda(str)
        (mapcar '(lambda(a / sym)(if(setq sym(cdr(assoc str a)))(set sym nil)))ls_initialparameters))
      "TEMP")
     
     (cond
      ;;今回選んだセッションと前回選んだセッションが違っていたら呼び出す
      ((=(strcat str_initialsession str_terraductsession)prev_select_sessionname)
       )
      (T
       ((lambda(str)
          (mapcar '(lambda(a / sym)(if(setq sym(cdr(assoc str a)))(set sym nil)))ls_initialparameters))
        "SYMBOL")
       
       (if(vl-catch-all-error-p
           (setq asis_session
                 (vl-catch-all-apply
                  'vla-Item (list dicts_all(strcat str_initialsession str_terraductsession)))))
           (setq asis_session(vla-Add dicts_all(strcat str_initialsession str_terraductsession))
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

       (setq prev_select_sessionname(strcat str_initialsession str_terraductsession))
       
       )
      )
     
     )
   
   )


  (setq path_dcl(if command_for_alter(vl-filename-mktemp ".dcl")
                  (vl-filename-mktemp "0" "0" ".dcl") )
        nn(strlen path_dcl) )
  (while(/=(substr path_dcl nn 1)"\\")(setq nn(1- nn)))
  (setq str_path_tempdirectory(substr path_dcl 1 nn) )

  (setq bool_loop T)
  (while bool_loop
    ((lambda( / point_view_center vec_x_onview vec_y_onview
                ls_screen_size_yx real_delta_x real_delta_y th point_base)
       (function_read_session)
       (if func_name
           (if(=(type func_name)'STR)
               (terraduct3d-menu func_name)
             (if(vl-position(type func_name)(list 'SUBR 'USUBR))
                 (func_name)
               )))
       ))
    )

  (vla-EndUndoMark (vla-get-ActiveDocument (vlax-get-Acad-Object)))
  (princ)
  )
  

;;FEATUREELEVSFROMSURF
(defun addkillobj(x)(setq ls_vnam_killobj(cons x ls_vnam_killobj)) )
(defun exckillobj(x)(setq ls_vnam_killobj(vl-remove x ls_vnam_killobj)) )

(defun terraduct3d-menu( str  / str ls_grfunc get_vnam_guide ls_guide_drawing)

  (setq str_edit str)

  (setq
   ls_vnam_highlight nil ls_vnam_visible nil ls_vnam_killobj nil
   *error* ;;error_profile
   (lambda( msg )
     (done_dialog 0)
     (if(= msg "")T (vla-EndUndoMark (vla-get-ActiveDocument (vlax-get-Acad-Object))) )
     
     (mapcar '(lambda(a / v e);;オブジェクト
                (if(vl-catch-all-error-p
                    (setq e(vl-catch-all-apply 'vlax-vla-object->ename(list a))))
                    (setq v nil e nil)
                  (setq v a))
                (if(if e(entget e))
                    (progn
                      (setq ls_vnam_highlight(vl-remove v ls_vnam_highlight)
                            ls_vnam_visible(vl-remove v ls_vnam_visible) )
                      (vla-delete v)
                      )))
             ls_vnam_killobj)
     (setq ls_vnam_killobj nil vnam_guide nil)
     
     (mapcar '(lambda(a / v e)
                (if(vl-catch-all-error-p
                    (setq e(vl-catch-all-apply 'vlax-vla-object->ename(list a))))
                    (setq v nil e nil)
                  (setq v a))
                (if(if e(entget e))
                    (vla-Highlight v :vlax-false)))
             ls_vnam_highlight)
     (setq ls_vnam_highlight nil)
     (mapcar '(lambda(a / v e)g
                (if(vl-catch-all-error-p
                    (setq e(vl-catch-all-apply 'vlax-vla-object->ename(list a))))
                    (setq v nil e nil)
                  (setq v a))
                (if(if e(entget e))
                    (vla-put-visible v :vlax-true)))
             ls_vnam_visible)
     (setq ls_vnam_visible nil)

     (mapcar '(lambda(lst / str sym val );;システム変数
                (setq str(car lst)sym(cadr lst))
                (if(setq val(eval sym))(setvar str val))
                (set sym nil) )
             (list(list "DELOBJ" 'temp_delobj)
                  (list "OSMODE" 'temp_obs)
                  (list "LOFTNORMALS" 'temp_loft)
                  (list "SELECTIONCYCLING" 'temp_selecycling)
                  ))
     
     (mapcar '(lambda(a /  val);;リリース
                (if a(vlax-release-object a))
                )
             ls_vla-release )
     (setq ls_vla-release nil)
     
     (setq bool_noeditdepth nil)
     
     ;;XRECORD保存
     ;; (mapcar
     ;;  '(lambda(lst / num array_data array_type str ls_hand)
     ;;     (if(setq ls_hand(eval(cdr lst)))
     ;;         (progn
     ;;           (setq str(car lst)num(length ls_hand)
     ;;                 array_data(vlax-make-safearray vlax-vbVariant(cons 0(1- num)))
     ;;                 array_type(vlax-make-safearray vlax-vbInteger(cons 0(1- num))) )
     ;;           (vlax-safearray-fill array_type(mapcar '(lambda(a)1005)ls_hand))
     ;;           (vlax-safearray-fill array_data ls_hand)
     ;;           (vla-SetXRecordData(vla-Item asis_session str)
     ;;                              array_type array_data )
     ;;           )))
     ;;  ls_duct_xrecords)
     
     (redraw)
     (gc)
     (setq *error*(lambda(msg)(princ msg)))
     (princ msg)
     )
   )

  (mapcar '(lambda(lst / sym str val)
             (setq str(car lst)sym(cadr lst)val(caddr lst))
             (set sym(getvar str))(setvar str val) )
          (list(list "SELECTIONCYCLING" 'temp_selecycling -2)
               ))
  

  

  (setq vnam_blockTable(vla-get-blocks(vla-get-ActiveDocument (vlax-get-acad-object)))
        ls_vla-release(list vnam_blockTable ));;errorよりあとにやる

  ;;asissession
  (setq ls_vnam_killobj nil int_guidemask 0)
  ((lambda( vnam_g ls_a ls_b / entna vnam )
     (set vnam_g
          ((lambda( / )
             (setq entna(entmakex
                         (append
                          (list(cons 0 "MTEXT")(cons 100 "AcDbEntity")(cons 100 "AcDbMText")
                               (cons 7 str_textstyle_gbo)(cons 62 255)(cons 40 0.01 )(list 10 0 0 0)
                               (cons 1 "AAAAAAAAA")(cons 71 1)
                               )
                          (if(= int_guidemask 1)
                              ;;(list(cons 90 1)(cons 63 177)(cons 421 986966)(cons 45 2.))
                              (list(cons 90 3)(cons 63 256)(cons 45 2.))
                            )
                          ))
                   vnam(vlax-ename->vla-object entna))
             (addkillobj vnam)
             vnam))
          )

     (mapcar
      '(lambda(sym ls_str)
         (set sym
              (mapcar
               '(lambda( int_posi str)
                  (setq entna(entmakex
                              (list(cons 0 "MTEXT")(cons 100 "AcDbEntity")(cons 100 "AcDbMText")
                                   (cons 7 str_textstyle_gbo)(cons 62 90)(cons 40 0.001 )(list 10 0 0 0)
                                   (cons 1(mix_strasc str))(cons 71 int_posi)
                                   ))
                        vnam(vlax-ename->vla-object entna))
                  (addkillobj vnam)
                  vnam)
               (mapcar 'car ls_str)(mapcar 'cdr ls_str))
              )
         )
      
      nil ;;(list ls_a ls_b)
      (list(list(list 7 36215 28857)(list 7 32066 28857)(list 1 20837 21147 36215 28857)
                (list 7 32294 26029 36215 28857)(list 7 32294 26029 88 26041 21521) ) 
           ;;起点 終点 入力起点 縦断起点 縦断X方向
           
           ;;分岐保留
           ;;(mapcar '(lambda(a)"")(inclist 0(setq num_limitbranch 20)))
           
           )
      )
     
     (setq ls_allvnam_guide(cons vnam_guide(append ls_vnam_guide ls_vnam_guidebranch)))
     )
   'vnam_guide 'ls_vnam_guide 'ls_vnam_guidebranch)


  (progn ;;初期値initial
    (setq bool_loop T bool_select nil p_selerac nil 
          bool_snap(<(getvar 'OSMODE)16384)
          ;;int_snap(if(<(getvar 'OSMODE)16384)0 1)
          
          ls_intchr(list 45 46 48 49 50 51 52 53 54 55 56 57)
          str_input "" ;;(if(= str_edit "solid")(rtos dist_unitsolid 2 3)"")
          str_edit_loop nil p_selerac nil
          p_gr5loop nil vec_view_gr5loop nil
          )

    (mapcar 'set
            '(str_gcol_w str_gcol_r str_gcol_y str_gcol_c str_gcol_g str_gcol_p)
            (if(bg-dark-p)
                (list "255" "10" "54" "134" "90" "220")
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
                 " , {\\C" str_gcol_p ";" "Enter} : "
                 "{\\C" str_gcol_c ";" 27770 23450 "}"
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
                 
                 ;;メニューをカーソル付近に置く、放す ;;<:文字を小さく、>:文字を大きく ^:上へv:下へ
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
    
    (setq ls_grfunc(grfunc_duct3d))
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

  (progn ;;unique
    (setq ls_booleditunique
          (list "home" "home"))
    
    (setq ls_lasgrid(list))
    (vlax-for
     vnam dicts_all
     (if(vl-catch-all-error-p
         (setq str(vl-catch-all-apply 'vla-get-name (list vnam))))
         (setq str nil)
       (if(vl-string-search "lasgrid-" str)
           (setq ls_lasgrid(cons(cons str vnam)ls_lasgrid )))
       )
     )

    (setq str_layprotect(mix_strasc(list 20445 35703 12467 12531 12463 12522 12540 12488 )))
    (entdel(entmakex(list(cons 0 "POINT")(list 10 0 0 0)(cons 8 str_layprotect))))
    
    (progn ;;ground
      (setq path_selectgrounddcl(strcat str_path_tempdirectory "selectground.dcl") 
            open_file (open path_selectgrounddcl "w")
            )
      (write_strlist
       open_file
       (list
        "Sedit :dialog"
        "{"
        (mix_strasc;;地形選択
         (list " label = \""  22320 24418 36984 25246 "\";"))

        (list_to_dcltext;;一定標高で入力
         (list "text"(cons "width" "30")(cons "fixed_width" "true")
               (cons "value"(mix_strasc(list 19968 23450 27161 39640 12391 20837 21147)))))
        (list_to_dcltext
         (list "edit_box"(cons "width" "10")(cons "fixed_width" "true")
               (cons "key" "groundvalue")))

        " spacer;"

        (list_to_dcltext;;入力済みデータから選択
         (list "text"(cons "width" "30")(cons "fixed_width" "true")
               (cons "value"(mix_strasc(list 20837 21147 28168 12415
                                             12487 12540 12479 12363 12425 36984 25246)))))
        (list_to_dcltext
         (list "list_box"(cons "width" "20")(cons "height" "10")
               (cons "key" "groundlas")))

        " :row"
        " {"
        "  alignment = left;"
        "  fixed_width = true;"
        (list_to_dcltext;;データをグリッドとして読込
         (list "button"(cons "width" "16")(cons "fixed_width" "true")
               (cons "key" "inputlas")
               (cons "label"(mix_strasc(list "las" 12487 12540 12479 12434 12464 12522 12483 12489 12392 12375 12390 35501 36796)))))

        (list_to_dcltext;;データを
         (list "button"(cons "width" "16")(cons "fixed_width" "true")
               (cons "key" "inputxml")
               (cons "label"(mix_strasc(list "xml" 12487 12540 12479 12434 12464 12522 12483 12489 12392 12375 12390 35501 36796)))))
        "  }"
        
        " :row"
        " {"
        "  alignment = left;"
        "  fixed_width = true;"
        "  fixed_height = true;"
        
        (list_to_dcltext;;;;入力データをブロック化
         (list "text"(cons "width" "20")(cons "fixed_width" "true")
               (cons "value"(mix_strasc(list  20837 21147 12487 12540 12479 12434 12502 12525 12483 12463 21270)))))
        (list_to_dcltext;;;;入力データをブロック化
         (list "popup_list"(cons "width" "20")(cons "fixed_width" "true")
               (cons "key" "insertblock") ))
        "  }"
        
        (list_to_dcltext;;ブロックを作成しなくてもグリッドの情報は内部データとして保存され使用できます
         (list "text"(cons "width" "65")(cons "fixed_width" "true")
               (cons "value"(mix_strasc(list 8251  12502 12525 12483 12463 12434 20316 25104 12375 12394 12367 12390 12418 12464 12522 12483 12489 12398 24773 22577 12399 20869 37096 12487 12540 12479 12392 12375 12390 20445 23384 12373 12428 20351 29992 12391 12365 12414 12377 )))))

        (list_to_dcltext;;ブロック作成の有無は処理時間に大きく影響するため注意してください
         (list "text"(cons "width" "65")(cons "fixed_width" "true")
               (cons "value"(mix_strasc(list 8251 12502 12525 12483 12463 20316 25104 12398 26377 28961 12399 20966 29702 26178 38291 12395 22823 12365 12367 24433 38911 12377 12427 12383 12417 27880 24847 12375 12390 12367 12384 12373 12356  )))))

        (list_to_dcltext;;ブロックを作成しても挿入しなかった場合、(0,0,0)に挿入すれば復元できます
         (list "text"(cons "width" "65")(cons "fixed_width" "true")
               (cons "value"(mix_strasc(list 8251  12502 12525 12483 12463 12434 20316 25104 12375 12390 12418 25407 20837 12375 12394 12363 12387 12383 22580 21512 12289 )))))

         (list_to_dcltext;;入力ファイル名がついたブロック定義を探し、(0,0,0)に挿入すれば復元できます
          (list "text"(cons "width" "65")(cons "fixed_width" "true")
                (cons "value"(mix_strasc(list 12288 12288 20837 21147 12501 12449 12452 12523 21517 12364 12388 12356 12383 12502 12525 12483 12463 23450 32681 12434 25506 12375 12289 "(0,0,0)" 12395 25407 20837 12377 12428 12400 24489 20803 12391 12365 12414 12377 )))))
         
        

        (list_to_dcltext;;※数値とデータどちらも入力されているときはデータを優先します
         (list "text"(cons "width" "65")(cons "fixed_width" "true")
               (cons "value"(mix_strasc
                             (list 8251 25968 20516 12392 12487 12540 12479
                                   12393 12385 12425 12418 20837 21147
                                   12373 12428 12390 12356 12427 12392 12365 12399
                                   12487 12540 12479 12434 20778 20808 12375 12414 12377)))))
        
        " ok_cancel;"
        " }";//end
        )
       )
      (close open_file)

      (setq settile_selectground
            (lambda( / func_las bool_loop str_inputdata)
              (setq bool_loop T)
              (while bool_loop
                (setq load_dcl (load_dialog path_selectgrounddcl))
                (new_dialog "Sedit" load_dcl)

                (setq addlist_las
                      (lambda(a)
                        (start_list "groundlas")
                        (mapcar 'add_list
                                (cons ""(mapcar '(lambda(a)
                                                   (vl-string-subst "" "lasgrid-"(car a)))
                                                ls_lasgrid)))
                        (end_list)
                        (set_tile "groundlas" (itoa(1+(if a a -1))))
                        
                        (start_list "insertblock")
                        (mapcar 'add_list
                                (mapcar 'mix_strasc
                                        (list;;作成しない,作成して挿入,作成して挿入しない
                                         (list 20316 25104 12375 12394 12356)
                                         (list 20316 25104 12375 12390 25407 20837)
                                         (list 20316 25104 12375 12390 25407 20837 12375 12394 12356)
                                         )))
                        (end_list)
                        )
                      accept_datainput
                      (lambda(n)
                        (setq str_inputdata(if(= n 0)"las" "xml"))
                        ;;(setq func_las(if(= n 0)load_las_to_grid load_xml_to_grid))
                        (setq int_inputlasinsert(atoi(get_tile "insertblock")))
                        (done_dialog 1)
                        )
                      
                      )


                
                (addlist_las(vl-position str_lasground ls_lasgrid))
                (set_tile "groundvalue"(if height_ground(as-numstr height_ground)""))
                
                (action_tile "inputlas" "(accept_datainput 0)")
                (action_tile "inputxml" "(accept_datainput 1)")
                ;; (mode_tile "inputxml" 1)
                
                (setq accept_ground
                      (lambda( / num val str bool_lay lst str_name bool)
                        (setq num(1-(as-atoi(get_tile "groundlas")))
                              str(get_tile "groundvalue"))
                        (if(if(> num -1)(setq str_name(car(nth num ls_lasgrid))))
                            (setq str_lasground str_name height_ground nil)
                          (if(/= str "")
                              (setq height_ground(atof str)str_lasground nil)
                            (setq bool T)))
                        
                        (if bool
                            (alert(mix_strasc(list 20309 12418 20837 21147 12373 12428 12390 12356 12414 12379 12435)))
                          (done_dialog 1))
                        )
                      )

                
                (action_tile "accept" "(accept_ground)")
                (setq dialog-box (start_dialog))
                (unload_dialog load_dcl)
                
                (if str_inputdata(setq height_ground nil str_lasground(load_las_to_grid str_inputdata)) )
                ;;(if func_las(setq height_ground nil str_lasground(func_las)) )
                
                (if(or str_lasground height_ground) (setq bool_loop nil)
                  (if(=(getvar "DIASTAT")0)
                      (progn ;;設定なしでキャンセルします\nモデル作成には標高の設定が必要なので必ず行ってください
                        (alert(mix_strasc(list 35373 23450 12394 12375 12391 12461 12515 12531 12475 12523 12375 12414 12377 "\n" 12514 12487 12523 20316 25104 12395 12399 27161 39640 12398 35373 23450 12364 24517 35201 12394 12398 12391 24517 12378 34892 12387 12390 12367 12384 12373 12356 )))
                        (setq bool_loop nil)
                        ))
                  )
                )
              
              )
            )
      )

    (setq
     point_guide_center nil
     
     func_loopunique
     (lambda( / dict xrec )
       (if point_guide_center T
         (cond
          ((if str_lasground(setq dict(cdr(assoc str_lasground ls_lasgrid))))
           (if(vl-catch-all-error-p
               (setq xrec(vl-catch-all-apply 'vla-Item (list dict "GRID-SIZE_CENTER"))))
               T
             (progn
               (vla-GetXRecordData xrec 'array_type 'array_data )
               (if array_data
                   (setq z_ave
                         (cadr(mapcar 'vlax-variant-value
                                      (vlax-safearray->list array_data)))
                         point_guide_center(carxyz(getvar "VIEWCTR")z_ave)
                         )
                 )
               ))
           )
          (height_ground
           (setq point_guide_center(carxyz(getvar "VIEWCTR")height_ground))
           )
          )
         )
       
       
       )
     )
    
    ;;uniqueじゃない
    (setq path_addattributedcl(make_dcl_attribute str_path_tempdirectory));;属性
    
    ;;パラメータls_initialparametersによりuniqueじゃない
    ;;(make_dcl_basesetting)
    ((lambda( / num_dclwrite num_column num_row num_row_max);;basicsetting;;外に出す
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
       
       
       (setq settile_basicsetting
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
                               (x-alert(list 12371 12398 12487 12540 12479 12395 12399 str 12364 12394 12356 12383 12417 35501 12415 36796 12417 12414 12379 12435 ))
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
       ) )
    
    
    );;unique



  (progn;;統一したい
    (setq textsize_guide_bo_temp textsize_guide_bo
          y_guidebase_temp y_guidebase
          bool_textclose nil bool_textfold nil)

    (setq ls_pstar
          (list(list 0. 1.0)
               (list 0.224514 0.309017) (list 0.951057 0.309017)
               (list 0.363271 -0.118034) (list 0.587785 -0.809017)
               (list 0.0 -0.381966)
               (list -0.587785 -0.809017) (list -0.363271 -0.118034)
               (list -0.951057 0.309017) (list -0.224514 0.309017)
               (list 0. 1.0)))
    
    
    (setq str_guide_prev "" bool_guideclick T num_gudeentesc 2
          str_editreturn nil int_row_guideprev nil
          ls_str_guide_prev(list)
          )
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
                           ;;setq lst
                           ;;gr5でテキストの行数が一致していれば星を付ける場所を探すだけ

                           ;; (cond
                           ;;  ((= int_grread int_exchageguidegr5)
                           ;;   (setq int_exchageguidegr5 5)
                           ;;   (mapcar '(lambda(str)
                           ;;              (setq ii(1+ ii)
                           ;;                    bool_selectmenu(= int_selectmenu ii)
                           ;;                    bool_starselectmenu(= int_starselectmenu ii))
                           ;;              ;;vl-string-subst
                           ;;              str
                           ;;              )
                           ;;           ls_str_guide_prev)
                             
                           ;;   )
                           ;;  (T
                           ;;   (setq int_exchageguidegr5 nil
                           ;;         lst
                           
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
                                        (if(and bool_selectmenu bool_input)str_guidebackspace)
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
                          '(lambda(lst / p1 p2 c p01 p02)
                             (setq p1(car lst)p2(cadr lst))
                             (if(setq c(caddr lst))T(setq c 7))
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
            (setq bool_replacegrread T int_grread 2 elem_grread 13)
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
            (setq str_input(strcat str_input(chr elem_grread))) )
           ((and bool_input(= elem_grread 8))
            (setq str_input(substr str_input 1(1-(strlen str_input)))) )
           ((and bool_input(vl-position elem_grread(list 92 165 65340 65509 -196 -27)))
            (setq str_input "") )
           ((and bool_input(vl-position elem_grread(list 47 65295 -241)))
            (setq str_input(as-numstr(eval sym_input))) )
           ((and(or bool_input
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

  

  (*error* "")
  )


(defun grfunc_duct3d( / )
  (list
   
   (list
    "exit"
    (cons "INITIAL"(lambda(bool)(if(car bool)(setq bool_loop nil))(if(cadr bool) (list )))) )
   
   (list
    "tempdistlength" ;;edsym 33
    (cons
     "INITIAL"
     (lambda(bool)
       (if(car bool)
           (setq bool_point T
                 ls_guideexplane nil
                 p_tempdist0 nil p_tempdist1 nil ls_strtempdist(list)
                 ls_guidemenu nil bool_linedisp nil
                 func_guidemenu
                 (lambda()
                   (mix_strasc
                    (list "2" 28857 12434 12463 12522 12483 12463 12375 12390 36317 38626 12434 28204 12427 12371 12392 12364 12391 12365 12414 12377
                          "\n" 12300 "!" 12301 12461 12540 12434 25276 12377 12392 28204 12387 12383 31623 25152 12434 32218 20998 12458 12502 12472 12455 12463 12488 12392 12375 12390 29983 25104 12391 12365 12427 12514 12540 12489 12395 20999 12426 26367 12360 12364 12391 12365 12414 12377
                          "\n" 12371 12398 32218 20998 12399 12467 12510 12531 12489 32066 20102 26178 12395 21066 38500 12373 12428 12414 12377 ))
                   
                   )
                 )
         )
       (if(cadr bool) (list ))
       ))

    (cons
     "GUIDE"
     (lambda( / bool p);;(d- grdisp_
       (setq bool(if(=(type elem_grread)'LIST)p_tempdist0)
             p(if p_tempdist1 p_tempdist1 elem_grread) )
       (list "\n{\\C" str_gcol_g ";" 36317 38626 "}" 9 ":"(if bool(as-numstr(distance p_tempdist0 p))"");;距離
             "\n{\\C" str_gcol_g ";" 916 "X}" 9 ":" (if bool(as-numstr(-(car p)(car p_tempdist0)))"")
             "\n{\\C" str_gcol_g ";" 916 "Y}" 9 ":" (if bool(as-numstr(-(cadr p)(cadr p_tempdist0)))"")
             "\n{\\C" str_gcol_g ";" 916 "Z}" 9 ":" (if bool(as-numstr(-(caddr p)(caddr p_tempdist0))) "")
             "\n{\\C" str_gcol_c "; !}" 9 ": " 35336 28204 12375 12383 32218 12434
             12467 12510 12531 12489 32066 20102 26178 12414 12391
             " " 12304
             (if bool_linedisp (list "{\\C" str_gcol_y ";" 27531 12377 "}")
               (list "{\\C" str_gcol_c ";" 27531 12373 12394 12356 "}"))
             12305
             ;;計測した線をコマンド終了時まで残す残さない
             "\n{\\C" str_gcol_c "; S}" 9 ": "
             12473 12490 12483 12503 20999 26367
             "  " 12304 32 (if bool_snap "ON" "OFF") 32 12305
             "\n"  21491 12463 12522 12483 12463 ",Enter:" 25147 12427
             ;;右クリック,Enter:戻る
             )
       ))
    
    (cons
     "MOVE"
     (lambda()(if p_tempdist0 (grdraw p_tempdist0(if p_tempdist1 p_tempdist1 elem_grread) 2)) )
     )

    (cons
     "CLICK"
     (lambda( / );;(d- gr3_profile()
       (cond
        (bool_linedisp
         (setq p_tempdist0 elem_grread
               p_tempdist1(getpoint p_tempdist0)
               vnam(vla-addline
                    (vla-get-modelspace(vla-get-activedocument(vlax-get-acad-object)))
                    (vlax-3d-point p_tempdist0)(vlax-3d-point p_tempdist1))
               )
         (vla-put-color vnam 4)
         (addkillobj vnam)
         )
        ((null p_tempdist0)(setq p_tempdist0 elem_grread))
        ((null p_tempdist1)
         (setq p_tempdist1 elem_grread)
         (if bool_inputmeasure
             (setq str_inputmeasure(as-numstr(distance p_tempdist0 p_tempdist1))
                   bool_replacegrread T int_grread 2 elem_grread 13
                   str_edit str_editreturn str_editreturn nil))
         )
        (T(setq p_tempdist0 elem_grread p_tempdist1 nil))
        )
       ))

    (cons
     "KEYBOAD"
     (lambda();;(d- gr2_profile()
       (cond
        ((or(= elem_grread 13)(= int_grread 25))
         (setq bool_inputmeasure nil)
         (setq str_edit str_editreturn str_editreturn nil ))
        ((vl-position elem_grread(list 33 65281 -255))
         ;;(list 65 97 65296 65313 -240 -223))
         (setq bool_linedisp(null bool_linedisp))
         )
        )
       ))
    
    )
   
   

   (list
    "home" ;;edsym editsymbol
    (cons
     "INITIAL"
     (lambda(bool);;initial
       (if(car bool)
           (setq bool_point nil 
                 bool_selectent nil bool_select nil int_selectmode nil
                 ls_ssget(list) xtype_ssget nil xdata_ssget nil

                 func_guidemenu
                 (lambda()
                   (mix_strasc;;各項目にカーソルを合わせて★のとき「？」を入力してください
                    (list 21508 38917 30446 12395 12459 12540 12477 12523 12434 21512 12431 12379 12390 9733 12398 12392 12365 12300 65311 12301 12434 20837 21147 12375 12390 12367 12384 12373 12356  )
                    ) )
                 
                 ls_guideexplane
                 (list)
                 
                 ls_guidemenu
                 (list
                  (list(list 49 ) ;;線形の投影
                       (cons "ITEM"(list 32218 24418 12398 25237 24433))
                       (cons "NEXTMODE" "projectlinear")
                       ;;平面図の線形などを選択して現在設定している標高に投影する
                       (cons "HELP"(lambda()(mix_strasc(list 24179 38754 22259 12398 32218 24418 12394 12393 12434 36984 25246 12375 12390 29694 22312 35373 23450 12375 12390 12356 12427 27161 39640 12395 25237 24433 12377 12427 ))))
                       )
                  (list(list 50 );;管路作成
                       (cons "ITEM"(list 31649 36335 20316 25104))
                       (cons "NEXTMODE" "makeductmain")
                       ;;管の中心が通る位置を設定した標高からの深さまたは標高自体を指定して管を作成する
                       (cons "HELP"(lambda()(mix_strasc(list 31649 12398 20013 24515 12364 36890 12427 20301 32622 12434 35373 23450 12375 12383 27161 39640 12363 12425 12398 28145 12373 12414 12383 12399 27161 39640 33258 20307 12434 25351 23450 12375 12390 31649 12434 20316 25104 12377 12427  ))))
                       )
                  (list(list 51 );;特殊部作成
                       (cons "ITEM"(list 29305 27530 37096 20316 25104))
                       (cons "NEXTMODE" "makeccboxmain")
                       ;;特殊部の角およびマンホール中心を指定して、現在設定している標高に合わせて配置する
                       (cons "HELP"(lambda()(mix_strasc(list 29305 27530 37096 12398 35282 12362 12424 12403 12510 12531 12507 12540 12523 20013 24515 12434 25351 23450 12375 12390 12289 29694 22312 35373 23450 12375 12390 12356 12427 27161 39640 12395 21512 12431 12379 12390 37197 32622 12377 12427 ))))
                       )
                  (list(list 52 );;情報を付与する
                       (cons "ITEM"(list 24773 22577 12434 20184 19982 12377 12427))
                       (cons "NEXTMODE" "addattribute")
                       ;;本コマンドで作成したオブジェクトに対し、属性情報や施工情報などを付与する
                       (cons "HELP"(lambda()(mix_strasc(list 26412 12467 12510 12531 12489 12391 20316 25104 12375 12383 12458 12502 12472 12455 12463 12488 12395 23550 12375 12289 23646 24615 24773 22577 12420 26045 24037 24773 22577 12394 12393 12434 20184 19982 12377 12427 ))))
                       )
                  (list(list 53 );;土被り、干渉確認
                       (cons "ITEM"(list 22303 34987 12426 12289 24178 28169 30906 35469))
                       (cons "NEXTMODE" "influencecheck")
                       ;;作成した管路の土被り許容値を設定して、それより浅い箇所を表記する,管同士の干渉している箇所を表記する
                       (cons "HELP"(lambda()(mix_strasc(list  20316 25104 12375 12383 31649 36335 12398 22303 34987 12426 35377 23481 20516 12434 35373 23450 12375 12390 12289 12381 12428 12424 12426 27973 12356 31623 25152 12434 34920 35352 12377 12427 "\n" 31649 21516 22763 12398 24178 28169 12375 12390 12356 12427 31623 25152 12434 34920 35352 12377 12427 ))))
                       
                       )
                  (list(list 54 );;データの入出力
                       (cons "ITEM"(list 12487 12540 12479 12398 20837 20986 21147))
                       (cons "NEXTMODE" "savecsvifc")
                       ;;本コマンドで作成したオブジェクトの情報をCSV,IFCで出力する\n作成したCSVまたはそれを編集したものを読み込んでオブジェクトを再現する
                       (cons "HELP"(lambda()(mix_strasc(list 26412 12467 12510 12531 12489 12391 20316 25104 12375 12383 12458 12502 12472 12455 12463 12488 12398 24773 22577 12434 "CSV,IFC" 12391 20986 21147 12377 12427 "\n" 20316 25104 12375 12383 "CSV" 12414 12383 12399 12381 12428 12434 32232 38598 12375 12383 12418 12398 12434 35501 12415 36796 12435 12391 12458 12502 12472 12455 12463 12488 12434 20877 29694 12377 12427 ))))
                       )
                  (list(list 83 );;基本設定
                       (cons "ITEM"(list 22522 26412 35373 23450))
                       (cons "LOADFUNCTION" settile_basicsetting)
                       ;;実行すると別ウィンドウが開くのでそこで内容を確認してください
                       (cons "HELP"(lambda()(mix_strasc(list 23455 34892 12377 12427 12392 21029 12454 12451 12531 12489 12454 12364 38283 12367 12398 12391 12381 12371 12391 20869 23481 12434 30906 35469 12375 12390 12367 12384 12373 12356  ))))
                       )
                  (list(list "ENTER");;終了
                       (cons "ITEM"(list 32066 20102))
                       (cons "NEXTMODE" "exit")
                       
                       )
                  )
                 ls_home_guide
                 (cons(list(cons "NEXTMODE" "home")
                           (cons "ITEM"(list 12513 12452 12531 12513 12491 12517 12540 )) )
                      ls_guidemenu)
                 ))
       
       (if(cadr bool)
           (progn
             (list ;;コマンドするたびに直すもの
              (list nil 'int_ductmode 0)
              (list nil 'int_ccboxmode 0)
              (list nil 'int_cancelduct 0)
              (list nil 'int_hidesolid 0)
              (list T 'int_projectnode 0)
              ;; (list nil 'int_colduct_temp nil)
              (list nil 'str_ductname nil)
              (list nil 'int_adddepth nil)
              ;; (list nil 'int_colduct_temp nil)
              ;; (list nil 'int_protectcon_temp nil)
              ;; (list nil 'radius_bend_temp nil)
              ;; (list nil 'radius_bend_temp nil)
              ;; (list nil 'diam_duct_temp nil)
              ;; (list nil 'width_protect_temp nil)
              ;; (list nil 'height_protect_temp nil)
              ;; (list nil 'filet_protect_temp nil)
              
              )

             
             ))
       ))

    ;;GUIDE,MOVE,CLICK,KEYBOAD
    )
   
   (list
    "projectlinear" ;;edsym
    (cons
     "INITIAL"
     (lambda(bool);;initial
       (if(car bool)
           (setq bool_point nil 
                 bool_selectent T bool_select T int_selectmode 0
                 ls_ssget(list(cons -4 "<OR")
                              (cons 0 "LINE,LWPOLYLINE,POLYLINE")
                              (cons -4 "<AND")
                              (cons 0 "INSERT")(list -3(list "terraduct3d"))
                              (cons -4 "AND>")
                              (cons -4 "OR>")
                              )
                 ls_ssgetinsert(list(cons 0 "INSERT")
                                    (cons -4 "<NOT")(list -3(list "terraduct3d"))(cons -4 "NOT>") )
                 xtype_ssget nil xdata_ssget nil
                 int_selectproject 0

                 str_projectname nil
                 func_guidemenu
                 (lambda()
                   (mix_strasc
                    (if ls_vnam_select
                        (list "Enter" 12391 29694 22312 12398 36984 25246 12434 29694 22312 35373 23450 12375 12390 12356 12427 27161 39640 12395 25237 24433 12375 12414 12377 "\n" 27161 39640 12398 35373 23450 12364 12373 12428 12390 12356 12394 12356 12392 12365 23455 34892 12391 12365 12414 12379 12435 );;Enterで現在の選択を現在設定している標高に投影します\n標高の設定がされていないとき実行できません
                      (list 24179 38754 22259 12398 31649 36335 12398 32218 12434 36984 25246 12375 12390 12367 12384 12373 12356 ));;平面図の管路の線を選択してください
                    ) )
                 
                 ls_guideexplane
                 (mapcar
                  'mix_strasc
                  (list(list 32218 20998 "," 12509 12522 12521 12452 12531 12394 12393 12398 32218 24418 12458 12502 12472 12455 12463 12488 12434 36984 25246 12375 12390 )
                       (list 29694 22312 36984 25246 20013 12398 22320 34920 38754 27161 39640 12395 25237 24433 12377 12427 )
                       (list 20870 24359 12434 21547 12416 12509 12522 12521 12452 12531 12398 12392 12365 12289 20870 24359 19978 12398 28857 12399 25237 24433 12373 12428 12414 12379 12435 )
                       (list 25237 24433 12375 12383 12458 12502 12472 12455 12463 12488 12434 36984 25246 12377 12427 12392 21517 31216 12434 35373 23450 12391 12365 12414 12377
                             "(" 35373 23450 12399 20219 24847 ")")
                       ;;線分,ポリラインなどの線形オブジェクトを選択して
                       ;;現在選択中の地表面標高に投影する
                       ;;円弧を含むポリラインのとき、円弧上の点は投影されません
                       ;;投影したオブジェクトを選択すると名称を設定できます(設定は任意です)
                       ))
                 
                 ls_guidemenu
                 (list
                  
                  (list(list 76);; 地表面標高
                       (cons "ITEM"(mix_strasc(list 22320 34920 38754 27161 39640)))
                       (cons "STATUS"
                             (lambda()
                               (if str_lasground
                                   (mix_strasc
                                    (list "{\\C" str_gcol_c ";" 12487 12540 12479 "} : "
                                          (vl-string-subst "" "lasgrid-" str_lasground)))
                                 (if height_ground
                                     (mix_strasc
                                      (list"{\\C" str_gcol_g ";" 27161 39640 "} : "
                                           (as-numstr height_ground)))
                                   (mix_strasc
                                    (list "{\\C" str_gcol_r ";"
                                          36984 25246 12373 12428 12390 12356 12414 12379 12435 "}"))
                                   ))
                               ))
                       (cons "LOADFUNCTION"(lambda()(settile_selectground)))
                       ;;使用する標高を\n-las読込\n-xml読込\n-一定標高の数値入力\nから選択できます
                       (cons "HELP"(lambda()(mix_strasc(list 20351 29992 12377 12427 27161 39640 12434 "\n- las" 35501 36796 "\n- xml" 35501 36796 "\n- " 19968 23450 27161 39640 12398 25968 20516 20837 21147 "\n" 12363 12425 36984 25246 12391 12365 12414 12377 ))))
                       )

                  
                  ;; (list(list 78);;名称
                  ;;      (cons "ITEM"(mix_strasc(list 21517 31216)))
                  ;;      (cons "INPUTSTR"
                  ;;            (lambda( / str i bool func str_bname str_hand)
                  ;;              (if(=(length ls_vnam_select)1)
                  ;;                  (if str_projectname 'str_projectname
                  ;;                    (progn
                  ;;                      (setq str_hand(vla-get-handle(car ls_vnam_select))i 0)
                  ;;                      (while
                  ;;                          (progn
                  ;;                            (setq str_bname(strcat str_hand "$"(itoa(setq i(1+ i)))))
                  ;;                            (null
                  ;;                             (vl-catch-all-error-p
                  ;;                              (vl-catch-all-apply 'vla-Item(list vnam_blocktable str_bname))))
                  ;;                            )
                  ;;                        )
                  ;;                      (setq str_projectname str_bname)
                  ;;                      'str_projectname ))
                  ;;                (progn;;
                  ;;                  (setq str_projectname nil)
                  ;;                  nil
                  ;;                  )
                  ;;                )
                  ;;              )
                  ;;            )
                  ;;      (cons "LOADFUNCTION"
                  ;;            (lambda(str / bool)
                  ;;              (setq bool
                  ;;                    (vl-catch-all-error-p
                  ;;                     (vl-catch-all-apply 'vla-Item(list vnam_blocktable str)))
                  ;;                    )
                  ;;              (if bool T
                  ;;                (if(= int_allow_overwrite 0)nil
                  ;;                  (progn
                  ;;                    (alert
                  ;;                     (mix_strasc(list 26082 12395 23384 22312 12377 12427 12502 12525 12483 12463 21517 31216 12391 12377 12364 19978 26360 12365 12373 12428 12414 12377 ))
                  ;;                     )
                  ;;                    T)))
                  ;;              )
                  ;;            )
                  ;;      (cons "STRINPUTALERT"
                  ;;            (mix_strasc(list 12502 12525 12483 12463 21517 31216 12364 26082 12395 23384 22312 12375 12390 12356 12414 12377 )))
                  ;;      ;;選択が単一のときのみ設定できます
                  ;;      (cons "HELP"(lambda()(mix_strasc(list 36984 25246 12364 21336 19968 12398 12392 12365 12398 12415 35373 23450 12391 12365 12414 12377))))
                  ;;      )
                  
                  (list(list 65);; 投影ピッチ
                       (cons "ITEM"(list 25237 24433 12500 12483 12481))
                       (cons "INPUT"(lambda()'pitch_project))
                       ;;選択した線形は、ここで設定したピッチで分割され、その点における標高を取得します\n元々の線形の節点に当たる部分は必ず投影されます\nピッチが0のとき節点のみが投影されます
                       (cons "HELP"(lambda()(mix_strasc(list 36984 25246 12375 12383 32218 24418 12399 12289 12371 12371 12391 35373 23450 12375 12383 12500 12483 12481 12391 20998 21106 12373 12428 12289 12381 12398 28857 12395 12362 12369 12427 27161 39640 12434 21462 24471 12375 12414 12377 "\n" 20803 12293 12398 32218 24418 12398 31680 28857 12395 24403 12383 12427 37096 20998 12399 24517 12378 25237 24433 12373 12428 12414 12377 "\n" 12500 12483 12481 12364 "0" 12398 12392 12365 31680 28857 12398 12415 12364 25237 24433 12373 12428 12414 12377(if int_selectmenu str_guide_inputval str_guide_selectval)  ))))
                       )
                  
                  ;; (list(list 50);;投影前に節点だった箇所に円
                  ;;      (cons "ITEM"(list  25237 24433 21069 12395 31680 28857 12384 12387 12383 31623 25152 12395 20870 ))
                  ;;      (cons "INPUTSWITCH"
                  ;;            (lambda()
                  ;;              (list 'int_projectnode
                  ;;                    (mapcar 'mix_strasc 
                  ;;                            (list(list "{\\C" str_gcol_y ";" 12354 12426 "}")
                  ;;                                 (list "{\\C" str_gcol_c ";" 12394 12375 "}")))
                  ;;                    )))
                  ;;      ;;ピッチ設定しているとき投影時に節点数が増え、元々節点だった箇所がわかりづらくなるので、円形のマーキングを作るかどうかの設定を行います
                  ;;      (cons "HELP"(lambda()(mix_strasc(list 12500 12483 12481 35373 23450 12375 12390 12356 12427 12392 12365 25237 24433 26178 12395 31680 28857 25968 12364 22679 12360 12289 20803 12293 31680 28857 12384 12387 12383 31623 25152 12364 12431 12363 12426 12389 12425 12367 12394 12427 12398 12391 12289 "\n" 20870 24418 12398 12510 12540 12461 12531 12464 12434 20316 12427 12363 12393 12358 12363 12398 35373 23450 12434 34892 12356 12414 12377 ))))
                  ;;      )
                  
                  (list(list 80);;選択モード切替
                       (cons "ITEM"(list 36984 25246 12514 12540 12489 20999 26367))
                       (cons "INPUTSWITCH"
                             (lambda()
                               (list 'int_selectmode
                                     (mapcar 'mix_strasc
                                             (list(list "{\\C" str_gcol_y ";" 36984 25246 "}")
                                                  (list "{\\C" str_gcol_c ";" 35299 38500 "}")))
                                     )))
                       (cons "HELP"(lambda()str_guide_selectmode))
                       )

                  (list(list 81);;投影済みの線をクリックしたとき
                       (cons "ITEM"(list 25237 24433 28168 12415 12398 32218 12434 12463 12522 12483 12463 12375 12383 12392 12365 ))
                       (cons "INPUTSWITCH"
                             (lambda()
                               (list 'int_selectproject
                                     (mapcar 'mix_strasc;;名称を設定する,標高変更選択に加える
                                             (list(list "{\\C" str_gcol_c ";"
                                                        21517 31216 12434 35373 23450 12377 12427 "}")
                                                  (list "{\\C" str_gcol_g ";"
                                                        27161 39640 22793 26356 36984 25246 12395 21152 12360 12427 "}")))
                                     )))
                       ;;名称設定時にはこの設定が必要です\n標高変更選択はこの設定を変えなくても範囲選択をすれば対象とすることができます\nクリックで対象としたいときはこの設定が必要です
                       (cons "HELP"(lambda()(mix_strasc(list 21517 31216 35373 23450 26178 12395 12399 12371 12398 35373 23450 12364 24517 35201 12391 12377 "\n" 27161 39640 22793 26356 36984 25246 12399 12371 12398 35373 23450 12434 22793 12360 12394 12367 12390 12418 31684 22258 36984 25246 12434 12377 12428 12400 23550 35937 12392 12377 12427 12371 12392 12364 12391 12365 12414 12377 "\n" 12463 12522 12483 12463 12391 23550 35937 12392 12375 12383 12356 12392 12365 12399 12371 12398 35373 23450 12364 24517 35201 12391 12377 )))) 
                       )
                  
                  (list(list nil)
                       (cons "ITEM"(list ))
                       (cons "BOOL"
                             (lambda()
                               ls_vnam_select
                               ))
                       (cons
                        "STATUS"
                        (lambda( / bool_depth ii ls_out)
                          (if ls_vnam_select
                              (list
                               (list "{\\C" str_gcol_y ";" 36984 25246 12354 12426  " " 9733 "}")
                               ))
                          )
                        )
                       )
                  
                  (list(list "ENTER");;メインメニューへ
                       (cons "ITEM"(list 12513 12452 12531 12513 12491 12517 12540 12408))
                       (cons "NEXTMODE" "home"))
                  )
                 
                 
                 ))
       
       (if(cadr bool)
           (progn
             (list ;;(list nil 'sym val)
              )
             ))
       ))

    ;;GUIDE,MOVE

    (cons
     "CLICK"
     (lambda()
       (if(if(setq set_ent(ssget elem_grread(list(cons 0 "INSERT"))))
              (progn
                
                (setq vnam(vlax-ename->vla-object(ssname set_ent 0)))
                (vla-getXData vnam "terraduct3d" 'array_Type 'array_Data )
                (if array_data
                    (setq ls_xdata
                          (split_list 0(mapcar 'vlax-variant-value
                                               (vlax-safearray->list array_data)))
                          str_type(cdr(assoc "terraduct3d" ls_xdata)))
                  (setq str_type nil) )
                (= str_type "PROJECT")))
           (cond
            ((= int_selectproject 0)
             (setq str_bname(vla-get-name vnam)
                   str_origin str_bname)
             
             (settile_strinput
              'str_bname
              (lambda(str / bool)
                (if(or(= str str_origin)
                      (vl-catch-all-error-p
                       (vl-catch-all-apply 'vla-Item(list vnam_blocktable str)))
                      )
                    T
                  (if(= int_allow_overwrite 0)nil
                    (progn
                      (alert
                       (mix_strasc(list 26082 12395 23384 22312 12377 12427 12502 12525 12483 12463 21517 31216 12391 12377 12364 19978 26360 12365 12373 12428 12414 12377 ))
                       )
                      T)))
                )
              
              (mix_strasc(list 21517 31216 22793 26356 ));;名称変更
              (mix_strasc(list 12502 12525 12483 12463 21517 31216 12364 26082 12395 23384 22312 12375 12390 12356 12414 12377 ))
              )

             (if(and(=(getvar "DIASTAT")1)(/= str_origin str_bname))
                 (vla-put-name(vl-catch-all-apply 'vla-Item(list vnam_blocktable str_origin))
                              str_bname))
             (setq bool_select nil p_selerac nil )
             
             )
            ((= int_selectproject 1)
             
             
             
             
             
             )
            )
         )
       
       ))
    
    (cons
     "KEYBOAD"
     (lambda( / ls_p bool_first radius_node ls_vnam block );;(d- gr2_home()

       (cond
        ((or(= elem_grread 13)(= int_grread 25))
         
         (mapcar '(lambda(v)
                    (if(if v(vl-position v ls_vnam_highlight))
                        (progn
                          (vla-Highlight v :vlax-false)
                          (setq ls_vnam_highlight(vl-remove v ls_vnam_highlight))
                          ))
                    )
                 ls_vnam_select)
         
         (while ls_vnam_select
           
           (setq vnam(car ls_vnam_select)ls_vnam_select(cdr ls_vnam_select))
           (if(if(setq bool_reference(=(vla-get-objectname vnam)"AcDbBlockReference"))
                  (progn
                    (vla-getXData vnam "terraduct3d" 'array_Type 'array_Data )
                    (if array_data
                        (setq ls_xdata
                              (split_list 0(mapcar 'vlax-variant-value
                                                   (vlax-safearray->list array_data)))
                              str_type(cdr(assoc "terraduct3d" ls_xdata)))
                      (setq str_type nil) )
                    (= str_type "PROJECT")
                    
                    ))
               (progn
                 (setq ls_p_center(list))
                 (vlax-for
                  obj
                  (vla-Item vnam_blocktable(vla-get-name vnam))
                  (cond
                   ((=(vla-get-ObjectName obj)"AcDb3dPolyline")
                    (setq vnam_update obj
                          ls_p(split_list 3(vlax-safearray->list(vlax-variant-value(vla-get-coordinates obj))))
                          )
                    )
                   ((=(vla-get-ObjectName obj)"AcDbCircle")
                    (setq p(vlax-safearray->list(vlax-variant-value(vla-get-center obj)))
                          ls_p_center (cons p ls_p_center)
                          p(car(project_to_ground
                                (list p)(list 0. 0. 1.)(list str_lasground height_ground)))
                          )
                    (vla-put-center obj(vlax-3d-point p))
                    )
                   )
                  )

                 (setq ls_p
                       (vl-remove-if
                        '(lambda(p / bool)
                           (setq ls_dummy ls_p_center bool T)
                           (while ls_dummy
                             (if(<(distance p(car ls_dummy))1e-8)
                                 (setq bool nil ls_dummy nil)
                               (setq ls_dummy(cdr ls_dummy)))
                             )
                           bool)
                        ls_p))
                 
                 (setq bool_first T ls_int_node(list 0) int_node 0
                       ls_p(apply 'append
                                  (mapcar
                                   '(lambda(p1 p2 / d n v ls_out ls_int)
                                      (setq d(distance p1 p2)
                                            v(unit_vector(mapcar '- p2 p1))
                                            n(if(= pitch_project 0.)1
                                               (1+(fix(/ d pitch_project))))
                                            d(/ d n)
                                            )
                                      (if bool_first
                                          (setq ls_int(inclist 0(1+ n))bool_first nil )
                                        (setq ls_int(inclist 1(1+ n))))
                                      
                                      (setq int_node(+ int_node n)
                                            ls_int_node(cons int_node ls_int_node))
                                      (mapcar '(lambda(i)(mapcar '(lambda(a b)(+ a(* i d b))) p1 v))
                                              ls_int)
                                      )
                                   ls_p(cdr ls_p)))
                       
                       ls_p(project_to_ground ls_p(list 0. 0. 1.)(list str_lasground height_ground))
                       
                       ls_p(apply 'append ls_p)
                       )
                 
                 (setq array_p(vlax-make-safearray vlax-vbDouble(cons 0 (1-(length ls_p)))))
                 (vlax-safearray-fill array_p ls_p)
                 (vla-put-coordinates vnam_update array_p)
                 
                 )
             (progn
               (setq ls_p(vl-catch-all-apply 'vla-get-coordinates(list vnam))
                     p0(vl-catch-all-apply 'vlax-curve-getstartpoint(list vnam))
                     p1(vl-catch-all-apply 'vlax-curve-getendpoint(list vnam))
                     int_col(get_visual_color vnam)
                     )
               
               (if(or(vl-catch-all-error-p p0) (vl-catch-all-error-p p1))
                   (progn
                     nil
                     )
                 (progn
                   (if(vl-catch-all-error-p ls_p)(setq ls_p(list p0 p1))
                     (setq ls_p(vlax-safearray->list(vlax-variant-value ls_p))
                           ls_p(if(=(vla-get-ObjectName vnam)"AcDbPolyline")
                                   (progn
                                     (setq z(vla-get-elevation vnam))
                                     (mapcar '(lambda(a)(carxyz a z))(split_list 2 ls_p))
                                     )
                                 (split_list 3 ls_p))
                           )
                     )
                   
                   (setq str_hand(vla-get-handle vnam)i 0)
                   
                   ;;(setq str_projectname
                   
                   (while
                       (progn
                         (setq str_bname(strcat str_hand "ROAD$"(itoa(setq i(1+ i)))))
                         (null
                          (vl-catch-all-error-p
                           (vl-catch-all-apply 'vla-Item(list vnam_blocktable str_bname))))
                         )
                     )
                   
                   (setq bool_first T ls_int_node(list 0) int_node 0
                         ls_p(apply 'append
                                    (mapcar
                                     '(lambda(p1 p2 / d n v ls_out ls_int)
                                        (setq d(distance p1 p2)
                                              v(unit_vector(mapcar '- p2 p1))
                                              n(if(= pitch_project 0.)1
                                                 (1+(fix(/ d pitch_project))))
                                              d(/ d n)
                                              )
                                        (if bool_first
                                            (setq ls_int(inclist 0(1+ n))bool_first nil )
                                          (setq ls_int(inclist 1(1+ n))))
                                        
                                        (setq int_node(+ int_node n)
                                              ls_int_node(cons int_node ls_int_node))
                                        (mapcar '(lambda(i)(mapcar '(lambda(a b)(+ a(* i d b))) p1 v))
                                                ls_int)
                                        )
                                     ls_p(cdr ls_p)))
                         
                         ls_p(project_to_ground ls_p(list 0. 0. 1.)(list str_lasground height_ground))
                         ;; ls_p(vl-remove nil ls_p)

                         radius_node(if(= pitch_project 0.)0.05 pitch_project)
                         
                         ls_vnam
                         (if(= int_projectnode 1)nil
                           (mapcar
                            '(lambda(n / p v)
                               (if(setq p(nth n ls_p))
                                   (progn
                                     (setq v(vla-addcircle
                                             (vla-get-modelspace(vla-get-activedocument(vlax-get-acad-object)))
                                             (vlax-3d-point p)radius_node))
                                     
                                     ))
                               )
                            ls_int_node))
                         ls_vnam(vl-remove nil ls_vnam)
                         
                         ls_p(apply 'append ls_p)
                         )

                   (setq block(vla-Add vnam_blockTable(vlax-3d-point 0 0 0)str_bname)
                         ls_vla-release(cons block ls_vla-release))
                   ;; ;; 含まれるすべてのオブジェクトを削除
                   ;; (vlax-for obj block(vla-delete obj))
                   (setq array_p(vlax-make-safearray vlax-vbDouble(cons 0 (1-(length ls_p)))))
                   (vlax-safearray-fill array_p ls_p)

                   (setq vnam(vla-Add3dPoly
                              (vla-get-ModelSpace(vla-get-ActiveDocument(vlax-get-acad-object)))array_p)
                         ls_vnam(cons vnam ls_vnam)
                         )
                   (vla-put-color vnam int_col)

                   (vla-copyobjects(vla-get-ActiveDocument(vlax-get-acad-object))
                                   (vlax-make-variant
                                    (vlax-safearray-fill
                                     (vlax-make-safearray
                                      vlax-vbObject(cons 0(1-(length ls_vnam))) )
                                     ls_vnam)
                                    )
                                   block)
                   
                   (setq vnam(vla-InsertBlock
                              (vla-get-ModelSpace(vla-get-ActiveDocument(vlax-get-acad-object)))
                              (vlax-3d-point 0 0 0)str_bname 1 1 1 0)
                         )
                   
                   (set_xda vnam(list(cons 1000 "PROJECT")
                                     (cons 1000 "PROJECT")(cons 1000 str_hand))
                            "terraduct3d")
                   
                   
                   (mapcar '(lambda(a)(vla-delete a))ls_vnam)
                   
                   ((lambda(v)
                      (vlax-release-object v)
                      (setq ls_vla-release(vl-remove v ls_vlarelease)))
                    block)
                   ))
               ))
           )
         
         (vla-Regen
          (vla-get-ActiveDocument (vlax-get-acad-object))
          acAllViewports
          )
         
         )
        )
       
       ))
    
    )

   (list
    "makeductmain" ;;edsym
    (cons
     "INITIAL"
     (lambda(bool);;initial
       (if(car bool)
           (progn
             (setq bool_point nil
                   bool_selectent nil bool_select nil int_selectmode -1
                   ls_ssget nil xtype_ssget nil xdata_ssget nil
                   ;;ls_ssgetを変えるときは注意
                   )

             (if(vl-position str_editreturn(list "insertarc"))
                 T
               (setq vnam_road nil ls_vnam_duct(list) vnam_depth nil entna_depth nil
                     int_ductdepth nil
                     int_max_duct 0 int_min_duct 0
                     bool_ductedit nil bool_solidon nil
                     int_connecttype 0
                     vnam_currentinsert nil
                     )
               )
             (setq int_selectmenu_ductedit nil int_adddepth nil
                   p_roadclick_temp nil

                   func_guidemenu
                   (lambda()
                     (mix_strasc
                      (list
                       (cond
                        
                        ((and(null str_lasground)(null height_ground))
                         ;;まず地表面標高を決定してください
                         (list 12414 12378 22320 34920 38754 27161 39640 12434 27770 23450 12375 12390 12367 12384 12373 12356))
                        (entna_depth
                         ;;既に作成済みの深度の位置を変更しています
                         (list 26082 12395 20316 25104 28168 12415 12398 28145 24230 12398 20301 32622 12434 22793 26356 12375 12390 12356 12414 12377 )
                         )
                        
                        ((= int_ductmode 0);;作成編集
                         (if(or ls_vnam_duct bool_ductedit)
                             (list
                              30452 32218 37096
                              ((lambda( / ii)
                                 (setq ii 0)
                                 (mapcar '(lambda(lst)
                                            (if(assoc "DEPTH" lst)(setq ii(1+ ii))))
                                         ls_vnam_duct)
                                 (if(=(rem ii 2)0) "< 1 > " "< 2 > ")
                                 ))
                              28857 30446 12434 36984 25246
                              "\n\n"
                              ;;編集項目
                              12304 32232 38598 38917 30446 12305 
                              "\n" 28145 24230 23544 27861 "(" 25991 23383 ");" 28145 12373 22793 26356
                              "\n" 28145 24230 23544 27861 "(" 25991 23383 20197 22806 ");" 20301 32622 22793 26356
                               ;;深さ寸法(文字);深さ変更,深さ寸法(文字以外);位置変更

                              "\n" 20870 24359 23544 27861 "(" 25991 23383 ");" 26354 12370 21322 24452
                              "\n" 20870 24359 23544 27861 "(" 25991 23383 20197 22806 ");"
                              20301 32622 22793 26356 "(" 21487 33021 12394 26178 12398 12415 ")" 
                              ;;円弧部;曲げ半径,
                              "\n" 30452 32218 37096 ";" 30452 32218 37096 12434 21066 38500
                              
                              )
                           
                           (list
                            (if(= int_allow_noroad_temp 1)
                                ;;任意点をクリックすると経路を使わず標高を指定できます
                                (list 20219 24847 28857 12434 12463 12522 12483 12463 12377 12427 12392 32076 36335 12434 20351 12431 12378 27161 39640 12434 25351 23450 12391 12365 12414 12377))
                            ;;線形オブジェクトを選択すると経路として設定します
                            "\n" 32218 24418 12458 12502 12472 12455 12463 12488 12434 36984 25246 12377 12427 12392 32076 36335 12392 12375 12390 35373 23450 12375 12414 12377
                            ;;作成済みの管路を選択して編集開始
                            "\n" 20316 25104 28168 12415 12398 31649 36335 12434 36984 25246
                            32232 38598 38283 22987
                            )
                           )
                         )
                        
                        ((= int_ductmode 1);;更新コピー
                         (list
                          ;;作成済みの管路をクリックすると実行
                          20316 25104 28168 12415 12398 31649 36335 12434 12463 12522 12483 12463 12377 12427 12392
                          (if(and(= depth_duct 0.)(= offset_duct 0.))
                              (list 26356 26032) (lis 12467 12500 12540))
                          23455 34892
                          )
                         )
                        
                        ((= int_ductmode 2) ;;削除
                         (list
                          ;;削除対象を選択してください
                          ;;\n管路を対象に選択でき、その他のものは選択しようとしてもハイライトされません
                          ;;\n選択有り無し
                          ;;\n選択後Enterで削除
                          21066 38500 23550 35937 12434 36984 25246 12375 12390 12367 12384 12373 12356
                          "\n" 31649 36335 12434 23550 35937 12395 36984 25246 12391 12365 12289 12381 12398 20182 12398 12418 12398 12399 36984 25246 12375 12424 12358 12392 12375 12390 12418 12495 12452 12521 12452 12488 12373 12428 12414 12379 12435
                          "\n" 36984 25246 12304
                          (if ls_vnam_select(list 26377 12426)(list 28961 12375)) 12305
                          "\n" 36984 25246 24460 "Enter" 12391 21066 38500
                          )
                         )
                        );;cond
                       
                       )
                      )
                     )
                   
                   ls_guideexplane
                   (mapcar
                    'mix_strasc
                    (list(list " - " 12300 "R=" 8734 12301 12399 20870 24359 12364 12394 12356 12371 12392 12434 34920 12377 )
                         ;;「R=∞」は円弧がないことを表す
                         (list " - " 32232 38598 38283 22987 26178 12289 27425 12398 25805 20316 12434 12377 12427 12414 12391 12399 12477 12522 12483 12489 12364 38750 34920 31034 12395 12394 12426 12414 12377 )
                         ;;編集開始時、次の操作をするまではソリッドが非表示になります
                         ))
                   
                   )
             (setq 
              ls_guidemenu
              (list
               
               (list(list 67);;管の色
                    (cons "ITEM"(mix_strasc(list 31649 12398 33394)))
                    (cons "INPUTCOLOR"(lambda()
                                        (if int_colduct_temp T(setq int_colduct_temp int_colduct))
                                        'int_colduct_temp))
                    (cons "LOADFUNCTION"
                          (lambda()
                            (if ls_vnam_duct
                                (setq bool_replacegrread T int_grread 3 elem_grread(list 0 0 0)
                                      bool_noeditdepth T))
                            ))
                    ;;カラーダイアログが表示され、管のモデルに使う色を選択できます
                    (cons "HELP"(lambda()(mix_strasc(list 12459 12521 12540 12480 12452 12450 12525 12464 12364 34920 31034 12373 12428 12289 31649 12398 12514 12487 12523 12395 20351 12358 33394 12434 36984 25246 12391 12365 12414 12377 ))))
                    )
               (list(list 82) ;;R経路の曲げ半径
                    (cons "ITEM"(mix_strasc(list 32076 36335 12398 26354 12370 21322 24452)))
                    (cons "INPUT"(lambda()
                                   (if radius_bend_temp T(setq radius_bend_temp radius_bend))
                                   'radius_bend_temp))
                    ;;管路が曲線となる箇所における曲げ半径
                    (cons "HELP"(lambda()(mix_strasc(list 31649 36335 12364 26354 32218 12392 12394 12427 31623 25152 12395 12362 12369 12427 26354 12370 21322 24452(if intselectmenu str_guide_inputval str_guide_selectval)))))
                    )
               
               (list(list 84);;T保護コンクリートタイプ
                    (cons "ITEM"(mix_strasc(list 20445 35703 12467 12531 12463 12522 12540 12488 12479 12452 12503)))
                    (cons "INPUTSWITCH"
                          (lambda()
                            (if int_protectcon_temp T(setq int_protectcon_temp int_protectcon))
                            (list 'int_protectcon_temp
                                  (mapcar '(lambda(str c)(strcat "{\\C" c ";" str "}"))
                                          ls_type_protect
                                          (list str_gcol_g str_gcol_c str_gcol_p))
                                  )))
                    (cons "LOADFUNCTION"
                          (lambda()
                            (if ls_vnam_duct
                                (setq bool_replacegrread T int_grread 3 elem_grread(list 0 0 0)
                                      bool_noeditdepth T))
                            ))
                    ;;保護コンクリートの形状を「全周」、「下半のみ」、「なし」から選択します
                    ;;管の中心と同じ位置を中心とする長方形形状,「全周」の半分の高さの長方形\n※高さ入力値の考え方は全周と同じなので、入力値の半分の高さとなる
                    (cons "HELP"
                          (lambda()
                            
                            (setq ls_guide_drawing
                                  (list
                                   1
                                   
                                   (list(list -12.0 5.0 0.0)(list -2.0 5.0 0.0)3)
                                   (list(list -2.0 5.0 0.0)(list -2.0 -5.0 0.0)3)
                                   (list(list -2.0 -5.0 0.0)(list -12.0 -5.0 0.0)3)
                                   (list(list -12.0 -5.0 0.0)(list -12.0 5.0 0.0)3)
                                   (list(list -12.5 -5.0 0.0)(list -16.5 -5.0 0.0)30)
                                   (list(list -12.5 5.0 0.0)(list -16.5 5.0 0.0)30)
                                   (list(list -16.0 -4.0 0.0)(list -16.0 4.0 0.0)30)
                                   (list(list -15.7321 -4.0 0.0)(list -16.0 -5.0 0.0)30)
                                   (list(list -16.0 -5.0 0.0)(list -16.2679 -4.0 0.0)30)
                                   (list(list -16.0 -5.0 0.0)(list -16.0 -4.0 0.0)30)
                                   (list(list -16.2679 4.0 0.0)(list -16.0 5.0 0.0)30)
                                   (list(list -16.0 5.0 0.0)(list -15.7321 4.0 0.0)30)
                                   (list(list -16.0 5.0 0.0)(list -16.0 4.0 0.0)30)
                                   (list(list 17.0 0.0 0.0)(list 18.0 0.0 0.0)3)
                                   (list(list 18.0 0.0 0.0)(list 18.0 -5.0 0.0)3)
                                   (list(list 18.0 -5.0 0.0)(list 8.0 -5.0 0.0)3)
                                   (list(list 8.0 -5.0 0.0)(list 8.0 0.0 0.0)3)
                                   (list(list 8.0 0.0 0.0)(list 9.0 0.0 0.0)3)
                                   (list(list -3.0 0.0 0.0)(list -3.1363 1.03528 0.0)256)
                                   (list(list -3.1363 1.03528 0.0)(list -3.5359 2.0 0.0)256)
                                   (list(list -3.5359 2.0 0.0)(list -4.17157 2.82843 0.0)256)
                                   (list(list -4.17157 2.82843 0.0)(list -5.0 3.4641 0.0)256)
                                   (list(list -5.0 3.4641 0.0)(list -5.96472 3.8637 0.0)256)
                                   (list(list -5.96472 3.8637 0.0)(list -7.0 4.0 0.0)256)
                                   (list(list -7.0 4.0 0.0)(list -8.03528 3.8637 0.0)256)
                                   (list(list -8.03528 3.8637 0.0)(list -9.0 3.4641 0.0)256)
                                   (list(list -9.0 3.4641 0.0)(list -9.82843 2.82843 0.0)256)
                                   (list(list -9.82843 2.82843 0.0)(list -10.4641 2.0 0.0)256)
                                   (list(list -10.4641 2.0 0.0)(list -10.8637 1.03528 0.0)256)
                                   (list(list -10.8637 1.03528 0.0)(list -11.0 -1.2865e-15 0.0)256)
                                   (list(list -11.0 -1.2865e-15 0.0)(list -10.8637 -1.03528 0.0)256)
                                   (list(list -10.8637 -1.03528 0.0)(list -10.4641 -2.0 0.0)256)
                                   (list(list -10.4641 -2.0 0.0)(list -9.82843 -2.82843 0.0)256)
                                   (list(list -9.82843 -2.82843 0.0)(list -9.0 -3.4641 0.0)256)
                                   (list(list -9.0 -3.4641 0.0)(list -8.03528 -3.8637 0.0)256)
                                   (list(list -8.03528 -3.8637 0.0)(list -7.0 -4.0 0.0)256)
                                   (list(list -7.0 -4.0 0.0)(list -5.96472 -3.8637 0.0)256)
                                   (list(list -5.96472 -3.8637 0.0)(list -5.0 -3.4641 0.0)256)
                                   (list(list -5.0 -3.4641 0.0)(list -4.17157 -2.82843 0.0)256)
                                   (list(list -4.17157 -2.82843 0.0)(list -3.5359 -2.0 0.0)256)
                                   (list(list -3.5359 -2.0 0.0)(list -3.1363 -1.03528 0.0)256)
                                   (list(list -3.1363 -1.03528 0.0)(list -3.0 0.0 0.0)256)
                                   (list(list 17.0 0.0 0.0)(list 16.8637 1.03528 0.0)256)
                                   (list(list 16.8637 1.03528 0.0)(list 16.4641 2.0 0.0)256)
                                   (list(list 16.4641 2.0 0.0)(list 15.8284 2.82843 0.0)256)
                                   (list(list 15.8284 2.82843 0.0)(list 15.0 3.4641 0.0)256)
                                   (list(list 15.0 3.4641 0.0)(list 14.0353 3.8637 0.0)256)
                                   (list(list 14.0353 3.8637 0.0)(list 13.0 4.0 0.0)256)
                                   (list(list 13.0 4.0 0.0)(list 11.9647 3.8637 0.0)256)
                                   (list(list 11.9647 3.8637 0.0)(list 11.0 3.4641 0.0)256)
                                   (list(list 11.0 3.4641 0.0)(list 10.1716 2.82843 0.0)256)
                                   (list(list 10.1716 2.82843 0.0)(list 9.5359 2.0 0.0)256)
                                   (list(list 9.5359 2.0 0.0)(list 9.1363 1.03528 0.0)256)
                                   (list(list 9.1363 1.03528 0.0)(list 9.0 -1.2865e-15 0.0)256)
                                   (list(list 9.0 -1.2865e-15 0.0)(list 9.1363 -1.03528 0.0)256)
                                   (list(list 9.1363 -1.03528 0.0)(list 9.5359 -2.0 0.0)256)
                                   (list(list 9.5359 -2.0 0.0)(list 10.1716 -2.82843 0.0)256)
                                   (list(list 10.1716 -2.82843 0.0)(list 11.0 -3.4641 0.0)256)
                                   (list(list 11.0 -3.4641 0.0)(list 11.9647 -3.8637 0.0)256)
                                   (list(list 11.9647 -3.8637 0.0)(list 13.0 -4.0 0.0)256)
                                   (list(list 13.0 -4.0 0.0)(list 14.0353 -3.8637 0.0)256)
                                   (list(list 14.0353 -3.8637 0.0)(list 15.0 -3.4641 0.0)256)
                                   (list(list 15.0 -3.4641 0.0)(list 15.8284 -2.82843 0.0)256)
                                   (list(list 15.8284 -2.82843 0.0)(list 16.4641 -2.0 0.0)256)
                                   (list(list 16.4641 -2.0 0.0)(list 16.8637 -1.03528 0.0)256)
                                   (list(list 16.8637 -1.03528 0.0)(list 17.0 0.0 0.0)256)
                                   (list(list 7.5 -5.0 0.0)(list 3.5 -5.0 0.0)30)
                                   (list(list 7.5 5.0 0.0)(list 3.5 5.0 0.0)30)
                                   (list(list 4.0 -4.0 0.0)(list 4.0 4.0 0.0)30)
                                   (list(list 4.26795 -4.0 0.0)(list 4.0 -5.0 0.0)30)
                                   (list(list 4.0 -5.0 0.0)(list 3.73205 -4.0 0.0)30)
                                   (list(list 4.0 -5.0 0.0)(list 4.0 -4.0 0.0)30)
                                   (list(list 3.73205 4.0 0.0)(list 4.0 5.0 0.0)30)
                                   (list(list 4.0 5.0 0.0)(list 4.26795 4.0 0.0)30)
                                   (list(list 4.0 5.0 0.0)(list 4.0 4.0 0.0)30)
                                   (list(list -17.058 -0.499814 0.0)(list -17.058 0.501034 0.0)30)
                                   (list(list -16.3944 0.501034 0.0)(list -16.3944 -0.499814 0.0)30)
                                   (list(list -17.058 0.0244395 0.0)(list -16.3944 0.0244395 0.0)30)
                                   (list(list 2.92967 -0.499814 0.0)(list 2.92967 0.501034 0.0)30)
                                   (list(list 3.59691 0.501034 0.0)(list 3.59691 -0.499814 0.0)30)
                                   (list(list 2.92967 0.0244395 0.0)(list 3.59691 0.0244395 0.0)30)
                                   (list(list -8.41327 -7.66339 0.0)(list -8.41327 -8.99785 0.0)4)
                                   (list(list -9.41411 -7.66339 0.0)(list -7.41242 -7.66339 0.0)4)
                                   (list(list -9.24547 -8.28663 0.0)(list -7.58106 -8.28663 0.0)4)
                                   (list(list -9.57909 -8.99785 0.0)(list -7.24744 -8.99785 0.0)4)
                                   (list(list -7.91468 -6.96316 0.0)(list -7.24744 -7.49842 0.0)4)
                                   (list(list -8.41327 -6.49757 0.0)(list -7.91468 -6.96316 0.0)4)
                                   (list(list -8.91186 -6.96316 0.0)(list -8.41327 -6.49757 0.0)4)
                                   (list(list -9.57909 -7.49842 0.0)(list -8.91186 -6.96316 0.0)4)
                                   (list(list -5.91665 -8.66424 0.0)(list -5.91665 -8.16565 0.0)4)
                                   (list(list -4.74716 -8.66424 0.0)(list -5.91665 -8.66424 0.0)4)
                                   (list(list -4.74716 -8.16565 0.0)(list -4.74716 -8.66424 0.0)4)
                                   (list(list -5.91665 -8.16565 0.0)(list -4.74716 -8.16565 0.0)4)
                                   (list(list -5.9753 -7.1648 0.0)(list -4.6885 -7.1648 0.0)4)
                                   (list(list -6.08162 -7.66339 0.0)(list -4.58218 -7.66339 0.0)4)
                                   (list(list -5.33373 -6.83118 0.0)(list -5.33373 -7.66339 0.0)4)
                                   (list(list -4.41721 -8.99785 0.0)(list -4.41721 -6.66621 0.0)4)
                                   (list(list -4.74716 -8.99785 0.0)(list -4.41721 -8.99785 0.0)4)
                                   (list(list -6.48489 -8.84388 0.0)(list -6.58021 -8.99785 0.0)4)
                                   (list(list -6.34191 -8.53226 0.0)(list -6.48489 -8.84388 0.0)4)
                                   (list(list -6.2466 -7.90902 0.0)(list -6.34191 -8.53226 0.0)4)
                                   (list(list -6.2466 -6.66621 0.0)(list -6.2466 -7.90902 0.0)4)
                                   (list(list -6.2466 -6.66621 0.0)(list -4.41721 -6.66621 0.0)4)
                                   (list(list 11.9923 -7.49842 0.0)(list 12.4909 -7.99701 0.0)6)
                                   (list(list 11.3251 -7.1648 0.0)(list 11.9923 -7.49842 0.0)6)
                                   (list(list 11.3251 -6.49757 0.0)(list 11.3251 -8.99785 0.0)6)
                                   (list(list 10.3279 -6.49757 0.0)(list 12.6595 -6.49757 0.0)6)
                                   (list(list 13.6567 -6.66621 0.0)(list 13.9903 -6.99982 0.0)6)
                                   (list(list 13.9903 -6.99982 0.0)(list 14.159 -7.32977 0.0)6)
                                   (list(list 15.1561 -6.99982 0.0)(list 14.8225 -7.32977 0.0)6)
                                   (list(list 15.3248 -6.66621 0.0)(list 15.1561 -6.99982 0.0)6)
                                   (list(list 14.4926 -6.49757 0.0)(list 14.4926 -8.99785 0.0)6)
                                   (list(list 15.6584 -8.16565 0.0)(list 13.3268 -8.16565 0.0)6)
                                   (list(list 13.4917 -7.49842 0.0)(list 15.4898 -7.49842 0.0)6)
                                   
                                   )
                                  )
                            
                            (mix_strasc
                             (list 20445 35703 12467 12531 12463 12522 12540 12488 12398 24418 29366 12434
                                   "\n- " 20840 21608 ":" 31649 12398 20013 24515 12392 21516 12376 20301 32622 12434 20013 24515 12392 12377 12427 38263 26041 24418 24418 29366
                                   "\n- " 19979 21322 12398 12415 ":" "," 12300 20840 21608 12301 12398 21322 20998 12398 39640 12373 12398 38263 26041 24418 "\n  " 8251 39640 12373 20837 21147 20516 12398 32771 12360 26041 12399 20840 21608 12392 21516 12376 12394 12398 12391 12289 20837 21147 20516 12398 21322 20998 12398 39640 12373 12392 12394 12427
                                   "\n- " 12394 12375  "\n" 12363 12425 36984 25246 12375 12414 12377
                                   str_startguidedrawing
                                   
                                   ))))
                    )
               
               (list(list 68) ;;D.管直径
                    (cons "ITEM"(mix_strasc(list 31649 30452 24452)))
                    (cons "INPUT"(lambda()
                                   (if diam_duct_temp T(setq diam_duct_temp diam_duct))
                                   'diam_duct_temp))
                    (cons "LOADFUNCTION"
                          (lambda()
                            (if ls_vnam_duct
                                (setq bool_replacegrread T int_grread 3 elem_grread(list 0 0 0)
                                      bool_noeditdepth T))
                            ))
                    ;;管の直径を入力
                    (cons "HELP"(lambda( / p)
                                  (mix_strasc(list 31649 12398 30452 24452 12434 20837 21147 (if int_selectmenu str_guide_inputval str_guide_selectval) ))))
                    )
               (list(list 87) ;;w.保護コンクリート幅
                    (cons "ITEM"(mix_strasc(list 20445 35703 12467 12531 12463 12522 12540 12488 24133)))
                    (cons "INPUT"(lambda()
                                   (if width_protect_temp T(setq width_protect_temp width_protect))
                                   'width_protect_temp))
                    (cons "LOADFUNCTION"
                          (lambda()
                            (if ls_vnam_duct
                                (setq bool_replacegrread T int_grread 3 elem_grread(list 0 0 0)
                                      bool_noeditdepth T))
                            ))
                    ;;保護コンクリートの幅（水平方向）を入力
                    (cons "HELP"
                          (lambda()
                            (setq ls_guide_drawing
                                  (list
                                   1
                                   
                                   (list(list -5.5 3.0 0.0)(list -8.0 3.0 0.0)4)
                                   (list(list -7.5 6.25 0.0)(list -7.5 1.75 0.0)4)
                                   (list(list -3.0 5.5 0.0)(list -3.0 8.0 0.0)4)
                                   (list(list -6.25 7.5 0.0)(list -1.75 7.5 0.0)4)
                                   (list(list -3.5 5.0 0.0)(list -10.625 5.0 0.0)2)
                                   (list(list -3.5 -5.0 0.0)(list -10.625 -5.0 0.0)2)
                                   (list(list -10.0 5.0 0.0)(list -10.0 -5.0 0.0)2)
                                   (list(list -5.0 3.5 0.0)(list -5.0 10.625 0.0)6)
                                   (list(list 5.0 3.5 0.0)(list 5.0 10.625 0.0)6)
                                   (list(list -5.0 10.0 0.0)(list 5.0 10.0 0.0)6)
                                   (list(list -5.0 3.0 0.0)(list -3.0 5.0 0.0)3)
                                   (list(list -3.0 5.0 0.0)(list 3.0 5.0 0.0)3)
                                   (list(list 3.0 5.0 0.0)(list 5.0 3.0 0.0)3)
                                   (list(list 5.0 3.0 0.0)(list 5.0 -3.0 0.0)3)
                                   (list(list 5.0 -3.0 0.0)(list 3.0 -5.0 0.0)3)
                                   (list(list 3.0 -5.0 0.0)(list -3.0 -5.0 0.0)3)
                                   (list(list -3.0 -5.0 0.0)(list -5.0 -3.0 0.0)3)
                                   (list(list -5.0 -3.0 0.0)(list -5.0 3.0 0.0)3)
                                   (list(list 3.75 10.3349 0.0)(list 5.0 10.0 0.0)6)
                                   (list(list 5.0 10.0 0.0)(list 3.75 9.66512 0.0)6)
                                   (list(list -3.75 9.66512 0.0)(list -5.0 10.0 0.0)6)
                                   (list(list -5.0 10.0 0.0)(list -3.75 10.3349 0.0)6)
                                   (list(list -9.66512 -3.75 0.0)(list -10.0 -5.0 0.0)2)
                                   (list(list -10.0 -5.0 0.0)(list -10.3349 -3.75 0.0)2)
                                   (list(list -10.3349 3.75 0.0)(list -10.0 5.0 0.0)2)
                                   (list(list -10.0 5.0 0.0)(list -9.66512 3.75 0.0)2)
                                   (list(list -1.75 7.16512 0.0)(list -3.0 7.5 0.0)4)
                                   (list(list -3.0 7.5 0.0)(list -1.75 7.83488 0.0)4)
                                   (list(list -6.25 7.83488 0.0)(list -5.0 7.5 0.0)4)
                                   (list(list -5.0 7.5 0.0)(list -6.25 7.16512 0.0)4)
                                   (list(list -7.83488 1.75 0.0)(list -7.5 3.0 0.0)4)
                                   (list(list -7.5 3.0 0.0)(list -7.16513 1.75 0.0)4)
                                   (list(list -7.16513 6.25 0.0)(list -7.5 5.0 0.0)4)
                                   (list(list -7.5 5.0 0.0)(list -7.83488 6.25 0.0)4)
                                   (list(list -0.625 11.5625 0.0)(list -0.208333 10.3125 0.0)6)
                                   (list(list 0.208333 10.3125 0.0)(list 0.625 11.5625 0.0)6)
                                   (list(list -0.208333 10.3125 0.0)(list 0.0 10.9583 0.0)6)
                                   (list(list 0.0 10.9583 0.0)(list 0.208333 10.3125 0.0)6)
                                   (list(list -4.41667 7.8125 0.0)(list -4.41667 9.0625 0.0)4)
                                   (list(list -4.41667 9.0625 0.0)(list -3.58333 9.0625 0.0)4)
                                   (list(list -4.41667 8.4375 0.0)(list -3.97917 8.4375 0.0)4)
                                   (list(list -8.64583 3.375 0.0)(list -8.64583 4.625 0.0)4)
                                   (list(list -8.64583 4.625 0.0)(list -7.8125 4.625 0.0)4)
                                   (list(list -8.64583 4.0 0.0)(list -8.20833 4.0 0.0)4)
                                   (list(list -11.1458 -0.625 0.0)(list -11.1458 0.625 0.0)2)
                                   (list(list -10.3125 0.625 0.0)(list -10.3125 -0.625 0.0)2)
                                   (list(list -11.1458 0.0 0.0)(list -10.3125 0.0 0.0)2)
                                   ))
                            
                            (mix_strasc(list 20445 35703 12467 12531 12463 12522 12540 12488 12398 24133 65288 27700 24179 26041 21521 65289 12434 20837 21147(if int_selectmenu str_guide_inputval str_guide_selectval)
                                             str_startguidedrawing))))
                    )
               (list(list 72) ;;H.保護コンクリート高さ
                    (cons "ITEM"(mix_strasc(list 20445 35703 12467 12531 12463 12522 12540 12488 39640 12373)))
                    (cons "INPUT"(lambda()
                                   (if height_protect_temp T(setq height_protect_temp height_protect))
                                   'height_protect_temp))
                    (cons "LOADFUNCTION"
                          (lambda()
                            (if ls_vnam_duct
                                (setq bool_replacegrread T int_grread 3 elem_grread(list 0 0 0)
                                      bool_noeditdepth T))
                            ))
                    ;;保護コンクリートの高さ（鉛直方向）を入力
                    (cons "HELP"
                          (lambda()
                            (setq ls_guide_drawing
                                  (list
                                   1
                                   
                                   (list(list -5.5 3.0 0.0)(list -8.0 3.0 0.0)4)
                                   (list(list -7.5 6.25 0.0)(list -7.5 1.75 0.0)4)
                                   (list(list -3.0 5.5 0.0)(list -3.0 8.0 0.0)4)
                                   (list(list -6.25 7.5 0.0)(list -1.75 7.5 0.0)4)
                                   (list(list -3.5 5.0 0.0)(list -10.625 5.0 0.0)2)
                                   (list(list -3.5 -5.0 0.0)(list -10.625 -5.0 0.0)2)
                                   (list(list -10.0 5.0 0.0)(list -10.0 -5.0 0.0)2)
                                   (list(list -5.0 3.5 0.0)(list -5.0 10.625 0.0)6)
                                   (list(list 5.0 3.5 0.0)(list 5.0 10.625 0.0)6)
                                   (list(list -5.0 10.0 0.0)(list 5.0 10.0 0.0)6)
                                   (list(list -5.0 3.0 0.0)(list -3.0 5.0 0.0)3)
                                   (list(list -3.0 5.0 0.0)(list 3.0 5.0 0.0)3)
                                   (list(list 3.0 5.0 0.0)(list 5.0 3.0 0.0)3)
                                   (list(list 5.0 3.0 0.0)(list 5.0 -3.0 0.0)3)
                                   (list(list 5.0 -3.0 0.0)(list 3.0 -5.0 0.0)3)
                                   (list(list 3.0 -5.0 0.0)(list -3.0 -5.0 0.0)3)
                                   (list(list -3.0 -5.0 0.0)(list -5.0 -3.0 0.0)3)
                                   (list(list -5.0 -3.0 0.0)(list -5.0 3.0 0.0)3)
                                   (list(list 3.75 10.3349 0.0)(list 5.0 10.0 0.0)6)
                                   (list(list 5.0 10.0 0.0)(list 3.75 9.66512 0.0)6)
                                   (list(list -3.75 9.66512 0.0)(list -5.0 10.0 0.0)6)
                                   (list(list -5.0 10.0 0.0)(list -3.75 10.3349 0.0)6)
                                   (list(list -9.66512 -3.75 0.0)(list -10.0 -5.0 0.0)2)
                                   (list(list -10.0 -5.0 0.0)(list -10.3349 -3.75 0.0)2)
                                   (list(list -10.3349 3.75 0.0)(list -10.0 5.0 0.0)2)
                                   (list(list -10.0 5.0 0.0)(list -9.66512 3.75 0.0)2)
                                   (list(list -1.75 7.16512 0.0)(list -3.0 7.5 0.0)4)
                                   (list(list -3.0 7.5 0.0)(list -1.75 7.83488 0.0)4)
                                   (list(list -6.25 7.83488 0.0)(list -5.0 7.5 0.0)4)
                                   (list(list -5.0 7.5 0.0)(list -6.25 7.16512 0.0)4)
                                   (list(list -7.83488 1.75 0.0)(list -7.5 3.0 0.0)4)
                                   (list(list -7.5 3.0 0.0)(list -7.16513 1.75 0.0)4)
                                   (list(list -7.16513 6.25 0.0)(list -7.5 5.0 0.0)4)
                                   (list(list -7.5 5.0 0.0)(list -7.83488 6.25 0.0)4)
                                   (list(list -0.625 11.5625 0.0)(list -0.208333 10.3125 0.0)6)
                                   (list(list 0.208333 10.3125 0.0)(list 0.625 11.5625 0.0)6)
                                   (list(list -0.208333 10.3125 0.0)(list 0.0 10.9583 0.0)6)
                                   (list(list 0.0 10.9583 0.0)(list 0.208333 10.3125 0.0)6)
                                   (list(list -4.41667 7.8125 0.0)(list -4.41667 9.0625 0.0)4)
                                   (list(list -4.41667 9.0625 0.0)(list -3.58333 9.0625 0.0)4)
                                   (list(list -4.41667 8.4375 0.0)(list -3.97917 8.4375 0.0)4)
                                   (list(list -8.64583 3.375 0.0)(list -8.64583 4.625 0.0)4)
                                   (list(list -8.64583 4.625 0.0)(list -7.8125 4.625 0.0)4)
                                   (list(list -8.64583 4.0 0.0)(list -8.20833 4.0 0.0)4)
                                   (list(list -11.1458 -0.625 0.0)(list -11.1458 0.625 0.0)2)
                                   (list(list -10.3125 0.625 0.0)(list -10.3125 -0.625 0.0)2)
                                   (list(list -11.1458 0.0 0.0)(list -10.3125 0.0 0.0)2)
                                   ))
                            
                            (mix_strasc(list 20445 35703 12467 12531 12463 12522 12540 12488 12398 39640 12373 65288 37467 30452 26041 21521 65289 12434 20837 21147
                                             (if int_selectmenu str_guide_inputval str_guide_selectval)
                                             str_startguidedrawing
                                             
                                             ))))
                    )
               
               (list(list 70) ;;F.保護コンクリート面取り
                    (cons "ITEM"(mix_strasc(list 20445 35703 12467 12531 12463 12522 12540 12488 38754 21462 12426)))
                    (cons "INPUT"(lambda()
                                   (if filet_protect_temp T(setq filet_protect_temp filet_protect))
                                   'filet_protect_temp))
                    (cons "LOADFUNCTION"
                          (lambda()
                            (if ls_vnam_duct
                                (setq bool_replacegrread T int_grread 3 elem_grread(list 0 0 0)
                                      bool_noeditdepth T))
                            ))
                    ;;保護コンクリートの面取り（直角二等辺三角形で切り取られるので等辺の長さ）を入力\n0のとき面取りを行いません
                    (cons "HELP"
                          (lambda()
                            (setq ls_guide_drawing
                                  (list
                                   1
                                   
                                   (list(list -5.5 3.0 0.0)(list -8.0 3.0 0.0)4)
                                   (list(list -7.5 6.25 0.0)(list -7.5 1.75 0.0)4)
                                   (list(list -3.0 5.5 0.0)(list -3.0 8.0 0.0)4)
                                   (list(list -6.25 7.5 0.0)(list -1.75 7.5 0.0)4)
                                   (list(list -3.5 5.0 0.0)(list -10.625 5.0 0.0)2)
                                   (list(list -3.5 -5.0 0.0)(list -10.625 -5.0 0.0)2)
                                   (list(list -10.0 5.0 0.0)(list -10.0 -5.0 0.0)2)
                                   (list(list -5.0 3.5 0.0)(list -5.0 10.625 0.0)6)
                                   (list(list 5.0 3.5 0.0)(list 5.0 10.625 0.0)6)
                                   (list(list -5.0 10.0 0.0)(list 5.0 10.0 0.0)6)
                                   (list(list -5.0 3.0 0.0)(list -3.0 5.0 0.0)3)
                                   (list(list -3.0 5.0 0.0)(list 3.0 5.0 0.0)3)
                                   (list(list 3.0 5.0 0.0)(list 5.0 3.0 0.0)3)
                                   (list(list 5.0 3.0 0.0)(list 5.0 -3.0 0.0)3)
                                   (list(list 5.0 -3.0 0.0)(list 3.0 -5.0 0.0)3)
                                   (list(list 3.0 -5.0 0.0)(list -3.0 -5.0 0.0)3)
                                   (list(list -3.0 -5.0 0.0)(list -5.0 -3.0 0.0)3)
                                   (list(list -5.0 -3.0 0.0)(list -5.0 3.0 0.0)3)
                                   (list(list 3.75 10.3349 0.0)(list 5.0 10.0 0.0)6)
                                   (list(list 5.0 10.0 0.0)(list 3.75 9.66512 0.0)6)
                                   (list(list -3.75 9.66512 0.0)(list -5.0 10.0 0.0)6)
                                   (list(list -5.0 10.0 0.0)(list -3.75 10.3349 0.0)6)
                                   (list(list -9.66512 -3.75 0.0)(list -10.0 -5.0 0.0)2)
                                   (list(list -10.0 -5.0 0.0)(list -10.3349 -3.75 0.0)2)
                                   (list(list -10.3349 3.75 0.0)(list -10.0 5.0 0.0)2)
                                   (list(list -10.0 5.0 0.0)(list -9.66512 3.75 0.0)2)
                                   (list(list -1.75 7.16512 0.0)(list -3.0 7.5 0.0)4)
                                   (list(list -3.0 7.5 0.0)(list -1.75 7.83488 0.0)4)
                                   (list(list -6.25 7.83488 0.0)(list -5.0 7.5 0.0)4)
                                   (list(list -5.0 7.5 0.0)(list -6.25 7.16512 0.0)4)
                                   (list(list -7.83488 1.75 0.0)(list -7.5 3.0 0.0)4)
                                   (list(list -7.5 3.0 0.0)(list -7.16513 1.75 0.0)4)
                                   (list(list -7.16513 6.25 0.0)(list -7.5 5.0 0.0)4)
                                   (list(list -7.5 5.0 0.0)(list -7.83488 6.25 0.0)4)
                                   (list(list -0.625 11.5625 0.0)(list -0.208333 10.3125 0.0)6)
                                   (list(list 0.208333 10.3125 0.0)(list 0.625 11.5625 0.0)6)
                                   (list(list -0.208333 10.3125 0.0)(list 0.0 10.9583 0.0)6)
                                   (list(list 0.0 10.9583 0.0)(list 0.208333 10.3125 0.0)6)
                                   (list(list -4.41667 7.8125 0.0)(list -4.41667 9.0625 0.0)4)
                                   (list(list -4.41667 9.0625 0.0)(list -3.58333 9.0625 0.0)4)
                                   (list(list -4.41667 8.4375 0.0)(list -3.97917 8.4375 0.0)4)
                                   (list(list -8.64583 3.375 0.0)(list -8.64583 4.625 0.0)4)
                                   (list(list -8.64583 4.625 0.0)(list -7.8125 4.625 0.0)4)
                                   (list(list -8.64583 4.0 0.0)(list -8.20833 4.0 0.0)4)
                                   (list(list -11.1458 -0.625 0.0)(list -11.1458 0.625 0.0)2)
                                   (list(list -10.3125 0.625 0.0)(list -10.3125 -0.625 0.0)2)
                                   (list(list -11.1458 0.0 0.0)(list -10.3125 0.0 0.0)2)
                                   ))
                            
                            (mix_strasc(list  20445 35703 12467 12531 12463 12522 12540 12488 12398 38754 21462 12426 65288 30452 35282 20108 31561 36794 19977 35282 24418 12391 20999 12426 21462 12425 12428 12427 12398 12391 31561 36794 12398 38263 12373 65289 12434 20837 21147 "\n0" 12398 12392 12365 38754 21462 12426 12434 34892 12356 12414 12379 12435
                                              (if int_selectmenu str_guide_inputval str_guide_selectval)
                                              str_startguidedrawing
                                              ))))
                    )
               
               (list(list 90);;Zキャンセル項目を選択
                    (cons "ITEM"(mix_strasc(list 12461 12515 12531 12475 12523 38917 30446 12434 36984 25246 )))
                    (cons "INPUTSWITCH"(lambda()(list 'int_cancelduct ls_switch_cancelduct)))
                    (cons "NOREFERENCE"
                          (setq ls_switch_cancelduct
                                (mapcar 'mix_strasc;;直前の高度入力,経路の選択,管路の編集
                                        (list(list "{\\C" str_gcol_g ";"
                                                   30452 21069 12398 28145 24230 20837 21147 "}")
                                             (list "{\\C" str_gcol_c ";"
                                                   32076 36335 12398 36984 25246 "}")
                                             (list "{\\C" str_gcol_p ";"
                                                   31649 36335 12398 32232 38598 "}")
                                             )
                                        )
                                ))
                    ;;BackSpaceを押したときに実行する内容を選びます\n直前の高度入力：管中心位置の入力のうち、最後に作成したものを削除します\n経路の選択：現在選択されている経路の選択を解除します（他の経路を選択したいときなどに使用）\n管路の編集：管路編集をキャンセルし、編集前の状態に戻します
                    (cons "HELP"(lambda()(mix_strasc(list "BackSpace" 12434 25276 12375 12383 12392 12365 12395 23455 34892 12377 12427 20869 23481 12434 36984 12403 12414 12377 "\n" 30452 21069 12398 39640 24230 20837 21147 65306 31649 20013 24515 20301 32622 12398 20837 21147 12398 12358 12385 12289 26368 24460 12395 20316 25104 12375 12383 12418 12398 12434 21066 38500 12375 12414 12377 "\n" 32076 36335 12398 36984 25246 65306 29694 22312 36984 25246 12373 12428 12390 12356 12427 32076 36335 12398 36984 25246 12434 35299 38500 12375 12414 12377 65288 20182 12398 32076 36335 12434 36984 25246 12375 12383 12356 12392 12365 12394 12393 12395 20351 29992 65289 "\n" 31649 36335 12398 32232 38598 65306 31649 36335 32232 38598 12434 12461 12515 12531 12475 12523 12375 12289 32232 38598 21069 12398 29366 24907 12395 25147 12375 12414 12377 ))))
                    )
               
               (list(list 8);;キャンセルの実行
                    (cons "ITEM"(mix_strasc(list 12461 12515 12531 12475 12523 12398 23455 34892)))
                    (cons "LOADFUNCTION"
                          (lambda( / bool x)
                            
                            (cond
                             ((= int_cancelduct 0)
                              (setq bool T)
                              (while(and ls_vnam_duct bool)
                                (setq x(car ls_vnam_duct)
                                      ls_vnam_duct(cdr ls_vnam_duct))
                                (if(assoc "DEPTH" x)(setq bool nil))
                                (mapcar
                                 '(lambda(vnam)
                                    (if(=(type vnam)'VLA-OBJECT)
                                        (progn
                                          (vla-delete vnam)
                                          (exckillobj vnam)
                                          ))
                                    )
                                 (cdr(assoc "OBJ" x)))
                                )
                              ;;ls_vnam_ductの整理todo
                              (setq ii(1- int_min_duct))
                              (while(null(assoc(list "LINE"(setq ii(1+ ii)))ls_vnam_duct)))
                              (setq int_min_duct ii ii(1- ii)num_subtract 0)
                              (while(<=(setq ii(1+ ii))int_max_duct)
                                (if(assoc(list "LINE" ii)ls_vnam_duct)
                                    (if(= num_subtract 0)T
                                      (mapcar
                                       '(lambda(lst / str i j ls_subst ls_prev)
                                          (setq str(car lst)i(cadr lst)j(+ ii num_subtract)
                                                ls_prev(if i(list str ii i)(list str ii))
                                                ls_subst(if i(list str j i)(list str j)))
                                          (if(setq lst(assoc ls_prev ls_vnam_duct))
                                              (setq ls_vnam_duct
                                                    (subst(subst ls_subst lsprev lst)
                                                          lst ls_vnam_duct)
                                                    )
                                            )
                                          )
                                       (list(list "LINE")(list "DEPTH" 0)(list "DEPTH" 1)
                                            (list "ARC" 0)(list "ARC" 1)))
                                      )
                                  (setq num_subtract(1- num_subtract)))
                                )
                              (setq int_max_duct(+ int_max_duct num_subtract))
                              
                              (setq bool_replacegrread T
                                    int_grread 3 elem_grread(list 0 0 0)
                                    bool_noeditdepth T)

                              (setq int_selectmenu_ductedit
                                    (if(= int_editdepth 0)int_selectmenu_ductedit_depth
                                      (if(= int_editdepth 1)int_selectmenu_ductedit_offset)))
                              
                              )
                             
                             ((= int_cancelduct 1)
                              (if vnam_road
                                  (progn
                                    (setq str(mix_strasc
                                              (list 32076 36335 12398 36984 25246 12434 35299 38500 12375 12414 12375 12383));;経路の選択を解除しました
                                          )
                                    (if(vl-position vnam_road ls_vnam_killobj)
                                        (progn
                                          (vla-delete vnam_road)
                                          (exckillobj vnam)
                                          ))
                                    (setq vnam_road nil
                                          int_ductdepth nil)
                                    
                                    )
                                (setq str(mix_strasc(list 32076 36335 12364 12394 12356 12398 12391 35299 38500 12375 12414 12379 12435  )));;経路がないので解除しません
                                )
                              (princ str)
                              )
                             
                             ((= int_cancelduct 2)

                              (if bool_ductedit
                                  (progn
                                    (setq str(mix_strasc(list 31649 36335 32232 38598 12434 32066 20102 12375 12414 12375 12383) );;管路編集を終了しました
                                          bool_ductedit nil
                                          )
                                    
                                    (mapcar
                                     '(lambda(lst)
                                        (if(setq lst(assoc "OBJ" lst))
                                            (mapcar
                                             '(lambda(vnam)
                                                (if(=(type vnam)'VLA-OBJECT)
                                                    (progn
                                                      
                                                      (vla-delete vnam)
                                                      (exckillobj vnam)
                                                      ))
                                                )
                                             (cdr lst)))
                                        )
                                     ls_vnam_duct)
                                    (setq ls_vnam_duct nil)
                                    (if vnam_currentinsert
                                        (progn
                                          (vla-put-visible vnam_currentinsert :vlax-true)
                                          (setq ls_vnam_visible
                                                (vl-remove vnam_currentinsert ls_vnam_visible)
                                                vnam_currentinsert nil)
                                          ))
                                    
                                    (x-alert(list str))
                                    
                                    )
                                
                                )
                              )
                             )
                            
                            )
                          )
                    (cons "HELP"(lambda()"no guide"))
                    
                    )
               
               (list(list 76);; 地表面標高
                    (cons "ITEM"(mix_strasc(list 22320 34920 38754 27161 39640)))
                    (cons "STATUS"
                          (lambda()
                            (if str_lasground
                                (mix_strasc
                                 (list "{\\C" str_gcol_c ";" 12487 12540 12479 "} : "
                                       (vl-string-subst "" "lasgrid-" str_lasground)))
                              (if height_ground
                                  (mix_strasc
                                   (list"{\\C" str_gcol_g ";" 27161 39640 "} : "
                                        (as-numstr height_ground)))
                                (mix_strasc
                                 (list "{\\C" str_gcol_r ";"
                                       36984 25246 12373 12428 12390 12356 12414 12379 12435 "}"))
                                ))
                            ))
                    (cons "LOADFUNCTION"(lambda()(settile_selectground)))
                    ;;使用する標高を\n-las読込\n-xml読込\n-一定標高の数値入力\nから選択できます
                    (cons "HELP"(mix_strasc(list 20351 29992 12377 12427 27161 39640 12434 "\n- las" 35501 36796 "\n- xml" 35501 36796 "\n- " 19968 23450 27161 39640 12398 25968 20516 20837 21147 "\n" 12363 12425 36984 25246 12391 12365 12414 12377 )))
                    
                    )

               (list(list 49);;高度入力方法
                    (cons "ITEM"(mix_strasc(list 28145 24230 20837 21147 26041 27861 )))
                    (cons "INPUTSWITCH"
                          (lambda()
                            (if int_inputdepth_temp T(setq int_inputdepth_temp int_inputdepth))
                            (if(and(= int_ductmode 2)(= int_inputdepth_temp 2))(setq int_inputdepth_temp 1))
                            (list 'int_inputdepth_temp
                                  (mapcar '(lambda(str c)(strcat "{\\C" c ";" str "}"))
                                          ls_type_inputdepth
                                          (if(= int_ductmode 2)
                                              (list str_gcol_y str_gcol_c)
                                            (list str_gcol_y str_gcol_c str_gcol_g)))
                                  )))
                    ;;現在の標高データからの距離または標高値自体を入力するかを選択します\n標高データからの距離を入力するとき、上を正とするか下を正とするかを選択できます
                    (cons "HELP"(lambda()(mix_strasc(list 29694 22312 12398 27161 39640 12487 12540 12479 12363 12425 12398 36317 38626 12414 12383 12399 27161 39640 20516 33258 20307 12434 20837 21147 12377 12427 12363 12434 36984 25246 12375 12414 12377 "\n" 27161 39640 12487 12540 12479 12363 12425 12398 36317 38626 12434 20837 21147 12377 12427 12392 12365 12289 19978 12434 27491 12392 12377 12427 12363 19979 12434 27491 12392 12377 12427 12363 12434 36984 25246 12391 12365 12414 12377 ))))
                    )
               
               (list(list 50);;
                    (cons "ITEM"(mix_strasc(list 28145 24230) ))
                    (cons "INPUT"(lambda()
                                   (if depth_duct T(setq depth_duct 0.))
                                   'depth_duct))
                    (cons "KEYSEARCH" "DEPTH")
                    (cons "LOADFUNCTION"
                          (lambda()

                            ;; (if(and(= int_ductdepth 1)entna_depth)
                            ;;     (setq p_depth nil
                                      
                                      
                            ;;           entna_depth
                            ;;           (make_2pdimension
                            ;;            entna_depth(list p_road p_depth vec_normal dist_normal
                            ;;                             nil(* 0.5 pi) 0. str_level str_dimstyle_ductlevel))
                                      
                            ;;           )
                            ;;   )
                            nil
                            ))
                    ;;深度標高を入力\n入力方法の違いについては1行上で確認してください
                    (cons "HELP"(lambda()(mix_strasc(list 28145 24230 27161 39640 12434 20837 21147  "\n" 20837 21147 26041 27861 12398 36949 12356 12395 12388 12356 12390 12399 "1" 34892 19978 12391 30906 35469 12375 12390 12367 12384 12373 12356 (if int_selectmenu str_guide_inputval str_guide_selectval)
))))
                    )

               (list(list 51);;経路に対する水平オフセット
                    (cons "ITEM"(mix_strasc
                                 (list 32076 36335 12395 23550 12377 12427 27700 24179 12458 12501 12475 12483 12488
                                       "(" 21521 12363 12387 12390 21491 12364 27491 ")"
                                       ;;向かって右が正
                                       )))
                    (cons "INPUT"(lambda()(if offset_duct T(setq offset_duct 0.))'offset_duct))
                    (cons "KEYSEARCH" "OFFSET")
                    ;;水平オフセットを入力
                    (cons "HELP"(lambda()(mix_strasc(list 27700 24179 12458 12501 12475 12483 12488 12434 20837 21147(if int_selectmenu str_guide_inputval str_guide_selectval) ))))
                    
                    )

               (list(list 52);;次に作成する箇所の追加方向
                    (cons "ITEM"(mix_strasc(list 27425 12395 20316 25104 12377 12427 31623 25152 12398 36861 21152 26041 21521 )))
                    (cons "INPUTSWITCH"
                          (lambda()
                            (if int_connecttype T(setq int_connecttype 0))
                            (list 'int_connecttype ls_switch_connecttype)))
                    (cons "NOREFERENCE"
                          (setq ls_switch_connecttype
                                (mapcar 'mix_strasc
                                        (list(list "{\\C" str_gcol_c ";"
                                                   32066 28857 20596 12395 36861 21152 "}")
                                             (list "{\\C" str_gcol_p ";"
                                                   36215 28857 20596 12395 36861 21152 "}")))
                                ))
                    ;;直線部を作成するとき、編集中の管路の後ろ側に続くように配置するか手前側に続くように配置するかを選択します
                    (cons "HELP"(lambda()(mix_strasc(list 30452 32218 37096 12434 20316 25104 12377 12427 12392 12365 12289 32232 38598 20013 12398 31649 36335 12398 24460 12429 20596 12395 32154 12367 12424 12358 12395 37197 32622 12377 12427 12363 25163 21069 20596 12395 32154 12367 12424 12358 12395 37197 32622 12377 12427 12363 12434 36984 25246 12375 12414 12377 ))))
                    
                    )
               
               (list(list 53);;編集中の管と保護コンの表示
                    (cons "ITEM"(mix_strasc(list 32232 38598 20013 12398 31649 12392 20445 35703 12467 12531 12398 34920 31034)))
                    (cons "INPUTSWITCH"(lambda() (list 'int_hidesolid ls_switch_hidesolid) ))
                    (cons "NOREFERENCE"
                          (setq ls_switch_hidesolid
                                (mapcar 'mix_strasc;;表示非表示
                                        (list(list "{\\C" str_gcol_c ";" 34920 31034 "}")
                                             (list "{\\C" str_gcol_g ";" 38750 34920 31034  "}")
                                             )
                                        )))
                    ;;編集中の管路のモデル部分が非表示になり、深度及び円弧の寸法のみが表示されるように切り替えることができます\n編集を完了すると非表示は終了します
                    (cons "HELP"(lambda()(mix_strasc(list 32232 38598 20013 12398 31649 36335 12398 12514 12487 12523 37096 20998 12364 38750 34920 31034 12395 12394 12426 12289 28145 24230 21450 12403 20870 24359 12398 23544 27861 12398 12415 12364 34920 31034 12373 12428 12427 12424 12358 12395 20999 12426 26367 12360 12427 12371 12392 12364 12391 12365 12414 12377 "\n" 32232 38598 12434 23436 20102 12377 12427 12392 38750 34920 31034 12399 32066 20102 12375 12414 12377 ))))
                    
                    )
               (list(list 54);;経路がないときの深度入力
                    (cons "ITEM"(mix_strasc(list 32076 36335 12364 12394 12356 12392 12365 12398 28145 24230 20837 21147 )))
                    (cons "INPUTSWITCH"
                          (lambda()
                            (if int_allow_noroad_temp T(setq int_allow_noroad_temp int_allow_noroad))
                            (list 'int_allow_noroad_temp ls_switch_allow_noroad)
                            ))
                    (cons "NOREFERENCE"
                          (setq ls_switch_allow_noroad
                                (mapcar 'mix_strasc;;表示非表示
                                        (list(list "{\\C" str_gcol_g ";"
                                                   35377 21487 12375 12394 12356 "}")
                                             (list "{\\C" str_gcol_c ";"
                                                   35377 21487 12377 12427 "}")
                                             )
                                        )))
                    ;;経路の選択をしなくてもクリックしたところの地表面標高を取得して深度設定ができる基に切り替えます\n許可しないとき経路を選ばなければ深度設定することはできません
                    (cons "HELP"(lambda()(mix_strasc(list 32076 36335 12398 36984 25246 12434 12375 12394 12367 12390 12418 12463 12522 12483 12463 12375 12383 12392 12371 12429 12398 22320 34920 38754 27161 39640 12434 21462 24471 12375 12390 28145 24230 35373 23450 12364 12391 12365 12427 22522 12395 20999 12426 26367 12360 12414 12377 "\n" 35377 21487 12375 12394 12356 12392 12365 32076 36335 12434 36984 12400 12394 12369 12428 12400 28145 24230 35373 23450 12377 12427 12371 12392 12399 12391 12365 12414 12379 12435 ))))
                    )
               
               (list(list 78);;管の名称
                    (cons "ITEM"(mix_strasc(list 31649 12398 21517 31216)))
                    (cons "INPUTSTR"
                          (lambda( / str i bool func)
                            (if(= str_ductname "")(setq str_ductname nil))
                            (if(if str_ductname
                                   (if(or(= int_allow_overwrite 1) vnam_currentinsert)
                                       nil
                                     (null
                                      (vl-catch-all-error-p
                                       (vl-catch-all-apply 'vla-Item(list vnam_blocktable str_ductname))))
                                     )
                                 T)
                                (progn
                                  (setq str "DUCT$" i 0 bool T)
                                  (while bool
                                    (setq i(1+ i)
                                          str_ductname(strcat str(substr(itoa(+ 1000 i))2))
                                          bool
                                          (null
                                           (vl-catch-all-error-p
                                            (vl-catch-all-apply 'vla-Item(list vnam_blocktable str_ductname))))
                                          )
                                    )
                                  ))
                            
                            'str_ductname)
                          )
                    (cons "NO-INPUT"
                          (lambda()
                            (if vnam_currentinsert
                                (progn
                                  (x-alert(list 32232 38598 20013 12399 21517 21069 12398 22793 26356 12399 12391 12365 12414 12379 12435 )) ;;編集中は名前の変更はできません
                                  T))
                            ))
                    (cons "LOADFUNCTION"
                          (lambda(str / bool)
                            (setq bool
                                  (vl-catch-all-error-p
                                   (vl-catch-all-apply 'vla-Item(list vnam_blocktable str)))
                                  )
                            (if bool T
                              (if(= int_allow_overwrite 0)nil
                                (progn
                                  (alert
                                   (mix_strasc(list 26082 12395 23384 22312 12377 12427 12502 12525 12483 12463 21517 31216 12391 12377 12364 19978 26360 12365 12373 12428 12414 12377 ))
                                   )
                                  T)))
                            )
                          )
                    (cons "STRINPUTALERT"
                          (mix_strasc(list 12502 12525 12483 12463 21517 31216 12364 26082 12395 23384 22312 12375 12390 12356 12414 12377 )))
                    ;;これから作成する管路、編集中の管路の名称を変更します\n管路はブロックとして作成されすでに使用されているブロック名称は設定できません
                    ;;\n名前を付けないとき「DUCT$(使用されていない数値)」を自動的に設定します
                    (cons "HELP"(lambda()(mix_strasc(list 12371 12428 12363 12425 20316 25104 12377 12427 31649 36335 12289 32232 38598 20013 12398 31649 36335 12398 21517 31216 12434 22793 26356 12375 12414 12377 "\n" 31649 36335 12399 12502 12525 12483 12463 12392 12375 12390 20316 25104 12373 12428 12377 12391 12395 20351 29992 12373 12428 12390 12356 12427 12502 12525 12483 12463 21517 31216 12399 35373 23450 12391 12365 12414 12379 12435 "\n" 21517 21069 12434 20184 12369 12394 12356 12392 12365 12300 "DUCT$(" 20351 29992 12373 12428 12390 12356 12394 12356 25968 20516 ")" 12301 12434 33258 21205 30340 12395 35373 23450 12375 12414 12377 ))))
                    )
               
               
               (list(list 77);;モード
                    (cons "ITEM"(mix_strasc(list 12514 12540 12489 20999 26367 )))
                    (cons "INPUTSWITCH" (lambda() (list 'int_ductmode ls_switch_ductmode )))
                    (cons "NOREFERENCE"
                          (setq ls_switch_ductmode
                                (mapcar 'mix_strasc
                                        (list(list "{\\C" str_gcol_y ";"
                                                   20316 25104 12539 32232 38598 "}")
                                             (list "{\\C" str_gcol_g ";"
                                                   26356 26032 12539 12467 12500 12540 "}")
                                             ;; (list "{\\C" str_gcol_c ";"
                                             ;;       26356 26032 12539 12467 12500 12540 "}")
                                             (list "{\\C" str_gcol_p ";"
                                                   21066 38500 "}")
                                             )))
                          )
                    (cons "LOADFUNCTION"
                          (lambda()
                            
                            (if vnam_road
                                (progn
                                  (if(vl-position vnam_road ls_vnam_killobj)
                                      (progn
                                        (vla-delete vnam_road)
                                        (exckillobj vnam_road)
                                        ))
                                  (setq vnam_road nil
                                        int_ductdepth nil)
                                  ))
                            
                            (mapcar
                             '(lambda(lst)
                                (if(setq lst(assoc "OBJ" lst))
                                    (mapcar
                                     '(lambda(vnam)
                                        (if(=(type vnam)'VLA-OBJECT)
                                            (progn
                                              (vla-delete vnam)
                                              (exckillobj vnam)
                                              ))
                                        )
                                     (cdr lst)))
                                )
                             ls_vnam_duct)
                            (setq ls_vnam_duct nil)


                            (if vnam_currentinsert
                                (progn
                                  (vla-put-visible vnam_currentinsert :vlax-true)
                                  (setq ls_vnam_visible
                                        (vl-remove vnam_currentinsert ls_vnam_visible)
                                        vnam_currentinsert nil)
                                  ))
                            
                            (cond
                             ((= int_ductmode 0)
                              (setq bool_selectent nil bool_select nil int_selectmode -1
                                    ls_ssget nil xtype_ssget nil xdata_ssget nil
                                    vnam_depth nil entna_depth nil
                                    int_ductdepth nil
                                    int_max_duct 0 int_min_duct 0
                                    bool_ductedit nil bool_solidon nil
                                    )
                              
                              )
                             ((= int_ductmode 1)
                              (setq vnam_depth nil entna_depth nil
                                    int_ductdepth nil
                                    bool_ductedit nil
                                    duct_depth 0.)
                              )
                             ((= int_ductmode 2)
                              (setq bool_selectent T bool_select T int_selectmode 0
                                    ls_ssget(list(cons 0 "INSERT"))
                                    xtype_ssget "DUCTBLOCK" xdata_ssget "terraduct3d"
                                    bool_ductedit nil
                                    )
                              )
                             )
                            
                            ))
                    ;;-作成・編集：新規管路の作成または作成済みの管路を選択して編集を行う
                    ;;\n-更新・コピー：深度オフセットが0のとき、モデルを更新,0でないとき適用してコピー
                    ;;\n-削除：管路を選択して削除
                    (cons "HELP"
                          (lambda()
                            (mix_strasc(list "-" 20316 25104 12539 32232 38598 65306 26032 35215 31649 36335 12398 20316 25104 12414 12383 12399 20316 25104 28168 12415 12398 31649 36335 12434 36984 25246 12375 12390 32232 38598 12434 34892 12358
                                             "\n-" 26356 26032 12539 12467 12500 12540 65306 28145 24230 12458 12501 12475 12483 12488 12364 "0" 12398 12392 12365 12289 12514 12487 12523 12434 26356 26032 ",0" 12391 12394 12356 12392 12365 36969 29992 12375 12390 12467 12500 12540
                                             "\n-" 21066 38500 65306 31649 36335 12434 36984 25246 12375 12390 21066 38500
                                             ))))
                    
                    )
               
               (list(list 80 );;P選択モード切替
                    (cons "ITEM"(mix_strasc(list 36984 25246 12514 12540 12489 20999 26367 )))
                    (cons "INPUTSWITCH"
                          (lambda()
                            (setq int_temp 0)
                            (if(= int_ductmode 2)
                                (list 'int_selectmode
                                      (mapcar 'mix_strasc
                                              (list(list "{\\C" str_gcol_y ";" 36984 25246 "}")
                                                   (list "{\\C" str_gcol_c ";" 35299 38500 "}")))
                                      )
                              (list 'int_temp ;;削除モードのとき使用可能
                                    (list(mix_strasc
                                          (list "{\\C" str_gcol_p ";"
                                                21066 38500 12514 12540 12489 12398 12392 12365 20351 29992 21487 33021
                                                "}")))
                                    )
                              )) )
                    (cons "HELP"(lambda()str_guide_selectmode))
                    )
               
               (list(list 65);;A直線部を追加挿入
                    (cons "ITEM"(mix_strasc(list  30452 32218 37096 12434 36861 21152 25407 20837  )))
                    (cons "STATUS"
                          (lambda()
                            (mix_strasc
                             (if(apply 'or(mapcar '(lambda(lst)(assoc "LINE" lst))ls_vnam_duct))
                                 (if int_adddepth;;直線部を選択してください,挿入可能
                                     (list "{\\C" str_gcol_y ";" 30452 32218 37096 12434 36984 25246 12375 12390 12367 12384 12373 12356 "}")
                                   (list "{\\C" str_gcol_g ";" 25407 20837 21487 33021 "}")
                                   )
                               (list "{\\C" str_gcol_p ";" ;;直線部がないので挿入できません
                                      30452 32218 37096 12364 12394 12356 12398 12391 25407 20837 12391 12365 12414 12379 12435 "}"))
                             )
                            ))
                    (cons "LOADFUNCTION"
                          (lambda()(if(apply 'or(mapcar '(lambda(lst)(assoc "LINE" lst))ls_vnam_duct))
                                       (progn;;深度を入力して直線部を選択してください
                                         (x-alert(list 28145 24230 12434 20837 21147 12375 12390 30452 32218 37096 12434 36984 25246 12375 12390 12367 12384 12373 12356 ))
                                         (setq int_adddepth 0 )
                                         )
                                     ;;現在の作業において、まだ直線部が生成されていません
                                     (x-alert(list 29694 22312 12398 20316 26989 12395 12362 12356 12390 12289 12414 12384 30452 32218 37096 12364 29983 25104 12373 12428 12390 12356 12414 12379 12435))
                                     ))
                          )
                    
                    (cons "HELP"
                          (lambda()
                            (setq ls_guide_drawing
                                  (list
                                   1
                                   
                                   (list(list -15.0 9.0 0.0)(list 15.0 9.0 0.0)256)
                                   (list(list -11.7321 6.0 0.0)(list -12.0 5.0 0.0)30)
                                   (list(list -12.0 5.0 0.0)(list -12.2679 6.0 0.0)30)
                                   (list(list -12.0 5.0 0.0)(list -12.0 9.0 0.0)30)
                                   (list(list -12.2679 8.0 0.0)(list -12.0 9.0 0.0)30)
                                   (list(list -12.0 9.0 0.0)(list -11.7321 8.0 0.0)30)
                                   (list(list 11.2679 6.0 0.0)(list 11.0 5.0 0.0)30)
                                   (list(list 11.0 5.0 0.0)(list 10.7321 6.0 0.0)30)
                                   (list(list 11.0 5.0 0.0)(list 11.0 9.0 0.0)30)
                                   (list(list 10.7321 8.0 0.0)(list 11.0 9.0 0.0)30)
                                   (list(list 11.0 9.0 0.0)(list 11.2679 8.0 0.0)30)
                                   (list(list -12.0 5.0 0.0)(list 11.0 5.0 0.0)3)
                                   (list(list -15.0 -4.0 0.0)(list 15.0 -4.0 0.0)256)
                                   (list(list -11.7321 -7.0 0.0)(list -12.0 -8.0 0.0)30)
                                   (list(list -12.0 -8.0 0.0)(list -12.2679 -7.0 0.0)30)
                                   (list(list -12.0 -8.0 0.0)(list -12.0 -4.0 0.0)30)
                                   (list(list -12.2679 -5.0 0.0)(list -12.0 -4.0 0.0)30)
                                   (list(list -12.0 -4.0 0.0)(list -11.7321 -5.0 0.0)30)
                                   (list(list 11.2679 -7.0 0.0)(list 11.0 -8.0 0.0)30)
                                   (list(list 11.0 -8.0 0.0)(list 10.7321 -7.0 0.0)30)
                                   (list(list 11.0 -8.0 0.0)(list 11.0 -4.0 0.0)30)
                                   (list(list 10.7321 -5.0 0.0)(list 11.0 -4.0 0.0)30)
                                   (list(list 11.0 -4.0 0.0)(list 11.2679 -5.0 0.0)30)
                                   (list(list -12.0 -8.0 0.0)(list -6.0 -8.0 0.0)3)
                                   (list(list 5.0 -8.0 0.0)(list 11.0 -8.0 0.0)3)
                                   (list(list -4.0 -11.0 0.0)(list 2.0 -11.0 0.0)6)
                                   (list(list -11.7321 -10.0 0.0)(list -12.0 -11.0 0.0)4)
                                   (list(list -12.0 -11.0 0.0)(list -12.2679 -10.0 0.0)4)
                                   (list(list -12.0 -11.0 0.0)(list -12.0 -8.0 0.0)4)
                                   (list(list -12.2679 -9.0 0.0)(list -12.0 -8.0 0.0)4)
                                   (list(list -12.0 -8.0 0.0)(list -11.7321 -9.0 0.0)4)
                                   (list(list -7.87298 -8.0 0.0)(list -7.35182 -8.0691 0.0)6)
                                   (list(list -7.35182 -8.0691 0.0)(list -6.86667 -8.27161 0.0)6)
                                   (list(list -6.86667 -8.27161 0.0)(list -6.45105 -8.59355 0.0)6)
                                   (list(list -6.45105 -8.59355 0.0)(list -6.13368 -9.01266 0.0)6)
                                   (list(list -6.13368 -9.01266 0.0)(list -5.93649 -9.5 0.0)6)
                                   (list(list -4.0 -11.0 0.0)(list -4.52116 -10.9309 0.0)6)
                                   (list(list -4.52116 -10.9309 0.0)(list -5.00631 -10.7284 0.0)6)
                                   (list(list -5.00631 -10.7284 0.0)(list -5.42193 -10.4065 0.0)6)
                                   (list(list -5.42193 -10.4065 0.0)(list -5.7393 -9.98734 0.0)6)
                                   (list(list -5.7393 -9.98734 0.0)(list -5.93649 -9.5 0.0)6)
                                   (list(list 5.87298 -8.0 0.0)(list 5.35182 -8.0691 0.0)6)
                                   (list(list 5.35182 -8.0691 0.0)(list 4.86667 -8.27161 0.0)6)
                                   (list(list 4.86667 -8.27161 0.0)(list 4.45105 -8.59355 0.0)6)
                                   (list(list 4.45105 -8.59355 0.0)(list 4.13368 -9.01266 0.0)6)
                                   (list(list 4.13368 -9.01266 0.0)(list 3.93649 -9.5 0.0)6)
                                   (list(list 2.0 -11.0 0.0)(list 2.52116 -10.9309 0.0)6)
                                   (list(list 2.52116 -10.9309 0.0)(list 3.00631 -10.7284 0.0)6)
                                   (list(list 3.00631 -10.7284 0.0)(list 3.42193 -10.4065 0.0)6)
                                   (list(list 3.42193 -10.4065 0.0)(list 3.7393 -9.98734 0.0)6)
                                   (list(list 3.7393 -9.98734 0.0)(list 3.93649 -9.5 0.0)6)
                                   (list(list -3.73205 -10.0 0.0)(list -4.0 -11.0 0.0)30)
                                   (list(list -4.0 -11.0 0.0)(list -4.26795 -10.0 0.0)30)
                                   (list(list -4.0 -11.0 0.0)(list -4.0 -4.0 0.0)30)
                                   (list(list -4.26795 -5.0 0.0)(list -4.0 -4.0 0.0)30)
                                   (list(list -4.0 -4.0 0.0)(list -3.73205 -5.0 0.0)30)
                                   (list(list 2.26795 -10.0 0.0)(list 2.0 -11.0 0.0)30)
                                   (list(list 2.0 -11.0 0.0)(list 1.73205 -10.0 0.0)30)
                                   (list(list 2.0 -11.0 0.0)(list 2.0 -4.0 0.0)30)
                                   (list(list 1.73205 -5.0 0.0)(list 2.0 -4.0 0.0)30)
                                   (list(list 2.0 -4.0 0.0)(list 2.26795 -5.0 0.0)30)
                                   (list(list 8.66123 4.33552 0.0)(list 9.1595 3.66803 0.0)3)
                                   (list(list 8.16061 3.66803 0.0)(list 8.66123 4.33552 0.0)3)
                                   (list(list 8.66123 4.33552 0.0)(list 8.66123 2.16852 0.0)3)
                                   (list(list 12.4429 2.32364 0.0)(list 12.6591 2.16852 0.0)3)
                                   (list(list 12.1233 2.55632 0.0)(list 12.4429 2.32364 0.0)3)
                                   (list(list 11.4793 2.32364 0.0)(list 11.6932 2.47876 0.0)3)
                                   (list(list 11.1596 2.16852 0.0)(list 11.4793 2.32364 0.0)3)
                                   (list(list 12.1233 3.33428 0.0)(list 12.1233 2.63388 0.0)3)
                                   (list(list 11.6932 3.33428 0.0)(list 11.6932 2.63388 0.0)3)
                                   (list(list 11.1596 2.63388 0.0)(list 12.6591 2.63388 0.0)3)
                                   (list(list 11.3735 3.02404 0.0)(list 12.4429 3.02404 0.0)3)
                                   (list(list 12.6591 3.55756 0.0)(list 12.6591 3.72444 0.0)3)
                                   (list(list 12.6098 3.50116 0.0)(list 12.6591 3.55756 0.0)3)
                                   (list(list 12.0387 3.50116 0.0)(list 12.6098 3.50116 0.0)3)
                                   (list(list 11.9917 3.55756 0.0)(list 12.0387 3.50116 0.0)3)
                                   (list(list 11.9917 3.94537 0.0)(list 11.9917 3.55756 0.0)3)
                                   (list(list 12.5628 3.94537 0.0)(list 11.9917 3.94537 0.0)3)
                                   (list(list 12.5628 4.33552 0.0)(list 12.5628 3.94537 0.0)3)
                                   (list(list 12.0387 4.33552 0.0)(list 12.5628 4.33552 0.0)3)
                                   (list(list 11.8248 3.55756 0.0)(list 11.8248 3.72444 0.0)3)
                                   (list(list 11.7778 3.50116 0.0)(list 11.8248 3.55756 0.0)3)
                                   (list(list 11.2066 3.50116 0.0)(list 11.7778 3.50116 0.0)3)
                                   (list(list 11.1596 3.55756 0.0)(list 11.2066 3.50116 0.0)3)
                                   (list(list 11.1596 3.94537 0.0)(list 11.1596 3.55756 0.0)3)
                                   (list(list 11.7308 3.94537 0.0)(list 11.1596 3.94537 0.0)3)
                                   (list(list 11.7308 4.33552 0.0)(list 11.7308 3.94537 0.0)3)
                                   (list(list 11.2066 4.33552 0.0)(list 11.7308 4.33552 0.0)3)
                                   (list(list 11.491 2.00164 0.0)(list 12.8237 2.00164 0.0)3)
                                   (list(list 10.9928 2.15677 0.0)(list 11.491 2.00164 0.0)3)
                                   (list(list 10.8259 2.31189 0.0)(list 10.9928 2.15677 0.0)3)
                                   (list(list 10.8259 2.31189 0.0)(list 10.4921 2.00164 0.0)3)
                                   (list(list 10.8259 3.40009 0.0)(list 10.8259 2.31189 0.0)3)
                                   (list(list 10.4921 3.40009 0.0)(list 10.8259 3.40009 0.0)3)
                                   (list(list 10.659 4.33552 0.0)(list 10.8259 3.86781 0.0)3)
                                   (list(list 14.5136 4.33552 0.0)(list 15.5853 4.33552 0.0)3)
                                   (list(list 15.4654 2.15677 0.0)(list 15.8227 2.00164 0.0)3)
                                   (list(list 14.9907 2.7796 0.0)(list 15.4654 2.15677 0.0)3)
                                   (list(list 14.8708 3.08985 0.0)(list 14.9907 2.7796 0.0)3)
                                   (list(list 14.8708 3.40009 0.0)(list 14.8708 3.08985 0.0)3)
                                   (list(list 14.396 2.62448 0.0)(list 14.1563 2.00164 0.0)3)
                                   (list(list 14.5136 3.24497 0.0)(list 14.396 2.62448 0.0)3)
                                   (list(list 14.5136 4.33552 0.0)(list 14.5136 3.24497 0.0)3)
                                   (list(list 15.5853 3.40009 0.0)(list 14.5136 3.40009 0.0)3)
                                   (list(list 15.5853 4.33552 0.0)(list 15.5853 3.40009 0.0)3)
                                   (list(list 14.5136 4.33552 0.0)(list 15.5853 4.33552 0.0)3)
                                   (list(list 14.1446 3.00054 0.0)(list 14.2644 3.16741 0.0)3)
                                   (list(list 13.9072 2.83366 0.0)(list 14.1446 3.00054 0.0)3)
                                   (list(list 13.5499 2.66679 0.0)(list 13.9072 2.83366 0.0)3)
                                   (list(list 13.9072 2.00164 0.0)(list 13.7873 2.00164 0.0)3)
                                   (list(list 13.9072 4.50005 0.0)(list 13.9072 2.00164 0.0)3)
                                   (list(list 13.5499 3.8349 0.0)(list 14.2644 3.8349 0.0)3)
                                   (list(list -0.335853 -1.33113 0.0)(list -0.335853 1.6679 0.0)2)
                                   (list(list 0.331642 1.6679 0.0)(list 0.331642 -1.33113 0.0)2)
                                   (list(list 0.996787 -0.665986 0.0)(list -0.00210513 -1.66488 0.0)2)
                                   (list(list -0.00210513 -1.66488 0.0)(list -1.001 -0.665986 0.0)2)
                                   (list(list -19.7966 -8.99897 0.0)(list -19.5302 -8.99897 0.0)4)
                                   (list(list -19.5302 -8.99897 0.0)(list -19.5302 -9.39851 0.0)4)
                                   (list(list -19.5302 -9.39851 0.0)(list -19.6634 -9.66487 0.0)4)
                                   (list(list -19.6634 -9.66487 0.0)(list -19.9964 -9.99782 0.0)4)
                                   (list(list -19.5302 -9.39851 0.0)(list -19.3971 -9.66487 0.0)4)
                                   (list(list -19.3971 -9.66487 0.0)(list -19.0617 -9.99782 0.0)4)
                                   (list(list -18.0628 -9.99782 0.0)(list -17.9296 -9.86464 0.0)4)
                                   (list(list -17.9296 -9.86464 0.0)(list -17.9296 -9.26533 0.0)4)
                                   (list(list -17.9296 -9.26533 0.0)(list -18.7953 -9.26533 0.0)4)
                                   (list(list -18.6621 -9.86464 0.0)(list -18.7953 -9.99782 0.0)4)
                                   (list(list -18.4623 -9.59828 0.0)(list -18.6621 -9.86464 0.0)4)
                                   (list(list -18.3957 -9.26533 0.0)(list -18.4623 -9.59828 0.0)4)
                                   (list(list -18.3957 -8.99897 0.0)(list -18.3957 -9.26533 0.0)4)
                                   (list(list -18.196 -9.99782 0.0)(list -18.0628 -9.99782 0.0)4)
                                   (list(list -17.2637 -9.06556 0.0)(list -16.6644 -9.06556 0.0)4)
                                   (list(list -16.6644 -9.06556 0.0)(list -16.6644 -9.19874 0.0)4)
                                   (list(list -16.9308 -9.39851 0.0)(list -16.6644 -9.39851 0.0)4)
                                   (list(list -16.6644 -9.39851 0.0)(list -16.6644 -9.33192 0.0)4)
                                   (list(list -17.2637 -9.59828 0.0)(list -16.6644 -9.59828 0.0)4)
                                   (list(list -17.2637 -9.06556 0.0)(list -17.2637 -9.19874 0.0)4)
                                   (list(list -16.9974 -9.4651 0.0)(list -16.9974 -9.99782 0.0)4)
                                   (list(list -16.9308 -9.06556 0.0)(list -16.9308 -9.39851 0.0)4)
                                   (list(list -17.5301 -8.99897 0.0)(list -17.3969 -9.13215 0.0)4)
                                   (list(list -17.5967 -9.26533 0.0)(list -17.4635 -9.39851 0.0)4)
                                   (list(list -17.5967 -9.99782 0.0)(list -17.3969 -9.59828 0.0)4)
                                   (list(list -17.0639 -9.06556 0.0)(list -17.0639 -9.13215 0.0)4)
                                   (list(list -17.0639 -9.13215 0.0)(list -17.1305 -9.26533 0.0)4)
                                   (list(list -17.1305 -9.26533 0.0)(list -17.2637 -9.39851 0.0)4)
                                   (list(list -16.9974 -9.59828 0.0)(list -17.2637 -9.93123 0.0)4)
                                   (list(list -17.2637 -9.93123 0.0)(list -17.3303 -9.93123 0.0)4)
                                   (list(list -16.9974 -9.59828 0.0)(list -16.731 -9.93123 0.0)4)
                                   (list(list -16.731 -9.93123 0.0)(list -16.6644 -9.93123 0.0)4)
                                   (list(list -15.8653 -9.86464 0.0)(list -15.4658 -9.99782 0.0)4)
                                   (list(list -16.0651 -9.73146 0.0)(list -15.8653 -9.86464 0.0)4)
                                   (list(list -15.7987 -9.86464 0.0)(list -16.1983 -9.99782 0.0)4)
                                   (list(list -15.599 -9.73146 0.0)(list -15.7987 -9.86464 0.0)4)
                                   (list(list -15.599 -9.66487 0.0)(list -15.599 -9.73146 0.0)4)
                                   (list(list -16.1317 -9.66487 0.0)(list -15.599 -9.66487 0.0)4)
                                   (list(list -16.2649 -9.79805 0.0)(list -16.398 -9.99782 0.0)4)
                                   (list(list -16.2649 -9.13215 0.0)(list -16.2649 -9.79805 0.0)4)
                                   (list(list -15.6656 -9.13215 0.0)(list -15.6656 -9.53169 0.0)4)
                                   (list(list -15.6656 -9.53169 0.0)(list -15.9985 -9.53169 0.0)4)
                                   (list(list -15.9985 -9.53169 0.0)(list -15.9985 -9.13215 0.0)4)
                                   (list(list -15.8653 -8.99897 0.0)(list -15.8653 -9.13215 0.0)4)
                                   (list(list -16.1317 -9.33192 0.0)(list -15.5324 -9.33192 0.0)4)
                                   (list(list -16.2649 -9.13215 0.0)(list -15.4658 -9.13215 0.0)4)
                                   (list(list -14.4373 -9.14201 0.0)(list -15.197 -9.56868 0.0)4)
                                   (list(list -15.197 -9.56868 0.0)(list -14.4373 -9.99782 0.0)4)
                                   (list(list -14.1981 -9.23573 0.0)(list -14.1981 -9.18887 0.0)4)
                                   (list(list -14.1981 -9.18887 0.0)(list -14.1512 -9.09269 0.0)4)
                                   (list(list -14.1512 -9.09269 0.0)(list -14.1044 -9.04583 0.0)4)
                                   (list(list -14.1044 -9.04583 0.0)(list -14.0082 -8.99897 0.0)4)
                                   (list(list -14.0082 -8.99897 0.0)(list -13.8183 -8.99897 0.0)4)
                                   (list(list -13.8183 -8.99897 0.0)(list -13.7221 -9.04583 0.0)4)
                                   (list(list -13.7221 -9.04583 0.0)(list -13.6752 -9.09269 0.0)4)
                                   (list(list -13.6752 -9.09269 0.0)(list -13.6284 -9.18887 0.0)4)
                                   (list(list -13.6284 -9.18887 0.0)(list -13.6284 -9.28506 0.0)4)
                                   (list(list -13.6284 -9.28506 0.0)(list -13.6752 -9.37878 0.0)4)
                                   (list(list -13.6752 -9.37878 0.0)(list -13.7714 -9.52182 0.0)4)
                                   (list(list -13.7714 -9.52182 0.0)(list -14.2474 -9.99782 0.0)4)
                                   (list(list -14.2474 -9.99782 0.0)(list -13.5791 -9.99782 0.0)4)
                                   (list(list -13.2954 -9.99782 0.0)(list -13.2954 -8.99897 0.0)4)
                                   (list(list -13.2954 -8.99897 0.0)(list -12.8663 -8.99897 0.0)4)
                                   (list(list -12.8663 -8.99897 0.0)(list -12.7233 -9.04583 0.0)4)
                                   (list(list -12.7233 -9.04583 0.0)(list -12.6764 -9.09269 0.0)4)
                                   (list(list -12.6764 -9.09269 0.0)(list -12.6271 -9.18887 0.0)4)
                                   (list(list -12.6271 -9.18887 0.0)(list -12.6271 -9.28506 0.0)4)
                                   (list(list -12.6271 -9.28506 0.0)(list -12.6764 -9.37878 0.0)4)
                                   (list(list -12.6764 -9.37878 0.0)(list -12.7233 -9.4281 0.0)4)
                                   (list(list -12.7233 -9.4281 0.0)(list -12.8663 -9.47496 0.0)4)
                                   (list(list -12.8663 -9.47496 0.0)(list -13.2954 -9.47496 0.0)4)
                                   (list(list -12.9625 -9.47496 0.0)(list -12.6271 -9.99782 0.0)4)

                                   ))

                            ;;直線部を選択して直線部を追加します\n一旦、下方向に曲げ半径の1/5倍の値でオフセットさせるので深度を調整してください
                            (mix_strasc(list 30452 32218 37096 12434 36984 25246 12375 12390 30452 32218 37096 12434 36861 21152 12375 12414 12377 "\n" 19968 26086 12289 19979 26041 21521 12395 26354 12370 21322 24452 12398 "1/5" 20493 12398 20516 12391 12458 12501 12475 12483 12488 12373 12379 12427 12398 12391 28145 24230 12434 35519 25972 12375 12390 12367 12384 12373 12356 str_startguidedrawing ))))

                    
                    )
               
               
               (list(list nil)
                    (cons "ITEM"(list ))
                    (cons "BOOL"
                          (lambda()
                            T
                            ))
                    (cons
                     "STATUS"
                     (lambda( / bool_depth ii ls_out)

                       (setq ls_out
                             (cond
                              ((= int_ductmode 0);;作成編集
                               (if(or ls_vnam_duct bool_ductedit)
                                   (list
                                    (list
                                     "{\\C" str_gcol_y ";" 31649 36335 32232 38598 20013 " " 9733 "}")
                                    (list
                                     "{\\C" str_gcol_g ";" 30452 32218 37096 "{\\C"
                                     ((lambda( / ii)
                                        (setq ii 0)
                                        (mapcar '(lambda(lst)
                                                   (if(assoc "DEPTH" lst)(setq ii(1+ ii))))
                                                ls_vnam_duct)
                                        (if(=(rem ii 2)0)
                                            (list str_gcol_y "; " "< 1 > ");;9312);;9313))
                                          (list str_gcol_c "; " "< 2 > "))
                                        ))
                                     "}" 28857 30446 12434 36984 25246 "} " 12304 "{\\C"
                                     (if vnam_road
                                         (list str_gcol_c ";" 32076 36335 36984 25246 12354 12426)
                                       (list str_gcol_p ";" 32076 36335 36984 25246 12394 12375 )
                                       );;経路選択ありなし
                                     "}" 12305
                                     )
                                    (list
                                     "   {\\C" str_gcol_p ";"
                                     28145 24230 "(" 25991 23383 ")};" 28145 12373 22793 26356
                                     " , {\\C" str_gcol_p ";"
                                     28145 24230 "(" 25991 23383 20197 22806 ")};" 20301 32622 22793 26356
                                     ;;深さ寸法(文字);深さ変更,深さ寸法(文字以外);位置変更
                                     )
                                    (list
                                     "   {\\C" str_gcol_p ";"
                                     20870 24359 "(" 25991 23383 ")};" 26354 12370 21322 24452
                                     " , {\\C" str_gcol_p ";"
                                     20870 24359 "(" 25991 23383 20197 22806 ")};"
                                     20301 32622 22793 26356 "(" 21487 33021 12394 26178 12398 12415 ")" 
                                     ;;円弧部;曲げ半径,
                                     )
                                    (list
                                     "   {\\C" str_gcol_p ";" 30452 32218 37096 "};"
                                     30452 32218 37096 12434 21066 38500
                                     (if int_selectmenu T
                                       (list " , {\\C" str_gcol_p ";Enter}; : " 24418 29366 30906 23450))
                                     ;;直線部;直線部を削除,その他;直線部の追加
                                     )
                                    ;; (if(if entna_depth
                                    ;;        ((lambda(lst / ii)
                                    ;;           (setq ii -1)
                                    ;;           (while lst
                                    ;;             (if(assoc "LINE"(car lst))(setq ii(1+ ii)))
                                    ;;             (if(= ii 2)(setq lst nil)
                                    ;;               (setq lst(cdr lst)))
                                    ;;             )
                                    ;;           (= ii 2))
                                    ;;         ls_vnam_duct))
                                    ;;     (list "   {\\C" str_gcol_c ";F : "
                                    ;;           20182 12398 "3" 28857 12434 36984 25246 12375 12390 21516 19968 24179 38754 12395 12394 12427 12424 12358 12395 28145 24230 12434 35519 25972 12377 12427 ))
                                    ;; ;;他の3点を選択して同一平面になるように深度を調整する
                                    
                                    )
                                 
                                 (list
                                  (list
                                   "{\\C" str_gcol_g ";"
                                   " - " 32218 24418 12458 12502 12472 12455 12463 12488 12434 36984 25246 12375 12390
                                   32076 36335 12354 12426 12391 26032 35215 20316 25104 38283 22987
                                   ;;線形オブジェクトを選択して経路ありで新規作成開始
                                   )
                                  (list
                                   " - " 20219 24847 28857 12434 12463 12522 12483 12463 12375 12390
                                   32076 36335 12394 12375 12391 26032 35215 20316 25104 38283 22987
                                   ;;任意点をクリックして経路なしで新規作成開始
                                   )
                                  (list
                                   " - " 20316 25104 28168 12415 12398 31649 36335 12434 36984 25246
                                   32232 38598 38283 22987 "}"
                                   ;;作成済みの管路を選択して編集開始
                                   )
                                  )
                                 )
                               )
                              
                              ((= int_ductmode 1);;更新コピー
                               (list
                                (list
                                 " - " "{\\C" str_gcol_y ";"
                                 20316 25104 28168 12415 12398 31649 36335 12434 12463 12522 12483 12463 12377 12427 12392 23455 34892 "}"
                                 ;;作成済みの管路をクリックすると実行
                                 )
                                (list
                                 " - " "{\\C" str_gcol_g ";"
                                  12300 28145 24230 12301 12300 12458 12501 12475 12483 12488 12301 12364 12356 12378 12428 12418 "0" 12398 12392 12365 12289 29694 22312 35373 23450 12373 12428 12390 12356 12427 12497 12521 12513 12540 12479 12391 26356 26032 "}"
                                  )
                                (list
                                 " - " "{\\C" str_gcol_g ";"
                                 12381 12398 20182 12398 20516 12434 20837 21147 12375 12383 12392 12365 12289 36984 25246 12373 12428 12383 31649 36335 12398 12497 12521 12513 12540 12479 12434 20351 12387 12390 12467 12500 12540 "}"
                                 )
                                (list
                                 " - " "{\\C" str_gcol_c ";"
                                 "( " 12300 28145 24230 12301 12399 29694 22312 12398 20301 32622 12363 12425 12398 28145 24230 12434 20837 21147 " )}"
                                 )
                                ;;「深度」「オフセット」がいずれも0のとき、現在設定されているパラメータで更新
                                ;;その他の値を入力したとき、選択された管路のパラメータを使ってコピー
                                ;;「深度」は現在の位置からの深度を入力
                                
                                )
                               )
                              ((= int_ductmode 2) ;;削除
                               (list
                                (list
                                 "{\\C" str_gcol_p ";"
                                 " - " 31649 36335 12434 36984 25246 12375 12390 "Enter : "
                                 21066 38500 "}"
                                 ;;管路を選択してEnter:削除
                                 )
                                )
                               )
                              );;cond
                             ls_out(vl-remove nil ls_out)
                             )
                       ls_out
                       ))
                    )
                    
               (list(list "ENTER");;メインメニューへ
                    (cons "ITEM"(mix_strasc(list 12513 12452 12531 12513 12491 12517 12540 12408)))
                    (cons "LOADFUNCTION"
                          (lambda()
                            (setq str_next
                                  (if bool_ductedit
                                      (if ls_vnam_duct nil "home")
                                    "home"))

                            ;; (if(if bool_ductedit ls_vnam_duct)
                            ;;     (setq str_next nil )
                            ;;   (progn
                            ;;     (setq str_next "home")
                            ;;     (mapcar
                            ;;      '(lambda(lst)
                            ;;         (mapcar '(lambda(v)
                                               
                            ;;                    (if(=(type v)'VLA-OBJECT)
                            ;;                        (progn
                            ;;                          (vl-delete a)
                            ;;                          (exckillobj v)))
                            ;;                    )
                            ;;                 (cdr(assoc "OBJ" lst)))
                            ;;         )
                            ;;      ls_vnam_duct)
                            ;;     (setq ls_vnam_duct nil)
                            ;;     ))
                            
                            ))
                    (cons "NEXTMODE" "home")
                    )
               )
              )
             
             (setq int_selectmenu_ductedit_depth
                   ((lambda(lst / a ii)
                      (setq ii -1)
                      (while lst
                        (setq a(car lst)lst(cdr lst)ii(1+ ii))
                        (if(vl-position(cons "KEYSEARCH" "DEPTH")a)(setq lst nil))
                        )
                      ii)
                    ls_guidemenu)
                   
                   int_selectmenu_ductedit_offset
                   ((lambda(lst / a ii)
                      (setq ii -1)
                      (while lst
                        (setq a(car lst)lst(cdr lst)ii(1+ ii))
                        (if(vl-position(cons "KEYSEARCH" "OFFSET")a)(setq lst nil))
                        )
                      ii)
                    ls_guidemenu)
                   )
             
             
             ))
       
       (if(cadr bool) (list ) )
       ))
    
    (cons
     "MOVE"
     (lambda( / p);;(d- gr5_home()(progn))
 
       (if(setq vnam(cadr(assoc "OBJ"(assoc(list "SOLID" 0)ls_vnam_duct))))
           ((lambda(bool / visble-1 visible-2)
              (if(if bool(null bool_firsttime_editduct))
                  (setq visible-1 :vlax-false visible-2 :vlax-true)
                (setq visible-1 :vlax-true visible-2 :vlax-false))
              
              (if(=(vla-get-visible vnam)visible-1)
                  (progn(vla-put-visible vnam visible-2)
                        (if(setq vnam(cadr(assoc "OBJ"(assoc(list "MESH" 0)ls_vnam_duct))))
                            (vla-put-visible vnam visible-2)))))
            (= int_hidesolid 0)))

       ;;p_roadclick_temp
       (cond
        ((or(= int_editdepth 2)(null int_selectmenu_ductedit))
         
         (setq p_ground elem_grread)
         (setq p_click p_ground)
         
         (if(vl-position int_ductdepth(list 0 1))
             (if(or str_lasground height_ground)
                 (if(setq p(car(project_to_ground
                                (list p_ground)vec_view
                                (list str_lasground height_ground))))
                     (setq p_ground p)
                   (progn;;
                     
                     (setq ls_p
                           (project_to_ground
                            (mapcar '(lambda(a)(mapcar '+ p_ground a))
                                    (list(list size_grid 0. 0.) (list (- size_grid)0. 0.)
                                         (list 0. size_grid 0.) (list 0.(- size_grid)0.)))
                            vec_view(list str_lasground height_ground))
                           )
                     (if(setq ls_p(vl-remove nil ls_p))
                         (setq nn(length ls_p)
                               p_ground(mapcar '(lambda(func)(/(apply '+(mapcar 'func ls_p))nn))
                                               (list car cadr caddr))
                               )
                       )
                     )
                   )
               (if ls_p_road
                   (setq p_ground
                         ((lambda(p v z / cosdist d n bool )
                            (setq cosdist(caddr v))
                            (if(<(abs cosdist)1e-8) (setq p(carxyz elem_grread z))
                              (setq d(/(- z(caddr p))cosdist)
                                    p(mapcar '(lambda(a b)(+ a(* d b)))p v)) )
                            p )
                          p_ground vec_view
                          (/(apply '+(mapcar 'caddr ls_p_road))(length ls_p_road))
                          )
                         )
                 )
               ))
         
         (if vnam_road
             (if(setq p_close(vlax-curve-getclosestpointto vnam_road p_ground nil))
                 ((lambda(ls_p / p vec vec1 vec2 x y ang ang1 ang2)

                    (while ls_p
                      (setq p(car ls_p)ls_p(cdr ls_p))
                      (if(<(distance p p_close)0.2)
                          (setq ls_p nil p_close p ))
                      )
                    
                    (if(setq vec_road(xvla-normal vnam_road p_close))
                        (progn
                          (setq vec(trans-x vec_road(list 0 0 1)vec_view)
                                ang(atan(cadr vec)(car vec))
                                ang(+ ang(* 0.9 pi))
                                vec1(list(cos ang)(sin ang)0.)
                                vec1(trans-x vec1 vec_view(list 0 0 1))
                                ang(+ ang(* 0.2 pi))
                                vec2(list(cos ang)(sin ang)0.)
                                vec2(trans-x vec2 vec_view(list 0 0 1))
                                
                                )
                          
                          (mapcar
                           '(lambda(v)
                              (grdraw p_close(mapcar '(lambda(a b)(+ a(* height_text 3. b)))p_close v) 3))
                           (list vec1 vec2))
                          ))
                    (grdraw p_ground p_close 2)
                    (setq p_ground p_close)

                    (if(= int_inputdepth_temp 2)T
                      ((lambda(p c / s ls_p p0 p1 p2)
                         (setq s(* height_text 8.(if(= int_inputdepth_temp 0)1 -1))
                               p0(mapcar '+ p(list 0 0 s))
                               p1(mapcar '(lambda(a x y)(+ a(*  0.1 s x)(* -0.3 s y)))
                                         p0 vec_x_onview vec_y_onview)
                               p2(mapcar '(lambda(a x y)(+ a(* -0.1 s x)(* -0.3 s y)))
                                         p0 vec_x_onview vec_y_onview)
                               )
                         (mapcar '(lambda(p0 p1)(grdraw p0 p1 c))
                                 (list p p0 p0)(list p0 p1 p2))
                         )
                       p_close 4)
                      )
                    
                    )
                  ls_p_road)
               (setq p_ground nil);;closeしてないときは作成できない
               )
           )
         )
        
        ((= int_selectmenu int_selectmenu_ductedit)

         (if vnam_road
             ((lambda(ls_p p_close / p vec vec1 vec2 x y ang ang1 ang2)
                (if(setq vec_road(xvla-normal vnam_road p_close))
                    (progn
                      (setq vec(trans-x vec_road(list 0 0 1)vec_view)
                            ang(atan(cadr vec)(car vec))
                            ang(+ ang(* 0.9 pi))
                            vec1(list(cos ang)(sin ang)0.)
                            vec1(trans-x vec1 vec_view(list 0 0 1))
                            ang(+ ang(* 0.2 pi))
                            vec2(list(cos ang)(sin ang)0.)
                            vec2(trans-x vec2 vec_view(list 0 0 1))
                            )
                      (mapcar
                       '(lambda(v)
                          (grdraw p_close(mapcar '(lambda(a b)(+ a(* height_text 3. b)))p_close v) 3))
                       (list vec1 vec2))
                      ))
                )
              ls_p_road p_roadclick_temp)
           
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
            p_roadclick_temp 3)
           )

         (if(= int_inputdepth_temp 2)T
           ((lambda(p c / s ls_p p0 p1 p2)
              (setq s(* height_text 8.(if(= int_inputdepth_temp 0)1 -1))
                    p0(mapcar '+ p(list 0 0 s))
                    p1(mapcar '(lambda(a x y)(+ a(*  0.1 s x)(* -0.3 s y)))
                              p0 vec_x_onview vec_y_onview)
                    p2(mapcar '(lambda(a x y)(+ a(* -0.1 s x)(* -0.3 s y)))
                              p0 vec_x_onview vec_y_onview)
                    )
              (mapcar '(lambda(p0 p1)(grdraw p0 p1 c))
                      (list p p0 p0)(list p0 p1 p2))
              )
            p_roadclick_temp 4)
           )
         
         
         
         )
        ((and(null int_selectmenu)
             (or(and(= int_editdepth 0)(= int_selectmenu_ductedit int_selectmenu_ductedit_depth))
                (and(= int_editdepth 1)(= int_selectmenu_ductedit int_selectmenu_ductedit_offset))
                ))

         (setq int_selectmenu_ductedit -1
               p_ground p_roadclick_temp p_roadclick_temp nil
               p_click nil p_road nil
               int_grread 3)
         )
        ((and(null int_selectmenu)(= int_editdepth 1))
         (setq int_selectmenu_ductedit int_selectmenu_ductedit_offset
               int_selectmenu int_selectmenu_ductedit_offset
               bool_replacegrread T int_grread 2
               elem_grread(caar(nth int_selectmenu_ductedit ls_guidemenu))
               )
         )
        (T
         (setq int_selectmenu_ductedit nil
               p_roadclick_temp nil
               int_grread 5)

         )
        )
       
       ))

    (cons
     "CLICK"
     (lambda( / p_road  ls_arc bool vnam_insertany str_type);;(d- gr3_home()
       
       (if bool_firsttime_editduct
           (setq bool_firsttime_editduct
                 (if(< bool_firsttime_editduct 1)(1+ bool_firsttime_editduct))))

       (setq bool
             ((lambda(lst / a ls_nent bool entna str_height ls_current
                          entna_arc vnam_arc ls_currentarc p00 p01 p11 p10 vnam
                          num_line ii str_entselect vnam_select str_wholetype
                          
                          )
                (cond
                 ((= int_ductmode 2) T)
                 (entna_depth nil)
                 (int_adddepth nil)
                 (bool_noeditdepth nil)
                 (p_roadclick_temp nil)
                 (p_click

                  (setq vnam_insertduct nil ls_xdata nil)
                  (if(setq set_ent(ssget p_click ls_ssget))
                      (setq entna(ssname set_ent 0)
                            str_wholetype(cdr(assoc 0(entget entna)))
                            )
                    (setq entna nil str_wholetype nil )
                    )
                  
                  (cond
                   ((= str_wholetype "INSERT")
                    (setq vnam_insertduct(vlax-ename->vla-object entna))
                    (vla-getXData vnam_insertduct "terraduct3d" 'array_Type 'array_Data )

                    (if array_data
                        (setq ls_xdata
                              (split_list 0(mapcar 'vlax-variant-value
                                                   (vlax-safearray->list array_data)))
                              str_type(cdr(assoc "terraduct3d" ls_xdata))
                              bool nil)
                      (if(setq set_ent(ssget p_click
                                             (list(cons -4 "<NOT")(cons 0 "INSERT")
                                                  (cons -4 "NOT>"))))
                          (setq entna(ssname set_ent 0)
                                bool nil vnam_insertduct nil
                                vnam_insertany(vlax-ename->vla-object entna) )
                        (setq bool nil vnam_insertduct nil
                              vnam_insertany  nil)
                        )
                      )
                    
                    )
                   
                   (T
                    
                    (setq ls_nent(nentselp p_click)
                          entna(car ls_nent)ls_nent(cdr ls_nent))
                    (if entna
                        (progn
                          (setq str_entselect(cdr(assoc 0(entget entna)))
                                vnam_select(vlax-ename->vla-object entna)
                                bool T)
                          
                          (while(and bool ls_nent)
                            (if(if(=(type(car ls_nent))'LIST)
                                   (=(type(caar ls_nent))'ENAME))
                                (setq ls_nent(car ls_nent)bool nil)
                              (setq ls_nent(cdr ls_nent)))
                            )
                          (if ls_nent T(setq ls_nent(list vnam_select)))
                          
                          ))
                    (setq bool nil)

                    
                    
                    (while lst
                      (setq ls_current(car lst)lst(cdr lst))

                      
                      (if(vl-position(cadr(assoc "OBJ" ls_current))ls_nent)
                          (cond
                           ((setq a(assoc "DEPTH" ls_current))
                            (setq entna_depth(cadr(assoc "OBJ" ls_current))
                                  vnam_depth(caddr(assoc "OBJ" ls_current))
                                  ;;int_duct(cadar ls_current)
                                  int_duct(cadr a)int_side(caddr a)
                                  vnam_line(cadr(assoc "OBJ"(assoc(list "LINE" int_duct)ls_vnam_duct)))

                                  ls_gcode(entget entna_depth)
                                  p_road(cdr(assoc 13 ls_gcode))
                                  p14(cdr(assoc 14 ls_gcode))

                                  height_temp
                                  (if(= int_inputdepth_temp 2)(caddr p14)
                                    (*(if(= int_inputdepth 1)1 -1)
                                      (-(caddr p_road)(caddr p14))))
                                  
                                  ;;vnam_road(cadr(assoc "ROAD" lst))
                                  lst nil)
                            
                            (if(= str_entselect "MTEXT")
                                (progn
                                  
                                  (setq str_height (as-numstr height_temp) )
                                  
                                  (settile_strinput
                                   'str_height nil
                                   (nth int_inputdepth_temp ls_type_inputdepth)nil )

                                  ;;(setq depth_duct(atof str_height)
                                        ;;p_road(cdr(assoc 13 ls_gcode))

                                  (setq p13
                                        (carxyz
                                         p14(if(= int_inputdepth_temp 2)
                                                (atof str_height)
                                              (+(caddr p_road)
                                                (*(if(= int_inputdepth 1)-1 1)
                                                  (atof str_height))))
                                         ))
                                  
                                  (entmod(subst(cons 14 p13)(cons 14 p14)ls_gcode))

                                  (if vnam_line
                                      ((if(= int_side 0)vla-put-startpoint vla-put-endpoint)
                                       vnam_line(vlax-3d-point p13)))
                                  
                                  (setq entna_depth nil
                                        bool_noeditdepth T
                                        bool nil )
                                  )
                              (progn
                                
                                (setq depth_duct(atof(rtos height_temp 2 6)))
                                
                                (if(and vnam_road vnam_line)
                                    (setq offset_duct
                                          ((lambda( / vec p)
                                             (setq vec(carxyz
                                                       (mapcar '-(vlax-curve-getstartpoint vnam_line)
                                                               (vlax-curve-getendpoint vnam_line))
                                                       0)
                                                   vec(unit_vector vec)
                                                   vec(trans-x(list 1 0 0)vec(list 0 0 1))
                                                   p(vlax-curve-getclosestpointto vnam_road p_road nil)
                                                   )
                                             
                                             (atof(rtos(apply '+(mapcar '*(mapcar '- p p_road)vec))2 6))
                                             ))
                                          )
                                  )
                                
                                (setq bool T )
                                (vla-put-color vnam_depth 2)
                                
                                )
                              )
                            
                            )
                           ((setq ls_currentarc(assoc "ARC" ls_current))
                            (setq entna_arc(cadr(assoc "OBJ" ls_current))
                                  vnam_arc(caddr(assoc "OBJ" ls_current))
                                  lst nil)
                            
                            (if(= str_entselect "MTEXT")
                                (progn
                                  (setq str_radius(as-numstr(cdr(assoc "RADIUS" ls_current)))
                                        p_arcprev(caadr(assoc "POSITION" ls_current)))
                                  
                                  (settile_strinput
                                   'str_radius(lambda(str)(>(atof str)0))
                                   (mix_strasc(list 32076 36335 12398 26354 12370 21322 24452))
                                   (mix_strasc(list "0" 12424 12426 22823 12365 12356 20516 12434 20837 21147 12375 12390 12367 12384 12373 12356 ))
                                   )
                                  
                                  (setq rr(atof str_radius)
                                        bool nil
                                        bool_noeditdepth T
                                        ls_vnam_duct
                                        (subst(list ls_currentarc
                                                    (assoc "OBJ" ls_current)
                                                    (cons "RADIUS" rr)
                                                    (cons "POSITION" nil)
                                                    )
                                              ls_current ls_vnam_duct)
                                        
                                        )

                                  (if(=(caddr ls_currentarc)0)T
                                    ((lambda(lst)
                                       (setq ls_vnam_duct
                                             (subst(subst(cons "RADIUS" rr)(assoc "RADIUS" lst)lst)
                                                   lst ls_vnam_duct)
                                             )
                                       )
                                     (assoc(list "ARC"(cadr ls_currentarc)0)ls_vnam_duct)
                                     ))
                                  
                                  )
                              
                              (if(cdr(assoc "POSITION" ls_current))
                                  (progn
                                    
                                    (setq bool T )
                                    (vla-put-color vnam_arc 2)
                                    
                                    (setq ii(cadr ls_currentarc)
                                          vnam(cadr(assoc "OBJ"(assoc(list "LINE" ii)ls_vnam_duct)))
                                          p00(vlax-curve-getstartpoint vnam)
                                          p01(vlax-curve-getendpoint vnam)
                                          vnam(cadr(assoc "OBJ"(assoc(list "LINE"(1+ ii))ls_vnam_duct)))
                                          p10(vlax-curve-getstartpoint vnam)
                                          p11(vlax-curve-getendpoint vnam)

                                          ls_arcmove
                                          (cdr(connect_twist_curve
                                               (list nil p00 p01 p10 p11
                                                     (cdr(assoc "RADIUS" ls_current))length_arccenter)))

                                          ls_currentarc0(assoc(list "ARC" ii 0)ls_vnam_duct)
                                          ls_currentarc1(assoc(list "ARC" ii 1)ls_vnam_duct)
                                          str_edit "insertarc")

                                    )
                                (progn;;ここはとおらん
                                  (x-alert
                                   (list 20870 24359 12434 25375 12416 "2" 30452 32218 12364
                                         21516 19968 24179 38754 20869 12395 12394 12356 12392 12365 12399
                                         20870 24359 12398 20301 32622 12399 19968 24847 12395
                                         27770 23450 12373 12428 12414 12377
                                         "\n" 31227 21205 12391 12365 12414 12379 12435 ))
                                  ;;円弧を挟む2直線が同一平面内にないときは円弧の位置は一意に決定されます
                                  ;;移動できません
                                  
                                  (setq bool T)
                                  )
                                )
                              )

                            )
                           
                           ((setq num_line(cadr(assoc "LINE" ls_current)))
                            
                            (mapcar
                             '(lambda(lst)
                                (setq ls_vnam_duct(vl-remove lst ls_vnam_duct)
                                      lst(cdr(assoc "OBJ" lst))
                                      )
                                (mapcar
                                 '(lambda(vnam)
                                    (if(=(type vnam)'VLA-OBJECT)
                                        (progn
                                          (vla-delete vnam)
                                          (exckillobj vnam)
                                          ))
                                    )
                                 lst)
                                )
                             (list ls_current
                                   (assoc(list "DEPTH" num_line 0)ls_vnam_duct)
                                   (assoc(list "DEPTH" num_line 1)ls_vnam_duct)
                                   (assoc(list "ARC" num_line 0)ls_vnam_duct)
                                   (assoc(list "ARC" num_line 1)ls_vnam_duct))
                             )
                            
                            (while(setq lst(assoc(list "LINE"(setq num_line(1+ num_line)))
                                                 ls_vnam_duct))
                              (setq ls_vnam_duct
                                    (subst(subst(list "LINE"(1- num_line))
                                                (list "LINE" num_line)
                                                lst)
                                          lst ls_vnam_duct))
                              (mapcar
                               '(lambda(str i / lst)
                                  (if(setq lst(assoc(list str num_line i)ls_vnam_duct))
                                      (setq ls_vnam_duct
                                            (subst(subst(list str(1- num_line)i)
                                                        (list str num_line i)
                                                        lst)
                                                  lst ls_vnam_duct)))
                                  )
                               (list "DEPTH" "DEPTH" "ARC" "ARC")
                               (list 0 1 0 1))
                              )
                            
                            (setq int_max_duct(1- int_max_duct))
                            
                            (setq int_adddepth nil bool_noeditdepth T
                                  bool_replacegrread T int_grread 3 elem_grread(list 0 0 0)
                                  )
                            
                            (setq bool T)
                            )
                           )
                        )
                      )
                    
                    )
                   )
                  
                  
                  bool
                  
                  )
                 )
                )
              ls_vnam_duct)
             )

       (cond
        (bool )
        ((if int_adddepth
             (if(<(abs depth_duct)1e-8)
                 (progn;;深度の絶対値を0より大きくしてください
                   (x-alert(list 28145 24230 12398 32118 23550 20516 12434 "0" 12424 12426 22823 12365 12367 12375 12390 12367 12384 12373 12356 ))
                   T )
               (if(?(abs depth_duct)(* 2. radius_bend_temp))
                   (progn;;深度の絶対値を曲げ半径の2倍より小さくしてください
                     (x-alert(list 28145 24230 12398 32118 23550 20516 12434 26354 12370 21322 24452 12398 "2" 20493 12424 12426 23567 12373 12367 12375 12390 12367 12384 12373 12356 ))
                     T )
                 )))
         )
        ((if int_adddepth
             (if(setq set_ent(ssget "CP"(mapcar '(lambda(v / vec)
                                                   (setq vec(mapcar
                                                             '(lambda(x y)(* 0.4 height_text
                                                                             (+(*(car v)x)(*(cadr v)y))))
                                                             vec_x_onview vec_y_onview))
                                                   (mapcar '+ elem_grread vec)
                                                   )
                                                (list(list 1 1)(list 1 -1)(list -1 -1)(list -1 1))
                                                )
                                    (list(cons 0 "LINE"))))
                 (progn
                   (setq vnam_line(vlax-ename->vla-object(ssname set_ent 0)))
                   (null(apply 'or(mapcar
                                   '(lambda(lst / num)
                                      (if(setq num(vl-position(list "OBJ" vnam_line)lst))
                                          (setq num_line(cadr(assoc "LINE" lst))))
                                      )
                                   ls_vnam_duct)))
                   )
               T)
           )
         ;;選択対象ではありません/n管路の直線部を選択してください
         (x-alert(list 36984 25246 23550 35937 12391 12399 12354 12426 12414 12379 12435 "/n" 31649 36335 12398 30452 32218 37096 12434 36984 25246 12375 12390 12367 12384 12373 12356 ))
         )
        
        ((and int_adddepth num_line)
         
         (setq ps(vlax-curve-getstartpoint vnam_line)
               pe(vlax-curve-getendpoint vnam_line)

               ls_p0(mapcar '(lambda(d1 d2 b / p)
                               (setq p(mapcar '(lambda(a b)(+(* d1 a)(* d2 b)))ps pe))
                               (if b(mapcar '+ p(list 0 0 (- depth_duct)))p))
                            (list 0.7 0.6 0.4 0.3 0)(list 0.3 0.4 0.6 0.7 1.)(list nil T T nil nil))
               
               ls_p1(project_to_ground
                     ls_p0(list 0 0 1)(list str_lasground height_ground))

               entna(cadr(assoc "OBJ"(assoc(list "DEPTH" num_line 1)ls_vnam_duct)))
               vec_normal(cdr(assoc 210(entget entna)))
               
               ls_entna
               (mapcar '(lambda(p0 p1 / d str e vnam)
                          (setq d(apply '+(mapcar '* vec_normal p0))
                                str(strcat "GL"(if(<(-(caddr p0)(caddr p1))0)"-" "+")"<>\\P"
                                           "EL = "(rtos(caddr p0)2 int_unitelevation))
                                )
                          (setq e(make_2pdimension
                                  entna(list p1 p0 vec_normal d
                                             nil(* 0.5 pi) 0. str str_dimstyle_ductlevel)))
                          (if entna
                              (setq entna nil)
                            (progn
                              (setq vnam(vlax-ename->vla-object e))
                              (vla-put-color vnam int_colductdimedit)
                              (addkillobj vnam)
                              (list p0 e vnam)
                              )
                            )
                          )
                       ls_p0 ls_p1)
               ls_entna(cdr ls_entna)
               vnam(cadr(assoc "OBJ"(assoc(list "LINE" num_line)ls_vnam_duct)))
               
               )
         (vla-put-endpoint vnam(vlax-3d-point(car ls_p0)))

         (setq ls_vnam_duct
               (mapcar
                '(lambda(lst / str ls_num)
                   (setq ls_num(car lst)str(car ls_num))
                   (if(if(vl-position str(list "LINE" "DEPTH" "ARC"))
                          (>(cadr ls_num)num_line))
                       (subst(mapcar '(lambda(a b)(if b(+ 2 a)a))
                                     ls_num(list nil T nil))
                             ls_num lst)
                     lst)
                   )
                ls_vnam_duct)
               )

         (setq ps nil)
         (mapcar
          '(lambda(lst / p i)
             (setq p(car lst)lst(cdr lst))
             (if ps
                 (progn
                   (setq vnam_line
                         (vla-addline
                          (vla-get-modelspace(vla-get-activedocument(vlax-get-acad-object)))
                          (vlax-3d-point ps)(vlax-3d-point p))
                         ls_vnam_duct(cons(list(list "LINE" num_line)
                                               (list "OBJ" vnam_line))
                                          ls_vnam_duct)
                         ps nil i 1
                         )
                   (vla-put-color vnam_line int_colductdimedit)
                   (addkillobj vnam_line)
                   )
               (setq num_line(1+ num_line)ps p i 0)
               )
             
             (setq ls_vnam_duct
                   (cons(list(list "DEPTH" num_line i)
                             (cons "OBJ" lst)
                             (list "ROAD" vnam_road hand_road) )
                        ls_vnam_duct)
                   )
             )
          ls_entna)
         
         
         (setq int_adddepth nil bool_noeditdepth T int_max_duct(+ int_max_duct 2)
               int_editarcposition_temp 1
               bool_replacegrread T int_grread 3 elem_grread(list 0 0 0)
               )
         
         )
        
        ((if(and(vl-position int_ductdepth(list 0 1))bool_ductedit
                (< int_editdepth 2)
                (null vnam_insertany)(null vnam_insertduct))
             (if(or(= int_selectmenu_ductedit -1)entna_depth
                   bool_noeditdepth)
                 ;;(= str_editreturn "insertarc"))
                 nil
               (if p_roadclick_temp T
                 (setq p_roadclick_temp p_ground
                       bool_replacegrread T int_grread 2
                       int_selectmenu_ductedit int_selectmenu_ductedit_depth
                       elem_grread(caar(nth int_selectmenu_ductedit ls_guidemenu))
                       ))
               )
           )
         
         )
        
        ((and bool_ductedit
              ;;(vl-position int_ductdepth(list 0 1));;切りたい
              (null vnam_insertany)(null vnam_insertduct))
         (if int_selectmenu_ductedit(setq int_selectmenu_ductedit nil))
         (if(vl-position str_editreturn(list "insertarc" ))
             (setq str_editreturn "home"))
         
         (setq vec_normal(unit_vector(carxyz(if vec_road(mapcar '- vec_road)vec_view)0.))
               vec_x_offset(trans-x(list 1. 0. 0.)vec_normal(list 0 0 1)))
         
         (if bool_noeditdepth(setq bool_noeditdepth nil)
           (progn
             (if p_road T
               (setq p_road(mapcar '(lambda(a b)(+ a(* b offset_duct)))p_ground vec_x_offset)))
             
             (setq dist_normal(apply '+(mapcar '* vec_normal p_road))
                   p_depth(if(= int_inputdepth_temp 0)(mapcar '+ p_road(list 0. 0. depth_duct))
                            (if(= int_inputdepth_temp 1)(mapcar '- p_road(list 0. 0. depth_duct))
                              (if(= int_inputdepth_temp 2)(carxyz p_road depth_duct)
                                )))
                   )
             
             (setq str_level(strcat "GL"(if(<(-(caddr p_depth)(caddr p_road))0)"-" "+")"<>\\P"
                                    "EL = "(rtos(caddr p_depth)2 int_unitelevation))
                   
                   )
             (setq entna_depth(make_2pdimension
                               entna_depth(list p_road p_depth vec_normal dist_normal
                                                nil(* 0.5 pi) 0. str_level str_dimstyle_ductlevel))
                   )
             
             (if(vl-position entna_depth(mapcar '(lambda(a)(cadr(assoc "OBJ" a)))ls_vnam_duct)) T
               (progn
                 (setq int_ductend(if(= int_connecttype 0)int_min_duct int_max_duct)
                       int_searchnumber(if(= int_connecttype 0)0 1))
                 
                 ((lambda( / bool ii jj)
                    (setq bool T ii int_ductend int_duct ii)
                    (while bool
                      (if(assoc(list "DEPTH" ii(+ 0 int_searchnumber))ls_vnam_duct)
                          (if(assoc(list "DEPTH" ii(- 1 int_searchnumber))ls_vnam_duct)
                              (setq ii(+ ii(if(= int_connecttype 0)1 -1)))
                            (setq jj 1 bool nil))
                        (setq jj 0 bool nil))
                      )
                    (setq int_duct ii int_ductdepth jj)
                    ))
                 (if(= int_connecttype 0)
                     (setq int_max_duct(max int_duct int_max_duct))
                   (setq int_min_duct(min int_min_duct int_duct)))
                 
                 (setq vnam_depth(vlax-ename->vla-object entna_depth)
                       ls_vnam_duct(cons(list(list "DEPTH" int_duct int_ductdepth)
                                             (list "OBJ" entna_depth vnam_depth)
                                             (list "ROAD" vnam_road hand_road))
                                        ls_vnam_duct)
                       )
                 
                 (addkillobj vnam_depth)
                 ))
             
             (setq entna_depth nil)
             (vla-put-color vnam_depth int_colductdimedit)
             (if(and(setq entna0(cadr(assoc "OBJ"(assoc(list "DEPTH" int_duct 0)ls_vnam_duct))))
                    (setq entna1(cadr(assoc "OBJ"(assoc(list "DEPTH" int_duct 1)ls_vnam_duct)))))
                 (progn
                   (setq p0(vlax-3d-point(cdr(assoc 14(entget entna0))))
                         p1(vlax-3d-point(cdr(assoc 14(entget entna1)))))
                   (if(setq vnam_line(cadr(assoc "OBJ"(assoc(list "LINE" int_duct)ls_vnam_duct))))
                       (mapcar '(lambda(func p)(func vnam_line p))
                               (list vla-put-startpoint vla-put-endpoint)(list p0 p1))
                     (progn
                       (setq vnam_line
                             (vla-addline
                              (vla-get-modelspace(vla-get-activedocument(vlax-get-acad-object)))
                              p0 p1)
                             ls_vnam_duct(cons(list(list "LINE" int_duct)
                                                   (list "OBJ" vnam_line))
                                              ls_vnam_duct)
                             )
                       (vla-put-color vnam_line int_colductdimedit)
                       (addkillobj vnam_line)
                       )
                     )
                   )
               )
             ))
         
         (setq num_duct(- int_min_duct 2)
               ls_pmesh(list) ls_pbar(list) ls_center(list)ls_vec(list)
               pitch_mesh pitch_project
               p00 nil p01 nil p10 nil p11 nil vec0 nil vec1 nil
               ls_arc nil
               )
         
         (if(<(abs pitch_mesh)1e-8)(setq pitch_mesh 0.5))

         (while(<=(setq num_duct(1+ num_duct))int_max_duct)
           (setq vnam_line0(cadr(assoc "OBJ"(assoc(list "LINE" num_duct)ls_vnam_duct)))
                 vnam_line1(cadr(assoc "OBJ"(assoc(list "LINE"(1+ num_duct))ls_vnam_duct))))
           
           (if vnam_line0(setq p00 p10 p01 p11))
           (if vnam_line1
               (setq p10(vlax-curve-getstartpoint vnam_line1)
                     p11(vlax-curve-getendpoint vnam_line1)
                     ))
           (cond
            ((and vnam_line0 vnam_line1))
            (vnam_line0;;初回限定
             (setq lst(car ls_pbar)p(car lst))
             (if(<(apply '+(mapcar '*(mapcar '- p01 p)(caddr lst)))-1e-8)
                 T
               (progn
                 (setq vec0(unit_vector(mapcar '- p01 p00)));;これでいいとは思うがp01-pでもいい
                 ;; (if(= int_protectcon_temp 2)T
                 ((lambda( / d n i dd pm)
                    (setq d(distance p p01) n(1+(fix(/ d pitch_mesh))) d(/ d n)
                          dd(- d))
                    (if(< d 1e-8)(setq ls_pmesh(cons(list p vec0)ls_pmesh))
                      (while(>=(setq n(1- n))-1)
                        (setq dd(+ dd d)
                              pm(mapcar '(lambda(a b)(+ a(* dd b)))p vec0)
                              ls_pmesh(cons(list pm vec0)ls_pmesh)
                              )
                        )
                      )
                    ))
                 ;;)
                 
                 (setq ls_pbar(cons(list p01 nil vec0) ls_pbar))
                 ))
             
             )
            (vnam_line1;;最終回限定
             (setq ls_pbar(cons(list p10 nil(unit_vector(mapcar '- p11 p10)))
                               ls_pbar))
             )
            )
           

           ;; (if(if(and vnam_line0 vnam_line1 )
           ;;        (if(or(= num_duct int_duct)(=(1+ num_duct)int_duct))
           ;;            (progn
                        
           ;;              ;;todo不要か
           ;;              nil)
           ;;          T)
           ;;      T)
           ;;     (progn
           ;;       T
           ;;       )


           (if(and vnam_line0 vnam_line1 )
               (progn
                 
                 (if(setq ls_currentarc0(assoc(list "ARC" num_duct 0)ls_vnam_duct))T
                   (setq ls_currentarc0(list(list "ARC" num_duct 0)(list "OBJ" )
                                            (cons "RADIUS" radius_bend_temp)
                                            (cons "POSITION" nil) )
                         ))
                 (if(setq ls_currentarc1(assoc(list "ARC" num_duct 1)ls_vnam_duct))T
                   (setq ls_currentarc1(list(list "ARC" num_duct 1)(list "OBJ" )
                                            (cons "RADIUS" radius_bend_temp)
                                            (cons "POSITION" nil))
                         ls_vnam_duct(cons ls_currentarc1(cons ls_currentarc0 ls_vnam_duct))
                         ))
                 
                 (setq rr(cdr(assoc "RADIUS" ls_currentarc0)))
                 
                 (setq ls_arc(connect_twist_curve ;;(list vec_normal pc p0 p1 p2 length_straight)
                              (list p00 p01 p10 p11 rr length_arccenter
                                    ((lambda( / lst px0 px1 vec)
                                       (setq lst(cdr(assoc "POSITION" ls_currentarc0)))
                                       ;;(setq px0(cadar lst)px1(caddr(car lst)))
                                       ;; (if(if(and px0 px1)
                                       ;;        (and
                                       ;;         (<(distance(list 0 0 0)
                                       ;;                    (cross_product
                                       ;;                     (unit_vector(mapcar '- p01 p00))
                                       ;;                     (unit_vector(mapcar '- px0 p00))))1e-8)
                                       ;;         (<(distance(list 0 0 0)
                                       ;;                    (cross_product
                                       ;;                     (unit_vector(mapcar '- p11 p10))
                                       ;;                     (unit_vector(mapcar '- px1 p10))))1e-8)
                                       ;;         ))
                                       ;;     lst
                                       ;;   (progn
                                       ;;     (setq int_editarcposition_temp 1)
                                       ;;     nil
                                       ;;     )
                                       ;;)
                                       lst
                                       
                                       ))
                                    (strcat(itoa num_duct)"-"(itoa(1+ num_duct))":")))
                       )

                 (if(=(car ls_arc)"BREAK")
                     (if(cdr ls_arc)
                         (setq num_duct(1+ int_max_duct)
                               bool_arcfirsttime
                               (or(/= int_editarcposition 0)
                                  int_editarcposition_temp)
                               
                               ls_arcmove(cdr ls_arc)
                               str_edit "insertarc")
                       (setq num_duct(1+ int_max_duct)
                             
                             ;; bool_replacegrread T
                             ;; int_grread 2 elem_grread 8
                             )
                       )
                   
                   (mapcar
                    '(lambda(ls_current lst / e vnam vec_normal pc p0 p1 p2 length_straight e_out)
                       (mapcar 'set '(pc p0 p1 p2 vec_normal length_straight)lst)
                       
                       (setq e(cadr(assoc "OBJ" ls_current))
                             dist_normal(apply '+(mapcar '* pc vec_normal))
                             e_out
                             (make_arcdimension
                              e(list pc p0 p1 p2 rr vec_normal dist_normal
                                     (strcat
                                      "R="(if(<(distance p1 p2)1e-8)(chr 8734)(as-numstr rr)))
                                     str_dimstyle_ductlevel))
                             )
                       
                       
                       
                       (if e
                           (setq ls_vnam_duct
                                 (subst(list(assoc "ARC" ls_current)
                                            (assoc "OBJ" ls_current)
                                            (cons "RADIUS" rr)
                                            (assoc "POSITION" ls_current)
                                            )
                                       ls_current ls_vnam_duct)
                                 vnam(caddr(assoc "OBJ" ls_current))
                                 )
                         (progn
                           (setq vnam(vlax-ename->vla-object e_out)
                                 ls_vnam_duct
                                 (subst(list(assoc "ARC" ls_current)
                                            (list "OBJ" e_out vnam)
                                            (cons "RADIUS" rr)
                                            
                                            (assoc "POSITION" ls_current)
                                            )
                                       ls_current ls_vnam_duct)
                                 )
                           (vla-put-Arrowhead1Type vnam 19)
                           (vla-put-Arrowhead2Type vnam 19)
                           (vla-put-ExtLine1Suppress vnam -1)
                           (vla-put-ExtLine2Suppress vnam -1)
                           (vla-put-HorizontalTextPosition vnam 3)
                           (vla-put-TextInsideAlign vnam 0)
                           (vla-put-VerticalTextPosition vnam 2)
                           (addkillobj vnam)
                           ))
                       (vla-put-color vnam int_colductdimedit)

                       e_out)
                    (list ls_currentarc0 ls_currentarc1)ls_arc)
                   )
                 
                 ))
           
           
           (mapcar
            '(lambda(i)
               (if(if(setq entna(cadr(assoc "OBJ"(assoc(list "ARC" num_duct i)ls_vnam_duct))))
                      (progn
                        (setq p(caar ls_pbar)
                              ls_gcode(entget entna) p15(cdr(assoc 15 ls_gcode))
                              p13(cdr(assoc 13 ls_gcode))p14(cdr(assoc 14 ls_gcode))
                              )
                        (>(distance p13 p14)1e-8)
                        ))
                   (progn
                     (setq vec_normal(cdr(assoc 210 ls_gcode))
                           vec13(mapcar '- p13 p15)rr(distance p15 p13)
                           vec14(mapcar '- p14 p15)
                           vec13(mapcar '(lambda(a)(/ a rr))vec13)
                           vec14(mapcar '(lambda(a)(/ a rr))vec14)
                           vec0(cross_product vec_normal vec13)
                           vec1(cross_product vec_normal vec14)
                           )
                     (if(<(apply '+(mapcar '* vec0 vec14))0)(setq vec0(mapcar '- vec0)))
                     (if(>(apply '+(mapcar '* vec1 vec13))0)(setq vec1(mapcar '- vec1)))
                     (if(<(apply '+(mapcar '*(mapcar '- p13 p)vec0))-1e-8)
                         (setq ls_pbar(cons(list p13 nil vec0)(cdr ls_pbar)))
                       ;; (if(= int_protectcon_temp 2)T
                       ((lambda(p / d n i dd pm)
                          (setq d(distance p p13) n(1+(fix(/ d pitch_mesh))) d(/ d n)
                                dd(- d))
                          (while(>(setq n(1- n))-1)
                            (setq dd(+ dd d)
                                  pm(mapcar '(lambda(a b)(+ a(* dd b)))p vec0)
                                  ls_pmesh(cons(list pm vec0)ls_pmesh)
                                  )
                            )
                          )
                        p)
                       ;; )
                       )
                     
                     (setq ls_pbar(cons(list p13 p15 vec0)ls_pbar)
                           ls_pbar(cons(list p14 nil vec1)ls_pbar)
                           )
                     
                     ;; (if(= int_protectcon_temp 2)T
                     ((lambda( / d n i ang r vec_x vec_y pm dd c s v)
                        (setq ang(atan(distance(cross_product vec13 vec14)(list 0 0 0))
                                      (apply '+(mapcar '* vec13 vec14)))
                              vec_x(mapcar '(lambda(a)(* a rr))vec13)
                              vec_y(mapcar '(lambda(a)(* a rr))vec0)
                              d(* rr ang) n(1+(fix(/ d pitch_mesh))) d(/ ang n)
                              ang 0)
                        
                        (while(>(setq n(1- n))0)
                          (setq ang(+ ang d)c(cos ang)s(sin ang)
                                v(mapcar '(lambda(x y)(+ (* -1 s x)(* c y)))vec13 vec0)
                                pm(mapcar '(lambda(a x y)(+ a(* c x)(* s y)))p15 vec_x vec_y)
                                ls_pmesh(cons(list pm v)ls_pmesh)
                                )
                          )
                        ))
                     ;; )
                     
                     
                     
                     ))
               )
            (if(=(car ls_arc)"BREAK")nil(list 0 1)))
           
           );;ARC
         (if(= str_edit "insertarc")T(setq int_editarcposition_temp nil))
         
         (if(if ls_arc(=(car ls_arc)"BREAK"))nil
           (progn

             (setq ls_pcenterline(mapcar 'car(reverse ls_pmesh)))
             
             (if(setq lst(assoc(list "SOLID" 0)ls_vnam_duct))
                 (progn
                   (setq vnam(cadr(assoc "OBJ" lst))
                         ls_vnam_duct(vl-remove lst ls_vnam_duct))
                   (vla-delete vnam)
                   (exckillobj vnam)
                   ))
             
             (if(and(= int_ductsolidmesh 1)ls_pbar)
                 (progn
                   (setq ls_pbar(reverse ls_pbar)
                         entna(bendpipe_sld
                               (* 0.5 diam_duct_temp)
                               (mapcar 'car ls_pbar)(mapcar 'cadr ls_pbar)(mapcar 'caddr ls_pbar)
                               (getvar "CLAYER") nil)
                         vnam(vlax-ename->vla-object entna)
                         ls_vnam_duct(cons(list(list "SOLID" 0)(list "OBJ" vnam)
                                               (cons "DIAM" diam_duct_temp) )
                                          ls_vnam_duct)
                         
                         )
                   (vla-put-color vnam int_colduct_temp)
                   (if(= int_hidesolid 1)(vla-put-visible vnam :vlax-false))
                   
                   (addkillobj vnam)
                   
                   )
               (if ls_pmesh
                   (progn
                     
                     (setq numy 32
                           ls_intmesh(inclist 0 numy)
                           ang_d(/(* 2. pi)numy)
                           r(* 0.5 diam_duct_temp)
                           vecxp nil vecyp nil
                           )
                     
                     (setq ls_pduct
                           (mapcar
                            '(lambda(lst / p v vecx vecy)
                               (setq p(car lst)v(cadr lst)
                                     v(unit_vector(carxyz v 0.))
                                     vecx(trans-x(list 1 0 0)v(list 0 0 1))
                                     vecy(trans-x(list 0 1 0)v(list 0 0 1))
                                     )

                               (if(if vecxp(>(apply '+(mapcar '* vecx vecxp))0)T)T
                                 (setq vecx vecxp))
                               (setq vecxp vecx)
                               (if(<(caddr vecy)0)(setq vecy(mapcar '- vecy)))
                               (mapcar '(lambda(i / c s)
                                          (setq c(cos(* ang_d i))s(sin(* ang_d i)))
                                          (mapcar '(lambda(a x y)(+(* r c x)(* r s y)a))
                                                  p vecx vecy))
                                       ls_intmesh)
                               )
                            (reverse ls_pmesh)
                            )
                           
                           numx(length ls_pduct)
                           ls_p(apply 'append(apply 'append ls_pduct))
                           )
                     
                     (setq array_mesh(vlax-make-safearray vlax-vbDouble(cons 0(1-(length ls_p)))))
                     (vlax-safearray-fill array_mesh ls_p)
                     (setq vnam
                           (vla-Add3DMesh
                            (vla-get-ModelSpace(vla-get-ActiveDocument (vlax-get-acad-object)))
                            numx numy array_mesh)

                           ls_vnam_duct(cons(list(list "SOLID" 0)(list "OBJ" vnam)
                                                 (cons "DIAM" diam_duct_temp) )
                                            ls_vnam_duct)
                           
                           )
                     (vla-put-color vnam int_colduct_temp)
                     (vla-put-nclose vnam :vlax-true)
                     (if(= int_hidesolid 1)(vla-put-visible vnam :vlax-false))
                     
                     (addkillobj vnam)

                     )
                 )
               )
             
             (if(setq lst(assoc(list "MESH" 0)ls_vnam_duct))
                 (progn
                   (setq vnam(cadr(assoc "OBJ" lst))
                         ls_vnam_duct(vl-remove lst ls_vnam_duct))
                   (vla-delete vnam)
                   (exckillobj vnam)
                   ))

             
             (if(if(= int_protectcon_temp 2)nil ls_pmesh)
                 (progn
                   (setq numy(if(= int_protectcon_temp 0)
                                 (if(= filet_protect_temp 0.)4 8)
                               (if(= int_protectcon_temp 1)
                                   (if(= filet_protect_temp 0.)-4 -6)
                                 ))
                         int_corner numy
                         
                         w(* 0.5 width_protect_temp)
                         h(* 0.5 height_protect_temp)
                         f filet_protect_temp
                         
                         ls_pmesh
                         (mapcar
                          '(lambda(lst / p v vecx vecy)
                             (setq p(car lst)v(cadr lst)
                                   v(unit_vector(carxyz v 0.))
                                   vecx(trans-x(list 1 0 0)v(list 0 0 1))
                                   vecy(trans-x(list 0 1 0)v(list 0 0 1))
                                   )
                             
                             (vl-remove
                              nil
                              (mapcar '(lambda(lst / bool iw iwf ih ihf)
                                         (mapcar 'set '(bool iw iwf ih ihf)lst)
                                         (if(vl-position numy bool)
                                             (mapcar '(lambda(a x y)(+(*(+(* iw w)(* iwf f))x)
                                                                      (*(+(* ih h)(* ihf f))y) a))
                                                     p vecx vecy))
                                         )
                                      (list(list(list 8 4)-1  1 1 0)
                                           (list(list 8)  -1  0 1 -1)
                                           (list(list -6 -4)-1 0 0 0)
                                           (list(list 8 4 -6 -4)-1 0 -1 1)
                                           (list(list 8 -6)-1  1 -1 0)
                                           (list(list 8 -6) 1 -1 -1 0)
                                           (list(list 8 4 -6 -4) 1 0 -1 1)
                                           (list(list -6 -4) 1 0 0 0)
                                           (list(list 8)   1  0 1 -1)
                                           (list(list 8 4) 1 -1 1 0)
                                           ))
                              )
                             )
                          (reverse ls_pmesh)
                          )
                         
                         numx(length ls_pmesh)
                         numy(abs numy)
                         ls_p(apply 'append(apply 'append ls_pmesh))
                         )
                   
                   (setq array_mesh(vlax-make-safearray vlax-vbDouble(cons 0(1-(length ls_p)))))
                   (vlax-safearray-fill array_mesh ls_p)
                   (setq vnam
                         (vla-Add3DMesh
                          (vla-get-ModelSpace(vla-get-ActiveDocument (vlax-get-acad-object)))
                          numx numy array_mesh)
                         ls_vnam_duct(cons(list(list "MESH" 0)(list "OBJ" vnam)
                                               (cons "CORNER" int_corner)
                                               (cons "WIDTH" width_protect_temp)
                                               (cons "HEIGHT" height_protect_temp)
                                               (cons "FILET"  filet_protect_temp)
                                               )
                                          ls_vnam_duct)
                         )

                   (vla-put-layer vnam str_layprotect)
                   (vla-put-color vnam int_colprotect)
                   (vla-put-nclose vnam :vlax-true)
                   (if(= int_hidesolid 1)(vla-put-visible vnam :vlax-false))
                   
                   (addkillobj vnam)
                   
                   ))
             ))
         
         
         (if(= int_editstatus 2)
             (setq bool_replacegrread T int_grread 2 elem_grread 13))

         )

        ((and(null bool_ductedit)vnam_insertduct(= str_type "DUCTBLOCK"))
         
         (setq str(vla-get-name vnam_insertduct)
               vnam(vla-Item(vla-get-Blocks (vla-get-ActiveDocument(vlax-get-acad-object)))str)
               ls_search(list)num_min nil num_max nil
               int_protectcon_temp 2
               int_editstatus(if(and(= int_ductmode 1)(= depth_duct 0.)(= offset_duct 0.))1
                               (if(= int_ductmode 1)2 0))
               )
         (if(= int_editstatus 2)
             (progn
               (setq str "DUCT$" i 0 bool T)
               (while bool
                 (setq i(1+ i)
                       str_ductname(strcat str(substr(itoa(+ 1000 i))2))
                       bool
                       (null
                        (vl-catch-all-error-p
                         (vl-catch-all-apply 'vla-Item(list vnam_blocktable str_ductname))))
                       )
                 )
               )
           (setq str_ductname str))
         

         (vlax-for
          obj vnam
          (vla-getXData obj "terraduct3d" 'array_Type 'array_Data )
          (setq ls_xdata
                (if array_data
                    (split_list 0(mapcar 'vlax-variant-value
                                         (vlax-safearray->list array_data))))
                str_type(cdr(assoc "terraduct3d" ls_xdata))
                )
          (cond
           ((or(= str_type "DEPTH")(= str_type "ARC"))
            (setq num(cdr(assoc "NUM" ls_xdata))
                  int_side(cdr(assoc "SIDE" ls_xdata))
                  )
            (if num_min(setq num_min(min num_min num)num_max(max num_max num))
              (setq num_min num num_max num))
            (setq lst(cons(list str_type num int_side)
                          (cons(cons "OBJ" obj)
                               ls_xdata))
                  ls_search(cons lst ls_search))
            )
           ((= int_editstatus 1))
           
           ((= str_type "DUCTSOLID")
            (setq diam_duct_temp(cdr(assoc "DIAM" ls_xdata))
                  int_colduct_temp(vla-get-color obj)
                  )
            )
           ((= str_type "CONMESH")
            (setq int_protectcon_temp
                  (if(<(cdr(assoc "CORNER" ls_xdata))0)1 0)
                  width_protect_temp(cdr(assoc "WIDTH" ls_xdata))
                  height_protect_temp(cdr(assoc "HEIGHT" ls_xdata))
                  filet_protect_temp(cdr(assoc "FILET" ls_xdata))
                  )
            )
           )
          )

         (setq int_min_duct 0 int_max_duct(- num_max num_min)
               num_min(1- num_min) ls_vnam_duct(list)
               vnam_currentinsert vnam_insertduct
               vnam_insertduct nil vnam_road nil
               ls_line(list)
               ls_roadfrominsert(list)
               num_temp num_min
               )

         (while(<=(setq num_temp(1+ num_temp))num_max)
           (setq ls_p
                 (mapcar
                  '(lambda(int_side / ls_gcode)
                     (setq lst(assoc(list "DEPTH" num_temp int_side)ls_search)
                           ls_search(vl-remove lst ls_search)
                           vnam_road
                           ((lambda(hand_road / entna_road vnam str int_col)
                              (if(setq vnam
                                       ((lambda(h / block v)
                                          (if h T(setq h ""))
                                          (if(vl-catch-all-error-p
                                              (setq block(vl-catch-all-apply 'vla-Item(list vnam_blocktable h))))
                                              nil
                                            (vlax-for
                                             obj block
                                             (if entna_road T
                                               (if(=(vla-get-ObjectName obj)"AcDb3dPolyline")
                                                   (setq entna_road(vlax-vla-object->ename obj)
                                                         v obj
                                                         ))
                                               )
                                             )
                                            )
                                          (if entna_road T
                                            (if(setq entna_road(handent h))
                                                (setq v(vlax-ename->vla-object entna_road))))
                                          v
                                          )
                                        hand_road)
                                       )
                                  (if(progn
                                       (setq str(cdr(assoc 2(entget(cdr(assoc 330(entget entna_road)))))))
                                       (vl-position str(list "*Model_Space" "*Paper_Space"))
                                       )
                                      T
                                    (if(vl-position vnam ls_roadfrominsert)T
                                      (progn
                                        
                                        (setq ls_roadfrominsert(cons vnam ls_roadfrominsert)
                                              int_col(get_visual_color vnam)
                                              vnam
                                              (vla-CopyObjects
                                               (vla-get-ActiveDocument(vlax-get-acad-object))
                                               (vlax-make-variant
                                                (vlax-safearray-fill
                                                 (vlax-make-safearray vlax-vbObject (cons 0 0))
                                                 (list vnam)))
                                               (vla-get-ModelSpace
                                                (vla-get-ActiveDocument(vlax-get-acad-object)))
                                               )
                                              vnam(car(vlax-safearray->list(vlax-variant-value vnam)))
                                              )
                                        (addkillobj vnam)
                                        (vla-put-color vnam int_col)
                                        ))
                                    ) )
                              vnam)
                            (cdr(assoc "ROAD" lst)))
                           
                           vnam(cdr(assoc "OBJ" lst))
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
                           entna(vlax-vla-object->ename vnam)
                           )

                     (if(or str_lasground height_ground)
                         ((lambda( / p13 p14 lst v pp13 pp14 str)
                            (setq lst(entget entna)
                                  p13(cdr(assoc 13 lst))p14(cdr(assoc 14 lst))
                                  pp13(car(project_to_ground
                                           (list p13)(list 0 0 1)(list str_lasground height_ground)))
                                  pp14(mapcar '+ pp13(mapcar '- p14 p13))
                                  str(strcat "GL"(if(<(-(caddr pp14)(caddr pp13))0)"-" "+")"<>\\P"
                                             "EL = "(rtos(caddr pp14)2 int_unitelevation))
                                  lst(subst(cons 13 pp13)(cons 13 p13)lst)
                                  lst(subst(cons 14 pp14)(cons 14 p14)lst)
                                  lst(subst(cons 1 str)(assoc 1 lst)lst)
                                  )
                            
                            (entmod lst)
                            ))
                       )
                     
                     (setq ls_vnam_duct(cons(list(list "DEPTH" num_temp int_side)
                                                 (list "OBJ" entna vnam)
                                                 (list "ROAD" vnam_road hand_road) )
                                            ls_vnam_duct)
                           )
                     
                     (vla-put-color vnam int_colductdimedit)
                     (addkillobj vnam)

                     (if(= int_editstatus 2)
                         (if(= int_side 0)
                             (setq vnam0 vnam entna0 entna)
                           (setq vnam1 vnam entna1 entna)))
                     
                     (cdr(assoc 14 (entget entna)))
                     )
                  (list 0 1))

                 p00(car ls_p)p01(cadr ls_p)
                 
                 )
           (if(= int_editstatus 2)
               (progn
                 (setq vec(unit_vector(carxyz(mapcar '- p00 p01)0))
                       vec_offset(trans-x(list 1 0 0)vec(list 0 0 1))
                      vec_offset
                       (mapcar '(lambda(x y)(+(* offset_duct x)(* depth_duct y)))
                               vec_offset(list 0 0(if(= int_inputdepth_temp 1)-1 1)))
                       p10(mapcar '+ p00 vec_offset)
                       p11(mapcar '+ p01 vec_offset)
                       ls_line(cons(list p00 p01 p10 p11)ls_line)
                       )
                 
                 (mapcar
                  '(lambda(vnam entna)
                     (vla-Move vnam(vlax-3d-point 0 0 0)(vlax-3d-point vec_offset))
                     (setq ls_gcode(entget entna))
                     (if(setq p(car(project_to_ground
                                    (list(cdr(assoc 13 ls_gcode)))(list 0 0 1)
                                    (list str_lasground height_ground))))
                         (entmod(subst(cons 13 p)(assoc 13 ls_gcode)ls_gcode)))
                     )
                  (list vnam0 vnam1)(list entna0 entna1))
                 
                 )
             (setq p10 p00 p11 p01))

           ;;todo
           (setq vnam
                 (vla-addline
                  (vla-get-modelspace(vla-get-activedocument(vlax-get-acad-object)))
                  (vlax-3d-point p10)(vlax-3d-point p11))
                 ls_vnam_duct(cons(list(list "LINE" num_temp)
                                       (list "OBJ" vnam))
                                  ls_vnam_duct)
                 )
           (addkillobj vnam)

           (if(setq lst(assoc(list "ARC"(1- num_temp)0)ls_search))
               ((lambda( / ls_gcode ls_p0 ls_p1 rr entna0 entna1)
                  (setq ls_search(vl-remove lst ls_search)
                        rr(cdr(assoc "RADIUS" lst))
                        vnam(cdr(assoc "OBJ" lst))
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
                        entna0(vlax-vla-object->ename vnam)
                        
                        ls_gcode(entget entna0)
                        ls_p0
                        ((lambda( / pc p0 p1 p2)
                           (setq pc(cdr(assoc 15 ls_gcode))
                                 p0(cdr(assoc 13 ls_gcode))p1(cdr(assoc 14 ls_gcode))
                                 p2(mapcar '(lambda(a b)(+ a(* rr b)))
                                           pc(unit_vector(mapcar '(lambda(a b c)(+ a b(* -2 c)))
                                                                 p0 p1 pc)))
                                 )
                           (list pc p0 p1 p2)
                           ))


                        lst(assoc(list "ARC"(1- num_temp)1)ls_search)
                        ls_search(vl-remove lst ls_search)
                        vnam(cdr(assoc "OBJ" lst))
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
                        entna1(vlax-vla-object->ename vnam)
                        
                        ls_gcode(entget entna1)
                        ls_p1
                        ((lambda( / pc p0 p1 p2)
                           (setq pc(cdr(assoc 15 ls_gcode))
                                 p0(cdr(assoc 13 ls_gcode))p1(cdr(assoc 14 ls_gcode))
                                 p2(mapcar '(lambda(a b)(+ a(* rr b)))
                                           pc(unit_vector(mapcar '(lambda(a b c)(+ a b(* -2 c)))
                                                                 p0 p1 pc)))
                                 )
                           (list pc p0 p1 p2)
                           ))
                        )

                  (if(= int_editstatus 2)
                      ((lambda( / p00 p01 p10 p11 p02 p03 p12 p13 lst px0 px1 vec0 vec1 veco)
                         (mapcar 'set '(p00 p01 p10 p11)(cadr ls_line))
                         (mapcar 'set '(p02 p03 p12 p13)(car ls_line))
                         (setq vec0(unit_vector(mapcar '- p01 p00))
                               vec1(unit_vector(mapcar '- p03 p02))
                               veco
                               (if(<(distance(cross_product vec0 vec1)(list 0 0 0))1e-8)
                                   vec_offset
                                 (if(if(setq px0(inters p00 p01 p02 p03 nil))
                                        (setq px1(inters p00 p01 p02 p03 nil)))
                                     (mapcar '- px1 px0)
                                   vec_offset) )
                               ls_p0
                               (mapcar '(lambda(lst)
                                          (mapcar '(lambda(p)(mapcar '+ p veco))
                                                  lst))
                                       (list ls_p0 ls_p1))
                               )
                         
                         ))
                    (setq ls_p0(list ls_p0 ls_p1)))


                  
                  (if(apply 'and
                            (mapcar '(lambda(lst)
                                       (<(distance(cadr lst)(caddr lst))1e-8))
                                    ls_p0))
                      (setq ls_p0(list)))
                  
                  (mapcar
                   '(lambda(entna i)
                      (setq vnam(vlax-ename->vla-object entna)
                            ls_vnam_duct(cons(list(list "ARC"(1- num_temp)i)
                                                  (list "OBJ" entna vnam)
                                                  (cons "RADIUS" rr)
                                                  (cons "POSITION" ls_p0)
                                                  )
                                             ls_vnam_duct)
                            )

                      (vla-put-Arrowhead1Type vnam 19)
                      (vla-put-Arrowhead2Type vnam 19)
                      (vla-put-ExtLine1Suppress vnam -1)
                      (vla-put-ExtLine2Suppress vnam -1)
                      (vla-put-HorizontalTextPosition vnam 3)
                      (vla-put-TextInsideAlign vnam 0)
                      (vla-put-VerticalTextPosition vnam 2)
                      (addkillobj vnam)
                      (vla-put-color vnam int_colductdimedit)
                      )
                   (list entna0 entna1)(list 0 1))
                  
                  )) )
           )

         (cond
          ((= int_ductmode 0)
           (setq bool_replacegrread T int_grread 3 elem_grread(list 0 0 0)
                 bool_noeditdepth T
                 int_ductdepth 0 bool_ductedit T
                 bool_firsttime_editduct 0)
           
           ;;(setq bool_firsta
           
           (vla-put-visible vnam_currentinsert :vlax-false)
           (setq ls_vnam_visible(cons vnam_currentinsert ls_vnam_visible))
           )
          
          ((= int_ductmode 1)
           (setq bool_replacegrread T int_grread 3 elem_grread(list 0 0 0)
                 bool_noeditdepth T bool_renewcopyduct T
                 vnam_road nil
                 int_ductdepth 0 bool_ductedit T)
           
           )
          )
         
         
         )

        ((if(or(= (cdr(assoc "terraduct3d" ls_xdata))"PROJECT")vnam_insertany)
             (progn
               (setq hand_road(vla-get-name vnam_insertduct)
                     vnam(vla-Item(vla-get-Blocks(vla-get-ActiveDocument(vlax-get-acad-object)))
                                  hand_road)
                     vnam_road_temp nil)
               
               (vlax-for
                obj vnam
                (if(if vnam_road_temp nil(=(vla-get-ObjectName obj)"AcDb3dPolyline"))
                    (progn 
                      (setq ls_p(vl-catch-all-apply 'vla-get-coordinates(list obj))
                            vnam_road_temp
                            (vla-Add3dPoly
                             (vla-get-ModelSpace(vla-get-ActiveDocument(vlax-get-acad-object)))
                             ls_p)
                            ls_vnam_killobj(cons vnam_road_temp ls_vnam_killobj)
                            )

                      (if(and(null ls_vnam_duct)(= int_getcolor_ductroad 0))
                          (progn
                            (setq int_colduct_temp(get_visual_color obj))
                            (vla-put-color vnam_road_temp int_colduct_temp)
                            
                            )
                        (vla-put-color vnam_road_temp(get_visual_color obj))
                        )
                      
                      )
                  )
                )
               
               (if vnam_road_temp
                   (setq vnam_road vnam_road_temp
                         ls_p_road(split_list 3(vlax-safearray->list(vlax-variant-value ls_p)))))
               
               )
           
           (if(if(setq vnam(car ls_vnam_select))
                  (progn
                    (setq ls_p(vl-catch-all-apply 'vla-get-coordinates(list vnam))
                          p0(vl-catch-all-apply 'vlax-curve-getstartpoint(list vnam))
                          p1(vl-catch-all-apply 'vlax-curve-getendpoint(list vnam))
                          ls_vnam_select nil )
                    (if(null(or(vl-catch-all-error-p p0)(vl-catch-all-error-p p1)))
                        (>(distance p0 p1)1e-8))))
               (progn
                 
                 (if(and(null ls_vnam_duct)(= int_getcolor_ductroad 0))
                     (progn
                       (setq int_colduct_temp(get_visual_color vnam))
                       ))
                 (setq vnam_road vnam
                       hand_road(vla-get-handle vnam_road)
                       ls_p_road(if(vl-catch-all-error-p ls_p)(list p0 p1)
                                  (progn
                                    (setq ls_p(vlax-safearray->list(vlax-variant-value ls_p)))
                                    (if(=(vla-get-ObjectName vnam_road)"AcDbPolyline")
                                        (mapcar '(lambda(a)(carxyz a(vla-get-elevation vnam_road)))
                                                (split_list 2 ls_p))
                                      (split_list 3 ls_p))
                                    ))
                       )
                 )
             )
           )
         
         (setq int_ductdepth 0 bool_ductedit T  int_duct 0 int_selectmenu -1)
         
         )
        
        ((= int_allow_noroad_temp 1)
         
         (x-alert(list 32076 36335 36984 25246 28961 12375 12391 20316 25104 12434 38283 22987 12375 12414 12377 )) ;;経路選択無しで作成を開始します
         
         (setq ls_p_road nil int_ductdepth 0 bool_ductedit T int_duct 0 int_selectmenu -1)
         
         )
        )


       
       ))
    
    (cons
     "KEYBOAD"
     (lambda( / ls_p bool_first);;(d- gr2_home()
       (cond
        
        ((if entna_depth
             (if(vl-position elem_grread(list 70 102 65318 65350 -218 -186))
                 ((lambda(lst / ii)
                    (while lst
                      (if(assoc "LINE"(car lst))(setq ii(1+ ii)))
                      (if(= ii 2)(setq lst nil)
                        (setq lst(cdr lst)))
                      )
                    (= ii 2))
                  ls_vnam_duct)))
         (setq str_edit "depthflatplane")
         
         )
        ((and(or(= elem_grread 13)(= int_grread 25))
             bool_ductedit ls_vnam_duct)
         (setq num_duct(1- int_min_duct)ls_vnam_copy(list))
        
         (while(<=(setq num_duct(1+ num_duct))int_max_duct)
           (setq num_x(- num_duct int_min_duct))
           (if(setq vnam(cadr(assoc "OBJ"(assoc(list "LINE" num_duct)ls_vnam_duct))))
               (progn
                 (setq vec_line
                       ((lambda( / p1 p2 vec)
                          (setq p1(vlax-curve-getstartpoint vnam)
                                p2(vlax-curve-getendpoint vnam)
                                vec(unit_vector(mapcar '- p1 p2))
                                )
                          (if(and(<(abs(car vec))1e-8)(<(abs(cadr vec))1e-8))T
                            (setq vec(unit_vector(carxyz vec 0))))
                          vec))
                       ls_vnam_copy(cons vnam ls_vnam_copy)
                       )
                 (vla-put-color vnam int_colductdim)
                 (set_xda vnam(list(cons 1000 "LINE")
                                   (cons 1000 "NUM")(cons 1071 num_x))
                          "terraduct3d")
                 
                 (mapcar
                  '(lambda(ii / entna ls_gcode p13 p14 dd str_level)
                     (setq lst(assoc(list "DEPTH" num_duct ii)ls_vnam_duct)
                           entna(cadr(assoc "OBJ" lst))
                           ls_gcode(entget entna)
                           p13(cdr(assoc 13 ls_gcode))p14(cdr(assoc 14 ls_gcode))
                           str_level(cdr(assoc 1 ls_gcode))
                           dd(apply '+(mapcar '* vec_line p13))
                           )

                     
                     (make_2pdimension
                      entna(list p13 p14 vec_line dd nil(* 0.5 pi)
                                 0. str_level str_dimstyle_ductlevel))
                     (setq vnam(caddr(assoc "OBJ" lst))
                           ls_vnam_copy(cons vnam ls_vnam_copy)
                           )
                     (vla-put-color vnam int_colductdim)
                     (set_xda vnam(list(cons 1000 "DEPTH")
                                       (cons 1000 "NUM")(cons 1071 num_x)
                                       (cons 1000 "SIDE")(cons 1071 ii)
                                       (cons 1000 "ROAD")(cons 1000(caddr(assoc "ROAD" lst)))
                                       )
                              "terraduct3d")
                     )
                  (list 0 1))
                 
                 ))
           
           (mapcar
            '(lambda(ii / )
               (if(setq lst(assoc(list "ARC" num_duct ii)ls_vnam_duct))
                   (progn
                     (setq vnam(caddr(assoc "OBJ" lst))
                           ls_vnam_copy(cons vnam ls_vnam_copy))
                     (vla-put-color vnam int_colductdim)
                     (set_xda vnam(list(cons 1000 "ARC")
                                       (cons 1000 "NUM")(cons 1071 num_x)
                                       (cons 1000 "SIDE")(cons 1071 ii)
                                       (cons 1000 "RADIUS")(cons 1040(cdr(assoc "RADIUS" lst)))
                                       )
                              "terraduct3d")
                     ))
               )
            (list 0 1))
           
           )

         
         (if(setq lst(assoc(list "SOLID" 0)ls_vnam_duct))
             (progn
               (setq vnam(cadr(assoc "OBJ" lst))
                     ls_vnam_copy(cons vnam ls_vnam_copy))
               (vla-put-visible vnam :vlax-true)
               (set_xda vnam(list(cons 1000 "DUCTSOLID")
                                 (cons 1000 "DIAM")(cons 1040(cdr(assoc "DIAM" lst)))
                                 )
                        "terraduct3d")

               (if ls_pcenterline
                   (progn
                     (setq ls_p(apply 'append ls_pcenterline)
                           array_p(vlax-make-safearray vlax-vbDouble(cons 0(1-(length ls_p)))))
                     (vlax-safearray-fill array_p ls_p)
                     (setq vnam(vla-Add3dPoly
                                (vla-get-ModelSpace(vla-get-ActiveDocument(vlax-get-acad-object)))
                                array_p))
                     (addkillobj vnam)
                     (setq ls_vnam_copy(cons vnam ls_vnam_copy))
                     (set_xda vnam(list(cons 1000 "CENTERLINE"))"terraduct3d")
                     ))
               ))

         (if(setq lst(assoc(list "MESH" 0)ls_vnam_duct))
             (progn
               (setq vnam(cadr(assoc "OBJ" lst))
                     ls_vnam_copy(cons vnam ls_vnam_copy))
               (vla-put-visible vnam :vlax-true)
               (set_xda vnam(list(cons 1000 "CONMESH")
                                 (cons 1000 "CORNER")(cons 1071(cdr(assoc "CORNER" lst)))
                                 (cons 1000 "WIDTH")(cons 1040(cdr(assoc "WIDTH" lst)))
                                 (cons 1000 "HEIGHT")(cons 1040(cdr(assoc "HEIGHT" lst)))
                                 (cons 1000 "FILET")(cons 1040(cdr(assoc "FILET" lst)))
                                 )
                        "terraduct3d")
               ))
         
         (if(null ls_vnam_copy)
             (x-alert(list 28145 24230 35373 23450 12364 36275 12426 12394 12356 12383 12417 20309 12418 20316 25104 12373 12428 12414 12379 12435 )) ;;深度設定が足りないため何も作成されません
           (progn
             
             (if(vl-catch-all-error-p
                 (setq block(vl-catch-all-apply 'vla-Item(list vnam_blocktable str_ductname))))
                 (setq block(vla-Add vnam_blockTable(vlax-3d-point 0 0 0)str_ductname) )
               (progn
                 (if vnam_currentinsert
                     (progn
                       (vla-delete vnam_currentinsert)
                       (setq ls_vnam_visible
                             (vl-remove vnam_currentinsert ls_vnam_visible)
                             vnam_currentinsert nil)
                       ))
                 (if(setq set_ent(ssget "X"(list(cons 2 str_ductname))))
                     (progn
                       (setq num(sslength set_ent))
                       (while(>(setq num(1- num))-1)
                         (setq vnam(vlax-ename->vla-object(ssname set_ent num)))
                         (vla-delete vnam)
                         )
                       ))
                 
                 (vla-delete block)
                 (mapcar '(lambda(v)
                            (vlax-release-object v)
                            (setq ls_vla-release(vl-remove v ls_vla-release))
                            )
                         (list block))
                 (setq block(vla-Add vnam_blockTable(vlax-3d-point 0 0 0)str_ductname))
                 )
               )
             
             (setq ls_vla-release(cons block ls_vla-release) )
             (vlax-for obj block(vla-delete obj))
             (vla-copyobjects
              (vla-get-ActiveDocument (vlax-get-acad-object))
              (vlax-make-variant
               (vlax-safearray-fill
                (vlax-make-safearray vlax-vbObject(cons 0 (1-(length ls_vnam_copy))))
                (reverse ls_vnam_copy))
               )
              block)
             
             (mapcar '(lambda(v)
                        (vlax-release-object v)
                        (setq ls_vla-release(vl-remove v ls_vla-release))
                        )
                     (list block))
             
             (mapcar '(lambda(v)
                        (vla-delete v)
                        (exckillobj v)
                        )
                     ls_vnam_copy)

             (setq vnam(vla-InsertBlock
                        (vla-get-ModelSpace(vla-get-ActiveDocument(vlax-get-acad-object)))
                        (vlax-3d-point 0 0 0)str_ductname 1 1 1 0)
                   )
             (set_xda vnam(list(cons 1000 "DUCTBLOCK"))"terraduct3d")
             ))

         (mapcar '(lambda(lst)
                    (if(setq lst(cdr(assoc "OBJ" lst)))
                        (mapcar '(lambda(a)(if(=(type a)'VLA-OBJECT)
                                               (if(vlax-erased-p a)T(vla-delete a))))
                                lst)
                      ))
                 ls_vnam_duct)
         
         (setq ls_vnam_duct(list) vnam_depth nil entna_depth nil
               bool_ductedit nil
               int_ductdepth 0 int_selectmenu nil str_ductname nil
               int_max_duct 0 int_min_duct 0 )

         (if(= int_editstatus 2)
             (progn
               (setq bool_ductedit nil int_editstatus nil)
               ))
         
         )
        
        ((and(or(= elem_grread 13)(= int_grread 25))
             bool_ductedit)
         (setq bool_ductedit nil str_edit "home")
         
         )
        
        ((and(or(= elem_grread 13)(= int_grread 25))
             (= int_ductmode 2))
         
         (mapcar
          '(lambda(vnam / str set_ent num)
             
             (vla-getXData vnam "terraduct3d" 'array_Type 'array_Data )
             (if(if array_data
                    (progn
                      (setq ls_xdata
                            (split_list 0(mapcar 'vlax-variant-value
                                                 (vlax-safearray->list array_data))))
                      (=(cdr(assoc "terraduct3d" ls_xdata))"DUCTBLOCK")
                      ))
                 (progn
                   (setq str(vla-get-name vnam)
                         vnam_insertname
                         (vla-Item(vla-get-Blocks (vla-get-ActiveDocument(vlax-get-acad-object)))str))
                   ;;(vla-delete vnam)
                   (if(setq set_ent(ssget "X"(list(cons 2 str))))
                       (progn
                         (setq num(sslength set_ent))
                         (while(>(setq num(1- num))-1)
                           (setq vnam(vlax-ename->vla-object(ssname set_ent num)))
                           (vla-delete vnam)
                           )
                         ))

                   (if (vl-catch-all-error-p
                        (vl-catch-all-apply 'vla-get-Handle (list vnam_insertname)))
                       T(vla-delete vnam_insertname))
                   
                   ;; (vl-remove vnam_insertname ls_vnam_highlight)
                   )
               )
             
             
             )
          ls_vnam_select)
         (setq ls_vnam_highlight(list)
               ls_vnam_select(list))
         )
        )
       ))
    )
   
   
   (list
    "insertarc" ;;edsym
    (cons
     "INITIAL"
     (lambda(bool /  delta_arc bool_loop);;initial
       (if(car bool)
           (progn
             ;;temp
             (setq bool_point nil
                   bool_selectent nil bool_select nil int_selectmode -1
                   ls_ssget nil xtype_ssget nil xdata_ssget nil
                   )
             
             (mapcar 'set '(vec_normal dist_normal radius_arcmove
                                       p_line00 p_line01 p_line10 p_line11
                                       vec_line0 vec_line1 length_s
                                       bool_linetype lst)
                     ls_arcmove)
             (setq point_decide nil ls_limit0 nil ls_limit1 nil)
             (cond
              ((= bool_linetype 0)
               (setq vec_linenormal(car lst) dist_linenormal(cadr lst)
                     dist_lineparallel
                     (sqrt(* dist_linenormal(-(* 2. radius_arcmove)dist_linenormal)))
                     )
               )
              ((= bool_linetype 1)
               (mapcar
                'set '(vec_nline0 vec_nline1 p_centerx p_arcstartx p_arcendx p_arcwayx p_crossx)
                lst)

               (setq p_center_limit00
                     (3dcross_3pl vec_normal dist_normal
                                  vec_nline0(+(apply '+(mapcar '* p_crossx vec_nline0))(* -1 radius_arcmove))
                                  vec_nline1(+(apply '+(mapcar '* p_crossx vec_nline1))(* 2 radius_arcmove))
                                  )
                     ls_limit0
                     (cal_arcposition radius_arcmove nil p_center_limit00 p_crossx
                                      vec_line0 vec_nline0 vec_line1 vec_nline1)
                     
                     p_center_limit10(nth 2 ls_limit0)
                     
                     p_center_limit11
                     (3dcross_3pl vec_normal dist_normal
                                  vec_nline0(+(apply '+(mapcar '* p_crossx vec_nline0))(* 2 radius_arcmove))
                                  vec_nline1(+(apply '+(mapcar '* p_crossx vec_nline1))(* -1 radius_arcmove))
                                  )
                     ls_limit1
                     (cal_arcposition radius_arcmove nil p_center_limit11 p_crossx
                                      vec_line1 vec_nline1 vec_line0 vec_nline0)
                     p_center_limit01(nth 2 ls_limit1)
                     
                     dist_limit00(apply '+(mapcar '* p_center_limit00 vec_line0))
                     dist_limit01(apply '+(mapcar '* p_center_limit01 vec_line0))
                     dist_limit10(apply '+(mapcar '* p_center_limit10 vec_line1))
                     dist_limit11(apply '+(mapcar '* p_center_limit11 vec_line1))
                     
                     )
               )
              ((= bool_linetype 2)

               ;; (mapcar
               ;;  'set '(vec_nline0 vec_nline1 p_centerx p_arcstartx p_arcendx p_arcwayx p_crossx)
               ;;  lst)
               (setq vec_normal(unit_vector(cross_product(mapcar '- p_line10 p_line01)vec_line0))
                     dist_normal(apply '+(mapcar '* vec_normal p_line01)))
               
               
               (setq delta_arc(* -0.1 radius_arcmove) bool_loop T d 0)
               (while bool_loop
                 (setq d(+ d delta_arc)
                       p_temp(mapcar '(lambda(a b)(+ a(* d b)))p_line01 vec_line0)
                       lst(cal_twisttangent
                           radius_arcmove p_temp
                           p_line00 p_line01 p_line10 p_line11 vec_line0 vec_line1 length_s)
                       )
                 
                 (if(setq p(cadar lst))
                     (cond
                      ((<(distance p p_line01)1e-8)
                       (setq ls_limit0 lst p_limit0 p_temp bool_loop nil))
                      (T
                       (setq ls_limit0 lst p_limit0 p_temp)
                       (if(>(*(apply '+(mapcar '*(mapcar '- p p_line01)vec_line0))delta_arc)0)
                           (setq delta_arc(* -0.1 delta_arc)) )
                       )
                      )
                   (if(null ls_limit0)(setq delta_arc(* -2. delta_arc))
                     (setq bool_loop nil))
                   )
                 )
               
               (setq delta_arc(* -0.1 radius_arcmove) bool_loop T d 0)
               (while bool_loop
                 (setq d(+ d delta_arc)
                       p_temp(mapcar '(lambda(a b)(+ a(* d b)))p_line10 vec_line1)
                       lst(cal_twisttangent
                           radius_arcmove p_temp
                           p_line10 p_line11 p_line00 p_line01 vec_line1 vec_line0 length_s)
                       )
                 (if(setq p(cadar lst))
                     (cond
                      ((<(distance p p_line10)1e-8)
                       (setq ls_limit1 lst bool_loop nil p_limit1 p_temp))
                      (T
                       (setq ls_limit1 lst p_limit1 p_temp)
                       (if(>(*(apply '+(mapcar '*(mapcar '- p p_line10)vec_line1))delta_arc)0)
                           (setq delta_arc(* -0.1 delta_arc)) )
                       )
                      )
                   (if(null ls_limit1)(setq delta_arc(* -2. delta_arc))
                     (setq bool_loop nil))
                   )
                 )
               
               (setq p_center_limit00(caar ls_limit0)
                     p_center_limit01(caadr ls_limit0)
                     
                     p_center_limit10(caadr ls_limit1)
                     p_center_limit11(caar ls_limit1)
                     
                     dist_limit00(apply '+(mapcar '* p_center_limit00 vec_line0))
                     dist_limit01(apply '+(mapcar '* p_center_limit01 vec_line0))
                     dist_limit10(apply '+(mapcar '* p_center_limit10 vec_line1))
                     dist_limit11(apply '+(mapcar '* p_center_limit11 vec_line1))
                     
                     )
               
               
               )
              )
             
             (setq ls_entna_arctemp
                   (mapcar '(lambda(i / v e)
                              (setq e(make_arcdimension
                                      nil(list(list 0 0 0)(list 1 0 0)(list 1 0 0)(list 1 0 0)1(list 0 0 1)
                                              0 " " str_dimstyle_ductlevel))
                                    vnam(vlax-ename->vla-object e)
                                    )
                              (vla-put-Arrowhead1Type vnam 19)
                              (vla-put-Arrowhead2Type vnam 19)
                              (vla-put-ExtLine1Suppress vnam -1)
                              (vla-put-ExtLine2Suppress vnam -1)
                              (addkillobj vnam)
                              (list e vnam))
                           (list 0 1))
                   p_arcstart nil
                   int_baseline 0

                   func_guidemenu
                   (lambda()
                     ;;todo
                     
                     (mix_strasc
                      (list
                       ;;クリックして起点終点の通過位置を決定
                       ;;または通過点採用のキー入力をしてください
                       ;;Enter:変更せずに戻る
                       12463 12522 12483 12463 12375 12390 36215 28857 32066 28857 12398 36890 36942 20301 32622 12434 27770 23450 "\n" 12414 12383 12399 36890 36942 28857 25505 29992 12398 12461 12540 20837 21147 12434 12375 12390 12367 12384 12373 12356
                       "\nEnter:"
                       (if p_arcprev
                           (list 22793 26356 12379 12378 12395 25147 12427 )
                         (list 21021 22238 12394 12398 12391 24517 12378 12393 12371 12363 36984 25246 12375 12390 12367 12384 12373 12356 ))
                       
                       )
                      ) )
                   
                   
                   ls_guideexplane
                   (mapcar
                    'mix_strasc
                    (list(list " - " 20870 24359 12398 36215 28857 12418 12375 12367 12399 32066 28857 12364 36890 36942 12377 12427 20301 32622 12434 27770 23450 12377 12427 )
                         ;;円弧の起点もしくは終点が通過する位置を決定する
                         ))
                   
                   ls_guidemenu
                   (list
                    (list(list 82) ;;R経路の曲げ半径
                         (cons "ITEM"(list 32076 36335 12398 26354 12370 21322 24452))
                         (cons "INPUT"(lambda() 'radius_arcmove))
                         (cons "HELP"(lambda()(mix_strasc(list 31649 36335 12364 26354 32218 12392 12394 12427 31623 25152 12395 12362 12369 12427 26354 12370 21322 24452(if intselectmenu str_guide_inputval str_guide_selectval)))))
                         
                         )
                    
                    (list(list 49);;近接点切替
                         (cons "ITEM"(list 36817 25509 28857 20999 26367  ))
                         (cons "INPUTSWITCH"
                               (lambda()
                                 (list 'int_baseline
                                       (mapcar 'mix_strasc
                                               (list(list "{\\C" str_gcol_g ";" 36215 28857 20596 "}")
                                                    (list "{\\C" str_gcol_c ";"32066 28857 20596 "}"))))
                                 ))
                         ;;マウスカーソルに近接する円弧が起点側か終点側か選択してください
                         (cons "HELP"(lambda()(mix_strasc(list 12510 12454 12473 12459 12540 12477 12523 12395 36817 25509 12377 12427 20870 24359 12364 36215 28857 20596 12363 32066 28857 20596 12363 36984 25246 12375 12390 12367 12384 12373 12356))))
                         
                         )

                    (list(list 50);;起点側終点を通過する円弧を採用する
                         (cons "ITEM"(list 36215 28857 20596 32066 28857 12434 36890 36942 12377 12427 20870 24359 12434 25505 29992 12377 12427))
                         (cons "LOADFUNCTION"
                               (lambda()
                                 (if(= bool_linetype 2)
                                     (setq int_baseline 0 elem_grread p_limit0)
                                   (setq int_baseline 0 elem_grread p_line01 ))
                                 (setq bool_replacegrread T int_grread 3)
                                 
                                 ))
                         (cons "HELP"(lambda()(mix_strasc(list 36215 28857 20596 32066 28857 12434 36890 36942 12377 12427 20870 24359 12434 25505 29992 12377 12427))))
                         )
                    
                    (list(list 51);;終点側起点を通過する円弧を採用する
                         (cons "ITEM"(list 32066 28857 20596 36215 28857 12434 36890 36942 12377 12427 20870 24359 12434 25505 29992 12377 12427))
                         (cons "LOADFUNCTION"
                               (lambda()
                                 (if(= bool_linetype 2)
                                     (setq int_baseline 0 elem_grread p_limit1)
                                   (setq int_baseline 1 elem_grread p_line10))
                                 (setq bool_replacegrread T int_grread 3)
                                 
                                 ))
                         (cons "HELP"(lambda()(mix_strasc(list 32066 28857 20596 36215 28857 12434 36890 36942 12377 12427 20870 24359 12434 25505 29992 12377 12427))))
                         )
                    
                    (list(list "ENTER");;変更せずに戻る
                         (cons "ITEM"(list 22793 26356 12379 12378 12395 25147 12427 ))
                         (cons "LOADFUNCTION"
                               (lambda()
                                 (if p_arcprev
                                     (setq bool_replacegrread T int_grread 3
                                           int_baseline 0 elem_grread p_arcprev )
                                   (x-alert(list 21021 22238 12394 12398 12391 24517 12378 12393 12371 12363 36984 25246 12375 12390 12367 12384 12373 12356 ))
                                   ;;初回なので必ずどこか選択してください
                                   )
                                 )))
                    )
                   
                   )

             (if bool_arcfirsttime
                 (setq bool_arcfirsttime nil bool_replacegrread T
                       int_grread 2
                       elem_grread(+ 49(if int_editarcposition_temp
                                           int_editarcposition_temp int_editarcposition))
                       ))
             
             ))
       
       (if(cadr bool) (list ) )
       ))
    
    (cons
     "MOVE"
     (lambda();;gr5
       (if(if vec_normal(<(abs(apply '+(mapcar '* vec_normal vec_view)))1e-8))
           (progn;;円弧の法線とビュー方向が直交しているとき描画できません;;ビューを回転させてください
             (x-alert(list 20870 24359 12398 27861 32218 12392 12499 12517 12540 26041 21521 12364
                           30452 20132 12375 12390 12356 12427 12392 12365
                           25551 30011 12391 12365 12414 12379 12435
                           "\n" 12499 12517 12540 12434 22238 36578
                           12373 12379 12390 12367 12384 12373 12356))
             (setq bool_replacegrread T int_grread 11)
             )
         (progn
           
           (setq p_proj
                 ((lambda(p vec_normal dist_normal ps / d  vec)
                    (setq d(- dist_normal(apply '+(mapcar '* p vec_normal))))
                    (if ps(setq vec vec_normal)
                      (setq d(/ d(apply '+(mapcar '* vec_view vec_normal)))vec vec_view))
                    (mapcar '(lambda(a b)(+ a(* d b)))p vec)
                    )
                  elem_grread vec_normal dist_normal nil
                  )
                 
                 d(apply '+(mapcar '*(mapcar '- p_proj(if(= int_baseline 0)p_line01 p_line10))
                                   (if(= int_baseline 0)vec_line0 vec_line1)))
                 p_temp(mapcar '(lambda(a b)(+ a(* d b)))(if(= int_baseline 0)p_line01 p_line10)
                               (if(= int_baseline 0)vec_line0 vec_line1))
                 
                 )
           
           (grdraw p_proj p_temp 2)
           ;; (grdraw p_center_limit00  p_center_limit01 3)
           ;; (grdraw p_center_limit11  p_center_limit01 1)

           (setq ls_arc_temp nil)
           ((lambda( p sym int_temp / lst bool ls_p px vec pc pco pho)
              (cond
               ((= bool_linetype 0)
                ;;平行
                (setq bool(= int_temp 0)
                      px(mapcar '(lambda(a x y)
                                   (+ a(*(if bool 1 -1)dist_lineparallel x)
                                      (*(if bool 1 -1)dist_linenormal y)))
                                p vec_line1 vec_linenormal)
                      pc(mapcar '(lambda(a y)(+ a(*(if bool 1 -1)radius_arcmove y)))
                                p vec_linenormal)
                      pco(mapcar '(lambda(a b)(+(- a)(* 2. b)))pc px)
                      pho(mapcar '(lambda(a y)(+ a(*(if bool 1 -1)radius_arcmove y)))
                                 pco vec_linenormal)
                      ls_p
                      (if bool(list(list pc p px vec_normal)(list pco px pho vec_normal) )
                        (list(list pco pho px vec_normal)(list pc px p vec_normal)))
                      )

                (set sym
                     (mapcar
                      '(lambda(lst / pc p0 p1 p2)
                         (setq pc(car lst)p0(cadr lst)p1(caddr lst)
                               p2(mapcar '(lambda(a b)(+ a(* radius_arcmove b)))
                                         pc(unit_vector(mapcar '(lambda(a b c)(+ a b(* -2 c)))
                                                               p0 p1 pc)))
                               )
                         (list pc p0 p1 p2 vec_normal))
                      ls_p)
                     )
                
                )
               ((= bool_linetype 1)
                ;;交差
                (setq bool(= int_temp 0)
                      d(apply '+(mapcar '* p(if bool vec_line0 vec_line1)))
                      ls_p
                      (if bool
                          (cond
                           ((> d dist_limit00)
                            (mapcar '(lambda(lst)(mapcar '(lambda(i)(nth i ls_limit0))lst))
                                    (list(list 0 1 4)(list 2 4 3))) )
                           ((< d dist_limit01)
                            (mapcar '(lambda(lst)(mapcar '(lambda(i)(nth i ls_limit1))lst))
                                    (list(list 2 3 4)(list 0 4 1))) )
                           (T
                            (setq lst(cal_arcposition
                                      radius_arcmove p nil p_crossx
                                      vec_line0 vec_nline0 vec_line1 vec_nline1)
                                  )
                            (mapcar '(lambda(x)(mapcar '(lambda(i)(nth i lst))x))
                                    (if(caddr lst) (list(list 0 1 4)(list 2 4 3))
                                      (list(list 0 1 4)(list 0 4 4))))
                            )
                           )
                        
                        (cond
                         ((< d dist_limit10)
                          (mapcar '(lambda(lst)(mapcar '(lambda(i)(nth i ls_limit0))lst))
                                  (list(list 0 1 4)(list 2 4 3))) )
                         ((> d dist_limit11)
                          (mapcar '(lambda(lst)(mapcar '(lambda(i)(nth i ls_limit1))lst))
                                  (list(list 2 3 4)(list 0 4 1))) )
                         (T
                          (setq lst(cal_arcposition
                                    radius_arcmove p nil p_crossx
                                    vec_line1 vec_nline1 vec_line0 vec_nline0)
                                )
                          
                          (mapcar '(lambda(x)(mapcar '(lambda(i)(nth i lst))x))
                                  (if(caddr lst) (list(list 2 3 4)(list 0 4 1))
                                    (list(list 0 4 1)(list 0 1 1))))
                          )
                         )
                        )
                      )

                (set sym
                     (mapcar
                      '(lambda(lst / pc p0 p1 p2)
                         (setq pc(car lst)p0(cadr lst)p1(caddr lst)
                               p2(mapcar '(lambda(a b)(+ a(* radius_arcmove b)))
                                         pc(unit_vector(mapcar '(lambda(a b c)(+ a b(* -2 c)))
                                                               p0 p1 pc)))
                               )
                         (list pc p0 p1 p2 vec_normal))
                      ls_p)
                     )

                
                )
               ((= bool_linetype 2)
                (setq bool(= int_temp 0)
                      d(apply '+(mapcar '* p(if bool vec_line0 vec_line1))))
                (if bool
                    (set sym(mapcar
                             '(lambda(lst ls_i)
                                (mapcar '(lambda(i)(nth i lst))ls_i))
                             (cal_twisttangent
                              radius_arcmove p
                              p_line00 p_line01 p_line10 p_line11 vec_line0 vec_line1 length_s)
                             (list(list 0 1 2 3 4 5)(list 0 2 1 3 4 5))
                             )
                         )
                  (set sym(mapcar
                           '(lambda(lst ls_i)
                              (mapcar '(lambda(i)(nth i lst))ls_i))
                           (reverse
                            (cal_twisttangent
                             radius_arcmove p
                             p_line10 p_line11 p_line00 p_line01 vec_line1 vec_line0 length_s))
                           (list(list 0 1 2 3 4 5)(list 0 2 1 3 4 5))
                           )
                       )
                  )
                
                
                )

               )
              
              
              
              )
            p_temp 'ls_arc_temp int_baseline )

           (if ls_arc_temp
               (progn
                 
                 (mapcar '(lambda(e lst bool / pc p0 p1 p2 v r d)
                            (mapcar 'set '(pc p0 p1 p2 v)lst)
                            (if bool(grdraw p0 p_line01 4)(grdraw p1 p_line10 4))
                            (setq d(apply '+(mapcar '* v pc)))
                            (make_arcdimension e(list pc p0 p1 p2
                                                      radius_arcmove v d
                                                      " " str_dimstyle_ductlevel))
                            (grdraw pc p0 8)
                            (grdraw pc p1 8)
                            (if(and(= bool_linetype 2)(equal(= int_baseline 0)bool))
                                (mapcar '(lambda(p)(grdraw p_temp p 8))(list p0 p1)))
                            )
                         (mapcar 'car ls_entna_arctemp) ls_arc_temp(list T nil))
                         
                 )
             (progn
               
               (mapcar '(lambda(e p bool / pc p0 p1 p2 v r d ls_gcode)
                          (setq ls_gcode(entget e)
                                p0(cdr(assoc(if bool 13 14)ls_gcode))
                                p1(cdr(assoc(if bool 14 13)ls_gcode))
                                pc(cdr(assoc 15 ls_gcode))
                                )
                          (grdraw p0 p 240)
                          (grdraw pc p0 8)
                          (grdraw pc p1 8)
                          )
                       (mapcar 'car ls_entna_arctemp)(list p_line01 p_line10)(list T nil))
               
               )
             
             )
           
           ))
       ))
    
    (cons
     "CLICK"
     (lambda();;gr3

       (if(if point_decide T (setq point_decide ls_arc_temp))
           (progn
             (setq ls_vnam_duct
                   (subst(list(assoc "ARC" ls_currentarc0)
                              (assoc "OBJ" ls_currentarc0)
                              (cons "RADIUS" radius_arcmove)
                              (cons "POSITION" point_decide)
                              )
                         ls_currentarc0 ls_vnam_duct)
                   ls_vnam_duct
                   (subst(list(assoc "ARC" ls_currentarc1)
                              (assoc "OBJ" ls_currentarc1)
                              (cons "RADIUS" radius_arcmove)
                              (cons "POSITION" point_decide)
                              )
                         ls_currentarc1 ls_vnam_duct)
                   )
             
             (mapcar '(lambda(v)
                        (setq v(cadr v))
                        (exckillobj v)
                        (vla-delete v))
                     ls_entna_arctemp)
             
             
             (setq bool_replacegrread T int_grread 3 
                   bool_noeditdepth T str_edit "makeductmain"
                   str_editreturn "insertarc"
                   p_arcprev nil
                   )
             )
         (progn
           (x-alert(list 20870 24359 12364 34920 31034 12373 12428 12390 12356 12394 12356 29366 24907 12391 12398 12463 12522 12483 12463 12399 28961 21177 12391 12377 ))
           ;;円弧が表示されていない状態でのクリックは無効です
           )
         )
       
       
       ))
    ;;KEYBOAD
    )




   (list
    "makeccboxmain" ;;edsym
    (cons
     "INITIAL"
     (lambda(bool);;initial
       (if(car bool)
           (progn
             ;;temp
             (setq bool_point T
                   bool_selectent nil bool_select nil int_selectmode -1
                   ls_ssget nil xtype_ssget nil xdata_ssget nil
                   )

             (setq p_ccbox0 nil p_ccbox1 nil p_ccbox2 nil p_manhole nil p_manhole_edge nil
                   str_colccbox0 91 str_colccbox1 111
                   str_colccbox2 131 str_colmanhole 151 str_colmanhole_edge 171
                   
                   )
             
             (setq
              func_guidemenu
              (lambda()
                (mix_strasc
                 (list )
                 ) )
              
              
              ls_guideexplane
              (mapcar
               'mix_strasc
               (list(list " - " 24179 38754 24418 29366 12434 12463 12522 12483 12463 12391 27770 23450 12377 12427)
                    (list " - " 24179 38754 24418 29366 12399 24517 12378 38263 26041 24418 12392 12394 12426 12289)
                    (list "   3" 28857 30446 12399 "1,2" 28857 30446 12392 24179 34892 12394 30452 32218 19978 12398 20219 24847 12398 28857 12434 36984 25246 12375 12390 12424 12356 )
                    (list " - " 12510 12531 12507 12540 12523 12398 20013 24515 12434 36984 25246 12375 12394 12356 12392 12365 12289 12510 12531 12507 12540 12523 12434 20316 25104 12375 12394 12356 )
                    ))
              ;;平面形状をクリックで決定する
              ;;平面形状は必ず長方形となり、3点目は1,2点目と平行な直線上の任意の点を選択してよい
              ;;マンホールの中心を選択しないとき、マンホールを作成しない
              ls_guidemenu
              (list
               (list(list 76);; 地表面標高
                    (cons "ITEM"(list 22320 34920 38754 27161 39640))
                    (cons "STATUS"
                          (lambda()
                            (if str_lasground
                                (mix_strasc
                                 (list "{\\C" str_gcol_c ";" 12487 12540 12479 "} : "
                                       (vl-string-subst "" "lasgrid-" str_lasground)))
                              (if height_ground
                                  (mix_strasc
                                   (list"{\\C" str_gcol_g ";" 27161 39640 "} : "
                                        (as-numstr height_ground)))
                                (mix_strasc
                                 (list "{\\C" str_gcol_r ";"
                                       36984 25246 12373 12428 12390 12356 12414 12379 12435 "}"))
                                ))
                            ))
                    (cons "LOADFUNCTION"(lambda()(settile_selectground)))
                    (cons "HELP"(lambda()(mix_strasc(list 20351 29992 12377 12427 27161 39640 12434 "\n- las" 35501 36796 "\n- xml" 35501 36796 "\n- " 19968 23450 27161 39640 12398 25968 20516 20837 21147 "\n" 12363 12425 36984 25246 12391 12365 12414 12377 ))))
                    )

               (list(list 67);;特殊部の色
                    (cons "ITEM"(list 29305 27530 37096 12398 33394))
                    (cons "INPUTCOLOR"(lambda()
                                        (if int_colccbox_temp T(setq int_colccbox_temp int_colccbox))
                                        'int_colccbox_temp))
                    (cons "LOADFUNCTION"
                          (lambda()
                            nil
                            ))
                    ;;カラーダイアログが表示され、特殊部のモデルに使う色を選択できます
                    (cons "HELP"(lambda()(mix_strasc(list 12459 12521 12540 12480 12452 12450 12525 12464 12364 34920 31034 12373 12428 12289 29305 27530 37096 12398 12514 12487 12523 12395 20351 12358 33394 12434 36984 25246 12391 12365 12414 12377 ))))
                    )
               
               
               (list(list 69);;E特殊部天端から地表面まで
                    (cons "ITEM"(list 29305 27530 37096 22825 31471 12363 12425 22320 34920 38754 12414 12391))
                    (cons "INPUT"(lambda()
                                   (if height_ccboxtop_temp T
                                     (setq height_ccboxtop_temp height_ccboxtop))
                                   'height_ccboxtop_temp))
                    (cons "LOADUNCTION"
                          (lambda()nil))
                    ;;特殊部の天端から地表面までの高さで、マンホールを作成するときマンホール高さを表す数値となります
                    (cons "HELP"(lambda()(mix_strasc(list 29305 27530 37096 12398 22825 31471 12363 12425 22320 34920 38754 12414 12391 12398 39640 12373 12391 12289 12510 12531 12507 12540 12523 12434 20316 25104 12377 12427 12392 12365 12510 12531 12507 12540 12523 39640 12373 12434 34920 12377 25968 20516 12392 12394 12426 12414 12377 (if intselectmenu str_guide_inputval str_guide_selectval)))))
                    )
               (list(list 72);;H特殊部高さ
                    (cons "ITEM"(list 29305 27530 37096 39640 12373))
                    (cons "INPUT"(lambda()
                                   (if height_ccbox_temp T
                                     (setq height_ccbox_temp height_ccbox))
                                   'height_ccbox_temp))
                    (cons "LOADUNCTION"
                          (lambda()nil))
                    ;;
                    (cons "HELP"(lambda()(mix_strasc(list(if intselectmenu str_guide_inputval str_guide_selectval)))))
                    )
               (list(list 68);;Dマンホール直径
                    (cons "ITEM"(list 12510 12531 12507 12540 12523 30452 24452))
                    (cons "INPUT"(lambda()
                                   (if diam_manhole_temp T
                                     (setq diam_manhole_temp diam_manhole))
                                   'diam_manhole_temp))
                    (cons "LOADUNCTION"
                          (lambda()nil))
                    (cons "HELP"(lambda()(mix_strasc(list(if intselectmenu str_guide_inputval str_guide_selectval)))))
                    )
               (list(list 87);;W均しコンオフセット
                    (cons "ITEM"(list 22343 12375 12467 12531 12458 12501 12475 12483 12488))
                    (cons "INPUT"(lambda()
                                   (if offset_levelingcon_temp T
                                     (setq offset_levelingcon_temp offset_levelingcon))
                                   'offset_levelingcon_temp))
                    (cons "LOADUNCTION"
                          (lambda()nil))
                    ;;特殊部の全周に入力値を加える
                    (cons "HELP"(lambda()(mix_strasc(list 29305 27530 37096 12398 20840 21608 12395 20837 21147 20516 12434 21152 12360 12427(if intselectmenu str_guide_inputval str_guide_selectval)))))
                    )
               (list(list 84);;T均しコン高さ
                    (cons "ITEM"(list 22343 12375 12467 12531 39640 12373))
                    (cons "INPUT"(lambda()
                                   (if height_levelingcon_temp T
                                     (setq height_levelingcon_temp height_levelingcon))
                                   'height_levelingcon_temp))
                    (cons "LOADUNCTION"
                          (lambda()nil))
                    (cons "HELP"(lambda()(mix_strasc(list(if intselectmenu str_guide_inputval str_guide_selectval)))))
                    )
               
               (list(list 49);;特殊部起点を選択
                    (cons "ITEM"(list 29305 27530 37096 32 36215 28857 12434 36984 25246 ))
                    (cons "GETPOINT"(lambda()(list p_ccbox0(itoa str_colccbox0))))
                    (cons "LOADFUNCTION"
                          (lambda(bool)
                            (setq int_selectmenu nil)
                            (if(/= int_ccboxmode 0)
                                (x-alert(list 20316 25104 12514 12540 12489 12398 12392 12365 23455 34892 21487 33021 ))
                              (progn
                                (if bool(setq p_ccbox0 elem_grread))
                                (setq bool_replacegrread T int_grread 2 elem_grread 50)
                                ))
                            ))
                    ;;特殊部の点を3つ選択します
                    ;;1点目と2点目で特殊部のいずれかの辺を表し、3点目はその辺と平行な辺上の任意の点を表します
                    ;;1点目
                    (cons "HELP"(lambda()(mix_strasc(list 29305 27530 37096 12398 28857 12434 "3" 12388 36984 25246 12375 12414 12377 "\n1" 28857 30446 12392 "2" 28857 30446 12391 29305 27530 37096 12398 12356 12378 12428 12363 12398 36794 12434 34920 12375 12289 "3" 28857 30446 12399 12381 12398 36794 12392 24179 34892 12394 36794 19978 12398 20219 24847 12398 28857 12434 34920 12375 12414 12377 "\n1" 28857 30446 (if intselectmenu str_guide_point)))))
                    )
               
               (list(list 50);;特殊部辺の端点を選択
                    (cons "ITEM"(list 29305 27530 37096 32 36794 12398 31471 28857 12434 36984 25246  ))
                    (cons "GETPOINT"(lambda()(list p_ccbox1(itoa str_colccbox1))))
                    (cons "LOADFUNCTION"
                          (lambda(bool)
                            (setq int_selectmenu nil)
                            (if(/= int_ccboxmode 0)
                                (x-alert(list 20316 25104 12514 12540 12489 12398 12392 12365 23455 34892 21487 33021 ))
                              (progn
                                (if bool(setq p_ccbox1 elem_grread))
                                (setq bool_replacegrread T int_grread 2 elem_grread 51)
                                ))
                            ))
                    (cons "HELP"(lambda()(mix_strasc(list 29305 27530 37096 12398 28857 12434 "3" 12388 36984 25246 12375 12414 12377 "\n1" 28857 30446 12392 "2" 28857 30446 12391 29305 27530 37096 12398 12356 12378 12428 12363 12398 36794 12434 34920 12375 12289 "3" 28857 30446 12399 12381 12398 36794 12392 24179 34892 12394 36794 19978 12398 20219 24847 12398 28857 12434 34920 12375 12414 12377 "\n2" 28857 30446 (if intselectmenu str_guide_point)))))
                    )
               
               (list(list 51);;特殊部対辺上の点を選択
                    (cons "ITEM"(list 29305 27530 37096 32 23550 36794 19978 12398 28857 12434 36984 25246) )
                    (cons "GETPOINT"(lambda()(list p_ccbox2(itoa str_colccbox2))) )
                    (cons "LOADFUNCTION"
                          (lambda(bool)
                            (setq int_selectmenu nil)
                            (if(/= int_ccboxmode 0)
                                (x-alert(list 20316 25104 12514 12540 12489 12398 12392 12365 23455 34892 21487 33021 ))
                              (progn
                                (if bool(setq p_ccbox2 elem_grread))
                                (setq bool_replacegrread T int_grread 2 elem_grread 52)
                                ))
                            ))
                    (cons "HELP"(lambda()(mix_strasc(list 29305 27530 37096 12398 28857 12434 "3" 12388 36984 25246 12375 12414 12377 "\n1" 28857 30446 12392 "2" 28857 30446 12391 29305 27530 37096 12398 12356 12378 12428 12363 12398 36794 12434 34920 12375 12289 "3" 28857 30446 12399 12381 12398 36794 12392 24179 34892 12394 36794 19978 12398 20219 24847 12398 28857 12434 34920 12375 12414 12377 "\n3" 28857 30446 (if intselectmenu str_guide_point)))))
                    
                    )
               
               (list(list 52);;マンホール-中心を選択
                    (cons "ITEM"(list 12510 12531 12507 12540 12523 32 20013 24515 12434 36984 25246 ))
                    (cons "GETPOINT" (lambda() (list p_manhole(itoa str_colmanhole))))
                    (cons "GETPOINTSNAP" ",_cen")
                    (cons "LOADFUNCTION"
                          (lambda(bool)
                            (setq int_selectmenu nil)
                            (if(/= int_ccboxmode 0)
                                (x-alert(list 20316 25104 12514 12540 12489 12398 12392 12365 23455 34892 21487 33021 ))
                              (progn
                                (setq str_addsnap "")
                                (if bool(setq p_manhole elem_grread))
                                (setq bool_replacegrread T int_grread 2 elem_grread 53)
                                ))
                            ))
                    ;;マンホールの中心,選択しないときマンホールは作成されません
                    (cons "HELP"(lambda()(mix_strasc(list 12510 12531 12507 12540 12523 12398 20013 24515 " " 36984 25246 12375 12394 12356 12392 12365 12510 12531 12507 12540 12523 12399 20316 25104 12373 12428 12414 12379 12435(if intselectmenu str_guide_point)))))
                    
                    )
               (list(list 53);;マンホール-円周上の点を選択
                    (cons "ITEM"(list 12510 12531 12507 12540 12523 32 20870 21608 19978 12398 28857 12434 36984 25246 ))
                    (cons "GETPOINT" (lambda()(list p_manhole_edge(itoa str_colmanhole_edge))))
                    (cons "GETPOINTSNAP" ",_nea")
                    (cons "LOADFUNCTION"
                          (lambda(bool)
                            (setq int_selectmenu nil)
                            (if(/= int_ccboxmode 0)
                                (x-alert(list 20316 25104 12514 12540 12489 12398 12392 12365 23455 34892 21487 33021 ))
                              (progn
                                (setq str_addsnap "")
                                (if bool(setq p_manhole_edge elem_grread))
                                (if(and p_manhole p_manhole_edge)
                                    (setq diam_manhole_temp
                                          (* 2.(distance(carxy p_manhole)(carxy p_manhole_edge)))
                                          diam_manhole_temp(atof(rtos diam_manhole_temp 2 8))
                                          ))
                                (setq bool_replacegrread T int_grread 2 elem_grread 54)
                                ))
                            ))
                    ;;マンホール中心があるとき、選択するとマンホール直径の入力値を計算して入力します
                    ;;\nマンホールがないとき、直径の入力値を使うときは選択する必要がありません
                    (cons "HELP"(lambda()(mix_strasc(list 12510 12531 12507 12540 12523 20013 24515 12364 12354 12427 12392 12365 12289 36984 25246 12377 12427 12392 12510 12531 12507 12540 12523 30452 24452 12398 20837 21147 20516 12434 35336 31639 12375 12390 20837 21147 12375 12414 12377  "\n" 12510 12531 12507 12540 12523 12364 12394 12356 12392 12365 12289 30452 24452 12398 20837 21147 20516 12434 20351 12358 12392 12365 12399 36984 25246 12377 12427 24517 35201 12364 12354 12426 12414 12379 12435
 (if intselectmenu str_guide_point)))))
                    
                    )

               (list(list 78);;特殊部の名称
                    (cons "ITEM"(list 29305 27530 37096 12398 21517 31216))
                    (cons "INPUTSTR"
                          (lambda( / str i bool func)
                            (if(= str_ccboxname "")(setq str_ccboxname nil))
                            (if(if str_ccboxname
                                   (if(= int_allow_overwrite 1) nil
                                     (null
                                      (vl-catch-all-error-p
                                       (vl-catch-all-apply 'vla-Item(list vnam_blocktable str_ccboxname))))
                                     )
                                 T)
                                (progn
                                  (setq str "CCBOX$" i 0 bool T)
                                  (while bool
                                    (setq i(1+ i)
                                          str_ccboxname(strcat str(substr(itoa(+ 1000 i))2))
                                          bool
                                          (null
                                           (vl-catch-all-error-p
                                            (vl-catch-all-apply 'vla-Item(list vnam_blocktable str_ccboxname))))
                                          )
                                    )
                                  ))
                            'str_ccboxname)
                          )
                    (cons "NO-INPUT" (lambda()nil ))
                    (cons "LOADFUNCTION"
                          (lambda(str / bool)
                            (setq bool
                                  (vl-catch-all-error-p
                                   (vl-catch-all-apply 'vla-Item(list vnam_blocktable str)))
                                  )
                            (if bool T
                              (if(= int_allow_overwrite 0)nil
                                (progn
                                  (alert
                                   (mix_strasc(list 26082 12395 23384 22312 12377 12427 12502 12525 12483 12463 21517 31216 12391 12377 12364 19978 26360 12365 12373 12428 12414 12377 ))
                                   )
                                  T)))
                            )
                          )
                    (cons "STRINPUTALERT"
                          (mix_strasc(list 12502 12525 12483 12463 21517 31216 12364 26082 12395 23384 22312 12375 12390 12356 12414 12377 )))
                    
                    ;;これから作成する特殊部、編集中の特殊部管路の名称を変更します\n特殊部はブロックとして作成されすでに使用されているブロック名称は設定できません
                    ;;\n名前を付けないとき「CCBOX$(使用されていない数値)」を自動的に設定します
                    (cons "HELP"(lambda()(mix_strasc(list 12371 12428 12363 12425 20316 25104 12377 12427 29305 27530 37096 12289 32232 38598 20013 12398 29305 27530 37096 31649 36335 12398 21517 31216 12434 22793 26356 12375 12414 12377 "\n" 29305 27530 37096 12399 12502 12525 12483 12463 12392 12375 12390 20316 25104 12373 12428 12377 12391 12395 20351 29992 12373 12428 12390 12356 12427 12502 12525 12483 12463 21517 31216 12399 35373 23450 12391 12365 12414 12379 12435 "\n" 21517 21069 12434 20184 12369 12394 12356 12392 12365 12300 "CCBOX$(" 20351 29992 12373 12428 12390 12356 12394 12356 25968 20516 ")" 12301 12434 33258 21205 30340 12395 35373 23450 12375 12414 12377))))
                    
                    )
               
               (list(list 77)
                    (cons "ITEM"(list 12514 12540 12489 20999 26367 ))
                    (cons "INPUTSWITCH"
                          (lambda()
                            (list 'int_ccboxmode
                                  (mapcar 'mix_strasc;;作成名称変更削除
                                          (list(list "{\\C" str_gcol_y ";" 20316 25104 "}")
                                               (list "{\\C" str_gcol_g ";" 21517 31216 22793 26356 "}")
                                               (list "{\\C" str_gcol_p ";" 21066 38500 "}")
                                               )))
                            ))
                    (cons "LOADFUNCTION"
                          (lambda()
                            
                            (cond
                             ((= int_ccboxmode 0)
                              
                              (setq bool_point T
                                    bool_selectent nil bool_select nil int_selectmode -1
                                    ls_ssget nil xtype_ssget nil xdata_ssget nil
                                    p_ccbox0 nil p_ccbox1 nil p_ccbox2 nil
                                    p_manhole nil p_manhole_edge nil
                                    )
                              
                              )
                             ((= int_ccboxmode 1)
                              (setq bool_point nil
                                    bool_selectent nil bool_select nil int_selectmode -1
                                    ls_vnam_selevt(list)
                                    ls_ssget(list(cons 0 "INSERT"))
                                    xtype_ssget "CCBOXBLOCK" xdata_ssget "terraduct3d"
                                    p_ccbox0 nil p_ccbox1 nil p_ccbox2 nil
                                    p_manhole nil  p_manhole_edge nil
                                    )
                              )
                             ((= int_ccboxmode 2)
                              (setq bool_point nil
                                    bool_selectent T bool_select T int_selectmode 0
                                    ls_vnam_selevt(list)
                                    ls_ssget(list(cons 0 "INSERT"))
                                    xtype_ssget "CCBOXBLOCK" xdata_ssget "terraduct3d"
                                    p_ccbox0 nil p_ccbox1 nil p_ccbox2 nil
                                    p_manhole nil p_manhole_edge nil
                                    )
                              )
                             )
                            
                            ))
                    
                    ;;-作成：平面図の点を選択して特殊部作成
                    ;;\n-名称変更：作成済みの特殊部を選択して名称を編集する
                    ;;\n-削除：特殊部を選択して削除
                    (cons "HELP"
                          (lambda()
                            (mix_strasc(list "-" 20316 25104 65306 24179 38754 22259 12398 28857 12434 36984 25246 12375 12390 29305 27530 37096 20316 25104 "\n-" 21517 31216 22793 26356 65306 20316 25104 28168 12415 12398 29305 27530 37096 12434 36984 25246 12375 12390 21517 31216 12434 32232 38598 12377 12427 "\n-" 21066 38500 65306 29305 27530 37096 12434 36984 25246 12375 12390 21066 38500 ))))
                    
                    )
               
               (list(list 80 );;P選択モード切替
                    (cons "ITEM"(list 36984 25246 12514 12540 12489 20999 26367 ))
                    (cons "INPUTSWITCH"
                          (lambda()
                            (setq int_temp 0)
                            (if(= int_ccboxmode 2)
                                (list 'int_selectmode
                                      (mapcar 'mix_strasc
                                              (list(list "{\\C" str_gcol_y ";" 36984 25246 "}")
                                                   (list "{\\C" str_gcol_c ";" 35299 38500 "}")))
                                      )
                              (list 'int_temp ;;削除モードのとき使用可能
                                    (list(mix_strasc
                                          (list "{\\C" str_gcol_p ";"
                                                21066 38500 12514 12540 12489 12398 12392 12365 20351 29992 21487 33021 "}")))
                                    )
                              )) )
                    (cons "HELP"(lambda()str_guide_selectmode))
                    )
               
               
               (list(list nil)
                    (cons "ITEM"(list ))
                    (cons "BOOL"
                          (lambda()
                            T
                            ))
                    (cons
                     "STATUS"
                     (lambda( / bool_depth ii ls_out)
                       (setq ls_out
                             (cond
                              ((= int_ccboxmode 0);;作成編集
                               (if(and p_ccbox0 p_ccbox1 p_ccbox2)
                                   (list
                                    (list
                                     "{\\C" str_gcol_y ";";;特殊部作成
                                     " - Enter : " 29305 27530 37096 20316 25104 "}"
                                     "{\\C" str_gcol_g ";";;マンホールありなし
                                     "  " 12510 12531 12507 12540 12523
                                     (if p_manhole(list 12354 12426)(list 12394 12375 )) "}"
                                     )
                                    )
                                 (list
                                  (list
                                   "{\\C" str_gcol_p ";" ;;点の選択が足りません
                                    28857 12398 36984 25246 12364 36275 12426 12414 12379 12435 "}"
                                    )
                                  )
                                 )
                               )
                              
                              ((= int_ccboxmode 1);;名称変更
                               (list
                                (list
                                 "{\\C" str_gcol_g ";";;特殊部作成
                                 " - " 20316 25104 28168 12415 12398 29305 27530 37096 12434 36984 25246 12377 12427 12392 29694 22312 35373 23450 12373 12428 12390 12356 12427 21517 31216 12434 36969 29992 12373 12379 12414 12377 )
                                ;;作成済みの特殊部を選択すると現在設定されている名称を適用させます
                                (list
                                 " - " 26032 12383 12395 35373 23450 12373 12428 12383 21517 31216 12364 12377 12391 12395 22259 38754 20869 12391 20351 12431 12428 12390 12356 12427 12392 12365 12289 )
                                
                                ;;新たに設定された名称がすでに図面内で使われているとき、
                                (list
                                 "   " 12381 12398 12458 12502 12472 12455 12463 12488 12399 21066 38500 12373 12428 12414 12377 )
                                ;;そのオブジェクトは削除されます
                                )
                               
                               )
                              ((= int_ccboxmode 2) ;;削除
                               (list
                                (list
                                 "{\\C" str_gcol_p ";"
                                 " - " 29305 27530 37096 12434 36984 25246 12375 12390 "Enter : "
                                 21066 38500 "}"
                                 ;;特殊部を選択してEnter:削除
                                 )
                                )
                               )
                              );;cond
                             ls_out(vl-remove nil ls_out)
                             )
                       ls_out
                       ))
                    )
               
               (list(list "ENTER");;メインメニューへ
                    (cons "ITEM"(list 12513 12452 12531 12513 12491 12517 12540 12408))
                    (cons "LOADFUNCTION"
                          (lambda()
                            (setq str_next
                                  (if(or p_ccbox0 p_ccbox1 p_ccbox2 p_manhole)
                                      nil "home"))
                            ))
                    (cons "NEXTMODE" "home")
                    )

               )

              bool_replacegrread T int_grread 2 elem_grread 49
              
              
              )
             
             ))
       
       (if(cadr bool) (list ) )
       ))
    
    (cons
     "MOVE"
     (lambda();;gr5

       (mapcar
        '(lambda(p c / s ls_p)
           (if p
               (progn
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
                 ))
           )
        (list p_ccbox0 p_ccbox1 p_ccbox2 p_manhole p_manhole_edge)
        (list str_colccbox0 str_colccbox1 str_colccbox2 str_colmanhole str_colmanhole_edge)
        )
       
       ))
    
    (cons
     "CLICK"
     (lambda( / )
       (cond
        ((if(if(= int_ccboxmode 1)
                (if(setq set_ent(ssget elem_grread(list(cons 0 "INSERT")(list -3(list "terraduct3d")))))
                    (setq vnam(vlax-ename->vla-object(ssname set_ent 0)))))
             (progn
               (vla-getXData vnam "terraduct3d" 'array_Type 'array_Data )
               (setq ls_xdata
                     (if array_data
                         (split_list 0(mapcar 'vlax-variant-value
                                              (vlax-safearray->list array_data))))
                     )
               
               (=(cdr(assoc "terraduct3d" ls_xdata))"CCBOXBLOCK")
               ))
         
         (setq str_bname(vla-get-name vnam)
               block(vl-catch-all-apply 'vla-Item(list vnam_blocktable str_bname)))
         

         (settile_strinput;;名称変更
          'str_bname
          (lambda(a)
            (if(= a str_bname)T
              (vl-catch-all-error-p
               (vl-catch-all-apply 'vla-Item(list vnam_blocktable a))))
            )
          
          (mix_strasc(list 21517 31216 22793 26356 ))
          (mix_strasc(list 26082 12395 23384 22312 12377 12427 21517 31216 12399 20351 12360 12414 12379 12435 ))
          ;;既に存在する名称は使えません
          )
         
         (if(if str_bname(/= str_bname ""))(vla-put-name block str_bname))

         
         (mapcar '(lambda(v)
                    (vlax-release-object v)
                    (setq ls_vla-release(vl-remove v ls_vla-release))
                    )
                 (list block))
         )
        )
       )
     )
    
    ;;  (lambda()nil ))

    (cons
     "KEYBOAD"
     (lambda( / ls_p)
       (cond
        ((and(or(= elem_grread 13)(= int_grread 25))
             (= int_ccboxmode 2))
         
         (mapcar
          '(lambda(vnam)
             (if(vlax-erased-p vnam)T
               (progn
                 (setq str(vla-get-name vnam))
                 (if(setq set_ent(ssget "X"(list(cons 2 str))))
                     (progn
                       (setq num(sslength set_ent))
                       (while(>(setq num(1- num))-1)
                         (setq vnam(vlax-ename->vla-object(ssname set_ent num))
                               ls_vnam_highlight(vl-remove vnam ls_vnam_highlight))
                         
                         (vla-delete vnam)
                         )
                       ))
                 (vla-delete
                  (vla-Item(vla-get-Blocks(vla-get-ActiveDocument
                                           (vlax-get-acad-object)))
                           str) )

                 ))
             )
          ls_vnam_select)
         
         )
        ((and(or(= elem_grread 13)(= int_grread 25))
             p_ccbox0 p_ccbox1 p_ccbox2
             (or str_lasground height_ground))

         (setq vecx(unit_vector(carxyz(mapcar '- p_ccbox1 p_ccbox0)0))
               vecy(list(-(cadr vecx))(car vecx))
               width_ccbox(apply '+(mapcar '*(mapcar '- p_ccbox2 p_ccbox0)vecy))
               )
         
         (cond
          ((<(abs width_ccbox)1e-8)
           (x-alert(list "3" 28857 12364 30452 32218 19978 12391 12354 12427 12383 12417 20316 25104 12391 12365 12414 12379 12435 ))
           ;;3点が直線上であるため作成できません
           )
          ((setq ls_p
                 (if p_manhole
                     (project_to_ground
                      (list p_manhole)
                      (list 0. 0. 1.)(list str_lasground height_ground))
                   (project_to_ground
                    (list p_ccbox0 p_ccbox1 p_ccbox2)
                    (list 0. 0. 1.)(list str_lasground height_ground))))

           (if(< width_ccbox 0) (setq width_ccbox(- width_ccbox) vecy(mapcar '- vecy)))
           
           (setq elevation_ground(/(apply '+(mapcar 'caddr ls_p))(length ls_p))
                 elevation_ccbox(- elevation_ground height_ccboxtop_temp height_ccbox_temp)
                 widthx05(* 0.5(apply '+(mapcar '*(mapcar '- p_ccbox1 p_ccbox0)vecx)))
                 
                 p_ccboxc(mapcar '(lambda(a x y)(+ a(* widthx05 x)(* 0.5 width_ccbox y)))
                                 p_ccbox0 vecx vecy)
                 p_ccboxc(carxyz p_ccboxc elevation_ccbox)
                 entna(rac_sld widthx05(* 0.5 width_ccbox)height_ccbox_temp
                               p_ccboxc 0(list 0 0 1)vecx(getvar "CLAYER")nil)
                 ;;(code_loft_solid lsls_point nil nil)
                 vnam(vlax-ename->vla-object entna)
                 ls_vnam(list vnam)
                 )
           (set_xda vnam(list(cons 1000 "CCBOXSOLID")
                             )
                    "terraduct3d")
           (vla-put-color vnam int_colccbox_temp)
           
           (setq vec_normal(list(-(car vecy))(-(cadr vecy))0)
                 dist_normal(apply '+(mapcar '* p_ccbox0 vec_normal))
                 )
           (mapcar
            '(lambda(lst / p0 p1 vec_normal dist_normal vecy ang hh str_xdata)
               (mapcar 'set '(p0 p1 vec_normal dist_normal vecy ang hh str_xdata)lst)
               (setq entna(make_2pdimension
                           nil(list p0 p1 vec_normal dist_normal vecy ang hh
                                    "" str_dimstyle_ductlevel))
                     vnam(vlax-ename->vla-object entna)
                     ls_vnam(cons vnam ls_vnam)
                     )
               (set_xda vnam(list(cons 1000 str_xdata) )
                        "terraduct3d")
               )
            (list(list(carxyz p_ccbox0 elevation_ccbox)(carxyz p_ccbox1 elevation_ccbox)
                      vec_normal dist_normal(list 0 0 1)0.(* 0.2 height_ccbox_temp)
                      "CCBOXWIDTH-X")
                 (list(carxyz p_ccbox0 elevation_ccbox)
                      (carxyz p_ccbox0(+ elevation_ccbox height_ccbox_temp))
                      vec_normal dist_normal(mapcar '- vecx)(* 0.5 pi)
                      (* 0.2 height_ccbox_temp)
                      "CCBOXHEIGHT")
                 (list(carxyz p_ccbox0(+ elevation_ccbox height_ccbox_temp))
                      (carxyz p_ccbox0 elevation_ground)
                      vec_normal dist_normal(mapcar '- vecx)(* 0.5 pi)
                      (* 0.2 height_ccbox_temp)
                      "CCBOXVBACANT")
                 (list(mapcar '(lambda(a b)(+ a(* -1 b width_ccbox)))
                              (carxyz p_ccbox0 elevation_ccbox)vec_normal)
                      (carxyz p_ccbox0 elevation_ccbox)
                      (mapcar '- vecx)(-(apply '+(mapcar '* vecx p_ccbox0)))
                      (list 0 0 1)0.(* 0.2 height_ccbox_temp)
                      "CCBOXWIDTH-Y")
                 )
            )
           
           (if(< height_levelingcon_temp 1e-8)T
             (progn
               (setq entna(rac_sld(+ widthx05 offset_levelingcon_temp)
                                  (+(abs(* 0.5 width_ccbox)) offset_levelingcon_temp)
                                  height_levelingcon_temp
                                  (carxyz p_ccboxc(- elevation_ccbox height_levelingcon_temp))
                                  0(list 0 0 1)vecx(getvar "CLAYER")nil)
                     vnam(vlax-ename->vla-object entna)
                     ls_vnam(cons vnam ls_vnam)
                     )
               
               (set_xda vnam(list(cons 1000 "LEVELINGCON")
                                 (cons 1000 "OFFSET")(cons 1040 offset_levelingcon_temp)
                                 (cons 1000 "HEIGHT")(cons 1040 height_levelingcon_temp)
                                 )
                        "terraduct3d")
               (vla-put-color vnam int_colccbox_temp)
               
               ))
           
           (if(and(/= height_ccboxtop_temp 0.)(/= diam_manhole_temp 0.)p_manhole)
               (progn
                 (setq entna(pole_sld(* diam_manhole_temp 0.5)
                                     height_ccboxtop_temp
                                     (carxyz p_manhole(+ elevation_ccbox height_ccbox_temp))
                                     (list 0 0 1)(getvar "CLAYER")nil)
                       vnam(vlax-ename->vla-object entna)
                       ls_vnam(cons vnam ls_vnam)
                       )
                 
                 (set_xda vnam(list(cons 1000 "MANHOLE")
                                   (cons 1000 "CENTER-X")(cons 1040(car p_manhole))
                                   (cons 1000 "CENTER-Y")(cons 1040(cadr p_manhole))
                                   (cons 1000 "DIAM")(cons 1040 diam_manhole_temp)
                                   (cons 1000 "HEIGHT")(cons 1040 height_ccboxtop_temp)
                                   )
                          "terraduct3d")
                 (vla-put-color vnam int_colccbox_temp)
                 
                 ))


           (if(vl-catch-all-error-p
               (setq block(vl-catch-all-apply 'vla-Item(list vnam_blocktable str_ccboxname))))
               (setq block(vla-Add vnam_blockTable(vlax-3d-point 0 0 0)str_ccboxname) )
             (progn
               ;; (if vnam_currentinsert
               ;;     (progn
               ;;       (vla-delete vnam_currentinsert)
               ;;       (setq ls_vnam_visible
               ;;             (vl-remove vnam_currentinsert ls_vnam_visible)
               ;;             vnam_currentinsert nil)
               ;;       ))
               (if(setq set_ent(ssget "X"(list(cons 2 str_ccboxname))))
                   (progn
                     (setq num(sslength set_ent))
                     (while(>(setq num(1- num))-1)
                       (setq vnam(vlax-ename->vla-object(ssname set_ent num)))
                       (vla-delete vnam)
                       )
                     ))
               (vla-delete block)
               (mapcar '(lambda(v)
                          (vlax-release-object v)
                          (setq ls_vla-release(vl-remove v ls_vla-release))
                          )
                       (list block))
               (setq block(vla-Add vnam_blockTable(vlax-3d-point 0 0 0)str_ccboxname))
               )
             )

           (setq ls_vla-release(cons block ls_vla-release))

           (vla-copyobjects(vla-get-ActiveDocument(vlax-get-acad-object))
                           (vlax-make-variant
                            (vlax-safearray-fill
                             (vlax-make-safearray
                              vlax-vbObject(cons 0(1-(length ls_vnam))) )
                             ls_vnam)
                            )
                           block)
           
           (setq vnam(vla-InsertBlock
                      (vla-get-ModelSpace(vla-get-ActiveDocument(vlax-get-acad-object)))
                      (vlax-3d-point 0 0 0)str_ccboxname 1 1 1 0)
                 )
             
           (set_xda vnam(list(cons 1000 "CCBOXBLOCK")
                             (cons 1000 "PROJECT-D")(cons 1000(if str_lasground str_lasground ""))
                             (cons 1000 "PROJECT-H")(cons 1040(if height_ground height_ground 0.))
                             )
                    "terraduct3d")
           
           (mapcar '(lambda(a)(vla-delete a))ls_vnam)
           
           )
          (T
           (x-alert(list 36984 25246 12375 12383 28857 12363 12425 27161 39640 12364 21462 24471 12391 12365 12414 12379 12435 ))
           ;;選択した点から標高が取得できません
           )
          )
         
         
         (setq p_ccbox0 nil p_ccbox1 nil p_ccbox2 nil p_manhole nil p_manhole_edge nil)
         )
        ((and(or(= elem_grread 13)(= int_grread 25))
             (or str_lasground height_ground))
         (x-alert(list 28857 12398 36984 25246 12364 36275 12426 12414 12379 12435 ))
         ;;点の選択が足りません
         )
        ((and(or(= elem_grread 13)(= int_grread 25)))
         (x-alert(list 22320 34920 38754 12398 36984 25246 12364 12354 12426 12414 12379 12435 ))
         ;;地表面の選択がありません
         )
        )
       
       ))
    
    
    )

   (list
    "addattribute" ;;edsym
    (cons
     "INITIAL"
     (lambda(bool)
       (if(car bool)
           (progn
             (setq bool_point nil
                   bool_selectent nil bool_select nil int_selectmode -1
                   ls_ssget nil xtype_ssget nil xdata_ssget nil
                   )
             
             (setq func_guidemenu
                   (lambda()
                     ;;本コマンドで作成したものを選択すると属性付与できます
                     ;;\n対象をクリックしてください(範囲選択はできません)
                     (mix_strasc
                      (list 26412 12467 12510 12531 12489 12391 20316 25104 12375 12383 12418 12398 12434 36984 25246 12377 12427 12392 23646 24615 20184 19982 12391 12365 12414 12377
                             "\n" 23550 35937 12434 12463 12522 12483 12463 12375 12390 12367 12384 12373 12356 "(" 31684 22258 36984 25246 12399 12391 12365 12414 12379 12435 ")")
                      ) )
                   
                   ls_guideexplane
                   (mapcar
                    'mix_strasc
                    (list(list " - "36984 25246 12375 12390 19979 12373 12356)
                         (list " - " 20184 19982 24460 12395 32232 38598 12434 34892 12358 12392 12522 12475 12483 12488 12373 12428 12414 12377 )
                         ;;付与後に編集を行うとリセットされます
                         ) )
                   
                   ls_guidemenu
                   (list
                    (list(list nil)
                         (cons "ITEM"(list ))
                         (cons "BOOL"
                               (lambda()
                                 nil
                                 ))
                         (cons "STATUS" (lambda( / bool_depth ii ls_out)
                                          nil
                                          ))
                         )
                    
                    (list(list "ENTER");;メインメニューへ
                         (cons "ITEM"(list 12513 12452 12531 12513 12491 12517 12540 12408))
                         (cons "NEXTMODE" "home")
                         )
                    
                    )
                   )
             
             ))
       (if(cadr bool) nil)
       ))
     
    ;; (cons
    ;;  "GUIDE"
    ;;  (lambda() (list "\n" 36984 25246 12375 12390 19979 12373 12356)))
    ;;選択して下さい
    
    ;;MOVE

    (cons
     "CLICK"
     (lambda()
       (settile_att
        path_addattributedcl elem_grread
        (list(list -3(list "terraduct3d"))) )
       ))
    
    (cons
     "KEYBOAD"
     (lambda();;gr2
       (if(or(= elem_grread 13)(= int_grread 25))
           (setq str_edit "home"))
       ))
    
    )
   
   
   (list
    "influencecheck"
    (cons
     "INITIAL"
     (lambda(bool);;initial
       (if(car bool)
           (progn
             ;;temp
             (setq bool_point nil
                   bool_selectent T bool_select T int_selectmode 0
                   ls_ssget(list(cons 0 "INSERT"))
                   xtype_ssget "DUCTBLOCK" xdata_ssget "terraduct3d"
                   )
             
             (setq ls_vnam_tempinfluence(list)
                   int_execute_influence nil)
             (setq
              func_guidemenu
              (lambda()
                ;;対象に対して土被り、干渉、または両方を計算します
                ;;\n管路を対象に選択でき、その他のものは選択しようとしてもハイライトされません
                ;;\n何も選択しないとき、すべて選択したことになります
                ;;\n選択有り無し
                ;;\n作成されるオブジェクトは一時的なものでコマンドを終了すると削除されます
                ;;\n確定させたいときはSpaceコマンドを選択してください
                (mix_strasc
                 (list 23550 35937 12395 23550 12375 12390 22303 34987 12426 12289 24178 28169 12289 12414 12383 12399 20001 26041 12434 35336 31639 12375 12414 12377 "\n" 31649 36335 12434 23550 35937 12395 36984 25246 12391 12365 12289 12381 12398 20182 12398 12418 12398 12399 36984 25246 12375 12424 12358 12392 12375 12390 12418 12495 12452 12521 12452 12488 12373 12428 12414 12379 12435 "\n" 20309 12418 36984 25246 12375 12394 12356 12392 12365 12289 12377 12409 12390 36984 25246 12375 12383 12371 12392 12395 12394 12426 12414 12377
                       "\n" 36984 25246 12304
                       (if ls_vnam_select(list 26377 12426)(list 28961 12375)) 12305
                        "\n" 20316 25104 12373 12428 12427 12458 12502 12472 12455 12463 12488 12399 19968 26178 30340 12394 12418 12398 12391 12467 12510 12531 12489 12434 32066 20102 12377 12427 12392 21066 38500 12373 12428 12414 12377 "\n" 30906 23450 12373 12379 12383 12356 12392 12365 12399 "Space" 12467 12510 12531 12489 12434 36984 25246 12375 12390 12367 12384 12373 12356
                        
                       )
                 ) )
              
              ls_guideexplane
              (mapcar
               'mix_strasc
               (list(list " - " 31649 36335 12434 36984 25246 12375 12390 23455 34892 )
                    (list " - " 20309 12418 36984 25246 12375 12394 12356 12392 12365 12289 20316 25104 12375 12383 12377 12409 12390 12398 31649 36335 12364 23550 35937 12395 12394 12427 )
                    (list " - " 36984 25246 26178 12395 "Enter," 21491 12463 12522 12483 12463 12391 20840 36984 25246 35299 38500 )
                    ))
              ;;管路を選択して実行
              ;;何も選択しないとき、作成したすべての管路が対象になる
              ;;選択時にEnter,右クリックで全選択解除
              
              
              ls_guidemenu
              (list
               
               (list(list 76);; 地表面標高
                    (cons "ITEM"(list 22320 34920 38754 27161 39640))
                    (cons "STATUS"
                          (lambda()
                            (if str_lasground
                                (mix_strasc
                                 (list "{\\C" str_gcol_c ";" 12487 12540 12479 "} : "
                                       (vl-string-subst "" "lasgrid-" str_lasground)))
                              (if height_ground
                                  (mix_strasc
                                   (list"{\\C" str_gcol_g ";" 27161 39640 "} : "
                                        (as-numstr height_ground)))
                                (mix_strasc
                                 (list "{\\C" str_gcol_r ";"
                                       36984 25246 12373 12428 12390 12356 12414 12379 12435 "}"))
                                ))
                            ))
                    (cons "LOADFUNCTION"(lambda()(settile_selectground)))
                    (cons "HELP"(lambda()(mix_strasc(list 20351 29992 12377 12427 27161 39640 12434 "\n- las" 35501 36796 "\n- xml" 35501 36796 "\n- " 19968 23450 27161 39640 12398 25968 20516 20837 21147 "\n" 12363 12425 36984 25246 12391 12365 12414 12377 ))))
                    )
               
               (list(list 49);;土被り許容値に満たない箇所を一時表示
                    (cons "ITEM"(list 22303 34987 12426 35377 23481 20516 12395 28288 12383 12394 12356 31623 25152 12434 19968 26178 34920 31034  ))
                    (cons "NEXTMODE" str_edit);;nilでは先に進めない
                    (cons "LOADFUNCTION"
                          (lambda()
                            (setq str_next nil
                                  int_execute_influence 1)
                            ))
                    (cons "CLICKFUNCTION"
                          (lambda()(setq bool_replacegrread T int_grread 2 elem_grread 49) ))
                    
                    )

               (list(list 50);;Q土被り許容値
                    (cons "ITEM"(list 22303 34987 12426 35377 23481 20516 ))
                    (cons "INPUT"(lambda()
                                   (if allow_cover_temp T
                                     (setq allow_cover_temp allow_cover))
                                   'allow_cover_temp))
                    (cons "LOADUNCTION"
                          (lambda()nil))

                    ;;土被りがこの値よりも小さいものが表記の対象となります
                    (cons "HELP"(lambda()(mix_strasc(list 22303 34987 12426 12364 12371 12398 20516 12424 12426 12418 23567 12373 12356 12418 12398 12364 34920 35352 12398 23550 35937 12392 12394 12426 12414 12377 (if int_selectmenu str_guide_inputval str_guide_selectval) ))))
                    
                    )
               
               ;; (list(list 65);;A土被り表示ピッチ
               ;;      (cons "ITEM"(list 22303 34987 12426 34920 31034 12500 12483 12481 ))
               ;;      (cons "INPUT"(lambda()
               ;;                     (if pitch_cover_temp T
               ;;                       (setq pitch_cover_temp pitch_cover))
               ;;                     'pitch_cover_temp))
               ;;      (cons "LOADUNCTION"
               ;;            (lambda()nil))
               ;;      )

               (list(list 51);;Z土被りNG色
                    (cons "ITEM"(list 22303 34987 12426 "NG" 33394 ))
                    (cons "INPUTCOLOR"(lambda()
                                        (if int_colcoverng_temp T(setq int_colcoverng_temp int_colcoverng))
                                        'int_colcoverng_temp))
                    (cons "LOADFUNCTION"
                          (lambda()
                            nil
                            ))
                    )
               
               (list(list 81);;Q干渉許容値に満たない箇所を一時表示
                    (cons "ITEM"(list 24178 28169 35377 23481 20516 12395 28288 12383 12394 12356 31623 25152 12434 19968 26178 34920 31034 ))
                    (cons "NEXTMODE" str_edit);;nilでは先に進めない
                    (cons "LOADFUNCTION"
                          (lambda()
                            (setq str_next nil
                                  int_execute_influence 2)
                            ))
                    (cons "CLICKFUNCTION"
                          (lambda()(setq bool_replacegrread T int_grread 2 elem_grread 81) ))
                    
                    )
               
               (list(list 87);;W干渉許容値
                    (cons "ITEM"(list 24178 28169 35377 23481 20516 ))
                    (cons "INPUT"(lambda()
                                   (if allow_influence_temp T
                                     (setq allow_influence_temp allow_influence))
                                   'allow_influence_temp))
                    (cons "LOADUNCTION"
                          (lambda()nil))
                    ;;2つの管の表面から表面までの最短距離がこの値よりも小さいとき表示します\nわずかな干渉を許すときは負の値を入力してください
                    (cons "HELP"(lambda()(mix_strasc(list "2" 12388 12398 31649 12398 34920 38754 12363 12425 34920 38754 12414 12391 12398 26368 30701 36317 38626 12364 12371 12398 20516 12424 12426 12418 23567 12373 12356 12392 12365 34920 31034 12375 12414 12377 "\n" 12431 12378 12363 12394 24178 28169 12434 35377 12377 12392 12365 12399 36000 12398 20516 12434 20837 21147 12375 12390 12367 12384 12373 12356 (if int_selectmenu str_guide_inputval str_guide_selectval) ))))
                    )
               
               (list(list 69);;E干渉NG色
                    (cons "ITEM"(list 24178 28169 "NG" 33394  ))
                    (cons "INPUTCOLOR"
                          (lambda()(if int_colinfluence_temp T(setq int_colinfluence_temp int_colinfluence))
                            'int_colinfluence_temp))
                    (cons "LOADFUNCTION"
                          (lambda()
                            nil
                            ))
                    )

               (list(list 65);;A土被りと干渉どちらも表示
                    (cons "ITEM"(list 22303 34987 12426 12392 24178 28169 12393 12385 12425 12418 34920 31034 ))
                    (cons "NEXTMODE" str_edit);;nilでは先に進めない
                    (cons "LOADFUNCTION"
                          (lambda()
                            (setq str_next nil
                                  int_execute_influence 3)
                            ))
                    (cons "CLICKFUNCTION"
                          (lambda()(setq bool_replacegrread T int_grread 2 elem_grread 65) ))
                    )
               
               (list(list 32);;一時表示オブジェクトを残す
                    (list "ITEM"(list 19968 26178 34920 31034 12458 12502 12472 12455 12463 12488 12434 27531 12377 ))
                    (cons "NEXTMODE" str_edit)
                    (cons "LOADFUNCTION"
                          (lambda()
                            (if ls_vnam_tempinfluence
                                (setq str_next nil
                                      int_execute_influence 32)
                              (progn
                                (x-alert(list 23550 35937 12364 12354 12426 12414 12379 12435 ))
                                nil)
                              )
                            ))
                    ;;
                    
                    )
               
               (list(list 8);;一時オブジェクトを消す
                    (cons "ITEM"(list  19968 26178 12458 12502 12472 12455 12463 12488 12434 28040 12377 ))
                    (cons "LOADFUNCTION"
                          (lambda()
                            (mapcar '(lambda(vnam)
                                       (vla-delete vnam)
                                       (exckillobj vnam) )
                                    ls_vnam_tempinfluence)
                            (setq ls_vnam_tempinfluence nil)
                            ))
                    )
               
               (list(list 80 );;P選択モード切替
                    (cons "ITEM"(list 36984 25246 12514 12540 12489 20999 26367 ))
                    (cons "INPUTSWITCH"
                          (lambda()
                            (list 'int_selectmode
                                  (mapcar 'mix_strasc
                                          (list(list "{\\C" str_gcol_y ";" 36984 25246 "}")
                                               (list "{\\C" str_gcol_c ";" 35299 38500 "}")))
                                  )
                            ) )
                    (cons "HELP"(lambda()str_guide_selectmode))
                    )
               
               (list(list nil)
                    (cons "ITEM"(list ))
                    (cons "BOOL"
                          (lambda()
                            T
                            ))
                    (cons
                     "STATUS"
                     (lambda( / bool_depth ii ls_out)
                       (if ls_vnam_select
                           (list(list(mix_strasc(list "{\\C" str_gcol_y ";"
                                                      9733 "  " 36984 25246 20013 "}")))))
                       ))
                    )
               
               (list(list "ENTER");;メインメニューへ
                    (cons "ITEM"(list 12513 12452 12531 12513 12491 12517 12540 12408))
                    (cons "LOADFUNCTION"
                          (lambda()
                            (mapcar '(lambda(vnam)
                                       (vla-delete vnam)
                                       (exckillobj vnam) )
                                    ls_vnam_tempinfluence)
                            (setq ls_vnam_tempinfluence nil)
                            ))
                    (cons "NEXTMODE" "home")
                    )
               
               )
              
              )
             
             ))
       
       (if(cadr bool) (list ) )
       ))

    ;;MOVE
    ;;CLICK

    (cons
     "KEYBOAD"
     (lambda( / bool_default ls_solidinfluence vec_normal dist_normal)

       (cond
        ((or(= elem_grread 13)(= int_grread 25))
         (mapcar '(lambda(v)(vla-Highlight v :vlax-false))ls_vnam_select)
         (setq ls_vnam_select nil ls_vnam_highlight nil)
         )
        ((vl-position int_execute_influence(list 1 2 3))
         (setq vec_normal(unit_vector(carxyz vec_view 0.)))
         
         (if ls_vnam_select T
           ((lambda( / num ls_out vnam)
              (setq set_ent(ssget "X"(list(cons 0 "INSERT")(list -3(list "terraduct3d"))))
                    num(if set_ent(sslength set_ent)0) )
              (while(>(setq num(1- num))-1)
                (setq vnam(vlax-ename->vla-object(ssname set_ent num))
                      ls_vnam_select(cons vnam ls_vnam_select))
                )
              ))
           )
         
         (mapcar
          '(lambda(vnam / length_road str d nn vnam_line vnam_solid rr ls_p )
             (setq str(vla-get-name vnam)
                   vnam(vla-Item(vla-get-Blocks (vla-get-ActiveDocument(vlax-get-acad-object)))str)
                   )
             
             (vlax-for
              obj vnam
              
              (if(and vnam_solid vnam_line) T
                (progn
                  
                  (vla-getXData obj "terraduct3d" 'array_Type 'array_Data )
                  (setq ls_xdata
                        (if array_data
                            (split_list 0(mapcar 'vlax-variant-value
                                                 (vlax-safearray->list array_data))))
                        str_type(cdr(assoc "terraduct3d" ls_xdata))
                        )

                  (cond
                   ((= str_type "DUCTSOLID")
                    (setq rr(* 0.5(cdr(assoc "DIAM" ls_xdata))))
                    )
                   ((= str_type "CENTERLINE")
                    (setq vnam_line obj)
                    )
                   )
                  ))
              )

             (if(and vnam_line rr)
                 (progn
                   (setq length_road(vla-get-length vnam_line)
                         nn(1+(fix(/ length_road rr)))
                         d(/ length_road nn)
                         ls_p(list(vlax-curve-getendpoint vnam_line))
                         )
                   (while(< 1e-8(setq length_road(- length_road d)))
                     (setq ls_p(cons(vlax-curve-getpointatdist vnam_line length_road)ls_p))
                     )
                   (setq ls_p(reverse(cons(vlax-curve-getstartpoint vnam_line)ls_p)))
                   )
               
               )
             
             (if(and(vl-position int_execute_influence(list 1 3))rr)
                 (mapcar
                  '(lambda(p / vec pp d cosz )
                     (setq pp(car(project_to_ground
                                  (list p)(list 0. 0. 1.)(list str_lasground height_ground)))
                           d(distance p pp)
                           vec(xvla-normal vnam_line p)
                           cosz(sqrt(- 1.(expt(caddr vec)2)))
                           )
                     (if(< cosz 1e-8)T
                       (if(<(+(caddr pp)(-(caddr p))(/ rr cosz))allow_cover_temp)
                           (progn
                             (setq dist_normal(apply '+(mapcar '* vec_normal p))
                                   entna(make_2pdimension
                                         nil(list pp(mapcar '+ p(list 0 0(/ rr cosz)))
                                                  vec_normal dist_normal nil(* 0.5 pi)0.
                                                  "CD=<>" str_dimstyle_ductlevel))
                                   vnam(vlax-ename->vla-object entna)
                                   ls_vnam_tempinfluence
                                   (cons vnam ls_vnam_tempinfluence)
                                   )

                             (vla-put-Arrowhead1Type vnam 6)
                             (vla-put-Arrowhead2Type vnam 6)
                             (vla-put-ArrowheadSize vnam(max 0.1(* 0.1 allow_cover_temp)))
                             (vla-put-ExtLine1Suppress vnam -1)
                             (vla-put-ExtLine2Suppress vnam -1)
                             (vla-put-Textgap vnam(max 0.01(* 0.01 allow_cover_temp)))
                             (vla-put-TextHeight vnam(max 0.04(* 0.04 allow_cover_temp)))
                             
                             
                             (vla-put-color vnam int_colcoverng_temp)
                             (addkillobj vnam)
                             )))
                     )
                  ls_p)
               )
             
             (if(and(vl-position int_execute_influence(list 2 3))vnam_line rr)
                 (progn
                   (setq vnam
                         (vla-CopyObjects
                          (vla-get-ActiveDocument(vlax-get-acad-object))
                          (vlax-make-variant
                           (vlax-safearray-fill
                            (vlax-make-safearray vlax-vbObject (cons 0 0))
                            (list vnam_line)))
                          (vla-get-ModelSpace(vla-get-ActiveDocument(vlax-get-acad-object)))
                          )
                         vnam(car(vlax-safearray->list(vlax-variant-value vnam)))
                         ls_solidinfluence(cons(cons rr(cons vnam ls_p))ls_solidinfluence)
                         )
                   (addkillobj vnam)
                   ) )
             
             )
          ls_vnam_select)

         
         (if(vl-position int_execute_influence(list 2 3))
             ((lambda(ls_origin
                      / ls_another v0 v1 vnam r0 r1 ls_pc vec0 vec1 ls_vec0
                      vec_close dist_close
                      )
                (while ls_origin
                  (setq ls_p(car ls_origin)ls_origin(cdr ls_origin)
                        r0(car ls_p)v0(cadr ls_p)ls_p(cddr ls_p)
                        ls_another ls_origin
                        ls_vec0(mapcar '(lambda(p)(xvla-normal v0 p))ls_p)
                        )
                  
                  (while ls_another
                    (setq lst(car ls_another)ls_another(cdr ls_another)
                          r1(car lst)v1(cadr lst)
                          )
                    (mapcar
                     '(lambda(p vec0 / vec p_close d cosz sinh)
                        (setq p_close(vlax-curve-getclosestpointto v1 p nil))
                        (if(cond
                            ((<(setq dist_close(distance p p_close))
                               (+ r0 r1 allow_influence_temp))
                             T)
                            (T nil)
                            
                            ((progn;;直角でない方向を考えていたが不要
                               (setq vec_close(mapcar '- p_close p)
                                     vec_close(mapcar '(lambda(a)(/ a dist_close))vec_close))
                               (<(setq sinh(distance(cross_product vec_close vec0)(list 0 0 0)))1e-8)
                               )
                             nil)
                            ((progn
                               (setq dist_close(- dist_close(/ r0 sinh))
                                     vec1(xvla-normal v1 p_close))
                               (<(setq sinh(distance(cross_product vec_close vec1)(list 0 0 0)))1e-8)
                               )
                             nil)
                            (T
                             (setq dist_close(- dist_close(/ r1 sinh)))
                             (< dist_close allow_influence_temp)
                             )
                            
                            )
                            (progn
                              (setq vec_normal(cross_product(mapcar '- p p_close)vec_view)
                                    vec_normal
                                    (if(<(distance(list 0 0 0)vec_normal)1e-8)
                                        (unit_vector(cross_product(mapcar '- p p_close)vec_x_onview))
                                      (unit_vector(cross_product vec_normal(mapcar '- p p_close))))
                                    )
                              
                              (setq dist_normal(apply '+(mapcar '* vec_normal p))
                                    entna(make_2pdimension
                                          nil(list p p_close vec_normal dist_normal nil
                                                   ((lambda(p0 p1)
                                                      (angle(carxy p0)(carxy p1)) )
                                                    (trans-x p(list 0 0 1)vec_normal)
                                                    (trans-x p_close(list 0 0 1)vec_normal))
                                                   0. "IR=<>" str_dimstyle_ductlevel))
                                    vnam(vlax-ename->vla-object entna)
                                    ls_vnam_tempinfluence
                                    (cons vnam ls_vnam_tempinfluence)
                                    )

                              (vla-put-Arrowhead1Type vnam 6)
                              (vla-put-Arrowhead2Type vnam 6)
                              (vla-put-ArrowheadSize vnam(max 0.1(* 0.1 allow_influence_temp)))
                              (vla-put-ExtLine1Suppress vnam -1)
                              (vla-put-ExtLine2Suppress vnam -1)
                              (vla-put-ExtLineFixedLenSuppress vnam -1)
                              (vla-put-ForceLineInside vnam -1)
                              (vla-put-TextGap vnam(max 0.01(* 0.01 allow_influence_temp)))
                              (vla-put-TextHeight vnam(max 0.04(* 0.04 allow_influence_temp)))
                              (vla-put-TextInside vnam -1)
                              (vla-put-TextInsideAlign vnam 0)
                              (vla-put-TextOutsideAlign vnam 0)
                              (vla-put-VerticalTextPosition vnam 1)

                              
                              (vla-put-color vnam int_colinfluence_temp)
                              (addkillobj vnam)
                              
                              ))
                        )
                     ls_p ls_vec0)
                    
                    
                    
                    
                    )
                  )
                )
              ls_solidinfluence)
           )
         
         
         )
        ((= int_execute_influence 32)
         (mapcar '(lambda(vnam)(exckillobj vnam))ls_vnam_tempinfluence)
         (setq ls_vnam_tempinfluence nil bool_default T)
         
         )
        (T(setq bool_default T) )
        )
       (setq int_execute_influence nil)
       (if bool_default T
         (progn
           (mapcar '(lambda(v)(vla-Highlight v :vlax-false))ls_vnam_select)
           (setq ls_vnam_select nil ls_vnam_highlight nil)
           ))
       
       
       ))
    
    )
   
   (list
    "savecsvifc" ;;edsym
    (cons
     "INITIAL"
     (lambda(bool);;initial
       (if(car bool)
           (progn
             ;;temp
             (setq bool_point nil
                   bool_selectent T bool_select T int_selectmode 0
                   ls_ssget(list(cons 0 "INSERT"))
                   xtype_ssget "DUCTBLOCK,CCBOXBLOCK" xdata_ssget "terraduct3d"
                   )
             
             (setq int_overwrite_readcsv 0)
             (setq int_savecsvifc nil)
             
             (setq
              func_guidemenu
              (lambda()
                ;;投影した経路、管路、特殊部を対象に選択でき、その他のものは選択しようとしてもハイライトされません
                ;;\n何も選択しないとき、すべて選択したことになります
                ;;\n選択有り無し
                (mix_strasc
                 (list 25237 24433 12375 12383 32076 36335 12289 31649 36335 12289 29305 27530 37096 12434 23550 35937 12395 36984 25246 12391 12365 12289 12381 12398 20182 12398 12418 12398 12399 36984 25246 12375 12424 12358 12392 12375 12390 12418 12495 12452 12521 12452 12488 12373 12428 12414 12379 12435
                       "\n" 20309 12418 36984 25246 12375 12394 12356 12392 12365 12289 12377 12409 12390 36984 25246 12375 12383 12371 12392 12395 12394 12426 12414 12377
                       "\n" 36984 25246 12304
                       (if ls_vnam_select(list 26377 12426)(list 28961 12375)) 12305
                       )
                 ) )
              
              ls_guideexplane
              (mapcar
               'mix_strasc
               (list(list " - " 20309 12418 36984 25246 12375 12390 12356 12394 12356 12392 12365 22259 38754 20869 12398 12377 12409 12390 12364 23550 35937 12395 12394 12426 12414 12377 )
                    ))
              ;;何も選択していないとき図面内のすべてが対象になります
              
              ls_guidemenu
              (list
               
               (list(list 49);;CSV保存
                    (cons "ITEM"(list "CSV" 20445 23384  ))
                    (cons "NEXTMODE" str_edit);;
                    (cons "LOADFUNCTION"
                          (lambda()(setq str_next nil int_savecsvifc 1) ))
                    (cons "CLICKFUNCTION"
                          (lambda()(setq bool_replacegrread T int_grread 2 elem_grread 49) ))
                    
                    )
               
               (list(list 50);;CSV読込
                    (cons "ITEM"(list "CSV" 35501 36796 ))
                    (cons "NEXTMODE" str_edit);;
                    (cons "LOADFUNCTION"
                          (lambda()(setq str_next nil int_savecsvifc 2) ))
                    (cons "CLICKFUNCTION"
                          (lambda()(setq bool_replacegrread T int_grread 2 elem_grread 50) ))
                    
                    )

               (list(list 51);;CSV読込み時の名称上書き
                    (list "ITEM"(list "CSV" 35501 36796 12415 26178 12398 21517 31216 19978 26360 12365 ))
                    (cons "INPUTSWITCH"
                          (lambda()
                            (list 'int_overwrite_readcsv
                                  (mapcar 'mix_strasc
                                          (list(list "{\\C" str_gcol_y ";" 31105 27490 "}")
                                               (list "{\\C" str_gcol_c ";" 35377 21487 "}")))
                                  )
                            ) )
                    ;;読み込んだ管や特殊部の名称が現在図面内に存在するものと一致するときにどうするか設定してください
                    ;;許可すると現在図面内に存在する同名オブジェクトはすべて削除されます
                    ;;完了時に読み込みできなかったオブジェクト（禁止時）、上書きしたオブジェクト（許可時）を報告し、クリップボードに貼り付けます
                    (cons "HELP"
                          (lambda()
                            (mix_strasc
                             (list 35501 12415 36796 12435 12384 31649 12420 29305 27530 37096 12398 21517 31216 12364 29694 22312 22259 38754 20869 12395 23384 22312 12377 12427 12418 12398 12392 19968 33268 12377 12427 12392 12365 12395 12393 12358 12377 12427 12363 35373 23450 12375 12390 12367 12384 12373 12356
                                   "\n" 35377 21487 12377 12427 12392 29694 22312 22259 38754 20869 12395 23384 22312 12377 12427 21516 21517 12458 12502 12472 12455 12463 12488 12399 12377 12409 12390 21066 38500 12373 12428 12414 12377
                                   "\n" 23436 20102 26178 12395 35501 12415 36796 12415 12391 12365 12394 12363 12387 12383 12458 12502 12472 12455 12463 12488 65288 31105 27490 26178 65289 12289 19978 26360 12365 12375 12383 12458 12502 12472 12455 12463 12488 65288 35377 21487 26178 65289 12434 22577 21578 12375 12289 12463 12522 12483 12503 12508 12540 12489 12395 36028 12426 20184 12369 12414 12377 ))))
                    )
               
               (list(list 52);;IFC保存
                    (cons "ITEM"(list "IFC" 20445 23384  ))
                    (cons "NEXTMODE" str_edit);;
                    (cons "LOADFUNCTION"
                          (lambda()(setq str_next nil int_savecsvifc 3) ))
                    (cons "CLICKFUNCTION"
                          (lambda()(setq bool_replacegrread T int_grread 2 elem_grread 52) ))
                    
                    )
               
               (list(list 80 );;P選択モード切替
                    (cons "ITEM"(list 36984 25246 12514 12540 12489 20999 26367 ))
                    (cons "INPUTSWITCH"
                          (lambda()
                            (list 'int_selectmode
                                  (mapcar 'mix_strasc
                                          (list(list "{\\C" str_gcol_y ";" 36984 25246 "}")
                                               (list "{\\C" str_gcol_c ";" 35299 38500 "}")))
                                  )
                            ) )
                    (cons "HELP"(lambda()str_guide_selectmode))
                    )
               
               (list(list nil)
                    (cons "ITEM"(list ))
                    (cons "BOOL"
                          (lambda()
                            nil
                            ))
                    (cons "STATUS" (lambda( / bool_depth ii ls_out) "" ))
                    )
               
               (list(list "ENTER");;メインメニューへ
                    (cons "ITEM"(list 12513 12452 12531 12513 12491 12517 12540 12408))
                    (cons "NEXTMODE" "home")
                    )
               
               )
              
              
              )
             
             ))
       
       (if(cadr bool) (list ) )
       ))
    
    
    (cons
     "KEYBOAD"
     (lambda( / ls_str_out num ls_out_parameter str_out)
       
       (cond
        ((null int_savecsvifc))
        ((vl-position int_savecsvifc(list 1 3))
         (if ls_vnam_select T
           ((lambda( / num ls_out vnam)
              (setq set_ent(ssget "X"(list(cons 0 "INSERT")(list -3(list "terraduct3d"))))
                    num(if set_ent(sslength set_ent)0) )
              (while(>(setq num(1- num))-1)
                (setq vnam(vlax-ename->vla-object(ssname set_ent num))
                      ls_vnam_select(cons vnam ls_vnam_select))
                )
              ))
           )
         
         (if(= int_savecsvifc 1)
             (setq ls_str_out
                   (list "DATA"))
                    ;; (mix_strasc;;読込時にEXPLANE列は不要で、DATA列より右が必要です
                    ;;  (list "EXPLANE,DATA," 35501 36796 26178 12395 "EXPLANE" 21015 12399
                    ;;        19981 35201 12391 12289 "DATA" 21015 12424 12426
                    ;;        21491 12364 24517 35201 12391 12377 )))) )
           (setq ls_ifcparameter
                 (list "type" "name" "r" "g" "b" "properties" "x" "y" "z" "dx" "dy" "radius" "thickness"
                       "ex" "ey" "ez" "rotate" "height" "shapename" "face" "xx" "rebar")
                 ls_str_out
                 (cons(substr(apply 'strcat(mapcar '(lambda(str)(strcat "," str))ls_ifcparameter))2)
                      ls_str_out)
                 )
           )

         (setq ls_str_out_duct(list)
               ls_str_out_ccbox(list)
               ls_str_out_road(list)
               )
         
         (mapcar
          '(lambda(vnam)
             (vla-getXData vnam "terraduct3d" 'array_Type 'array_Data )
             (setq ls_xdata
                   (if array_data
                       (split_list 0(mapcar 'vlax-variant-value
                                            (vlax-safearray->list array_data))))
                   str_type(cdr(assoc "terraduct3d" ls_xdata))
                   )

             (vla-getXData vnam "attributedata" 'array_Type 'array_Data )
             (setq ls_xdata_att
                   (if array_data
                       (mapcar '(lambda(lst)(strcat(car lst)","(cadr lst)))
                               (split_list 2(cdr(mapcar 'vlax-variant-value
                                                        (vlax-safearray->list array_data)))) ))
                   )

             ;; (setq p_insert(vlax-safearray->list(vlax-variant-value(vla-get-insertionpoint vnam)))
             ;;       rotation_insert(vla-get-rotation vnam)
             ;;       vec_insert(vlax-safearray->list(vlax-variant-value(vla-get-normal vnam)))
             ;;       x_scale(vla-get-XScaleFactor vnam)
             ;;       transMat(vlax-tmatrix (list(list 0 -1 0 0)
             ;;                                  (list 1 0 0 0)
             ;;                                  (list 0 0 1 0)
             ;;                                  (list 0 0 0 1)))
             ;;       )
             ;; (vla-scaleentity vnam(vlax-3d-point 0 0 0)x_scale)
             ;; (vla-Rotate3D vnam(vlax-3d-point 0 0 0)(vlax-3d-point 0 0 1) rotation_insert)
             ;; (vla-Move vnam(vlax-3d-point 0 0 0)(vlax-3d-point p_insert))
             ;; ;; (vla-TransformBy vnam transMat)

             (setq str(vla-get-name vnam)
                   vnam(vla-Item(vla-get-Blocks (vla-get-ActiveDocument(vlax-get-acad-object)))str)
                   ls_out_parameter(list)
                   ls_search(list) num_min nil num_max nil
                   )
             
             (cond
              ((= str_type "DUCTBLOCK")
               (if(= int_savecsvifc 1)
                   (setq str_out(strcat "DUCTBLOCK," str) )
                 (setq ls_out_parameter(cons(cons "NAME" str)ls_out_parameter))
                 )
               
               (vlax-for
                obj vnam
                (vla-getXData obj "terraduct3d" 'array_Type 'array_Data )
                (setq ls_xdata
                      (if array_data
                          (split_list 0(mapcar 'vlax-variant-value
                                               (vlax-safearray->list array_data))))
                      str_type(cdr(assoc "terraduct3d" ls_xdata))
                      )
                
                (cond
                 ((or(= str_type "DEPTH")(= str_type "ARC"))
                  (setq int_line(cdr(assoc "NUM" ls_xdata))
                        int_side(cdr(assoc "SIDE" ls_xdata))
                        )
                  (if num_min(setq num_min(min num_min int_line)
                                   num_max(max num_max int_line))
                    (setq num_min int_line num_max int_line))
                  (setq lst(cons(list str_type int_line int_side)
                                (cons(cons "OBJ"(vlax-vla-object->ename obj))ls_xdata))
                        ls_search(cons lst ls_search))
                  )
                 ((= str_type "DUCTSOLID")
                  
                  (setq ls_out_parameter
                        (cons(assoc "DIAM" ls_xdata)ls_out_parameter)
                        )
                  (if(= int_savecsvifc 1)
                      (setq ls_out_parameter
                            (cons(cons "COLDUCT"(vla-get-color obj))ls_out_parameter)
                            )
                    (setq ls_out_parameter
                          (cons(cons "TCOLDUCT"
                                     ((lambda(vtc)
                                        (mapcar '(lambda(a)(a vtc))
                                                (list vla-get-red vla-get-green vla-get-blue)))
                                      (vla-get-truecolor obj)))
                               ls_out_parameter)
                          )
                    )
                  
                  )
                 ((= str_type "CONMESH")
                  
                  (setq ls_out_parameter
                        (cons(cons "COLCON"(vla-get-color obj))ls_out_parameter) )
                  (if(= int_savecsvifc 1)
                      (setq ls_out_parameter
                            (cons(cons "TYPE"(if(<(cdr(assoc "CORNER" ls_xdata))0)1 0))ls_out_parameter)
                            ls_out_parameter
                            (cons(assoc "WIDTH" ls_xdata)ls_out_parameter)
                            ls_out_parameter
                            (cons(assoc "HEIGHT" ls_xdata)ls_out_parameter)
                            ls_out_parameter
                            (cons(assoc "FILET" ls_xdata)ls_out_parameter)
                            )
                    (setq ls_out_parameter(cons(cons "MESHOBJ" obj)ls_out_parameter)
                          ls_out_parameter
                          (cons(cons "TCOLMESH"
                                     ((lambda(vtc)
                                        (mapcar '(lambda(a)(a vtc))
                                                (list vla-get-red vla-get-green vla-get-blue)))
                                      (vla-get-truecolor obj)))
                               ls_out_parameter)
                          )
                    
                    )
                  )
                 ((= str_type "CENTERLINE")
                  (setq ls_out_parameter(cons(cons "CENTERLINE" obj)ls_out_parameter))
                  
                  )
                 )
                )

               (if(= int_savecsvifc 1)
                   (progn
                     (mapcar
                      '(lambda(lst / str str_initial)
                         (if(setq str(cdr(assoc(car lst)ls_out_parameter)))T
                           (setq str(cadr lst)))
                         (setq str_out(strcat str_out ","(as-numstr str))))
                      (list(list "COLDUCT")(list "COLCON")
                           (list "DIAM" diam_duct)(list "TYPE" 2)
                           (list "WIDTH" width_protect)
                           (list "HEIGHT" height_protect)
                           (list "FILET" filet_protect)
                           )
                      )
                     (setq ls_str_out_duct(cons(strcat str_out)ls_str_out_duct)
                           ls_str_out_duct
                           (cons(strcat "ATTRIBUTE"
                                        (apply 'strcat
                                               (mapcar '(lambda(str)(strcat "," str))
                                                       ls_xdata_att
                                                       )))
                                ls_str_out_duct)
                           )
                     )
                 )
               
               (setq num_min(1- num_min) ls_line(list)
                     ls_roadfrominsert(list)
                     num_temp num_min
                     str_out ""
                     )

               
               (if(= int_savecsvifc 1)
                   (while(<=(setq num_temp(1+ num_temp))num_max)
                     
                     (mapcar
                      '(lambda(int_side / ls_gcode)
                         (setq lst(assoc(list "DEPTH" num_temp int_side)ls_search)
                               ls_search(vl-remove lst ls_search)
                               hand_road(cdr(assoc "ROAD" lst))
                               ls_gcode(entget(cdr(assoc "OBJ" lst)))
                               )

                         (setq str_out(apply
                                       'strcat
                                       (mapcar
                                        '(lambda(ii / lst)
                                           (setq lst(cdr(assoc ii ls_gcode)))
                                           (if(=(type lst)'LIST)
                                               (apply
                                                'strcat
                                                (mapcar '(lambda(a)(strcat(as-numstr a)"," ))lst))
                                             (strcat(as-numstr lst)","))
                                           )
                                        (list 13 14 3)))
                               str_out(strcat "DEPTH," str_out(if hand_road hand_road ""))
                               ls_str_out_duct(cons str_out ls_str_out_duct)
                               )
                         
                         )
                      (list 0 1))
                     
                     
                     (if(assoc(list "ARC" num_temp 0)ls_search)
                         (mapcar
                          '(lambda(int_side / ls_gcode)
                             (setq lst(assoc(list "ARC" num_temp int_side)ls_search)
                                   ls_search(vl-remove lst ls_search)
                                   ls_gcode(entget(cdr(assoc "OBJ" lst)))
                                   str_out(apply
                                           'strcat
                                           (mapcar
                                            '(lambda(ii / lst)
                                               (setq lst(cdr(assoc ii ls_gcode)))
                                               (if(=(type lst)'LIST)
                                                   (apply
                                                    'strcat
                                                    (mapcar '(lambda(a)(strcat(as-numstr a)","))lst))
                                                 (strcat(as-numstr lst)","))
                                               )
                                            (list 15 13 14 10 11 210 3)))
                                   str_out(strcat "ARC," str_out)
                                   ls_str_out_duct(cons str_out ls_str_out_duct)
                                   )
                             )
                          (list 0 1))
                       )
                     )
                 
                 
                 (if T
                     (progn;;REBARTYPE
                       (setq lst(assoc(list "DEPTH"(1+ num_temp)0)ls_search)
                             ls_search(vl-remove lst ls_search)
                             ls_gcode(entget(cdr(assoc "OBJ" lst)))
                             str_out ""
                             )

                       (mapcar '(lambda(a)(setq str_out(strcat str_out ","(as-numstr a))))
                               (cdr(assoc 14 ls_gcode)))
                       (setq str_out(substr str_out 2))
                       
                       (while(assoc(list "ARC"(setq num_temp(1+ num_temp))0)ls_search)
                         (mapcar
                          '(lambda(int_side / ls_gcode)
                             (setq lst(assoc(list "ARC" num_temp int_side)ls_search)
                                   ls_search(vl-remove lst ls_search)
                                   ls_gcode(entget(cdr(assoc "OBJ" lst)))
                                   )
                             
                             (mapcar
                              '(lambda(lst)
                                 (setq str_out(strcat str_out(car lst)))
                                 (mapcar '(lambda(a)(setq str_out(strcat str_out ","(as-numstr a))))
                                         (cdr lst))
                                 )
                              (list
                               (if(= int_side 0)
                                   (cons "|POLY" (cdr(assoc 13 ls_gcode)))
                                 (list ""))
                               (cons "|ARC"(cdr(assoc 14 ls_gcode)))
                               (cons "" (cdr(assoc 15 ls_gcode)))
                               )
                              )
                             
                             )
                          (list 0 1))
                         )

                       (setq lst(assoc(list "DEPTH" num_temp 1)ls_search)
                             ls_search(vl-remove lst ls_search)
                             ls_gcode(entget(cdr(assoc "OBJ" lst)))
                             str_out(strcat str_out "|POLY")
                             )
                       
                       (mapcar '(lambda(a)(setq str_out(strcat str_out ","(as-numstr a)) ))
                               (cdr(assoc 14 ls_gcode)))
                       
                       (setq ls_out_parameter(cons(cons "REBAR" str_out)ls_out_parameter))
                       
                       )
                   (progn;;FACETYPE
                     
                     
                     
                     
                     )
                   )
                 )
               
               (if(= int_savecsvifc 1)
                   (setq ls_str_out_duct(cons "DUCTBLOCKEND," ls_str_out_duct)
                         
                         )


                 (if nil
                     (progn;;REBARTYPE
                       (setq ls_rgb(cdr(assoc "TCOLDUCT" ls_out_parameter))
                             str(apply
                                 'strcat
                                 (mapcar
                                  '(lambda(str / strx strc)
                                     (setq strx
                                           (cond
                                            ((= str "type")"REBAR")
                                            ((= str "name")(cdr(assoc "NAME" ls_out_parameter)))
                                            ((= str "r")(as-numstr(/(car ls_rgb)255.)))
                                            ((= str "g")(as-numstr(/(cadr ls_rgb)255.)))
                                            ((= str "b")(as-numstr(/(caddr ls_rgb)255.)))
                                            ((= str "properties")
                                             (substr (apply 'strcat
                                                            (mapcar '(lambda(str)(strcat "|" str))
                                                                    ls_xdata_att))
                                                     2)
                                             )
                                            ((= str "radius")
                                             (as-numstr(* 0.5(cdr(assoc "DIAM" ls_out_parameter)))))
                                            ((= str "rebar")
                                             (strcat "\""(cdr(assoc "REBAR" ls_out_parameter))"\""))
                                            (T "")
                                            )
                                           )
                                     (strcat "," strx))
                                  ls_ifcparameter))
                             ls_str_out(cons(substr str 2)ls_str_out)
                             )
                       )
                   (progn

                     (progn
                       (setq vnam(cdr(assoc "CENTERLINE" ls_out_parameter))
                             ls_p(vlax-safearray->list(vlax-variant-value(vla-get-coordinates vnam)))
                             numy 32
                             
                             ls_rgb(cdr(assoc "TCOLDUCT" ls_out_parameter))
                             ls_p(split_list 3 ls_p)
                             
                             str(apply
                                 'strcat
                                 (mapcar
                                  '(lambda(str / strx)
                                     (setq strx
                                           (cond
                                            ((= str "type")"FACEUNION")
                                            ((= str "name")(cdr(assoc "NAME" ls_out_parameter)))
                                            ((= str "r")(as-numstr(/(car ls_rgb)255.)))
                                            ((= str "g")(as-numstr(/(cadr ls_rgb)255.)))
                                            ((= str "b")(as-numstr(/(caddr ls_rgb)255.)))
                                            ((= str "properties")
                                             (substr (apply 'strcat
                                                            (mapcar '(lambda(str)(strcat "|" str))
                                                                    ls_xdata_att))
                                                     2)
                                             )
                                            (T(setq str ""))
                                            ))
                                     (strcat "," strx))
                                  ls_ifcparameter))

                             ls_str_out(cons(substr str 2)ls_str_out)
                             )
                       
                       (setq ii 0 num(1-(length ls_p))
                             ls_intmesh(inclist 0 numy)
                             ang_d(/(* 2. pi)numy)
                             r(* 0.5(cdr(assoc "DIAM" ls_out_parameter)))
                             vecxp nil vecyp nil
                             )


                       
                       (setq ls_p(mapcar
                                  '(lambda(p / vec vecx vecy)
                                     (setq vec(xvla-normal vnam p)
                                           vecx(trans-x(list -1 0 0)vec(list 0 0 1))
                                           vecy(trans-x(list 0 1 0)vec(list 0 0 1))
                                           )
                                     (if(if vecxp(>(apply '+(mapcar '* vecx vecxp))0)T)T
                                       (setq vecx vecxp))
                                     (setq vecxp vecx)
                                     (if(<(caddr vecy)0)(setq vecy(mapcar '- vecy)))
                                     
                                     (mapcar '(lambda(i / c s)
                                                (setq c(cos(* ang_d i))s(sin(* ang_d i)))
                                                (mapcar '(lambda(a x y)(+(* r c x)(* r s y)a))
                                                        p vecx vecy))
                                             ls_intmesh)
                                     )
                                  ls_p)
                             
                             )

                       
                       
                       (mapcar
                        '(lambda(lst1 lst2 / p01 p02 p03 p04 p11 p12 p13 p14 ls_pmesh jj)
                           ;;(mapcar 'set '(p01 p02 p03 p04)lst1)
                           ;;(mapcar 'set '(p11 p12 p13 p14)lst2)

                           
                           (setq ii(1+ ii)str_p "" i 0 
                                 ls_p_mesh
                                 (mapcar '(lambda(j / p01 p02 p11 p12)
                                            (setq p01(nth(rem j numy)lst1)
                                                  p02(nth(rem(1+ j)numy)lst1)
                                                  p11(nth(rem j numy)lst2)
                                                  p12(nth(rem(1+ j)numy)lst2))
                                            (list p01 p02 p11 p02 p12 p11)
                                            )
                                         ls_intmesh )
                                 ls_p_mesh
                                 (apply 'append(apply 'append ls_p_mesh))
                                 ;;(if(= ii 1)(append p01 p04 p02 p04 p03 p02))
                                 ;;(if(= ii num)(append p14 p11 p13 p11 p12 p13))
                                 
                                 )
                           (mapcar '(lambda(a )
                                      (setq i(1+ i)
                                            str_p(strcat str_p(if(=(rem i 9)1)"|" ",")
                                                         (as-numstr a))
                                            ))
                                   ls_p_mesh)
                           
                           (setq str_p(strcat "\""(substr str_p 2)"\"")
                                 str(apply 'strcat
                                           (mapcar '(lambda(str)
                                                      (if(= str "type")(setq str "FACE")
                                                        (if(= str "face")(setq str str_p)
                                                          (setq str "")))
                                                      (strcat "," str))
                                                   ls_ifcparameter))
                                 ls_str_out(cons(substr str 2) ls_str_out)
                                 )
                           )
                        ls_p(cdr ls_p))
                       
                       (setq str(apply 'strcat
                                       (mapcar '(lambda(str)
                                                  (if(= str "type")(setq str "FACEEND")
                                                    (setq str ""))
                                                  (strcat "," str))
                                               ls_ifcparameter))
                             ls_str_out(cons(substr str 2)ls_str_out)
                             )
                       )



                     
                     (if(setq vnam(cdr(assoc "MESHOBJ" ls_out_parameter)))
                         (progn
                           (setq ls_p(vlax-safearray->list(vlax-variant-value(vla-get-coordinates vnam)))
                                 numy(vla-get-NVertexCount vnam)
                                 ls_rgb(cdr(assoc "TCOLMESH" ls_out_parameter))
                                 ls_p(split_list numy(split_list 3 ls_p))
                                 
                                 str(apply
                                     'strcat
                                     (mapcar
                                      '(lambda(str / strx)
                                         (setq strx
                                               (cond
                                                ((= str "type")"FACEUNION")
                                                ((= str "name")
                                                 (strcat(cdr(assoc "NAME" ls_out_parameter))"-CON"))
                                                ((= str "r")(as-numstr(/(car ls_rgb)255.)))
                                                ((= str "g")(as-numstr(/(cadr ls_rgb)255.)))
                                                ((= str "b")(as-numstr(/(caddr ls_rgb)255.)))
                                                ((= str "properties")
                                                 (substr (apply 'strcat
                                                                (mapcar '(lambda(str)(strcat "|" str))
                                                                        ls_xdata_att))
                                                         2)
                                                 )
                                                (T(setq str ""))
                                                ))
                                         (strcat "," strx))
                                      ls_ifcparameter))

                                 ls_str_out(cons(substr str 2)ls_str_out)
                                 )
                           
                           (setq ii 0 num(1-(length ls_p))
                                 ls_intmesh(inclist 0 numy))
                           
                           (mapcar
                            '(lambda(lst1 lst2 / p01 p02 p03 p04 p11 p12 p13 p14 ls_pmesh jj)
                               ;;(mapcar 'set '(p01 p02 p03 p04)lst1)
                               ;;(mapcar 'set '(p11 p12 p13 p14)lst2)

                               
                               (setq ii(1+ ii)str_p "" i 0 
                                     ls_p_mesh
                                     (mapcar '(lambda(j / p01 p02 p11 p12)
                                                (setq p01(nth(rem j numy)lst1)
                                                      p02(nth(rem(1+ j)numy)lst1)
                                                      p11(nth(rem j numy)lst2)
                                                      p12(nth(rem(1+ j)numy)lst2))
                                                (list p01 p02 p11 p02 p12 p11)
                                                )
                                             ls_intmesh )
                                     ls_p_mesh
                                     (apply 'append(apply 'append ls_p_mesh))
                                     ;;(if(= ii 1)(append p01 p04 p02 p04 p03 p02))
                                     ;;(if(= ii num)(append p14 p11 p13 p11 p12 p13))
                                     
                                     )
                               (mapcar '(lambda(a )
                                          (setq i(1+ i)
                                                str_p(strcat str_p(if(=(rem i 9)1)"|" ",")
                                                             (as-numstr a))
                                                ))
                                       ls_p_mesh)
                               
                               (setq str_p(strcat "\""(substr str_p 2)"\"")
                                     str(apply 'strcat
                                               (mapcar '(lambda(str)
                                                          (if(= str "type")(setq str "FACE")
                                                            (if(= str "face")(setq str str_p)
                                                              (setq str "")))
                                                          (strcat "," str))
                                                       ls_ifcparameter))
                                     ls_str_out(cons(substr str 2) ls_str_out)
                                     )
                               )
                            ls_p(cdr ls_p))
                           
                           (setq str(apply 'strcat
                                           (mapcar '(lambda(str)
                                                      (if(= str "type")(setq str "FACEEND")
                                                        (setq str ""))
                                                      (strcat "," str))
                                                   ls_ifcparameter))
                                 ls_str_out(cons(substr str 2)ls_str_out)
                                 )
                           ))
                     ))
                 
                 )
               
               
               )
              ((= str_type "CCBOXBLOCK")
               (if(= int_savecsvifc 1)
                   (setq str_out(strcat "CCBOX," str))
                 (setq ls_out_parameter(cons(cons "NAME" str)ls_out_parameter))
                 )
               
               (vlax-for
                obj vnam
                (vla-getXData obj "terraduct3d" 'array_Type 'array_Data )
                (setq ls_xdata
                      (if array_data
                          (split_list 0(mapcar 'vlax-variant-value
                                               (vlax-safearray->list array_data))))
                      str_type(cdr(assoc "terraduct3d" ls_xdata))
                      )

                (setq ls_gcode(entget(vlax-vla-object->ename obj)))
                (cond
                 ((= str_type "CCBOXSOLID")
                  (if(= int_savecsvifc 1)
                      (setq ls_out_parameter
                            (cons(cons "COLCCBOX"(vla-get-color obj))ls_out_parameter))
                    (setq ls_rgb((lambda(vtc)(mapcar '(lambda(a)(a vtc))
                                                     (list vla-get-red vla-get-green vla-get-blue)))
                                 (vla-get-truecolor obj))
                          ))
                  )
                 ((= str_type "CCBOXWIDTH-X")
                  (setq p13(cdr(assoc 13 ls_gcode))p14(cdr(assoc 14 ls_gcode))
                        ls_out_parameter
                        (cons(cons "ZBOTTOM"(caddr p13))ls_out_parameter)
                        ls_out_parameter
                        (cons(cons "DIMSTYLE"(cdr(assoc 3 ls_gcode)))ls_out_parameter)
                        )
                  (if(= int_savecsvifc 1)
                      (setq ls_out_parameter
                            (cons(cons "X0"(car p13))ls_out_parameter)
                            ls_out_parameter
                            (cons(cons "Y0"(cadr p13))ls_out_parameter)
                            ls_out_parameter
                            (cons(cons "X1"(car p14))ls_out_parameter)
                            ls_out_parameter
                            (cons(cons "Y1"(cadr p14))ls_out_parameter)
                            )
                    (setq ls_out_parameter
                          (cons(cons "X0"(* 0.5(+(car p13)(car p14))))ls_out_parameter)
                          ls_out_parameter
                          (cons(cons "rotate"(-(angle(carxy p13)(carxy p14))))ls_out_parameter)
                          ls_out_parameter
                          (cons(cons "dx"(distance p13 p14))ls_out_parameter)
                          )
                    )
                  
                  )
                 ((= str_type "CCBOXHEIGHT")
                  (setq p(cdr(assoc 14 ls_gcode))
                        ls_out_parameter
                        (cons(cons "ZTOP"(caddr p))ls_out_parameter)
                        )
                  )
                 ((= str_type "CCBOXVBACANT")
                  (setq p(cdr(assoc 14 ls_gcode))
                        ls_out_parameter
                        (cons(cons "ZGROUND"(caddr p))ls_out_parameter)
                        )
                  )
                 ((= str_type "CCBOXWIDTH-Y")
                  (setq p(cdr(assoc 13 ls_gcode)))
                  (if(= int_savecsvifc 1)
                      (setq ls_out_parameter
                            (cons(cons "X2"(car p))ls_out_parameter)
                            ls_out_parameter
                            (cons(cons "Y2"(cadr p))ls_out_parameter)
                            )
                    (setq ls_out_parameter
                          (cons(cons "Y0"(* 0.5(+(cadr p)(cadr(cdr(assoc 14 ls_gcode))))))
                               ls_out_parameter)
                          ls_out_parameter
                          (cons(cons "dy"(distance p(cdr(assoc 14 ls_gcode))))ls_out_parameter)
                          )
                    )
                  )
                 
                 ((= str_type "LEVELINGCON")
                  (setq ls_out_parameter
                        (cons(cons "LEVELINGOFFSET"(cdr(assoc "OFFSET" ls_xdata)))
                             ls_out_parameter)
                        ls_out_parameter
                        (cons(cons "LEVELINGHEIGHT"(cdr(assoc "HEIGHT" ls_xdata)))
                             ls_out_parameter)
                        ))
                 
                 
                 ((= str_type "MANHOLE")
                  (setq ls_out_parameter
                        (cons(assoc "CENTER-X" ls_xdata) ls_out_parameter)
                        ls_out_parameter
                        (cons(assoc "CENTER-Y" ls_xdata) ls_out_parameter)
                        ls_out_parameter
                        (cons(cons "MANHOLEHEIGHT"(cdr(assoc "HEIGHT" ls_xdata)))
                             ls_out_parameter)
                        ls_out_parameter
                        (cons(cons "MANHOLERADIUS" 0.175) ;;(* 0.5(cdr(assoc "DIAM" ls_xdata))))
                             ls_out_parameter);;もどす
                        ))
                 
                 )
                
                )
               
               (if(= int_savecsvifc 1)
                   (progn
                     (mapcar
                      '(lambda(lst / str str_initial)
                         (if(setq str(cdr(assoc(car lst)ls_out_parameter)))T
                           (setq str(cadr lst)))
                         (setq str_out(strcat str_out ","(as-numstr str))))
                      (list(list "COLCCBOX")(list "ZBOTTOM")(list "ZTOP")(list "ZGROUND")
                           (list "X0")(list "Y0")(list "X1")(list "Y1")(list "X2")(list "Y2")
                           (list "LEVELINGOFFSET" 0)(list "LEVELINGHEIGHT" 0)
                           (list "CENTER-X" 0)(list "CENTER-Y" 0)
                           (list "MANHOLEHEIGHT" 0)(list "MANHOLERADIUS" 0)
                           (list "DIMSTYLE")
                           )
                      )
                     (setq ls_str_out_ccbox(cons str_out ls_str_out_ccbox)
                           ls_str_out_ccbox
                           (cons(strcat "ATTRIBUTE"
                                        (apply 'strcat
                                               (mapcar '(lambda(str)(strcat "," str))
                                                       ls_xdata_att
                                                       )))
                                ls_str_out_ccbox)
                           )
                     )
                 
                 ((lambda( / vec lst str_name str_r str_g str_b ls_con ang_rot)

                    (setq ls_con(list(cons "name"(cdr(assoc "NAME" ls_out_parameter)))
                                     (cons "r"(as-numstr(/(car ls_rgb)255.)))
                                     (cons "g"(as-numstr(/(cadr ls_rgb)255.)))
                                     (cons "b"(as-numstr(/(caddr ls_rgb)255.)))
                                     (cons "ex" "0")(cons "ey" "0")(cons "ez" "1")
                                     )
                          lst(list
                              (append ls_con
                                      (list(cons "type" "RECT")
                                           (cons "x"(cdr(assoc "X0" ls_out_parameter)))
                                           (cons "y"(cdr(assoc "Y0" ls_out_parameter)))
                                           (cons "z"(cdr(assoc "ZBOTTOM" ls_out_parameter)))
                                           (assoc "rotate" ls_out_parameter)
                                           (assoc "dx" ls_out_parameter)
                                           (assoc "dy" ls_out_parameter)
                                           (cons "height"
                                                 (-(cdr(assoc "ZTOP" ls_out_parameter))
                                                   (cdr(assoc "ZBOTTOM" ls_out_parameter))))
                                           )
                                      )
                              (if(setq z(cdr(assoc "LEVELINGHEIGHT" ls_out_parameter))
                                       w(cdr(assoc "LEVELINGOFFSET" ls_out_parameter)))
                                  (append ls_con
                                          (list(cons "type" "RECT")
                                               (cons "x"(cdr(assoc "X0" ls_out_parameter)))
                                               (cons "y"(cdr(assoc "Y0" ls_out_parameter)))
                                               (cons "z"(-(cdr(assoc "ZBOTTOM" ls_out_parameter))z))
                                               (assoc "rotate" ls_out_patrameter)
                                               (cons "dx"(+(cdr(assoc "dx" ls_out_parameter))(* 2 w)))
                                               (cons "dy"(+(cdr(assoc "dy" ls_out_parameter))(* 2 w)))
                                               (cons "height"(-(cdr(assoc "ZBOTTOM" ls_out_parameter))z))
                                               )
                                          )
                                )

                              (if(setq h(cdr(assoc "MANHOLEHEIGHT" ls_out_parameter)))
                                  (append ls_con
                                          (list(cons "type" "CIRCLE")
                                               (cons "x"(cdr(assoc "CENTER-X" ls_out_parameter)))
                                               (cons "y"(cdr(assoc "CENTER-Y" ls_out_parameter)))
                                               (cons "z"(-(cdr(assoc "ZTOP" ls_out_parameter))z))
                                               (cons "height"(cdr(assoc "MANHOLEHEIGHT" ls_out_parameter)))
                                               (cons "radius"(cdr(assoc "MANHOLERADIUS" ls_out_parameter)))
                                               )
                                          )
                                )
                              )
                          lst(vl-remove nil lst)
                          )
                    
                    (mapcar
                     '(lambda(lst)
                        (setq str
                              (apply
                               'strcat
                               (mapcar
                                '(lambda(str / strx)
                                   (if(setq strx(cdr(assoc str lst)))T(setq strx ""))
                                   (strcat ","(as-numstr strx)))
                                ls_ifcparameter )
                               )
                              ls_str_out(cons(substr str 2)ls_str_out)
                              
                              )
                        )
                     lst)
                    
                    ) )
                 )
               
               
               )
              
              ((and(= str_type "PROJECT")(= int_savecsvifc 1))
               (setq ls_p_ceircle(list))
               (vlax-for
                obj vnam

                (cond
                 ((=(vla-get-ObjectName obj)"AcDb3dPolyline")
                  (setq ls_p(split_list 3(vlax-safearray->list(vlax-variant-value(vla-get-coordinates obj))))
                        int_col(vla-get-color obj)
                        hand_road(vla-get-handle obj)
                        )
                  )
                 ((=(vla-get-ObjectName obj)"AcDbCircle")
                  (setq ls_p_center
                        (cons(vlax-safearray->list(vlax-variant-value(vla-get-center obj)))
                             ls_p_center))
                  )
                 )
                )
               
               (setq ls_str_out(cons(strcat "PROJECTROAD," str "," (as-numstr int_col)"," hand_road)
                                    ls_str_out)
                     ls_str_out
                     (cons(strcat "ATTRIBUTE"
                                  (apply 'strcat
                                         (mapcar '(lambda(str)(strcat "," str))
                                                 ls_xdata_att
                                                 )))
                          ls_str_out)
                     )
               
               (mapcar
                '(lambda(p / str_i str)
                   (setq str_i
                         (if(apply 'or(mapcar '(lambda(pc)(<(distance p pc)1e-8))ls_p_center))
                             "1" "0")
                         str
                         (apply 'strcat(mapcar '(lambda(a)(strcat ","(as-numstr a)))p))
                         str(strcat "POINT," str_i str)
                         ls_str_out(cons str ls_str_out)
                         )
                   )
                ls_p)
               
               (setq ls_str_out(cons(strcat "PROJECTROADEND")ls_str_out))
               
               )
              )

             
             )
          ls_vnam_select )

         (if(= int_savecsvifc 1)
             (setq ls_str_out(append ls_str_out_ccbox ls_str_out_duct ls_str_out)))
         
         (setq bool_ifcexe nil)
         (if(setq str_path
                  (if(= int_savecsvifc 1);;CSV保存
                      (getfiled(mix_strasc(list "CSV" 20445 23384))
                               (strcat(getvar "DWGPREFIX")"csvoutput")"csv" 1)
                    (if(= int_savecsvifc 3);;IFC保存
                        (getfiled(mix_strasc(list "IFC" 20445 23384))
                                 (strcat(getvar "DWGPREFIX")"ifcoutput")"ifc" 1)
                      )
                    )
                  )
             
             (progn
               (if(= int_savecsvifc 3)
                   (setq str_pathifc str_path
                         ;; cpath_terraduct3d(strcat (getenv "APPDATA")"\\" "terraduct3d-ac" "\\app")
                         str_path(strcat cpath_terraduct3d "\\pyexe\\csv_to_ifc.csv")))
               
               (if command_for_alter
                   (setq open_file (open str_path "w" ) );;"utf8"
                 (progn
                   (setq open_file (open str_path "w" "utf8") )
                   (write-char 65279 open_file) ;; BOM
                   ))

               
               (if open_file
                   (progn
                     (mapcar '(lambda(str)(write-line str open_file))(reverse ls_str_out))
                     (close open_file)
                     
                     (if(= int_savecsvifc 1)
                         (x-alert(list 20986 21147 12375 12414 12375 12383))
                       (setq bool_ifcexe T) )
                     
                     )
                 (progn
                   (alert(mix_strasc(list 12501 12449 12452 12523 12364 38283 12363 12428 12390 12356 12427 12383 12417 26360 36796 12415 12391 12365 12414 12379 12435 12391 12375 12383 )))
                   )
                 )
               
               )
           (x-alert(list 12501 12449 12452 12523 12364 36984 25246 12373 12428 12414 12379 12435 12391 12375 12383 ))
           )
         
         
         (if(and(= int_savecsvifc 3) str_path bool_ifcexe)
             (progn
               ;; (setq cpath_terraduct3d(strcat (getenv "APPDATA")"\\" "terraduct3d-ac" "\\app"))
               
               (setq str_input str_path str_data str_pathifc
                     str_path(strcat cpath_terraduct3d "\\pyexe\\main.exe")
                     bool_loop T size_file nil
                     int_time 100 ms_loop nil ms_max 20000 
                     )
               
               (setq str_lasfile str_laspath )
               
               (if(findfile str_data)(vl-file-delete str_data))
               
               
               (setq wsh (vlax-create-object "WScript.Shell"))
               (setq cmd(strcat "\"" str_path "\" "
                                "\"" "ifc"    "\" "
                                "\"" str_input "\" "
                                "\"" str_data "\"" ))
               (vlax-invoke-method wsh 'Run cmd 0 :vlax-true)
               (vlax-release-object wsh)
               
               (setq ms_loop(getvar "MILLISECS"))
               (while bool_loop
                 (setq ms_start(getvar "MILLISECS"))
                 (if(>(- ms_start ms_loop)ms_max)(setq bool_loop nil))
                 (while(<(-(getvar "MILLISECS")ms_start)int_time) )
                 (if(findfile str_data)
                     (if(= size_file(vl-file-size str_data))(setq bool_loop nil)
                       (setq size_file(vl-file-size str_data)) ))
                 )

               (if(findfile str_data)
                   T
                 (x-alert(list 35501 12415 36796 12415 12395 22833 25943 12375 12414 12375 12383))
                 )
               
               ))
         

         (mapcar '(lambda(v)(vla-Highlight v :vlax-false))ls_vnam_select)
         (setq ls_vnam_select nil ls_vnam_highlight nil)
         
         
         )
        
        ((if(= int_savecsvifc 2);;CSV読込
             (setq str_path
                   (getfiled(mix_strasc(list "CSV" 35501 36796))
                            (strcat(getvar "DWGPREFIX")"csvoutput")"csv" 0))
           (progn
             (x-alert(list 12501 12449 12452 12523 12364 36984 25246 12373 12428 12414 12379 12435 12391 12375 12383 ))
             nil
             ) )
         
         (setq open_file (open str_path "r")
               ls_str_in(list))
         
         (while(setq str(read-line open_file))
           (setq ls_str_in(cons(getlist_str_split str ",")ls_str_in))
           )
         (close open_file)

         (setq ls_hand_road(list)
               ls_str_in(reverse ls_str_in)
               int_objtype nil
               ls_read_duct(list)
               ls_read_ccbox(list)
               ls_read_projectroad(list)
               ls_existblock(list)
               )
         
         (setq radius_node(if(= pitch_project 0.)0.05 pitch_project))
         (if(if pitch_mesh(>(abs pitch_mesh)1e-8))T(setq pitch_mesh 0.5))
         (while ls_str_in
           (setq lst(car ls_str_in)ls_str_in(cdr ls_str_in)
                 str(car lst)ls_vnam_copy (list))
           
           (cond
            ((or(= str "DUCTBLOCK")(= str "CCBOX")(= str "PROJECTROAD"))
             (setq str_name(cadr lst)))
            ;; (
            ;;  (setq i 0)
            ;;  (while(progn
            ;;          (setq str_name(strcat "PROJECTROAD_READ" "$"(itoa(setq i(1+ i)))))
            ;;          (null(vl-catch-all-error-p
            ;;                (vl-catch-all-apply 'vla-Item(list vnam_blocktable str_name))))
            ;;          )
            ;;    )
            ;;  )
            (T(setq str_name nil))
            
            )
           
           (if(if str_name
                  (if(vl-catch-all-error-p
                      (setq block(vl-catch-all-apply 'vla-Item(list vnam_blocktable str_name))))
                      (setq block(vla-Add vnam_blockTable(vlax-3d-point 0 0 0)str_name) )
                    (progn
                      
                      (setq ls_existblock(cons str_name ls_existblock))
                      (if(= int_overwrite_readcsv 0)
                          (progn
                            (setq ls_vla-release(cons block ls_vla-release))
                            nil)
                        
                        (progn
                          (if(setq set_ent(ssget "X"(list(cons 2 str_name))))
                              (progn
                                (setq num(sslength set_ent))
                                (while(>(setq num(1- num))-1)
                                  (setq vnam(vlax-ename->vla-object(ssname set_ent num)))
                                  (vla-delete vnam)
                                  )
                                ))
                          (vla-delete block)
                          
                          (mapcar '(lambda(v)
                                     (vlax-release-object v)
                                     (setq ls_vla-release(vl-remove v ls_vla-release))
                                     )
                                  (list block))
                          (setq block(vla-Add vnam_blockTable(vlax-3d-point 0 0 0)str_name))
                          T
                          )
                        ))
                    ) )
               (progn
                 (setq ls_xdata_att nil)
                 (cond
                  ((= str "DUCTBLOCK")
                   
                   (mapcar '(lambda(lst val / sym str_type)
                              (setq sym(car lst)str_type(cadr lst))
                              (if(= str_type "REAL")(setq val(atof val))
                                (if(= str_type "INT")(setq val(atoi val))))
                              (set sym val)
                              )
                           (list(list 'str_name)
                                (list 'int_colduct_temp "INT")
                                (list 'int_colprotect_temp "INT")
                                (list 'diam_duct_temp "REAL")
                                (list 'int_protectcon_temp "INT")
                                (list 'width_protect_temp "REAL")
                                (list 'height_protect_temp "REAL")
                                (list 'filet_protect_temp "REAL")
                                )
                           (cdr lst))

                   (if(=(caar ls_str_in)"ATTRIBUTE")
                       (setq ls_xdata_att(cdar ls_str_in)
                             ls_str_in(cdr ls_str_in)))
                   
                   (setq int_duct -1 int_depth 0 ls_pmesh(list)ls_pbar(list)
                         p_arcend nil )
                   (while(progn(setq lst(car ls_str_in) str_type(car lst))
                               (if(= str_type "DUCTBLOCKEND")
                                   (progn(setq ls_str_in(cdr ls_str_in))
                                         nil)
                                 T) )
                     
                     (cond
                      ((= str_type "DEPTH")
                       (setq int_duct(1+ int_duct)
                             ls_dim
                             (mapcar
                              '(lambda(lst i / p0 p1)
                                 (setq lst(cdr lst)
                                       p0(mapcar 'atof(list(car lst)(cadr lst)(caddr lst)))
                                       lst(cdddr lst)
                                       p1(mapcar 'atof(list(car lst)(cadr lst)(caddr lst)))
                                       lst(cdddr lst)
                                       str_dim_temp(car lst)
                                       hand_road(cdr(assoc(cadr lst)ls_hand_road))
                                       )
                                 
                                 (list p0 p1(if hand_road hand_road(setq hand_road "")))
                                 )
                              ls_str_in (list 0 1)
                              )
                             ls_str_in(cddr ls_str_in)
                             p_line0(cadar ls_dim)p_line1(cadadr ls_dim)
                             vec_line(unit_vector(carxyz(mapcar '- p_line0 p_line1)0))
                             )

                       (mapcar
                        '(lambda(lst ii / p0 p1 h)
                           (setq p0(car lst)p1(cadr lst)h(caddr lst)
                                 str_level(strcat "GL"(if(<(-(caddr p1)(caddr p0))0)"-" "+")"<>\\P"
                                                  "EL = "(rtos(caddr p1)2 int_unitelevation))
                                 entna(make_2pdimension
                                       nil(list p0 p1 vec_line
                                                (apply '+(mapcar '* vec_line p0))
                                                nil(* 0.5 pi)
                                                0. str_level
                                                (if(vl-position str_dim_temp ls_dimstyle)
                                                    str_dim_temp str_dimstyle_ductlevel)
                                                )
                                       )
                                 vnam(vlax-ename->vla-object entna)
                                 ls_vnam_copy(cons vnam ls_vnam_copy)
                                 )
                           (vla-put-color vnam int_colductdim)
                           (set_xda vnam(list(cons 1000 "DEPTH")
                                             (cons 1000 "NUM")(cons 1071 int_duct)
                                             (cons 1000 "SIDE")(cons 1071 ii)
                                             (cons 1000 "ROAD")(cons 1000 h)
                                             )
                                    "terraduct3d")
                           )
                        ls_dim (list 0 1))
                       
                       (setq vnam
                             (vla-addline
                              (vla-get-modelspace(vla-get-activedocument(vlax-get-acad-object)))
                              (vlax-3d-point p_line0)(vlax-3d-point p_line1))
                             ls_vnam_copy(cons vnam ls_vnam_copy)
                             )
                       
                       (vla-put-color vnam int_colductdim)
                       (set_xda vnam(list(cons 1000 "LINE")
                                         (cons 1000 "NUM")(cons 1071 int_duct))
                                "terraduct3d")

                       
                       )
                      ((= str_type "ARC")
                       
                       (mapcar
                        '(lambda(lst ii / )
                           (setq lst(cdr lst)
                                 p15(mapcar 'atof(list(car lst)(cadr lst)(caddr lst)))
                                 lst(cdddr lst)
                                 p13(mapcar 'atof(list(car lst)(cadr lst)(caddr lst)))
                                 lst(cdddr lst)
                                 p14(mapcar 'atof(list(car lst)(cadr lst)(caddr lst)))
                                 lst(cdddr lst)
                                 p16(mapcar 'atof(list(car lst)(cadr lst)(caddr lst)))
                                 lst(cdddr lst)
                                 dist_normal(atof(caddr lst)) lst(cdddr lst)
                                 vec_normal(mapcar 'atof(list(car lst)(cadr lst)(caddr lst)))
                                 dist_normal(apply '+(mapcar '* p15 vec_normal))
                                 lst(cdddr lst)
                                 str_dim_temp(car lst)
                                 
                                 rr(distance p15 p13)
                                 entna(make_arcdimension
                                       nil(list p15 p13 p14 p16 rr
                                                vec_normal dist_normal
                                                (strcat "R="(if(< rr 1e-8)(chr 8734)(as-numstr rr)))
                                                (if(vl-position str_dim_temp ls_dimstyle)
                                                    str_dim_temp str_dimstyle_ductlevel)
                                                ))
                                 vnam(vlax-ename->vla-object entna)
                                 ls_vnam_copy(cons vnam ls_vnam_copy)
                                 )
                           
                           (vla-put-color vnam int_colductdim)
                           (vla-put-Arrowhead1Type vnam 19)
                           (vla-put-Arrowhead2Type vnam 19)
                           (vla-put-ExtLine1Suppress vnam -1)
                           (vla-put-ExtLine2Suppress vnam -1)
                           (vla-put-HorizontalTextPosition vnam 3)
                           (vla-put-TextInsideAlign vnam 0)
                           (vla-put-VerticalTextPosition vnam 2)
                           
                           (setq p_arcend p14)
                           (set_xda vnam(list(cons 1000 "ARC")
                                             (cons 1000 "NUM")(cons 1071 int_duct)
                                             (cons 1000 "SIDE")(cons 1071 ii)
                                             (cons 1000 "RADIUS")(cons 1040 rr)
                                             )
                                    "terraduct3d")

                           (if(<(distance p13 p14)1e-8) T
                             (progn
                               (setq vec13(mapcar '- p13 p15)
                                     vec14(mapcar '- p14 p15)
                                     vec13(mapcar '(lambda(a)(/ a rr))vec13)
                                     vec14(mapcar '(lambda(a)(/ a rr))vec14)
                                     vec0(cross_product vec_normal vec13)
                                     vec1(cross_product vec_normal vec14)
                                     )

                               (setq p(if ls_pbar(caar ls_pbar)p_line0))
                               
                               (if(<(apply '+(mapcar '* vec0 vec14))0)(setq vec0(mapcar '- vec0)))
                               (if(>(apply '+(mapcar '* vec1 vec13))0)(setq vec1(mapcar '- vec1)))
                               (if(<(apply '+(mapcar '*(mapcar '- p13 p)vec0))-1e-8)
                                   (setq ls_pbar(cons(list p13 nil vec0)(cdr ls_pbar)))
                                 ;; (if(= int_protectcon_temp 2)T
                                 ((lambda(p / d n i dd pm)
                                    (if ls_pbar T(setq ls_pbar(cons(list p_line0 nil vec0)(cdr ls_pbar))))
                                    (setq d(distance p p13) n(1+(fix(/ d pitch_mesh))) d(/ d n)
                                          dd(- d))
                                    (while(>(setq n(1- n))-1)
                                      (setq dd(+ dd d)
                                            pm(mapcar '(lambda(a b)(+ a(* dd b)))p vec0)
                                            ls_pmesh(cons(list pm vec0)ls_pmesh)
                                            )
                                      )
                                    )
                                  p)
                                 ;; )
                                 )
                               
                               (setq ls_pbar(cons(list p13 p15 vec0)ls_pbar)
                                     ls_pbar(cons(list p14 nil vec1)ls_pbar)
                                     )
                               
                               ((lambda( / d n i ang vec_x vec_y pm dd c s v)
                                  (setq ang(atan(distance(cross_product vec13 vec14)(list 0 0 0))
                                                (apply '+(mapcar '* vec13 vec14)))
                                        vec_x(mapcar '(lambda(a)(* a rr))vec13)
                                        vec_y(mapcar '(lambda(a)(* a rr))vec0)
                                        d(* rr ang) n(1+(fix(/ d pitch_mesh))) d(/ ang n)
                                        ang(- d))
                                  
                                  (while(>(setq n(1- n))-1)
                                    (setq ang(+ ang d)c(cos ang)s(sin ang)
                                          v(mapcar '(lambda(x y)(+ (* -1 s x)(* c y)))vec13 vec0)
                                          pm(mapcar '(lambda(a x y)(+ a(* c x)(* s y)))p15 vec_x vec_y)
                                          ls_pmesh(cons(list pm v)ls_pmesh)
                                          )
                                    )
                                  ))
                               
                               ))
                           )
                        ls_str_in
                        (list 0 1))
                       (setq ls_str_in(cddr ls_str_in))
                       
                       )
                      (T(setq ls_str_in(cdr ls_str_in)))
                      )

                     
                     )
                   
                   (if p_arcend T(setq p_arcend p_line0))
                   (setq ls_pbar(cons(list p_line1 nil vec_line) ls_pbar))

                   (setq vec0(unit_vector(mapcar '- p_line1 p_arcend)))
                   (if(<(apply '+(mapcar '*(mapcar '- p_line1 p_arcend)vec0))-1e-8)
                       (setq ls_pmesh(cons(list p_arcend vec0)ls_pmesh) )
                     ((lambda(p / d n i dd pm)
                        (setq d(distance p p_line1) n(1+(fix(/ d pitch_mesh))) d(/ d n)
                              dd(- d))
                        (while(>(setq n(1- n))-1)
                          (setq dd(+ dd d)
                                pm(mapcar '(lambda(a b)(+ a(* dd b)))p vec0)
                                ls_pmesh(cons(list pm vec0)ls_pmesh)
                                )
                          )
                        (setq ls_pmesh(cons(list p_line1 vec0)ls_pmesh))
                        
                        )
                      p_arcend)
                     ;; )
                     )

                   
                   (setq ls_pcenterline(mapcar 'car(reverse ls_pmesh)))

                   (if(= int_ductsolidmesh 1)
                       (progn
                         (setq ls_pbar(reverse ls_pbar)
                               entna(bendpipe_sld
                                     (* 0.5 diam_duct_temp)
                                     (mapcar 'car ls_pbar)(mapcar 'cadr ls_pbar)(mapcar 'caddr ls_pbar)
                                     (getvar "CLAYER") nil)
                               vnam(vlax-ename->vla-object entna)
                               )
                         )
                     (progn
                       (setq numy 32
                             ls_intmesh(inclist 0 numy)
                             ang_d(/(* 2. pi)numy)
                             r(* 0.5 diam_duct_temp)
                             vecxp nil vecyp nil
                             )
                       
                       (setq ls_pduct
                             (mapcar
                              '(lambda(lst / p v vecx vecy)
                                 (setq p(car lst)v(cadr lst)
                                       v(unit_vector(carxyz v 0.))
                                       vecx(trans-x(list 1 0 0)v(list 0 0 1))
                                       vecy(trans-x(list 0 1 0)v(list 0 0 1))
                                       )

                                 (if(if vecxp(>(apply '+(mapcar '* vecx vecxp))0)T)T
                                   (setq vecx vecxp))
                                 (setq vecxp vecx)
                                 (if(<(caddr vecy)0)(setq vecy(mapcar '- vecy)))
                                 (mapcar '(lambda(i / c s)
                                            (setq c(cos(* ang_d i))s(sin(* ang_d i)))
                                            (mapcar '(lambda(a x y)(+(* r c x)(* r s y)a))
                                                    p vecx vecy))
                                         ls_intmesh)
                                 )
                              (reverse ls_pmesh)
                              )
                             
                             numx(length ls_pduct)
                             ls_p(apply 'append(apply 'append ls_pduct))
                             )
                       
                       (setq array_mesh(vlax-make-safearray vlax-vbDouble(cons 0(1-(length ls_p)))))
                       (vlax-safearray-fill array_mesh ls_p)
                       (setq vnam
                             (vla-Add3DMesh
                              (vla-get-ModelSpace(vla-get-ActiveDocument (vlax-get-acad-object)))
                              numx numy array_mesh)
                             
                             )
                       )
                     )
                   
                   (vla-put-color vnam int_colduct_temp)
                   (setq ls_vnam_copy(cons vnam ls_vnam_copy))
                   (vla-put-visible vnam :vlax-true)
                   (vla-put-nclose vnam :vlax-true)
                   
                   (set_xda vnam(list(cons 1000 "DUCTSOLID")
                                     (cons 1000 "DIAM")(cons 1040 diam_duct_temp)
                                     )
                            "terraduct3d")
                   
                   (setq ls_p(apply 'append ls_pcenterline)
                         array_p(vlax-make-safearray vlax-vbDouble(cons 0(1-(length ls_p)))))
                   (vlax-safearray-fill array_p ls_p)
                   (setq vnam(vla-Add3dPoly
                              (vla-get-ModelSpace(vla-get-ActiveDocument(vlax-get-acad-object)))
                              array_p))
                   (setq ls_vnam_copy(cons vnam ls_vnam_copy))
                   (set_xda vnam(list(cons 1000 "CENTERLINE"))"terraduct3d")

                   
                   (if(setq numy(if(= int_protectcon_temp 0)
                                    (if(= filet_protect_temp 0.)4 8)
                                  (if(= int_protectcon_temp 1)
                                      (if(= filet_protect_temp 0.)-4 -6)
                                    )))
                       (progn
                         (setq int_corner numy
                               
                               w(* 0.5 width_protect_temp)
                               h(* 0.5 height_protect_temp)
                               f filet_protect_temp
                               
                               ls_pmesh
                               (mapcar
                                '(lambda(lst / p v vecx vecy)
                                   (setq p(car lst)v(cadr lst)
                                         v(unit_vector(carxyz v 0.))
                                         vecx(trans-x(list 1 0 0)v(list 0 0 1))
                                         vecy(trans-x(list 0 1 0)v(list 0 0 1))
                                         )
                                   
                                   (vl-remove
                                    nil
                                    (mapcar '(lambda(lst / bool iw iwf ih ihf)
                                               (mapcar 'set '(bool iw iwf ih ihf)lst)
                                               (if(vl-position numy bool)
                                                   (mapcar '(lambda(a x y)(+(*(+(* iw w)(* iwf f))x)
                                                                            (*(+(* ih h)(* ihf f))y) a))
                                                           p vecx vecy))
                                               )
                                            (list(list(list 8 4)-1  1 1 0)
                                                 (list(list 8)  -1  0 1 -1)
                                                 (list(list -6 -4)-1 0 0 0)
                                                 (list(list 8 4 -6 -4)-1 0 -1 1)
                                                 (list(list 8 -6)-1  1 -1 0)
                                                 (list(list 8 -6) 1 -1 -1 0)
                                                 (list(list 8 4 -6 -4) 1 0 -1 1)
                                                 (list(list -6 -4) 1 0 0 0)
                                                 (list(list 8)   1  0 1 -1)
                                                 (list(list 8 4) 1 -1 1 0)
                                                 ))
                                    )
                                   )
                                (reverse ls_pmesh)
                                )
                               
                               numx(length ls_pmesh)
                               numy(abs numy)
                               ls_p(apply 'append(apply 'append ls_pmesh))
                               )
                         
                         (setq array_mesh(vlax-make-safearray vlax-vbDouble(cons 0(1-(length ls_p)))))
                         (vlax-safearray-fill array_mesh ls_p)
                         (setq vnam
                               (vla-Add3DMesh
                                (vla-get-ModelSpace(vla-get-ActiveDocument (vlax-get-acad-object)))
                                numx numy array_mesh)

                               )
                         
                         (vla-put-layer vnam str_layprotect)
                         (vla-put-color vnam int_colprotect)
                         (vla-put-nclose vnam :vlax-true)

                         (setq ls_vnam_copy(cons vnam ls_vnam_copy))
                         (set_xda vnam(list(cons 1000 "CONMESH")
                                           (cons 1000 "CORNER")(cons 1071 int_corner)
                                           (cons 1000 "WIDTH")(cons 1040 width_protect_temp)
                                           (cons 1000 "HEIGHT")(cons 1040 height_protect_temp)
                                           (cons 1000 "FILET")(cons 1040 height_protect_temp)
                                           )
                                  "terraduct3d")
                         ))
                   
                   (setq ls_xdata(list(cons 1000 "DUCTBLOCK")))
                   
                   )
                  ((= str "CCBOX")

                   (mapcar '(lambda(lst val / sym str_type)
                              (setq sym(car lst)str_type(cadr lst))
                              (if(= str_type "REAL")(setq val(atof val))
                                (if(= str_type "INT")(setq val(atoi val))))
                              (set sym val)
                              )
                           (list(list 'str_name)
                                (list 'int_colccbox_temp "INT")
                                (list 'z_bottom "REAL")
                                (list 'z_top "REAL")
                                (list 'z_ground "REAL")
                                (list 'x0 "REAL") (list 'y0 "REAL")
                                (list 'x1 "REAL") (list 'y1 "REAL")
                                (list 'x2 "REAL") (list 'y2 "REAL")
                                (list 'offset_levelingcon_temp "REAL")
                                (list 'height_levelingcon_temp "REAL")
                                (list 'xc "REAL") (list 'yc "REAL")
                                (list 'height_manhole_temp "REAL")
                                (list 'radius_manhole_temp "REAL")
                                (list 'str_dim_temp)
                                )
                           (cdr lst))

                   (if(=(caar ls_str_in)"ATTRIBUTE")
                       (setq ls_xdata_att(cdar ls_str_in)
                             ls_str_in(cdr ls_str_in)))
                   
                   (setq vecx(list(- x1 x0)(- y1 y0)0)
                         distx(distance(list x0 y0)(list x1 y1))
                         vecx(mapcar '(lambda(a)(/ a distx))vecx)
                         vecy(list(- x2 x0)(- y2 y0)0)
                         disty(distance(list x0 y0)(list x2 y2))
                         vecy(mapcar '(lambda(a)(/ a disty))vecy)
                         p_ccbox0(mapcar '(lambda(a x y)(+(* x distx 0.5)(* y disty 0.5)a))
                                         (list x0 y0 z_bottom)vecx vecy)
                         
                         entna(rac_sld(* 0.5 distx)(* 0.5 disty)(- z_top z_bottom)
                                      p_ccbox0 0(list 0 0 1)vecx(getvar "CLAYER")nil)
                         vnam(vlax-ename->vla-object entna)
                         ls_vnam_copy(cons vnam ls_vnam_copy)
                         )
                   (set_xda vnam(list(cons 1000 "CCBOXSOLID") ) "terraduct3d")
                   (vla-put-color vnam int_colccbox_temp)
                   
                   (setq vec_normal(list(-(car vecy))(-(cadr vecy))0)
                         p0(mapcar '(lambda(a x y)(+ a(* -0.5 distx x)(* -0.5 disty y)))
                                   p_ccbox0 vecx vecy)
                         dist_normal(apply '+(mapcar '* p0 vec_normal))
                         )
                   
                   (mapcar
                    '(lambda(lst / p0 p1 vec_normal dist_normal vecy ang hh str_xdata)
                       (mapcar 'set '(p0 p1 vec_normal dist_normal vecy ang hh str_xdata)lst)
                       (setq entna(make_2pdimension
                                   nil(list p0 p1 vec_normal dist_normal vecy ang hh
                                            ""
                                            (if(vl-position str_dim_temp ls_dimstyle)
                                                str_dim_temp str_dimstyle_ductlevel)
                                            ))
                             vnam(vlax-ename->vla-object entna)
                             ls_vnam_copy(cons vnam ls_vnam_copy)
                             )
                       (set_xda vnam(list(cons 1000 str_xdata) )
                                "terraduct3d")
                       )
                    (list(list p0 (mapcar '(lambda(a b)(+ a(* b distx)))p0 vecx)
                               vec_normal dist_normal(list 0 0 1)0.(* 0.2(- z_top z_bottom))
                               "CCBOXWIDTH-X")
                         (list p0 (mapcar '+ p0(list 0 0(- z_top z_bottom)))
                               vec_normal dist_normal(mapcar '- vecx)(* 0.5 pi)
                               (* 0.2(- z_top z_bottom))
                               "CCBOXHEIGHT")
                         (list(mapcar '+ p0(list 0 0(- z_top z_bottom)))
                              (carxyz p0 z_ground)
                              vec_normal dist_normal(mapcar '- vecx)(* 0.5 pi)
                              (* 0.2(- z_top z_bottom))
                              "CCBOXVBACANT")
                         (list(mapcar '(lambda(a b)(+ a(* -1 b disty))) p0 vec_normal)
                              p0 (mapcar '- vecx)(-(apply '+(mapcar '* vecx p0)))
                              (list 0 0 1)0.(* 0.2(- z_top z_bottom))
                              "CCBOXWIDTH-Y")
                         )
                    )

                   (if(< height_levelingcon_temp 1e-8)T
                     (progn
                       (setq entna(rac_sld(+(* 0.5 distx)offset_levelingcon_temp)
                                          (+(* 0.5 disty)offset_levelingcon_temp)
                                          height_levelingcon_temp
                                          (carxyz p_ccbox0(- z_bottom height_levelingcon_temp))
                                          0(list 0 0 1)vecx(getvar "CLAYER")nil)
                             vnam(vlax-ename->vla-object entna)
                             ls_vnam_copy(cons vnam ls_vnam_copy)
                             )
                       
                       (set_xda vnam(list(cons 1000 "LEVELINGCON")
                                         (cons 1000 "OFFSET")(cons 1040 offset_levelingcon_temp)
                                         (cons 1000 "HEIGHT")(cons 1040 height_levelingcon_temp)
                                         )
                                "terraduct3d")
                       (vla-put-color vnam int_colccbox_temp)
                       
                       ))
                   
                   (if(or(= radius_manhole_temp 0.)(= height_manhole_temp 0.))T
                     (progn
                       (setq entna(pole_sld radius_manhole_temp
                                            height_manhole_temp
                                            (list xc yc z_top)
                                            (list 0 0 1)(getvar "CLAYER")nil)
                             vnam(vlax-ename->vla-object entna)
                             ls_vnam_copy(cons vnam ls_vnam_copy)
                             )
                       
                       (set_xda vnam(list(cons 1000 "MANHOLE")
                                         (cons 1000 "CENTER-X")(cons 1040(car p_manhole))
                                         (cons 1000 "CENTER-Y")(cons 1040(cadr p_manhole))
                                         (cons 1000 "DIAM")(cons 1040(* 2. radius_manhole_temp))
                                         (cons 1000 "HEIGHT")(cons 1040 height_ccboxtop_temp)
                                         )
                                "terraduct3d")
                       (vla-put-color vnam int_colccbox_temp)
                       
                       ))

                   (setq ls_xdata(list(cons 1000 "CCBOXBLOCK")
                                      (cons 1000 "PROJECT-D")(cons 1000(if str_lasground str_lasground ""))
                                      (cons 1000 "PROJECT-H")(cons 1040(if height_ground height_ground 0.))
                                      ))
                   
                   )
                  ((= str "PROJECTROAD")
                   (setq lst(cddr lst)
                         int_col(atoi(car lst))hand_road(cadr lst)ls_p(list))
                   
                   (if(=(caar ls_str_in)"ATTRIBUTE")
                       (setq ls_xdata_att(cdar ls_str_in)
                             ls_str_in(cdr ls_str_in)))
                   
                   (while(progn(setq lst(car ls_str_in)ls_str_in(cdr ls_str_in)
                                     str_type(car lst))
                               (= str_type "POINT"));;PROJECTROADEND
                     (setq i(atoi(cadr lst))
                           p(mapcar 'atof(cddr lst)))
                     (mapcar '(lambda(a)(setq ls_p(cons a ls_p)))p)
                     (if(= i 1)
                         (setq v(vla-addcircle
                                 (vla-get-modelspace(vla-get-activedocument(vlax-get-acad-object)))
                                 (vlax-3d-point p)radius_node)
                               ls_vnam_copy(cons v ls_vnam_copy)
                               )
                       )
                     
                     )

                   (setq ls_p(reverse ls_p))
                   (setq array_p(vlax-make-safearray vlax-vbDouble(cons 0 (1-(length ls_p)))))
                   (vlax-safearray-fill array_p ls_p)
                   (setq vnam(vla-Add3dPoly
                              (vla-get-ModelSpace(vla-get-ActiveDocument(vlax-get-acad-object)))array_p)
                         ls_vnam_copy(cons vnam ls_vnam_copy)
                         )
                   (vla-put-color vnam int_col)

                   (setq ls_xdata(list(cons 1000 "PROJECT")(cons 1000 "PROJECT")(cons 1000 "")))
                   
                   )
                  )
                 
                 (setq ls_vla-release(cons block ls_vla-release))
                 
                 (vla-copyobjects
                  (vla-get-ActiveDocument(vlax-get-acad-object))
                  (vlax-make-variant
                   (vlax-safearray-fill
                    (vlax-make-safearray
                     vlax-vbObject(cons 0(1-(length ls_vnam_copy))) )
                    ls_vnam_copy)
                   )
                  block)

                 (vlax-for
                  obj block
                  
                  (cond
                   ((=(vla-get-ObjectName obj)"AcDb3dPolyline")
                    (setq ls_hand_road(cons(cons hand_road(vla-get-handle obj) )ls_hand_road))
                    )
                   )
                  )
                 
                 (setq vnam(vla-InsertBlock
                            (vla-get-ModelSpace(vla-get-ActiveDocument(vlax-get-acad-object)))
                            (vlax-3d-point 0 0 0)str_name 1 1 1 0)
                       )
                 
                 (set_xda vnam ls_xdata "terraduct3d")
                 (if ls_xdata_att
                     (set_xda vnam(mapcar '(lambda(a)(cons 1000 a))ls_xdata_att)
                              "attributedata"))
                 (mapcar '(lambda(v)
                            (vla-delete v)
                            (exckillobj v)
                            )
                         ls_vnam_copy)
                 ))
           
           )

         (x-alert
          (if ls_existblock
              (if(= int_overwrite_readcsv 0)
                  (list ;;既存のブロック名称であったため上書きせずに読込完了しました
                   26082 23384 12398 12502 12525 12483 12463 21517 31216 12391 12354 12387 12383 12383 12417 19978 26360 12365 12379 12378 12395 35501 36796 23436 20102 12375 12414 12375 12383
                   "\n" (vl-princ-to-string ls_existblock)
                   )
                (list ;;既存のブロックを削除して新たに読み込みました
                 26082 23384 12398 12502 12525 12483 12463 12434 21066 38500 12375 12390 26032 12383 12395 35501 12415 36796 12415 12414 12375 12383
                 "\n"(vl-princ-to-string ls_existblock)
                 )
                )
            (list ;;既存のブロック名称と干渉せずすべて読込完了しました
             26082 23384 12398 12502 12525 12483 12463 21517 31216 12392 24178 28169 12379 12378 12377 12409 12390 35501 36796 23436 20102 12375 12414 12375 12383
             )))
         
         (setq ls_existblock(list))
         
         )
        )
       (setq int_selectmenu nil)
       

       
       ))
    
    ;;CLICK,KEYBOAD
    
    )

   
   
   )
  )



(defun cal_arcposition
    (rr ph pc px vec0 vecn0 vec1 vecn1 / dd dx pco pho pcc vecc)
  ;;phかpcはどちらかだけでもよい
  (setq dd(apply '+(mapcar '*(if ph ph pc)vec0))
        dx(apply '+(mapcar '* px vec0))
        )

  (cond
   ((> dd(+ dx rr))
    (if ph T(setq ph(mapcar '(lambda(a b)(+ a(* rr b)))pc vecn0)))
    (if pc T(setq pc(mapcar '(lambda(a b)(+ a(* -1 rr b)))ph vecn0)))
    (setq dd(apply '+(mapcar '*(mapcar '- pc px)vecn1))
          pco(mapcar '(lambda(a b)(+ a(*(+ rr(- dd))b)))pc vecn1)
          dd(-(* 4(expt rr 2))(expt(+ rr(- dd))2))
          )
    (if(< dd 0)(progn(alert "arcerror" )nil)
      (progn
        (setq dd(sqrt dd)
              pco(mapcar '(lambda(a b)(+ a(* dd b)))pco vec1)
              pho(mapcar '(lambda(a b)(+ a(* -1 rr b)))pco vecn1)
              pcc(mapcar '(lambda(a b)(* 0.5(+ a b)))pc pco)
              )
        (list pc ph pco pho pcc)
        ))
    )

   ((< dd dx )
    (if ph T(setq ph(mapcar '(lambda(a b)(+ a(* -1 rr b)))pc vecn0)))
    (if pc T(setq pc(mapcar '(lambda(a b)(+ a(* rr b)))ph vecn0)))
    (setq dd(apply '+(mapcar '*(mapcar '- pc px)vecn1))
          pco(mapcar '(lambda(a b)(+ a(* -1(+ rr dd)b)))pc vecn1)
          dd(-(* 4(expt rr 2))(expt(+ rr dd)2))
          )
    (if(< dd 0)(progn(alert "arcerror01" )nil)
      (progn
        (setq dd(sqrt dd)
              pco(mapcar '(lambda(a b)(+ a(* dd b)))pco vec1)
              pho(mapcar '(lambda(a b)(+ a(* rr b)))pco vecn1)
              pcc(mapcar '(lambda(a b)(* 0.5(+ a b)))pc pco)
              )
        (list pc ph pco pho pcc)
        ))
    )
   (T
    (setq vecc(unit_vector(mapcar '+ vec0 vec1))
          dd(/ rr(apply '+(mapcar '* vecn0 vecc)))
          pc(mapcar '(lambda(a b)(+ a(* dd b)))px vecc)
          )
    (list pc(mapcar '(lambda(a b)(+ a(* -1 rr b)))pc vecn0)
          nil nil(mapcar '(lambda(a b)(+ a(* -1 rr b)))pc vecn1))
    )
   )
  )

(defun cal_twisttangent
    (rr p_tan p00 p01 p10 p11 vec0 vec1 length_straight /
        ls_out bool_loop bool_first dh0 dh1 ph0 ph1 vec_tan delta_dh
        cos0 cos1 sin0 sin1 dist_corner dist0 dist1
        pc0 pt00 pt01 pt02 pc1 pt10 pt11 pt12
        vech0 vech1
        )
  (setq dh0(apply '+(mapcar '*(mapcar '- p_tan p11)vec0))
        dh0(/ dh0(apply '+(mapcar '* vec0 vec1)))
        dh1(apply '+(mapcar '*(mapcar '- p_tan p11)vec1))
        dh1(max dh1 dh0)
        delta_dh(* 0.1 rr)
        bool_loop T bool_first T
        dh1(- dh1 delta_dh)
        )

  (while bool_loop
    (setq dh1(+ dh1 delta_dh)
          ph1(mapcar '(lambda(a b)(+ a(* dh1 b)))p11 vec1)
          dist_corner(distance ph1 p_tan)
          vec_tan(mapcar '- ph1 p_tan)
          vec_tan(mapcar '(lambda(a)(/ a dist_corner))vec_tan)

          cos0(apply '+(mapcar '* vec0 vec_tan))
          sin0(sqrt(- 1.(expt cos0 2)))
          dist0(/(* rr(+ 1. cos0))sin0)
          
          cos1(-(apply '+(mapcar '* vec1 vec_tan)))
          sin1(sqrt(- 1.(expt cos1 2)))
          dist1(/(* rr(+ 1. cos1))sin1)
          )

    
    (if bool_first
        (setq bool_first nil bool_loop(>=(+ dist0 dist1)dist_corner)))
    (cond
     ((<(abs(- dist_corner dist0 dist1))1e-8)
      (setq vech0(unit_vector(cross_product vec_tan vec0))
            vech1(unit_vector(cross_product vec_tan vec1))
            pt00(mapcar '(lambda(a b)(+ a(* dist0 b)))p_tan vec0)
            pt01(mapcar '(lambda(a b)(+ a(* dist0 b)))p_tan vec_tan)
            dist0(+ dist0(* -1 rr(/ cos0 sin0)))
            pc0(mapcar '(lambda(a b c)(+ a(* dist0 b)(* dist0 c)))p_tan vec0 vec_tan)
            pt02(mapcar '(lambda(a b)(+ a(* rr b)))
                        pc0(unit_vector(mapcar '(lambda(a b c)(+ a b(* -2 c)))pt00 pt01 pc0)))

            pt10(mapcar '(lambda(a b)(+ a(* dist1 b)))ph1 vec1)
            pt11(mapcar '(lambda(a b)(+ a(* -1 dist1 b)))ph1 vec_tan)
            dist1(+ dist1(* -1 rr(/ cos1 sin1)))
            pc1(mapcar '(lambda(a b c)(+ a(* dist1 b)(* -1 dist1 c)))ph1 vec1 vec_tan)
            pt12(mapcar '(lambda(a b)(+ a(* rr b)))
                        pc1(unit_vector(mapcar '(lambda(a b c)(+ a b(* -2 c)))pt10 pt11 pc1)))

            ls_out(list(list pc0 pt00 pt01 pt02 vech0 length_straight)
                       (list pc1 pt10 pt11 pt12 vech1 length_straight)
                       )
            bool_loop nil
            )
      )
     ((>(* delta_dh(- dist_corner(+ dist0 dist1)))0)
      (setq delta_dh(* -0.1 delta_dh))
      )
     )

    )
  
  ls_out
  )


(defun connect_twist_curve
    (lst / p00 p01 p10 p11 r_bend length_straight str_alert
         vec0 vec1 vec2 vec3 cos_plane vec_plane sin_plane vec_plane height_plane 
         vec0n vec1n delta_ratio xx delta_x bool_loop pc
         vecp0 vecp1 length_slant cos_slant vec_normal sin_slant
         r_temp p1 p2 p3 vec_tan ls_p_through str_break bool_break)

  (setq str_break
        (mix_strasc(list 10 20870 24359 12398 20301 32622 12434
                         20219 24847 12395 35373 23450 12375 12390 12367 12384 12373 12356
                         )))
  ;;円弧の位置を任意に設定してください
  (if(setq bool_break(car lst))T(setq lst(cdr lst)))
  (mapcar 'set '(p00 p01 p10 p11 r_bend length_straight ls_p_through str_alert)lst)

  (cond
   ((progn
      (setq vec0(unit_vector(mapcar '- p00 p01))
            vec1(unit_vector(mapcar '- p11 p10))
            
            cos_plane(apply '+(mapcar '* vec0 vec1))
            vec_plane(cross_product vec0 vec1)
            sin_plane(distance(list 0 0 0)vec_plane)
            )
      (<(abs sin_plane)1e-8));;平行のとき
    (setq vec2(mapcar '- p10 p00)
          vec_normal(unit_vector(cross_product vec2 vec0)))
    

    (if(<(distance vec_normal(list 0 0 0))1e-8);;同一直線状
        (progn
          (princ;;同一直線状なので円弧を設定しません
           (mix_strasc(list "\n" str_alert "\n"
                            21516 19968 30452 32218 29366 12394 12398 12391 20870 24359 12434 35373 23450 12375 12414 12379 12435 )))
          
          (mapcar '(lambda(i)
                     (list(mapcar '+ p01(list r_bend 0 0))
                          p01 p01 p01(list 0 0 1)length_straight))
                  (list 0 1))
          )
      (if ls_p_through
          (mapcar '(lambda(lst / pc p0 p1 p2 vec)
                     (mapcar 'set '(pc p0 p1 p2)lst)
                     (list pc p0 p1 p2 vec_normal length_straight))
                  ls_p_through)
        (progn

          (setq vec3(cross_product vec0 vec_normal);;normalからみると必ず2本目が上
                d_space(* 0.5(apply '+(mapcar '* vec3 vec2))))
          
          (if(> d_space r_bend)
              (progn
                (x-alert;;2直線が平行で距離が開きすぎているため円弧を設定できません
                 (list str_alert "\n"
                       "2" 30452 32218 12364 24179 34892 12391 36317 38626 12364
                       38283 12365 12377 12366 12390 12356 12427 12383 12417
                       20870 24359 12434 35373 23450 12391 12365 12414 12379 12435
                       ))
                (list "BREAK")
                )
            (progn
              (if bool_break(princ str_break))
              (list "BREAK" vec_normal(apply '+(mapcar '* vec_normal p00))r_bend
                    p00 p01 p10 p11 vec0 vec1 length_straight
                    0(list vec3 d_space)
                    )
              ))
          )
        )
      )
    
    )
   ((progn
      (setq vec_plane(mapcar '(lambda(a)(/ a sin_plane))vec_plane)
            height_plane(apply '+(mapcar '*(mapcar '- p10 p00)vec_plane))
            )
      (<(abs height_plane)1e-8));;面内のとき
    
    (if ls_p_through
        (mapcar '(lambda(lst / pc p0 p1 p2 vec)
                   (mapcar 'set '(pc p0 p1 p2)lst)
                   (list pc p0 p1 p2 vec_plane length_straight))
                ls_p_through)
      (progn
        (setq vec0n(cross_product vec_plane vec0)
              vec1n(cross_product vec1 vec_plane)
              pc(3dcross_3pl vec_plane(apply '+(mapcar '* vec_plane p00))
                             vec0n(+(apply '+(mapcar '* vec0n p00))r_bend)
                             vec1n(+(apply '+(mapcar '* vec1n p10))r_bend)
                             )
              p0(mapcar '(lambda(a b)(+ a(* -1 r_bend b)))pc vec0n)
              p1(mapcar '(lambda(a b)(+ a(* -1 r_bend b)))pc vec1n)
              p2(mapcar '(lambda(a b)(+ a(* -1 r_bend b)))
                        pc(unit_vector(mapcar '+ vec0n vec1n)))
              )
    
        (if bool_break(princ str_break))
        (list "BREAK" vec_plane(apply '+(mapcar '* vec_plane p00))r_bend
              p00 p01 p10 p11 vec0 vec1 length_straight
              1(list vec0n vec1n pc p0 p1 p2
                     (inters p0(mapcar '+ p0 vec0)p1(mapcar '+ p1 vec1)nil)
                     )
              )
        ))
    )
   (T

    
    
    (if(if ls_p_through
           (setq ls_p_through
                 (cal_twisttangent
                  r_bend
                  ((lambda(lst / pc p0 p1 p2 d x h)
                     (setq x(car lst))
                     (setq pc(car x)p0(cadr x)p1(caddr x))
                     (if(<(distance p0 p1)1e-8)
                         (setq x(cadr lst)pc(car x)p0(cadr x)p1(caddr x) ))
                     
                     (setq p2(mapcar '(lambda(a b)(* 0.5(+ a b)))p0 p1)
                           d(/(expt(distance pc p0)2)(distance pc p2))
                           p2(mapcar '(lambda(a b)(+ a(* d b)))
                                     pc(unit_vector(mapcar '(lambda(a b c)(+(* -2 a)b c))pc p0 p1)))
                           d(apply '+(mapcar '*(mapcar '- p2 p00)vec0))
                           p2(mapcar '(lambda(a b)(+ a(* d b)))p00 vec0)
                           )
                     p2
                     )
                   ls_p_through)
                  p00 p01 p10 p11 vec0 vec1 length_straight)))
        (mapcar '(lambda(lst bool)
                   (if bool lst(mapcar '(lambda(i)(nth i lst))(list 0 2 1 3 4 5))))
                ls_p_through(list T nil))
      (progn
        (if bool_break(princ str_break))
        (list "BREAK" nil nil r_bend
              p00 p01 p10 p11 vec0 vec1 length_straight
              2(list )
              )
        ))
    

    )
   )
  
  )



(defun c:preview_grid()
  (setq ii(getint((lambda(lst / ii)
                    (setq ii -1)
                    (strcat "["
                            (substr
                             (apply
                              'strcat
                              (mapcar '(lambda(a)(setq ii(1+ ii))(strcat "/"(itoa ii)" " a))lst)
                              )
                             2)
                            "]"))
                  (mapcar 'car ls_lasgrid)))
        dict(cdr(nth ii ls_lasgrid))
        )
  
  (if(vl-catch-all-error-p
      (setq xrec(vl-catch-all-apply 'vla-Item (list dict "GRID-SIZE_CENTER"))))
      (progn(alert "LASDATAERROR01")(quit)))
  (vla-GetXRecordData xrec 'array_type 'array_data )
  (setq lst(if array_data
               (mapcar 'vlax-variant-value
                       (vlax-safearray->list array_data)))
        )
  (mapcar 'set '(size_grid z_ave z_min z_max)lst)
  (vlax-for
   vnam dict
   (if(vl-catch-all-error-p
       (setq str(vl-catch-all-apply 'vla-get-name (list vnam))))
       T
     (if(= str "GRID-SIZE_CENTER")T
       (progn
         (setq nn(vl-string-search "$" str)
               nx(atoi(substr str 1 nn)) ny(atoi(substr str(+ nn 2)))
               x(*(+ nx 0.5)size_grid) y(*(+ ny 0.5)size_grid)
               )
         (vla-GetXRecordData vnam 'array_type 'array_data )
         (setq z(vlax-variant-value(car(vlax-safearray->list array_data))))
         (vla-addcircle
          (vla-get-modelspace(vla-get-activedocument(vlax-get-acad-object)))
          (vlax-3d-point x y z)0.1)
         
         ))
     )
   )
  
  (princ)
  )



(defun cal_weight_grid
    (p dict size_grid int_width / x y z p00 p01 p10 p11 d
       int_deltax int_deltay nx0 ny0 nx1 ny1 ls_z ls_d ls_intx ls_inty ls_int
       str xrec array_type array_data ny nx ls_p z_ave sigma nn zz ww bool_just)
  (setq x(car p)y(cadr p)
        nx0(/ x size_grid)nx0(fix(- nx0(abs(rem nx0 1.))))
        ny0(/ y size_grid)ny0(fix(- ny0(abs(rem ny0 1.))))
        p00(list(*(+ nx0 0.5)size_grid)(*(+ ny0 0.5)size_grid))
        int_deltax(if(<=(car p00) x)1 -1)
        int_deltay(if(<=(cadr p00)y)1 -1)
        ;;nx1(+ nx0 int_deltax) ny1(+ ny0 int_deltay)
        )

  (if int_width T(setq int_width 1))
  
  ;; (setq p01(mapcar '+ p00(list(* int_deltax size_grid)0.)) 
  ;;       p10(mapcar '+ p00(list 0.(* int_deltay size_grid)))
  ;;       p11(mapcar '+ p10(list(* int_deltax size_grid)0.))
  ;;       )

  
  (setq ls_int(append(inclist(- int_width)0)(inclist 0(1+ int_width)))
        ls_inty ls_int
        ls_z(list))

  (while ls_inty
    (setq ny(+(*(car ls_inty)int_deltay)ny0) ls_inty(cdr ls_inty)
          ls_intx ls_int)
    (while ls_intx
      (setq nx(+(*(car ls_intx)int_deltax)nx0) ls_intx(cdr ls_intx))
      
      ;; (mapcar
      ;;  '(lambda(iy / str xrec array_type array_data ny nx)
      (setq str(strcat(itoa nx)"$"(itoa ny)))
      (if(vl-catch-all-error-p
          (setq xrec(vl-catch-all-apply 'vla-Item (list dict str))))
          nil
        (progn
          (vla-GetXRecordData xrec 'array_type 'array_data )
          (setq ls_z(cons(vlax-variant-value(car(vlax-safearray->list array_data)))
                         ls_z)
                ls_p(cons(mapcar '+ p00(list(* nx size_grid)(* ny size_grid)))
                         ls_p)
                )
          ))
      )
    )
  
  (if(=(setq nn(length ls_z))0)nil
    (progn
      (setq z_ave(/(apply '+ ls_z)nn)
            sigma(sqrt(/(apply '+(mapcar '(lambda(z)(expt(- z z_ave)2))ls_z))nn))
            ww 0 zz 0 bool_just nil)
      (mapcar
       '(lambda(z pg)
          (if bool_just T
            (if(>(abs(- z z_ave))(* 2 sigma))T
              (if(setq bool_just(<(setq d(distance p pg))1e-8))
                  (setq zz z ww 1.)
                (setq d(/ 1. d)zz(+ zz(* z d))ww(+ ww d))
                )))
          )
       ls_z ls_p)
      (list x y(/ zz ww))
      ))
  
  ;; (setq ls_d(mapcar '(lambda(z pg)(if z(distance p pg)0.))ls_z(list p11 p10 p01 p00))
  ;;       ww(apply '+ ls_d)
  ;;       )
  ;; (if(= ww 0.)nil
  ;;   (list x y(/(apply '+(mapcar '(lambda(a b)(if b(* a b)0.))ls_d ls_z))ww))
  ;;   )
  )


(defun project_to_ground ;;外れ値の検証方法を考える
    (ls_p vec_proj lst
          / str_las height_ground cosdist dict xrec num_limit delta_proj
          ls_gridlevel d v n bool ls_out bool_verti
          )
  (if(setq str_las(car lst))
      T
    (setq height_ground(cadr lst)))
  
  (setq cosdist(caddr vec_proj) num_limit 1000
        bool_verti(<(distance(list 0 0)(carxy vec_proj))1e-8))
  
  (cond
   ((if vec_verti(<(abs cosdist)1e-8))
    )
   
   ((<(abs cosdist)1e-8)
    (x-alert(list 25237 24433 26041 21521 12364 "XY" 24179 38754 12395 23550 12375 24179 34892 12391 12354 12427 12383 12417 25237 24433 12391 12365 12414 12379 12435
                  "\n" 12499 12517 12540 12398 26041 21521 12434 20462 27491 12375
                  12390 12289 "Enter" 12434 25276 12375 12390 12367 12384 12373 12356 
                  ))
    ;;ビューの方向を修正してEnter
    (getint(mix_strasc(list "\n" 12499 12517 12540 12398 26041 21521 12434 20462 27491 12375 12390 "Enter" )))
    (cal_viewtopleft textsize_guide_bo_temp x_guidebase y_guidebase_temp 10. T )
    
    nil)
   (height_ground
    (mapcar
     '(lambda(p / d)
        (setq d(/(- height_ground(caddr p))cosdist))
        (mapcar '(lambda(a b)(+ a(* d b)))p vec_proj)
        )
     ls_p)
    )
   
   ((setq dict(cdr(assoc str_las ls_lasgrid)))
    (if(vl-catch-all-error-p
        (setq xrec(vl-catch-all-apply 'vla-Item (list dict "GRID-SIZE_CENTER"))))
        (progn(alert "LASDATAERROR01")(quit)))
    (vla-GetXRecordData xrec 'array_type 'array_data )
    (setq lst(if array_data
                 (mapcar 'vlax-variant-value
                         (vlax-safearray->list array_data)))
          )
    (mapcar 'set '(size_grid z_ave z_min z_max)lst)

    (if(<(setq sindist(sqrt(- 1.(expt cosdist 2))))1e-8)T
      (setq delta_proj(/ size_grid sindist)))
    
    (if(<(caddr vec_proj)0)((lambda(a b)(setq p_min b p_max a))p_min p_max))
    
    (mapcar
     '(lambda(p / num z zz nx0 ny0 nx1 ny1 bool p_end p_min p_max p_plane0 p_plane1 ang_plane
                bool_yx nx ny p p_pre p0 p1 deltaz pb0 pb1)
        
        (setq d(/(- z_min(caddr p))cosdist)
              p_min(mapcar '(lambda(a b)(+ a(* d b)))p vec_proj)
              d(/(- z_max(caddr p))cosdist)
              p_max(mapcar '(lambda(a b)(+ a(* d b)))p vec_proj)
              p_plane0(carxy p_min)p_plane1(carxy p_max)
              ang_plane(angle p_plane0 p_plane1)
             
              nx0(/(car p_plane0)size_grid) nx0(fix(- nx0(abs(rem nx0 1.))))
              ny0(/(cadr p_plane0)size_grid)ny0(fix(- ny0(abs(rem ny0 1.))))
              nx1(/(car p_plane1)size_grid) nx1(fix(- nx1(abs(rem nx1 1.))))
              ny1(/(cadr p_plane1)size_grid)ny1(fix(- ny1(abs(rem ny1 1.))))
              
              )

        
        (if(and(= nx0 nx1)(= ny0 ny1))
            (progn
              (cal_weight_grid p_plane0 dict size_grid 3);;25点
              
              
              )
          (progn
            (setq d(distance p_min p_max)
                  n(1+(fix(/ d delta_proj)))
                  d(/ d n) v(mapcar '(lambda(a)(* d a))vec_proj)
                  n(1+ n)
                  p(mapcar '- p_min v)
                  bool T int_pm nil)
            
            ;;前後で正負逆転するところを見つける
            (while(and bool(>(setq n(1- n))-1))
              (setq p(mapcar '+ p v)
                    nx(/(car p)size_grid) nx(fix(- nx(abs(rem nx 1.))))
                    ny(/(cadr p)size_grid)ny(fix(- ny(abs(rem ny 1.)))))

              ;; (vla-addcircle
              ;;  (vla-get-modelspace(vla-get-activedocument(vlax-get-acad-object)))
              ;;  (vlax-3d-point p)0.01)

              
              (if(setq z((lambda(str / xrec array_type array_data)
                           (if(vl-catch-all-error-p
                               (setq xrec(vl-catch-all-apply 'vla-Item (list dict str))))
                               nil
                             (progn
                               (vla-GetXRecordData xrec 'array_type 'array_data )
                               (vlax-variant-value(car(vlax-safearray->list array_data)))
                               ))
                           )
                         (strcat(itoa nx)"$"(itoa ny)))
                       )
                  (progn
                    (setq deltaz(- z(caddr p)))
                    (if(if int_pm(<(* int_pm deltaz)0))
                        (setq bool nil pb1 p)
                      (setq int_pm(if(> deltaz 0)1 -1)
                            pb0 p))
                    ))
              )
            
            (if bool nil
              (progn
                (setq p0(cal_weight_grid pb0 dict size_grid nil)
                      p1(cal_weight_grid pb1 dict size_grid nil)
                      )

                ;; (vla-addline
                ;;  (vla-get-modelspace(vla-get-activedocument(vlax-get-acad-object)))
                ;;  (vlax-3d-point p0)(vlax-3d-point p1))
                ;; (vla-addline
                ;;  (vla-get-modelspace(vla-get-activedocument(vlax-get-acad-object)))
                ;;  (vlax-3d-point pb0)(vlax-3d-point pb1))

                
                (inters p0 p1 pb0 pb1);;nilつけない
                ))
            )
          )
        )
     ls_p)
    )
   (T;;投影対象がありません
    (x-alert(list 25237 24433 23550 35937 12364 12354 12426 12414 12379 12435))
    nil
    )
   
   )
  
  )


(defun get-polyface-data (ename / e data verts faces int_type)
  (setq e (entnext ename))
  (while e
    (setq data (entget e)int_type(cdr (assoc 70 data))
          e(if (= "SEQEND" (cdr (assoc 0 data)))nil(entnext e)) )
    (cond
     ((= int_type 192) ;; 頂点
      (setq verts (cons(cdr (assoc 10 data))verts))
      )
     ((= int_type 128) ;; フェース
      (setq faces(cons(mapcar '(lambda(i)(1-(abs(cdr(assoc i data)))))
                              (list 71 72 73 74))faces) )
      )
     )
    )
  (setq verts(reverse verts))
  (mapcar '(lambda(lst)(mapcar '(lambda(i j)(nth i verts))lst(list 1 2 3)))
          faces)
  )




(princ)





























