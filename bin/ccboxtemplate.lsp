;;;;=====================================================================
;;;; ccboxtemplate.lsp
;;;;   terraduct3D 追加モジュール : 特殊部テンプレート
;;;;
;;;;   td3dtemplate    … テンプレートの作成/編集/削除の画面
;;;;                     terraduct3D.lsp のメニューから呼ばれる(コマンドではない)
;;;;                     画面を閉じるとき、選択中の名前を str_templateccbox に返す
;;;;   c:td3dtmpexport … テンプレートを CSV に書き出し
;;;;   c:td3dtmpimport … CSV からテンプレートを取り込み
;;;;
;;;;   特殊部の配置そのものは terraduct3D.lsp が行う。
;;;;   このファイルはテンプレートの管理と、形状を組み立てる部品を提供する。
;;;;   terraduct3D.lsp から使うもの :
;;;;     td3dtemplate / td3d_tmp_read / td3d_tmp_fill / td3d_tmp_paramdef
;;;;     td3d_tmp_makeparts / td3d_tmp_copyparts / td3d_tmp_deleteparts
;;;;     td3d_tmp_alert / td3d_tmp_numstr / td3d_tmp_ccboxhead
;;;;
;;;;   依存 : %library.lsp  (mix_strasc rac_sld code_loft_solid unit_vector
;;;;                          cross_product carxyz set_xda write_strlist)
;;;;   ロード順 : %library.lsp のあと
;;;;
;;;;   断面の定義
;;;;     進行方向(2点クリックの方向)に直交する鉛直断面で内空を指定し、
;;;;     その断面を進行方向へ押し出す(ボックスカルバート型)。
;;;;     ハンチは内空四隅に入る。
;;;;     前面(起点側)・背面(終点側)の厚さを 0 にするとその面が開口になる。
;;;;=====================================================================

(vl-load-com)

(setq td3d_tmp_dictname  "terraduct3d_ccboxtemplate" ;;テンプレート保存用の図面辞書
      td3d_tmp_blockhead "TD3D-CCBOXTMP$"            ;;サンプルブロック名の接頭辞
      td3d_tmp_ccboxhead "CCBOX$"                    ;;配置した特殊部のブロック名接頭辞
      td3d_tmp_samplelen 1.0                         ;;サンプルブロックの延長
      td3d_tmp_quiet     nil                         ;;T のとき部材リストを表示しない
      )

;;---------------------------------------------------------------------
;; 共通ユーティリティ
;;---------------------------------------------------------------------
(defun td3d_tmp_alert( str )
  (alert str)(princ))

;;数値を短く表示する(既定値の提示用)
(defun td3d_tmp_numstr( a )
  (if a(rtos a 2 4)""))

;;アクティブ図面の辞書コレクション
(defun td3d_tmp_dicts( / )
  (vla-get-Dictionaries(vla-get-ActiveDocument(vlax-get-acad-object))))

;;テンプレート辞書を得る。bool_create が T のとき無ければ作る
(defun td3d_tmp_dict( bool_create / dicts dict )
  (setq dicts(td3d_tmp_dicts))
  (if(vl-catch-all-error-p
      (setq dict(vl-catch-all-apply 'vla-Item(list dicts td3d_tmp_dictname))))
      (setq dict(if bool_create(vla-Add dicts td3d_tmp_dictname)nil)))
  dict)

;;---------------------------------------------------------------------
;; パラメータ定義
;;   (キー 表示名 既定値 必須か)
;;   XRecord にはこの順序で 1040(実数) を並べて保存する
;;   ※順序を変更すると既存テンプレートが読めなくなるため、
;;     追加するときは必ず末尾に足すこと
;;---------------------------------------------------------------------
(defun td3d_tmp_paramdef( / )
  (list
   (list "WIDTH"   (mix_strasc(list 20869 31354 24133))                1.500 T  )
   (list "HEIGHT"  (mix_strasc(list 20869 31354 39640 12373))              1.800 T  )
   (list "TTOP"    (mix_strasc(list 38914 29256 21402))                0.200 T  )
   (list "TBOTTOM" (mix_strasc(list 24213 29256 21402))                0.200 T  )
   (list "TSIDE"   (mix_strasc(list 20596 22721 21402))                0.200 T  )
   (list "HTOPU"   (mix_strasc(list 12495 12531 12481 "(" 19978 ") " 27700 24179))       0.000 nil)
   (list "HTOPV"   (mix_strasc(list 12495 12531 12481 "(" 19978 ") " 37467 30452))       0.000 nil)
   (list "HBOTU"   (mix_strasc(list 12495 12531 12481 "(" 19979 ") " 27700 24179))       0.000 nil)
   (list "HBOTV"   (mix_strasc(list 12495 12531 12481 "(" 19979 ") " 37467 30452))       0.000 nil)
   (list "TFRONT"  (mix_strasc(list 21069 38754 "(" 36215 28857 20596 ")" 21402 " (0" 12391 38283 21475 ")")) 0.200 nil)
   (list "TBACK"   (mix_strasc(list 32972 38754 "(" 32066 28857 20596 ")" 21402 " (0" 12391 38283 21475 ")")) 0.200 nil)
   ))


;;---------------------------------------------------------------------
;; 辞書 入出力
;;---------------------------------------------------------------------
;;テンプレート名の一覧
(defun td3d_tmp_names( / dict ls_name str )
  (setq ls_name(list))
  (if(setq dict(td3d_tmp_dict nil))
      (vlax-for
       xrec dict
       (if(vl-catch-all-error-p
           (setq str(vl-catch-all-apply 'vla-get-name(list xrec))))
           T
         (setq ls_name(cons str ls_name)))))
  (reverse ls_name))

;;安全な nth
;;  nth は環境によっては空リストや範囲外で
;;  「引数の型が違います: consp nil」を出すため、car/cdr だけで書く
(defun td3d_tmp_nth( num lst / )
  (while(and lst(> num 0))(setq lst(cdr lst)num(1- num)))
  (car lst))

;;読み込んだ値リストをパラメータ定義の個数に揃える
;;  項目を後から追加しても古いテンプレートが読めるように、
;;  欠けている項目と実数でない項目は既定値で埋める
;;  ls_val が nil でも必ず定義個数のリストを返す
(defun td3d_tmp_fill( ls_val / )
  (mapcar '(lambda(lst / val)
             (setq val(car ls_val) ls_val(cdr ls_val))
             (cond
              ((=(type val)'REAL) val)
              ((=(type val)'INT)  (float val))
              (T (caddr lst))))
          (td3d_tmp_paramdef)))

;;テンプレートを読む。無ければ nil
(defun td3d_tmp_read( str_name / dict xrec array_type array_data ls_val )
  (if(setq dict(td3d_tmp_dict nil))
      (if(vl-catch-all-error-p
          (setq xrec(vl-catch-all-apply 'vla-Item(list dict str_name))))
          nil
        (progn
          (vla-GetXRecordData xrec 'array_type 'array_data)
          (if array_data
              (progn
                (setq ls_val(mapcar 'vlax-variant-value
                                    (vlax-safearray->list array_data)))
                (td3d_tmp_fill ls_val)))))))

;;テンプレートを書く
(defun td3d_tmp_write( str_name ls_val / dict xrec num array_type array_data )
  (setq dict(td3d_tmp_dict T) num(length ls_val))
  (if(vl-catch-all-error-p
      (setq xrec(vl-catch-all-apply 'vla-Item(list dict str_name))))
      (setq xrec(vla-AddXRecord dict str_name)))
  (setq array_data(vlax-make-safearray vlax-vbVariant(cons 0(1- num)))
        array_type(vlax-make-safearray vlax-vbInteger(cons 0(1- num))))
  (vlax-safearray-fill array_type(mapcar '(lambda(a)1040)ls_val))
  (vlax-safearray-fill array_data ls_val)
  (vla-SetXRecordData xrec array_type array_data)
  xrec)

;;テンプレートを消す(辞書とサンプルブロックの両方)
(defun td3d_tmp_erase( str_name / dict xrec blocks blk set_ent num vnam )
  (if(setq dict(td3d_tmp_dict nil))
      (if(vl-catch-all-error-p
          (setq xrec(vl-catch-all-apply 'vla-Item(list dict str_name))))
          T
        (vla-delete xrec)))
  (setq blocks(vla-get-Blocks(vla-get-ActiveDocument(vlax-get-acad-object))))
  (if(vl-catch-all-error-p
      (setq blk(vl-catch-all-apply
                'vla-Item(list blocks(strcat td3d_tmp_blockhead str_name)))))
      T
    (progn
      (if(setq set_ent(ssget "X"(list(cons 2(strcat td3d_tmp_blockhead str_name)))))
          (progn
            (setq num(sslength set_ent))
            (while(>(setq num(1- num))-1)
              (setq vnam(vlax-ename->vla-object(ssname set_ent num)))
              (vla-delete vnam))))
      (vl-catch-all-apply 'vla-delete(list blk))))
  (princ))

;;---------------------------------------------------------------------
;; パラメータの妥当性検査
;;   戻り値 : nil = 正常 / 文字列 = エラー内容
;;---------------------------------------------------------------------
(defun td3d_tmp_validate( ls_val / w h ttop tbot tside hu hv du dv tfr tbk )
  (mapcar 'set '(w h ttop tbot tside hu hv du dv tfr tbk)(td3d_tmp_fill ls_val))
  (cond
   ((or(<= w 0.)(<= h 0.))
    (mix_strasc(list 20869 31354 24133 12392 20869 31354 39640 12373 12399 27491 12398 20516 12434 20837 21147 12375 12390 12367 12384 12373 12356)))
   ((or(<= ttop 0.)(<= tbot 0.)(<= tside 0.))
    (mix_strasc(list 38914 29256 21402 12539 24213 29256 21402 12539 20596 22721 21402 12399 27491 12398 20516 12434 20837 21147 12375 12390 12367 12384 12373 12356)))
   ((or(< hu 0.)(< hv 0.)(< du 0.)(< dv 0.)(< tfr 0.)(< tbk 0.))
    (mix_strasc(list 12495 12531 12481 12392 21069 38754 12539 32972 38754 12398 21402 12373 12395 36000 12398 20516 12399 25351 23450 12391 12365 12414 12379 12435)))
   ((>=(+ hv dv)h)
    (mix_strasc(list 12495 12531 12481 12398 37467 30452 23544 27861 12398 21512 35336 12364 20869 31354 39640 12373 20197 19978 12391 12377)))
   ((>=(* 2.(max hu du))w)
    (mix_strasc(list 12495 12531 12481 12398 27700 24179 23544 27861 12364 20869 31354 24133 12395 23550 12375 12390 22823 12365 12377 12366 12414 12377)))
   (T nil)))

;;---------------------------------------------------------------------
;; 中空ボックスを「部材ごとの角柱」で組み立てる
;;   ls_val          : パラメータ値リスト
;;   p_center_bottom : 平面中心 かつ 底版下端の標高 にある点
;;   vecx            : 進行方向(水平・単位ベクトル)
;;   length_sld      : 延長
;;   戻り値 : ((VLAオブジェクト . 部材名) ...) / 失敗時 nil
;;
;;   ブーリアン差分は使わない。
;;     - 内空がそのまま見える(中実に見えない)
;;     - 部材ごとに数量・属性を持てる
;;     - CSV/IFC 出力で 1 部材 = 1 RECT として書き出せる
;;   ハンチだけは三角柱なので code_loft_solid を使うが、失敗しても
;;   本体の箱は残るようにしてある。
;;
;;   rac_sld( 半長 半幅 高さ 底面中心 0 上方向 進行方向 画層 xdata )
;;     半長 : 進行方向の 1/2 / 半幅 : 横方向の 1/2 / 高さ : 全高
;;     既存の特殊部作成と同じ使い方に合わせている
;;---------------------------------------------------------------------
(defun td3d_tmp_makeparts( ls_val p_center_bottom vecx length_sld str_layer
                           / w h ttop tbot tside hu hv du dv tfr tbk
                             vecz vecu len_in x_in ls_out fn_box fn_haunch )
  (setq ls_val(td3d_tmp_fill ls_val))
  (mapcar 'set '(w h ttop tbot tside hu hv du dv tfr tbk)ls_val)
  (setq vecz(list 0. 0. 1.)
        vecx(unit_vector(carxyz vecx 0.))
        vecu(unit_vector(cross_product vecz vecx))
        ;;内空は前面・背面の壁にはさまれた範囲
        len_in(- length_sld tfr tbk)
        x_in(* 0.5(- tfr tbk))   ;;内空の中心が進行方向にずれる量
        ls_out(list))

  ;;角柱を 1 つ作って ls_out に積む
  ;;  dz : 底面の高さ / dx : 進行方向のずれ / dulen : 横方向のずれ
  (setq fn_box
        (lambda(half_x half_u height dz dx dulen str_part / p ena vna)
          (setq p(mapcar '(lambda(a x u)(+ a(* dx x)(* dulen u)))
                         (mapcar '+ p_center_bottom(list 0. 0. dz))
                         vecx vecu)
                ena(rac_sld half_x half_u height p 0. vecz vecx str_layer nil))
          (if ena
              (progn
                (setq vna(vlax-ename->vla-object ena)
                      ls_out(cons(cons vna str_part)ls_out))
                vna))))

  ;;ハンチ(三角柱)を 1 つ作る
  ;;  sign_u sign_v で四隅を指定する。
  ;;  三角形は内空断面の座標系(u:横 / v:上)で反時計回りに並べ、
  ;;  進行方向へロフトして三角柱にする。
  ;;  ※ ext_sld は本体コードから一度も呼ばれていない未検証関数のため使わず、
  ;;    本体の管路作成でも使われている code_loft_solid を使う。
  (setq fn_haunch
        (lambda(size_u size_v sign_u sign_v
                        / uu vv ls_uv p_start ls_fro ls_rea ls_ena vna)
          (if(or(<= size_u 0.)(<= size_v 0.))nil
            (progn
              (setq uu(* sign_u 0.5 w) vv(* sign_v 0.5 h)
                    ;;左上・右下の隅はそのままだと時計回りになるため頂点順を逆にする
                    ls_uv(if(>(* sign_u sign_v)0.)
                             (list(list uu(- vv(* sign_v size_v)))
                                  (list uu vv)
                                  (list(- uu(* sign_u size_u))vv))
                           (list(list(- uu(* sign_u size_u))vv)
                                (list uu vv)
                                (list uu(- vv(* sign_v size_v)))))
                    ;;起点側の端面にある内空断面の中心
                    p_start(mapcar '(lambda(a x)(+ a(*(- x_in(* 0.5 len_in))x)))
                                   (mapcar '+ p_center_bottom
                                           (list 0. 0.(+ tbot(* 0.5 h))))
                                   vecx)
                    ;;断面座標(u,v)を世界座標へ
                    ls_fro(mapcar '(lambda(p)
                                     (mapcar '(lambda(a u z)
                                                (+ a(*(car p)u)(*(cadr p)z)))
                                             p_start vecu(list 0. 0. 1.)))
                                  ls_uv)
                    ;;終点側へ平行移動したものを後面にしてロフト
                    ls_rea(mapcar '(lambda(p)
                                     (mapcar '(lambda(a x)(+ a(* len_in x)))p vecx))
                                  ls_fro)
                    ls_ena(vl-catch-all-apply
                           'code_loft_solid(list(list ls_fro ls_rea)nil nil)))
              (if(or(vl-catch-all-error-p ls_ena)(null ls_ena)(null(car ls_ena)))nil
                (progn
                  (setq vna(vlax-ename->vla-object(car ls_ena)))
                  (vl-catch-all-apply 'vla-put-layer(list vna str_layer))
                  (setq ls_out(cons(cons vna "CCBOXHAUNCH")ls_out))
                  vna))))))

  (cond
   ((< length_sld 1e-6) nil)
   ((< len_in 1e-6) nil)
   (T
    ;;底版・頂版(側壁の外側まで含む全幅)
    (fn_box (* 0.5 length_sld)(+(* 0.5 w)tside) tbot 0. 0. 0. "CCBOXBOTTOM")
    (fn_box (* 0.5 length_sld)(+(* 0.5 w)tside) ttop (+ tbot h) 0. 0. "CCBOXTOP")

    ;;側壁(左右)
    (fn_box (* 0.5 length_sld)(* 0.5 tside) h tbot 0.
            (+(* 0.5 w)(* 0.5 tside)) "CCBOXWALL")
    (fn_box (* 0.5 length_sld)(* 0.5 tside) h tbot 0.
            (-(+(* 0.5 w)(* 0.5 tside))) "CCBOXWALL")

    ;;前面(起点側)・背面(終点側)の壁。厚さ 0 のときはその面が開口になる
    (if(> tfr 0.)
        (fn_box (* 0.5 tfr)(* 0.5 w) h tbot
                (-(-(* 0.5 length_sld)(* 0.5 tfr))) 0. "CCBOXFRONT"))
    (if(> tbk 0.)
        (fn_box (* 0.5 tbk)(* 0.5 w) h tbot
                (-(* 0.5 length_sld)(* 0.5 tbk)) 0. "CCBOXBACK"))

    ;;ハンチ(内空四隅)
    (fn_haunch hu hv  1.  1.)
    (fn_haunch hu hv -1.  1.)
    (fn_haunch du dv  1. -1.)
    (fn_haunch du dv -1. -1.)

    ;;どの部材が作られたかをコマンドラインに出す(確認用)
    ;;一括登録中は td3d_tmp_quiet を T にして黙らせる
    (setq ls_out(reverse ls_out))
    (if td3d_tmp_quiet T
      (progn
        (princ (mix_strasc(list "\\n" 20316 25104 12375 12383 37096 26448 " (" (itoa(length ls_out)) ") : ")))
        (mapcar '(lambda(a)(princ(strcat(cdr a)" ")))ls_out)
        (if(<= tfr 0.)
            (princ (mix_strasc(list "\\n  " 8251 21069 38754 "(" 36215 28857 20596 ")" 21402 12364 " 0 " 12398 12383 12417 21069 38754 12399 38283 21475 12391 12377))))
        (if(<= tbk 0.)
            (princ (mix_strasc(list "\\n  " 8251 32972 38754 "(" 32066 28857 20596 ")" 21402 12364 " 0 " 12398 12383 12417 32972 38754 12399 38283 21475 12391 12377))))))
    ls_out)))

;;部材リストを削除する
(defun td3d_tmp_deleteparts( ls_part / )
  (mapcar '(lambda(a)(vl-catch-all-apply 'vla-delete(list(car a))))ls_part)
  (princ))

;;部材リストをブロック定義へコピーする
(defun td3d_tmp_copyparts( ls_part blk / num )
  (setq num(length ls_part))
  (if(< num 1)nil
    (progn
      (vla-copyobjects
       (vla-get-ActiveDocument(vlax-get-acad-object))
       (vlax-make-variant
        (vlax-safearray-fill
         (vlax-make-safearray vlax-vbObject(cons 0(1- num)))
         (mapcar 'car ls_part)))
       blk)
      T)))

;;---------------------------------------------------------------------
;; サンプルブロック(テンプレートの形状見本)を作り直す
;;   原点に 延長 td3d_tmp_samplelen で作成し、ブロック定義として登録する
;;   モデル空間には挿入しない(定義だけを図面に残す)
;;---------------------------------------------------------------------
(defun td3d_tmp_makesampleblock( str_name ls_val
                                 / blocks str_bname blk ls_part set_ent num vnam )
  (setq blocks(vla-get-Blocks(vla-get-ActiveDocument(vlax-get-acad-object)))
        str_bname(strcat td3d_tmp_blockhead str_name))

  ;;既存の同名ブロックを消す
  (if(vl-catch-all-error-p
      (setq blk(vl-catch-all-apply 'vla-Item(list blocks str_bname))))
      T
    (progn
      (if(setq set_ent(ssget "X"(list(cons 2 str_bname))))
          (progn
            (setq num(sslength set_ent))
            (while(>(setq num(1- num))-1)
              (setq vnam(vlax-ename->vla-object(ssname set_ent num)))
              (vla-delete vnam))))
      (vl-catch-all-apply 'vla-delete(list blk))))

  (setq ls_part(td3d_tmp_makeparts ls_val(list 0. 0. 0.)(list 1. 0. 0.)
                                   td3d_tmp_samplelen(getvar "CLAYER")))
  (if(null ls_part)nil
    (progn
      (setq blk(vla-Add blocks(vlax-3d-point 0 0 0)str_bname))
      (td3d_tmp_copyparts ls_part blk)
      (td3d_tmp_deleteparts ls_part)
      (vl-catch-all-apply 'vlax-release-object(list blk))
      str_bname)))

;;---------------------------------------------------------------------
;; 標準テンプレート(国土交通省型)
;;   図面内に無い型式だけを自動登録する(1 つも無いときに限らない)。
;;   出典 : 国土交通省型(直轄で使われる標準型式)の内空寸法表。
;;   単位 : 表は mm。登録時に 1/1000 して図面単位(m)にする。
;;   注意 : 表にあるのは蓋厚 t だけなので、頂版・底版・側壁・前面・背面の
;;          すべてに t を入れている。実際の部材厚が判るものは修正して使うこと。
;;          ハンチは 0(表に記載が無いため)。
;;---------------------------------------------------------------------
(defun td3d_tmp_defaultdef( / )
  ;;(名前 内空幅 内空高さ 厚さ)  単位 mm
  (list
   (list (mix_strasc(list 22269 20132 30465 "_I" 22411))             1200 1350 130)
   (list (mix_strasc(list 22269 20132 30465 "_I" 22411 "BOX"))          1200 1800 130)
   (list (mix_strasc(list 22269 20132 30465 "_" 38651 21147 "II" 22411 "_" 27178 32622))    850 1100 130)
   (list (mix_strasc(list 22269 20132 30465 "_" 38651 21147 "II" 22411 "_" 30452 19978))    900 1100 130)
   (list (mix_strasc(list 22269 20132 30465 "_" 36890 20449 "II" 22411 "_H1000"))  1200 1000 130)
   (list (mix_strasc(list 22269 20132 30465 "_" 36890 20449 "II" 22411 "_H1150"))  1200 1150 130)
   (list (mix_strasc(list 22269 20132 30465 "_" 36890 20449 "II" 22411 "BOX"))     1200 1500 130)
   (list (mix_strasc(list 22269 20132 30465 "_" 20998 23696 26717 "_450x500"))   450  500 125)
   (list (mix_strasc(list 22269 20132 30465 "_" 20998 23696 26717 "_550x800"))   550  800 140)
   (list (mix_strasc(list 22269 20132 30465 "_" 31777 26131 12488 12521 12501))        400  400 120)
   ))

;;標準テンプレートを登録する。戻り値 : 登録した件数
;;  ※ thk という名前を使っているのは、t が AutoLISP の真値のため
(defun td3d_tmp_makedefault( ls_def / num bool_quiet )
  (if(null ls_def)(setq ls_def(td3d_tmp_defaultdef)))
  (setq num 0 bool_quiet td3d_tmp_quiet td3d_tmp_quiet T)
  (mapcar
   '(lambda(lst / str_name w h thk ls_val)
      (setq str_name(car lst)
            w  (/(float(cadr lst))1000.)
            h  (/(float(caddr lst))1000.)
            thk(/(float(cadddr lst))1000.)
            ;;WIDTH HEIGHT TTOP TBOTTOM TSIDE ハンチ4つ TFRONT TBACK
            ls_val(list w h thk thk thk 0. 0. 0. 0. thk thk))
      (if(td3d_tmp_validate ls_val)T
        (progn
          (td3d_tmp_write str_name ls_val)
          (td3d_tmp_makesampleblock str_name ls_val)
          (setq num(1+ num))
          (princ (mix_strasc(list "\\n  " str_name))))))
   ls_def)
  (setq td3d_tmp_quiet bool_quiet)
  num)

;;標準テンプレートのうち、図面に登録されていないものだけを補う
;;  「テンプレートが 1 つも無いとき」ではなく
;;  「国交省型のうち欠けているものがあるとき」に追加する。
;;  利用者が独自に作ったテンプレートがあっても国交省型は必ず揃う。
;;  すでにある同名のテンプレートは(内容を変えていても)そのまま残す。
;;  戻り値 : 追加した件数 / 追加が無ければ nil
(defun td3d_tmp_ensuredefault( / ls_now ls_add num )
  (setq ls_now(td3d_tmp_names)
        ls_add(vl-remove nil
                         (mapcar '(lambda(lst)
                                    (if(td3d_tmp_pos(car lst)ls_now)nil lst))
                                 (td3d_tmp_defaultdef))))
  (if(null ls_add)nil
    (progn
      (princ (mix_strasc(list "\\n" 22269 22303 20132 36890 30465 22411 12398 12358 12385 30331 37682 12373 12428 12390 12356 12394 12356 12418 12398 12434 36861 21152 12375 12414 12377)))
      (setq num(td3d_tmp_makedefault ls_add))
      (princ (mix_strasc(list "\\n" 22269 22303 20132 36890 30465 22411 12434 " " (itoa num) " " 20214 36861 21152 12375 12414 12375 12383)))
      num)))


;;=====================================================================
;; ダイアログ (DCL)
;;   コマンドウィンドウが狭くても使えるように、寸法入力は画面で行う。
;;   action_tile から呼ばれる関数はグローバルに定義する必要があるため、
;;   下記 3 つと受け渡し用の 2 変数だけはグローバルにしている。
;;     td3d_tmp_ls_name … 一覧に表示中のテンプレート名
;;     td3d_tmp_result  … ダイアログの結果 (名前 値リスト)
;;=====================================================================

;;DCL を書き出す一時フォルダ
(defun td3d_tmp_tempdir( / )
  (if(and(boundp 'str_path_tempdirectory)str_path_tempdirectory)
      str_path_tempdirectory
    (getvar "TEMPPREFIX")))

;;DCL ファイルを書き出す。戻り値 : パス / nil
(defun td3d_tmp_writedcl( / str_path open_file )
  (setq str_path(strcat(td3d_tmp_tempdir)"td3dccboxtemplate.dcl")
        open_file(open str_path "w"))
  (if(null open_file)nil
    (progn
      (write_strlist
       open_file
       (list
        "td3dtmp :dialog"
        "{"
        (mix_strasc(list " label = \"" 29305 27530 37096 12486 12531 12503 12524 12540 12488 "\";"))
        " :row"
        " {"
        "  :boxed_column"
        "  {"
        (mix_strasc(list "   label = \"" 30331 37682 28168 12415 "\";"))
        "   :list_box { key = \"tmplist\"; width = 26; height = 18; }"
        "  }"
        "  :boxed_column"
        "  {"
        (mix_strasc(list "   label = \"" 23544 27861 " (" 22259 38754 21336 20301 ")\";"))
        (mix_strasc(list "   :edit_box { key = \"name\"; label = \"" 12486 12531 12503 12524 12540 12488 21517 "\"; edit_width = 18; }"))
        "   spacer;"
        ;;パラメータ定義から入力欄を作る(定義を増やせば欄も増える)
        (mapcar '(lambda(lst)
                   (mix_strasc(list "   :edit_box { key = \"" (car lst) "\"; label = \"" (cadr lst) "\"; edit_width = 10; }")))
                (td3d_tmp_paramdef))
        "   spacer;"
        (mix_strasc(list "   :text { label = \"" 8251 12495 12531 12481 12399 27700 24179 12392 37467 30452 12398 20001 26041 12364 27491 12398 12392 12365 12384 12369 20837 12426 12414 12377 "\"; }"))
        "  }"
        " }"
        " :row"
        " {"
        "  alignment = right;"
        "  fixed_width = true;"
        (mix_strasc(list "  :button { key = \"btnsave\"; label = \"" 20445 23384 "\"; width = 12;"
                         " fixed_width = true; is_default = true; }"))
        (mix_strasc(list "  :button { key = \"btndel\"; label = \"" 21066 38500 "\"; width = 12;"
                         " fixed_width = true; }"))
        (mix_strasc(list "  :button { key = \"btnexp\"; label = \"CSV" 26360 20986 "\"; width = 12;"
                         " fixed_width = true; }"))
        (mix_strasc(list "  :button { key = \"btnimp\"; label = \"CSV" 21462 36796 "\"; width = 12;"
                         " fixed_width = true; }"))
        (mix_strasc(list "  :button { key = \"btnclose\"; label = \"" 27770 23450  "\"; width = 12;"
                         " fixed_width = true; is_cancel = true; }"))
        " }"
        "}"
        ))
      (close open_file)
      str_path)))

;;入力欄へ値を流し込む(ls_val が nil でも既定値で埋まる)
(defun td3d_tmp_settiles( ls_val )
  (mapcar '(lambda(lst val)(set_tile(car lst)(td3d_tmp_numstr val)))
          (td3d_tmp_paramdef)(td3d_tmp_fill ls_val))
  (princ))

;;入力欄から値を読む
(defun td3d_tmp_gettiles( / )
  (mapcar '(lambda(lst)(atof(get_tile(car lst))))(td3d_tmp_paramdef)))

;;一覧で選ばれたテンプレートを入力欄に読み込む
(defun td3d_tmp_dlg_select( / num str_name ls_val )
  (setq num(atoi(get_tile "tmplist")))
  (if(setq str_name(td3d_tmp_nth num td3d_tmp_ls_name))
      (if(setq ls_val(td3d_tmp_read str_name))
          (progn
            (set_tile "name" str_name)
            (td3d_tmp_settiles ls_val))))
  (princ))

;;保存ボタン(実際の保存はダイアログを閉じてから行う)
(defun td3d_tmp_dlg_save( / str_name ls_val str_err )
  (setq str_name(get_tile "name")
        ls_val(td3d_tmp_gettiles))
  (cond
   ((= str_name "")
    (alert (mix_strasc(list 12486 12531 12503 12524 12540 12488 21517 12434 20837 21147 12375 12390 12367 12384 12373 12356))))
   ((setq str_err(td3d_tmp_validate ls_val))
    (alert str_err))
   (T
    (setq td3d_tmp_result(list str_name ls_val))
    (done_dialog 2)))
  (princ))

;;削除ボタン
(defun td3d_tmp_dlg_delete( / num str_name )
  (setq num(atoi(get_tile "tmplist")))
  (if(setq str_name(td3d_tmp_nth num td3d_tmp_ls_name))
      (progn
        (setq td3d_tmp_result(list str_name nil))
        (done_dialog 3))
    (alert (mix_strasc(list 21066 38500 12377 12427 12486 12531 12503 12524 12540 12488 12434 19968 35239 12363 12425 36984 12435 12391 12367 12384 12373 12356))))
  (princ))

;;=====================================================================
;; コマンド : テンプレートの作成 / 編集 / 削除
;;=====================================================================
(defun td3dtemplate( / str_path load_dcl int_ret bool_loop
                       str_name ls_val )

  (if(null(setq str_path(td3d_tmp_writedcl)))
      (td3d_tmp_alert (mix_strasc(list 12480 12452 12450 12525 12464 23450 32681 12501 12449 12452 12523 12434 26360 12365 20986 12379 12414 12379 12435 12391 12375 12383)))
    (progn
      (td3d_tmp_ensuredefault)
      (setq bool_loop T)
      
      (while bool_loop
        (setq td3d_tmp_ls_name(td3d_tmp_names)
              td3d_tmp_result nil
              load_dcl(load_dialog str_path))
        (cond
         ((< load_dcl 0)
          (td3d_tmp_alert (mix_strasc(list 12480 12452 12450 12525 12464 12434 35501 12415 36796 12417 12414 12379 12435 12391 12375 12383)))
          (setq bool_loop nil))
         ((null(new_dialog "td3dtmp" load_dcl))
          (unload_dialog load_dcl)
          (td3d_tmp_alert (mix_strasc(list 12480 12452 12450 12525 12464 12434 38283 12369 12414 12379 12435 12391 12375 12383)))
          (setq bool_loop nil))
         (T
          (start_list "tmplist")
          (if td3d_tmp_ls_name(mapcar 'add_list td3d_tmp_ls_name))
          (end_list)
          (set_tile "name" "")
          (td3d_tmp_settiles nil);;既定値を表示

          (action_tile "tmplist"  "(td3d_tmp_dlg_select)")
          (action_tile "btnsave"  "(td3d_tmp_dlg_save)")
          (action_tile "btndel"   "(td3d_tmp_dlg_delete)")
          (action_tile "btnexp"   "(done_dialog 4)")
          (action_tile "btnimp"   "(done_dialog 5)")
          (action_tile
           "btnclose" "(setq str_templateccbox(get_tile \"name\"))(done_dialog 0)")

          (setq int_ret(start_dialog))
          (unload_dialog load_dcl)
          (setq load_dcl nil)

          ;;ソリッド作成や辞書の書き換えはダイアログを閉じてから行う
          (cond
           ((= int_ret 2)
            (setq str_name(car td3d_tmp_result)
                  ls_val(cadr td3d_tmp_result))
            (td3d_tmp_write str_name ls_val)
            (if(td3d_tmp_makesampleblock str_name ls_val)
                (princ (mix_strasc(list "\\n" 20445 23384 12375 12414 12375 12383 " : " str_name)))
              (progn
                (princ (mix_strasc(list "\\n" 20445 23384 12375 12414 12375 12383 " : " str_name)))
                (td3d_tmp_alert (mix_strasc(list 12486 12531 12503 12524 12540 12488 12399 20445 23384 12375 12414 12375 12383 12364 12289 24418 29366 12398 20316 25104 12395 22833 25943 12375 12414 12375 12383 "\\n" 23544 27861 12434 35211 30452 12375 12390 12367 12384 12373 12356)))))
            )
           ((= int_ret 3)
            (setq str_name(car td3d_tmp_result))
            (td3d_tmp_erase str_name)
            (princ (mix_strasc(list "\\n" 21066 38500 12375 12414 12375 12383 " : " str_name))))
           ((= int_ret 4)(td3d_tmp_export))
           ((= int_ret 5)(td3d_tmp_import))
           
           (T
            (if(vl-position str_templateccbox td3d_tmp_ls_name)T
              (setq str_templateccbox nil))
            (setq bool_loop nil)
            ))
          ))
        )))
  
  
  
  (princ)
  )


;;=====================================================================
;; CSV 入出力
;;   1 行目 : NAME + パラメータキー
;;            列の順序は問わない。ヘッダー名で対応付けるため、
;;            列が増減しても(古い CSV でも)読める。
;;   2 行目以降 : テンプレート 1 件につき 1 行
;;=====================================================================

;;文字列をダブルクォートで囲む(内部の " は "" にする)
(defun td3d_tmp_qq( str / num str_one str_out )
  (setq str_out "" num 0)
  (while(< num(strlen str))
    (setq num(1+ num)
          str_one(substr str num 1)
          str_out(strcat str_out(if(= str_one "\"")"\"\"" str_one))))
  (strcat "\"" str_out "\""))

;;CSV 1 行をフィールドに分解(ダブルクォート対応)
(defun td3d_tmp_csvsplit( str / len num str_one bool_q str_cur ls_out )
  (setq len(strlen str) num 0 str_cur "" bool_q nil ls_out(list))
  (while(< num len)
    (setq num(1+ num) str_one(substr str num 1))
    (cond
     (bool_q
      (if(= str_one "\"")
          (if(= (substr str(1+ num)1)"\"")
              (setq str_cur(strcat str_cur "\"") num(1+ num))
            (setq bool_q nil))
        (setq str_cur(strcat str_cur str_one))))
     ((= str_one "\"")(setq bool_q T))
     ((= str_one ",")(setq ls_out(cons str_cur ls_out) str_cur ""))
     (T(setq str_cur(strcat str_cur str_one)))))
  (reverse(cons str_cur ls_out)))

;;前後の空白・改行・BOM を取り除く
(defun td3d_tmp_trim( str / )
  (while(and(>(strlen str)0)
            (member(substr str 1 1)(list " " "\t"(chr 65279))))
    (setq str(substr str 2)))
  (while(and(>(strlen str)0)
            (member(substr str(strlen str)1)(list " " "\t" "\r" "\n")))
    (setq str(substr str 1(1-(strlen str)))))
  str)

;;リスト中の位置(見つからなければ nil)
(defun td3d_tmp_pos( str lst / num )
  (setq num 0)
  (while(and lst(/= str(car lst)))(setq lst(cdr lst)num(1+ num)))
  (if lst num nil))

;;num 番目のフィールド(無ければ "")
(defun td3d_tmp_field( num lst / str )
  (if(null num)""
    (progn(setq str(td3d_tmp_nth num lst))(if str str ""))))

;;ファイルを開く(アプリ本体と同じ流儀 : AutoCAD は UTF-8+BOM、他は既定)
(defun td3d_tmp_openwrite( str_path / f )
  (if(and(boundp 'command_for_alter)command_for_alter)
      (open str_path "w")
    (progn
      (if(vl-catch-all-error-p
          (setq f(vl-catch-all-apply 'open(list str_path "w" "utf8"))))
          (setq f(open str_path "w"))
        (if f(write-char 65279 f)))
      f)))

(defun td3d_tmp_openread( str_path / f )
  (if(and(boundp 'command_for_alter)command_for_alter)
      (open str_path "r")
    (if(vl-catch-all-error-p
        (setq f(vl-catch-all-apply 'open(list str_path "r" "utf8"))))
        (open str_path "r")
      f)))

;;---------------------------------------------------------------------
;; 書き出し
;;---------------------------------------------------------------------
(defun td3d_tmp_export( / str_path open_file ls_name num )
  (setq ls_name(td3d_tmp_names))
  (cond
   ((null ls_name)
    (td3d_tmp_alert (mix_strasc(list 26360 12365 20986 12377 12486 12531 12503 12524 12540 12488 12364 12354 12426 12414 12379 12435)))nil)
   ((null(setq str_path(getfiled (mix_strasc(list 12486 12531 12503 12524 12540 12488 12398 26360 12365 20986 12375))
                                 (strcat(getvar "DWGPREFIX")"ccboxtemplate")
                                 "csv" 1)))nil)
   ((null(setq open_file(td3d_tmp_openwrite str_path)))
    (td3d_tmp_alert (mix_strasc(list 12501 12449 12452 12523 12395 26360 12365 36796 12417 12414 12379 12435 12391 12375 12383)))nil)
   (T
    (setq num 0)
    (write-line
     (apply 'strcat(cons "NAME"(mapcar '(lambda(lst)(strcat ","(car lst)))
                                       (td3d_tmp_paramdef))))
     open_file)
    (mapcar
     '(lambda(str_name / ls_val)
        (if(setq ls_val(td3d_tmp_read str_name))
            (progn
              (write-line
               (apply 'strcat
                      (cons(td3d_tmp_qq str_name)
                           (mapcar '(lambda(v)(strcat ","(rtos v 2 6)))ls_val)))
               open_file)
              (setq num(1+ num)))))
     ls_name)
    (close open_file)
    (princ (mix_strasc(list "\\n" 26360 12365 20986 12375 12414 12375 12383 " : " (itoa num) 20214 "  " str_path)))
    (td3d_tmp_alert (mix_strasc(list 26360 12365 20986 12375 12414 12375 12383 "\\n" (itoa num) 20214 "\\n" str_path)))
    str_path)))

;;---------------------------------------------------------------------
;; 取り込み設定ダイアログ
;;---------------------------------------------------------------------
(defun td3d_tmp_writeimpdcl( / str_path open_file )
  (setq str_path(strcat(td3d_tmp_tempdir)"td3dccboximport.dcl")
        open_file(open str_path "w"))
  (if(null open_file)nil
    (progn
      (write_strlist
       open_file
       (list
        "td3dimp :dialog"
        "{"
        (mix_strasc(list " label = \"" 12486 12531 12503 12524 12540 12488 12398 21462 12426 36796 12415 "\";"))
        " :text { key = \"info1\"; width = 46; }"
        " :text { key = \"info2\"; width = 46; }"
        " spacer;"
        " :boxed_column"
        " {"
        (mix_strasc(list "  label = \"" 25968 20516 12398 21336 20301 "\";"))
        "  :popup_list { key = \"unit\"; width = 30; fixed_width = true; }"
        " }"
        " :boxed_column"
        " {"
        (mix_strasc(list "  label = \"" 21516 21517 12398 12486 12531 12503 12524 12540 12488 12364 12354 12427 12392 12365 "\";"))
        "  :popup_list { key = \"dup\"; width = 30; fixed_width = true; }"
        " }"
        " ok_cancel;"
        "}"
        ))
      (close open_file)
      str_path)))

;;単位の選択肢に対応する倍率
(defun td3d_tmp_unitscale( num / )
  (cond((= num 1)0.001)((= num 2)0.01)(T 1.0)))

(defun td3d_tmp_dlg_import( / )
  (setq td3d_tmp_result(list(atoi(get_tile "unit"))(atoi(get_tile "dup"))))
  (done_dialog 1)
  (princ))

;;戻り値 : (単位番号 同名処理番号) / nil
(defun td3d_tmp_importdlg( num_all num_dup / str_path load_dcl int_ret )
  (cond
   ((null(setq str_path(td3d_tmp_writeimpdcl)))
    (td3d_tmp_alert (mix_strasc(list 12480 12452 12450 12525 12464 23450 32681 12501 12449 12452 12523 12434 26360 12365 20986 12379 12414 12379 12435 12391 12375 12383)))nil)
   (T
    (setq td3d_tmp_result nil
          load_dcl(load_dialog str_path))
    (cond
     ((< load_dcl 0)(td3d_tmp_alert (mix_strasc(list 12480 12452 12450 12525 12464 12434 35501 12415 36796 12417 12414 12379 12435 12391 12375 12383)))nil)
     ((null(new_dialog "td3dimp" load_dcl))
      (unload_dialog load_dcl)
      (td3d_tmp_alert (mix_strasc(list 12480 12452 12450 12525 12464 12434 38283 12369 12414 12379 12435 12391 12375 12383)))nil)
     (T
      (set_tile "info1" (mix_strasc(list 35501 12415 36796 12435 12384 20214 25968 " : " (itoa num_all) 20214)))
      (set_tile "info2" (mix_strasc(list 12358 12385 21516 21517 12391 26082 12395 12354 12427 12418 12398 " : " (itoa num_dup) 20214)))
      (start_list "unit")
      (mapcar 'add_list
              (list (mix_strasc(list 22259 38754 21336 20301 12398 12414 12414 " (" 215 "1)"))
                    (mix_strasc(list "mm " 12392 12375 12390 35501 12416 " (" 215 "0.001)"))
                    (mix_strasc(list "cm " 12392 12375 12390 35501 12416 " (" 215 "0.01)"))))
      (end_list)
      (set_tile "unit" "0")
      (start_list "dup")
      (mapcar 'add_list(list (mix_strasc(list 19978 26360 12365 12377 12427)) (mix_strasc(list 12473 12461 12483 12503 12377 12427))))
      (end_list)
      (set_tile "dup" "0")
      (action_tile "accept" "(td3d_tmp_dlg_import)")
      (setq int_ret(start_dialog))
      (unload_dialog load_dcl)
      (if(= int_ret 1)td3d_tmp_result nil))
     ))))

;;---------------------------------------------------------------------
;; 取り込み
;;---------------------------------------------------------------------
(defun td3d_tmp_import( / str_path open_file str_line ls_head ls_idx int_name
                          ls_data ls_now lst num_dup ret scale int_dup
                          num_add num_ow num_skip num_err ls_err str_tail )
  (cond
   ((null(setq str_path(getfiled (mix_strasc(list 12486 12531 12503 12524 12540 12488 12398 21462 12426 36796 12415))
                                 (getvar "DWGPREFIX")"csv" 0)))nil)
   ((null(setq open_file(td3d_tmp_openread str_path)))
    (td3d_tmp_alert (mix_strasc(list 12501 12449 12452 12523 12434 38283 12369 12414 12379 12435 12391 12375 12383)))nil)
   ((null(setq str_line(read-line open_file)))
    (close open_file)
    (td3d_tmp_alert (mix_strasc(list 31354 12398 12501 12449 12452 12523 12391 12377)))nil)
   (T
    ;;--- ヘッダー ---
    (setq ls_head(mapcar '(lambda(a)(strcase(td3d_tmp_trim a)))
                         (td3d_tmp_csvsplit str_line))
          int_name(td3d_tmp_pos "NAME" ls_head)
          ls_idx(mapcar '(lambda(lst)(td3d_tmp_pos(car lst)ls_head))
                        (td3d_tmp_paramdef)))
    (cond
     ((null int_name)
      (close open_file)
      (td3d_tmp_alert (mix_strasc(list "1" 34892 30446 12395 " NAME " 21015 12364 35211 12388 12363 12426 12414 12379 12435 "\\nNAME,WIDTH,HEIGHT," 8230 " " 12398 24418 24335 12395 12375 12390 12367 12384 12373 12356)))
      nil)
     (T
      ;;--- データ行 ---
      (setq ls_data(list))
      (while(setq str_line(read-line open_file))
        (setq lst(mapcar 'td3d_tmp_trim(td3d_tmp_csvsplit str_line)))
        (if(=(td3d_tmp_field int_name lst)"")T
          (setq ls_data
                (cons
                 (cons
                  (td3d_tmp_field int_name lst)
                  (mapcar '(lambda(i lst_def / str)
                             (setq str(td3d_tmp_field i lst))
                             (if(= str "")(caddr lst_def)(atof str)))
                          ls_idx(td3d_tmp_paramdef)))
                 ls_data))))
      (close open_file)
      (setq ls_data(reverse ls_data))

      (cond
       ((null ls_data)
        (td3d_tmp_alert (mix_strasc(list 21462 12426 36796 12417 12427 12487 12540 12479 34892 12364 12354 12426 12414 12379 12435)))nil)
       (T
        ;;--- 同名の件数を数える ---
        (setq ls_now(td3d_tmp_names) num_dup 0)
        (mapcar '(lambda(a)(if(td3d_tmp_pos(car a)ls_now)(setq num_dup(1+ num_dup))))
                ls_data)

        ;;--- 設定ダイアログ ---
        (if(null(setq ret(td3d_tmp_importdlg(length ls_data)num_dup)))
            nil
          (progn
            (setq scale(td3d_tmp_unitscale(car ret))
                  int_dup(cadr ret)
                  num_add 0 num_ow 0 num_skip 0 num_err 0 ls_err(list)
                  td3d_tmp_quiet T)

            (mapcar
             '(lambda(a / str_name ls_val str_err bool_exist)
                (setq str_name(car a)
                      ls_val(mapcar '(lambda(v)(* v scale))(cdr a))
                      bool_exist(if(td3d_tmp_pos str_name ls_now)T nil))
                (cond
                 ((and bool_exist(= int_dup 1))
                  (setq num_skip(1+ num_skip)))
                 ((setq str_err(td3d_tmp_validate ls_val))
                  (setq num_err(1+ num_err)
                        ls_err(cons(strcat str_name " : " str_err)ls_err)))
                 (T
                  (td3d_tmp_write str_name ls_val)
                  (td3d_tmp_makesampleblock str_name ls_val)
                  (if bool_exist(setq num_ow(1+ num_ow))(setq num_add(1+ num_add))))
                 ))
             ls_data)

            (setq ls_err(reverse ls_err) td3d_tmp_quiet nil)
            (princ (mix_strasc(list "\\n" 21462 12426 36796 12415 32080 26524 " " 26032 35215 (itoa num_add) " / " 19978 26360 12365 (itoa num_ow) " / " 12473 12461 12483 12503 (itoa num_skip) " / " 12456 12521 12540 (itoa num_err))))
            (mapcar '(lambda(str)(princ(strcat "\n  " str)))ls_err)
            (setq str_tail(if ls_err (mix_strasc(list "\\n\\n" 12456 12521 12540 12398 20869 23481 12399 12467 12510 12531 12489 12521 12452 12531 12434 35211 12390 12367 12384 12373 12356)) ""))
            (td3d_tmp_alert
             (mix_strasc(list 21462 12426 36796 12415 12414 12375 12383 "\\n\\n" 26032 35215 " : " (itoa num_add) 20214 "\\n" 19978 26360 12365 " : " (itoa num_ow) 20214 "\\n" 12473 12461 12483 12503 " : " (itoa num_skip) 20214 "\\n" 12456 12521 12540 " : " (itoa num_err) 20214 str_tail)))
            T))
        ))
      ))
    )))

;;=====================================================================
;; コマンド : CSV 書き出し / 取り込み
;;   (テンプレート画面のボタンからも同じ処理を呼ぶ)
;;=====================================================================


;; (defun c:td3dtmpexport( / )(td3d_tmp_export)(princ))
;; (defun c:td3dtmpimport( / )(td3d_tmp_import)(princ))

(princ)
