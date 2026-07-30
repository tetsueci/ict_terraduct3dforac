;;;;=====================================================================
;;;; manholetemplate.lsp
;;;;   terraduct3D 追加モジュール : マンホール(人孔)テンプレート
;;;;
;;;;   td3dmanhole    … テンプレートの作成/編集/削除の画面
;;;;                    terraduct3D.lsp のメニューから呼ばれる(コマンドではない)
;;;;                    画面を閉じるとき、選択中の名前を str_templatemanhole に返す
;;;;   c:td3dmhexport … テンプレートを CSV に書き出し
;;;;   c:td3dmhimport … CSV からテンプレートを取り込み
;;;;
;;;;   マンホールの配置そのものは terraduct3D.lsp が行う。
;;;;   このファイルはテンプレートの管理と、形状を組み立てる部品を提供する。
;;;;   terraduct3D.lsp から使うもの :
;;;;     td3dmanhole / td3d_mh_read / td3d_mh_fill / td3d_mh_paramdef
;;;;     td3d_mh_needdirp / td3d_mh_makeparts
;;;;     td3d_mh_copyparts / td3d_mh_deleteparts
;;;;     td3d_mh_mhhead / td3d_mh_covercolor
;;;;
;;;;   依存 : %library.lsp  (mix_strasc unit_vector cross_product
;;;;                          carxyz set_xda write_strlist code_loft_solid)
;;;;   ロード順 : %library.lsp のあと
;;;;   ※ ccboxtemplate.lsp とは独立。関数名の頭を td3d_mh_ にして衝突を避けている。
;;;;
;;;;   形状生成は vla- 関数が基本(command は使わない)。
;;;;   偏心斜壁だけは ActiveX に斜め円錐を作る手段が無いため、
;;;;   %library.lsp の code_loft_solid で多角形ロフトする。
;;;;
;;;;   形状の考え方
;;;;     上から 鉄蓋 → 上部(首部) → 斜壁 → 下部(躯体) → 底版 の順に積む。
;;;;     鉄蓋は最上段の内空にぴったりはまる柱で、上面が天端(地表面)に一致する。
;;;;     斜壁高さが 0 のときは斜壁を作らず、上部と下部が直結する
;;;;     (上部高さも 0 なら下部だけのストレート型になる)。
;;;;     斜壁偏心を 1 にすると、指定した向きの側の内面が躯体の内面と
;;;;     一直線(鉛直)になる偏心斜壁になる。偏心量は自動計算。
;;;;     壁は外形−内空のドーナツ状リージョンを押し出して作る。
;;;;=====================================================================

(vl-load-com)

(setq td3d_mh_dictname  "terraduct3d_manholetemplate" ;;テンプレート保存用の図面辞書
      td3d_mh_blockhead "TD3D-MHTMP$"                 ;;サンプルブロック名の接頭辞
      td3d_mh_mhhead    "MANHOLE$"                    ;;配置したマンホールのブロック名接頭辞
      td3d_mh_covercolor 251                          ;;鉄蓋の色
      td3d_mh_quiet      nil                          ;;T のとき部材リストを表示しない
      td3d_mh_arcdiv     48                           ;;偏心斜壁の円の分割数
      )

;;---------------------------------------------------------------------
;; 共通ユーティリティ / 辞書入出力 / ダイアログ / CSV
;;   ccboxtemplate.lsp の td3d_tmp_* と同じ内容。
;;   このファイル単独で動くように td3d_mh_ として持つ。
;;---------------------------------------------------------------------
;;---------------------------------------------------------------------
;; 共通ユーティリティ
;;---------------------------------------------------------------------
(defun td3d_mh_alert( str )
  (alert str)(princ))

;;数値を短く表示する(既定値の提示用)
(defun td3d_mh_numstr( a )
  (if a(rtos a 2 4)""))

;;アクティブ図面の辞書コレクション
(defun td3d_mh_dicts( / )
  (vla-get-Dictionaries(vla-get-ActiveDocument(vlax-get-acad-object))))

;;テンプレート辞書を得る。bool_create が T のとき無ければ作る
(defun td3d_mh_dict( bool_create / dicts dict )
  (setq dicts(td3d_mh_dicts))
  (if(vl-catch-all-error-p
      (setq dict(vl-catch-all-apply 'vla-Item(list dicts td3d_mh_dictname))))
      (setq dict(if bool_create(vla-Add dicts td3d_mh_dictname)nil)))
  dict)

;;安全な nth
;;  nth は環境によっては空リストや範囲外で
;;  「引数の型が違います: consp nil」を出すため、car/cdr だけで書く
(defun td3d_mh_nth( num lst / )
  (while(and lst(> num 0))(setq lst(cdr lst)num(1- num)))
  (car lst))


;;---------------------------------------------------------------------
;; 辞書 入出力
;;---------------------------------------------------------------------
;;テンプレート名の一覧
(defun td3d_mh_names( / dict ls_name str )
  (setq ls_name(list))
  (if(setq dict(td3d_mh_dict nil))
      (vlax-for
       xrec dict
       (if(vl-catch-all-error-p
           (setq str(vl-catch-all-apply 'vla-get-name(list xrec))))
           T
         (setq ls_name(cons str ls_name)))))
  (reverse ls_name))

;;読み込んだ値リストをパラメータ定義の個数に揃える
;;  項目を後から追加しても古いテンプレートが読めるように、
;;  欠けている項目と実数でない項目は既定値で埋める
;;  ls_val が nil でも必ず定義個数のリストを返す
(defun td3d_mh_fill( ls_val / )
  (mapcar '(lambda(lst / val)
             (setq val(car ls_val) ls_val(cdr ls_val))
             (cond
              ((=(type val)'REAL) val)
              ((=(type val)'INT)  (float val))
              (T (caddr lst))))
          (td3d_mh_paramdef)))

;;テンプレートを読む。無ければ nil
(defun td3d_mh_read( str_name / dict xrec array_type array_data ls_val )
  (if(setq dict(td3d_mh_dict nil))
      (if(vl-catch-all-error-p
          (setq xrec(vl-catch-all-apply 'vla-Item(list dict str_name))))
          nil
        (progn
          (vla-GetXRecordData xrec 'array_type 'array_data)
          (if array_data
              (progn
                (setq ls_val(mapcar 'vlax-variant-value
                                    (vlax-safearray->list array_data)))
                (td3d_mh_fill ls_val)))))))

;;テンプレートを書く
(defun td3d_mh_write( str_name ls_val / dict xrec num array_type array_data )
  (setq dict(td3d_mh_dict T) num(length ls_val))
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
(defun td3d_mh_erase( str_name / dict xrec blocks blk set_ent num vnam )
  (if(setq dict(td3d_mh_dict nil))
      (if(vl-catch-all-error-p
          (setq xrec(vl-catch-all-apply 'vla-Item(list dict str_name))))
          T
        (vla-delete xrec)))
  (setq blocks(vla-get-Blocks(vla-get-ActiveDocument(vlax-get-acad-object))))
  (if(vl-catch-all-error-p
      (setq blk(vl-catch-all-apply
                'vla-Item(list blocks(strcat td3d_mh_blockhead str_name)))))
      T
    (progn
      (if(setq set_ent(ssget "X"(list(cons 2(strcat td3d_mh_blockhead str_name)))))
          (progn
            (setq num(sslength set_ent))
            (while(>(setq num(1- num))-1)
              (setq vnam(vlax-ename->vla-object(ssname set_ent num)))
              (vla-delete vnam))))
      (vl-catch-all-apply 'vla-delete(list blk))))
  (princ))

;;部材リストを削除する
(defun td3d_mh_deleteparts( ls_part / )
  (mapcar '(lambda(a)(vl-catch-all-apply 'vla-delete(list(car a))))ls_part)
  (princ))

;;部材リストをブロック定義へコピーする
(defun td3d_mh_copyparts( ls_part blk / num )
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

;;DCL を書き出す一時フォルダ
(defun td3d_mh_tempdir( / )
  (if(and(boundp 'str_path_tempdirectory)str_path_tempdirectory)
      str_path_tempdirectory
    (getvar "TEMPPREFIX")))

;;入力欄へ値を流し込む(ls_val が nil でも既定値で埋まる)
(defun td3d_mh_settiles( ls_val )
  (mapcar '(lambda(lst val)
             (set_tile(car lst)
                      (if(td3d_mh_togglep lst)
                          (if(>= val 0.5)"1" "0")
                        (td3d_mh_numstr val))))
          (td3d_mh_paramdef)(td3d_mh_fill ls_val))
  (princ))

;;入力欄から値を読む
(defun td3d_mh_gettiles( / str )
  (mapcar '(lambda(lst / str)
             (setq str(get_tile(car lst)))
             (if(td3d_mh_togglep lst)
                 (if(= str "1")1.0 0.0)
               (atof str)))
          (td3d_mh_paramdef)))

;;一覧で選ばれたテンプレートを入力欄に読み込む
(defun td3d_mh_dlg_select( / num str_name ls_val )
  (setq num(atoi(get_tile "tmplist")))
  (if(setq str_name(td3d_mh_nth num td3d_mh_ls_name))
      (if(setq ls_val(td3d_mh_read str_name))
          (progn
            (set_tile "name" str_name)
            (td3d_mh_settiles ls_val))))
  (princ))

;;保存ボタン(実際の保存はダイアログを閉じてから行う)
(defun td3d_mh_dlg_save( / str_name ls_val str_err )
  (setq str_name(get_tile "name")
        ls_val(td3d_mh_gettiles))
  (cond
   ((= str_name "")
    (alert (mix_strasc(list 12486 12531 12503 12524 12540 12488 21517 12434 20837 21147 12375 12390 12367 12384 12373 12356))))
   ((setq str_err(td3d_mh_validate ls_val))
    (alert str_err))
   (T
    (setq td3d_mh_result(list str_name ls_val))
    (done_dialog 2)))
  (princ))

;;削除ボタン
(defun td3d_mh_dlg_delete( / num str_name )
  (setq num(atoi(get_tile "tmplist")))
  (if(setq str_name(td3d_mh_nth num td3d_mh_ls_name))
      (progn
        (setq td3d_mh_result(list str_name nil))
        (done_dialog 3))
    (alert (mix_strasc(list 21066 38500 12377 12427 12486 12531 12503 12524 12540 12488 12434 19968 35239 12363 12425 36984 12435 12391 12367 12384 12373 12356))))
  (princ))

;;文字列をダブルクォートで囲む(内部の " は "" にする)
(defun td3d_mh_qq( str / num str_one str_out )
  (setq str_out "" num 0)
  (while(< num(strlen str))
    (setq num(1+ num)
          str_one(substr str num 1)
          str_out(strcat str_out(if(= str_one "\"")"\"\"" str_one))))
  (strcat "\"" str_out "\""))

;;CSV 1 行をフィールドに分解(ダブルクォート対応)
(defun td3d_mh_csvsplit( str / len num str_one bool_q str_cur ls_out )
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
(defun td3d_mh_trim( str / )
  (while(and(>(strlen str)0)
            (member(substr str 1 1)(list " " "\t"(chr 65279))))
    (setq str(substr str 2)))
  (while(and(>(strlen str)0)
            (member(substr str(strlen str)1)(list " " "\t" "\r" "\n")))
    (setq str(substr str 1(1-(strlen str)))))
  str)

;;リスト中の位置(見つからなければ nil)
(defun td3d_mh_pos( str lst / num )
  (setq num 0)
  (while(and lst(/= str(car lst)))(setq lst(cdr lst)num(1+ num)))
  (if lst num nil))

;;num 番目のフィールド(無ければ "")
(defun td3d_mh_field( num lst / str )
  (if(null num)""
    (progn(setq str(td3d_mh_nth num lst))(if str str ""))))

;;ファイルを開く(アプリ本体と同じ流儀 : AutoCAD は UTF-8+BOM、他は既定)
(defun td3d_mh_openwrite( str_path / f )
  (if(and(boundp 'command_for_alter)command_for_alter)
      (open str_path "w")
    (progn
      (if(vl-catch-all-error-p
          (setq f(vl-catch-all-apply 'open(list str_path "w" "utf8"))))
          (setq f(open str_path "w"))
        (if f(write-char 65279 f)))
      f)))

(defun td3d_mh_openread( str_path / f )
  (if(and(boundp 'command_for_alter)command_for_alter)
      (open str_path "r")
    (if(vl-catch-all-error-p
        (setq f(vl-catch-all-apply 'open(list str_path "r" "utf8"))))
        (open str_path "r")
      f)))

;;単位の選択肢に対応する倍率
(defun td3d_mh_unitscale( num / )
  (cond((= num 1)0.001)((= num 2)0.01)(T 1.0)))

(defun td3d_mh_dlg_import( / )
  (setq td3d_mh_result(list(atoi(get_tile "unit"))(atoi(get_tile "dup"))))
  (done_dialog 1)
  (princ))


;;---------------------------------------------------------------------
;; パラメータ定義
;;   (キー 表示名 既定値 必須か)
;;   XRecord にはこの順序で 1040(実数) を並べて保存する
;;   ※追加するときは必ず末尾に足すこと
;;---------------------------------------------------------------------
(defun td3d_mh_paramdef( / )
  (list
   (list "SHAPE"  (mix_strasc(list 24418 29366 " (0=" 20870 24418 " / 1=" 35282 24418 ")"))   0.000 nil)
   (list "TOPW"   (mix_strasc(list 19978 37096 " " 20869 31354 24133 "(" 20870 12399 20869 24452 ")"))    0.600 T  )
   (list "TOPL"   (mix_strasc(list 19978 37096 " " 20869 31354 38263 12373 "(" 20870 12399 28961 35222 ")"))  0.600 nil)
   (list "TOPT"   (mix_strasc(list 19978 37096 " " 22721 21402))                0.075 T  )
   (list "TOPH"   (mix_strasc(list 19978 37096 "(" 39318 37096 ")" 39640 12373 " (0" 12391 12394 12375 ")")) 0.300 nil)
   (list "TAPERH" (mix_strasc(list 26012 22721 39640 12373 " (0" 12391 12394 12375 ")"))       0.450 nil)
   (list "BOTW"   (mix_strasc(list 19979 37096 " " 20869 31354 24133 "(" 20870 12399 20869 24452 ")"))    0.900 T  )
   (list "BOTL"   (mix_strasc(list 19979 37096 " " 20869 31354 38263 12373 "(" 20870 12399 28961 35222 ")"))  0.900 nil)
   (list "BOTT"   (mix_strasc(list 19979 37096 " " 22721 21402))                0.075 T  )
   (list "BOTH"   (mix_strasc(list 19979 37096 "(" 36527 20307 ")" 39640 12373))           1.000 nil)
   (list "SLABT"  (mix_strasc(list 24213 29256 21402 " (0" 12391 12394 12375 ")"))         0.150 nil)
   (list "COVERD" (mix_strasc(list 37444 33995 " " 21628 12403 24452 " (" 25968 37327 29992 12539 24418 29366 12395 12399 20351 12431 12394 12356 ")")) 0.600 nil)
   (list "COVERT" (mix_strasc(list 37444 33995 " " 21402 12373 " (0" 12391 12394 12375 ")"))      0.060 nil)
   ;;5番目に "TOGGLE" を書くと、ダイアログではチェックボックスになる
   (list "TAPERECC" (mix_strasc(list 26012 22721 12434 20559 24515 12395 12377 12427 " (" 29255 20596 12364 37467 30452 ")")) 0.000 nil "TOGGLE")
   ))

;;ダイアログでトグル(チェックボックス)にする項目か
;;  パラメータ定義の5番目が "TOGGLE" のものが該当する
(defun td3d_mh_togglep( lst / )
  (= (td3d_mh_nth 4 lst) "TOGGLE"))

;;円形かどうか
(defun td3d_mh_roundp( ls_val / )
  (< (abs(td3d_mh_nth 0 ls_val)) 0.5))

;;向きの指定が必要か
;;  角形、または上下いずれかで 幅≠長さ のとき非対称とみなす
(defun td3d_mh_needdirp( ls_val / )
  (setq ls_val(td3d_mh_fill ls_val))
  (cond
   ;;偏心斜壁は軸対称でなくなるので向きが要る
   ((and(>(td3d_mh_nth 5 ls_val)0.)(td3d_mh_eccp ls_val)) T)
   ((null(td3d_mh_roundp ls_val)) T)
   ((>(abs(-(td3d_mh_nth 1 ls_val)(td3d_mh_nth 2 ls_val)))1e-8) T)
   ((>(abs(-(td3d_mh_nth 6 ls_val)(td3d_mh_nth 7 ls_val)))1e-8) T)
   (T nil)))

;;斜壁が偏心かどうか
(defun td3d_mh_eccp( ls_val / )
  (>=(td3d_mh_nth 13(td3d_mh_fill ls_val))0.5))

;;斜壁の偏心量
;;  片側の内面が躯体の内面と一直線(鉛直)になる量
;;    偏心量 = (下部内空幅 - 上部内空幅) / 2
;;  壁厚が上下で同じなら外面も同時に一直線になる
(defun td3d_mh_eccoffset( ls_val / )
  (setq ls_val(td3d_mh_fill ls_val))
  (if(td3d_mh_eccp ls_val)
      (* 0.5(-(td3d_mh_nth 6 ls_val)(td3d_mh_nth 1 ls_val)))
    0.))

;;テンプレートの合計高さ
(defun td3d_mh_totalheight( ls_val / )
  (setq ls_val(td3d_mh_fill ls_val))
  (+(td3d_mh_nth 4 ls_val)(td3d_mh_nth 5 ls_val)
    (td3d_mh_nth 9 ls_val)(td3d_mh_nth 10 ls_val)))


;;---------------------------------------------------------------------
;; パラメータの妥当性検査
;;---------------------------------------------------------------------
(defun td3d_mh_validate( ls_val / shape tw tl tt th tph bw bl bt bh st cd cvt ecc )
  (mapcar 'set '(shape tw tl tt th tph bw bl bt bh st cd cvt ecc)(td3d_mh_fill ls_val))
  (cond
   ((or(<= bw 0.)(<= bl 0.))
    (mix_strasc(list 19979 37096 12398 20869 31354 23544 27861 12399 27491 12398 20516 12434 20837 21147 12375 12390 12367 12384 12373 12356)))
   ((<= bt 0.)
    (mix_strasc(list 19979 37096 12398 22721 21402 12399 27491 12398 20516 12434 20837 21147 12375 12390 12367 12384 12373 12356)))
   ((and(> th 0.)(or(<= tw 0.)(<= tl 0.)))
    (mix_strasc(list 19978 37096 12398 39640 12373 12364 " 0 " 12391 12394 12356 12392 12365 12399 19978 37096 12398 20869 31354 23544 27861 12364 24517 35201 12391 12377)))
   ((and(> th 0.)(<= tt 0.))
    (mix_strasc(list 19978 37096 12398 39640 12373 12364 " 0 " 12391 12394 12356 12392 12365 12399 19978 37096 12398 22721 21402 12364 24517 35201 12391 12377)))
   ((and(> tph 0.)(or(<= tw 0.)(<= tl 0.)))
    (mix_strasc(list 26012 22721 12364 12354 12427 12392 12365 12399 19978 37096 12398 20869 31354 23544 27861 12364 24517 35201 12391 12377)))
   ((or(< th 0.)(< tph 0.)(< bh 0.)(< st 0.))
    (mix_strasc(list 39640 12373 12392 21402 12373 12395 36000 12398 20516 12399 25351 23450 12391 12365 12414 12379 12435)))
   ((< (td3d_mh_totalheight(td3d_mh_fill ls_val)) 1e-6)
    (mix_strasc(list 39640 12373 12398 21512 35336 12364 " 0 " 12391 12377)))
   ((and(> tph 0.)(or(> tw bw)(> tl bl)))
    (mix_strasc(list 26012 22721 12399 19978 37096 12364 19979 37096 12424 12426 23567 12373 12356 24517 35201 12364 12354 12426 12414 12377)))
   ;;角形の斜壁は 1 つのテーパー角で押し出すため、
   ;;幅と長さの縮み量が同じでないと表現できない
   ((and(> tph 0.)(>= shape 0.5)
        (>(abs(-(- bw tw)(- bl tl)))1e-8))
    (mix_strasc(list 35282 24418 12398 26012 22721 12399 " " 24133 12398 32302 12415 37327 " " 12392 " " 38263 12373 12398 32302 12415 37327 " " 12434 21516 12376 12395 12375 12390 12367 12384 12373 12356)))
   ((< cvt 0.)
    (mix_strasc(list 37444 33995 12398 21402 12373 12395 36000 12398 20516 12399 25351 23450 12391 12365 12414 12379 12435)))
   ;;蓋は最上段の内空にぴったりはめるので、その段の高さより厚いとはみ出す
   ((> cvt(if(> th 0.)th(if(> tph 0.)tph bh)))
    (mix_strasc(list 37444 33995 12398 21402 12373 12364 26368 19978 27573 12398 39640 12373 12424 12426 22823 12365 12367 12394 12387 12390 12356 12414 12377)))
   ;;偏心斜壁は円形のみ対応
   ((and(> tph 0.)(>= ecc 0.5)(>= shape 0.5))
    (mix_strasc(list 20559 24515 26012 22721 12399 20870 24418 12398 12415 23550 24540 12375 12390 12356 12414 12377)))
   (T nil)))

;;---------------------------------------------------------------------
;; ソリッド生成 (すべて vla- 関数。command は使わない)
;;
;;   考え方
;;     外形と内空の閉じた境界曲線をそれぞれリージョンにし、
;;     subtract してドーナツ状のリージョンを作り、それを押し出す。
;;     径が変わる段(斜壁)は、外形と内空をそれぞれ円錐台として
;;     テーパー角付きで押し出し、subtract して筒にする。
;;     (ドーナツ状リージョンにテーパーを掛けると内周が逆に広がるため使えない)
;;
;;   ※以前 vla-Boolean が効かなかったのは entmake で ACIS コードを直接
;;     組み立てたソリッドだったため。ここでは API が作った本物のソリッド
;;     なので、%library.lsp の bendpipe_sld と同じく正常に働く。
;;---------------------------------------------------------------------
(defun td3d_mh_space( / )
  (vla-get-ModelSpace(vla-get-ActiveDocument(vlax-get-acad-object))))

;;VLAオブジェクトのリストを safearray にする
(defun td3d_mh_objarray( ls_obj / sa )
  (setq sa(vlax-make-safearray vlax-vbObject(cons 0(1-(length ls_obj)))))
  (vlax-safearray-fill sa ls_obj)
  sa)

;;断面座標(u,v)を世界座標へ
(defun td3d_mh_toworld( p_base vecu vecv uu vv zz / )
  (mapcar '(lambda(a u v)(+ a(* uu u)(* vv v)))
          (mapcar '+ p_base(list 0. 0. zz))
          vecu vecv))

;;---------------------------------------------------------------------
;; 閉じた境界曲線を作る
;;   int_shape 0=円形(vla-AddCircle) / 1=角形(閉じた軽量ポリライン)
;;   p_center  中心(平面) / zz 標高 / w l 内空寸法 / offset 外側へのオフセット
;;   vecu      角形のときの向き
;;   戻り値 : VLAオブジェクト / 作れないとき nil
;;---------------------------------------------------------------------
(defun td3d_mh_curve( int_shape p_center zz w l offset vecu
                      / rr uu vv vecv space obj ls_xy arr )
  (setq space(td3d_mh_space))
  (if(< (abs int_shape) 0.5)
      ;;--- 円形 : 真円をそのまま使う(多角形近似ではない) ---
      (progn
        (setq rr(+(* 0.5 w)offset))
        (if(<= rr 1e-9)nil
          (vla-AddCircle space(vlax-3d-point(carxyz p_center zz))rr)))
    ;;--- 角形 : 閉じた軽量ポリライン ---
    (progn
      (setq uu(+(* 0.5 w)offset) vv(+(* 0.5 l)offset))
      (if(or(<= uu 1e-9)(<= vv 1e-9))nil
        (progn
          (setq vecv(unit_vector(cross_product(list 0. 0. 1.)vecu))
                ls_xy(apply 'append
                            (mapcar
                             '(lambda(p / q)
                                (setq q(td3d_mh_toworld p_center vecu vecv
                                                        (car p)(cadr p)0.))
                                (list(car q)(cadr q)))
                             (list(list uu(- vv))(list uu vv)
                                  (list(- uu)vv)(list(- uu)(- vv)))))
                arr(vlax-make-safearray vlax-vbDouble(cons 0(1-(length ls_xy)))))
          (vlax-safearray-fill arr ls_xy)
          (setq obj(vla-AddLightWeightPolyline space arr))
          (vla-put-Closed obj :vlax-true)
          (vla-put-Elevation obj zz)
          obj)))))

;;曲線1本からリージョンを作る(元の曲線は明示的に消す)
(defun td3d_mh_region( obj / reg )
  (if(null obj)nil
    (progn
      (setq reg(vl-catch-all-apply
                'vlax-safearray->list
                (list(vlax-variant-value
                      (vla-AddRegion(td3d_mh_space)
                                    (td3d_mh_objarray(list obj)))))))
      ;;AddRegion は元の曲線を消さないため明示的に削除
      (vl-catch-all-apply 'vla-delete(list obj))
      (if(or(vl-catch-all-error-p reg)(null reg))nil(car reg)))))

;;外形リージョン − 内空リージョン = ドーナツ状のリージョン
(defun td3d_mh_ringregion( int_shape p_center zz w l thk vecu / reg_out reg_in )
  (setq reg_out(td3d_mh_region(td3d_mh_curve int_shape p_center zz w l thk vecu))
        reg_in (td3d_mh_region(td3d_mh_curve int_shape p_center zz w l 0.  vecu)))
  (cond
   ((null reg_out)
    (if reg_in(vl-catch-all-apply 'vla-delete(list reg_in)))nil)
   ((null reg_in) reg_out)
   ((vl-catch-all-error-p
     (vl-catch-all-apply 'vla-Boolean(list reg_out 2 reg_in)));;2=acSubtraction
    (vl-catch-all-apply 'vla-delete(list reg_out))
    (vl-catch-all-apply 'vla-delete(list reg_in))
    nil)
   (T reg_out)))

;;リージョンを押し出してソリッドにする(リージョンは消す)
(defun td3d_mh_extrude( reg height ang str_layer / sol )
  (if(null reg)nil
    (progn
      (setq sol(vl-catch-all-apply
                'vla-AddExtrudedSolid
                (list(td3d_mh_space)reg height ang)))
      (vl-catch-all-apply 'vla-delete(list reg))
      (if(vl-catch-all-error-p sol)nil
        (progn(vl-catch-all-apply 'vla-put-Layer(list sol str_layer))sol)))))

;;---------------------------------------------------------------------
;; 壁(筒)を1つ作る
;;   z0 height        : 下端の標高と高さ
;;   w0 l0 t0         : 下端の 内空幅・内空長さ・壁厚
;;   w1 l1 t1         : 上端の 同上
;;   戻り値 : VLAオブジェクト(3DSOLID) / nil
;;---------------------------------------------------------------------
(defun td3d_mh_wall( int_shape p_center z0 height w0 l0 t0 w1 l1 t1 vecu str_layer
                     / tn_out tn_in eps sol sol_in reg )
  (cond
   ((<= height 1e-9) nil)

   ;;--- 一定断面 : ドーナツ状リージョンを1回押し出すだけ ---
   ;;    (径が変わらないので偏心量は無視される)
   ((and(<(abs(- w0 w1))1e-9)(<(abs(- l0 l1))1e-9)(<(abs(- t0 t1))1e-9))
    (td3d_mh_extrude(td3d_mh_ringregion int_shape p_center z0 w0 l0 t0 vecu)
                    height 0. str_layer))

   ;;--- 径が変わる(同心) : 外形ソリッド − 内空ソリッド ---
   ;;
   ;;  ドーナツ状リージョンにテーパー角を付けて押し出す方法は使えない。
   ;;  テーパーは「材料の内側へ」輪郭をオフセットするため、外周は縮むが
   ;;  内周(穴)は逆に広がってしまい、壁厚が上へ行くほど薄くなって
   ;;  途中で自己交差し、ソリッドの作成そのものが失敗する。
   ;;  そこで穴のない外形と内空をそれぞれ円錐台として押し出し、差し引く。
   (T
    (setq tn_out(/(-(+(* 0.5 w0)t0)(+(* 0.5 w1)t1))height)
          tn_in (/(-(* 0.5 w0)(* 0.5 w1))height)
          eps(* 0.002(max height 1.)))
    ;;上端で断面が消えてしまう指定は作れない
    (if(or(<=(+(* 0.5 w1)t1)1e-9)(<=(* 0.5 w1)1e-9))
        (setq sol nil)
      (setq sol(td3d_mh_extrude
                (td3d_mh_region(td3d_mh_curve int_shape p_center z0 w0 l0 t0 vecu))
                height(atan tn_out)str_layer)))
    (if(null sol)nil
      (progn
        ;;内空は上下に少しはみ出させて確実に貫通させる
        (setq reg(td3d_mh_region
                  (td3d_mh_curve int_shape p_center(- z0 eps)
                                 (+ w0(* 2. eps tn_in))(+ l0(* 2. eps tn_in))
                                 0. vecu))
              sol_in(td3d_mh_extrude reg(+ height(* 2. eps))(atan tn_in)str_layer))
        (cond
         ((null sol_in)(vl-catch-all-apply 'vla-delete(list sol))nil)
         ((vl-catch-all-error-p
           (vl-catch-all-apply 'vla-Boolean(list sol 2 sol_in)))
          (vl-catch-all-apply 'vla-delete(list sol_in))
          (vl-catch-all-apply 'vla-delete(list sol))
          nil)
         (T sol)))))
   ))

;;---------------------------------------------------------------------
;; 偏心(斜め)の筒を、リング状に分割したロフトを並べて作る
;;
;;   なぜ分割するか
;;     ActiveX には斜め円錐を直接作る手段が無い。
;;       - vla-AddExtrudedSolid のテーパー角は輪郭を一様に縮めるだけで
;;         中心をずらせない
;;       - vla-TransformBy によるせん断変換は、CAD がソリッドの
;;         非直交変換を受け付けないため失敗する
;;     そこで code_loft_solid でロフトすることになるが、こちらは entmake で
;;     ACIS コードを組み立てるため vla-Boolean が効かない。
;;     外形から内空を差し引く方法が使えず、中実になってしまう。
;;     そのため最初から穴が空くように、内空と外形の間を扇形に分けて
;;     一区画ずつ作る。差し引きが不要になる。
;;
;;   p0 z0 h        : 下端の中心(平面)・標高・高さ
;;   rin0 rout0     : 下端の 内空半径 / 外形半径
;;   rin1 rout1     : 上端の 内空半径 / 外形半径
;;   off            : 上端が vecu 方向へずれる量
;;   戻り値 : ((VLAオブジェクト . 部材名) …)  分割数だけ並ぶ
;;
;;   多角形は内接(頂点が呼び径の位置)。分割数は td3d_mh_arcdiv。
;;   各区画は上下面が水平、側面も平面になる(上端は下端の相似形を
;;   平行移動したものなので、側面はねじれない)。
;;---------------------------------------------------------------------
(defun td3d_mh_eccring( p0 z0 h rin0 rout0 rin1 rout1 off vecu str_layer
                        / num ii jj fn_p ls_out ls_ena vna )
  ;;区画の頂点 : tt(0=下端/1=上端)、kk(分割番号)、rr(半径)
  (setq fn_p
        (lambda(tt kk rr / cx ang)
          (setq cx(mapcar '(lambda(a u)(+ a(* tt off u)))p0 vecu)
                ang(/(* 2. pi kk)td3d_mh_arcdiv))
          (list(+(car cx)(* rr(cos ang)))
               (+(cadr cx)(* rr(sin ang)))
               (+ z0(* tt h)))))

  (setq num td3d_mh_arcdiv ii -1 ls_out(list))
  (while(<(setq ii(1+ ii))num)
    (setq jj(rem(1+ ii)num)
          ;;反時計回りになる順序 : 内i → 外i → 外j → 内j
          ls_ena(vl-catch-all-apply
                 'code_loft_solid
                 (list
                  (list
                   (list(fn_p 0. ii rin0)(fn_p 0. ii rout0)
                        (fn_p 0. jj rout0)(fn_p 0. jj rin0))
                   (list(fn_p 1. ii rin1)(fn_p 1. ii rout1)
                        (fn_p 1. jj rout1)(fn_p 1. jj rin1)))
                  nil nil)))
    (if(or(vl-catch-all-error-p ls_ena)(null ls_ena)(null(car ls_ena)))T
      (progn
        (setq vna(vlax-ename->vla-object(car ls_ena)))
        (vl-catch-all-apply 'vla-put-Layer(list vna str_layer))
        (setq ls_out(cons(cons vna "MHTAPER")ls_out)))))
  (reverse ls_out))

;;中実の板(底版)
(defun td3d_mh_slabsolid( int_shape p_center z0 height w l thk vecu str_layer / )
  (if(<= height 1e-9)nil
    (td3d_mh_extrude(td3d_mh_region(td3d_mh_curve int_shape p_center z0 w l thk vecu))
                    height 0. str_layer)))

;;---------------------------------------------------------------------
;; 鉄蓋(ぴったりサイズの円柱/角柱)
;;   最上段の内空と同じ断面で、上面が天端(地表面)に一致する
;;---------------------------------------------------------------------
(defun td3d_mh_coversolid( int_shape p_center ztop thk w l vecu str_layer / )
  (if(or(<= thk 1e-9)(<= w 1e-9))nil
    (td3d_mh_extrude
     (td3d_mh_region(td3d_mh_curve int_shape p_center(- ztop thk)w l 0. vecu))
     thk 0. str_layer)))

;;---------------------------------------------------------------------
;; マンホール本体を部材ごとに作る
;;   ls_val     : パラメータ値リスト
;;   p_top      : 天端(地表面)の中心点(z を含む)
;;   vecu       : 平面の向き(角形のときだけ意味を持つ。nil なら X 方向)
;;   height_all : 総高さ(nil ならテンプレートの合計をそのまま使う)
;;   戻り値 : ((VLAオブジェクト . 部材名) …) / 失敗時 nil
;;
;;   天端から下へ 上部 → 斜壁 → 下部 → 底版 の順に積む。
;;   総高さを指定したときは下部(躯体)の高さで差を吸収する。
;;---------------------------------------------------------------------
(defun td3d_mh_makeparts( ls_val p_top vecu height_all str_layer
                          / shape tw tl tt th tph bw bl bt bh st cd cvt ecc
                            p_center p_upper z ztop ls_out sol cw cl off_ecc )
  (setq ls_val(td3d_mh_fill ls_val))
  (mapcar 'set '(shape tw tl tt th tph bw bl bt bh st cd cvt ecc)ls_val)
  ;;偏心量は片側の内面が鉛直になる量。指定した向きの側が鉛直になる
  (setq off_ecc(if(> tph 0.)(td3d_mh_eccoffset ls_val)0.))

  (if(null vecu)(setq vecu(list 1. 0. 0.)))
  (setq vecu(unit_vector(carxyz vecu 0.))
        p_center(carxyz p_top 0.)
        ztop(caddr p_top)
        z ztop
        ls_out(list)
        ;;斜壁の上端は偏心量だけずれるので、その上に載る首部と蓋もずらす
        p_upper(mapcar '(lambda(a u)(+ a(* off_ecc u)))p_center vecu))

  ;;総高さの指定があれば下部の高さで調整する
  (if height_all(setq bh(- height_all th tph st)))

  (cond
   ((< bh 0.) nil)
   ((<(+ th tph bh st)1e-6) nil)
   (T
    ;;蓋 : 最上段の内空にぴったりはめ、上面を天端に合わせる
    ;;     壁のリングの穴にちょうど収まるので、壁とは干渉せず隙間もできない。
    ;;     鉄蓋呼び径(COVERD)は数量・表示用で、形状には使わない。
    (if(or(> th 0.)(> tph 0.))(setq cw tw cl tl)(setq cw bw cl bl))
    (if(> cvt 0.)
        (progn
          (setq sol(td3d_mh_coversolid shape p_upper ztop cvt cw cl vecu str_layer))
          (if sol(setq ls_out(cons(cons sol "MHCOVER")ls_out)))))

    ;;上部(首部)
    (if(> th 0.)
        (progn
          (setq sol(td3d_mh_wall shape p_upper(- z th)th
                                 tw tl tt tw tl tt vecu str_layer))
          (if sol(setq ls_out(cons(cons sol "MHNECK")ls_out)))
          (setq z(- z th))))

    ;;斜壁(下端=下部断面 / 上端=上部断面)
    ;;  同心 : 円錐台どうしの差し引き(真円・1ソリッド)
    ;;  偏心 : リング状に分割したロフトを並べる(最初から穴が空く)
    (if(> tph 0.)
        (progn
          (if(>(abs off_ecc)1e-9)
              (setq ls_out(append(reverse(td3d_mh_eccring
                                          p_center(- z tph)tph
                                          (* 0.5 bw)(+(* 0.5 bw)bt)
                                          (* 0.5 tw)(+(* 0.5 tw)tt)
                                          off_ecc vecu str_layer))
                                 ls_out))
            (progn
              (setq sol(td3d_mh_wall shape p_center(- z tph)tph
                                     bw bl bt tw tl tt vecu str_layer))
              (if sol(setq ls_out(cons(cons sol "MHTAPER")ls_out)))))
          (setq z(- z tph))))

    ;;下部(躯体)
    (if(> bh 0.)
        (progn
          (setq sol(td3d_mh_wall shape p_center(- z bh)bh
                                 bw bl bt bw bl bt vecu str_layer))
          (if sol(setq ls_out(cons(cons sol "MHBODY")ls_out)))
          (setq z(- z bh))))

    ;;底版
    (if(> st 0.)
        (progn
          (setq sol(td3d_mh_slabsolid shape p_center(- z st)st
                                      bw bl bt vecu str_layer))
          (if sol(setq ls_out(cons(cons sol "MHSLAB")ls_out)))
          (setq z(- z st))))

    (setq ls_out(reverse ls_out))
    (if(null ls_out)nil
      (progn
        (if td3d_mh_quiet T
          (progn
            (princ (mix_strasc(list "\n" 20316 25104 12375 12383 37096 26448 " (" (itoa(length ls_out)) ") : ")))
            (mapcar '(lambda(a)(princ(strcat(cdr a)" ")))ls_out)
            (princ (mix_strasc(list "\n  " 32207 39640 12373 " " (td3d_mh_numstr(- ztop z)))))))
        ls_out)))))


;;---------------------------------------------------------------------
;; IFC 出力用の形状行を作る
;;
;;   makeparts と同じ寸法・同じ並びで、部材 1 つにつき 1 行ぶんの
;;   ((列名 . 値) …) を返す。列名は terraduct3D.lsp の ls_ifcparameter に合わせる。
;;   色は部材ごとに違う(鉄蓋だけ別色)ので、ここでは付けずに呼び側で足す。
;;   "part" は列にはならないが、呼び側が色を引くために入れてある。
;;
;;   型の使い分け
;;     一定断面の筒   円形 → CYLINDER(外円柱 − 内円柱)
;;                    角形 → RECT 4 枚(隅で重ならないよう長短に分ける)
;;     中実の板・鉄蓋 円形 → CIRCLE / 角形 → RECT
;;     斜壁           CSV に該当する型が無いので FACE(IfcFacetedBrep)。
;;                    区画の分け方は makeparts の eccring と同じ
;;                    (円形は td3d_mh_arcdiv 分割の内接多角形。
;;                     同心の斜壁は CAD 側では真円の円錐台なので、
;;                     IFC では多角形近似になる)
;;---------------------------------------------------------------------
(defun td3d_mh_ifcrows( ls_val p_top vecu height_all
                        / shape tw tl tt th tph bw bl bt bh st cd cvt ecc
                          vecv ang p_center p_upper ztop z cw cl off_ecc
                          ls_out fn_rect fn_disc fn_ring fn_taper )
  (setq ls_val(td3d_mh_fill ls_val))
  (mapcar 'set '(shape tw tl tt th tph bw bl bt bh st cd cvt ecc)ls_val)
  (setq off_ecc(if(> tph 0.)(td3d_mh_eccoffset ls_val)0.))

  (if(null vecu)(setq vecu(list 1. 0. 0.)))
  (setq vecu(unit_vector(carxyz vecu 0.))
        vecv(unit_vector(cross_product(list 0. 0. 1.)vecu))
        ;;ifc_exchange.py は rotate の符号を逆に取るため -1 を掛ける
        ang(- (atan(cadr vecu)(car vecu)))
        p_center(carxyz p_top 0.)
        ztop(caddr p_top)
        z ztop
        ls_out(list)
        p_upper(mapcar '(lambda(a u)(+ a(* off_ecc u)))p_center vecu))

  (if height_all(setq bh(- height_all th tph st)))

  ;;矩形の角柱を 1 つ。du dv は中心から vecu vecv 方向へのずれ
  (setq fn_rect
        (lambda(p z0 height dx dy du dv str_part / q)
          (setq q(mapcar '(lambda(a u v)(+ a(* du u)(* dv v)))p vecu vecv)
                ls_out
                (cons(list(cons "type" "RECT")(cons "part" str_part)
                          (cons "x"(car q))(cons "y"(cadr q))(cons "z" z0)
                          (cons "ex" 0.)(cons "ey" 0.)(cons "ez" 1.)
                          (cons "rotate" ang)
                          (cons "dx" dx)(cons "dy" dy)
                          (cons "height" height))
                     ls_out))))

  ;;中実の柱(鉄蓋・底版)。w l は内空寸法、offset は外側への足し分
  (setq fn_disc
        (lambda(p z0 height w l offset str_part / )
          (if(or(<= height 1e-9)(<=(+(* 0.5 w)offset)1e-9))nil
            (if(td3d_mh_roundp ls_val)
                (setq ls_out
                      (cons(list(cons "type" "CIRCLE")(cons "part" str_part)
                                (cons "x"(car p))(cons "y"(cadr p))(cons "z" z0)
                                (cons "ex" 0.)(cons "ey" 0.)(cons "ez" 1.)
                                (cons "rotate" ang)
                                (cons "radius"(+(* 0.5 w)offset))
                                (cons "height" height))
                           ls_out))
              (fn_rect p z0 height(+ w(* 2. offset))(+ l(* 2. offset))
                       0. 0. str_part)))))

  ;;一定断面の筒。w l は内空寸法、thk は壁厚
  (setq fn_ring
        (lambda(p z0 height w l thk str_part / )
          (cond
           ((or(<= height 1e-9)(<= thk 1e-9)(<=(* 0.5 w)1e-9)) nil)
           ((td3d_mh_roundp ls_val)
            (setq ls_out
                  (cons(list(cons "type" "CYLINDER")(cons "part" str_part)
                            (cons "x"(car p))(cons "y"(cadr p))(cons "z" z0)
                            (cons "ex" 0.)(cons "ey" 0.)(cons "ez" 1.)
                            (cons "rotate" ang)
                            (cons "radius"(+(* 0.5 w)thk))
                            (cons "thickness" thk)
                            (cons "height" height))
                       ls_out)))
           (T
            ;;vecu 側の 2 枚は外形の全長、vecv 側の 2 枚は内空の幅にして隅の重なりを避ける
            (fn_rect p z0 height thk(+ l(* 2. thk))
                     (+(* 0.5 w)(* 0.5 thk)) 0. str_part)
            (fn_rect p z0 height thk(+ l(* 2. thk))
                     (-(+(* 0.5 w)(* 0.5 thk))) 0. str_part)
            (fn_rect p z0 height w thk 0.(+(* 0.5 l)(* 0.5 thk)) str_part)
            (fn_rect p z0 height w thk 0.(-(+(* 0.5 l)(* 0.5 thk))) str_part)))))

  ;;斜壁。下端=下部断面 / 上端=上部断面。区画ごとに FACE を1行出す
  (setq fn_taper
        (lambda(z0 height / num ii jj a0 a1 b0 b1)
          (if(<= height 1e-9)nil
            (progn
              (setq num(if(td3d_mh_roundp ls_val)td3d_mh_arcdiv 4)
                    a0(td3d_mh_ringloop shape p_center z0 bw bl 0.  vecu vecv num)
                    a1(td3d_mh_ringloop shape p_center z0 bw bl bt  vecu vecv num)
                    b0(td3d_mh_ringloop shape p_upper(+ z0 height)tw tl 0. vecu vecv num)
                    b1(td3d_mh_ringloop shape p_upper(+ z0 height)tw tl tt vecu vecv num)
                    ii -1)
              (while(<(setq ii(1+ ii))num)
                (setq jj(rem(1+ ii)num)
                      ls_out
                      (cons(list(cons "type" "FACE")(cons "part" "MHTAPER")
                                (cons "face"
                                      (td3d_mh_prismfaces
                                       ;;下面 : 内i → 外i → 外j → 内j (反時計回り)
                                       (list(td3d_mh_nth ii a0)(td3d_mh_nth ii a1)
                                            (td3d_mh_nth jj a1)(td3d_mh_nth jj a0))
                                       (list(td3d_mh_nth ii b0)(td3d_mh_nth ii b1)
                                            (td3d_mh_nth jj b1)(td3d_mh_nth jj b0)))))
                           ls_out)))))))

  (cond
   ((< bh 0.) nil)
   ((<(+ th tph bh st)1e-6) nil)
   (T
    ;;鉄蓋 : 最上段の内空と同じ断面。上面が天端に一致する
    (if(or(> th 0.)(> tph 0.))(setq cw tw cl tl)(setq cw bw cl bl))
    (if(> cvt 0.)(fn_disc p_upper(- ztop cvt)cvt cw cl 0. "MHCOVER"))

    (if(> th 0.)
        (progn(fn_ring p_upper(- z th)th tw tl tt "MHNECK")
              (setq z(- z th))))

    (if(> tph 0.)
        (progn(fn_taper(- z tph)tph)
              (setq z(- z tph))))

    (if(> bh 0.)
        (progn(fn_ring p_center(- z bh)bh bw bl bt "MHBODY")
              (setq z(- z bh))))

    (if(> st 0.)
        (progn(fn_disc p_center(- z st)st bw bl bt "MHSLAB")
              (setq z(- z st))))

    (reverse ls_out))))

;;外形(または内空)の閉じたループの頂点を返す
;;   int_shape 0=円形 / 1=角形、w l は内空寸法、offset は外側への足し分
;;   num は円形のときの分割数(角形は 4 として呼ぶ)
;;   +Z から見て反時計回りに並べる
(defun td3d_mh_ringloop( int_shape p zz w l offset vecu vecv num
                         / rr uu vv ii ang ls_out )
  (if(<(abs int_shape)0.5)
      (progn
        (setq rr(+(* 0.5 w)offset) ii -1 ls_out(list))
        (while(<(setq ii(1+ ii))num)
          (setq ang(/(* 2. pi ii)num)
                ls_out(cons(list(+(car p)(* rr(cos ang)))
                                (+(cadr p)(* rr(sin ang)))
                                zz)
                           ls_out)))
        (reverse ls_out))
    (progn
      (setq uu(+(* 0.5 w)offset) vv(+(* 0.5 l)offset))
      (mapcar '(lambda(q)(td3d_mh_toworld p vecu vecv(car q)(cadr q)zz))
              (list(list uu(- vv))(list uu vv)
                   (list(- uu)vv)(list(- uu)(- vv)))))))

;;点のリストを face 列の 1 面ぶんの文字列にする
(defun td3d_mh_facestr( ls_p / )
  (substr(apply 'strcat
                (mapcar '(lambda(p)
                           (apply 'strcat
                                  (mapcar '(lambda(a)(strcat ","(as-numstr a)))p)))
                        ls_p))
         2))

;;下面と上面のループから角柱の閉じたシェルを作り、face 列の文字列にする
;;  ls_a : 下面 / ls_b : 上面。同じ個数・同じ順序で渡す
;;  ls_a は ls_a → ls_b の向きを法線とする向き(反時計回り)であること
;;    → 下面は順序を逆にすると外向きになる
;;  面は "|"、座標は "," で区切る。CSV の1セルに入れるので "" で囲む
(defun td3d_mh_prismfaces( ls_a ls_b / num ii jj ls_face )
  (setq num(length ls_a)
        ls_face(list(td3d_mh_facestr(reverse ls_a))
                    (td3d_mh_facestr ls_b))
        ii -1)
  (while(<(setq ii(1+ ii))num)
    (setq jj(rem(1+ ii)num)
          ls_face(cons(td3d_mh_facestr
                       (list(td3d_mh_nth ii ls_a)(td3d_mh_nth jj ls_a)
                            (td3d_mh_nth jj ls_b)(td3d_mh_nth ii ls_b)))
                      ls_face)))
  (strcat "\""
          (substr(apply 'strcat
                        (mapcar '(lambda(str)(strcat "|" str))(reverse ls_face)))
                 2)
          "\""))

;;---------------------------------------------------------------------
;; サンプルブロック
;;---------------------------------------------------------------------
(defun td3d_mh_makesampleblock( str_name ls_val
                                / blocks str_bname blk ls_part set_ent num vnam )
  (setq blocks(vla-get-Blocks(vla-get-ActiveDocument(vlax-get-acad-object)))
        str_bname(strcat td3d_mh_blockhead str_name))
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

  (setq ls_part(td3d_mh_makeparts ls_val(list 0. 0. 0.)(list 1. 0. 0.)
                                  nil(getvar "CLAYER")))
  ;;見本でも鉄蓋の色を合わせておく
  (mapcar '(lambda(a)
             (if(= (cdr a)"MHCOVER")
                 (vl-catch-all-apply 'vla-put-color
                                     (list(car a)td3d_mh_covercolor))))
          ls_part)
  (if(null ls_part)nil
    (progn
      (setq blk(vla-Add blocks(vlax-3d-point 0 0 0)str_bname))
      (td3d_mh_copyparts ls_part blk)
      (td3d_mh_deleteparts ls_part)
      (vl-catch-all-apply 'vlax-release-object(list blk))
      str_bname)))

;;---------------------------------------------------------------------
;; 標準テンプレート
;;   下水道用組立マンホール(JIS A 5372 / JSWAS A-11)の内径・壁厚と、
;;   電気設備標準の鉄蓋呼び径から構成。単位は mm、登録時に 1/1000 する。
;;   ※斜壁高さはメーカー製品図が画像のため実寸未確認。上端600・下端内径
;;     からの推定値。判り次第ダイアログで修正して使うこと。
;;---------------------------------------------------------------------
(defun td3d_mh_defaultdef( / )
  ;;(名前 形状 上部内径 上部長 上部壁厚 上部高 斜壁高
  ;;       下部内径 下部長 下部壁厚 下部高 底版厚 鉄蓋径 鉄蓋厚 | 斜壁偏心)
  ;;  最後の偏心フラグ以外は mm。偏心フラグは 0=同心 / 1=偏心
  (list
   (list (mix_strasc(list "MH_1" 21495 "_" 20559 24515 26012 22721))  0  600  600 75 300 450  900  900  75 1000 150 600 60  1)
   (list (mix_strasc(list "MH_1" 21495 "_" 21516 24515 26012 22721))  0  600  600 75 300 450  900  900  75 1000 150 600 60  0)
   (list (mix_strasc(list "MH_1" 21495 "_" 30452 22721))      0  900  900 75 300   0  900  900  75 1000 150 600 60  0)
   (list (mix_strasc(list "MH_0" 21495 "_" 20559 24515 26012 22721))  0  600  600 75 300 400  750  750  75 1000 150 600 60  1)
   (list (mix_strasc(list "MH_2" 21495 "_" 20559 24515 26012 22721))  0  600  600 75 300 600 1200 1200 100 1000 150 600 60  1)
   (list (mix_strasc(list "MH_3" 21495 "_" 20559 24515 26012 22721))  0  600  600 75 300 750 1500 1500 125 1000 200 600 60  1)
   (list (mix_strasc(list "MH_Y" 21495 "_" 30452 22721))      0  600  600 75   0   0  600  600  75 1000 150 600 60  0)
   (list (mix_strasc(list "MH_" 39318 37096 12398 12415 "_600"))  0  600  600 75   0   0  600  600  75  500   0 600 60  0)
   (list (mix_strasc(list "MH_" 39318 37096 12398 12415 "_750"))  0  750  750 75   0   0  750  750  75  500   0 750 60  0)
   (list (mix_strasc(list "MH_" 35282 24418 "_400x800"))  1  400  800 75   0   0  400  800  75  600   0   0 60  0)
   (list (mix_strasc(list "MH_" 35282 24418 "_600x1200")) 1  600 1200 75   0   0  600 1200  75  800   0   0 60  0)
   ))

(defun td3d_mh_makedefault( ls_def / num bool_quiet )
  (if(null ls_def)(setq ls_def(td3d_mh_defaultdef)))
  (setq num 0 bool_quiet td3d_mh_quiet td3d_mh_quiet T)
  (mapcar
   '(lambda(lst / str_name ls_val ls_all)
      (setq str_name(car lst)
            ls_all(cdr lst)
            ;;先頭(形状)と末尾(偏心)はそのまま、間の寸法だけ mm → 図面単位
            ls_val(append
                   (list(float(car ls_all)))
                   (mapcar '(lambda(a)(/(float a)1000.))
                           (reverse(cdr(reverse(cdr ls_all)))))
                   (list(float(last ls_all)))))
      (if(td3d_mh_validate ls_val)T
        (progn
          (td3d_mh_write str_name ls_val)
          (td3d_mh_makesampleblock str_name ls_val)
          (setq num(1+ num))
          (princ (mix_strasc(list "\n  " str_name))))))
   ls_def)
  (setq td3d_mh_quiet bool_quiet)
  num)

;;標準テンプレートのうち、図面に無いものだけを補う
(defun td3d_mh_ensuredefault( / ls_now ls_add num )
  (setq ls_now(td3d_mh_names)
        ls_add(vl-remove nil
                         (mapcar '(lambda(lst)
                                    (if(td3d_mh_pos(car lst)ls_now)nil lst))
                                 (td3d_mh_defaultdef))))
  (if(null ls_add)nil
    (progn
      (princ (mix_strasc(list "\n" 27161 28310 12510 12531 12507 12540 12523 12398 12358 12385 30331 37682 12373 12428 12390 12356 12394 12356 12418 12398 12434 36861 21152 12375 12414 12377)))
      (setq num(td3d_mh_makedefault ls_add))
      (princ (mix_strasc(list "\n" 27161 28310 12510 12531 12507 12540 12523 12434 " " (itoa num) " " 20214 36861 21152 12375 12414 12375 12383)))
      num)))


(defun td3d_mh_writedcl( / str_path open_file )
  (setq str_path(strcat(td3d_mh_tempdir)"td3dmanhole.dcl")
        open_file(open str_path "w"))
  (if(null open_file)nil
    (progn
      (write_strlist
       open_file
       (list
        "td3dmh :dialog"
        "{"
        (mix_strasc(list " label = \"" 12510 12531 12507 12540 12523 12486 12531 12503 12524 12540 12488 "\";"))
        " :row"
        " {"
        "  :boxed_column"
        "  {"
        (mix_strasc(list "   label = \"" 30331 37682 28168 12415 "\";"))
        "   :list_box { key = \"tmplist\"; width = 26; height = 20; }"
        "  }"
        "  :boxed_column"
        "  {"
        (mix_strasc(list "   label = \"" 23544 27861 " (" 22259 38754 21336 20301 ")\";"))
        (mix_strasc(list "   :edit_box { key = \"name\"; label = \"" 12486 12531 12503 12524 12540 12488 21517 "\"; edit_width = 18; }"))
        "   spacer;"
        (mapcar '(lambda(lst)
                   (if(td3d_mh_togglep lst)
                       (mix_strasc(list "   :toggle { key = \"" (car lst) "\"; label = \"" (cadr lst) "\"; }"))
                     (mix_strasc(list "   :edit_box { key = \"" (car lst) "\"; label = \"" (cadr lst) "\"; edit_width = 10; }"))))
                (td3d_mh_paramdef))
        "   spacer;"
        (mix_strasc(list "   :text { label = \"" 8251 26012 22721 39640 12373 12364 " 0 " 12424 12426 22823 12365 12356 12392 12365 26012 22721 12364 20837 12426 12414 12377 "\"; }"))
        (mix_strasc(list "   :text { label = \"" 8251 35282 24418 12289 12414 12383 12399 24133 8800 38263 12373 12398 12392 12365 37197 32622 26178 12395 21521 12365 12434 32862 12365 12414 12377 "\"; }"))
        "  }"
        " }"
        " :row"
        " {"
        "  alignment = right;"
        "  fixed_width = true;"
        (mix_strasc(list "  :button { key = \"btnsave\"; label = \"" 20445 23384 "\"; width = 12; fixed_width = true; is_default = true; }"))
        (mix_strasc(list "  :button { key = \"btndel\"; label = \"" 21066 38500 "\"; width = 12; fixed_width = true; }"))
        (mix_strasc(list "  :button { key = \"btnexp\"; label = \"CSV" 26360 20986 "\"; width = 12; fixed_width = true; }"))
        (mix_strasc(list "  :button { key = \"btnimp\"; label = \"CSV" 21462 36796 "\"; width = 12; fixed_width = true; }"))
        (mix_strasc(list "  :button { key = \"btnclose\"; label = \"" 27770 23450 "\"; width = 12; fixed_width = true; is_cancel = true; }"))
        " }"
        "}"
        ))
      (close open_file)
      str_path)))


;;=====================================================================
;; コマンド : テンプレートの作成 / 編集 / 削除
;;=====================================================================
(defun td3dmanhole( / str_path load_dcl int_ret bool_loop
                      str_name ls_val )
  ;; (setq *error*
  ;;       (lambda(msg)
  ;;         (if(and(boundp 'load_dcl)load_dcl)
  ;;             (vl-catch-all-apply 'unload_dialog(list load_dcl)))
  ;;         (princ msg)))

  (if(null(setq str_path(td3d_mh_writedcl)))
      (td3d_mh_alert (mix_strasc(list 12480 12452 12450 12525 12464 23450 32681 12501 12449 12452 12523 12434 26360 12365 20986 12379 12414 12379 12435 12391 12375 12383)))
    (progn
      (td3d_mh_ensuredefault)
      (setq bool_loop T)
      (while bool_loop
        (setq td3d_mh_ls_name(td3d_mh_names)
              td3d_mh_result nil
              load_dcl(load_dialog str_path))
        (cond
         ((< load_dcl 0)
          (td3d_mh_alert (mix_strasc(list 12480 12452 12450 12525 12464 12434 35501 12415 36796 12417 12414 12379 12435 12391 12375 12383)))
          (setq bool_loop nil))
         ((null(new_dialog "td3dmh" load_dcl))
          (unload_dialog load_dcl)
          (td3d_mh_alert (mix_strasc(list 12480 12452 12450 12525 12464 12434 38283 12369 12414 12379 12435 12391 12375 12383)))
          (setq bool_loop nil))
         (T
          (start_list "tmplist")
          (if td3d_mh_ls_name(mapcar 'add_list td3d_mh_ls_name))
          (end_list)
          (set_tile "name" "")
          (td3d_mh_settiles nil)
          (action_tile "tmplist"  "(td3d_mh_dlg_select)")
          (action_tile "btnsave"  "(td3d_mh_dlg_save)")
          (action_tile "btndel"   "(td3d_mh_dlg_delete)")
          (action_tile "btnexp"   "(done_dialog 4)")
          (action_tile "btnimp"   "(done_dialog 5)")
          (action_tile "btnclose" "(setq str_templatemanhole(get_tile \"name\"))(done_dialog 0)")
          (setq int_ret(start_dialog))
          (unload_dialog load_dcl)
          (setq load_dcl nil)
          (cond
           ((= int_ret 2)
            (setq str_name(car td3d_mh_result) ls_val(cadr td3d_mh_result))
            (td3d_mh_write str_name ls_val)
            (if(td3d_mh_makesampleblock str_name ls_val)
                (princ (mix_strasc(list "\n" 20445 23384 12375 12414 12375 12383 " : " str_name)))
              (progn
                (princ (mix_strasc(list "\n" 20445 23384 12375 12414 12375 12383 " : " str_name)))
                (td3d_mh_alert (mix_strasc(list 12486 12531 12503 12524 12540 12488 12399 20445 23384 12375 12414 12375 12383 12364 12289 24418 29366 12398 20316 25104 12395 22833 25943 12375 12414 12375 12383 "\n" 23544 27861 12434 35211 30452 12375 12390 12367 12384 12373 12356))))))
           ((= int_ret 3)
            (setq str_name(car td3d_mh_result))
            (td3d_mh_erase str_name)
            (princ (mix_strasc(list "\n" 21066 38500 12375 12414 12375 12383 " : " str_name))))
           ((= int_ret 4)(td3d_mh_export))
           ((= int_ret 5)(td3d_mh_import))
           (T
            (if(vl-position str_templatemanhole td3d_mh_ls_name)T
              (setq str_templatemanhole nil))
            (setq bool_loop nil)
            )
           )
          ))
        )))
  (princ))


;;=====================================================================
;; コマンド : マンホールの配置
;;   1 点クリック。テンプレートが非対称のときだけ 2 点目で向きを聞く。
;;=====================================================================


(defun td3d_mh_export( / str_path open_file ls_name num )
  (setq ls_name(td3d_mh_names))
  (cond
   ((null ls_name)(td3d_mh_alert (mix_strasc(list 26360 12365 20986 12377 12486 12531 12503 12524 12540 12488 12364 12354 12426 12414 12379 12435)))nil)
   ((null(setq str_path(getfiled (mix_strasc(list 12510 12531 12507 12540 12523 12486 12531 12503 12524 12540 12488 12398 26360 12365 20986 12375))
                                 (strcat(getvar "DWGPREFIX")"manholetemplate")
                                 "csv" 1)))nil)
   ((null(setq open_file(td3d_mh_openwrite str_path)))
    (td3d_mh_alert (mix_strasc(list 12501 12449 12452 12523 12395 26360 12365 36796 12417 12414 12379 12435 12391 12375 12383)))nil)
   (T
    (setq num 0)
    (write-line
     (apply 'strcat(cons "NAME"(mapcar '(lambda(lst)(strcat ","(car lst)))
                                       (td3d_mh_paramdef))))
     open_file)
    (mapcar
     '(lambda(str_name / ls_val)
        (if(setq ls_val(td3d_mh_read str_name))
            (progn
              (write-line
               (apply 'strcat
                      (cons(td3d_mh_qq str_name)
                           (mapcar '(lambda(v)(strcat ","(rtos v 2 6)))ls_val)))
               open_file)
              (setq num(1+ num)))))
     ls_name)
    (close open_file)
    (princ (mix_strasc(list "\n" 26360 12365 20986 12375 12414 12375 12383 " : " (itoa num) 20214 "  " str_path)))
    (td3d_mh_alert (mix_strasc(list 26360 12365 20986 12375 12414 12375 12383 "\n" (itoa num) 20214 "\n" str_path)))
    str_path)))

(defun td3d_mh_writeimpdcl( / str_path open_file )
  (setq str_path(strcat(td3d_mh_tempdir)"td3dmhimport.dcl")
        open_file(open str_path "w"))
  (if(null open_file)nil
    (progn
      (write_strlist
       open_file
       (list
        "td3dmhimp :dialog"
        "{"
        (mix_strasc(list " label = \"" 12510 12531 12507 12540 12523 12486 12531 12503 12524 12540 12488 12398 21462 12426 36796 12415 "\";"))
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
        (mix_strasc(list " :text { label = \"" 8251 24418 29366 "(SHAPE)" 21015 12399 21336 20301 22793 25563 12398 23550 35937 22806 12391 12377 "\"; }"))
        " ok_cancel;"
        "}"
        ))
      (close open_file)
      str_path)))


(defun td3d_mh_importdlg( num_all num_dup / str_path load_dcl int_ret )
  (cond
   ((null(setq str_path(td3d_mh_writeimpdcl)))
    (td3d_mh_alert (mix_strasc(list 12480 12452 12450 12525 12464 23450 32681 12501 12449 12452 12523 12434 26360 12365 20986 12379 12414 12379 12435 12391 12375 12383)))nil)
   (T
    (setq td3d_mh_result nil load_dcl(load_dialog str_path))
    (cond
     ((< load_dcl 0)(td3d_mh_alert (mix_strasc(list 12480 12452 12450 12525 12464 12434 35501 12415 36796 12417 12414 12379 12435 12391 12375 12383)))nil)
     ((null(new_dialog "td3dmhimp" load_dcl))
      (unload_dialog load_dcl)
      (td3d_mh_alert (mix_strasc(list 12480 12452 12450 12525 12464 12434 38283 12369 12414 12379 12435 12391 12375 12383)))nil)
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
      (action_tile "accept" "(td3d_mh_dlg_import)")
      (setq int_ret(start_dialog))
      (unload_dialog load_dcl)
      (if(= int_ret 1)td3d_mh_result nil))
     ))))

(defun td3d_mh_import( / str_path open_file str_line ls_head ls_idx int_name
                         ls_data ls_now lst num_dup ret scale int_dup
                         num_add num_ow num_skip num_err ls_err str_tail )
  (cond
   ((null(setq str_path(getfiled (mix_strasc(list 12510 12531 12507 12540 12523 12486 12531 12503 12524 12540 12488 12398 21462 12426 36796 12415))
                                 (getvar "DWGPREFIX")"csv" 0)))nil)
   ((null(setq open_file(td3d_mh_openread str_path)))
    (td3d_mh_alert (mix_strasc(list 12501 12449 12452 12523 12434 38283 12369 12414 12379 12435 12391 12375 12383)))nil)
   ((null(setq str_line(read-line open_file)))
    (close open_file)(td3d_mh_alert (mix_strasc(list 31354 12398 12501 12449 12452 12523 12391 12377)))nil)
   (T
    (setq ls_head(mapcar '(lambda(a)(strcase(td3d_mh_trim a)))
                         (td3d_mh_csvsplit str_line))
          int_name(td3d_mh_pos "NAME" ls_head)
          ls_idx(mapcar '(lambda(lst)(td3d_mh_pos(car lst)ls_head))
                        (td3d_mh_paramdef)))
    (cond
     ((null int_name)
      (close open_file)
      (td3d_mh_alert (mix_strasc(list "1" 34892 30446 12395 " NAME " 21015 12364 35211 12388 12363 12426 12414 12379 12435)))nil)
     (T
      (setq ls_data(list))
      (while(setq str_line(read-line open_file))
        (setq lst(mapcar 'td3d_mh_trim(td3d_mh_csvsplit str_line)))
        (if(=(td3d_mh_field int_name lst)"")T
          (setq ls_data
                (cons(cons(td3d_mh_field int_name lst)
                          (mapcar '(lambda(i lst_def / str)
                                     (setq str(td3d_mh_field i lst))
                                     (if(= str "")(caddr lst_def)(atof str)))
                                  ls_idx(td3d_mh_paramdef)))
                     ls_data))))
      (close open_file)
      (setq ls_data(reverse ls_data))
      (cond
       ((null ls_data)(td3d_mh_alert (mix_strasc(list 21462 12426 36796 12417 12427 12487 12540 12479 34892 12364 12354 12426 12414 12379 12435)))nil)
       (T
        (setq ls_now(td3d_mh_names) num_dup 0)
        (mapcar '(lambda(a)(if(td3d_mh_pos(car a)ls_now)(setq num_dup(1+ num_dup))))
                ls_data)
        (if(null(setq ret(td3d_mh_importdlg(length ls_data)num_dup)))nil
          (progn
            (setq scale(td3d_mh_unitscale(car ret))
                  int_dup(cadr ret)
                  num_add 0 num_ow 0 num_skip 0 num_err 0 ls_err(list)
                  td3d_mh_quiet T)
            (mapcar
             '(lambda(a / str_name ls_val str_err bool_exist ii)
                (setq str_name(car a) ii -1
                      ;;形状(先頭)は倍率をかけない
                      ls_val(mapcar '(lambda(v)
                                       (setq ii(1+ ii))
                                       (if(= ii 0)v(* v scale)))
                                    (cdr a))
                      bool_exist(if(td3d_mh_pos str_name ls_now)T nil))
                (cond
                 ((and bool_exist(= int_dup 1))(setq num_skip(1+ num_skip)))
                 ((setq str_err(td3d_mh_validate ls_val))
                  (setq num_err(1+ num_err)
                        ls_err(cons(strcat str_name " : " str_err)ls_err)))
                 (T
                  (td3d_mh_write str_name ls_val)
                  (td3d_mh_makesampleblock str_name ls_val)
                  (if bool_exist(setq num_ow(1+ num_ow))(setq num_add(1+ num_add))))))
             ls_data)
            (setq ls_err(reverse ls_err) td3d_mh_quiet nil)
            (princ (mix_strasc(list "\n" 21462 12426 36796 12415 32080 26524 " " 26032 35215 (itoa num_add) " / " 19978 26360 12365 (itoa num_ow) " / " 12473 12461 12483 12503 (itoa num_skip) " / " 12456 12521 12540 (itoa num_err))))
            (mapcar '(lambda(str)(princ(strcat "\n  " str)))ls_err)
            (setq str_tail(if ls_err (mix_strasc(list "\n\n" 12456 12521 12540 12398 20869 23481 12399 12467 12510 12531 12489 12521 12452 12531 12434 35211 12390 12367 12384 12373 12356)) ""))
            (td3d_mh_alert (mix_strasc(list 21462 12426 36796 12415 12414 12375 12383 "\n\n" 26032 35215 " : " (itoa num_add) 20214 "\n" 19978 26360 12365 " : " (itoa num_ow) 20214 "\n" 12473 12461 12483 12503 " : " (itoa num_skip) 20214 "\n" 12456 12521 12540 " : " (itoa num_err) 20214 str_tail)))
            T))
        ))
      ))
    )))


(defun c:td3dmhexport( / )(td3d_mh_export)(princ))
(defun c:td3dmhimport( / )(td3d_mh_import)(princ))

(princ)
