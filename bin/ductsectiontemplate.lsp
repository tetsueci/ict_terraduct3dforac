;;;;=====================================================================
;;;; ductsectiontemplate.lsp
;;;;   terraduct3D 追加モジュール : 管路断面の登録(DCL版)
;;;;
;;;;   (td3dductsection) … 断面の作成/編集/削除の画面。単体で動く
;;;;                       画面を閉じるとき、選択中の断面名を str_ductsection に返す
;;;;
;;;;   保存形式 : 図面辞書 "terraduct3d_ductsection" に断面名ごとの XRecord
;;;;     並び : (1071 版数=1)(1071 管数) のあと、管1本につき次の10項目
;;;;       (1000 名前)(1040 X)(1040 Y)(1040 管径)
;;;;       (1071 保護コンタイプ)(1040 幅)(1040 高さ)(1040 フィレット)
;;;;       (1071 管の色)(1071 保護コンの色)
;;;;     ※項目を増やすときは版数を上げ、read 側で分岐する
;;;;
;;;;   保護コンタイプ : 0=全周 / 1=下半 / 2=なし (既存CSVの TYPE と同じ)
;;;;     CORNER への変換は配置側で行う
;;;;       全周 : フィレットあり  8 / なし  4
;;;;       下半 : フィレットあり -6 / なし -4
;;;;     幅・高さは中心からの半値(既存 CONMESH の WIDTH HEIGHT と同じ)
;;;;
;;;;   管の名前 : ここでは基本名だけを持つ。配置時に同名ブロックがあれば
;;;;              td3d_ds_uniquename で「名前$整数」の空き番号を付ける
;;;;
;;;;   依存 : %library.lsp (mix_strasc write_strlist as-numstr)
;;;;   ロード順 : %library.lsp のあと
;;;;   ガイドメニューへの組み込みはまだ行っていない
;;;;=====================================================================

(vl-load-com)

(setq td3d_ds_dictname "terraduct3d_ductsection" ;;断面保存用の図面辞書
      td3d_ds_arcseg   32                        ;;プレビュー円の分割数
      )

;;---------------------------------------------------------------------
;; 共通ユーティリティ
;;   ccboxtemplate.lsp の td3d_tmp_* と同じ内容。
;;   このファイル単独で動くように td3d_ds_ として持つ。
;;---------------------------------------------------------------------
(defun td3d_ds_alert( str )
  (alert str)(princ))

(defun td3d_ds_numstr( a )
  (if a(rtos a 2 4)""))

(defun td3d_ds_dicts( / )
  (vla-get-Dictionaries(vla-get-ActiveDocument(vlax-get-acad-object))))

(defun td3d_ds_dict( bool_create / dicts dict )
  (setq dicts(td3d_ds_dicts))
  (if(vl-catch-all-error-p
      (setq dict(vl-catch-all-apply 'vla-Item(list dicts td3d_ds_dictname))))
      (setq dict(if bool_create(vla-Add dicts td3d_ds_dictname)nil)))
  dict)

;;安全な nth (nth は範囲外で落ちる環境があるため car/cdr だけで書く)
(defun td3d_ds_nth( num lst / )
  (while(and lst(> num 0))(setq lst(cdr lst)num(1- num)))
  (car lst))

;;断面名の一覧
(defun td3d_ds_names( / dict ls_name str )
  (setq ls_name(list))
  (if(setq dict(td3d_ds_dict nil))
      (vlax-for
       xrec dict
       (if(vl-catch-all-error-p
           (setq str(vl-catch-all-apply 'vla-get-name(list xrec))))
           T
         (setq ls_name(cons str ls_name)))))
  (reverse ls_name))

;;---------------------------------------------------------------------
;; 管1本ぶんのデータ
;;   (名前 X Y 管径 タイプ 幅 高さ フィレット 管色 保護コン色)
;;   欠けや型違いを既定値で埋めて必ず10項目にする
;;---------------------------------------------------------------------
(defun td3d_ds_fillpipe( lst / fnum fint a )
  (setq fnum(lambda(a dflt)
              (cond((=(type a)'REAL)a)
                   ((=(type a)'INT)(float a))
                   ((=(type a)'STR)(atof a))
                   (T dflt)))
        fint(lambda(a dflt)
              (cond((=(type a)'INT)a)
                   ((=(type a)'REAL)(fix a))
                   ((=(type a)'STR)(atoi a))
                   (T dflt))))
  (list
   (if(=(type(car lst))'STR)(car lst)"P")
   (fnum(td3d_ds_nth 1 lst)0.)
   (fnum(td3d_ds_nth 2 lst)0.)
   (fnum(td3d_ds_nth 3 lst)0.2)
   (fint(td3d_ds_nth 4 lst)0)
   (fnum(td3d_ds_nth 5 lst)0.3)
   (fnum(td3d_ds_nth 6 lst)0.3)
   (fnum(td3d_ds_nth 7 lst)0.)
   (fint(td3d_ds_nth 8 lst)3)
   (fint(td3d_ds_nth 9 lst)8)
   ))

;;---------------------------------------------------------------------
;; 辞書 入出力
;;---------------------------------------------------------------------
;;断面を読む。戻り値 : 管リスト / 無ければ nil
(defun td3d_ds_read( str_name / dict xrec array_type array_data ls cnt ls_out lst ii )
  (if(setq dict(td3d_ds_dict nil))
      (if(vl-catch-all-error-p
          (setq xrec(vl-catch-all-apply 'vla-Item(list dict str_name))))
          nil
        (progn
          (vla-GetXRecordData xrec 'array_type 'array_data)
          (if array_data
              (progn
                (setq ls(mapcar 'vlax-variant-value
                                (vlax-safearray->list array_data))
                      ls(cdr ls);;版数(いまは1のみ)
                      cnt(car ls)ls(cdr ls)
                      ls_out(list))
                (if(=(type cnt)'INT)T(setq cnt 0))
                (while(>(setq cnt(1- cnt))-1)
                  (setq lst(list)ii -1)
                  (while(<(setq ii(1+ ii))10)
                    (setq lst(cons(car ls)lst)ls(cdr ls)))
                  (setq ls_out(cons(td3d_ds_fillpipe(reverse lst))ls_out)))
                (reverse ls_out)))))))

;;断面を書く(空の断面も書ける)
(defun td3d_ds_write( str_name ls_pipe / dict xrec ls_val ls_type num
                                          array_type array_data )
  (setq dict(td3d_ds_dict T)
        ls_pipe(mapcar 'td3d_ds_fillpipe ls_pipe)
        ls_val(append(list 1(length ls_pipe))(apply 'append ls_pipe))
        ls_type(append
                (list 1071 1071)
                (apply 'append
                       (mapcar '(lambda(a)
                                  (list 1000 1040 1040 1040 1071
                                        1040 1040 1040 1071 1071))
                               ls_pipe)))
        num(length ls_val))
  (if(vl-catch-all-error-p
      (setq xrec(vl-catch-all-apply 'vla-Item(list dict str_name))))
      (setq xrec(vla-AddXRecord dict str_name)))
  (setq array_data(vlax-make-safearray vlax-vbVariant(cons 0(1- num)))
        array_type(vlax-make-safearray vlax-vbInteger(cons 0(1- num))))
  (vlax-safearray-fill array_type ls_type)
  (vlax-safearray-fill array_data ls_val)
  (vla-SetXRecordData xrec array_type array_data)
  xrec)

;;断面を消す
(defun td3d_ds_erase( str_name / dict xrec )
  (if(setq dict(td3d_ds_dict nil))
      (if(vl-catch-all-error-p
          (setq xrec(vl-catch-all-apply 'vla-Item(list dict str_name))))
          T
        (vla-delete xrec)))
  (princ))

;;---------------------------------------------------------------------
;; 妥当性検査
;;---------------------------------------------------------------------
;;名前(断面名・管名で共用)。戻り値 : nil=正常 / 文字列=エラー内容
(defun td3d_ds_validname( str / )
  (cond
   ((or(null str)(= str ""))
    (mix_strasc(list 21517 21069 12434 20837 21147 12375 12390 12367 12384 12373 12356)));;名前を入力してください
   ((or(vl-string-search " " str)(vl-string-search(chr 12288)str))
    (mix_strasc(list 12473 12506 12540 12473 12399 20351 12360 12414 12379 12435)));;スペースは使えません
   ((vl-string-search "$" str);;「$」は重複時の連番に使うため入力禁止
    (mix_strasc(list 12300 "$" 12301 12399 20351 12360 12414 12379 12435)))
   ((vl-string-search "," str)
    (mix_strasc(list 12300 "," 12301 12399 20351 12360 12414 12379 12435)))
   (T nil)))

;;管1本。戻り値 : nil=正常 / 文字列=エラー内容
(defun td3d_ds_validpipe( lst / name x y d ct w h f c1 c2 str )
  (mapcar 'set '(name x y d ct w h f c1 c2)(td3d_ds_fillpipe lst))
  (cond
   ((setq str(td3d_ds_validname name))str)
   ((<= d 1e-9);;管径は正の値を入力してください
    (mix_strasc(list 31649 24452 12399 27491 12398 20516 12434 20837 21147 12375 12390 12367 12384 12373 12356)))
   ((or(< ct 0)(> ct 2));;保護コンタイプが不正です
    (mix_strasc(list 20445 35703 12467 12531 12479 12452 12503 12364 19981 27491 12391 12377)))
   ((and(/= ct 2)(or(<= w 1e-9)(<= h 1e-9)));;保護コンの幅と高さは正の値を入力してください
    (mix_strasc(list 20445 35703 12467 12531 12398 24133 12392 39640 12373 12399 27491 12398 20516 12434 20837 21147 12375 12390 12367 12384 12373 12356)))
   ((< f 0.);;フィレットに負の値は指定できません
    (mix_strasc(list 12501 12451 12524 12483 12488 12395 36000 12398 20516 12399 25351 23450 12391 12365 12414 12379 12435)))
   ((and(/= ct 2)(> f 0.)(>= f(min w h)));;フィレットが大きすぎます
    (mix_strasc(list 12501 12451 12524 12483 12488 12364 22823 12365 12377 12366 12414 12377)))
   ((or(< c1 1)(> c1 255)(< c2 1)(> c2 255));;色は 1-255 で指定してください
    (mix_strasc(list 33394 12399 " 1-255 " 12391 25351 23450 12375 12390 12367 12384 12373 12356)))
   (T nil)))

;;---------------------------------------------------------------------
;; 配置時のブロック名(将来の組み込み用)
;;   既にあれば 名前$1 $2 … の空き番号を返す
;;---------------------------------------------------------------------
(defun td3d_ds_uniquename( str / blocks ii )
  (setq blocks(vla-get-Blocks(vla-get-ActiveDocument(vlax-get-acad-object))))
  (if(vl-catch-all-error-p(vl-catch-all-apply 'vla-Item(list blocks str)))
      str
    (progn
      (setq ii 0)
      (while(null(vl-catch-all-error-p
                  (vl-catch-all-apply
                   'vla-Item(list blocks(strcat str "$"(itoa(setq ii(1+ ii))))))))
        )
      (strcat str "$"(itoa ii)))))

;;---------------------------------------------------------------------
;; DCL 書き出し
;;---------------------------------------------------------------------
(defun td3d_ds_tempdir( / )
  (if(and(boundp 'str_path_tempdirectory)str_path_tempdirectory)
      str_path_tempdirectory
    (getvar "TEMPPREFIX")))

(defun td3d_ds_writedcl( / str_path open_file )
  (setq str_path(strcat(td3d_ds_tempdir)"td3dductsection.dcl")
        open_file(open str_path "w"))
  (if(null open_file)nil
    (progn
      (write_strlist
       open_file
       (list
        "td3dds :dialog"
        "{"
        (mix_strasc(list " label = \"" 31649 36335 26029 38754 12398 30331 37682 "\";"));;管路断面の登録
        " :row"
        " {"
        ;;--- 断面 ---
        "  :boxed_column"
        "  {"
        (mix_strasc(list "   label = \"" 26029 38754 "\";"))
        "   :list_box { key = \"seclist\"; width = 22; height = 21; }"
        (mix_strasc(list "   :edit_box { key = \"secname\"; label = \"" 26029 38754 21517 "\"; edit_width = 12; }"))
        "   :row"
        "   {"
        (mix_strasc(list "    :button { key = \"btnsnew\"; label = \"" 26032 35215 "\"; }"));;新規
        (mix_strasc(list "    :button { key = \"btnscopy\"; label = \"" 35079 35069 "\"; }"));;複製
        "   }"
        "   :row"
        "   {"
        (mix_strasc(list "    :button { key = \"btnsren\"; label = \"" 12522 12493 12540 12512 "\"; }"));;リネーム
        (mix_strasc(list "    :button { key = \"btnsdel\"; label = \"" 21066 38500 "\"; }"));;削除
        "   }"
        "  }"
        ;;--- 管一覧 ---
        "  :boxed_column"
        "  {"
        (mix_strasc(list "   label = \"" 31649 19968 35239 "\";"));;管一覧
        "   :list_box { key = \"pipelist\"; width = 26; height = 21; }"
        "   :row"
        "   {"
        (mix_strasc(list "    :button { key = \"btnpadd\"; label = \"" 36861 21152 "\"; }"));;追加
        (mix_strasc(list "    :button { key = \"btnpcopy\"; label = \"" 35079 35069 "\"; }"))
        (mix_strasc(list "    :button { key = \"btnpdel\"; label = \"" 21066 38500 "\"; }"))
        "   }"
        "   :row"
        "   {"
        (mix_strasc(list "    :button { key = \"btnpup\"; label = \"" 19978 12408 "\"; }"));;上へ
        (mix_strasc(list "    :button { key = \"btnpdown\"; label = \"" 19979 12408 "\"; }"));;下へ
        "   }"
        "  }"
        ;;--- 管の編集 ---
        "  :boxed_column"
        "  {"
        (mix_strasc(list "   label = \"" 31649 12398 32232 38598 " (" 22259 38754 21336 20301 ")\";"));;管の編集(図面単位)
        (mix_strasc(list "   :edit_box { key = \"pname\"; label = \"" 21517 21069 "\"; edit_width = 12; }"))
        (mix_strasc(list "   :edit_box { key = \"px\"; label = \"X (" 21407 28857 12363 12425 31649 20013 24515 ")\"; edit_width = 10; }"));;X(原点から管中心)
        (mix_strasc(list "   :edit_box { key = \"py\"; label = \"Y (" 21407 28857 12363 12425 31649 20013 24515 ")\"; edit_width = 10; }"))
        (mix_strasc(list "   :edit_box { key = \"pdiam\"; label = \"" 31649 24452 "\"; edit_width = 10; }"));;管径
        (mix_strasc(list "   :popup_list { key = \"pcontype\"; label = \"" 20445 35703 12467 12531 12479 12452 12503 "\"; edit_width = 10; }"));;保護コンタイプ
        (mix_strasc(list "   :edit_box { key = \"pw\"; label = \"" 20445 35703 12467 12531 24133 " (" 20013 24515 12363 12425 ")\"; edit_width = 10; }"));;保護コン幅(中心から)
        (mix_strasc(list "   :edit_box { key = \"ph\"; label = \"" 20445 35703 12467 12531 39640 12373 " (" 20013 24515 12363 12425 ")\"; edit_width = 10; }"));;保護コン高さ(中心から)
        (mix_strasc(list "   :edit_box { key = \"pf\"; label = \"" 12501 12451 12524 12483 12488 "\"; edit_width = 10; }"));;フィレット
        "   :row"
        "   {"
        (mix_strasc(list "    :edit_box { key = \"pcolduct\"; label = \"" 31649 12398 33394 "\"; edit_width = 5; }"));;管の色
        (mix_strasc(list "    :button { key = \"btncold\"; label = \"" 33394 "...\"; width = 6; fixed_width = true; }"))
        "   }"
        "   :row"
        "   {"
        (mix_strasc(list "    :edit_box { key = \"pcolcon\"; label = \"" 20445 35703 12467 12531 12398 33394 "\"; edit_width = 5; }"));;保護コンの色
        (mix_strasc(list "    :button { key = \"btncolc\"; label = \"" 33394 "...\"; width = 6; fixed_width = true; }"))
        "   }"
        "   spacer;"
        (mix_strasc(list "   :button { key = \"btnapply\"; label = \"" 21453 26144 "\"; is_default = true; }"));;反映
        (mix_strasc(list "   :text { label = \"" 8251 21453 26144 12391 36984 25246 20013 12398 31649 12395 20516 12364 20837 12426 12414 12377 "\"; }"));;※反映で選択中の管に値が入ります
        "  }"
        ;;--- プレビュー ---
        "  :boxed_column"
        "  {"
        (mix_strasc(list "   label = \"" 12503 12524 12499 12517 12540 "\";"));;プレビュー
        "   :image { key = \"preview\"; width = 45; height = 22;"
        "            fixed_width = true; fixed_height = true; color = 0; }"
        (mix_strasc(list "   :text { label = \"" 21407 28857 "=" 30333 " / " 36984 25246 20013 "=" 40644 33394 12398 21313 23383 "\"; }"));;原点=白 / 選択中=黄色の十字
        "  }"
        " }"
        ;;--- 下段ボタン ---
        " :row"
        " {"
        "  alignment = right;"
        "  fixed_width = true;"
        (mix_strasc(list "  :button { key = \"btnexp\"; label = \"CSV" 26360 20986 "\"; width = 12; fixed_width = true; }"));;CSV書出
        (mix_strasc(list "  :button { key = \"btnimp\"; label = \"CSV" 21462 36796 "\"; width = 12; fixed_width = true; }"));;CSV取込
        (mix_strasc(list "  :button { key = \"btnok\"; label = \"" 27770 23450 "\"; width = 12; fixed_width = true; }"));;決定
        (mix_strasc(list "  :button { key = \"btncancel\"; label = \"" 12461 12515 12531 12475 12523 "\"; width = 12; fixed_width = true; is_cancel = true; }"));;キャンセル
        " }"
        "}"
        ))
      (close open_file)
      str_path)))

;;---------------------------------------------------------------------
;; プレビュー描画
;;   image タイルに vector_image で描く。円は td3d_ds_arcseg 角形近似。
;;   保護コンの角数は CORNER の考え方と同じ
;;     全周 : フィレットあり8角 / なし4角(矩形)
;;     下半 : 上が開いたコの字。フィレットありは下隅を落として6角相当
;;---------------------------------------------------------------------
(defun td3d_ds_preview( ls_pipe int_sel
                        / dx dy xmin xmax ymin ymax ww hh sc cx cy
                          fnx fny fline fpoly ii lst name x y d ct w h f c1 c2
                          rr ang0 ang1 jj ls_p )
  (setq dx(dimx_tile "preview")dy(dimy_tile "preview"))
  (start_image "preview")
  (fill_image 0 0 dx dy 0)

  ;;--- 描画範囲(原点を必ず含める) ---
  (setq xmin -0.1 xmax 0.1 ymin -0.1 ymax 0.1)
  (mapcar
   '(lambda(lst / x y d ct w h rr)
      (mapcar 'set '(name x y d ct w h)lst)
      (setq rr(* 0.5 d))
      (if(/= ct 2)(setq rr(max rr w h)))
      (setq xmin(min xmin(- x rr))xmax(max xmax(+ x rr))
            ymin(min ymin(- y rr))ymax(max ymax(+ y rr))))
   ls_pipe)

  (setq ww(max(- xmax xmin)0.2)hh(max(- ymax ymin)0.2)
        sc(min(/(* 0.88 dx)ww)(/(* 0.88 dy)hh))
        cx(* 0.5(+ xmin xmax))cy(* 0.5(+ ymin ymax))
        ;;モデル座標 → 画像座標(Yは上下反転)
        fnx(lambda(x)(fix(+(*(- x cx)sc)(* 0.5 dx)0.5)))
        fny(lambda(y)(fix(+(*(- cy y)sc)(* 0.5 dy)0.5)))
        fline(lambda(p q c)(vector_image(fnx(car p))(fny(cadr p))
                                        (fnx(car q))(fny(cadr q))c))
        ;;閉じた(または開いた)折れ線
        fpoly(lambda(ls_p c bool_close / p0 pp)
               (setq p0(car ls_p)pp p0 ls_p(cdr ls_p))
               (mapcar '(lambda(p)(fline pp p c)(setq pp p))ls_p)
               (if bool_close(fline pp p0 c)))
        )

  ;;--- 原点 : 画像を貫く薄い軸線(範囲内のときだけ)と白の十字 ---
  (setq ii(fnx 0.))
  (if(and(>= ii 0)(< ii dx))(vector_image ii 0 ii(1- dy)8))
  (setq ii(fny 0.))
  (if(and(>= ii 0)(< ii dy))(vector_image 0 ii(1- dx)ii 8))
  (fline(list -0.05 0.)(list 0.05 0.)7)
  (fline(list 0. -0.05)(list 0. 0.05)7)

  ;;--- 管と保護コン ---
  (setq ii -1)
  (mapcar
   '(lambda(lst / )
      (setq ii(1+ ii))
      (mapcar 'set '(name x y d ct w h f c1 c2)lst)
      (setq rr(* 0.5 d))

      ;;保護コン
      (cond
       ((= ct 0);;全周(フィレットありは8角形、なしは矩形)
        (if(> f 0.)
            (fpoly(list(list(+(- x w)f)(+ y h))(list(-(+ x w)f)(+ y h))
                       (list(+ x w)(-(+ y h)f))(list(+ x w)(+(- y h)f))
                       (list(-(+ x w)f)(- y h))(list(+(- x w)f)(- y h))
                       (list(- x w)(+(- y h)f))(list(- x w)(-(+ y h)f)))
                  c2 T)
          (fpoly(list(list(- x w)(+ y h))(list(+ x w)(+ y h))
                     (list(+ x w)(- y h))(list(- x w)(- y h)))
                c2 T)))
       ((= ct 1);;下半(上が開く)
        (if(> f 0.)
            (fpoly(list(list(- x w)(+ y h))(list(- x w)(+(- y h)f))
                       (list(+(- x w)f)(- y h))(list(-(+ x w)f)(- y h))
                       (list(+ x w)(+(- y h)f))(list(+ x w)(+ y h)))
                  c2 nil)
          (fpoly(list(list(- x w)(+ y h))(list(- x w)(- y h))
                     (list(+ x w)(- y h))(list(+ x w)(+ y h)))
                c2 nil)))
       )

      ;;管の円
      (setq jj -1 ls_p(list))
      (while(<(setq jj(1+ jj))td3d_ds_arcseg)
        (setq ang0(/(* 2. pi jj)td3d_ds_arcseg)
              ls_p(cons(list(+ x(* rr(cos ang0)))(+ y(* rr(sin ang0))))ls_p)))
      (fpoly ls_p c1 T)

      ;;選択中の管の十字(黄)
      (if(if int_sel(= ii int_sel))
          (progn
            (fline(list(- x rr)y)(list(+ x rr)y)2)
            (fline(list x(- y rr))(list x(+ y rr))2)))
      )
   ls_pipe)

  (end_image)
  (princ))

;;---------------------------------------------------------------------
;; ダイアログ内の状態(グローバル)
;;   td3d_ds_cache    : 編集した断面 ((名前 . 管リスト) …)
;;   td3d_ds_dellist  : 決定時に削除する断面名
;;   td3d_ds_secnames : 表示中の断面名リスト
;;   td3d_ds_cursec   : 選択中の断面名
;;   td3d_ds_pipes    : 選択中の断面の管リスト(作業用)
;;   td3d_ds_ipipe    : 選択中の管の番号(0はじまり) / nil
;;   ※辞書への書き込みはダイアログを閉じてから行う(規約)。
;;     ダイアログ内の操作はすべてこのキャッシュに対して行う
;;---------------------------------------------------------------------

;;キャッシュから断面を取り出す(無ければ辞書から読んで入れる)
(defun td3d_ds_getsec( str / lst )
  (if(setq lst(assoc str td3d_ds_cache))
      (cdr lst)
    (progn
      (setq td3d_ds_cache(cons(cons str(td3d_ds_read str))td3d_ds_cache))
      (cdr(assoc str td3d_ds_cache)))))

;;キャッシュへ断面を入れる
(defun td3d_ds_setsec( str ls / lst )
  (if(setq lst(assoc str td3d_ds_cache))
      (setq td3d_ds_cache(subst(cons str ls)lst td3d_ds_cache))
    (setq td3d_ds_cache(cons(cons str ls)td3d_ds_cache))))

;;---------------------------------------------------------------------
;; ダイアログ部品(表示の更新)
;;---------------------------------------------------------------------
(defun td3d_ds_dlg_seclist( / num )
  (start_list "seclist")
  (mapcar 'add_list td3d_ds_secnames)
  (end_list)
  (if(setq num(vl-position td3d_ds_cursec td3d_ds_secnames))
      (set_tile "seclist"(itoa num)))
  (princ))

(defun td3d_ds_dlg_pipelist( / ii )
  (start_list "pipelist")
  (setq ii 0)
  (mapcar '(lambda(lst)
             (add_list(strcat(itoa(setq ii(1+ ii)))" : "(car lst)
                             "  D="(td3d_ds_numstr(td3d_ds_nth 3 lst)))))
          td3d_ds_pipes)
  (end_list)
  (if(and td3d_ds_ipipe(< td3d_ds_ipipe(length td3d_ds_pipes)))
      (set_tile "pipelist"(itoa td3d_ds_ipipe)))
  (princ))

;;選択中の管を入力欄へ(無ければ既定値)
(defun td3d_ds_dlg_fields( / lst name x y d ct w h f c1 c2 )
  (setq lst(if td3d_ds_ipipe(td3d_ds_nth td3d_ds_ipipe td3d_ds_pipes)))
  (mapcar 'set '(name x y d ct w h f c1 c2)(td3d_ds_fillpipe lst))
  (set_tile "pname" name)
  (set_tile "px"(td3d_ds_numstr x))
  (set_tile "py"(td3d_ds_numstr y))
  (set_tile "pdiam"(td3d_ds_numstr d))
  (set_tile "pcontype"(itoa ct))
  (set_tile "pw"(td3d_ds_numstr w))
  (set_tile "ph"(td3d_ds_numstr h))
  (set_tile "pf"(td3d_ds_numstr f))
  (set_tile "pcolduct"(itoa c1))
  (set_tile "pcolcon"(itoa c2))
  (princ))

;;入力欄から管1本ぶんを読む
(defun td3d_ds_dlg_gettiles( / )
  (list(get_tile "pname")
       (atof(get_tile "px"))(atof(get_tile "py"))(atof(get_tile "pdiam"))
       (atoi(get_tile "pcontype"))
       (atof(get_tile "pw"))(atof(get_tile "ph"))(atof(get_tile "pf"))
       (atoi(get_tile "pcolduct"))(atoi(get_tile "pcolcon"))))

(defun td3d_ds_dlg_preview( / )
  (td3d_ds_preview td3d_ds_pipes td3d_ds_ipipe))

;;---------------------------------------------------------------------
;; ダイアログ部品(操作)
;;---------------------------------------------------------------------
;;断面が選ばれていないときの警告
(defun td3d_ds_needsec( / )
  (if td3d_ds_cursec nil
    (progn;;断面を一覧から選んでください
      (td3d_ds_alert(mix_strasc(list 26029 38754 12434 19968 35239 12363 12425 36984 12435 12391 12367 12384 12373 12356)))
      T)))

;;断面リストのクリック
(defun td3d_ds_dlg_selsec( / num str )
  (setq num(atoi(get_tile "seclist")))
  (if(setq str(td3d_ds_nth num td3d_ds_secnames))
      (progn
        (setq td3d_ds_cursec str
              td3d_ds_pipes(td3d_ds_getsec str)
              td3d_ds_ipipe(if td3d_ds_pipes 0))
        (set_tile "secname" str)
        (td3d_ds_dlg_pipelist)
        (td3d_ds_dlg_fields)
        (td3d_ds_dlg_preview)))
  (princ))

;;断面の新規/複製 (bool_copy = T で複製)
(defun td3d_ds_dlg_snew( bool_copy / str strerr )
  (setq str(get_tile "secname"))
  (cond
   ((setq strerr(td3d_ds_validname str))(td3d_ds_alert strerr))
   ((vl-position str td3d_ds_secnames);;同名の断面が既にあります
    (td3d_ds_alert(mix_strasc(list 21516 21517 12398 26029 38754 12364 26082 12395 12354 12426 12414 12377))))
   ((and bool_copy(null td3d_ds_cursec))(td3d_ds_needsec))
   (T
    (setq td3d_ds_secnames(append td3d_ds_secnames(list str))
          td3d_ds_dellist(vl-remove str td3d_ds_dellist))
    (td3d_ds_setsec str(if bool_copy td3d_ds_pipes(list)))
    (setq td3d_ds_cursec str
          td3d_ds_pipes(td3d_ds_getsec str)
          td3d_ds_ipipe(if td3d_ds_pipes 0))
    (td3d_ds_dlg_seclist)
    (td3d_ds_dlg_pipelist)
    (td3d_ds_dlg_fields)
    (td3d_ds_dlg_preview)))
  (princ))

;;断面のリネーム(secname 欄の値へ)
(defun td3d_ds_dlg_sren( / str strerr )
  (setq str(get_tile "secname"))
  (cond
   ((td3d_ds_needsec))
   ((setq strerr(td3d_ds_validname str))(td3d_ds_alert strerr))
   ((= str td3d_ds_cursec))
   ((vl-position str td3d_ds_secnames)
    (td3d_ds_alert(mix_strasc(list 21516 21517 12398 26029 38754 12364 26082 12395 12354 12426 12414 12377))))
   (T
    ;;古い名前は決定時に辞書から消す(新規追加だった場合は消しても無害)
    (setq td3d_ds_dellist(cons td3d_ds_cursec td3d_ds_dellist)
          td3d_ds_secnames(subst str td3d_ds_cursec td3d_ds_secnames)
          td3d_ds_cache(vl-remove(assoc td3d_ds_cursec td3d_ds_cache)td3d_ds_cache)
          td3d_ds_cursec str)
    (td3d_ds_setsec str td3d_ds_pipes)
    (td3d_ds_dlg_seclist)))
  (princ))

;;断面の削除
(defun td3d_ds_dlg_sdel( / )
  (cond
   ((td3d_ds_needsec))
   (T
    (setq td3d_ds_dellist(cons td3d_ds_cursec td3d_ds_dellist)
          td3d_ds_secnames(vl-remove td3d_ds_cursec td3d_ds_secnames)
          td3d_ds_cache(vl-remove(assoc td3d_ds_cursec td3d_ds_cache)td3d_ds_cache)
          td3d_ds_cursec nil td3d_ds_pipes(list)td3d_ds_ipipe nil)
    (set_tile "secname" "")
    (td3d_ds_dlg_seclist)
    (td3d_ds_dlg_pipelist)
    (td3d_ds_dlg_fields)
    (td3d_ds_dlg_preview)))
  (princ))

;;管リストのクリック
(defun td3d_ds_dlg_selpipe( / num )
  (setq num(atoi(get_tile "pipelist")))
  (if(<(setq num(max num 0))(length td3d_ds_pipes))
      (progn
        (setq td3d_ds_ipipe num)
        (td3d_ds_dlg_fields)
        (td3d_ds_dlg_preview)))
  (princ))

;;管の追加/複製 (bool_copy = T で選択中を複製)
(defun td3d_ds_dlg_padd( bool_copy / lst )
  (cond
   ((td3d_ds_needsec))
   ((and bool_copy(null td3d_ds_ipipe)))
   (T
    (setq lst(if bool_copy(td3d_ds_nth td3d_ds_ipipe td3d_ds_pipes)
               (td3d_ds_fillpipe(list(strcat "P"(itoa(1+(length td3d_ds_pipes)))))))
          td3d_ds_pipes(append td3d_ds_pipes(list lst))
          td3d_ds_ipipe(1-(length td3d_ds_pipes)))
    (td3d_ds_setsec td3d_ds_cursec td3d_ds_pipes)
    (td3d_ds_dlg_pipelist)
    (td3d_ds_dlg_fields)
    (td3d_ds_dlg_preview)))
  (princ))

;;管の削除
(defun td3d_ds_dlg_pdel( / ii )
  (cond
   ((td3d_ds_needsec))
   ((null td3d_ds_ipipe))
   (T
    (setq ii -1
          td3d_ds_pipes(vl-remove-if '(lambda(a)(=(setq ii(1+ ii))td3d_ds_ipipe))
                                     td3d_ds_pipes)
          td3d_ds_ipipe(if td3d_ds_pipes
                           (min td3d_ds_ipipe(1-(length td3d_ds_pipes))))
          )
    (td3d_ds_setsec td3d_ds_cursec td3d_ds_pipes)
    (td3d_ds_dlg_pipelist)
    (td3d_ds_dlg_fields)
    (td3d_ds_dlg_preview)))
  (princ))

;;管の並び替え (int_d = -1:上へ / 1:下へ)
(defun td3d_ds_dlg_pmove( int_d / jj ii lst_a lst_b )
  (cond
   ((td3d_ds_needsec))
   ((null td3d_ds_ipipe))
   (T
    (setq jj(+ td3d_ds_ipipe int_d))
    (if(or(< jj 0)(>= jj(length td3d_ds_pipes)))T
      (progn
        (setq lst_a(td3d_ds_nth td3d_ds_ipipe td3d_ds_pipes)
              lst_b(td3d_ds_nth jj td3d_ds_pipes)
              ii -1
              td3d_ds_pipes
              (mapcar '(lambda(a)
                         (setq ii(1+ ii))
                         (cond((= ii td3d_ds_ipipe)lst_b)
                              ((= ii jj)lst_a)
                              (T a)))
                      td3d_ds_pipes)
              td3d_ds_ipipe jj)
        (td3d_ds_setsec td3d_ds_cursec td3d_ds_pipes)
        (td3d_ds_dlg_pipelist)
        (td3d_ds_dlg_preview)))))
  (princ))

;;反映 : 入力欄の値を選択中の管へ
(defun td3d_ds_dlg_apply( / lst strerr ii )
  (cond
   ((td3d_ds_needsec))
   ((null td3d_ds_ipipe);;管を一覧から選んでください(または追加)
    (td3d_ds_alert(mix_strasc(list 31649 12434 19968 35239 12363 12425 36984 12435 12391 12367 12384 12373 12356))))
   ((setq strerr(td3d_ds_validpipe(setq lst(td3d_ds_dlg_gettiles))))
    (td3d_ds_alert strerr))
   (T
    (setq lst(td3d_ds_fillpipe lst)
          ii -1
          td3d_ds_pipes(mapcar '(lambda(a)(if(=(setq ii(1+ ii))td3d_ds_ipipe)lst a))
                               td3d_ds_pipes))
    (td3d_ds_setsec td3d_ds_cursec td3d_ds_pipes)
    (td3d_ds_dlg_pipelist)
    (td3d_ds_dlg_preview)))
  (princ))

;;色ボタン : CAD標準の色選択。使えないCADでは番号入力のまま
(defun td3d_ds_dlg_color( str_key / int_col ret )
  (setq int_col(atoi(get_tile str_key))
        ret(vl-catch-all-apply
            'acad_colordlg(list(if(and(> int_col 0)(< int_col 256))int_col 3)nil)))
  (if(or(vl-catch-all-error-p ret)(null ret))T
    (set_tile str_key(itoa ret)))
  (princ))

;;決定(選択中の断面名をグローバルに返して閉じる)
(defun td3d_ds_dlg_ok( / )
  (setq td3d_ds_result td3d_ds_cursec)
  (done_dialog 0)
  (princ))

;;---------------------------------------------------------------------
;; 決定時の書き込み(ダイアログを閉じてから呼ぶ)
;;---------------------------------------------------------------------
(defun td3d_ds_applycache( / )
  (mapcar 'td3d_ds_erase td3d_ds_dellist)
  (mapcar '(lambda(a)(td3d_ds_write(car a)(cdr a)))(reverse td3d_ds_cache))
  (setq td3d_ds_cache(list)td3d_ds_dellist(list))
  (princ))

;;---------------------------------------------------------------------
;; CSV 入出力
;;   1行目 : SECTION,NAME,X,Y,DIAM,CONTYPE,WIDTH,HEIGHT,FILET,COLDUCT,COLCON
;;   2行目以降 : 管1本につき1行(ヘッダー名で列対応。順序は問わない)
;;   取込は同名の断面を丸ごと置き換える。値は図面単位のまま読む
;;---------------------------------------------------------------------
(defun td3d_ds_qq( str / num str_one str_out )
  (setq str_out "" num 0)
  (while(< num(strlen str))
    (setq num(1+ num)
          str_one(substr str num 1)
          str_out(strcat str_out(if(= str_one "\"")"\"\"" str_one))))
  (strcat "\"" str_out "\""))

(defun td3d_ds_csvsplit( str / len num str_one bool_q str_cur ls_out )
  (setq len(strlen str)num 0 str_cur "" ls_out(list))
  (while(< num len)
    (setq num(1+ num)str_one(substr str num 1))
    (cond
     ((= str_one "\"")
      (if(and bool_q(= "\""(substr str(1+ num)1)))
          (setq str_cur(strcat str_cur "\"")num(1+ num))
        (setq bool_q(null bool_q))))
     ((and(= str_one ",")(null bool_q))
      (setq ls_out(cons str_cur ls_out)str_cur ""))
     (T(setq str_cur(strcat str_cur str_one)))))
  (reverse(cons str_cur ls_out)))

(defun td3d_ds_trim( str / )
  (if str(vl-string-trim " \t\r" str)""))

(defun td3d_ds_pos( str lst / num )
  (setq num(vl-position str(mapcar 'strcase lst)))
  num)

(defun td3d_ds_export( / str_path open_file num_sec num_pipe )
  (setq str_path(getfiled(mix_strasc(list "CSV" 26360 20986))
                         (strcat(getvar "DWGPREFIX")"ductsection")"csv" 1))
  (if(null str_path)
      (td3d_ds_alert(mix_strasc(list 12501 12449 12452 12523 12364 36984 25246 12373 12428 12414 12379 12435 12391 12375 12383)))
    (if(null(setq open_file(open str_path "w")))
        (td3d_ds_alert(mix_strasc(list 12501 12449 12452 12523 12364 38283 12363 12428 12390 12356 12427 12383 12417 26360 36796 12415 12391 12365 12414 12379 12435 12391 12375 12383)))
      (progn
        (write-line "SECTION,NAME,X,Y,DIAM,CONTYPE,WIDTH,HEIGHT,FILET,COLDUCT,COLCON" open_file)
        (setq num_sec 0 num_pipe 0)
        (mapcar
         '(lambda(str_sec)
            (setq num_sec(1+ num_sec))
            (mapcar
             '(lambda(lst)
                (setq num_pipe(1+ num_pipe))
                (write-line
                 (strcat(td3d_ds_qq str_sec)","(td3d_ds_qq(car lst))
                        (apply 'strcat
                               (mapcar '(lambda(a)(strcat ","(as-numstr a)))
                                       (cdr lst))))
                 open_file))
             (td3d_ds_read str_sec)))
         (td3d_ds_names))
        (close open_file)
        ;;書き出しました
        (td3d_ds_alert
         (mix_strasc(list 26360 12365 20986 12375 12414 12375 12383 "\n"
                          26029 38754 " : "(itoa num_sec)"\n"
                          31649 " : "(itoa num_pipe)"\n" str_path)))
        )))
  (princ))

(defun td3d_ds_import( / str_path open_file str_line ls_head ls_idx ls_field
                         str_sec ls_sec lst lst_sec num_sec num_pipe num_err )
  (setq str_path(getfiled(mix_strasc(list "CSV" 21462 36796))
                         (strcat(getvar "DWGPREFIX")"ductsection")"csv" 0))
  (if(null str_path)
      (td3d_ds_alert(mix_strasc(list 12501 12449 12452 12523 12364 36984 25246 12373 12428 12414 12379 12435 12391 12375 12383)))
    (if(null(setq open_file(open str_path "r")))
        (td3d_ds_alert(mix_strasc(list 12501 12449 12452 12523 12364 38283 12369 12414 12379 12435 12391 12375 12383)))
      (progn
        ;;見出し行 : 列の位置をヘッダー名で対応付ける
        (setq ls_head(mapcar 'td3d_ds_trim
                             (td3d_ds_csvsplit(td3d_ds_trim(read-line open_file))))
              ls_idx(mapcar '(lambda(str)(td3d_ds_pos str ls_head))
                            (list "SECTION" "NAME" "X" "Y" "DIAM" "CONTYPE"
                                  "WIDTH" "HEIGHT" "FILET" "COLDUCT" "COLCON")))
        (if(vl-position nil ls_idx)
            (progn
              (close open_file)
              ;;1行目に必要な列が見つかりません
              (td3d_ds_alert
               (mix_strasc(list "1" 34892 30446 12395 24517 35201 12394 21015 12364 35211 12388 12363 12426 12414 12379 12435 "\nSECTION,NAME,X,Y,DIAM,CONTYPE,WIDTH,HEIGHT,FILET,COLDUCT,COLCON"))))
          (progn
            (setq ls_sec(list)num_err 0)
            (while(setq str_line(read-line open_file))
              (setq str_line(td3d_ds_trim str_line))
              (if(= str_line "")T
                (progn
                  (setq ls_field(td3d_ds_csvsplit str_line)
                        lst(mapcar '(lambda(num)(td3d_ds_trim(td3d_ds_nth num ls_field)))
                                   ls_idx)
                        str_sec(car lst)
                        lst(td3d_ds_fillpipe(cdr lst)))
                  (cond
                   ((or(td3d_ds_validname str_sec)(td3d_ds_validpipe lst))
                    (setq num_err(1+ num_err)))
                   (T
                    (if(setq lst_sec(assoc str_sec ls_sec))
                        (setq ls_sec(subst(cons str_sec(cons lst(cdr lst_sec)))
                                          lst_sec ls_sec))
                      (setq ls_sec(cons(cons str_sec(list lst))ls_sec))))))))
            (close open_file)

            (setq num_sec 0 num_pipe 0)
            (mapcar '(lambda(a)
                       (setq num_sec(1+ num_sec)
                             num_pipe(+ num_pipe(length(cdr a))))
                       (td3d_ds_write(car a)(reverse(cdr a))))
                    (reverse ls_sec))
            ;;取り込みました(同名の断面は置き換え)
            (td3d_ds_alert
             (mix_strasc(list 21462 12426 36796 12415 12414 12375 12383
                              " (" 21516 21517 12398 26029 38754 12399 32622 12365 25563 12360 ")\n"
                              26029 38754 " : "(itoa num_sec)"\n"
                              31649 " : "(itoa num_pipe)"\n"
                              12456 12521 12540 " : "(itoa num_err))))
            )))))
  (princ))

;;---------------------------------------------------------------------
;; 画面 : 断面の作成 / 編集 / 削除
;;   (td3dductsection) で呼ぶ。閉じると選択中の断面名が str_ductsection に入る
;;---------------------------------------------------------------------
(defun td3dductsection( / str_path load_dcl int_ret bool_loop num )

  (if(null(setq str_path(td3d_ds_writedcl)))
      (td3d_ds_alert(mix_strasc(list 12480 12452 12450 12525 12464 23450 32681 12501 12449 12452 12523 12434 26360 12365 20986 12379 12414 12379 12435 12391 12375 12383)))
    (progn
      (setq bool_loop T
            td3d_ds_cache(list)td3d_ds_dellist(list)
            td3d_ds_cursec nil td3d_ds_pipes(list)td3d_ds_ipipe nil
            td3d_ds_result nil)

      (while bool_loop
        (setq td3d_ds_secnames(td3d_ds_names)
              load_dcl(load_dialog str_path))
        (cond
         ((< load_dcl 0)
          (td3d_ds_alert(mix_strasc(list 12480 12452 12450 12525 12464 12434 35501 12415 36796 12417 12414 12379 12435 12391 12375 12383)))
          (setq bool_loop nil))
         ((null(new_dialog "td3dds" load_dcl))
          (unload_dialog load_dcl)
          (td3d_ds_alert(mix_strasc(list 12480 12452 12450 12525 12464 12434 38283 12369 12414 12379 12435 12391 12375 12383)))
          (setq bool_loop nil))
         (T
          ;;保護コンタイプの選択肢
          (start_list "pcontype")
          (mapcar 'add_list
                  (mapcar 'mix_strasc
                          (list(list 20840 21608);;全周
                               (list 19979 21322);;下半
                               (list 12394 12375);;なし
                               )))
          (end_list)

          (td3d_ds_dlg_seclist)
          (td3d_ds_dlg_pipelist)
          (td3d_ds_dlg_fields)
          (td3d_ds_dlg_preview)
          (if td3d_ds_cursec(set_tile "secname" td3d_ds_cursec))

          (action_tile "seclist"   "(td3d_ds_dlg_selsec)")
          (action_tile "btnsnew"   "(td3d_ds_dlg_snew nil)")
          (action_tile "btnscopy"  "(td3d_ds_dlg_snew T)")
          (action_tile "btnsren"   "(td3d_ds_dlg_sren)")
          (action_tile "btnsdel"   "(td3d_ds_dlg_sdel)")
          (action_tile "pipelist"  "(td3d_ds_dlg_selpipe)")
          (action_tile "btnpadd"   "(td3d_ds_dlg_padd nil)")
          (action_tile "btnpcopy"  "(td3d_ds_dlg_padd T)")
          (action_tile "btnpdel"   "(td3d_ds_dlg_pdel)")
          (action_tile "btnpup"    "(td3d_ds_dlg_pmove -1)")
          (action_tile "btnpdown"  "(td3d_ds_dlg_pmove 1)")
          (action_tile "btnapply"  "(td3d_ds_dlg_apply)")
          (action_tile "btncold"   "(td3d_ds_dlg_color \"pcolduct\")")
          (action_tile "btncolc"   "(td3d_ds_dlg_color \"pcolcon\")")
          (action_tile "btnok"     "(td3d_ds_dlg_ok)")
          (action_tile "btncancel" "(done_dialog 1)")
          (action_tile "btnexp"    "(done_dialog 4)")
          (action_tile "btnimp"    "(done_dialog 5)")

          (setq int_ret(start_dialog))
          (unload_dialog load_dcl)
          (setq load_dcl nil)

          ;;辞書の書き換えはダイアログを閉じてから行う
          (cond
           ((= int_ret 0);;決定 : 全編集を書き込み
            (td3d_ds_applycache)
            (setq str_ductsection td3d_ds_result)
            (if(vl-position str_ductsection(td3d_ds_names))T
              (setq str_ductsection nil))
            (princ(mix_strasc(list "\n" 20445 23384 12375 12414 12375 12383)));;保存しました
            (setq bool_loop nil))
           ((= int_ret 4);;CSV書出 : 編集を確定してから書き出し、画面へ戻る
            (td3d_ds_applycache)
            (td3d_ds_export))
           ((= int_ret 5);;CSV取込 : 編集を確定してから取り込み、画面へ戻る
            (td3d_ds_applycache)
            (td3d_ds_import)
            ;;取り込んだ断面を選び直してもらう
            (setq td3d_ds_cursec nil td3d_ds_pipes(list)td3d_ds_ipipe nil))
           (T;;キャンセル(Esc含む) : 編集をすべて破棄
            (setq td3d_ds_cache(list)td3d_ds_dellist(list))
            (princ(mix_strasc(list "\n" 32232 38598 12434 30772 26820 12375 12414 12375 12383)));;編集を破棄しました
            (setq bool_loop nil))
           ))
         ))
      ))
  (princ))

(princ)
