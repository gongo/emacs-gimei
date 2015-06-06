emacs-gimei
====================

Emacs Lisp port of [willnet/gimei](https://github.com/willnet/gimei).

Usage
--------------------

```lisp
(require 'gimei)

;;
;; Generate name
;;

(setq name (gimei:new-name))
(gimei:kanji-of name)          ;; "浅野 深桜"
(gimei:hiragana-of name)       ;; "あさの みお"
(gimei:katakana-of name)       ;; "アサノ ミオ"
(gimei:last:kanji-of name)     ;; "浅野"
(gimei:last:hiragana-of name)  ;; "あさの"
(gimei:last:katakana-of name)  ;; "アサノ"
(gimei:first:kanji-of name)    ;; "深桜"
(gimei:first:hiragana-of name) ;; "みお"
(gimei:first:katakana-of name) ;; "ミオ"

(gimei:kanji-of (gimei:new-male))   ;; "篠田 繁夫"
(gimei:kanji-of (gimei:new-female)) ;; "稲葉 澪"

;;
;; Generate address
;;

(setq address (gimei:new-address))
(gimei:address:kanji-of address)    ;; "山梨県杵築市宮浦"
(gimei:address:hiragana-of address) ;; "やまなしけんきつきしみやのうら"
(gimei:address:katakana-of address) ;; "ヤマナシケンキツキシミヤノウラ"

(gimei:prefecture:kanji-of address) ;; "山梨県"
(gimei:city:hiragana-of address)    ;;"きつきし"
(gimei:town:katakana-of address)    ;; "ミヤノウラ"

;;
;; Else..
;;

(gimei:kanji)    ;; "宮原敏也"
(gimei:hiragana) ;; "あだちてつ"
(gimei:katakana) ;; "キシモトコタロウ"

(gimei:address:kanji)    ;; "神奈川県河内長野市藤助新田"
(gimei:address:hiragana) ;; "みえけんわたりぐんやまもとちょうきたじゅう"
(gimei:address:katakana) ;; "オオサカフコユグンシントミチョウナカノバタチョウ"
```

SEE ALSO
--------------------

- [willnet/gimei](https://github.com/willnet/gimei)
- [mattn/go-gimei](https://github.com/mattn/go-gimei)

License
--------------------

MIT License
