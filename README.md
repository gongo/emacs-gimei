emacs-gimei
====================

Emacs Lisp port of [willnet/gimei](https://github.com/willnet/gimei).

Usage
--------------------

```lisp
(require 'gimei)

(setq name (gimei:new-name))
(gimei:kanji name)          ;; "浅野 深桜"
(gimei:hiragana name)       ;; "あさの みお"
(gimei:katakana name)       ;; "アサノ ミオ"
(gimei:last:kanji name)     ;; "浅野"
(gimei:last:hiragana name)  ;; "あさの"
(gimei:last:katakana name)  ;; "アサノ"
(gimei:first:kanji name)    ;; "深桜"
(gimei:first:hiragana name) ;; "みお"
(gimei:first:katakana name) ;; "ミオ"

(gimei:kanji (gimei:new-male))   ;; "篠田 繁夫"
(gimei:kanji (gimei:new-female)) ;; "稲葉 澪"

(setq address (gimei:new-address))
(gimei:address:kanji address)    ;; "山梨県杵築市宮浦"
(gimei:address:hiragana address) ;; "やまなしけんきつきしみやのうら"
(gimei:address:katakana address) ;; "ヤマナシケンキツキシミヤノウラ"

(gimei:prefecture:kanji address) ;; "山梨県"
(gimei:city:hiragana address)    ;;"きつきし"
(gimei:town:katakana address)    ;; "ミヤノウラ"
```

SEE ALSO
--------------------

- [willnet/gimei](https://github.com/willnet/gimei)
- [mattn/go-gimei](https://github.com/mattn/go-gimei)

License
--------------------

MIT License
