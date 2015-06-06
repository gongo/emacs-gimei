(setq gimei-test:names
      '(
        ("first-name"
         ("male" ("太郎" "たろう" "タロウ"))
         ("female" ("花子" "はなこ" "ハナコ")))
        ("last-name"
         ("山田" "やまだ" "ヤマダ"))
        ))

(setq gimei-test:addresses
      '(
        ("prefecture" ("沖縄県" "おきなわけん" "オキナワケン"))
        ("city" ("沖縄市" "おきなわし" "オキナワシ"))
        ("town" ("沖縄" "おきなわ" "オキナワ"))
        ))

(defun in-my-fixture (body)
  (unwind-protect
      (progn
        (let ((gimei->names gimei-test:names)
              (gimei->addresses gimei-test:addresses))
          (funcall body)))))

(ert-deftest gimei:new-name ()
  (in-my-fixture
   (lambda ()
     (should (gimei:name-p (gimei:new-name))))))

(ert-deftest gimei:new-male ()
  (in-my-fixture
   (lambda ()
     (let ((name (gimei:new-male)))
       (should (gimei:male-p name))
       (should (string-equal "山田 太郎" (gimei:kanji-of name)))
       (should (string-equal "やまだ たろう" (gimei:hiragana-of name)))
       (should (string-equal "ヤマダ タロウ" (gimei:katakana-of name)))
       ))))

(ert-deftest gimei:new-female ()
  (in-my-fixture
   (lambda ()
     (let ((name (gimei:new-female)))
       (should (gimei:female-p (gimei:new-female)))
       (should (string-equal "山田 花子" (gimei:kanji-of name)))
       (should (string-equal "やまだ はなこ" (gimei:hiragana-of name)))
       (should (string-equal "ヤマダ ハナコ" (gimei:katakana-of name)))
       ))))

(ert-deftest gimei:new-address ()
  (in-my-fixture
   (lambda ()
     (let ((address (gimei:new-address)))
       (should (gimei:address-p (gimei:new-address)))
       (should (string-equal "沖縄県沖縄市沖縄" (gimei:address:kanji-of address)))
       (should (string-equal "おきなわけんおきなわしおきなわ" (gimei:address:hiragana-of address)))
       (should (string-equal "オキナワケンオキナワシオキナワ" (gimei:address:katakana-of address)))
       ))))
