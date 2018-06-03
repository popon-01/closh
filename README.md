# Closh
SchemeサブセットのCommon Lispによる実装です。

## サポートする特殊形式
    lambda, quote('), quasiquote(`), 
    unquote(,), unquote-splicing(,@),
    set! let, let\*, letrec,
    if, cond, and, or, begin, do, quasi
    def-macro (Common Lisp風マクロの定義)
    cl-mode (部分的なCommon LispのS式の評価)
    
## サポートする関数
### 整数
number?, +, -, *, /, =, <, <=, >, >=

### リスト
null?, pair?, list?, symbol?,
car, cdr, cons, list, length, memq, last, append,
set-car!, set-cdr!

### ブール値
boolean?, not

### 文字列
string?, string-append,
symbol->string, string->symbol, string->number, number->string

### 関数
procedure?

### 比較
eq?, neq?, equal?

### その他
load

## defmacro
Schemeのdefineのスタイルで書いてください。
```
(defmacro (my-if test then else) 
    `(cond (,test ,then) 
           (,else else)))
```

## cl-mode
クォートなどは特に不要です。
```
(cl-mode (loop for i below 10 do (print i)))
```
