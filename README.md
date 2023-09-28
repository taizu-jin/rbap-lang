# RBAP lang

RBAP (Regressive Business Application Programming) is a high-level ( ͡~ ͜ʖ ͡°) programming language
based on ABAP programming language by SAP.

Project is written in Rust and contains bytecode compiler & bytecode virtual machine for the language.

```abap
DATA: lv_string TYPE string,
      lv_template_string TYPE string,
      lv_int TYPE i.

lv_string = '(ツ)'.
WRITE lv_string.

lv_template_string = |\_{ lv_string }_/|.
WRITE: / ' ', lv_template_string, ' '.

WRITE: / lv_int.
lv_int = 5 + 10.
WRITE: / lv_int.
WRITE: / 'answer to life:', / (lv_int - 2 * 5) * (16 - 32 / 4) + 2.

IF lv_int == 15 AND 5 < 10 OR 15 > 5 OR 5 <> 10 OR NOT (5 <> 5) OR NOT rbap_false AND rbap_true.
    WRITE: 'TRUE'.
ENDIF.

METHOD sum IMPORTING iv_x TYPE i
                     iv_y TYPE i 
           RETURNING rv_sum TYPE i.
  rv_sum = iv_x + iv_y.
ENDMETHOD.

METHOD sub IMPORTING iv_x TYPE i
                     iv_y TYPE i 
           RETURNING rv_sub TYPE i.
  rv_sub = iv_x - iv_y.
ENDMETHOD.

DATA: lv_sum TYPE i,
      lv_sub TYPE i.

lv_sum = sum(2, 1).
lv_sub = sub(lv_sum, 2).

WRITE:/ lv_sum, / lv_sub.
```

# CLI tool

CLI tool has the following functions.

1. Run source file
```
rbap-lang run <source.rbap>
```

2. Compile source file
```
rbap-lang compile <source.rbap> [dest]
```

3. Execute compiled file
```
rbap-lang execute <compiled>
```

4. Enter REPL
```
rbap-lang 
```

5. Help
```
rbap-lang  --help
```
