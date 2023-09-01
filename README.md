# RBAP lang

RBAP (Regressive Business Application Programming) is a high-level ( ͡~ ͜ʖ ͡°) programming language
based on ABAP programming language by SAP.

Project is written in Rust and contains bytecode compiler & bytecode virtual machine for the language.

```abap
DATA: lv_string TYPE string,
      lv_int TYPE i.

lv_string = '(ツ)'.
WRITE lv_string.

DATA(lv_template_string) = |\_{ lv_string }_/|.
WRITE: / ' ', lv_template_string, ' '.

WRITE: / lv_int.
lv_int = 5 + 10.
WRITE: / lv_int.
WRITE: / 'answer to life:', / (lv_int - 2 * 5) * (16 - 32 / 4) + 2.

IF lv_int = 15 AND 5 < 10 OR 15 > 5 OR 5 <> 10.
    WRITE: 'TRUE'.
ENDIF.
```
