# getx
### _Frode Fjeld <frodevf@gmail.com>_

## Intro

A DWIM-ish mechanism to query hierarchical data structures. Or,
CL:GETF on steriods!

## Syntax

    (getx:? data &rest indicators)

## Description

The main function GETX:? performs a getx query. The first argument is
the data-structure to be queried, while the remaining arguments,
called indicators, specify the query. For example:

    > (defparameter *data* '(:foo 1 :bar 2 :zap (:zonk 400 :zupp 500)))
    > (getx:? *data* :zap :zonk) => 400

In other words, each indicator/query is executed left to right, each
where each query is applied to the result from the previous one. The
result of the last query is returned from GETX:?.

As in the example above, most atom indicators are looked up in a plist
under EQ as if by CL:GETF. The full set of query processing rules are:

  1. A CL:INTEGER indicator is looked up by index. An out-of-bounds
     index returns NIL. A negative index is counted from the end. DATA
     must be either a vector or a proper list. Effort is taken to
     traverse lists exactly once.

  2. A CL:FUNCTIONP indicator is applied to the DATA.

  3. A CL:CONSP indicator is "special", see below.

  4. A CL:HASH-TABLE-P DATA object is sent to CL:GETHASH using the
     indicator as key.

  5. A CL:STANDARD-OBJECT or CL:STRUCTURE-OBJECT is sent to
     CL:SLOT-VALUE using the indicator as slot-name.

  6. Look up indicator treating DATA as a plist, as if by CL:GETF.

Note that GETX:? is a normal function, and so standard CL evaluation
rules apply to the indicators.

## Special indicators

Special indicators is a more extensible syntax for indicators, and
allows for more complex queries:

    > (getx:? company-plists
              (getx:seek :name "Acme" 'str:contains?)
              :employees
              (getx:select :first-name "Frode")
              (getx:multiple :email :phone))

Assuming a list of company records, each represented as a plist, this
query does the following:

  1. Search out the first company whose :NAME property matches "Acme"
     under STR:CONTAINS?.

  2. For that company record, extract the :EMPLOYEES property.

  3. Assuming a list of EMPLOYEE records (represented as plists),
     choose all records whose :FIRST-NAME property is
     "Frode". GETX:SELECT implicitly fans out the remaining query for
     each such record.

  4. Return the :EMAIL and :PHONE properties for the employee record.

Note here that GETX:SEEK finds exactly one element (or NIL) and
continues the query with that element. In contrast, GETX:SELECT can
match any number of records, and applies the remaining query to each
such record, returning a list of the results. The special indicator
GETX:EACH explicitly does this same fan-out across a list, without any
selection.

### Special indicators and evaluation

Note that GETX:SEEK, GETX:SELECT etc. are normal functions, and by CL
evaluation rules cannot themselves perform the query according to
GETX:? query processing. Rather, these functions return "special
forms" (i.e. lists) that are recognized by GETX:? to perform the
relevant operation. For example:

    > (getx:select :first-name "Frode")
    => (GETX::%SELECT :FIRST-NAME "Frode" EQUAL)

This list is then processed by rule 3 of the GETX:? query processing
rules above. Each "special form" is documented by its own
docstring. The idea is to have resonably intuitive surface syntax that
also adheres to standard evaluation rules, and is reasonably
efficient. Often, the transformation above can be inlined and
performed compile-time. See macro GETX::DEFINE-GETX for details.

Non-special indicators are all self-evaluating, and thereby avoid this
problem.

## Example

Here is a complete, self-contained example form to play with:

     > (getx:? '((:name "Froogle, Inc"
                  :address "Earth"
                  :employees ((:first-name "Bob"
                               :email "bob@bob.com")))
                 (:name "Acme Lispcode"
                  :employees ((:first-name "Frode"
                               :email "frodevf@gmail.com"
                               :phone "1234567")
                              (:first-name "Alice"
                               :email "alice@acme-lisp.com"))))
               (getx:seek :name "Acme" 'str:contains?)
               :employees
               (getx:select :first-name "Frode")
               (getx:multiple :email :phone))


## License

This is free and unencumbered software released into the public domain.

Anyone is free to copy, modify, publish, use, compile, sell, or
distribute this software, either in source code form or as a compiled
binary, for any purpose, commercial or non-commercial, and by any
means.

In jurisdictions that recognize copyright laws, the author or authors
of this software dedicate any and all copyright interest in the
software to the public domain. We make this dedication for the benefit
of the public at large and to the detriment of our heirs and
successors. We intend this dedication to be an overt act of
relinquishment in perpetuity of all present and future rights to this
software under copyright law.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.

