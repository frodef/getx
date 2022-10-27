# getx
### _Frode Fjeld <frodevf@gmail.com>_

## Intro

A DWIM-ish mechanism - or query language of sorts - to query
hierarchical data structures. Or, CL:GETF on steriods!

## Syntax

    (getx:? data &rest indicators)

## Description

The function GETX:? performs a getx query. The first argument is the
data-structure to be queried, while the remaining arguments, called
indicators, specify the query. For example:

    > (defparameter *data* '(:foo 1 :bar 2 :zap (:zonk 400 :zupp 500)))
    > (getx:? *data* :zap :zonk) => 400

In other words, each indicator is processed left to right, where
(usually) each sub-query is applied to the result from the previous
one. The result of the last sub-query is eventually returned from
GETX:?.

As in the example above, most atom indicators are looked up in a plist
under EQ as if by CL:GETF. The full set of query processing rules are:


0. A CL:NULL indicator is ignored, i.e. the query proceeds with the
   same DATA.

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

6. Otherwise, look up indicator in DATA as a plist, as if by CL:GETF.

Note that GETX:? is a normal function, and so standard CL evaluation
rules apply to its arguments.

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

3. Assuming a list of employee records (again represented as plists),
   choose all records whose :FIRST-NAME property is
   "Frode". GETX:SELECT implicitly fans out the remaining query for
   each such record. Fanning out means that the remaining query is
   processed once for each list element, and the list of results is
   returned.

4. Return the :EMAIL and :PHONE properties for the employee
   record. (Again, this will happen once for each match found by the
   previous GETX:SELECT indicator.)

Note here that GETX:SEEK finds exactly one element (or NIL) and
continues the query with that element. In contrast, GETX:SELECT can
match any number of records, and applies the remaining query to each
such record, returning a list of the results. The special indicator
GETX:EACH explicitly does this same fan-out across a list, without any
selection.

### Special indicators and evaluation

This section contains information that can normally be ignored.

GETX:SEEK, GETX:SELECT etc. are normal functions, and by CL evaluation
rules cannot themselves perform the query according to the GETX:?
query processing rules. Rather, these functions return internal
"special indicators" (i.e. lists) that are subsequently recognized by
GETX:? to perform the relevant operation. To illustrate:

    > (getx:select :first-name "Frode")
    => (GETX::%SELECT :FIRST-NAME "Frode" EQUAL)

No query processing is performed here, just trivial list manipulation.
The resulting list (indicator) is processed by rule 3 of the GETX:?
query processing rules above. In other words, these two forms are
equivalent:

    > (getx:? data (getx:select :name "Frode"))
    > (getx:? data '(getx::%select :name "Frode" equal))

The list whose CAR is GETX::%SELECT is the actual indicator, while the
function GETX:SELECT is merely a convenient way to create that
indicator. GETX::%SELECT also names the function that actually
performs the query processing, while GETX:SELECT is referred to as its
"surface syntax function".

Each "special indicator" is documented by its own docstring, and its
syntax is given by the surface syntax function's lambda-list. The idea
is to have resonably intuitive surface syntax that also adheres to
standard evaluation rules, and is reasonably efficient. Often, the
transformation above can be inlined and performed compile-time. See
macro GETX::DEFINE-GETX and GETX:?? for details.

Non-special indicators, such as integers and keywords, are mostly
self-evaluating, and thereby avoid this problem.

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

