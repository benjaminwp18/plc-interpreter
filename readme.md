# PLC Interpreter Project (Group 6)

## Interpreter
### binding.rkt
#### Manage bindings in the state
 - binding-lookup
 - binding-status
 - binding-set
 - binding-create

#### Binding statuses returned by `binding-status`
 - binding-unbound
 - binding-uninit
 - binding-init

#### Creating a state
 - empty-stt

#### State layers
 - binding-push-layer
 - binding-pop-layer

### common.rkt
 - not-null?
 - in-list?
 - not-equal?

### value.rkt
 - value-generic

### state.rkt
 - interpret
 - interpret-tree

## Tester
### tester.rkt
#### Convert raw HTML assignment tests
Run `tests/convert_tests.py` to convert raw HTML tests in `tests/raw_html` to
machine-readable tests in `tests/html`. This should only be done when new raw
tests are added.

#### Test HTML suites
You may need to install `html-parsing` and `ansi-color` to run the tester:

```bash
raco pkg install html-parsing
raco pkg install ansi-color
```

Then:
 - test-html: `(test-html)`
 - test-html-file: `(test-html-file "part1tests")`
 - test-html-single: `(test-html-single "part1tests" 5)`

#### Test source files (.j)
 - test-src: `(test-src)`

#### Old HTML tester & utility
 - test-raw-html (for files in `tests/raw_html`): `(test-raw-html)`
 - parse-str: `(parse-str "your code string here")`
