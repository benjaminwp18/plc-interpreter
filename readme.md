# PLC Interpreter Project (Group 6)

## Interpreter
### binding.rkt
#### Bindings in the state
 - binding-lookup
 - binding-status
 - binding-set
 - binding-create
 - binding-unbound
 - binding-uninit
 - binding-init

#### Whole state
 - empty-stt
 - stt-empty?

### common.rkt
 - non-null?
 - in-list?

### value.rkt
 - value-generic

### state.rkt
 - interpret
 - interpret-tree

## Tester
### tester.rkt
 - test-src
 - test-html