# Compile to Brainfuck

Brainfuck is a simple but hard to use programming language.
The idea here is to create a language which compiles to brainfuck. Here are some considerations:

- Code is not addressable
    - Functions must be inline
    - No first class functions/recursion

## Calling Convention

- When a function is called, the active cell is the first unused cell, so the function can act as if it is a program in itself
- Except that arguments are passed to the left of the active cell when the function is called
- When a function returs, the arguments should be removed, and in their place is the return value, which is also the active cell

## Example Code


    def iseven(a)
        b = 0
        while a
            if b
                b = 0
            else
                b = 1
            end
            a = dec(a)
        end
    return b


## Steps

- &#9745; Defining the language
- &#9745; Parser
- &#9746; Code generator