Blah
====

Blah is a toy programming language written by Tian Yu Zhang.

It is written from scratch completely by hand, without using any generated code.

###Samples

Here is a sample of Blah code implementing FizzBuzz:

    # the mod operator does not exist yet
    to check_a_divides_b with a, b do
        return a = 0 or (b / a) * a = b
    done

    num: 1
    while num < 100 then
        if check_a_divides_b(15, num) then
            writeLine('FizzBuzz')

        else if check_a_divides_b(3, num) then
            writeLine('Fizz')

        else if check_a_divides_b(5, num) then
            writeLine('Buzz')

        otherwise
            writeLine(num)
        fi

        num: num + 1
    repeat

And here is a sample of recursion:

    to calcFactorial with n do
        if n = 0 then
            return 1
        otherwise
            return n * calcFactorial(n - 1)
        fi
    done

    writeLine(calcFactorial(10))

You check out more examples in the tests directory.

###Compile and Use

To compile Blah, run `ghc --make main.hs` in the root directory of the project.

To run Blah in script mode, run `./blah your_blah_file.hs`.

To run Blah in REPL mode, run `./blah`.

Blah is very slow and there are probably many bugs, so feel free to improve on it or just tell me how to improve it.


